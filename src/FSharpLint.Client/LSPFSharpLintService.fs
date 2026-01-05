module FSharpLint.Client.LSPFSharpLintService

open System
open System.IO
open System.Threading
open System.Threading.Tasks
open StreamJsonRpc
open FSharpLint.Client.Contracts
open FSharpLint.Client.LSPFSharpLintServiceTypes
open FSharpLint.Client.FSharpLintToolLocator

type ServiceState =
    { Daemons: Map<FSharpLintVersion, RunningFSharpLintTool>
      FolderToVersion: Map<Folder, FSharpLintVersion> }

    static member Empty: ServiceState =
        { Daemons = Map.empty
          FolderToVersion = Map.empty }

[<RequireQualifiedAccess>]
type GetDaemonError =
    | DotNetToolListError of error: DotNetToolListError
    | FSharpLintProcessStart of error: ProcessStartError
    | InCompatibleVersionFound
    | CompatibleVersionIsKnownButNoDaemonIsRunning of version: FSharpLintVersion

type Msg =
    | GetDaemon of folder: Folder * replyChannel: AsyncReplyChannel<Result<JsonRpc, GetDaemonError>>
    | Reset of AsyncReplyChannel<unit>

let private createAgent (ct: CancellationToken) =
    let tryGetVersionFromCache state version folder (replyChannel: AsyncReplyChannel<Result<JsonRpc, GetDaemonError>>) =
        let daemon = Map.tryFind version state.Daemons

        match daemon with
        | Some daemon ->
            // We have a daemon for the required version in the cache, check if we can still use it.
            if daemon.Process.HasExited then
                // weird situation where the process has crashed.
                // Trying to reboot
                (daemon :> IDisposable).Dispose()

                let newDaemonResult = createFor daemon.StartInfo

                match newDaemonResult with
                | Ok newDaemon ->
                    replyChannel.Reply(Ok newDaemon.RpcClient)

                    Some { FolderToVersion = Map.add folder version state.FolderToVersion
                           Daemons = Map.add version newDaemon state.Daemons }
                | Error pse ->
                    replyChannel.Reply(Error(GetDaemonError.FSharpLintProcessStart pse))
                    Some state
            else
                // return running client
                replyChannel.Reply(Ok daemon.RpcClient)

                Some { state with FolderToVersion = Map.add folder version state.FolderToVersion }
        | None -> None

    let processor (inbox: MailboxProcessor<Msg>) =
        let rec messageLoop (state: ServiceState) =
            async {
                let! msg = inbox.Receive()

                let nextState =
                    match msg with
                    | GetDaemon(folder, replyChannel) ->
                        // get the version for that folder
                        // look in the cache first
                        let versionFromCache = Map.tryFind folder state.FolderToVersion
                        match versionFromCache with
                        | Some version ->
                            tryGetVersionFromCache state version folder replyChannel
                            |> Option.defaultWith(fun () ->
                                // This is a strange situation, we know what version is linked to that folder but there is no daemon
                                // The moment a version is added, is also the moment a daemon is re-used or created
                                replyChannel.Reply(
                                    Error(GetDaemonError.CompatibleVersionIsKnownButNoDaemonIsRunning version)
                                )

                                state)
                        | None ->
                            // Try and find a version of fsharplint daemon for our current folder
                            let fsharpLintToolResult: Result<FSharpLintToolFound, FSharpLintToolError> =
                                findFSharpLintTool folder

                            match fsharpLintToolResult with
                            | Ok(FSharpLintToolFound(version, startInfo)) ->
                                let createDaemon() =
                                    let createDaemonResult = createFor startInfo

                                    match createDaemonResult with
                                    | Ok daemon ->
                                        replyChannel.Reply(Ok daemon.RpcClient)

                                        { Daemons = Map.add version daemon state.Daemons
                                          FolderToVersion = Map.add folder version state.FolderToVersion }
                                    | Error pse ->
                                        replyChannel.Reply(Error(GetDaemonError.FSharpLintProcessStart pse))
                                        state


                                tryGetVersionFromCache state version folder replyChannel
                                |> Option.defaultWith(fun () -> createDaemon())
                            | Error FSharpLintToolError.NoCompatibleVersionFound ->
                                replyChannel.Reply(Error GetDaemonError.InCompatibleVersionFound)
                                state
                            | Error(FSharpLintToolError.DotNetListError dotNetToolListError) ->
                                replyChannel.Reply(Error(GetDaemonError.DotNetToolListError dotNetToolListError))
                                state
                    | Reset replyChannel ->
                        Map.toList state.Daemons
                        |> List.iter (fun (_, daemon) -> (daemon :> IDisposable).Dispose())

                        replyChannel.Reply()
                        ServiceState.Empty

                return! messageLoop nextState
            }

        messageLoop ServiceState.Empty

    MailboxProcessor.Start(processor, cancellationToken = ct)

type FSharpLintServiceError =
    | DaemonNotFound of GetDaemonError
    | FileDoesNotExist
    | FilePathIsNotAbsolute
    | CancellationWasRequested

let isPathAbsolute (path: string) : bool =
    if
        String.IsNullOrWhiteSpace path
        || path.IndexOfAny(Path.GetInvalidPathChars()) <> -1
        || not (Path.IsPathRooted path)
    then
        false
    else
        let pathRoot = Path.GetPathRoot path
        // Accepts X:\ and \\UNC\PATH, rejects empty string, \ and X:, but accepts / to support Linux
        if pathRoot.Length <= 2 && pathRoot <> "/" then
            false
        else if pathRoot.[0] <> '\\' || pathRoot.[1] <> '\\' then
            true
        else
            pathRoot.Trim('\\').IndexOf('\\') <> -1 // A UNC server name without a share name (e.g "\\NAME" or "\\NAME\") is invalid

let private isCancellationRequested (requested: bool) : Result<unit, FSharpLintServiceError> =
    if requested then
        Error FSharpLintServiceError.CancellationWasRequested
    else
        Ok()

let private getFolderFor (file: FileInfo) (): Result<Folder, FSharpLintServiceError> =
    let handleFile (file: FileInfo) =
        if not (isPathAbsolute file.FullName) then
            Error FSharpLintServiceError.FilePathIsNotAbsolute
        else match Folder.FromFile file.FullName with
             | None -> Error FSharpLintServiceError.FileDoesNotExist
             | Some folder -> Ok folder

    handleFile file

let private getDaemon (agent: MailboxProcessor<Msg>) (folder: Folder) : Result<JsonRpc, FSharpLintServiceError> =
    let daemon = agent.PostAndReply(fun replyChannel -> GetDaemon(folder, replyChannel))

    match daemon with
    | Ok daemon -> Ok daemon
    | Error gde -> Error(FSharpLintServiceError.DaemonNotFound gde)

let private fileNotFoundResponse (file: FileInfo) : Async<FSharpLintResponse> = async {
        return {
            Code = int FSharpLintResponseCode.ErrFileNotFound
            FilePath = file.FullName
            Result = Content $"File \"%s{file.FullName}\" does not exist."
        }
    }

let private fileNotAbsoluteResponse (file: FileInfo) : Async<FSharpLintResponse> = async {
        return {
            Code = int FSharpLintResponseCode.ErrFilePathIsNotAbsolute
            FilePath = file.FullName
            Result = Content $"\"%s{file.FullName}\" is not an absolute file path. Relative paths are not supported."
        }
    }

let private daemonNotFoundResponse (file: FileInfo) (error: GetDaemonError) : Async<FSharpLintResponse> =
    let content, code =
        match error with
        | GetDaemonError.DotNetToolListError(DotNetToolListError.ProcessStartError(ProcessStartError.ExecutableFileNotFound(executableFile,
                                                                                                                            arguments,
                                                                                                                            workingDirectory,
                                                                                                                            pathEnvironmentVariable,
                                                                                                                            error)))
        | GetDaemonError.FSharpLintProcessStart(ProcessStartError.ExecutableFileNotFound(executableFile,
                                                                                       arguments,
                                                                                       workingDirectory,
                                                                                       pathEnvironmentVariable,
                                                                                       error)) ->
            ($"FSharpLint.Client tried to run `%s{executableFile} %s{arguments}` inside working directory \"{workingDirectory}\" but could not find \"%s{executableFile}\" on the PATH (%s{pathEnvironmentVariable}). Error: %s{error}",
            FSharpLintResponseCode.ErrDaemonCreationFailed)
        | GetDaemonError.DotNetToolListError(DotNetToolListError.ProcessStartError(ProcessStartError.UnexpectedException(executableFile,
                                                                                                                         arguments,
                                                                                                                         error)))
        | GetDaemonError.FSharpLintProcessStart(ProcessStartError.UnexpectedException(executableFile, arguments, error)) ->
            ($"FSharpLint.Client tried to run `%s{executableFile} %s{arguments}` but failed with \"%s{error}\"",
            FSharpLintResponseCode.ErrDaemonCreationFailed)
        | GetDaemonError.DotNetToolListError(DotNetToolListError.ExitCodeNonZero(executableFile,
                                                                                 arguments,
                                                                                 exitCode,
                                                                                 error)) ->
            ($"FSharpLint.Client tried to run `%s{executableFile} %s{arguments}` but exited with code {exitCode} {error}",
            FSharpLintResponseCode.ErrDaemonCreationFailed)
        | GetDaemonError.InCompatibleVersionFound ->
            ("FSharpLint.Client did not found a compatible dotnet tool version to launch as daemon process",
            FSharpLintResponseCode.ErrToolNotFound)
        | GetDaemonError.CompatibleVersionIsKnownButNoDaemonIsRunning(FSharpLintVersion version) ->
            ($"FSharpLint.Client found a compatible version `%s{version}` but no daemon could be launched.",
            FSharpLintResponseCode.ErrDaemonCreationFailed)

    async {
        return {
            Code = int code
            FilePath = file.FullName
            Result = Content content
        }
    }

let private cancellationWasRequestedResponse (file: FileInfo) : Async<FSharpLintResponse> = async {
        return {
            Code = int FSharpLintResponseCode.ErrCancellationWasRequested
            FilePath = file.FullName
            Result = Content "FSharpLintService is being or has been disposed."
        }
    }

let mapResultToResponse (file: FileInfo) (result: Result<Task<FSharpLintResponse>, FSharpLintServiceError>) =
    match result with
    | Ok version -> Async.AwaitTask version
    | Error FSharpLintServiceError.FileDoesNotExist -> fileNotFoundResponse file
    | Error FSharpLintServiceError.FilePathIsNotAbsolute -> fileNotAbsoluteResponse file
    | Error(FSharpLintServiceError.DaemonNotFound err) -> daemonNotFoundResponse file err
    | Error FSharpLintServiceError.CancellationWasRequested -> cancellationWasRequestedResponse file

type LSPFSharpLintService() =
    let cts = new CancellationTokenSource()
    let agent = createAgent cts.Token

    interface IFSharpLintService with
        member this.Dispose() =
            if not cts.IsCancellationRequested then
                agent.PostAndReply Reset |> ignore<_>
                cts.Cancel()

        member _.VersionAsync(versionRequest: VersionRequest, ?cancellationToken: CancellationToken) : Async<FSharpLintResponse> =
            isCancellationRequested cts.IsCancellationRequested
            |> Result.bind (getFolderFor (FileInfo versionRequest.FilePath))
            |> Result.bind (getDaemon agent)
            |> Result.map (fun client ->
                client
                    .InvokeWithCancellationAsync<string>(Methods.Version, cancellationToken = Option.defaultValue cts.Token cancellationToken)
                    .ContinueWith(fun (task: Task<string>) ->
                        { Code = int FSharpLintResponseCode.OkCurrentDaemonVersion
                          Result = Content task.Result
                          FilePath = versionRequest.FilePath }))
            |> mapResultToResponse (FileInfo versionRequest.FilePath)
