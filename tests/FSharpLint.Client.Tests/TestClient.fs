module FSharpLint.Client.Tests

open NUnit.Framework
open System.IO
open System
open Contracts
open LSPFSharpLintService
open LSPFSharpLintServiceTypes

let (</>) path1 path2 = Path.Combine(path1, path2)

let basePath = TestContext.CurrentContext.TestDirectory </> ".." </> ".." </> ".." </> ".." </> ".." |> DirectoryInfo
let fsharpLintConsoleDll = 
    basePath.FullName </> "src" </> "FSharpLint.Console" </> "bin" </> "Release" </> "net9.0" </> "dotnet-fsharplint.dll"
    |> FileInfo
let fsharpConsoleOutputDir = 
    Folder.FromFile fsharpLintConsoleDll.FullName
    |> Option.defaultWith (fun () -> failwith $"Console project output dir (dir of file {fsharpLintConsoleDll}) does not exist.")

[<RequireQualifiedAccess>]
type ToolStatus = | Available | NotAvailable
type ToolLocationOverride(toolStatus: ToolStatus) =
    let tempFolder = Path.GetTempFileName()

    do match toolStatus with
       | ToolStatus.Available -> 
            Environment.SetEnvironmentVariable("FSHARPLINT_SEARCH_PATH_OVERRIDE", Folder.Unwrap fsharpConsoleOutputDir)
       | ToolStatus.NotAvailable ->
            let path = Environment.GetEnvironmentVariable("PATH")
            // ensure bin dir is not in path
            if path.Contains(Folder.Unwrap fsharpConsoleOutputDir, StringComparison.InvariantCultureIgnoreCase) then
                Assert.Inconclusive()

            File.Delete(tempFolder)
            Directory.CreateDirectory(tempFolder) |> ignore<DirectoryInfo>

            // set search path to an empty dir
            Environment.SetEnvironmentVariable("FSHARPLINT_SEARCH_PATH_OVERRIDE", tempFolder)

    interface IDisposable with
        member this.Dispose() =
            if File.Exists tempFolder then
                File.Delete tempFolder

let runVersionCall (file: File) (service: IFSharpLintService) =
    async {
        let request =
            {
                FilePath = File.Unwrap file
            }
        let! version = service.VersionAsync(request) |> Async.AwaitTask
        return version
    }
    |> Async.RunSynchronously

let getTestHintsFile () =
    let filePath = basePath.FullName </> "tests" </> "FSharpLint.FunctionalTest.TestedProject" </> "FSharpLint.FunctionalTest.TestedProject.NetCore" </> "TestHints.fs"
    filePath
    |> File.From
    |> Option.defaultWith (fun () -> failwith $"File {filePath} does not exist.")

[<Test>]
let TestDaemonNotFound() =
    using (new ToolLocationOverride(ToolStatus.NotAvailable)) <| fun _ ->

        let testHintsFile = getTestHintsFile()
        let fsharpLintService: IFSharpLintService = new LSPFSharpLintService() :> IFSharpLintService
        let versionResponse = runVersionCall testHintsFile fsharpLintService

        Assert.AreEqual(LanguagePrimitives.EnumToValue FSharpLintResponseCode.ErrToolNotFound, versionResponse.Code)

[<Test>]
let TestDaemonVersion() =
    using (new ToolLocationOverride(ToolStatus.Available)) <| fun _ ->

        let testHintsFile = getTestHintsFile()
        let fsharpLintService: IFSharpLintService = new LSPFSharpLintService() :> IFSharpLintService
        let versionResponse = runVersionCall testHintsFile fsharpLintService

        match versionResponse.Result with
        | Content result -> Assert.IsFalse (String.IsNullOrWhiteSpace result)
        // | _ -> Assert.Fail("Response should be a version number")

        Assert.AreEqual(LanguagePrimitives.EnumToValue FSharpLintResponseCode.OkCurrentDaemonVersion, versionResponse.Code)

[<Test>]
let TestFilePathShouldBeAbsolute() =
    using (new ToolLocationOverride(ToolStatus.Available)) <| fun _ ->

        let testHintsFile = getTestHintsFile()
        let fsharpLintService: IFSharpLintService = new LSPFSharpLintService() :> IFSharpLintService
        let versionResponse = runVersionCall testHintsFile fsharpLintService

        Assert.AreEqual(LanguagePrimitives.EnumToValue FSharpLintResponseCode.ErrFilePathIsNotAbsolute, versionResponse.Code)

[<Test>]
let TestFileShouldExists() =
    using (new ToolLocationOverride(ToolStatus.Available)) <| fun _ ->

        let testHintsFile = getTestHintsFile()
        let fsharpLintService: IFSharpLintService = new LSPFSharpLintService() :> IFSharpLintService
        let versionResponse = runVersionCall testHintsFile fsharpLintService

        Assert.AreEqual(LanguagePrimitives.EnumToValue FSharpLintResponseCode.ErrFileNotFound, versionResponse.Code)
