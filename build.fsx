// --------------------------------------------------------------------------------------
// FAKE build script
// --------------------------------------------------------------------------------------
#r "paket: groupref build //"
#load ".fake/build.fsx/intellisense.fsx"

open Fake.Core
open Fake.DotNet
open Fake.Tools
open Fake.IO
open Fake.IO.FileSystemOperators
open Fake.IO.Globbing.Operators
open Fake.Core.TargetOperators
open Fake.Api

open System
open System.IO

Target.initEnvironment()

// --------------------------------------------------------------------------------------
// Information about the project to be used at NuGet and in AssemblyInfo files
// --------------------------------------------------------------------------------------

let project = "FSharpLint"

let authors = "Matthew Mcveigh"

let gitOwner = "fsprojects"
let gitName = "FSharpLint"
let gitHome = "https://github.com/" + gitOwner
let gitUrl = gitHome + "/" + gitName

// --------------------------------------------------------------------------------------
// Helpers
// --------------------------------------------------------------------------------------
let isNullOrWhiteSpace = System.String.IsNullOrWhiteSpace

let exec cmd args dir =
    let proc =
        CreateProcess.fromRawCommandLine cmd args
        |> CreateProcess.ensureExitCodeWithMessage (sprintf "Error while running '%s' with args: %s" cmd args)
    (if isNullOrWhiteSpace dir then proc
    else proc |> CreateProcess.withWorkingDirectory dir)
    |> Proc.run
    |> ignore

let getBuildParam var =
    let value = Environment.environVar var
    if String.IsNullOrWhiteSpace value then
        None
    else
        Some value
let DoNothing = ignore

// --------------------------------------------------------------------------------------
// Build variables
// --------------------------------------------------------------------------------------

let buildDir  = "./build/"
let nugetDir  = "./out/"
let rootDir = __SOURCE_DIRECTORY__ |> DirectoryInfo

System.Environment.CurrentDirectory <- rootDir.FullName
let changelogFilename = "CHANGELOG.md"
let changelog = Changelog.load changelogFilename

let githubRef = Environment.GetEnvironmentVariable "GITHUB_REF"
let tagPrefix = "refs/tags/"
let isTag =
    if isNull githubRef then
        false
    else
        githubRef.StartsWith tagPrefix

let nugetVersion =
    match (changelog.Unreleased, isTag) with
    | (Some _unreleased, true) -> failwith "Shouldn't publish a git tag for changes outside a real release"
    | (None, true) ->
        changelog.LatestEntry.NuGetVersion
    | (_, false) ->
        let current = changelog.LatestEntry.NuGetVersion |> SemVer.parse
        let bumped = { current with
                            Patch = current.Patch + 1u
                            Original = None
                            PreRelease = None }
        let bumpedBaseVersion = string bumped

        let nugetPreRelease = Path.Combine(rootDir.FullName, "nugetPreRelease.fsx")
        let procResult =
            CreateProcess.fromRawCommand
                "dotnet"
                [
                    "fsi"
                    nugetPreRelease
                    bumpedBaseVersion
                ]
            |> CreateProcess.redirectOutput
            |> CreateProcess.ensureExitCode
            |> Proc.run
        procResult.Result.Output.Trim()

let PackageReleaseNotes baseProps =
    if isTag then
        ("PackageReleaseNotes", sprintf "%s/blob/v%s/CHANGELOG.md" gitUrl nugetVersion)::baseProps
    else
        baseProps

// --------------------------------------------------------------------------------------
// Build Targets
// --------------------------------------------------------------------------------------

// --------------------------------------------------------------------------------------
// Release Targets
// --------------------------------------------------------------------------------------

Target.create "Push" (fun _ ->
    let push key =
        Paket.push (fun p -> { p with WorkingDir = nugetDir; ApiKey = key; ToolType = ToolType.CreateLocalTool() })

    let key = getBuildParam "nuget-key"
    match getBuildParam "GITHUB_EVENT_NAME" with
    | None ->
        match key with
        | None ->
            let key = UserInput.getUserPassword "NuGet Key: "
            push key
        | Some key ->
            push key

    | Some "push" ->
        match key with
        | None ->
            Console.WriteLine "No nuget-key env var found, skipping..."
        | Some key ->
            if isTag then
                push key
            else
                match getBuildParam "GITHUB_SHA" with
                | None ->
                    failwith "GITHUB_SHA should have been populated"
                | Some commitHash ->
                    let gitArgs = sprintf "describe --exact-match --tags %s" commitHash
                    let proc =
                        CreateProcess.fromRawCommandLine "git" gitArgs
                        |> Proc.run
                    if proc.ExitCode <> 0 then
                        // commit is not a tag, so go ahead pushing a prerelease
                        push key
                    else
                        Console.WriteLine "Commit mapped to a tag, skipping pushing prerelease..."
    | _ ->
        Console.WriteLine "Github event name not 'push', skipping..."

)

// --------------------------------------------------------------------------------------
// Build order
// --------------------------------------------------------------------------------------
Target.create "Default" DoNothing
Target.create "Release" DoNothing

"Default"
  ==> "Push"
  ==> "Release"

Target.runOrDefaultWithArguments "Default"
