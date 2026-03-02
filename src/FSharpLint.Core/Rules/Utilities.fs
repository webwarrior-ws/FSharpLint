module FSharpLint.Rules.Utilities

open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax

module TypedTree =
    let tryGetLastGenericArg (fSharpType: FSharpType) =
        Seq.tryLast fSharpType.GenericArguments

    [<TailCall>]
    let rec private getReturnType (fSharpType: FSharpType) =
        if fSharpType.IsFunctionType then
            match tryGetLastGenericArg fSharpType with
            | Some argType -> getReturnType argType
            | None -> fSharpType
        else
            fSharpType

    let getFunctionReturnType
        (checkInfo: FSharpCheckFileResults)
        (lines: array<string>)
        (funcIdent: SynLongIdent) : Option<FSharpType> =
        let maybeSymbolUse =
            match List.tryLast funcIdent.LongIdent with
            | Some ident ->
                checkInfo.GetSymbolUseAtLocation(
                    ident.idRange.EndLine,
                    ident.idRange.EndColumn,
                    lines.[ident.idRange.EndLine - 1],
                    List.singleton ident.idText)
            | None -> None
        match maybeSymbolUse with
        | Some symbolUse ->
            match symbolUse.Symbol with
            | :? FSharpMemberOrFunctionOrValue as func when func.IsFunction ->
                Some <| getReturnType func.FullType
            | _ -> None
        | _ -> None

module LibraryHeuristics =
    open System.IO

    type LibraryHeuristicResultByPath =
        | Likely
        | Unlikely
        | Uncertain

    let private projectNamesUnlikelyToBeLibraries =
        [
            "tests"
            "test"
            "testing"
            "console"
            "CLI"
            "TUI"
        ]
        |> Seq.map (fun name -> name.ToLowerInvariant())

    let private possibleProjectNameSegmentSeparators =
        [|
            '.'
            '_'
            '-'
        |]

    [<TailCall>]
    let rec private howLikelyIsInLibrary (fsInfo: FileSystemInfo) =
        let libraryAbbrev = "lib"
        let nameSegments =
            Helper.Naming.QuickFixes.splitByCaseChange fsInfo.Name
            |> Seq.map (fun segment -> segment.ToLowerInvariant())
        if nameSegments |> Seq.contains libraryAbbrev then
            Likely
        elif
            nameSegments
            |> Seq.exists (
                fun segment ->
                    let subSegments = segment.Split possibleProjectNameSegmentSeparators
                    subSegments
                    |> Seq.exists (fun subSegment ->
                        projectNamesUnlikelyToBeLibraries
                        |> Seq.exists (fun noLibName -> noLibName = subSegment)
                    )
            ) then
            Unlikely
        elif fsInfo.Name.ToLowerInvariant().EndsWith libraryAbbrev then
            Likely
        else
            let maybeParent =
                match fsInfo with
                | :? FileInfo as fileInfo -> Some fileInfo.Directory
                | :? DirectoryInfo as dirInfo -> Option.ofObj dirInfo.Parent
                | _ -> None
            match maybeParent with
            | Some parent -> howLikelyIsInLibrary parent
            | None -> Uncertain

    let howLikelyFileIsInLibrary (filePath: string): LibraryHeuristicResultByPath =
        if System.String.IsNullOrEmpty filePath then
            Uncertain
        else
            howLikelyIsInLibrary <| FileInfo filePath
