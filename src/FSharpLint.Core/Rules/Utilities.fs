module FSharpLint.Rules.Utilities

open System.IO
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
    open System
    open FSharpLint.Framework.Rules
    open FSharpLint.Framework.Ast
    open FSharpLint.Framework

    type LibraryHeuristicResult =
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
    let rec howLikelyLintTargetIsInLibraryJudgingByNames (fs: FileSystemInfo): LibraryHeuristicResult =
        let nonRecursiveFunc (fs: FileSystemInfo): LibraryHeuristicResult =
            let libraryAbbrev = "lib"
            let targetName = Path.GetFileNameWithoutExtension fs.FullName
            let nameSegments =
                Helper.Naming.QuickFixes.splitByCaseChange targetName
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
            elif targetName.ToLowerInvariant().EndsWith libraryAbbrev then
                Likely
            else
                Uncertain

        match nonRecursiveFunc fs with
        | Likely -> Likely
        | Unlikely -> Unlikely
        | Uncertain ->
            match fs with
            | :? FileInfo as file ->
                howLikelyLintTargetIsInLibraryJudgingByNames file.Directory
            | :? DirectoryInfo as dir ->
                // some tests have fake paths, so we need this to not throw inside them
                if not dir.Exists then
                    Uncertain
                else
                    let maybeParentDir = Option.ofObj dir.Parent
                    match dir.EnumerateFiles "*.fsproj" |> Seq.tryHead with
                    | Some _projFile ->
                        match maybeParentDir with
                        | None -> Uncertain
                        | Some parentDir -> nonRecursiveFunc parentDir
                    | None ->
                        match maybeParentDir with
                        | None -> Uncertain
                        | Some parentDir ->
                            howLikelyLintTargetIsInLibraryJudgingByNames parentDir
            | _ -> Uncertain

    let private hasEntryPoint (checkFileResults: FSharpCheckFileResults) (maybeProjectCheckResults: Option<FSharpCheckProjectResults>) =
        let hasEntryPointInTheSameFile =
            match checkFileResults.ImplementationFile with
            | Some implFile -> implFile.HasExplicitEntryPoint
            | None -> 
                let allSymbolUses = checkFileResults.GetAllUsesOfAllSymbolsInFile()
                allSymbolUses 
                    |> Seq.exists 
                        (fun symbolUse -> 
                            symbolUse.IsFromDefinition
                            && symbolUse.Symbol.HasAttribute<EntryPointAttribute>() )

        hasEntryPointInTheSameFile
        ||
        match maybeProjectCheckResults with
        | Some projectCheckResults ->
            projectCheckResults.AssemblyContents.ImplementationFiles
            |> Seq.exists (fun implFile -> implFile.HasExplicitEntryPoint)
        | None ->
            false
    
    let extractAttributeNames (attributes: SynAttributes) =
        seq {
            for attr in extractAttributes attributes do
                match attr.TypeName with
                | SynLongIdent([ident], _, _) -> yield ident.idText
                | _ -> ()
        }

    let private testMethodAttributes = [ "Test"; "TestMethod" ]
    let private testClassAttributes = [ "TestFixture"; "TestClass" ]

    let private areThereTestsInSameFileOrProject (nodes: array<AbstractSyntaxArray.Node>) (maybeProjectCheckResults: FSharpCheckProjectResults option) =
        let isTestMethodOrClass node =
            match node with
            | AstNode.MemberDefinition(SynMemberDefn.Member(SynBinding(_, _, _, _, attributes, _, _, _, _, _, _, _, _), _)) ->
                attributes 
                |> extractAttributeNames
                |> Seq.exists (fun name -> testMethodAttributes |> List.contains name)
            | AstNode.TypeDefinition(SynTypeDefn.SynTypeDefn(SynComponentInfo(attributes, _, _, _, _, _, _, _), _, _, _, _, _)) ->
                attributes 
                |> extractAttributeNames
                |> Seq.exists (fun name -> testClassAttributes |> List.contains name)
            | _ -> false
    
        let containsTests declaration =
            match declaration with
            | FSharpImplementationFileDeclaration.Entity(entity, declarations) when entity.IsClass ->
                entity.Attributes
                |> Seq.exists (fun attr -> testClassAttributes |> List.contains attr.AttributeType.DisplayName)
                ||
                declarations
                |> Seq.exists
                    (fun memberDecl ->
                        match memberDecl with
                        | FSharpImplementationFileDeclaration.MemberOrFunctionOrValue(method, _, _) when method.IsMethod ->
                            method.Attributes
                            |> Seq.exists (fun attr -> testMethodAttributes |> List.contains attr.AttributeType.DisplayName)
                        | _ -> false)
            | _ -> false

        match maybeProjectCheckResults with
        | Some projectCheckResults ->
            projectCheckResults.AssemblyContents.ImplementationFiles
            |> Seq.exists (fun implFile -> 
                implFile.Declarations
                |> Seq.exists containsTests
            )
        | None ->
            nodes |> Array.exists (fun node -> isTestMethodOrClass node.Actual)

    let isInObsoleteMethodOrFunction parents =
        let isObsolete node =
            match node with
            | AstNode.MemberDefinition(SynMemberDefn.Member(SynBinding(_, _, _, _, attributes, _, _, _, _, _, _, _, _), _)) ->
                attributes 
                |> extractAttributeNames
                |> Seq.contains "Obsolete"
            | AstNode.Binding(SynBinding(_, _, _, _, attributes, _, _, _, _, _, _, _, _)) ->
                attributes 
                |> extractAttributeNames
                |> Seq.contains "Obsolete"
            | _ -> false

        parents |> List.exists isObsolete

    let checkIfInLibrary (args: AstNodeRuleParams) : LibraryHeuristicResult =
        let lastHint =
            lazy(
                if areThereTestsInSameFileOrProject args.SyntaxArray args.ProjectCheckInfo then
                    Unlikely
                else
                    match args.CheckInfo with
                    | Some checkFileResults when hasEntryPoint checkFileResults args.ProjectCheckInfo ->
                        Unlikely
                    | _ -> Uncertain
            )

        match (args.CheckInfo, args.ProjectCheckInfo) with
        | Some _checkFileResults, Some checkProjectResults ->
            let projectFile = FileInfo checkProjectResults.ProjectContext.ProjectOptions.ProjectFileName
            match howLikelyLintTargetIsInLibraryJudgingByNames projectFile with
            | Uncertain -> lastHint.Value
            | veryCertain -> veryCertain
        | Some _checkFileResults, None ->
            // args.FilePath is empty when running tests
            if String.IsNullOrEmpty args.FilePath then
                lastHint.Value
            else
                match howLikelyLintTargetIsInLibraryJudgingByNames (FileInfo args.FilePath) with
                | Uncertain -> lastHint.Value
                | veryCertain -> veryCertain

        | _ ->
            let updatedLastHint =
                lazy(
                    if areThereTestsInSameFileOrProject args.SyntaxArray args.ProjectCheckInfo then
                        Unlikely
                    else
                        Uncertain
                )

            // args.FilePath is empty when running tests
            if String.IsNullOrEmpty args.FilePath then
                updatedLastHint.Value
            else
                match howLikelyLintTargetIsInLibraryJudgingByNames (FileInfo args.FilePath) with
                | Uncertain -> updatedLastHint.Value
                | veryCertain -> veryCertain

