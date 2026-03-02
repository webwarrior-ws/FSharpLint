module FSharpLint.Rules.NoAsyncRunSynchronouslyInLibrary

open FSharp.Compiler.Syntax
open FSharp.Compiler.Symbols
open FSharp.Compiler.Text
open FSharp.Compiler.CodeAnalysis
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharpLint.Framework.Utilities
open FSharpLint.Rules.Utilities.LibraryHeuristics

let hasEntryPoint (checkFileResults: FSharpCheckFileResults) (maybeProjectCheckResults: Option<FSharpCheckProjectResults>) =
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

let testMethodAttributes = [ "Test"; "TestMethod" ]
let testClassAttributes = [ "TestFixture"; "TestClass" ]

let areThereTestsInSameFileOrProject (nodes: array<AbstractSyntaxArray.Node>) (maybeProjectCheckResults: FSharpCheckProjectResults option) =
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

let checkIfInLibrary (args: AstNodeRuleParams) : bool =
    let ruleNotApplicable =
        isInObsoleteMethodOrFunction (args.GetParents args.NodeIndex)
        ||
        match (args.CheckInfo, args.ProjectCheckInfo) with
        | Some checkFileResults, Some checkProjectResults ->
            let projectFile = System.IO.FileInfo checkProjectResults.ProjectContext.ProjectOptions.ProjectFileName
            match howLikelyProjectIsLibrary projectFile.Name with
            | Likely -> false
            | Unlikely -> true
            | Uncertain ->
                hasEntryPoint checkFileResults args.ProjectCheckInfo
                || areThereTestsInSameFileOrProject args.SyntaxArray args.ProjectCheckInfo
        | Some checkFileResults, None ->
            hasEntryPoint checkFileResults None
            || areThereTestsInSameFileOrProject args.SyntaxArray args.ProjectCheckInfo
        | _ ->
            areThereTestsInSameFileOrProject args.SyntaxArray args.ProjectCheckInfo

    ruleNotApplicable

let runner args =
    match args.AstNode with
    | AstNode.Identifier(["Async"; "RunSynchronously"], range) ->
        let ruleIsApplicable = not (checkIfInLibrary args)
        if ruleIsApplicable then
            Array.singleton 
                { 
                    Range = range
                    Message = Resources.GetString "NoAsyncRunSynchronouslyInLibrary"
                    SuggestedFix = None
                    TypeChecks = List.Empty 
                }
        else
            Array.empty
    | _ -> Array.empty

let rule =
    AstNodeRule
        {
            Name = "NoAsyncRunSynchronouslyInLibrary"
            Identifier = Identifiers.NoAsyncRunSynchronouslyInLibrary
            RuleConfig =
                {
                    AstNodeRuleConfig.Runner = runner
                    Cleanup = ignore
                }
        }
