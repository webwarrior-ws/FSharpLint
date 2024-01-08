module FSharpLint.Rules.DisallowShadowing

open System
open FSharp.Compiler.Syntax
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules


let private checkIdentifier (args: AstNodeRuleParams) (identifier: Ident) : array<WarningDetails> =
    let name = identifier.idText
    match args.CheckInfo with
    | Some checkResults -> 
        let allUsages = checkResults.GetAllUsesOfAllSymbolsInFile()
        let definitionsWithSameName = 
            allUsages 
            |> Seq.filter (fun usage -> usage.IsFromDefinition && usage.Symbol.DisplayName = name)

        let definitionsBeforeCurrent =
            definitionsWithSameName
            |> Seq.filter 
                (fun usage -> 
                    (usage.Range.StartLine, usage.Range.StartColumn) 
                        < (identifier.idRange.StartLine, identifier.idRange.StartColumn) )
            |> Seq.toArray

        let rangeIncludedsDefinitions range =
            definitionsBeforeCurrent
            |> Array.exists (fun usage -> ExpressionUtilities.rangeContainsOtherRange range usage.Range)
        
        let parents = args.GetParents args.NodeIndex
        
        let rec processAstNode (node: AstNode) =
            match node with
            | AstNode.ModuleOrNamespace(SynModuleOrNamespace(_, _, _, declarations, _, _, _, _, _)) ->
                declarations |> List.exists processModuleDeclaration
            | _ -> false
        and processModuleDeclaration (moduleDecl: SynModuleDecl) =
            match moduleDecl with
            | SynModuleDecl.Let(_, bindings, _) ->
                bindings 
                |> List.exists 
                    (fun binding ->
                        match binding with
                        | SynBinding(_, _, _, _, _, _, _, _, _, _, range, _, _) ->
                            rangeIncludedsDefinitions range)
            | _ -> false

        let isShadowing =
            parents |> List.exists processAstNode

        if isShadowing then
            Array.singleton { 
                Range = identifier.idRange
                Message = "RulesDisallowShadowing"
                SuggestedFix = None
                TypeChecks = List.Empty }
        else
            Array.empty
                
    | None -> Array.empty

let runner (args: AstNodeRuleParams) =
    match args.AstNode with
    | Pattern(SynPat.Named(SynIdent(ident, _), _, _, _)) ->
        checkIdentifier args ident
    | Lambda(lambda, _) ->
        let rec checkSimplePattern (pattern: SynSimplePat) =
            match pattern with
            | SynSimplePat.Id(ident, _, _, _, _, _) ->
                checkIdentifier args ident
            | SynSimplePat.Attrib(pat, _, _)
            | SynSimplePat.Typed(pat, _, _) ->
                checkSimplePattern pat

        lambda.Arguments 
        |> List.toArray
        |> Array.collect
            (fun argPatterns -> 
                match argPatterns with
                | SynSimplePats.SimplePats(patterns, _)
                | SynSimplePats.Typed(SynSimplePats.SimplePats(patterns, _), _, _) -> 
                    patterns
                    |> List.toArray
                    |> Array.collect checkSimplePattern
                | _ -> Array.empty)
    | _ ->
        Array.empty

let rule =
    { Name = "DisallowShadowing"
      Identifier = Identifiers.DisallowShadowing
      RuleConfig = { AstNodeRuleConfig.Runner = runner; Cleanup = ignore } }
    |> AstNodeRule