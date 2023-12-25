module FSharpLint.Rules.UsedUnderscorePrefixedElements

open System

open FSharp.Compiler.Symbols
open FSharp.Compiler.CodeAnalysis

open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let private tryFindNodeIndexForUsage (usage: FSharpSymbolUse) (syntaxArray: array<AbstractSyntaxArray.Node>): Option<int> =
    syntaxArray 
    |> Array.tryFindIndex 
        (fun each -> 
            match each.Actual with
            | Expression(ExpressionUtilities.Identifier(_, range))
            | Identifier (_, range) -> range = usage.Range
            | _ -> false)

let runner (args: AstNodeRuleParams) =
    // hack to only run rule once
    if args.NodeIndex = 0 then
        match args.CheckInfo with
        | Some checkResults -> 
            let allUsages = checkResults.GetAllUsesOfAllSymbolsInFile() |> Seq.toArray
            seq {
                for usage in allUsages do
                    match usage.Symbol with
                    | :? FSharpMemberOrFunctionOrValue as symbol -> 
                        if not usage.IsFromDefinition && symbol.DisplayName.StartsWith "_" 
                                && symbol.DisplayName <> "_" && not symbol.IsCompilerGenerated && not symbol.IsMember then
                            let nameWithoutUnderscore = symbol.DisplayName.[1..]
                            let clashesWithOtherDefinitions =
                                match tryFindNodeIndexForUsage usage args.SyntaxArray with
                                | Some symbolNodeIndex ->
                                    let parents = args.GetParents symbolNodeIndex |> List.toArray
                                    //|> List.exists (fun parent -> definesSymbol parent nameWithoutUnderscore)
                                    let declarationsOfnameWithoutUnderscore =
                                        allUsages 
                                        |> Array.filter 
                                            (fun each -> each.Symbol.DisplayName = nameWithoutUnderscore && each.IsFromDefinition)
                                    declarationsOfnameWithoutUnderscore.Length > 0
                                | None -> false
                                
                            if clashesWithOtherDefinitions then
                                yield {
                                    Range = usage.Range
                                    Message = Resources.GetString ("RulesUsedUnderscorePrefixedElements")
                                    SuggestedFix = None
                                    TypeChecks = List.Empty
                                }
                            else
                                for range in [usage.Range; symbol.DeclarationLocation] do
                                    let warningDetrails = 
                                        lazy(
                                            let fromText = symbol.DisplayName
                                            Some { FromRange = range; FromText = fromText; ToText = nameWithoutUnderscore })
                                    yield {
                                        Range = range
                                        Message = Resources.GetString ("RulesUsedUnderscorePrefixedElements")
                                        SuggestedFix = Some warningDetrails
                                        TypeChecks = List.Empty
                                    }
                        else
                            ()
                    | _ -> () }
            |> Seq.distinctBy (fun wargningDetails -> wargningDetails.Range)
            |> Seq.toArray
        | None -> Array.empty
    else
        Array.empty

let rule =
    { Name = "UsedUnderscorePrefixedElements"
      Identifier = Identifiers.UsedUnderscorePrefixedElements
      RuleConfig = { AstNodeRuleConfig.Runner = runner; Cleanup = ignore } }
    |> AstNodeRule
