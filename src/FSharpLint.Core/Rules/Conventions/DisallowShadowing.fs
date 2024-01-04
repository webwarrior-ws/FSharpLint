module FSharpLint.Rules.DisallowShadowing

open System

open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Syntax
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
                    usage.Range.StartLine < identifier.idRange.StartLine 
                    || usage.Range.StartColumn < identifier.idRange.StartColumn)
        
        definitionsBeforeCurrent
        |> Seq.toArray
        |> Array.collect
            (fun definition ->
                // tried different approaches, none of them works
                    Array.empty )
                
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