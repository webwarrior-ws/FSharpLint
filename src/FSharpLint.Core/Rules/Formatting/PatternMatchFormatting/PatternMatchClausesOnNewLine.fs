module FSharpLint.Rules.PatternMatchClausesOnNewLine

open System
open FSharp.Compiler.Syntax
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharpLint.Rules.Helper

let check args _ (clauses:SynMatchClause list) _ =
    clauses
    |> List.toArray
    |> Array.pairwise
    |> Array.choose (fun (clauseOne, clauseTwo) ->
        if clauseOne.Range.EndLine = clauseTwo.Range.StartLine then
            { Range = clauseTwo.Range
              Message = Resources.GetString("RulesFormattingPatternMatchClausesOnNewLineError")
              Fix = None
              TypeChecks = [] } |> Some
        else
            None)

let runner (args:AstNodeRuleParams) = PatternMatchFormatting.isActualPatternMatch args check

let rule =
    { Name = "PatternMatchClausesOnNewLine"
      Identifier = Identifiers.PatternMatchClausesOnNewLine
      RuleConfig = { AstNodeRuleConfig.Runner = runner; Cleanup = ignore } }
    |> AstNodeRule
