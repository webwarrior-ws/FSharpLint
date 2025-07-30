module FSharpLint.Rules.TupleCommaSpacing

open System
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharpLint.Rules.Helper

// Check for single space after commas in tuple.
let checkTupleCommaSpacing (args:AstNodeRuleParams) (tupleExprs:SynExpr list) tupleRange _ =
    tupleExprs
    |> List.toArray
    |> Array.pairwise
    |> Array.choose (fun (expr, nextExpr) ->
        if expr.Range.EndLine = nextExpr.Range.StartLine && expr.Range.EndColumn + 2 <> nextExpr.Range.StartColumn then
            let commaRange = Range.mkRange "" expr.Range.End nextExpr.Range.Start
            let fix =
                ExpressionUtilities.tryFindTextOfRange commaRange args.FileContent
                |> Option.map (fun commaText ->
                    lazy(
                        { FromRange = commaRange
                          ToText = ", " } |> Some
                        ) )
            { Range = commaRange
              Message = Resources.GetString("RulesFormattingTupleCommaSpacingError")
              Fix = fix
              TypeChecks = [] } |> Some
        else
            None)

let runner (args:AstNodeRuleParams) = TupleFormatting.isActualTuple args checkTupleCommaSpacing

let rule =
    { Name = "TupleCommaSpacing"
      Identifier = Identifiers.TupleCommaSpacing
      RuleConfig = { AstNodeRuleConfig.Runner = runner; Cleanup = ignore } }
    |> AstNodeRule
