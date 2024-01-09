module FSharpLint.Rules.EnsureTailCallDiagnosticsInRecursiveFunctions

open System

open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let private emitWarning (func: UnneededRecKeyword.RecursiveFunctionInfo) =
    { Range = func.Range
      Message =
        String.Format(
            Resources.GetString "RulesEnsureTailCallDiagnosticsInRecursiveFunctions",
            func.Identifier.idText
        )
      SuggestedFix = None
      TypeChecks = list.Empty }

let runner (args: AstNodeRuleParams) =
    match args.CheckInfo with
    | Some checkInfo ->
        let functions = UnneededRecKeyword.getRecursiveFunctions args.AstNode
        functions
        |> Array.choose
            (fun func ->
                if UnneededRecKeyword.functionIsCalledByOneOf checkInfo func functions then
                    let hasTailCallAttribute =
                        func.Attributes 
                        |> List.collect (fun attrs -> attrs.Attributes) 
                        |> List.exists 
                            (fun attr -> 
                                match attr.TypeName with
                                | SynLongIdent([ident], _, _) ->
                                    ident.idText = "TailCall" || ident.idText = "TailCallAttribute"
                                | _ -> false)
                    if hasTailCallAttribute then
                        None
                    else
                        Some(emitWarning func)
                else
                    None)
    | _ -> Array.empty

let rule =
    { Name = "EnsureTailCallDiagnosticsInRecursiveFunctions"
      Identifier = Identifiers.EnsureTailCallDiagnosticsInRecursiveFunctions
      RuleConfig = { AstNodeRuleConfig.Runner = runner; Cleanup = ignore } }
    |> AstNodeRule
