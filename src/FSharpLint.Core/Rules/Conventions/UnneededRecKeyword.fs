module FSharpLint.Rules.UnneededRecKeyword

open System
open FSharp.Compiler.Syntax
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Text
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion

type internal RecursiveFunctionInfo =
    {
        Identifier: Ident
        Range: range
        Body: SynExpr
        Attributes: SynAttributes
    }

let internal getRecursiveFunctions (astNode: AstNode) : array<RecursiveFunctionInfo>  =
    match astNode with
    | AstNode.ModuleDeclaration (SynModuleDecl.Let (true, bindings, _)) ->
        [|
            for binding in bindings do
                match binding with
                | SynBinding (_, _, _, _, attributes, _, _, SynPat.LongIdent (SynLongIdent([ident], _, _), _, _, _, _, range), _, body, _, _, _) ->
                    yield { Identifier = ident; Range = range; Body = body; Attributes = attributes } 
                | _ -> ()
        |]
    | _ -> Array.empty

/// Check if callee is called by one of callers.
let internal functionIsCalledByOneOf 
    (checkInfo: FSharpCheckFileResults) 
    (callee: RecursiveFunctionInfo) 
    (callers: array<RecursiveFunctionInfo>) =
    let funcName = callee.Identifier.idText
    checkInfo.GetAllUsesOfAllSymbolsInFile()
    |> Seq.exists (fun usage -> 
        usage.Symbol.DisplayName = funcName 
        && Array.exists 
            (fun caller -> ExpressionUtilities.rangeContainsOtherRange caller.Body.Range usage.Range)
            callers)

let internal emitWarning (func: RecursiveFunctionInfo) =
    { Range = func.Range
      Message =
          String.Format(
              Resources.GetString "RulesUnneededRecKeyword",
              func.Identifier.idText
          )
      SuggestedFix = None
      TypeChecks = list.Empty }

let runner (args: AstNodeRuleParams) =
    match args.CheckInfo with
    | Some checkInfo ->
        let functions = getRecursiveFunctions args.AstNode
        functions
        |> Array.choose
            (fun func ->
                if not <| functionIsCalledByOneOf checkInfo func functions then
                    Some(emitWarning func)
                else
                    None)
    | _ -> Array.empty

let rule =
    { Name = "UnneededRecKeyword"
      Identifier = Identifiers.UnneededRecKeyword
      RuleConfig =
        { AstNodeRuleConfig.Runner = runner
          Cleanup = ignore } }
    |> AstNodeRule
