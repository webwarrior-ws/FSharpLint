module FSharpLint.Rules.AsynchronousFunctionNames

open System
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open FSharp.Compiler.Syntax

let (|HasAsyncPrefix|HasAsyncSuffix|HasNoAsyncPrefixOrSuffix|) (pattern: SynLongIdent) =
    match List.tryLast pattern.LongIdent with
    | Some name ->
        if name.idText.StartsWith "Async" then
            HasAsyncPrefix name.idText
        elif name.idText.EndsWith "Async" then
            HasAsyncSuffix name.idText
        else
            HasNoAsyncPrefixOrSuffix name.idText
    | _ -> HasNoAsyncPrefixOrSuffix String.Empty

let (|ReturnsTask|ReturnsAsync|ReturnsNonAsync|) (returnInfo: SynBindingReturnInfo) =
    match returnInfo with
    | SynBindingReturnInfo(SynType.LongIdent(SynLongIdent(typeIdent, _, _)), _, _, _) ->
        match List.tryLast typeIdent with
        | Some ident when ident.idText = "Async" -> ReturnsAsync
        | Some ident when ident.idText = "Task" -> ReturnsTask
        | _ -> ReturnsNonAsync
    | _ -> ReturnsNonAsync

let runner (args: AstNodeRuleParams) =
    match args.AstNode with
    | AstNode.Binding (SynBinding (_, _, _, _, _, _, _, SynPat.LongIdent(HasNoAsyncPrefixOrSuffix name, _, _, _, _, identRange), returnInfo, _, _, _, _)) ->
        match returnInfo with
        | Some ReturnsTask ->
            Array.singleton
                {
                    Range = identRange
                    Message = $"This function returns Task. Consider naming it {name}Async"
                    SuggestedFix = None
                    TypeChecks = List.empty
                }
        | Some ReturnsAsync ->
            Array.singleton
                {
                    Range = identRange
                    Message = $"This function returns Async. Consider naming it Async{name}"
                    SuggestedFix = None
                    TypeChecks = List.empty
                }
        | _ -> 
            // TODO: get type from args.CheckInfo
            Array.empty
    | _ -> Array.empty

let rule =
    AstNodeRule
        {
            Name = "AsynchronousFunctionNames"
            Identifier = Identifiers.AsynchronousFunctionNames
            RuleConfig =
                {
                    AstNodeRuleConfig.Runner = runner
                    Cleanup = ignore
                }
        }
