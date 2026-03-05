module FSharpLint.Rules.NoAsyncRunSynchronouslyInLibrary

open System
open System.IO
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

let runner args =
    match args.AstNode with
    | AstNode.Identifier(["Async"; "RunSynchronously"], range) ->
        let ruleIsApplicable =
            if isInObsoleteMethodOrFunction (args.GetParents args.NodeIndex) then
                false
            else
                // we're considering Unlike=Uncertain here because if it's .fsx it's not lib for sure, and if it's just .fs we prefer to not give false positives,
                // especially taking in account that this rule is enabled by default
                checkIfInLibrary args = Likely
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
