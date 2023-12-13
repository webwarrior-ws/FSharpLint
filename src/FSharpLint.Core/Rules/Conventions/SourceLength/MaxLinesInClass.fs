module FSharpLint.Rules.MaxLinesInClass

open FSharp.Compiler.Syntax
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let runner (config:Helper.SourceLength.Config) (args:AstNodeRuleParams) =
    match args.AstNode with
    | AstNode.TypeDefinition(SynTypeDefn(_, repr, _, _, range, _)) ->
        match repr with
        | SynTypeDefnRepr.ObjectModel(_) ->
            Helper.SourceLength.checkSourceLengthRule config range "Classes and interface"
        | _ -> Array.empty
    | _ -> Array.empty

let rule config =
    { Name = "MaxLinesInClass"
      Identifier = Identifiers.MaxLinesInClass
      RuleConfig = { AstNodeRuleConfig.Runner = runner config; Cleanup = ignore } }
    |> AstNodeRule