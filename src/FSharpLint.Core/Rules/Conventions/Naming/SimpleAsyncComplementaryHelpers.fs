module FSharpLint.Rules.SimpleAsyncComplementaryHelpers

open System
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open Helper.Naming.Asynchronous
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text

type private Func =
    | AsyncFunc of range: range * baseName: string
    | TaskFunc of range: range * baseName: string
    with
        member this.BaseName =
            match this with
            | AsyncFunc(_, name) -> name
            | TaskFunc(_, name) -> name

[<TailCall>]
let rec private getBindings (acc: list<SynBinding>) (declarations: list<SynModuleDecl>) =
    match declarations with
    | SynModuleDecl.Let(_, bindings, _) :: rest -> getBindings (acc @ bindings) rest
    | SynModuleDecl.NestedModule(_, _, innerDecls, _, _, _) :: rest -> getBindings acc (innerDecls @ rest)
    | [] -> acc
    | _ :: rest -> getBindings acc rest

let runner (args: AstNodeRuleParams) =
    let emitWarning (func: Func) =
        match func with
        | AsyncFunc(range, baseName) ->
            Array.singleton
                {
                    Range = range
                    Message = sprintf "Create %s that just calls Async.StartAsTask(AsyncBar())" (baseName + asyncSuffixOrPrefix)
                    SuggestedFix = None
                    TypeChecks = List.empty
                }
        | TaskFunc(range, baseName) ->
            Array.singleton
                {
                    Range = range
                    Message = sprintf "Create %s that just calls async { return Async.AwaitTask (BarAsync()) }" (asyncSuffixOrPrefix + baseName)
                    SuggestedFix = None
                    TypeChecks = List.empty
                }

    let processDeclarations (declarations: list<SynModuleDecl>) =
        let bindings = getBindings List.Empty declarations

        let funcs = 
            bindings
            |> List.choose
                (fun binding ->
                    match binding with
                    | SynBinding(_, _, _, _, _, _, _, SynPat.LongIdent(funcIdent, _, _, _, (None | Some(SynAccess.Public _)), _), _, _, _, _, _) ->
                        match funcIdent with
                        | HasAsyncPrefix name ->
                            Some <| AsyncFunc(funcIdent.Range, name.Substring asyncSuffixOrPrefix.Length)
                        | HasAsyncSuffix name ->
                            Some <| TaskFunc(funcIdent.Range, name.Substring(0, name.Length - asyncSuffixOrPrefix.Length))
                        | HasNoAsyncPrefixOrSuffix _ ->
                            None
                    | _ -> None)

        let asyncFuncs = funcs |> List.filter (fun func -> func.IsAsyncFunc)
        let taskFuncs = funcs |> List.filter (fun func -> func.IsTaskFunc)
        
        let checkFuncs (targetFuncs: list<Func>) (otherFuncs: list<Func>) =
            targetFuncs
            |> List.map
                (fun func ->
                    if otherFuncs |> List.exists (fun otherFunc -> otherFunc.BaseName = func.BaseName) then
                        Array.empty
                    else
                        emitWarning func)
            |> Array.concat
        
        Array.append (checkFuncs asyncFuncs taskFuncs) (checkFuncs taskFuncs asyncFuncs)

    match args.AstNode with
    | Ast.ModuleOrNamespace(SynModuleOrNamespace(_, _, _, declarations, _, _, _, _, _)) ->
        processDeclarations declarations
    | ModuleDeclaration(SynModuleDecl.NestedModule(_, _, declarations, _, _, _)) ->
        processDeclarations declarations
    | _ -> Array.empty

let rule =
    AstNodeRule
        {
            Name = "SimpleAsyncComplementaryHelpers"
            Identifier = Identifiers.SimpleAsyncComplementaryHelpers
            RuleConfig =
                {
                    AstNodeRuleConfig.Runner = runner
                    Cleanup = ignore
                }
        }
