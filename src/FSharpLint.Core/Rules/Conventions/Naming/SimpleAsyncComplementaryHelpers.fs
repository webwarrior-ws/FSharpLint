module FSharpLint.Rules.SimpleAsyncComplementaryHelpers

open System
open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules
open Helper.Naming.Asynchronous
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text

type private ReturnType =
    | Async of typeParam: Option<SynType>
    | Task of typeParam: Option<SynType>

type private Func =
    {
        BaseName: string
        Range: range
        ReturnType: ReturnType
    }

[<TailCall>]
let rec private getBindings (acc: list<SynBinding>) (declarations: list<SynModuleDecl>) =
    match declarations with
    | SynModuleDecl.Let(_, bindings, _) :: rest -> getBindings (acc @ bindings) rest
    | SynModuleDecl.NestedModule(_, _, innerDecls, _, _, _) :: rest -> getBindings acc (innerDecls @ rest)
    | [] -> acc
    | _ :: rest -> getBindings acc rest

let runner (args: AstNodeRuleParams) =
    let emitWarning (func: Func) =
        let funcDefinitionString = 
            match ExpressionUtilities.tryFindTextOfRange func.Range args.FileContent with
            | Some text -> text
            | None -> failwithf "Invalid range: %A" func.Range

        let tryGetTypeParamString (maybeTypeParam: Option<SynType>) =
            match maybeTypeParam with
            | Some typeParam -> ExpressionUtilities.tryFindTextOfRange typeParam.Range args.FileContent
            | None -> None

        let message =
            match func.ReturnType with
            | Async(Some(SynType.LongIdent (SynLongIdent ([ unitIdent ], [], [None])))) when unitIdent.idText = "unit" -> 
                let newFuncName = funcDefinitionString.Replace(asyncSuffixOrPrefix + func.BaseName, func.BaseName + asyncSuffixOrPrefix)
                sprintf "Create %s: Task that just calls Async.StartAsTask(AsyncBar())" newFuncName
            | Async typeParam -> 
                let newFuncName = funcDefinitionString.Replace(asyncSuffixOrPrefix + func.BaseName, func.BaseName + asyncSuffixOrPrefix)
                let typeDefString =
                    match tryGetTypeParamString typeParam with
                    | Some str -> $": Task<{str}>"
                    | None -> ""
                sprintf "Create %s%s that just calls Async.StartAsTask(AsyncBar())" newFuncName typeDefString
            | Task typeParam ->
                let newFuncName = funcDefinitionString.Replace(func.BaseName + asyncSuffixOrPrefix, asyncSuffixOrPrefix + func.BaseName)
                let typeDefString =
                    match tryGetTypeParamString typeParam with
                    | Some str -> $": Async<{str}>"
                    | None -> ""
                sprintf "Create %s%s that just calls async { return Async.AwaitTask (BarAsync()) }" newFuncName typeDefString

        Array.singleton
            {
                Range = func.Range
                Message = message
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
                    | SynBinding(_, _, _, _, _, _, _, SynPat.LongIdent(funcIdent, _, _, _, (None | Some(SynAccess.Public _)), _), returnInfo, _, range, _, _) ->
                        let returnTypeParam =
                            match returnInfo with
                            | Some(SynBindingReturnInfo(SynType.App(SynType.LongIdent(SynLongIdent(_, _, _)), _, [ typeParam ], _, _, _, _), _, _, _)) ->
                                Some typeParam 
                            | _ -> None
                        
                        match funcIdent with
                        | HasAsyncPrefix name ->
                            Some
                                { 
                                    BaseName = name.Substring asyncSuffixOrPrefix.Length
                                    Range = range
                                    ReturnType = Async returnTypeParam
                                }
                        | HasAsyncSuffix name ->
                            Some
                                {
                                    BaseName = name.Substring(0, name.Length - asyncSuffixOrPrefix.Length)
                                    Range = range
                                    ReturnType = Task returnTypeParam
                                }
                        | HasNoAsyncPrefixOrSuffix _ ->
                            None
                    | _ -> None)

        let asyncFuncs = funcs |> List.filter (fun func -> func.ReturnType.IsAsync)
        let taskFuncs = funcs |> List.filter (fun func -> func.ReturnType.IsTask)
        
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
