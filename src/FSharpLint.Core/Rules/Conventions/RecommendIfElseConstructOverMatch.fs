module FSharpLint.Rules.RecommendIfElseConstructOverMatch

open FSharpLint.Framework
open FSharpLint.Framework.Suggestion
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Compiler.CodeAnalysis
open FSharpLint.Framework.Ast
open FSharpLint.Framework.Rules

let runner args =
    let isBasicControlFlow (synMatchClauses: List<SynMatchClause>) =
        let isDiscriminatedUnionCase (names: list<string>) (symbolUseRange: range) =
            match args.CheckInfo with
            | Some fileCheckInfo -> 
                let symbolUse =
                    fileCheckInfo.GetSymbolUseAtLocation(
                        symbolUseRange.StartLine,
                        symbolUseRange.EndColumn,
                        args.Lines.[symbolUseRange.StartLine],
                        names)
                match symbolUse with
                | Some(usage) ->
                    match usage.Symbol with
                    | :? FSharp.Compiler.Symbols.FSharpUnionCase -> true
                    | _ -> false
                | _ -> false
            | None -> false
    
        let isFirstClauseTarget firstClause =
            match firstClause with
            | SynMatchClause(SynPat.Const(_), _, _, _, _, _) -> true
            | SynMatchClause(SynPat.Named(SynIdent.SynIdent(ident, _), _, _, range), None, _, _, _, _) -> 
                not <| isDiscriminatedUnionCase (List.singleton ident.idText) range
            | SynMatchClause(SynPat.LongIdent(SynLongIdent(ident, _, _), _, _, SynArgPats.Pats [], _, range), _, _, _, _, _) -> 
                not <| isDiscriminatedUnionCase (ident |> List.map (fun id -> id.idText)) range
            | _ -> false

        let isLastClauseTarget lastClause =
            match lastClause with
            | SynMatchClause(SynPat.Wild _, None, _, _, _, _) -> true
            | _ -> false

        match synMatchClauses with
        | [ firstClause; lastClause ] -> isFirstClauseTarget firstClause && isLastClauseTarget lastClause
        | _ -> false

    match args.AstNode with
    | AstNode.Expression(SynExpr.Match (_, _, synMatchClauses, range, _))
        when isBasicControlFlow synMatchClauses ->
        { Range = range
          Message = Resources.GetString "RecommendIfElseConstructOverMatch"
          SuggestedFix = None
          TypeChecks = List.empty }
        |> Array.singleton
    | _ -> Array.empty

let rule =
    { Name = "RecommendIfElseConstructOverMatch"
      Identifier = Identifiers.RecommendIfElseConstructOverMatch
      RuleConfig = { AstNodeRuleConfig.Runner = runner; Cleanup = ignore } }
    |> AstNodeRule
