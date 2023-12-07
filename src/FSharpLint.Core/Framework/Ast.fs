﻿namespace FSharpLint.Framework

open FSharp.Compiler.Text

/// Used to walk the FSharp Compiler's abstract syntax tree,
/// so that each node can be visited by a list of visitors.
module Ast =

    open FSharp.Compiler.Syntax
    

    /// Nodes in the AST to be visited.
    [<NoEquality; NoComparison>]
    type AstNode =
        | Expression of SynExpr
        | Pattern of SynPat
        | SimplePattern of SynSimplePat
        | SimplePatterns of SynSimplePats
        | ModuleOrNamespace of SynModuleOrNamespace
        | ModuleDeclaration of SynModuleDecl
        | Binding of SynBinding
        | TypeDefinition of SynTypeDefn
        | MemberDefinition of SynMemberDefn
        | ComponentInfo of SynComponentInfo
        | ExceptionRepresentation of SynExceptionDefnRepr
        | UnionCase of SynUnionCase
        | EnumCase of SynEnumCase
        | TypeRepresentation of SynTypeDefnRepr
        | TypeSimpleRepresentation of SynTypeDefnSimpleRepr
        | Type of SynType
        | Field of SynField
        | Match of SynMatchClause
        | ConstructorArguments of SynArgPats
        | TypeParameter of SynTypar
        | InterfaceImplementation of SynInterfaceImpl
        | Identifier of string list * range : range
        | File of ParsedInput
        | LambdaBody of SynExpr
        | LambdaArg of SynSimplePats
        | Else of SynExpr

    /// Concatenates the nested-list structure of `SynAttributes` into a `SynAttribute list` to keep other code
    /// mostly unchanged.
    let extractAttributes (attrs:SynAttributes) =
        attrs
        |> List.collect (fun attrList -> attrList.Attributes)

    /// Extracts an expression from parentheses e.g. ((x + 4)) -> x + 4
    let rec removeParens = function
        | SynExpr.Paren(x, _, _, _) -> removeParens x
        | x -> x

    /// Inlines pipe operators to give a flat function application expression
    /// e.g. `x |> List.map id` to `List.map id x`.
    let (|FuncApp|_|) functionApplication =
        let rec flatten flattened exprToFlatten =
            match exprToFlatten with
            | SynExpr.App(_, _, x, y, _) ->
                match x with
                | SynExpr.App(_, true, SynExpr.Ident(op), rhs, _) as app ->
                    let lhs = y

                    match op.idText with
                    | "op_PipeRight" | "op_PipeRight2" | "op_PipeRight3" ->
                        flatten [rhs] lhs
                    | "op_PipeLeft" | "op_PipeLeft2" | "op_PipeLeft3" ->
                        flatten (lhs::flattened) rhs
                    | _ -> flatten (lhs::flattened) app
                | x ->
                    let leftExpr, rightExpr = (x, y)
                    flatten (rightExpr::flattened) leftExpr
            | expr -> expr::flattened

        match functionApplication with
        | AstNode.Expression(SynExpr.App(_, _, _, _, range) as functionApplication) ->
            Some(flatten [] functionApplication, range)
        | _ -> None

    [<NoEquality; NoComparison>]
    type Lambda = { Arguments:SynSimplePats list; Body:SynExpr }

    let (|Lambda|_|) lambda =
        /// A match clause is generated by the compiler for each wildcard argument,
        /// this function extracts the body expression of the lambda from those statements.
        let rec removeAutoGeneratedMatchesFromLambda = function
            | SynExpr.Match(DebugPointAtBinding.NoneAtInvisible,
                            _,
                            [SynMatchClause(SynPat.Wild(_), _, expr, _, _, _)], _, _) ->
                removeAutoGeneratedMatchesFromLambda expr
            | x -> x

        let (|IsCurriedLambda|_|) = function
            | SynExpr.Lambda(_, _, parameter, (SynExpr.Lambda(_) as inner), _, _, _) as outer
                    when outer.Range = inner.Range ->
                Some(parameter, inner)
            | _ -> None

        let rec getLambdaParametersAndExpression parameters = function
            | IsCurriedLambda(parameter, curriedLambda) ->
                getLambdaParametersAndExpression (parameter::parameters) curriedLambda
            | SynExpr.Lambda(_, _, parameter, body, _, _, _) ->
                { Arguments = parameter::parameters |> List.rev
                  Body = removeAutoGeneratedMatchesFromLambda body } |> Some
            | _ -> None

        match lambda with
        | AstNode.Expression(SynExpr.Lambda(_, _, _, _, _, range, _) as lambda) ->
            getLambdaParametersAndExpression [] lambda
            |> Option.map (fun x -> (x, range))
        | _ -> None

    let (|Cons|_|) pattern =
        match pattern with
        | SynPat.LongIdent(SynLongIdent([identifier], _, _),
                           _, _,
                           SynArgPats.Pats([SynPat.Tuple(_, [lhs; rhs], _)]), _, _)
                when identifier.idText = "op_ColonColon" ->
            Some(lhs, rhs)
        | _ -> None

    /// Gets a string literal from the AST.
    let (|StringLiteral|_|) node =
        match node with
        | Expression(SynExpr.Const(SynConst.String(value, _, _), range)) -> Some(value, range)
        | _ -> None

    module List =
        let inline revIter f items =
            items |> List.rev |> List.iter f

    let inline private moduleDeclarationChildren node add =
        match node with
        | SynModuleDecl.NestedModule(componentInfo, _, moduleDeclarations, _, _, _) ->
            moduleDeclarations |> List.revIter (ModuleDeclaration >> add)
            add <| ComponentInfo componentInfo
        | SynModuleDecl.Let(_, bindings, _) -> bindings |> List.revIter (Binding >> add)
        | SynModuleDecl.Expr(expression, _) -> add <| Expression expression // inner do?
        | SynModuleDecl.Types(typeDefinitions, _) -> typeDefinitions |> List.revIter (TypeDefinition >> add)
        | SynModuleDecl.Exception(SynExceptionDefn.SynExceptionDefn(repr, _, members, _), _) ->
            members |> List.revIter (MemberDefinition >> add)
            add <| ExceptionRepresentation repr
        | SynModuleDecl.NamespaceFragment(moduleOrNamespace) -> add <| ModuleOrNamespace moduleOrNamespace
        | SynModuleDecl.Open(_)
        | SynModuleDecl.Attributes(_)
        | SynModuleDecl.HashDirective(_)
        | SynModuleDecl.ModuleAbbrev(_) -> ()

    let inline private typeChildren node add =
        match node with
        | SynType.LongIdentApp(synType, _, _, types, _, _, _)
        | SynType.App(synType, _, types, _, _, _, _) ->
            types |> List.revIter (Type >> add)
            add <| Type synType
        | SynType.Tuple(_, types, _) ->
            types |> List.revIter (snd >> Type >> add)
        | SynType.Fun(synType, synType1, _, _)
        | SynType.StaticConstantNamed(synType, synType1, _)
        | SynType.MeasureDivide(synType, synType1, _) ->
            add <| Type synType1
            add <| Type synType
        | SynType.Var(_)
        | SynType.Anon(_)
        | SynType.LongIdent(_)
        | SynType.StaticConstant(_) -> ()
        | SynType.WithGlobalConstraints(synType, _, _)
        | SynType.HashConstraint(synType, _)
        | SynType.MeasurePower(synType, _, _)
        | SynType.Array(_, synType, _) -> add <| Type synType
        | SynType.StaticConstantExpr(expression, _) -> add <| Expression expression
        | SynType.AnonRecd (_, typeNames, _) ->
            typeNames |> List.revIter (snd >> Type >> add)
        | SynType.Paren(innerType, _) ->
            add <| Type innerType

    /// Concatenates the typed-or-untyped structure of `SynSimplePats` into a `SynSimplePat list` to keep other code
    /// mostly unchanged.
    let inline extractPatterns (simplePats:SynSimplePats) =
        let rec loop pat acc =
            match pat with
            | SynSimplePats.SimplePats(patterns, _range) -> patterns @ acc
            | SynSimplePats.Typed(patterns, _type, _range) -> loop patterns acc
        loop simplePats []

    let inline private memberDefinitionChildren node add =
        match node with
        | SynMemberDefn.Member(binding, _) -> add <| Binding binding
        | SynMemberDefn.ImplicitCtor(_, _, patterns, _, _, _) ->
            let combinedPatterns = extractPatterns patterns
            combinedPatterns |> List.revIter (SimplePattern >> add)
        | SynMemberDefn.ImplicitInherit(synType, expression, _, _) ->
            add <| Expression expression
            add <| Type synType
        | SynMemberDefn.LetBindings(bindings, _, _, _) -> bindings |> List.revIter (Binding >> add)
        | SynMemberDefn.Interface(synType, _, Some(members), _) ->
            members |> List.revIter (MemberDefinition >> add)
            add <| Type synType
        | SynMemberDefn.Interface(synType, _, None, _)
        | SynMemberDefn.Inherit(synType, _, _) -> add <| Type synType
        | SynMemberDefn.Open(_)
        | SynMemberDefn.AbstractSlot(_) -> ()
        | SynMemberDefn.ValField(field, _) -> add <| Field field
        | SynMemberDefn.NestedType(typeDefinition, _, _) -> add <| TypeDefinition typeDefinition
        | SynMemberDefn.AutoProperty(_, _, _, Some(synType), _, _, _, _, _, expression, _, _, _) ->
            add <| Expression expression
            add <| Type synType
        | SynMemberDefn.AutoProperty(_, _, _, None, _, _, _, _, _, expression, _, _, _) ->
            add <| Expression expression
        | SynMemberDefn.GetSetMember(_) ->
            failwith "GetSetMember case not implemented" // ???

    let inline private patternChildren node add =
        match node with
        | SynPat.IsInst(synType, _) -> add <| Type synType
        | SynPat.QuoteExpr(expression, _) -> add <| Expression expression
        | SynPat.Typed(pattern, synType, _) ->
            add <| Type synType
            add <| Pattern pattern
        | SynPat.Or(pattern, pattern1, _, _) ->
            add <| Pattern pattern1
            add <| Pattern pattern
        | SynPat.ArrayOrList(_, patterns, _)
        | SynPat.Tuple(_, patterns, _)
        | SynPat.Ands(patterns, _) -> patterns |> List.revIter (Pattern >> add)
        | SynPat.Attrib(pattern, _, _)
        | SynPat.Paren(pattern, _) -> add <| Pattern pattern
        | SynPat.Named(_, _, _, _) -> () // ???
        | SynPat.Record(patternsAndIdentifier, _) -> patternsAndIdentifier |> List.revIter (fun (_, _, pattern) -> pattern |> Pattern |> add)
        | SynPat.Const(_)
        | SynPat.Wild(_)
        | SynPat.FromParseError(_)
        | SynPat.InstanceMember(_)
        | SynPat.DeprecatedCharRange(_)
        | SynPat.Null(_)
        | SynPat.OptionalVal(_) -> ()
        | Cons(lhs, rhs) ->
            add <| Pattern rhs
            add <| Pattern lhs
        | SynPat.LongIdent(_, _, _, constructorArguments, _, _) ->
            add <| ConstructorArguments constructorArguments
        | x -> failwithf "Unexpected SynPat case: %A" x // ???

    let inline private expressionChildren node add =
        match node with
        | SynExpr.Paren(expression, _, _, _)
        | SynExpr.DotGet(expression, _, _, _)
        | SynExpr.DotIndexedGet(expression, _, _, _)
        | SynExpr.LongIdentSet(_, expression, _)
        | SynExpr.Do(expression, _)
        | SynExpr.Assert(expression, _)
        | SynExpr.ComputationExpr(_, expression, _)
        | SynExpr.ArrayOrListComputed(_, expression, _)
        | SynExpr.AddressOf(_, expression, _, _)
        | SynExpr.InferredDowncast(expression, _)
        | SynExpr.InferredUpcast(expression, _)
        | SynExpr.DoBang(expression, _)
        | SynExpr.Lazy(expression, _)
        | SynExpr.TraitCall(_, _, expression, _)
        | SynExpr.YieldOrReturn(_, expression, _)
        | SynExpr.YieldOrReturnFrom(_, expression, _) -> add <| Expression expression
        | SynExpr.SequentialOrImplicitYield(_, expression1, expression2, ifNotExpression, _) ->
            add <| Expression expression1
            add <| Expression expression2
            add <| Expression ifNotExpression
        | SynExpr.Quote(expression, _, expression1, _, _)
        | SynExpr.Sequential(_, _, expression, expression1, _)
        | SynExpr.NamedIndexedPropertySet(_, expression, expression1, _)
        | SynExpr.DotIndexedSet(expression, _, expression1, _, _, _)
        | SynExpr.JoinIn(expression, _, expression1, _)
        | SynExpr.While(_, expression, expression1, _)
        | SynExpr.TryFinally(expression, expression1, _, _, _, _)
        | SynExpr.Set(expression, expression1, _)
        | SynExpr.DotSet(expression, _, expression1, _) ->
            add <| Expression expression1
            add <| Expression expression
        | SynExpr.Typed(expression, synType, _) ->
            add <| Type synType
            add <| Expression expression
        | SynExpr.Tuple(_, expressions, _, _)
        | SynExpr.ArrayOrList(_, expressions, _) -> expressions |> List.revIter (Expression >> add)
        | SynExpr.Record(_, Some(expr, _), _, _) -> add <| Expression expr
        | SynExpr.Record(_, None, _, _) -> ()
        | SynExpr.AnonRecd(_, Some (expr,_), _, _) ->
            add <| Expression expr
        | SynExpr.AnonRecd(_, None, _, _) -> ()
        | SynExpr.ObjExpr(synType, _, _, bindings, _, _, _, _) ->
            bindings |> List.revIter (Binding >> add)
            add <| Type synType
        | SynExpr.ImplicitZero(_)
        | SynExpr.Null(_)
        | SynExpr.Const(_)
        | SynExpr.DiscardAfterMissingQualificationAfterDot(_)
        | SynExpr.FromParseError(_)
        | SynExpr.LibraryOnlyILAssembly(_)
        | SynExpr.LibraryOnlyStaticOptimization(_)
        | SynExpr.LibraryOnlyUnionCaseFieldGet(_)
        | SynExpr.LibraryOnlyUnionCaseFieldSet(_)
        | SynExpr.ArbitraryAfterError(_) -> ()
        | SynExpr.DotNamedIndexedPropertySet(expression, _, expression1, expression2, _)
        | SynExpr.For(_, _, _, _, expression, _, expression1, expression2, _) ->
            add <| Expression expression2
            add <| Expression expression1
            add <| Expression expression
        | SynExpr.LetOrUseBang(_, _, _, pattern, rightHandSide, andBangs, leftHandSide, _, _) ->
            add <| Expression rightHandSide
            add <| Expression leftHandSide
            // TODO: is the the correct way to handle the new `and!` syntax?
            andBangs |> List.iter (fun (SynExprAndBang(_, _, _, pattern, body, _, _)) ->
                add <| Expression body
                add <| Pattern pattern
            )
            add <| Pattern pattern
        | SynExpr.ForEach(_, _, _, _, pattern, expression, expression1, _) ->
            add <| Expression expression1
            add <| Expression expression
            add <| Pattern pattern
        | SynExpr.MatchLambda(_, _, matchClauses, _, _) ->
            matchClauses |> List.revIter (Match >> add)
        | SynExpr.TryWith(expression, matchClauses, _, _, _, _)
        | SynExpr.MatchBang(_, expression, matchClauses, _, _)
        | SynExpr.Match(_, expression, matchClauses, _, _) ->
            matchClauses |> List.revIter (Match >> add)
            add <| Expression expression
        | SynExpr.TypeApp(expression, _, types, _, _, _, _) ->
            types |> List.revIter (Type >> add)
            add <| Expression expression
        | SynExpr.New(_, synType, expression, _)
        | SynExpr.TypeTest(expression, synType, _)
        | SynExpr.Upcast(expression, synType, _)
        | SynExpr.Downcast(expression, synType, _) ->
            add <| Type synType
            add <| Expression expression
        | SynExpr.LetOrUse(_, _, bindings, expression, _, _) ->
            add <| Expression expression
            bindings |> List.revIter (Binding >> add)
        | SynExpr.Ident(ident) -> add <| Identifier([ident.idText], ident.idRange)
        | SynExpr.LongIdent(_, SynLongIdent(ident, _, _), _, range) ->
            add <| Identifier(ident |> List.map (fun x -> x.idText), range)
        | SynExpr.IfThenElse(cond, body, Some(elseExpr), _, _, _, _) ->
            add <| Else elseExpr
            add <| Expression body
            add <| Expression cond
        | SynExpr.IfThenElse(cond, body, None, _, _, _, _) ->
            add <| Expression body
            add <| Expression cond
        | SynExpr.InterpolatedString(contents, _, range) -> 
            contents
            |> List.iter (
                function
                    | SynInterpolatedStringPart.String _ ->
                        ()
                    | SynInterpolatedStringPart.FillExpr (expr, _ident) ->
                        add <| Expression expr
            )
        | SynExpr.Lambda(_)
        | SynExpr.App(_)
        | SynExpr.Fixed(_) -> ()
        | SynExpr.DebugPoint(_) -> () // ???
        | SynExpr.Dynamic(_) -> () // ???
        | SynExpr.IndexFromEnd(_) -> () // ???
        | SynExpr.IndexRange(_) -> () // ???

    let inline private typeSimpleRepresentationChildren node add =
        match node with
        | SynTypeDefnSimpleRepr.Union(_, unionCases, _) -> unionCases |> List.revIter (UnionCase >> add)
        | SynTypeDefnSimpleRepr.Enum(enumCases, _) -> enumCases |> List.revIter (EnumCase >> add)
        | SynTypeDefnSimpleRepr.Record(_, fields, _) -> fields |> List.revIter (Field >> add)
        | SynTypeDefnSimpleRepr.TypeAbbrev(_, synType, _) -> add <| Type synType
        | SynTypeDefnSimpleRepr.Exception(exceptionRepr) -> add <| ExceptionRepresentation exceptionRepr
        | SynTypeDefnSimpleRepr.General(_)
        | SynTypeDefnSimpleRepr.LibraryOnlyILAssembly(_)
        | SynTypeDefnSimpleRepr.None(_) -> ()

    let inline private simplePatternsChildren node add =
        match node with
        | SynSimplePats.SimplePats(simplePatterns, _) ->
            simplePatterns |> List.revIter (SimplePattern >> add)
        | SynSimplePats.Typed(simplePatterns, synType, _) ->
            add <| Type synType
            add <| SimplePatterns simplePatterns

    let inline private simplePatternChildren node add =
        match node with
        | SynSimplePat.Typed(simplePattern, synType, _) ->
            add <| Type synType
            add <| SimplePattern simplePattern
        | SynSimplePat.Attrib(simplePattern, _, _) -> add <| SimplePattern simplePattern
        | SynSimplePat.Id(identifier, _, _, _, _, _) -> add <| Identifier([identifier.idText], identifier.idRange)

    let inline private matchChildren node add =
        match node with
        | SynMatchClause(pattern, Some(expression), expression1, _, _, _) ->
            add <| Expression expression1
            add <| Expression expression
            add <| Pattern pattern
        | SynMatchClause(pattern, None, expression1, _, _, _) ->
            add <| Expression expression1
            add <| Pattern pattern

    let inline private constructorArgumentsChildren node add =
        match node with
        | SynArgPats.Pats(patterns) ->
            patterns |> List.revIter (Pattern >> add)
        | SynArgPats.NamePatPairs(namePatterns, _) ->
            namePatterns |> List.revIter (fun (_, _, pattern) -> pattern |> Pattern |> add)

    let inline private typeRepresentationChildren node add =
        match node with
        | SynTypeDefnRepr.ObjectModel(_, members, _) ->
            members |> List.revIter (MemberDefinition >> add)
        | SynTypeDefnRepr.Simple(typeSimpleRepresentation, _) ->
            add <| TypeSimpleRepresentation typeSimpleRepresentation
        | SynTypeDefnRepr.Exception(exceptionRepr) ->
            add <| ExceptionRepresentation exceptionRepr

    let inline private unionCaseChildren node add =
        match node with
        | SynUnionCase(caseType=(SynUnionCaseKind.Fields fields)) ->
            fields |> List.revIter (Field >> add)
        | SynUnionCase(caseType=(SynUnionCaseKind.FullType (fullType, _))) ->
            add (Type fullType)

    /// Extracts the child nodes to be visited from a given node.
    let traverseNode node add =
        match node with
        | ModuleDeclaration(x) -> moduleDeclarationChildren x add
        | ModuleOrNamespace(SynModuleOrNamespace(_, _, _, moduleDeclarations, _, _, _, _, _)) ->
            moduleDeclarations |> List.revIter (ModuleDeclaration >> add)
        | Binding(SynBinding(_, _, _, _, _, _, _, pattern, _, expression, _, _, _)) ->
            add <| Expression expression
            add <| Pattern pattern
        | ExceptionRepresentation(SynExceptionDefnRepr.SynExceptionDefnRepr(_, unionCase, _, _, _, _)) ->
            add <| UnionCase unionCase
        | TypeDefinition(SynTypeDefn(componentInfo, typeRepresentation, members, implicitCtor, _, _)) ->
            implicitCtor |> Option.iter (MemberDefinition >> add)
            members |> List.revIter (MemberDefinition >> add)
            add <| TypeRepresentation typeRepresentation
            add <| ComponentInfo componentInfo
        | TypeSimpleRepresentation(x) -> typeSimpleRepresentationChildren x add
        | Type(x) -> typeChildren x add
        | Match(x) -> matchChildren x add
        | MemberDefinition(x) -> memberDefinitionChildren x add
        | Field(SynField(_, _, _, synType, _, _, _, _)) -> add <| Type synType
        | Pattern(x) -> patternChildren x add
        | ConstructorArguments(x) -> constructorArgumentsChildren x add
        | SimplePattern(x) -> simplePatternChildren x add
        | LambdaArg(x)
        | SimplePatterns(x) -> simplePatternsChildren x add
        | InterfaceImplementation(SynInterfaceImpl(synType, _, bindings, _, _)) ->
            bindings |> List.revIter (Binding >> add)
            add <| Type synType
        | TypeRepresentation(x) -> typeRepresentationChildren x add
        | FuncApp(exprs, _) -> exprs |> List.revIter (Expression >> add)
        | Lambda({ Arguments = args; Body = body }, _) ->
            add <| LambdaBody(body)
            args |> List.revIter (fun arg -> add <| LambdaArg arg)
            
        | LambdaBody(x)
        | Else(x)
        | Expression(x) -> expressionChildren x add

        | File(ParsedInput.ImplFile(ParsedImplFileInput(_, _, _, _, _, moduleOrNamespaces, _, _))) ->
            moduleOrNamespaces |> List.revIter (ModuleOrNamespace >> add)

        | UnionCase(x) -> unionCaseChildren x add
        | File(ParsedInput.SigFile(_))
        | ComponentInfo(_)
        | EnumCase(_)
        | Identifier(_)
        | TypeParameter(_) -> ()
