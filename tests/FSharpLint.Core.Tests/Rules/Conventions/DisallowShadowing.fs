module FSharpLint.Core.Tests.Rules.Conventions.DisallowShadowing

open NUnit.Framework

open FSharpLint.Rules

[<TestFixture>]
type TestConventionsDisallowShadowing() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(DisallowShadowing.rule)

    [<Test>]
    member this.``Should produce error for shadowed variable``() =
        this.Parse """
let foo = 0
let foo = 1"""

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.``Should produce error for shadowed variable (function argument)``() =
        this.Parse """
let foo = 0
let bar foo = foo + 1"""

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.``Should produce error for shadowed variable (lambda function argument)``() =
        this.Parse """
let foo = 0
(fun foo -> foo + 1) 0"""

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.``Should produce error for shadowed variable (match pattern)``() =
        this.Parse """
let foo = 0
match 1 with
| foo -> foo"""

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.``Should produce error for shadowed variable (as match pattern)``() =
        this.Parse """
let foo = 0
match (1, 2) with
| (x, y) as foo -> foo"""

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.``Should not produce error when variable with same name exists in another module``() =
        this.Parse """
module Foo =
    let foo = 0
let foo = 1"""

        Assert.IsTrue this.NoErrorsExist
