module FSharpLint.Core.Tests.Rules.Conventions.UnneededRecKeyword

open NUnit.Framework
open FSharpLint.Framework.Rules
open FSharpLint.Rules

[<TestFixture>]
type TestConventionsUnneededRecKeyword() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(UnneededRecKeyword.rule)

    [<Test>]
    member this.UnneededRecKeywordShouldNotProduceError() =
        this.Parse """
let rec Foo () =
    if someParam then
        Foo()
    else
        ()"""

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.UnneededRecKeywordShouldProduceError_1() =
        this.Parse """
let rec Foo someParam =
    ()"""

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.UnneededRecKeywordShouldProduceError_2() =
        this.Parse """
let rec Foo someParam =
    ()

[<EntryPoint>]
let main args =
    let Foo () =
        ()

    Foo()
    0"""

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.``UnneededRecKeyword should produce error for functions with rec ... and that aren't mutually recursive``() =
        this.Parse """
let rec Foo someParam =
    ()
and Bar someParam =
    Foo ()"""

        Assert.IsTrue <| this.ErrorExistsAt(4, 4)

    [<Test>]
    member this.``UnneededRecKeyword should not produce error for functions with rec ... and that are mutually recursive``() =
        this.Parse """
let rec Foo someParam =
    Bar ()
and Bar someParam =
    Foo ()"""

        Assert.IsTrue this.NoErrorsExist

    [<Test>]
    member this.``UnneededRecKeyword should not produce error for functions with rec ... and that are recursive``() =
        this.Parse """
let rec Foo someParam =
    Foo (someParam + 1)
and Bar someParam =
    Bar (someParam + 1)"""

        Assert.IsTrue this.NoErrorsExist
