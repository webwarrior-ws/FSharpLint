module FSharpLint.Core.Tests.Rules.Conventions.SourceLength

open System
open NUnit.Framework
open FSharpLint.Rules
open FSharpLint.Rules.Helper.SourceLength
open FSharpLint.Core.Tests

let generateNewLines numNewLines numIndents =
    Array.mapi
        (fun index _ ->
            let indentationChars =
                if index = 0 then
                    String.Empty
                else
                    String.replicate numIndents " "
            $"{indentationChars}printf System.String.Empty\n")
            (Array.create numNewLines "")
    |> String.concat ""

let generateAbstractMembers numMembers numIndents =
    Array.init numMembers (fun index -> $"abstract member Foo%i{index} : unit -> unit\n")
    |> String.concat (String.replicate numIndents " ")

[<Literal>]
let FunctionLength = 70
[<TestFixture>]
type TestMaxLinesInFunction() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(MaxLinesInFunction.rule { Config.MaxLines = FunctionLength })

    [<Test>]
    member this.FunctionTooManyLines() =
        this.Parse($"""
module Program

let dog x =
    %s{generateNewLines FunctionLength 4}
    ()""")
        Assert.IsTrue(this.ErrorExistsAt(4, 4))

    [<Test>]
    member this.FunctionNotTooManyLines() =
        this.Parse($"""
module Program

let dog x =
    %s{generateNewLines (FunctionLength - 4) 4}
    ()""")
        Assert.IsFalse(this.ErrorExistsAt(4, 4))

    [<Test>]
    member this.FunctionTooManyLinesWithComment() =
        this.Parse($"""
module Program

let dog x =
    // Foo
    // Bar
    // Buzz
    %s{generateNewLines (FunctionLength - 3) 4}
    ()""")
        Assert.IsFalse this.ErrorsExist

    [<Test>]
    member this.FunctionTooManyLinesWithMultiLineComment() =
        this.Parse($"""
module Program

let dog x =
    (*
    Foo
    Bar
    *)
    %s{generateNewLines (FunctionLength - 4) 4}
    ()""")
        Assert.IsFalse this.ErrorsExist

    [<Test>]
    member this.FunctionTooManyLinesWithNestsedMultiLineComment() =
        this.Parse($"""
module Program

let dog x =
    (*
    Foo (* baz *)
    let (*) = id
    Bar
    *)
    let (*) a b = a + b
    %s{generateNewLines (FunctionLength - 5) 4}
    ()""")
        Assert.IsFalse this.ErrorsExist

[<Literal>]
let LambdaFunctionLength = 5
[<TestFixture>]
type TestMaxLinesInLambdaFunction() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(MaxLinesInLambdaFunction.rule { Config.MaxLines = LambdaFunctionLength })

    [<Test>]
    member this.LambdaFunctionTooManyLines() =
        this.Parse($"""
module Program

let dog = fun x ->
    match x with
    | Some(x) ->
        %s{generateNewLines LambdaFunctionLength 8}
        ()
    | None -> ()
        """)
        Assert.IsTrue(this.ErrorExistsAt(4, 10))

    [<Test>]
    member this.``Multiple arguments in a lamba should not be treated as separate lambdas.``() =
        this.Parse($"""
module Program

let dog = fun x y ->
    match x with
    | Some(x) ->
        %s{generateNewLines LambdaFunctionLength 8}
        ()
    | None -> ()
        """)

        Assert.AreEqual(1, Seq.length <| this.ErrorsAt(4, 10))

    [<Test>]
    member this.LambdaFunctionNotTooManyLines() =
        this.Parse """
module Program

let dog = fun x ->
    match x with
    | Some(x) ->
        ()
    | None -> ()
"""

        Assert.IsFalse(this.ErrorExistsAt(4, 10))

[<Literal>]
let MatchLambdaFunctionLength = 70
[<TestFixture>]
type TestMaxLinesInMatchLambdaFunction() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(MaxLinesInMatchLambdaFunction.rule { Config.MaxLines = MatchLambdaFunctionLength })

    [<Test>]
    member this.MatchFunctionTooManyLines() =
        this.Parse($"""
module Program

let dog = function
| Some(x) ->
    %s{generateNewLines MatchLambdaFunctionLength 4}
    ()
| None -> ()""")
        Assert.IsTrue(this.ErrorExistsAt(4, 10))

    [<Test>]
    member this.MatchFunctionNotTooManyLines() =
        this.Parse($"""
module Program

let dog = function
| Some(x) ->
    %s{generateNewLines (MatchLambdaFunctionLength - 5) 4}
    ()
| None -> ()""")
        Assert.IsFalse(this.ErrorExistsAt(4, 4))

[<Literal>]
let ValueLength = 70
[<TestFixture>]
type TestMaxLinesInValue() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(MaxLinesInValue.rule { Config.MaxLines = ValueLength })

    [<Test>]
    member this.ValueTooManyLines() =
        this.Parse($"""
module Program

let dog =
    %s{generateNewLines ValueLength 4}
    ()""")
        Assert.IsTrue(this.ErrorExistsAt(4, 4))

    [<Test>]
    member this.ValueNotTooManyLines() =
        this.Parse($"""
module Program

let dog =
    %s{generateNewLines (ValueLength - 4) 4}
    ()""")
        Assert.IsFalse(this.ErrorExistsAt(4, 4))

[<Literal>]
let ConstructorLength = 70
[<TestFixture>]
type TestMaxLinesInConstructor() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(MaxLinesInConstructor.rule { Config.MaxLines = ConstructorLength })

    [<Test>]
    member this.ConstructorTooManyLines() =
        this.Parse($"""
module Program

type MyClass(x) =
    new() =
        %s{generateNewLines ConstructorLength 8}
        MyClass(0)
      """)
        Assert.IsTrue(this.ErrorExistsAt(5, 4))

    [<Test>]
    member this.ConstructorNotTooManyLines() =
        this.Parse """
module Program

type MyClass(x) =
    new() = MyClass(0)
"""

        Assert.IsFalse(this.ErrorExistsAt(5, 4))

[<Literal>]
let MemberLength = 70
[<TestFixture>]
type TestMaxLinesInMember() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(MaxLinesInMember.rule { Config.MaxLines = MemberLength })
    // TODO: Add tests.

[<Literal>]
let PropertyLength = 70
[<TestFixture>]
type TestMaxLinesInProperty() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(MaxLinesInProperty.rule { Config.MaxLines = PropertyLength })

    [<Test>]
    member this.PropertyNotTooManyLines() =
        this.Parse """
module Program

type Class() =
    let mutable value = 10
    member this.Property1 with get() =
        value
"""
        Assert.IsFalse(this.ErrorExistsAt(6, 31))

[<Literal>]
let ClassLength = 500
[<TestFixture>]
type TestMaxLinesInClass() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(MaxLinesInClass.rule { Config.MaxLines = ClassLength })

    [<Test>]
    member this.ClassTooManyLines() =
        this.Parse($"""
module Program

type MyClass2() as this =
    do
        %s{generateNewLines ClassLength 8}
    member this.PrintMessage() = ()""")
        Assert.IsTrue(this.ErrorExistsAt(4, 5))

    [<Test>]
    member this.ClassNotTooManyLines() =
        this.Parse """
module Program

type MyClass2() as this =
    member this.PrintMessage() = ()
"""

        Assert.IsFalse(this.ErrorExistsAt(4, 5))

    [<Test>]
    member this.InterfaceTooManyLines() =
        this.Parse($"""
module Program

type IPrintable =
    %s{generateAbstractMembers ClassLength 4}
    abstract member Print : unit -> unit""")

        Assert.IsTrue(this.ErrorExistsAt(4, 5))

    [<Test>]
    member this.InterfaceNotTooManyLines() =
        this.Parse """
module Program

type IPrintable =
    abstract member Print : unit -> unit
"""

        Assert.IsFalse(this.ErrorExistsAt(4, 5))

[<Literal>]
let UnionLength = 500
[<TestFixture>]
type TestMaxLinesInUnion() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(MaxLinesInUnion.rule { Config.MaxLines = UnionLength })
    // TODO: Add tests.

[<Literal>]
let RecordLength = 500
[<TestFixture>]
type TestMaxLinesInRecord() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(MaxLinesInRecord.rule { Config.MaxLines = RecordLength })

    [<Test>]
    member this.RecordTooManyLines() =
        this.Parse($"""
module Program

type Record =
    {{
        %s{generateNewLines RecordLength 8}
        dog: int
    }}""")
        Assert.IsTrue(this.ErrorExistsAt(4, 5))

    [<Test>]
    member this.RecordNotTooManyLines() =
        this.Parse """
module Program

type Record = { dog: int }
"""

        Assert.IsFalse(this.ErrorExistsAt(4, 5))

[<Literal>]
let EnumLength = 1000
[<TestFixture>]
type TestMaxLinesInEnum() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(MaxLinesInEnum.rule { Config.MaxLines = EnumLength })
    // TODO: Add tests.

[<Literal>]
let ModuleLength = 1000
[<TestFixture>]
type TestMaxLinesInModule() =
    inherit TestAstNodeRuleBase.TestAstNodeRuleBase(MaxLinesInModule.rule { Config.MaxLines = ModuleLength })

    [<Test>]
    member this.ModuleTooManyLines() =
        this.Parse($"""
module Program
{generateNewLines ModuleLength 0}
let foo = ""
exception SomeException of string""")
        Assert.IsTrue(this.ErrorExistsAt(2, 0))

    [<Test>]
    member this.ModuleNotTooManyLines() =
        this.Parse($"""
module Program
{generateNewLines (ModuleLength - 4) 0}
let foo = ""
exception SomeException of string""")
        Assert.IsFalse(this.ErrorExistsAt(2, 0))
