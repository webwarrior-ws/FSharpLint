module FSharpLint.Core.Tests.Rules.Smells.NoAsyncRunSynchronouslyInLibrary

open NUnit.Framework
open FSharpLint.Framework.Rules
open FSharpLint.Rules
open FSharpLint.Rules.NoAsyncRunSynchronouslyInLibrary
open FSharpLint.Rules.Utilities.LibraryHeuristics

[<TestFixture>]
type TestNoAsyncRunSynchronouslyInLibrary() =
    inherit FSharpLint.Core.Tests.TestAstNodeRuleBase.TestAstNodeRuleBase(NoAsyncRunSynchronouslyInLibrary.rule)

    [<Test>]
    member this.``Async.RunSynchronously should not be used in library code``() =
        this.Parse("""
module Program

async {
    return ()
}
|> Async.RunSynchronously""")

        Assert.IsTrue this.ErrorsExist

    [<Test>]
    member this.``Async.RunSynchronously may be used in code that declares entry point``() =
        this.Parse("""
module Program

[<EntryPoint>]
let main () =
    async {
        return ()
    }
    |> Async.RunSynchronously""")

        this.AssertNoWarnings()

    [<Test>]
    member this.``Async.RunSynchronously may be used in code module that has function with entry point``() =
        this.Parse("""
module Program

let foo () =
    async {
        return ()
    }
    |> Async.RunSynchronously

[<EntryPoint>]
let main () =
    0""")

        this.AssertNoWarnings()

    [<Test>]
    member this.``Async.RunSynchronously may be used in NUnit test code``() =
        this.Parse("""
module Program

[<TestFixture>]
type FooTest () =
    [<Test>]
    member this.Foo() =
        async {
            return ()
        }
        |> Async.RunSynchronously""")

        this.AssertNoWarnings()

    [<Test>]
    member this.``Async.RunSynchronously may be used in MSTest test code``() =
        this.Parse("""
module Program

[<TestClass>]
type FooTest () =
    [<TestMethod>]
    member this.Foo() =
        async {
            return ()
        }
        |> Async.RunSynchronously""")

        this.AssertNoWarnings()

    [<Test>]
    member this.``Async.RunSynchronously may be used in module with tests``() =
        this.Parse("""
module Program

let foo () =
    async {
        return ()
    }
    |> Async.RunSynchronously

[<TestClass>]
type FooTest () =
    [<TestMethod>]
    member this.Foo() =
        ()""")

        this.AssertNoWarnings()

    [<Test>]
    member this.``Async.RunSynchronously may be used in methods with Obsolete attribute``() =
        this.Parse("""
module Program

type FooTest () =
    [<Obsolete>]
    member this.Foo() =
        async {
            return ()
        }
        |> Async.RunSynchronously""")

        this.AssertNoWarnings()

    [<Test>]
    member this.``Async.RunSynchronously may be used in functions with Obsolete attribute``() =
        this.Parse("""
module Program

[<Obsolete>]
let Foo() =
    async {
        return ()
    }
    |> Async.RunSynchronously""")

        this.AssertNoWarnings()

[<TestFixture>]
type TestNoAsyncRunSynchronouslyInLibraryHeuristic() =

    [<Test>]
    member this.``Unlikely to be library if contains "tests" in name``() =
        Assert.AreEqual(
            LibraryHeuristicResultByPath.Unlikely,
            howLikelyFileIsInLibrary "/dummy/IntegrationTests"
        )

    [<Test>]
    member this.``Unlikely to be library if contains "testing" in name``() =
        Assert.AreEqual(
            LibraryHeuristicResultByPath.Unlikely,
            howLikelyFileIsInLibrary "/dummy/UnitTesting"
        )

    [<Test>]
    member this.``Unlikely to be library if contains "test" in name``() =
        Assert.AreEqual(
            LibraryHeuristicResultByPath.Unlikely,
            howLikelyFileIsInLibrary "/dummy/TestSuite"
        )

    [<Test>]
    member this.``Unlikely to be library if contains "console" in name``() =
        Assert.AreEqual(
            LibraryHeuristicResultByPath.Unlikely,
            howLikelyFileIsInLibrary "/dummy/FooConsole"
        )

    [<Test>]
    member this.``Likely to be library if contains Contains "Lib" as a PascalCase segment``() =
        Assert.AreEqual(
            LibraryHeuristicResultByPath.Likely,
            howLikelyFileIsInLibrary "/dummy/LibFoo"
        )

    [<Test>]
    member this.``Uncertain if contains contains "Lib" but not as a PascalCase segment``() =
        Assert.AreEqual(
            LibraryHeuristicResultByPath.Uncertain,
            howLikelyFileIsInLibrary "/LibreOfficeProg"
        )

    [<Test>]
    member this.``Likely to be library if contains ends with "lib" (case-insensitive)``() =
        Assert.AreEqual(
            LibraryHeuristicResultByPath.Likely,
            howLikelyFileIsInLibrary "/dummy/FooLib"
        )

    [<Test>]
    member this.``Unlikely to be library if contains "CLI" in name``() =
        Assert.AreEqual(
            LibraryHeuristicResultByPath.Unlikely,
            howLikelyFileIsInLibrary "/dummy/FooCLI"
        )

    [<Test>]
    member this.``Uncertain to be library if contains "cli" in name not related to CLI``() =
        Assert.AreEqual(
            LibraryHeuristicResultByPath.Uncertain,
            howLikelyFileIsInLibrary "/InclinedDriver"
        )

    [<Test>]
    member this.``Unlikely to be library if contains "TUI" in name``() =
        Assert.AreEqual(
            LibraryHeuristicResultByPath.Unlikely,
            howLikelyFileIsInLibrary "/dummy/FooTUI"
        )

    [<Test>]
    member this.``Likely to be library if it starts with "lib", e.g. camelCase``() =
        Assert.AreEqual(
            LibraryHeuristicResultByPath.Likely,
            howLikelyFileIsInLibrary "/dummy/libFoo"
        )

    [<Test>]
    member this.``Unlikely to be library if it contains "console", but segments are separated by dots``() =
        Assert.AreEqual(
            LibraryHeuristicResultByPath.Unlikely,
            howLikelyFileIsInLibrary "/dummy/foo.console.app"
        )

    [<Test>]
    member this.``Unlikely to be library if it contains "console", but segments are separated by dashes``() =
        Assert.AreEqual(
            LibraryHeuristicResultByPath.Unlikely,
            howLikelyFileIsInLibrary "/dummy/foo-console-app"
        )

    [<Test>]
    member this.``Unlikely to be library if it contains "console", but segments are separated by underscores``() =
        Assert.AreEqual(
            LibraryHeuristicResultByPath.Unlikely,
            howLikelyFileIsInLibrary "/dummy/foo_console_app"
        )

    [<Test>]
    member this.``Unlikely to be library if path segment contains "console"``() =
        Assert.AreEqual(
            LibraryHeuristicResultByPath.Unlikely,
            howLikelyFileIsInLibrary "/Console/Foo.fsproj"
        )

    [<Test>]
    member this.``Unlikely to be library if path segment contains "test"``() =
        Assert.AreEqual(
            LibraryHeuristicResultByPath.Unlikely,
            howLikelyFileIsInLibrary "/Tests/Foo.fsproj"
        )

    [<Test>]
    member this.``Unlikely to be library if path segment contains "TUI"``() =
        Assert.AreEqual(
            LibraryHeuristicResultByPath.Unlikely,
            howLikelyFileIsInLibrary "/FooTUI/Foo.fsproj"
        )

    [<Test>]
    member this.``Likely to be library if path segment contains "Lib" as a PascalCase segment``() =
        Assert.AreEqual(
            LibraryHeuristicResultByPath.Likely,
            howLikelyFileIsInLibrary "/src/LibFoo/Foo.fsproj"
        )

    [<Test>]
    member this.``Likely to be library if path segment ends with "Lib"``() =
        Assert.AreEqual(
            LibraryHeuristicResultByPath.Likely,
            howLikelyFileIsInLibrary "/src/FooLib/Whatever/Foo.fsproj"
        )

    [<Test>]
    member this.``Uncertain if path segment don't indicate likelyhood being in library``() =
        Assert.AreEqual(
            LibraryHeuristicResultByPath.Uncertain,
            howLikelyFileIsInLibrary "/src/Foo/Whatever/Foo.fsproj"
        )

