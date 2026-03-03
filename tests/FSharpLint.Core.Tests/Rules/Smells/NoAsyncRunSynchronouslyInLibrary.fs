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
    member private this.HowLikelyFileIsInLibrary path =
        howLikelyFileIsInLibrary (System.IO.FileInfo path)

    [<Test>]
    member this.``Unlikely to be library if contains "tests" in name``() =
        Assert.AreEqual(
            LibraryHeuristicResultByPath.Unlikely,
            this.HowLikelyFileIsInLibrary "/dummy/IntegrationTests.fsproj"
        )

    [<Test>]
    member this.``Unlikely to be library if contains "testing" in name``() =
        Assert.AreEqual(
            LibraryHeuristicResultByPath.Unlikely,
            this.HowLikelyFileIsInLibrary "/dummy/UnitTesting.fsproj"
        )

    [<Test>]
    member this.``Unlikely to be library if contains "test" in name``() =
        Assert.AreEqual(
            LibraryHeuristicResultByPath.Unlikely,
            this.HowLikelyFileIsInLibrary "/dummy/TestSuite.fsproj"
        )

    [<Test>]
    member this.``Unlikely to be library if contains "console" in name``() =
        Assert.AreEqual(
            LibraryHeuristicResultByPath.Unlikely,
            this.HowLikelyFileIsInLibrary "/dummy/FooConsole.fsproj"
        )

    [<Test>]
    member this.``Likely to be library if contains Contains "Lib" as a PascalCase segment``() =
        Assert.AreEqual(
            LibraryHeuristicResultByPath.Likely,
            this.HowLikelyFileIsInLibrary "/dummy/LibFoo.fsproj"
        )

    [<Test>]
    member this.``Uncertain if contains contains "Lib" but not as a PascalCase segment``() =
        Assert.AreEqual(
            LibraryHeuristicResultByPath.Uncertain,
            this.HowLikelyFileIsInLibrary "/LibreOfficeProg.fsproj"
        )

    [<Test>]
    member this.``Likely to be library if contains ends with "lib" (case-insensitive)``() =
        Assert.AreEqual(
            LibraryHeuristicResultByPath.Likely,
            this.HowLikelyFileIsInLibrary "/dummy/FooLib.fsproj"
        )

    [<Test>]
    member this.``Unlikely to be library if contains "CLI" in name``() =
        Assert.AreEqual(
            LibraryHeuristicResultByPath.Unlikely,
            this.HowLikelyFileIsInLibrary "/dummy/FooCLI.fsproj"
        )

    [<Test>]
    member this.``Uncertain to be library if contains "cli" in name not related to CLI``() =
        Assert.AreEqual(
            LibraryHeuristicResultByPath.Uncertain,
            this.HowLikelyFileIsInLibrary "/InclinedDriver.fsproj"
        )

    [<Test>]
    member this.``Unlikely to be library if contains "TUI" in name``() =
        Assert.AreEqual(
            LibraryHeuristicResultByPath.Unlikely,
            this.HowLikelyFileIsInLibrary "/dummy/FooTUI.fsproj"
        )

    [<Test>]
    member this.``Likely to be library if it starts with "lib", e.g. camelCase``() =
        Assert.AreEqual(
            LibraryHeuristicResultByPath.Likely,
            this.HowLikelyFileIsInLibrary "/dummy/libFoo.fsproj"
        )

    [<Test>]
    member this.``Unlikely to be library if it contains "console", but segments are separated by dots``() =
        Assert.AreEqual(
            LibraryHeuristicResultByPath.Unlikely,
            this.HowLikelyFileIsInLibrary "/dummy/foo.console.app.fsproj"
        )

    [<Test>]
    member this.``Unlikely to be library if it contains "console", but segments are separated by dashes``() =
        Assert.AreEqual(
            LibraryHeuristicResultByPath.Unlikely,
            this.HowLikelyFileIsInLibrary "/dummy/foo-console-app.fsproj"
        )

    [<Test>]
    member this.``Unlikely to be library if it contains "console", but segments are separated by underscores``() =
        Assert.AreEqual(
            LibraryHeuristicResultByPath.Unlikely,
            this.HowLikelyFileIsInLibrary "/dummy/foo_console_app.fsproj"
        )

    [<Test>]
    member this.``Unlikely to be library if path segment contains "console"``() =
        Assert.AreEqual(
            LibraryHeuristicResultByPath.Unlikely,
            this.HowLikelyFileIsInLibrary "/Console/Foo.fsproj"
        )

    [<Test>]
    member this.``Unlikely to be library if path segment contains "test"``() =
        Assert.AreEqual(
            LibraryHeuristicResultByPath.Unlikely,
            this.HowLikelyFileIsInLibrary "/Tests/Foo.fsproj"
        )

    [<Test>]
    member this.``Unlikely to be library if path segment contains "TUI"``() =
        Assert.AreEqual(
            LibraryHeuristicResultByPath.Unlikely,
            this.HowLikelyFileIsInLibrary "/FooTUI/Foo.fsproj"
        )

    [<Test>]
    member this.``Likely to be library if path segment contains "Lib" as a PascalCase segment``() =
        Assert.AreEqual(
            LibraryHeuristicResultByPath.Likely,
            this.HowLikelyFileIsInLibrary "/src/LibFoo/Foo.fsproj"
        )

    [<Test>]
    member this.``Likely to be library if path segment ends with "Lib"``() =
        Assert.AreEqual(
            LibraryHeuristicResultByPath.Likely,
            this.HowLikelyFileIsInLibrary "/src/FooLib/Whatever/Foo.fsproj"
        )

    [<Test>]
    member this.``Uncertain if path segment don't indicate likelyhood being in library``() =
        Assert.AreEqual(
            LibraryHeuristicResultByPath.Uncertain,
            this.HowLikelyFileIsInLibrary "/src/Foo/Whatever/Foo.fsproj"
        )
