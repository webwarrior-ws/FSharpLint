module TestApp

open System
open System.IO
open NUnit.Framework

let getErrorsFromOutput (output:string) =
    let splitOutput = output.Split([|Environment.NewLine|], StringSplitOptions.None)

    set [ for i in 1..splitOutput.Length - 1 do
            if splitOutput.[i].StartsWith "Error" then yield splitOutput.[i - 1] ]

type TemporaryFile(fileContent, extension) =
    let filename = Path.ChangeExtension(Path.GetTempFileName(), extension)
    do
        File.WriteAllText(filename, fileContent)

    member __.FileName = filename

    interface System.IDisposable with
        member __.Dispose() =
            File.Delete(filename)

let main input =
    use stdout = new StringWriter()
    let existing = Console.Out
    Console.SetOut(stdout)
    try
        let returnCode = FSharpLint.Console.Program.main input
        (returnCode, getErrorsFromOutput <| stdout.ToString())
    finally
        Console.SetOut(existing)

[<TestFixture>]
type TestConsoleApplication() =
    [<Test>]
    member __.``Lint file, expected rules are triggered.``() =
        let fileContent = """
        type Signature =
            abstract member Encoded : string
            abstract member PathName : string
        """
        use input = new TemporaryFile(fileContent, "fs")

        let (returnCode, errors) = main [| "lint"; input.FileName |]

        Assert.AreEqual(-1, returnCode)
        Assert.AreEqual(set ["Consider changing `Signature` to be prefixed with `I`."], errors)

    [<Test>]
    member __.``Lint source without any config, rule enabled in default config is triggered for given source.``() =
        let input = """
        type Signature =
            abstract member Encoded : string
            abstract member PathName : string
        """

        let (returnCode, errors) = main [| "lint"; input |]

        Assert.AreEqual(-1, returnCode)
        Assert.AreEqual(set ["Consider changing `Signature` to be prefixed with `I`."], errors)

    [<Test>]
    member __.``Lint source with valid config to disable rule, disabled rule is not triggered for given source.``() =
        let fileContent = """
        {
            "InterfaceNames": {
                "enabled": false
            }
        }
        """
        use config = new TemporaryFile(fileContent, "json")

        let input = """
        type Signature =
            abstract member Encoded : string
            abstract member PathName : string
        """

        let (returnCode, errors) = main [| "lint"; "--lint-config"; config.FileName; input |]

        Assert.AreEqual(0, returnCode)
        Assert.AreEqual(Set.empty, errors)
        
    [<Test>]
    member __.``Lint source with error suppressed, no error is given.``() =
        let input = """
        // fsharplint:disable-next-line
        type Signature =
            abstract member Encoded : string
            abstract member PathName : string
        """
        
        let (returnCode, errors) = main [| "lint"; input |]
        
        Assert.AreEqual(0, returnCode)
        Assert.AreEqual(Set.empty, errors)

    [<Test>]
    member __.``Regression test: typePrefixing rule with old config format should still work``() =
        let fileContent = """
        {
            "typePrefixing": {
                "enabled": true
            }
        }
        """
        use config = new TemporaryFile(fileContent, "json")

        let input = """
        module Program

        type X = int Generic
        """

        let (returnCode, errors) = main [| "lint"; "--lint-config"; config.FileName; input |]

        Assert.AreEqual(-1, returnCode)
        Assert.AreEqual(set ["Use prefix syntax for generic type."], errors)

(* temporarily disabled until PR is finished
    [<Test>]
*)
    member __.``TypePrefixing rule Hybrid mode should still work (after it gets renamed to HybridWeak)``() =
        let fileContent = """
        {
            "typePrefixing": {
                "enabled": true,
                "config": {
                    "mode": "Hybrid"
                }
            }
        }
        """
        use config = new TemporaryFile(fileContent, "json")

        let input = """
        module Program

        type X = int Generic
        """

        let (returnCode, errors) = main [| "lint"; "--lint-config"; config.FileName; input |]

        Assert.AreEqual(-1, returnCode)
        Assert.AreEqual(set ["Use prefix syntax for generic type."], errors)

    [<Test>]
    member __.``TypePrefixing rule HybridStrict mode should complain about List<Foo>``() =
        let fileContent = """
        {
            "typePrefixing": {
                "enabled": true,
                "config": {
                    "mode": "HybridStrict"
                }
            }
        }
        """
        use config = new TemporaryFile(fileContent, "json")

        let input = """
        module Program

        type X = List<int>
        """

        let (returnCode, errors) = main [| "lint"; "--lint-config"; config.FileName; input |]

        Assert.AreEqual(-1, returnCode, "Return code of HybridStrict against List<Foo> should not be zero")
        Assert.AreNotEqual(0, errors.Count, "Number of errors for HybridStrict mode against List<Foo> should not be zero")
        Assert.AreEqual("Use postfix syntax for F# type List.", errors.MaximumElement)

    [<Test>]
    member __.``TypePrefixing rule HybridStrict mode should complain about array<Foo>``() =
        let fileContent = """
        {
            "typePrefixing": {
                "enabled": true,
                "config": {
                    "mode": "HybridStrict"
                }
            }
        }
        """
        use config = new TemporaryFile(fileContent, "json")

        let input = """
        module Program

        type X = array<int>
        """

        let (returnCode, errors) = main [| "lint"; "--lint-config"; config.FileName; input |]

        Assert.AreEqual(-1, returnCode, "Return code of HybridStrict against array<Foo> should not be zero")
        Assert.AreNotEqual(0, errors.Count, "Number of errors for HybridStrict mode against array<Foo> should not be zero")
        Assert.AreEqual("Use special postfix syntax for F# type array.", errors.MaximumElement)

    [<Test>]
    member __.``TypePrefixing rule HybridWeak mode should not complain about List<Foo>``() =
        let fileContent = """
        {
            "typePrefixing": {
                "enabled": true,
                "config": {
                    "mode": "HybridWeak"
                }
            }
        }
        """
        use config = new TemporaryFile(fileContent, "json")

        let input = """
        module Program

        type X = List<int>
        """

        let (returnCode, errors) = main [| "lint"; "--lint-config"; config.FileName; input |]

        Assert.AreEqual(0, returnCode)
        if errors.Count = 0 then
            Assert.AreEqual(0, errors.Count, "Return code of HybridWeak against List<Foo> should not be zero (=success; no suggestions)")
        else
            Assert.AreEqual(0, errors.Count, "No errors should happen, but we got at least one: " + errors.MaximumElement)
