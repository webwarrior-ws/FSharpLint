﻿module TestConfiguration

open NUnit.Framework
open FSharpLint.Framework
open FSharpLint.Framework.Configuration

type System.String with
    member path.ToPlatformIndependentPath() =
        path.Replace('\\', System.IO.Path.DirectorySeparatorChar)

let configWithHints hints =
     { Configuration.Zero with Hints = hints }


[<TestFixture>]
type TestConfiguration() =
    [<Test>]
    member __.``Ignore all files ignores any given file.``() =
        let ignorePaths = [ IgnoreFiles.parseIgnorePath "*" ]

        let path = @"D:\dog\source.fs".ToPlatformIndependentPath()

        IgnoreFiles.shouldFileBeIgnored ignorePaths path
        |> Assert.IsTrue

    [<Test>]
    member __.``Ignoring a file name not inside a path does not ignore the path``() =
        let ignorePaths = [ IgnoreFiles.parseIgnorePath "cat" ]

        let path = @"D:\dog\source.fs".ToPlatformIndependentPath()

        IgnoreFiles.shouldFileBeIgnored ignorePaths path
        |> Assert.IsFalse

    [<Test>]
    member __.``Ignoring a file doesn't ignore a directory.``() =
        let ignorePaths = [ IgnoreFiles.parseIgnorePath "dog" ]

        let path = @"D:\dog\source.fs".ToPlatformIndependentPath()

        IgnoreFiles.shouldFileBeIgnored ignorePaths  path
        |> Assert.IsFalse

    [<Test>]
    member __.``Ignoring a directory doesn't ignore a file.``() =
        let ignorePaths = [ IgnoreFiles.parseIgnorePath "source.fs/" ]

        let path = @"D:\dog\source.fs".ToPlatformIndependentPath()

        IgnoreFiles.shouldFileBeIgnored ignorePaths path
        |> Assert.IsFalse

    [<Test>]
    member __.``Ignoring all files in a given directory ignores a given file from the directory.``() =
        let ignorePaths = [ IgnoreFiles.parseIgnorePath "dog/*" ]

        let path = @"D:\dog\source.fs".ToPlatformIndependentPath()

        IgnoreFiles.shouldFileBeIgnored ignorePaths path
        |> Assert.IsTrue

    [<Test>]
    member __.``Ignoring a file that does not exist inside a directory that does exist does not ignore the file.``() =
        let ignorePaths = [ IgnoreFiles.parseIgnorePath "dog/source1" ]

        let path = @"D:\dog\source.fs".ToPlatformIndependentPath()

        IgnoreFiles.shouldFileBeIgnored ignorePaths path
        |> Assert.IsFalse

    [<Test>]
    member __.``Ignoring the contents of a directory and then negating a specific file ignores all files other than the negated file.``() =
        let ignorePaths =
            [ IgnoreFiles.parseIgnorePath "dog/*"
              IgnoreFiles.parseIgnorePath "!source.*" ]

        let path = @"D:\dog\source.fs".ToPlatformIndependentPath()

        IgnoreFiles.shouldFileBeIgnored ignorePaths path
        |> Assert.IsFalse

        let path = @"D:\dog\source2.fs".ToPlatformIndependentPath()

        IgnoreFiles.shouldFileBeIgnored ignorePaths path
        |> Assert.IsTrue

    [<Test>]
    member __.``Ingoring a file that was previously negated ignores the file.``() =
        let ignorePaths =
            [ IgnoreFiles.parseIgnorePath "dog/*"
              IgnoreFiles.parseIgnorePath "!source.*"
              IgnoreFiles.parseIgnorePath "dog/*" ]

        let path = @"D:\dog\source.fs".ToPlatformIndependentPath()

        IgnoreFiles.shouldFileBeIgnored ignorePaths path
        |> Assert.IsTrue

    [<Test>]
    member __.``Camel case JSON config correctly parsed into expected config records`` () =
        let expectedConfig = 
            { Configuration.Zero with NoTabCharacters = Some { Enabled = true; Config = None } }


        let config =
            """{
    "noTabCharacters": {
        "enabled": true
    }
}
"""
        let actualConfig = parseConfig config

        Assert.AreEqual(expectedConfig.NoTabCharacters, actualConfig.NoTabCharacters)

    [<Test>]
    member __.``Combining config with empty one results in unchanged original config`` () =
        let defaultConfig = Configuration.defaultConfiguration

        let combinedConfig = Configuration.combineConfigs defaultConfig Configuration.Zero

        Assert.AreEqual(defaultConfig, combinedConfig)

    [<Test>]
    member __.``When combining one config with another non-empty values are overriden`` () =
        let baseConfig = Configuration.Zero

        let partialConfig = { Configuration.Zero with NoTabCharacters = Some { Enabled = true; Config = None } }

        let combinedConfig = Configuration.combineConfigs baseConfig partialConfig

        Assert.AreEqual(combinedConfig.NoTabCharacters, partialConfig.NoTabCharacters)

    [<Test>]
    member __.``When combining one config with another empty values are not overriden`` () =
        let baseConfig = { Configuration.Zero with NoTabCharacters = Some { Enabled = true; Config = None } }

        let partialConfig = Configuration.Zero

        let combinedConfig = Configuration.combineConfigs baseConfig partialConfig

        Assert.AreEqual(combinedConfig.NoTabCharacters, baseConfig.NoTabCharacters)
