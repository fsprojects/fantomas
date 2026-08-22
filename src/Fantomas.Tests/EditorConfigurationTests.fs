module Fantomas.Tests.EditorConfigurationTests

open System
open Fantomas.Core
open Fantomas
open NUnit.Framework
open System.IO

let private (==) (actual: 'T) (expected: 'T) =
    Assert.That(actual, Is.EqualTo<'T> expected)

let private silentLog: Serilog.ILogger = Serilog.Core.Logger.None

let private defaultConfig = FormatConfig.Default
let tempName () = Guid.NewGuid().ToString("N")

type ConfigurationFile
    internal
    (
        config: FormatConfig,
        rootFolderName: string,
        ?editorConfigHeader: string,
        ?subFolder: string,
        ?isRoot: bool,
        ?content: string
    ) =
    let rootDir = Path.Join(Path.GetTempPath(), rootFolderName)

    do
        if not (Directory.Exists(rootDir)) then
            Directory.CreateDirectory(rootDir) |> ignore

    let editorConfigPath =
        match subFolder with
        | Some sf ->
            let dirPath = Path.Join(rootDir, sf)

            if not (Directory.Exists(dirPath)) then
                Directory.CreateDirectory(dirPath) |> ignore

            Path.Join(rootDir, sf, ".editorconfig")
        | None -> Path.Join(rootDir, ".editorconfig")

    let header = Option.defaultValue "[*.fs]" editorConfigHeader

    let content =
        match content with
        | Some c -> c
        | None ->
            let root =
                match isRoot with
                | Some true -> "root=true"
                | _ -> String.empty

            $"%s{root}\n\n%s{header}\n%s{EditorConfig.configToEditorConfig config}"

    do File.WriteAllText(editorConfigPath, content)

    interface IDisposable with
        member this.Dispose() : unit =
            if Directory.Exists(rootDir) then
                Directory.Delete(rootDir, true)

type FSharpFile
    internal
    (rootFolderName: string, ?fsharpFileExtension: string, ?subFolder: string, ?content: string, ?fileName: string) =
    let rootDir = Path.Join(Path.GetTempPath(), rootFolderName)

    do
        if not (Directory.Exists(rootDir)) then
            Directory.CreateDirectory(rootDir) |> ignore

    let extension = Option.defaultValue ".fs" fsharpFileExtension

    let fsharpFile =
        Option.defaultValue (sprintf "%s%s" (tempName ()) extension) fileName

    let fsharpFilePath =
        match subFolder with
        | Some sf ->
            let dirPath = Path.Join(rootDir, sf)

            if not (Directory.Exists(dirPath)) then
                Directory.CreateDirectory(dirPath) |> ignore

            Path.Join(rootDir, sf, fsharpFile)
        | None -> Path.Join(rootDir, fsharpFile)

    let content = Option.defaultValue String.empty content
    do File.WriteAllText(fsharpFilePath, content)

    member __.FSharpFile: string = fsharpFilePath

    interface IDisposable with
        member this.Dispose() : unit =
            if Directory.Exists(rootDir) then
                Directory.Delete(rootDir, true)

[<Test>]
let ``single configuration file`` () =
    let rootFolderName = tempName ()

    use configFixture = new ConfigurationFile(defaultConfig, rootFolderName)

    use fsharpFile = new FSharpFile(rootFolderName, fsharpFileExtension = ".fs")

    let config = EditorConfig.readConfiguration silentLog fsharpFile.FSharpFile

    config == defaultConfig

[<Test>]
let ``pointing to subfolder should return parent config file as well`` () =
    let rootFolder = tempName ()
    let subFolder = tempName ()

    use parentConfig =
        new ConfigurationFile({ defaultConfig with IndentSize = 3 }, rootFolder)

    use childConfig =
        new ConfigurationFile({ defaultConfig with IndentSize = 2 }, rootFolder, subFolder = subFolder)

    use fsharpFile = new FSharpFile(rootFolder, subFolder = subFolder)

    let config = EditorConfig.readConfiguration silentLog fsharpFile.FSharpFile

    config.IndentSize == 2

[<Test>]
let ``parent config should not be taking into account when child is root`` () =
    let rootFolder = tempName ()
    let subFolder = tempName ()

    use parentConfig =
        new ConfigurationFile(
            { defaultConfig with
                MaxRecordWidth = 10 },
            rootFolder
        )

    use childConfig =
        new ConfigurationFile({ defaultConfig with IndentSize = 2 }, rootFolder, subFolder = subFolder, isRoot = true)

    use fsharpFile = new FSharpFile(rootFolder, subFolder = subFolder)

    let config = EditorConfig.readConfiguration silentLog fsharpFile.FSharpFile
    config.MaxRecordWidth == defaultConfig.MaxRecordWidth
    config.IndentSize == 2

[<Test>]
let ``configuration file should not affect file extension`` () =
    let rootFolder = tempName ()

    use configFixture =
        new ConfigurationFile(
            { defaultConfig with
                MaxLineLength = 90 },
            rootFolder
        )

    use fsharpFile = new FSharpFile(rootFolder, fsharpFileExtension = ".fsx")
    let config = EditorConfig.readConfiguration silentLog fsharpFile.FSharpFile
    config.MaxLineLength == defaultConfig.MaxLineLength

[<Test>]
let ``fantomas configuration file`` () =
    let rootDir = tempName ()

    let myConfig =
        """
[*.fs]
fsharp_max_if_then_else_short_width=25
fsharp_max_value_binding_width=40
fsharp_max_function_binding_width=40
"""

    use configFixture =
        new ConfigurationFile(defaultConfig, rootDir, content = myConfig)

    use fsharpFile = new FSharpFile(rootDir)

    let config = EditorConfig.readConfiguration silentLog fsharpFile.FSharpFile

    config.MaxIfThenElseShortWidth == 25
    config.MaxValueBindingWidth == 40
    config.MaxFunctionBindingWidth == 40

[<Test>]
let ``non existing file should return defaults for readConfiguration`` () =
    let rootDir = tempName ()

    use configFixture = new ConfigurationFile(defaultConfig, rootDir)

    let config =
        EditorConfig.readConfiguration silentLog (Path.Join(Path.GetTempPath(), "bogus.fs"))

    config == defaultConfig

// In the future we could ensure that the Default config isn't
// being generated every time because it's a property getter
// Assert.That(Object.ReferenceEquals(config, defaultConfig))

[<Test>]
let ``non existing file should return None for tryReadConfiguration`` () =
    let rootDir = tempName ()

    use configFixture = new ConfigurationFile(defaultConfig, rootDir)

    let config =
        EditorConfig.tryReadConfiguration (Path.Join(Path.GetTempPath(), "bogus.fs"))

    config == None

[<Test>]
let ``indent_style tab edge case`` () =
    let rootDir = tempName ()

    let editorConfig =
        """
[*.fs]
indent_style=tab
indent_size=tab
tab_width=5
"""

    use configFixture =
        new ConfigurationFile(defaultConfig, rootDir, content = editorConfig)

    use fsharpFile = new FSharpFile(rootDir)
    let config = EditorConfig.readConfiguration silentLog fsharpFile.FSharpFile
    config.IndentSize == 5

[<Test>]
let ``print default editorconfig settings`` () =
    FormatConfig.Default |> EditorConfig.configToEditorConfig |> printfn "%s"

[<Test>]
let ``list and array number_of_items parsing tests`` () =
    let rootDir = tempName ()

    let editorConfig =
        """
[*.fs]
fsharp_array_or_list_multiline_formatter = number_of_items
fsharp_max_array_or_list_number_of_items = 4
"""

    use configFixture =
        new ConfigurationFile(defaultConfig, rootDir, content = editorConfig)

    use fsharpFile = new FSharpFile(rootDir)
    let config = EditorConfig.readConfiguration silentLog fsharpFile.FSharpFile
    config.MaxArrayOrListNumberOfItems == 4
    config.ArrayOrListMultilineFormatter == NumberOfItems

[<Test>]
let ``list and array character_width parsing test with single option`` () =
    let rootDir = tempName ()

    let editorConfig =
        """
[*.fs]
fsharp_max_array_or_list_width = 123
"""

    use configFixture =
        new ConfigurationFile(defaultConfig, rootDir, content = editorConfig)

    use fsharpFile = new FSharpFile(rootDir)

    let config = EditorConfig.readConfiguration silentLog fsharpFile.FSharpFile

    config.MaxArrayOrListWidth == 123

[<Test>]
let ``record number_of_items parsing tests`` () =
    let rootDir = tempName ()

    let editorConfig =
        """
[*.fs]
fsharp_record_multiline_formatter = number_of_items
fsharp_max_record_number_of_items = 4
"""

    use configFixture =
        new ConfigurationFile(defaultConfig, rootDir, content = editorConfig)

    use fsharpFile = new FSharpFile(rootDir)

    let config = EditorConfig.readConfiguration silentLog fsharpFile.FSharpFile

    config.MaxRecordNumberOfItems == 4

    config.RecordMultilineFormatter == NumberOfItems

[<Test>]
let ``record character_width parsing test with single option`` () =
    let rootDir = tempName ()

    let editorConfig =
        """
[*.fs]
fsharp_max_record_width = 123
"""

    use configFixture =
        new ConfigurationFile(defaultConfig, rootDir, content = editorConfig)

    use fsharpFile = new FSharpFile(rootDir)

    let config = EditorConfig.readConfiguration silentLog fsharpFile.FSharpFile

    config.MaxRecordWidth == 123

[<Test>]
let ``infix operator expression character_width parsing test with single option`` () =
    let rootDir = tempName ()

    let editorConfig =
        """
[*.fs]
fsharp_max_infix_operator_expression = 123
"""

    use configFixture =
        new ConfigurationFile(defaultConfig, rootDir, content = editorConfig)

    use fsharpFile = new FSharpFile(rootDir)

    let config = EditorConfig.readConfiguration silentLog fsharpFile.FSharpFile

    config.MaxInfixOperatorExpression == 123

[<Test>]
let ``end_of_line = cr should throw`` () =
    let rootDir = tempName ()

    let editorConfig =
        """
[*.fs]
end_of_line = cr
"""

    use configFixture =
        new ConfigurationFile(defaultConfig, rootDir, content = editorConfig)

    use fsharpFile = new FSharpFile(rootDir)

    // A FormatException rather than a bare exception, because that is the type the CLI matches
    // on to decide what to print. Anything else reports an empty message at normal verbosity.
    let ex =
        Assert.Throws<FormatException>(fun () ->
            EditorConfig.readConfiguration silentLog fsharpFile.FSharpFile |> ignore)

    ex.Message
    == "Carriage returns are not valid for F# code, please use one of 'lf' or 'crlf'"

let valid_eol_settings = [ EndOfLineStyle.LF; EndOfLineStyle.CRLF ]

[<TestCaseSource("valid_eol_settings")>]
let can_parse_end_of_line_setting (eol: EndOfLineStyle) =
    let rootDir = tempName ()

    let editorConfig =
        sprintf
            """
[*.fs]
end_of_line = %s
"""
            (EndOfLineStyle.ToConfigString eol)

    use configFixture =
        new ConfigurationFile(defaultConfig, rootDir, content = editorConfig)

    use fsharpFile = new FSharpFile(rootDir)

    let config = EditorConfig.readConfiguration silentLog fsharpFile.FSharpFile

    config.EndOfLine == eol

[<Test>]
let fsharp_multiLine_lambda_closing_newline () =
    let rootDir = tempName ()

    let editorConfig =
        """
[*.fs]
fsharp_multi_line_lambda_closing_newline = true
"""

    use configFixture =
        new ConfigurationFile(defaultConfig, rootDir, content = editorConfig)

    use fsharpFile = new FSharpFile(rootDir)

    let config = EditorConfig.readConfiguration silentLog fsharpFile.FSharpFile

    Assert.That(config.MultiLineLambdaClosingNewline, Is.True)

[<Test>]
let fsharp_experimental_keep_indent_in_branch () =
    let rootDir = tempName ()

    let editorConfig =
        """
[*.fs]
fsharp_experimental_keep_indent_in_branch = true
"""

    use configFixture =
        new ConfigurationFile(defaultConfig, rootDir, content = editorConfig)

    use fsharpFile = new FSharpFile(rootDir)

    let config = EditorConfig.readConfiguration silentLog fsharpFile.FSharpFile

    Assert.That(config.ExperimentalKeepIndentInBranch, Is.True)

[<Test>]
let fsharp_bar_before_discriminated_union_declaration () =
    let rootDir = tempName ()

    let editorConfig =
        """
[*.fs]
fsharp_bar_before_discriminated_union_declaration = true
"""

    use configFixture =
        new ConfigurationFile(defaultConfig, rootDir, content = editorConfig)

    use fsharpFile = new FSharpFile(rootDir)

    let config = EditorConfig.readConfiguration silentLog fsharpFile.FSharpFile

    Assert.That(config.BarBeforeDiscriminatedUnionDeclaration, Is.True)

[<Test>]
let insert_final_newline () =
    let rootDir = tempName ()

    let editorConfig =
        """
[*.fs]
insert_final_newline = false
"""

    use configFixture =
        new ConfigurationFile(defaultConfig, rootDir, content = editorConfig)

    use fsharpFile = new FSharpFile(rootDir)

    let config = EditorConfig.readConfiguration silentLog fsharpFile.FSharpFile

    Assert.That(config.InsertFinalNewline, Is.False)

[<Test>]
let ``fsharp_multiline_bracket_style = stroustrup`` () =
    let rootDir = tempName ()

    let editorConfig =
        """
[*.fs]
fsharp_multiline_bracket_style = stroustrup
"""

    use configFixture =
        new ConfigurationFile(defaultConfig, rootDir, content = editorConfig)

    use fsharpFile = new FSharpFile(rootDir)

    let config = EditorConfig.readConfiguration silentLog fsharpFile.FSharpFile

    Assert.That(config.MultilineBracketStyle, Is.EqualTo Stroustrup)

[<Test>]
let ``fsharp_multiline_bracket_style = aligned`` () =
    let rootDir = tempName ()

    let editorConfig =
        """
[*.fs]
fsharp_multiline_bracket_style = aligned
"""

    use configFixture =
        new ConfigurationFile(defaultConfig, rootDir, content = editorConfig)

    use fsharpFile = new FSharpFile(rootDir)

    let config = EditorConfig.readConfiguration silentLog fsharpFile.FSharpFile

    Assert.That(config.MultilineBracketStyle, Is.EqualTo Aligned)

[<Test>]
let ``fsharp_multiline_bracket_style = cramped`` () =
    let rootDir = tempName ()

    let editorConfig =
        """
[*.fs]
fsharp_multiline_bracket_style = cramped
"""

    use configFixture =
        new ConfigurationFile(defaultConfig, rootDir, content = editorConfig)

    use fsharpFile = new FSharpFile(rootDir)

    let config = EditorConfig.readConfiguration silentLog fsharpFile.FSharpFile

    Assert.That(config.MultilineBracketStyle, Is.EqualTo Cramped)

[<Test>]
let fsharp_prefer_computation_expression_name_on_same_line () =
    let rootDir = tempName ()

    let editorConfig =
        """
[*.fs]
fsharp_newline_before_multiline_computation_expression = false
"""

    use configFixture =
        new ConfigurationFile(defaultConfig, rootDir, content = editorConfig)

    use fsharpFile = new FSharpFile(rootDir)

    let config = EditorConfig.readConfiguration silentLog fsharpFile.FSharpFile

    Assert.That(config.NewlineBeforeMultilineComputationExpression, Is.False)

[<Test>]
let fsharp_stroustrup_final_list_arguments () =
    let rootDir = tempName ()

    let editorConfig =
        """
[*.fs]
fsharp_experimental_elmish = true
"""

    use configFixture =
        new ConfigurationFile(defaultConfig, rootDir, content = editorConfig)

    use fsharpFile = new FSharpFile(rootDir)

    let config = EditorConfig.readConfiguration silentLog fsharpFile.FSharpFile

    Assert.That(config.ExperimentalElmish, Is.True)

let private parse (settings: (string * string) list) =
    EditorConfig.parseOptionsFromEditorConfig defaultConfig (readOnlyDict settings)

[<Test>]
let ``an unparsable value is reported and the default is used`` () =
    let config, problems = parse [ "fsharp_experimental_elmish", "not_a_bool" ]

    problems
    == [ EditorConfig.EditorConfigProblem.UnrecognizedValue("fsharp_experimental_elmish", "not_a_bool") ]

    config.ExperimentalElmish == defaultConfig.ExperimentalElmish

[<Test>]
let ``an unknown fsharp setting is reported`` () =
    let _, problems = parse [ "fsharp_bogus_option", "true" ]

    problems
    == [ EditorConfig.EditorConfigProblem.UnknownSetting "fsharp_bogus_option" ]

[<Test>]
let ``a misspelled setting is reported, and the spelling Fantomas expects is one it supports`` () =
    let _, problems = parse [ "fsharp_multiline_brackets_style", "stroustrup" ]

    problems
    == [ EditorConfig.EditorConfigProblem.UnknownSetting "fsharp_multiline_brackets_style" ]

    Assert.That(EditorConfig.supportedSettings, Does.Contain "fsharp_multiline_bracket_style")

[<Test>]
let ``prefixing a setting editorconfig itself defines is reported`` () =
    let config, problems = parse [ "fsharp_max_line_length", "100" ]

    problems
    == [ EditorConfig.EditorConfigProblem.UnknownSetting "fsharp_max_line_length" ]

    config.MaxLineLength == defaultConfig.MaxLineLength

[<Test>]
let ``the settings editorconfig itself defines are supported unprefixed`` () =
    let config, problems = parse [ "max_line_length", "60" ]
    problems == []
    config.MaxLineLength == 60

[<Test>]
let ``the supported settings list the editorconfig ones first, then the Fantomas ones`` () =
    let editorConfigOwn, fantomasOwn =
        EditorConfig.supportedSettings
        |> List.partition (fun (setting: string) -> not (setting.StartsWith("fsharp_", StringComparison.Ordinal)))

    editorConfigOwn
    == [ "end_of_line"; "indent_size"; "insert_final_newline"; "max_line_length" ]

    EditorConfig.supportedSettings == editorConfigOwn @ fantomasOwn

    fantomasOwn
    == List.sortWith (fun left right -> String.CompareOrdinal(left, right)) fantomasOwn

[<Test>]
let ``values the editorconfig spec defines are not reported as mistakes`` () =
    // `indent_size = tab` is not something an author writes: the library derives it from
    // `indent_style = tab`. `unset` and `off` are spec values meaning "no value here".
    let _, problems =
        parse
            [ "indent_size", "tab"
              "max_line_length", "off"
              "end_of_line", "unset"
              "insert_final_newline", "unset" ]

    problems == []

[<Test>]
let ``keys are matched without regard to case`` () =
    // editorconfig keys are case insensitive. The library lowercases what it reads from a file,
    // so this is what a request from an editor relies on.
    let config, problems = parse [ "FSHARP_MAX_RECORD_WIDTH", "120" ]

    problems == []
    config.MaxRecordWidth == 120

[<Test>]
let ``a value editorconfig does not define is reported, prefix or not`` () =
    // Only the spec vocabulary is excused. A typo in one of editorconfig's own settings is still
    // a typo, and warned about the same as any other.
    let _, problems =
        parse
            [ "indent_size", "banana"
              "max_line_length", "wide"
              "insert_final_newline", "maybe" ]

    problems
    == [ EditorConfig.EditorConfigProblem.UnrecognizedValue("indent_size", "banana")
         EditorConfig.EditorConfigProblem.UnrecognizedValue("insert_final_newline", "maybe")
         EditorConfig.EditorConfigProblem.UnrecognizedValue("max_line_length", "wide") ]

[<Test>]
let ``an unparsable value for a Fantomas setting is still reported`` () =
    let _, problems = parse [ "fsharp_max_record_width", "banana" ]

    problems
    == [ EditorConfig.EditorConfigProblem.UnrecognizedValue("fsharp_max_record_width", "banana") ]

[<Test>]
let ``settings belonging to other tools are left alone`` () =
    let _, problems =
        parse
            [ "indent_style", "space"
              "trim_trailing_whitespace", "true"
              "some_other_tool_setting", "42" ]

    problems == []

[<Test>]
let ``every problem in one set of settings is reported together`` () =
    let _, problems =
        parse
            [ "fsharp_bogus_option", "true"
              "fsharp_another_bogus_option", "4"
              "fsharp_experimental_elmish", "not_a_bool" ]

    problems
    == [ EditorConfig.EditorConfigProblem.UnknownSetting "fsharp_another_bogus_option"
         EditorConfig.EditorConfigProblem.UnknownSetting "fsharp_bogus_option"
         EditorConfig.EditorConfigProblem.UnrecognizedValue("fsharp_experimental_elmish", "not_a_bool") ]

[<Test>]
let ``problems are reported by kind, and by setting name within a kind`` () =
    let _, problems =
        parse
            [ "fsharp_space_before_colon", "not_a_bool"
              "fsharp_zzz_bogus_option", "true"
              "fsharp_experimental_elmish", "not_a_bool"
              "fsharp_aaa_bogus_option", "true" ]

    problems
    == [ EditorConfig.EditorConfigProblem.UnknownSetting "fsharp_aaa_bogus_option"
         EditorConfig.EditorConfigProblem.UnknownSetting "fsharp_zzz_bogus_option"
         EditorConfig.EditorConfigProblem.UnrecognizedValue("fsharp_experimental_elmish", "not_a_bool")
         EditorConfig.EditorConfigProblem.UnrecognizedValue("fsharp_space_before_colon", "not_a_bool") ]
