module Fantomas.Tests.EditorConfigurationTests

open System
open Fantomas.Core
open Fantomas
open NUnit.Framework
open System.IO

let private (==) (actual: 'T) (expected: 'T) =
    Assert.That(actual, Is.EqualTo<'T> expected)

/// These tests are about what a configuration comes out as, not about what gets said about it.
/// `EditorConfigReportTests` covers the reporting.
let private ignoreProblems: EditorConfigReport.EditorConfigReporter = fun _ _ -> ()

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
    )
    =
    let rootDir = Path.Join(Path.GetTempPath(), rootFolderName)

    do
        if not (Directory.Exists(rootDir)) then
            Directory.CreateDirectory(rootDir) |> ignore

    let editorConfigPath =
        match subFolder with
        | None -> Path.Join(rootDir, ".editorconfig")
        | Some sf ->

        let dirPath = Path.Join(rootDir, sf)

        if not (Directory.Exists(dirPath)) then
            Directory.CreateDirectory(dirPath) |> ignore

        Path.Join(rootDir, sf, ".editorconfig")

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
    (rootFolderName: string, ?fsharpFileExtension: string, ?subFolder: string, ?content: string, ?fileName: string)
    =
    let rootDir = Path.Join(Path.GetTempPath(), rootFolderName)

    do
        if not (Directory.Exists(rootDir)) then
            Directory.CreateDirectory(rootDir) |> ignore

    let extension = Option.defaultValue ".fs" fsharpFileExtension

    let fsharpFile =
        Option.defaultValue (sprintf "%s%s" (tempName ()) extension) fileName

    let fsharpFilePath =
        match subFolder with
        | None -> Path.Join(rootDir, fsharpFile)
        | Some sf ->

        let dirPath = Path.Join(rootDir, sf)

        if not (Directory.Exists(dirPath)) then
            Directory.CreateDirectory(dirPath) |> ignore

        Path.Join(rootDir, sf, fsharpFile)

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

    let config =
        EditorConfigReport.readConfiguration ignoreProblems fsharpFile.FSharpFile

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

    let config =
        EditorConfigReport.readConfiguration ignoreProblems fsharpFile.FSharpFile

    config.IndentSize == 2

[<Test>]
let ``parent config should not be taking into account when child is root`` () =
    let rootFolder = tempName ()
    let subFolder = tempName ()

    use parentConfig =
        new ConfigurationFile(
            { defaultConfig with
                MaxRecordWidth = 10
            },
            rootFolder
        )

    use childConfig =
        new ConfigurationFile({ defaultConfig with IndentSize = 2 }, rootFolder, subFolder = subFolder, isRoot = true)

    use fsharpFile = new FSharpFile(rootFolder, subFolder = subFolder)

    let config =
        EditorConfigReport.readConfiguration ignoreProblems fsharpFile.FSharpFile

    config.MaxRecordWidth == defaultConfig.MaxRecordWidth
    config.IndentSize == 2

[<Test>]
let ``configuration file should not affect file extension`` () =
    let rootFolder = tempName ()

    use configFixture =
        new ConfigurationFile(
            { defaultConfig with
                MaxLineLength = 90
            },
            rootFolder
        )

    use fsharpFile = new FSharpFile(rootFolder, fsharpFileExtension = ".fsx")

    let config =
        EditorConfigReport.readConfiguration ignoreProblems fsharpFile.FSharpFile

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

    let config =
        EditorConfigReport.readConfiguration ignoreProblems fsharpFile.FSharpFile

    config.MaxIfThenElseShortWidth == 25
    config.MaxValueBindingWidth == 40
    config.MaxFunctionBindingWidth == 40

// A section header with no keys under it sets nothing, so there is no configuration to hand back
// and nothing to complain about.
[<Test>]
let ``an editorconfig that sets nothing reads as no configuration at all`` () =
    let rootDir = tempName ()

    use _configFixture =
        new ConfigurationFile(defaultConfig, rootDir, content = "\n[*.fs]\n")

    use fsharpFile = new FSharpFile(rootDir)

    EditorConfig.tryReadConfiguration fsharpFile.FSharpFile == None

[<Test>]
let ``non existing file should return defaults for readConfiguration`` () =
    let rootDir = tempName ()

    use configFixture = new ConfigurationFile(defaultConfig, rootDir)

    let config =
        EditorConfigReport.readConfiguration ignoreProblems (Path.Join(Path.GetTempPath(), "bogus.fs"))

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

    let config =
        EditorConfigReport.readConfiguration ignoreProblems fsharpFile.FSharpFile

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

    let config =
        EditorConfigReport.readConfiguration ignoreProblems fsharpFile.FSharpFile

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

    let config =
        EditorConfigReport.readConfiguration ignoreProblems fsharpFile.FSharpFile

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

    let config =
        EditorConfigReport.readConfiguration ignoreProblems fsharpFile.FSharpFile

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

    let config =
        EditorConfigReport.readConfiguration ignoreProblems fsharpFile.FSharpFile

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

    let config =
        EditorConfigReport.readConfiguration ignoreProblems fsharpFile.FSharpFile

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

    // `end_of_line` is the one setting `cr` is read as a value for, and refusing it is the whole
    // point: a run cannot be asked to write carriage returns. A FormatException rather than a bare
    // exception, because that is the type the CLI matches on to decide what to print. Anything else
    // reports an empty message at normal verbosity.
    let ex =
        Assert.Throws<FormatException>(fun () ->
            EditorConfigReport.readConfiguration ignoreProblems fsharpFile.FSharpFile
            |> ignore
        )

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

    let config =
        EditorConfigReport.readConfiguration ignoreProblems fsharpFile.FSharpFile

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

    let config =
        EditorConfigReport.readConfiguration ignoreProblems fsharpFile.FSharpFile

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

    let config =
        EditorConfigReport.readConfiguration ignoreProblems fsharpFile.FSharpFile

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

    let config =
        EditorConfigReport.readConfiguration ignoreProblems fsharpFile.FSharpFile

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

    let config =
        EditorConfigReport.readConfiguration ignoreProblems fsharpFile.FSharpFile

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

    let config =
        EditorConfigReport.readConfiguration ignoreProblems fsharpFile.FSharpFile

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

    let config =
        EditorConfigReport.readConfiguration ignoreProblems fsharpFile.FSharpFile

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

    let config =
        EditorConfigReport.readConfiguration ignoreProblems fsharpFile.FSharpFile

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

    let config =
        EditorConfigReport.readConfiguration ignoreProblems fsharpFile.FSharpFile

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

    let config =
        EditorConfigReport.readConfiguration ignoreProblems fsharpFile.FSharpFile

    Assert.That(config.ExperimentalElmish, Is.True)

let private parse (settings: (string * string) list) =
    EditorConfig.parseOptionsFromEditorConfig defaultConfig (readOnlyDict settings)

[<Test>]
let ``an unparsable value is reported and the default is used`` () =
    let config, problems = parse [ "fsharp_experimental_elmish", "not_a_bool" ]

    problems
    == [
        EditorConfig.EditorConfigProblem.UnrecognizedValue("fsharp_experimental_elmish", "not_a_bool")
    ]

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
    == [
        EditorConfig.EditorConfigProblem.UnknownSetting "fsharp_multiline_brackets_style"
    ]

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

let private compareSettings (left: string) (right: string) : int =
    String.Compare(left, right, StringComparison.OrdinalIgnoreCase)

[<Test>]
let ``the supported settings list the editorconfig ones first, then the Fantomas ones`` () =
    let editorConfigOwn, fantomasOwn =
        EditorConfig.supportedSettings
        |> List.partition (EditorConfig.isFantomasSetting >> not)

    editorConfigOwn
    == [ "end_of_line"; "indent_size"; "insert_final_newline"; "max_line_length" ]

    EditorConfig.supportedSettings == editorConfigOwn @ fantomasOwn
    fantomasOwn == List.sortWith compareSettings fantomasOwn

[<Test>]
let ``values the editorconfig spec defines are not reported as mistakes`` () =
    // `indent_size = tab` is not something an author writes: the library derives it from
    // `indent_style = tab`. `unset` and `off` are spec values meaning "no value here".
    let _, problems =
        parse
            [
                "indent_size", "tab"
                "max_line_length", "off"
                "end_of_line", "unset"
                "insert_final_newline", "unset"
            ]

    problems == []

[<Test>]
let ``keys are matched without regard to case`` () =
    // editorconfig keys are case insensitive. The library lowercases what it reads from a file,
    // so this is what a request from an editor relies on.
    let config, problems = parse [ "FSHARP_MAX_RECORD_WIDTH", "120" ]

    problems == []
    config.MaxRecordWidth == 120

[<Test>]
let ``values are matched without regard to case as well`` () =
    let config, problems =
        parse
            [
                "fsharp_experimental_elmish", "TRUE"
                "fsharp_multiline_bracket_style", "Stroustrup"
                "end_of_line", "LF"
            ]

    problems == []
    Assert.That(config.ExperimentalElmish, Is.True)
    Assert.That(config.MultilineBracketStyle, Is.EqualTo Stroustrup)
    config.EndOfLine == EndOfLineStyle.LF

[<Test>]
let ``a problem names the setting the way it was written`` () =
    // The library lowercases what it reads from a file, but an editor sends its own dictionary
    // through untouched, and pointing at a spelling nobody wrote helps nobody find the line.
    let _, problems =
        parse [ "FSHARP_Bogus_Option", "true"; "Fsharp_Max_Record_Width", "banana" ]

    problems
    == [
        EditorConfig.EditorConfigProblem.UnknownSetting "FSHARP_Bogus_Option"
        EditorConfig.EditorConfigProblem.UnrecognizedValue("Fsharp_Max_Record_Width", "banana")
    ]

// Every parser used to be tried on every setting, so a value that means something to one of them
// decided the outcome for all the others. `cr` is the one that does damage: `EndOfLineStyle` raises
// on it rather than answering, which failed the whole run with a message about line endings.
[<Test>]
let ``a value that means something to another setting is a problem, not a failure`` () =
    let config, problems = parse [ "fsharp_max_record_width", "cr" ]

    problems
    == [
        EditorConfig.EditorConfigProblem.UnrecognizedValue("fsharp_max_record_width", "cr")
    ]

    config.MaxRecordWidth == defaultConfig.MaxRecordWidth

[<Test>]
let ``a value is only read as the type its own setting has`` () =
    // `stroustrup` is a bracket style and nothing else; `4` is a number and nothing else.
    let _, problems =
        parse
            [
                "fsharp_experimental_elmish", "stroustrup"
                "fsharp_multiline_bracket_style", "4"
            ]

    problems
    == [
        EditorConfig.EditorConfigProblem.UnrecognizedValue("fsharp_experimental_elmish", "stroustrup")
        EditorConfig.EditorConfigProblem.UnrecognizedValue("fsharp_multiline_bracket_style", "4")
    ]

[<Test>]
let ``a value editorconfig does not define is reported, prefix or not`` () =
    // Only the spec vocabulary is excused. A typo in one of editorconfig's own settings is still
    // a typo, and warned about the same as any other.
    let _, problems =
        parse
            [
                "indent_size", "banana"
                "max_line_length", "wide"
                "insert_final_newline", "maybe"
            ]

    problems
    == [
        EditorConfig.EditorConfigProblem.UnrecognizedValue("indent_size", "banana")
        EditorConfig.EditorConfigProblem.UnrecognizedValue("insert_final_newline", "maybe")
        EditorConfig.EditorConfigProblem.UnrecognizedValue("max_line_length", "wide")
    ]

[<Test>]
let ``an unparsable value for a Fantomas setting is still reported`` () =
    let _, problems = parse [ "fsharp_max_record_width", "banana" ]

    problems
    == [
        EditorConfig.EditorConfigProblem.UnrecognizedValue("fsharp_max_record_width", "banana")
    ]

// A mistake in `max_line_length` was silently ignored exactly as a mistake in a `fsharp_` setting
// was, and it is the same mistake.
[<Test>]
let ``a misspelling of an unprefixed setting is reported too`` () =
    let _, problems = parse [ "max_line_lenght", "100" ]

    problems
    == [ EditorConfig.EditorConfigProblem.UnknownSetting "max_line_lenght" ]

// The reason the distance for unprefixed keys is tighter than the one a suggestion is offered at.
// `indent_style` is three edits from `indent_size` and sits in very nearly every `.editorconfig`
// ever written; warning about it would put a false report in front of almost every user.
[<Test>]
let ``settings other tools really use are not read as misspellings of ours`` () =
    let _, problems =
        parse
            [
                "indent_style", "space"
                "tab_width", "4"
                "trim_trailing_whitespace", "true"
                "charset", "utf-8"
                "root", "true"
                "quote_type", "double"
                "dotnet_diagnostic_CA1000_severity", "none"
            ]

    problems == []

[<Test>]
let ``a negative width is not a width`` () =
    let config, problems = parse [ "fsharp_max_record_width", "-5" ]

    problems
    == [
        EditorConfig.EditorConfigProblem.UnrecognizedValue("fsharp_max_record_width", "-5")
    ]

    config.MaxRecordWidth == defaultConfig.MaxRecordWidth

[<Test>]
let ``settings belonging to other tools are left alone`` () =
    let _, problems =
        parse
            [
                "indent_style", "space"
                "trim_trailing_whitespace", "true"
                "some_other_tool_setting", "42"
            ]

    problems == []

[<Test>]
let ``every problem in one set of settings is reported together`` () =
    let _, problems =
        parse
            [
                "fsharp_bogus_option", "true"
                "fsharp_another_bogus_option", "4"
                "fsharp_experimental_elmish", "not_a_bool"
            ]

    problems
    == [
        EditorConfig.EditorConfigProblem.UnknownSetting "fsharp_another_bogus_option"
        EditorConfig.EditorConfigProblem.UnknownSetting "fsharp_bogus_option"
        EditorConfig.EditorConfigProblem.UnrecognizedValue("fsharp_experimental_elmish", "not_a_bool")
    ]

[<Test>]
let ``problems are reported by kind, and by setting name within a kind`` () =
    let _, problems =
        parse
            [
                "fsharp_space_before_colon", "not_a_bool"
                "fsharp_zzz_bogus_option", "true"
                "fsharp_experimental_elmish", "not_a_bool"
                "fsharp_aaa_bogus_option", "true"
            ]

    problems
    == [
        EditorConfig.EditorConfigProblem.UnknownSetting "fsharp_aaa_bogus_option"
        EditorConfig.EditorConfigProblem.UnknownSetting "fsharp_zzz_bogus_option"
        EditorConfig.EditorConfigProblem.UnrecognizedValue("fsharp_experimental_elmish", "not_a_bool")
        EditorConfig.EditorConfigProblem.UnrecognizedValue("fsharp_space_before_colon", "not_a_bool")
    ]

// ---- where each setting came from ----

let private settingOf (resolved: EditorConfig.ResolvedConfig) (setting: string) : EditorConfig.ResolvedSetting =
    resolved.Settings
    |> List.find (fun (candidate: EditorConfig.ResolvedSetting) -> candidate.Setting = setting)

[<Test>]
let ``a setting nothing wrote comes from no .editorconfig`` () =
    let rootFolderName = tempName ()
    use fsharpFile = new FSharpFile(rootFolderName)

    let resolved = EditorConfig.resolveConfiguration fsharpFile.FSharpFile

    (settingOf resolved "fsharp_max_record_width").SetBy == None

[<Test>]
let ``a setting an .editorconfig wrote names that file`` () =
    let rootFolderName = tempName ()

    use configFixture =
        new ConfigurationFile(defaultConfig, rootFolderName, content = "root=true\n\n[*.fs]\nmax_line_length=100\n")

    use fsharpFile = new FSharpFile(rootFolderName)

    let resolved = EditorConfig.resolveConfiguration fsharpFile.FSharpFile

    (settingOf resolved "max_line_length").Value == "100"

    (settingOf resolved "max_line_length").SetBy
    == Some(Path.GetFullPath(Path.Join(Path.GetTempPath(), rootFolderName, ".editorconfig")))

[<Test>]
let ``a setting written the same as the default still comes from the file that wrote it`` () =
    // The whole reason the origin is worked out from the chain rather than from comparing the
    // resolved value against the default: writing a default down is not the same as not writing it.
    let rootFolderName = tempName ()

    use configFixture =
        new ConfigurationFile(defaultConfig, rootFolderName, content = "root=true\n\n[*.fs]\nindent_size=4\n")

    use fsharpFile = new FSharpFile(rootFolderName)

    let resolved = EditorConfig.resolveConfiguration fsharpFile.FSharpFile

    (settingOf resolved "indent_size").Value == "4"
    (settingOf resolved "indent_size").SetBy.IsSome == true

[<Test>]
let ``the nearer of two .editorconfig files is the one a setting is credited to`` () =
    let rootFolder = tempName ()
    let subFolder = tempName ()

    use parentConfig =
        new ConfigurationFile(
            defaultConfig,
            rootFolder,
            content = "root=true\n\n[*.fs]\nmax_line_length=100\nfsharp_max_record_width=50\n"
        )

    use childConfig =
        new ConfigurationFile(
            defaultConfig,
            rootFolder,
            subFolder = subFolder,
            content = "[*.fs]\nmax_line_length=80\n"
        )

    use fsharpFile = new FSharpFile(rootFolder, subFolder = subFolder)

    let resolved = EditorConfig.resolveConfiguration fsharpFile.FSharpFile

    let parentPath =
        Path.GetFullPath(Path.Join(Path.GetTempPath(), rootFolder, ".editorconfig"))

    let childPath =
        Path.GetFullPath(Path.Join(Path.GetTempPath(), rootFolder, subFolder, ".editorconfig"))

    // Overruled by the nearer file, so the nearer file is the one to change.
    (settingOf resolved "max_line_length").Value == "80"
    (settingOf resolved "max_line_length").SetBy == Some childPath

    // Left alone by the nearer file, so it still belongs to the one further up.
    (settingOf resolved "fsharp_max_record_width").SetBy == Some parentPath

[<Test>]
let ``a setting whose value cannot be read is credited to nobody`` () =
    // The default is what will be used, so naming the file that wrote it would say the value came
    // from somewhere it did not. The problem is where that is reported.
    let rootFolderName = tempName ()

    use configFixture =
        new ConfigurationFile(
            defaultConfig,
            rootFolderName,
            content = "root=true\n\n[*.fs]\nfsharp_max_record_width=banana\n"
        )

    use fsharpFile = new FSharpFile(rootFolderName)

    let resolved = EditorConfig.resolveConfiguration fsharpFile.FSharpFile

    (settingOf resolved "fsharp_max_record_width").SetBy == None

    resolved.Problems
    == [
        EditorConfig.EditorConfigProblem.UnrecognizedValue("fsharp_max_record_width", "banana")
    ]

[<Test>]
let ``every setting Fantomas has is resolved, whether or not anything set it`` () =
    let rootFolderName = tempName ()
    use fsharpFile = new FSharpFile(rootFolderName)

    let resolved = EditorConfig.resolveConfiguration fsharpFile.FSharpFile

    resolved.Settings
    |> List.map (fun (setting: EditorConfig.ResolvedSetting) -> setting.Setting)
    == EditorConfig.supportedSettings

[<Test>]
let ``a configuration with nothing behind it credits nothing`` () =
    let resolved = EditorConfig.withoutEditorConfig defaultConfig

    resolved.Config == defaultConfig
    resolved.EditorConfigFiles == []
    resolved.Problems == []

    resolved.Settings
    |> List.forall (fun (setting: EditorConfig.ResolvedSetting) -> setting.SetBy.IsNone)
    == true
