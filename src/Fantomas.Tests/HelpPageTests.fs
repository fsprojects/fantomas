module Fantomas.Tests.HelpPageTests

open System
open System.Text.RegularExpressions
open NUnit.Framework
open FsUnitTyped
open Fantomas.Core
open Fantomas.Arguments
open Fantomas.Theme
open Fantomas.HelpPage

/// The page never draws a status glyph, so only the palette varies here.
/// The tests pin this rather than take whatever started the test host, so an assertion is about
/// the page and not about how it was run.
let private invocation: string = "fantomas"

let private themed (palette: Palette) : Theme =
    {
        Palette = palette
        Glyphs = GlyphSet.Ascii
    }

let private plainPage: string =
    render (themed Palette.NoColour) invocation Command.Format |> String.concat "\n"

/// Any select graphic rendition sequence, whatever colour it sets.
let private anyEscapeSequence: Regex = Regex(@"\[[0-9;]*m")

[<Test>]
let ``the page lists every flag`` () =
    for flag in
        [
            "--check"
            "--out"
            "--force"
            "--json"
            "--daemon"
            "--verbosity"
            "--version"
            "--help"
        ] do
        plainPage |> shouldContainText flag

let private pageFor (command: Command) : string =
    render (themed Palette.NoColour) invocation command |> String.concat "\n"

[<Test>]
let ``a command's page lists every flag that command accepts, and no other`` () =
    // The property that makes this safe. The page asks `argumentsRefusedBy`, the rule that refuses
    // a flag at run time, so a page offering something the run would refuse cannot happen without
    // the refusal changing too.
    for command in [ Command.Check; Command.Profile; Command.Doctor; Command.Daemon ] do
        let page: string = pageFor command

        for spelling in [ "--out"; "--force"; "--json" ] do
            let accepted: bool =
                argumentFor spelling
                |> Option.map (fun argument -> List.isEmpty (argumentsRefusedBy command [ argument ]))
                |> Option.defaultValue false

            if accepted then
                page |> shouldContainText spelling
            else
                page |> shouldNotContainText spelling

[<Test>]
let ``a command's page leaves out the flag that is its own older spelling`` () =
    // Offering `--check` to someone already running `check` says nothing they can act on.
    pageFor Command.Check |> shouldNotContainText "--check"
    pageFor Command.Daemon |> shouldNotContainText "--daemon"

[<Test>]
let ``a command that takes no paths carries no section about them`` () =
    pageFor Command.Daemon |> shouldNotContainText "A path is a folder"
    pageFor Command.Check |> shouldContainText "A path is a folder"

[<Test>]
let ``only the overview carries the links`` () =
    // Somebody reading a command's page has already found Fantomas and is asking a narrow question
    // about one verb. The links are for somebody still working out what the tool is.
    for command in [ Command.Check; Command.Profile; Command.Doctor; Command.Daemon ] do
        pageFor command |> shouldNotContainText "https://"

    plainPage |> shouldContainText "https://fsprojects.github.io/fantomas/docs"

[<Test>]
let ``a command's page names the command and what it does`` () =
    pageFor Command.Profile |> shouldContainText "profile <paths>"
    pageFor Command.Profile |> shouldContainText "slowest first"

[<Test>]
let ``the doctor page says it takes one file rather than any path`` () =
    // Every other command takes files and folders in any number, so a page telling this one's
    // reader that a folder is searched recursively describes a run it will refuse.
    pageFor Command.Doctor |> shouldContainText "doctor <file>"
    pageFor Command.Doctor |> shouldContainText "One file"
    pageFor Command.Doctor |> shouldNotContainText "A path is a folder"

[<Test>]
let ``the overview lists the older spellings last`` () =
    // They still work and still have to be findable, but nobody reading this page for the first
    // time should meet them before the flags to reach for.
    let page: string = plainPage

    let indexOf (text: string) : int =
        page.IndexOf(text, StringComparison.Ordinal)

    indexOf "--check" |> shouldBeGreaterThan (indexOf "--out")
    indexOf "--check" |> shouldBeGreaterThan (indexOf "--help")
    indexOf "--daemon" |> shouldBeGreaterThan (indexOf "--help")

[<Test>]
let ``the page lists the commands a run can name`` () =
    plainPage |> shouldContainText "check <paths>"
    plainPage |> shouldContainText "profile <paths>"
    plainPage |> shouldContainText "daemon"

[<Test>]
let ``the flags that a command replaced say they are the older spelling`` () =
    // Both keep working, so the page has to say which is which rather than list them twice as
    // though they were different things.
    for flag in [ "--check"; "--daemon" ] do
        plainPage |> shouldContainText flag

    plainPage |> shouldContainText "The older spelling of the check command"
    plainPage |> shouldContainText "The older spelling of the daemon command"

[<Test>]
let ``the page does not offer the flag the profile command replaced`` () =
    plainPage |> shouldNotContainText "--profile"

[<Test>]
let ``the page lists the short forms of the flags that have one`` () =
    plainPage |> shouldContainText "-v"
    plainPage |> shouldContainText "-h"

[<Test>]
let ``the page carries the version`` () =
    let versionNumber: string = CodeFormatter.GetVersion().Split('+').[0]
    plainPage |> shouldContainText versionNumber

[<Test>]
let ``the commit hash is trimmed to the short form git itself shows`` () =
    match CodeFormatter.GetVersion().Split('+') with
    | [| _; commit |] when commit.Length > 9 ->
        plainPage |> shouldContainText (commit.Substring(0, 9))
        plainPage |> shouldNotContainText commit
    | _ -> Assert.Ignore "This build's version carries no commit hash to trim"

[<Test>]
let ``the page links the documentation, the Discord and the llms files`` () =
    for url in
        [
            "https://fsprojects.github.io/fantomas/docs"
            "https://fsprojects.github.io/fantomas/docs/end-users/Configuration.html"
            "https://discord.com/channels/196693847965696000/1493226271767924747"
            "https://fsprojects.github.io/fantomas/llms.txt"
            "https://fsprojects.github.io/fantomas/llms-full.txt"
        ] do
        plainPage |> shouldContainText url

[<Test>]
let ``the page says what an input path may be`` () =
    for extension in [ ".fs"; ".fsi"; ".fsx"; ".ml"; ".mli" ] do
        plainPage |> shouldContainText extension

    plainPage |> shouldContainText ".editorconfig"
    plainPage |> shouldContainText ".fantomasignore"

[<Test>]
let ``a page without colour carries no escape sequences`` () =
    anyEscapeSequence.IsMatch plainPage |> shouldEqual false

[<Test>]
let ``a page with eight bit colour uses the 256 colour codes`` () =
    let page: string =
        render (themed Palette.EightBit) invocation Command.Format |> String.concat "\n"

    // 38;5;38 is the closest 256 colour to the blue the website uses.
    page |> shouldContainText "[1;38;5;38m"

[<Test>]
let ``a page with four bit colour falls back to the basic codes`` () =
    let page: string =
        render (themed Palette.FourBit) invocation Command.Format |> String.concat "\n"

    page |> shouldContainText "[1;36m"
    page |> shouldNotContainText "38;5;"

[<Test>]
let ``colour changes what is written but not what it says`` () =
    let coloured: string =
        render (themed Palette.EightBit) invocation Command.Format |> String.concat "\n"

    anyEscapeSequence.Replace(coloured, "") |> shouldEqual plainPage

[<Test>]
let ``the two column layout lines up whether or not there is colour`` () =
    // The page is laid out in fixed columns, and a decorated string still has to measure as the
    // text it decorates or the right hand column moves.
    let widths (palette: Palette) : int list =
        render (themed palette) invocation Command.Format
        |> List.map (fun (line: string) -> anyEscapeSequence.Replace(line, "").Length)

    widths Palette.EightBit |> shouldEqual (widths Palette.NoColour)
