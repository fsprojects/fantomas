module Fantomas.Tests.HelpPageTests

open System.Text.RegularExpressions
open NUnit.Framework
open FsUnitTyped
open Fantomas.Core
open Fantomas.Theme
open Fantomas.HelpPage

/// The page never draws a status glyph, so only the palette varies here.
let private themed (palette: Palette) : Theme =
    {
        Palette = palette
        Glyphs = GlyphSet.Ascii
    }

let private plainPage: string =
    render (themed Palette.NoColour) |> String.concat "\n"

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

[<Test>]
let ``the page lists the commands a run can name`` () =
    plainPage |> shouldContainText "profile <paths>"

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
    let page: string = render (themed Palette.EightBit) |> String.concat "\n"

    // 38;5;38 is the closest 256 colour to the blue the website uses.
    page |> shouldContainText "[1;38;5;38m"

[<Test>]
let ``a page with four bit colour falls back to the basic codes`` () =
    let page: string = render (themed Palette.FourBit) |> String.concat "\n"

    page |> shouldContainText "[1;36m"
    page |> shouldNotContainText "38;5;"

[<Test>]
let ``colour changes what is written but not what it says`` () =
    let coloured: string = render (themed Palette.EightBit) |> String.concat "\n"

    anyEscapeSequence.Replace(coloured, "") |> shouldEqual plainPage

[<Test>]
let ``the two column layout lines up whether or not there is colour`` () =
    // The page is laid out in fixed columns, and a decorated string still has to measure as the
    // text it decorates or the right hand column moves.
    let widths (palette: Palette) : int list =
        render (themed palette)
        |> List.map (fun (line: string) -> anyEscapeSequence.Replace(line, "").Length)

    widths Palette.EightBit |> shouldEqual (widths Palette.NoColour)
