module Fantomas.Tests.ThemeTests

open System
open System.Text.RegularExpressions
open NUnit.Framework
open FsUnitTyped
open Fantomas.Theme

/// Any select graphic rendition sequence, whatever it sets.
let private anyEscapeSequence: Regex = Regex(@"\u001b\[[0-9;]*m")

let private themed (palette: Palette) (glyphs: GlyphSet) : Theme = { Palette = palette; Glyphs = glyphs }

let private allGlyphs (theme: Theme) : string list =
    let g: StatusGlyphs = statusGlyphs theme

    [ g.Formatted; g.Unchanged; g.Ignored; g.NeedsFormatting; g.Errored ]

[<Test>]
let ``a redirected stream takes neither colour nor the nicer glyphs`` () =
    let theme: Theme = detect true

    theme.Palette |> shouldEqual Palette.NoColour
    theme.Glyphs |> shouldEqual GlyphSet.Ascii

[<Test>]
let ``the ascii set is five characters of one cell`` () =
    let glyphs: string list = allGlyphs (themed Palette.NoColour GlyphSet.Ascii)

    glyphs |> shouldEqual [ "+"; "="; "-"; "!"; "x" ]

[<Test>]
let ``the unicode set keeps the two states that were already ascii`` () =
    // `=` and `!` are chosen ascii on purpose, so two of the five are the same character either
    // way and the two schemes stay recognisably parallel rather than reading as two designs.
    let unicode: string list = allGlyphs (themed Palette.NoColour GlyphSet.Unicode)
    let ascii: string list = allGlyphs (themed Palette.NoColour GlyphSet.Ascii)

    List.zip unicode ascii
    |> List.choose (fun (u: string, a: string) -> if u = a then Some u else None)
    |> shouldEqual [ "="; "!" ]

let private bothSets: GlyphSet list = [ GlyphSet.Unicode; GlyphSet.Ascii ]

[<Test>]
let ``every state is a different character, so shape carries the meaning`` () =
    // Nobody has to tell red from green to read the column, which is what makes the fallback and
    // NO_COLOR lossless rather than degraded.
    for glyphSet in bothSets do
        allGlyphs (themed Palette.NoColour glyphSet)
        |> List.distinct
        |> List.length
        |> shouldEqual 5

[<Test>]
let ``without colour a glyph carries no escape sequence`` () =
    for glyphSet in bothSets do
        for glyph in allGlyphs (themed Palette.NoColour glyphSet) do
            anyEscapeSequence.IsMatch glyph |> shouldEqual false

[<Test>]
let ``the three status colours are the three exit codes`` () =
    let g: StatusGlyphs = statusGlyphs (themed Palette.EightBit GlyphSet.Ascii)

    // 0, and nothing to act on.
    g.Formatted |> shouldContainText "[32m"
    // 99, something to act on that did not fail.
    g.NeedsFormatting |> shouldContainText "[33m"
    // 1, a failure.
    g.Errored |> shouldContainText "[31m"

[<Test>]
let ``colour changes what is written but not what it says`` () =
    let coloured: string list = allGlyphs (themed Palette.EightBit GlyphSet.Unicode)
    let plain: string list = allGlyphs (themed Palette.NoColour GlyphSet.Unicode)

    coloured
    |> List.map (fun (glyph: string) -> anyEscapeSequence.Replace(glyph, ""))
    |> shouldEqual plain

[<Test>]
let ``eight bit falls back to the basic codes when only four are available`` () =
    let eightBit: string = link (themed Palette.EightBit GlyphSet.Ascii) "text"
    let fourBit: string = link (themed Palette.FourBit GlyphSet.Ascii) "text"

    eightBit |> shouldContainText "38;5;"
    fourBit |> shouldNotContainText "38;5;"

[<Test>]
let ``a decorated string measures as the text it decorates`` () =
    // The output is laid out in fixed columns, so a coloured left hand column still has to measure
    // as its text or the right hand column moves.
    visibleLength (title (themed Palette.EightBit GlyphSet.Ascii) "Fantomas")
    |> shouldEqual (String.length "Fantomas")

[<Test>]
let ``a two column row starts its right hand column where it was asked to`` () =
    let written: ResizeArray<string> = ResizeArray()
    let theme: Theme = themed Palette.EightBit GlyphSet.Ascii

    writeRow written.Add 20 (flagName theme "--check") "Report which files need formatting."

    let plain: string = anyEscapeSequence.Replace(written.[0], "")
    plain.IndexOf("Report", StringComparison.Ordinal) |> shouldEqual 20

[<Test>]
let ``the plain theme takes neither colour nor the nicer glyphs`` () =
    // What the daemon hands an editor and what a test asserts against, named once rather than built
    // where it is needed.
    plain |> shouldEqual (themed Palette.NoColour GlyphSet.Ascii)
