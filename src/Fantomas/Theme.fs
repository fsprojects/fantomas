module Fantomas.Theme

open System
open System.Text
open System.Text.RegularExpressions
open Spectre.Console

[<RequireQualifiedAccess; Struct>]
type Palette =
    | NoColour
    | FourBit
    | EightBit

[<RequireQualifiedAccess; Struct>]
type GlyphSet =
    | Unicode
    | Ascii

[<Struct>]
type Theme = { Palette: Palette; Glyphs: GlyphSet }

// Select graphic rendition sequences, so a decorated string can still be measured.
let escapeSequence: Regex = Regex(@"\u001b\[[0-9;]*m", RegexOptions.Compiled)

// What `AnsiConsole` answers for is standard out and nothing else. Standard error has to be asked
// through a console of its own, or the two streams share one answer and the split above is only
// half real: `fantomas src > log.txt` from a terminal came back with no colour on the diagnostics,
// on the stream still attached to one. Creating this is also what turns virtual terminal processing
// on for the error handle on Windows, which is the reason the package is here at all.
let errorConsole: Lazy<IAnsiConsole> =
    lazy AnsiConsole.Create(AnsiConsoleSettings(Out = AnsiConsoleOutput(Console.Error)))

let paletteOf (capabilities: Capabilities) (redirected: bool) : Palette =
    // Redirection is decided here rather than left to Spectre. Spectre turns ANSI back on when it
    // detects a CI environment, because a CI log viewer renders escape codes and a progress bar
    // there is worth colouring. What Fantomas prints is not: it gets piped into a file, a pager or
    // a script that reads it, on a build agent as much as anywhere else. The stream being a
    // terminal is the question that has to be answered, so both have to agree.
    let colorsEnabled: bool =
        not redirected
        && capabilities.Ansi
        && capabilities.ColorSystem <> ColorSystem.NoColors

    // `ColorSystem` runs NoColors, Legacy, Standard, EightBit, TrueColor. The four bit fallback is
    // what a console reporting Legacy or Standard gets, which in practice means the Windows legacy
    // console: every TERM a Unix terminal sets, `vt100` and `xterm-mono` included, comes back as
    // EightBit or better. So the branch looks unreachable from a mac or a Linux box and is not.
    // Do not delete it on the strength of a terminal that cannot reach it.
    if not colorsEnabled then
        Palette.NoColour
    elif capabilities.ColorSystem >= ColorSystem.EightBit then
        Palette.EightBit
    else
        Palette.FourBit

// The encoding of the stream being asked about, not the console's. `Console.OutputEncoding` is one
// answer for both streams, and pairing it with one stream's redirection is the same half measure the
// colour detection above had: the question is what this stream will carry.
let detectGlyphs (encoding: Encoding) (redirected: bool) : GlyphSet =
    // 65001 is UTF-8. A console on any other code page turns the nicer characters into mojibake,
    // and a redirected stream is being read by a build log or an agent, where the plain set is
    // what travels. Both carry the same five states rather than one of them carrying none.
    if not redirected && encoding.CodePage = 65001 then
        GlyphSet.Unicode
    else
        GlyphSet.Ascii

let themeOf (capabilities: Capabilities) (encoding: Encoding) (redirected: bool) : Theme =
    {
        Palette = paletteOf capabilities redirected
        Glyphs = detectGlyphs encoding redirected
    }

// What the terminal can do is Spectre.Console's answer to give: it knows the TERM values, it
// honours NO_COLOR, and it reports which colour system is available.
let detect (redirected: bool) : Theme =
    themeOf AnsiConsole.Profile.Capabilities Console.Out.Encoding redirected

let forOutput () : Theme = detect Console.IsOutputRedirected

// Every part of this answers for standard error: its capabilities, its encoding and its
// redirection. Two of the three used to come from standard out.
let forError () : Theme =
    themeOf errorConsole.Value.Profile.Capabilities Console.Error.Encoding Console.IsErrorRedirected

let plain: Theme =
    {
        Palette = Palette.NoColour
        Glyphs = GlyphSet.Ascii
    }

// The palette is written out as escape codes rather than drawn by Spectre, because Spectre wraps
// what it writes to the console width and this output is laid out in fixed columns.
let decorate (theme: Theme) (eightBit: string) (fallback: string) (text: string) : string =
    match theme.Palette with
    | Palette.NoColour -> text
    | Palette.EightBit -> String.Concat("\u001b[", eightBit, "m", text, "\u001b[0m")
    | Palette.FourBit -> String.Concat("\u001b[", fallback, "m", text, "\u001b[0m")

// 38;5;38 is the closest 256 colour to the blue the website uses.
let title (theme: Theme) (text: string) : string = decorate theme "1;38;5;38" "1;36" text

let link (theme: Theme) (text: string) : string = decorate theme "38;5;38" "36" text
let heading (theme: Theme) (text: string) : string = decorate theme "1" "1" text

let flagName (theme: Theme) (text: string) : string = decorate theme "1;38;5;80" "1;36" text

let placeholder (theme: Theme) (text: string) : string = decorate theme "38;5;245" "2" text
let muted (theme: Theme) (text: string) : string = decorate theme "2" "2" text

// The three status colours are the three exit codes: 0, 99 and 1. There are three because there
// are three outcomes, not because three looked balanced.
let positive (theme: Theme) (text: string) : string = decorate theme "32" "32" text
let attention (theme: Theme) (text: string) : string = decorate theme "33" "33" text
let negative (theme: Theme) (text: string) : string = decorate theme "31" "31" text

[<NoComparison; NoEquality>]
type StatusGlyphs =
    {
        Formatted: string
        Unchanged: string
        Ignored: string
        NeedsFormatting: string
        Errored: string
    }

let statusGlyphs (theme: Theme) : StatusGlyphs =
    // Two of the five are the same character either way, chosen ASCII on purpose so the two sets
    // stay recognisably parallel rather than reading as two different designs.
    let glyphs: string * string * string * string * string =
        match theme.Glyphs with
        | GlyphSet.Ascii -> "+", "=", "-", "!", "x"
        | GlyphSet.Unicode -> "✔", "=", "○", "!", "✘"

    let formatted, unchanged, ignored, needsFormatting, errored = glyphs

    {
        Formatted = positive theme formatted
        Unchanged = muted theme unchanged
        Ignored = muted theme ignored
        NeedsFormatting = attention theme needsFormatting
        Errored = negative theme errored
    }

let visibleLength (text: string) : int = escapeSequence.Replace(text, "").Length

let writeRow (write: string -> unit) (column: int) (left: string) (right: string) : unit =
    let padding: string = String(' ', max 1 (column - visibleLength left))
    write (String.Concat(left, padding, right))

let writeContinuation (write: string -> unit) (column: int) (right: string) : unit =
    write (String.Concat(String(' ', column), right))
