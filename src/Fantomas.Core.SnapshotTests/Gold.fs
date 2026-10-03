/// Comparing what came out with the gold file that holds what came out last time.
module Fantomas.Core.SnapshotTests.Gold

open System.IO
open System.Text
open System.Text.RegularExpressions
open Fantomas.Core
open Fantomas.Core.SnapshotTests.Problems

/// Where what came out goes when it differs from a gold: `name.gold.fs` becomes `name.actual.fs`.
let actualPath (goldPath: string) : string =
    let name: string = Path.GetFileName goldPath

    let at: int = name.LastIndexOf(".gold.", System.StringComparison.Ordinal)

    let actualName: string =
        name.Substring(0, at) + ".actual." + name.Substring(at + ".gold.".Length)

    Path.Combine(Path.GetDirectoryName goldPath, actualName)

/// The bytes a gold holds for a result: UTF-8, without a byte order mark.
let bytesOf (actual: string) : byte array = UTF8Encoding(false).GetBytes actual

/// Whether the gold at `goldPath` holds `actual`, byte for byte.
let holds (goldPath: string) (actual: string) : bool =
    File.Exists goldPath && File.ReadAllBytes goldPath = bytesOf actual

/// A gold whose line endings formatting at `endOfLine` never gives: a `\r\n` where every line ends
/// in `\n`, or a `\n` without its `\r` where every line ends in `\r\n`. An editor saves a file with
/// its own line endings, and `.gitattributes` commits them as they are. The golds of an ignored case
/// are written by hand, and one saved that way would never match, so its case would stay ignored
/// after its bug is fixed.
let lineEndingsProblem (endOfLine: EndOfLineStyle) (goldPath: string) : Problem option =
    let text: string = File.ReadAllText goldPath

    match endOfLine with
    | EndOfLineStyle.LF when text.Contains "\r\n" ->
        Some(Problem.GoldLineEndings(Case.relativeToProject goldPath, "\\n"))
    | EndOfLineStyle.CRLF when Regex.IsMatch(text, "(?<!\r)\n") ->
        Some(Problem.GoldLineEndings(Case.relativeToProject goldPath, "\\r\\n"))
    | _ -> None

/// Compare `actual` with the gold at `goldPath`, byte for byte.
///
/// A difference writes `actual` beside the gold as its `.actual` file, to be looked at and renamed over
/// the gold to accept it, and returns the line diff. With `FANTOMAS_UPDATE_SNAPSHOTS=1` the gold is
/// rewritten instead. A match removes an `.actual` left over from an earlier run.
let verify (goldPath: string) (actual: string) : Problem option =
    let actualFile: string = actualPath goldPath

    let encoding: Encoding = UTF8Encoding(false)
    let actualBytes: byte array = bytesOf actual

    let expected: byte array option =
        if File.Exists goldPath then
            Some(File.ReadAllBytes goldPath)
        else
            None

    if expected = Some actualBytes then
        File.Delete actualFile
        None
    elif Case.isUpdating then
        File.WriteAllBytes(goldPath, actualBytes)
        File.Delete actualFile
        None
    else
        File.WriteAllBytes(actualFile, actualBytes)

        let relative: string = Case.relativeToProject goldPath

        match expected with
        | None -> Some(Problem.NoGold relative)
        | Some expected ->

        let byteOrderMark: byte array = Encoding.UTF8.GetPreamble()
        let hasByteOrderMark: bool = expected.Length >= 3 && expected[0..2] = byteOrderMark

        // Decoded without the mark, so a gold that differs in nothing else shows no line diff.
        let expectedText: string =
            if hasByteOrderMark then
                encoding.GetString(expected, 3, expected.Length - 3)
            else
                encoding.GetString expected

        let diff: string = Diff.lineDiff (expectedText.Split '\n') (actual.Split '\n')

        let byteOrderMarkNote: string =
            if hasByteOrderMark then
                "The gold starts with a byte order mark, which formatting never writes.\n"
            else
                ""

        let lineEndingNote: string =
            if expectedText.Contains "\r\n" <> actual.Contains "\r\n" then
                "The gold and the result end their lines differently. The diff shows each carriage return as `\\r`.\n"
            else
                ""

        let note: string = byteOrderMarkNote + lineEndingNote

        Some(Problem.GoldDiffers(relative, note + diff))
