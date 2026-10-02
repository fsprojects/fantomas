/// Comparing what came out with the gold file that holds what came out last time.
module Fantomas.Core.SnapshotTests.Gold

open System.IO
open Fantomas.Core.SnapshotTests.Problems

/// Where what came out goes when it differs from a gold: `name.gold.fs` becomes `name.actual.fs`.
let actualPath (goldPath: string) : string =
    let name: string = Path.GetFileName goldPath

    let at: int = name.LastIndexOf(".gold.", System.StringComparison.Ordinal)

    let actualName: string =
        name.Substring(0, at) + ".actual." + name.Substring(at + ".gold.".Length)

    Path.Combine(Path.GetDirectoryName goldPath, actualName)

/// Compare `actual` with the gold at `goldPath`, byte for byte.
///
/// A difference writes `actual` beside the gold as its `.actual` file, to be looked at and renamed over
/// the gold to accept it, and returns the line diff. With `FANTOMAS_UPDATE_SNAPSHOTS=1` the gold is
/// rewritten instead. A match removes an `.actual` left over from an earlier run.
let verify (goldPath: string) (actual: string) : Problem option =
    let actualFile: string = actualPath goldPath

    let expected: string option =
        if File.Exists goldPath then
            Some(File.ReadAllText goldPath)
        else
            None

    if expected = Some actual then
        File.Delete actualFile
        None
    elif Case.isUpdating then
        File.WriteAllText(goldPath, actual)
        File.Delete actualFile
        None
    else
        File.WriteAllText(actualFile, actual)

        let relative: string = Case.relativeToProject goldPath

        match expected with
        | None -> Some(Problem.NoGold relative)
        | Some expected ->

        Some(Problem.GoldDiffers(relative, Diff.lineDiff (expected.Split '\n') (actual.Split '\n')))
