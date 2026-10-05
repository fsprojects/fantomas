/// Output compared with a file that holds what it was last time, the way the gold files of
/// `Fantomas.Core.SnapshotTests` are, with its line diff.
module Fantomas.Tests.Snapshot

open System
open System.IO
open NUnit.Framework
open Fantomas.Core.SnapshotTests
open Fantomas.Tests.TestHelpers

/// Compare `actual` with the gold at `gold`.
///
/// A mismatch fails with a line diff and writes what came out beside the gold, `name.gold` as
/// `name.actual`, to be looked at and renamed over the gold to accept it. With
/// `FANTOMAS_UPDATE_SNAPSHOTS=1` the gold is rewritten instead, which is what the `UpdateSnapshots`
/// pipeline of `build.fsx` does.
let verify (gold: string) (actual: string) : unit =
    let actualFile: string = Path.ChangeExtension(gold, ".actual")
    let actual: string = String.normalizeNewLine actual

    let expected: string option =
        if File.Exists gold then
            Some(File.ReadAllText gold |> String.normalizeNewLine)
        else
            None

    if expected = Some actual then
        File.Delete actualFile
    elif Environment.GetEnvironmentVariable "FANTOMAS_UPDATE_SNAPSHOTS" = "1" then
        File.WriteAllText(gold, actual)
        File.Delete actualFile
    else
        File.WriteAllText(actualFile, actual)

        match expected with
        | None -> Assert.Fail $"There is no gold at %s{gold} yet. What came out is in %s{actualFile}."
        | Some expected ->

        let diff: string = Diff.lineDiff (expected.Split '\n') (actual.Split '\n')
        Assert.Fail $"The output differs from %s{gold}. What came out is in %s{actualFile}.\n\n%s{diff}"
