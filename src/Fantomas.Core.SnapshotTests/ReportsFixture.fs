namespace Fantomas.Core.SnapshotTests

open System
open System.IO
open NUnit.Framework

/// Writes `reports/shapes.md` and `reports/trivia.md` once every test of the run has finished, when
/// `FANTOMAS_SNAPSHOT_REPORTS=1` asks for them, as the `SnapshotReports` pipeline does. The reports
/// cover every case, whatever the run was filtered to. They are no test, so they neither show up in
/// the test count nor fail a run.
[<SetUpFixture>]
type ReportsFixture() =

    [<OneTimeTearDown>]
    member _.WriteReports() : unit =
        if Environment.GetEnvironmentVariable "FANTOMAS_SNAPSHOT_REPORTS" = "1" then
            let shapes, trivia = Reports.render ()
            Directory.CreateDirectory Case.reportsDirectory |> ignore
            File.WriteAllText(Path.Combine(Case.reportsDirectory, "shapes.md"), shapes)
            File.WriteAllText(Path.Combine(Case.reportsDirectory, "trivia.md"), trivia)
