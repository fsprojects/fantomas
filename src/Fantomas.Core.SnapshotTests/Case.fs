/// One case under `cases/`: where it lives, what its front matter says, and the source it holds.
module Fantomas.Core.SnapshotTests.Case

open System
open System.IO
open System.Text.RegularExpressions
open Fantomas.Core
open Fantomas.EditorConfig

let projectDirectory: string = __SOURCE_DIRECTORY__

let casesDirectory: string = Path.Combine(__SOURCE_DIRECTORY__, "cases")

let reportsDirectory: string = Path.Combine(__SOURCE_DIRECTORY__, "reports")

/// `FANTOMAS_UPDATE_SNAPSHOTS=1` rewrites a gold that differs instead of failing on it, which is what
/// the `UpdateSnapshots` pipeline of `build.fsx` sets.
let isUpdating: bool =
    Environment.GetEnvironmentVariable "FANTOMAS_UPDATE_SNAPSHOTS" = "1"

/// A path relative to `cases/`, with forward slashes on every platform, which is how a case is named.
let relativeToCases (path: string) : string =
    Path.GetRelativePath(casesDirectory, path).Replace('\\', '/')

/// A path relative to the project, with forward slashes, which is how a problem names a file.
let relativeToProject (path: string) : string =
    Path.GetRelativePath(projectDirectory, path).Replace('\\', '/')

/// A case file name is lower case words joined by dashes, an issue number first when it has one.
/// No dots, so that `name.gold.fs` and `name.DEBUG.gold.fs` can only ever belong to `name.fs`.
let private caseName: Regex = Regex("^[a-z0-9]+(-[a-z0-9]+)*$")

/// Whether a file is a case rather than a gold or an `.actual`: an F# file whose name, without its
/// extension, has no dot in it.
let isCaseFile (path: string) : bool =
    let extension: string = Path.GetExtension path

    (extension = ".fs" || extension = ".fsi")
    && not (Path.GetFileNameWithoutExtension(path).Contains '.')

/// Every case, as a path relative to `cases/`, in a stable order.
let all () : string array =
    if not (Directory.Exists casesDirectory) then
        Array.empty
    else

    Directory.GetFiles(casesDirectory, "*", SearchOption.AllDirectories)
    |> Array.choose (fun (path: string) -> if isCaseFile path then Some(relativeToCases path) else None)
    |> Array.sort

type Case =
    {
        /// Relative to `cases/`, with forward slashes: `oak/TypeDefn/Union/single-case.fs`.
        RelativePath: string
        FullPath: string
        /// The folders between `cases/` and the file.
        Folders: string list
        /// The file name without its extension.
        Stem: string
        /// `.fs` or `.fsi`.
        Extension: string
        IsSignature: bool
        /// The front matter, as written, in the order written.
        Properties: (string * string) list
        Config: FormatConfig
        /// What gets formatted: the file without its front matter, with every line ending made `\n`.
        /// A case written on Windows formats the same as one written anywhere else; what line
        /// endings come out is up to `end_of_line`.
        Source: string
    }

/// The configuration a case formats with when its front matter sets nothing. `end_of_line` is fixed
/// to `lf` rather than left to the machine, so a gold reads the same on every platform.
let defaultConfig: FormatConfig =
    { FormatConfig.Default with
        EndOfLine = EndOfLineStyle.LF
    }

/// Read the properties into a configuration, failing on anything Fantomas cannot act on. A key that is
/// no setting at all fails too, where the tool would pass over it as belonging to another tool: in a
/// case it can only be a mistake.
let configOf (properties: (string * string) list) : FormatConfig =
    let unknown: string list =
        let isSetting (key: string) : bool =
            List.exists
                (fun (setting: string) -> String.Equals(setting, key, StringComparison.OrdinalIgnoreCase))
                supportedSettings

        properties
        |> List.choose (fun (key: string, _) -> if isSetting key then None else Some key)

    if not unknown.IsEmpty then
        failwith $"""The front matter sets what is not a setting: %s{String.concat ", " unknown}."""

    let config, problems =
        parseOptionsFromEditorConfig defaultConfig (readOnlyDict properties)

    if not problems.IsEmpty then
        failwith $"The front matter has settings Fantomas cannot act on: %A{problems}"

    config

let private frontMatterStart: string = "(*---"

let private frontMatterEnd: string = "---*)"

/// Split a case file into its front matter properties and the source after it.
let splitFrontMatter (text: string) : (string * string) list * string =
    if not (text.StartsWith(frontMatterStart, StringComparison.Ordinal)) then
        [], text
    else

    let close: int = text.IndexOf(frontMatterEnd, StringComparison.Ordinal)

    if close < 0 then
        failwith $"The front matter opens with `%s{frontMatterStart}` and never closes with `%s{frontMatterEnd}`."

    let inside: string =
        text.Substring(frontMatterStart.Length, close - frontMatterStart.Length)

    let afterClose: int = close + frontMatterEnd.Length

    // The newline that ends the front matter belongs to it, so the source starts on the next line.
    let sourceStart: int =
        if text.Substring(afterClose).StartsWith("\n", StringComparison.Ordinal) then
            afterClose + 1
        else
            afterClose

    let properties: (string * string) list =
        inside.Split('\n')
        |> Array.choose (fun (line: string) ->
            let line: string = line.Trim()

            // `#` and `;` start a comment in an `.editorconfig`, and `#` lines are the description.
            let isComment: bool =
                line.StartsWith("#", StringComparison.Ordinal)
                || line.StartsWith(";", StringComparison.Ordinal)

            if String.IsNullOrEmpty line || isComment then
                None
            else
                Some line
        )
        |> Array.map (fun (line: string) ->
            match line.IndexOf '=' with
            | -1 -> failwith $"The front matter line `%s{line}` is not `key = value`."
            | at -> line.Substring(0, at).Trim(), line.Substring(at + 1).Trim()
        )
        |> Array.toList

    properties, text.Substring sourceStart

let read (relativePath: string) : Case =
    let fullPath: string = Path.Combine(casesDirectory, relativePath)
    let stem: string = Path.GetFileNameWithoutExtension fullPath

    if not (caseName.IsMatch stem) then
        failwith
            $"`%s{stem}` is not a case name: lower case words joined by dashes, an issue number first when there is one."

    let extension: string = Path.GetExtension fullPath

    let properties, source =
        splitFrontMatter ((File.ReadAllText fullPath).Replace("\r\n", "\n"))

    {
        RelativePath = relativePath
        FullPath = fullPath
        Folders =
            relativePath.Split('/')
            |> Array.toList
            |> List.take (relativePath.Split('/').Length - 1)
        Stem = stem
        Extension = extension
        IsSignature = extension = ".fsi"
        Properties = properties
        Config = configOf properties
        Source = source
    }

/// The name a define combination gets in a gold file name: `no-defines` for the empty one, the
/// defines joined by `+` otherwise. `no-defines` cannot be a define, which has no dash in it.
let combinationName (defines: string list) : string =
    if List.isEmpty defines then
        "no-defines"
    else
        defines |> List.sort |> String.concat "+"

/// Where the merged result of a case is kept.
let goldPath (case: Case) : string =
    Path.Combine(Path.GetDirectoryName case.FullPath, $"%s{case.Stem}.gold%s{case.Extension}")

/// Where the result for one define combination is kept.
let defineGoldPath (case: Case) (defines: string list) : string =
    Path.Combine(
        Path.GetDirectoryName case.FullPath,
        $"%s{case.Stem}.%s{combinationName defines}.gold%s{case.Extension}"
    )

/// Every gold file beside a case that belongs to it, whether or not it should still be there.
let existingGolds (case: Case) : string list =
    Directory.GetFiles(Path.GetDirectoryName case.FullPath, $"%s{case.Stem}.*")
    |> Array.filter (fun (path: string) ->
        let name: string = Path.GetFileName path

        name.EndsWith($".gold%s{case.Extension}", StringComparison.Ordinal)
        && name.StartsWith($"%s{case.Stem}.", StringComparison.Ordinal)
    )
    |> Array.sort
    |> Array.toList
