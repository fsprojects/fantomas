/// Whether a case is where it says it is. The folders a case sits in are a claim about it: that
/// it contains a node, and that it sets a setting and the setting matters. These checks hold every
/// case to the claim its path makes.
module Fantomas.Core.SnapshotTests.Placement

open System
open System.Reflection
open System.Text.RegularExpressions
open Microsoft.FSharp.Reflection
open Fantomas.Core
open Fantomas.Core.SyntaxOak
open Fantomas.EditorConfig
open Fantomas.Core.SnapshotTests.Problems

/// What the folders below `oak/`, or below a setting, name.
[<NoComparison; NoEquality>]
type NodeFolder =
    /// A node class every case in the folder has to contain.
    | NodeClass of System.Type
    /// A union case every case in the folder has to contain: `Expr.Lambda` for `Expr/Lambda/`.
    | UnionCase of union: System.Type * caseName: string
    /// A folder that names no node, such as `scenarios/<old test file>/`.
    | Unchecked of reason: string

/// Resolve the folders that name a node: `[ "TypeDefn"; "Union" ]` for a union case, or
/// `[ "UnionCase" ]` for a node class, which is the folder name with `Node` after it.
let resolveNodeFolder (folders: string list) : Result<NodeFolder, string> =
    match folders with
    | [] -> Error "The case is not in a folder that names a node."
    | [ name ] ->
        OakFacts.syntaxOakTypes
        |> Array.tryFind (fun (t: System.Type) ->
            (t.Name = name + "Node" || t.Name = name) && typeof<Node>.IsAssignableFrom t
        )
        |> function
            // No node is an instance of an abstract class itself, so no case could ever pass.
            | Some nodeClass when nodeClass.IsAbstract ->
                Error
                    $"`%s{name}` names `%s{nodeClass.Name}`, which is abstract: the case goes in the folder of the node class it contains."
            | Some nodeClass -> Ok(NodeClass nodeClass)
            | None -> Error $"`%s{name}` names no node class: there is no `%s{name}Node` in SyntaxOak."
    | [ unionName; caseName ] ->
        match
            OakFacts.syntaxOakTypes
            |> Array.tryFind (fun (t: System.Type) -> t.Name = unionName && FSharpType.IsUnion t)
        with
        | None -> Error $"`%s{unionName}` is no union in SyntaxOak."
        | Some union ->

        match
            FSharpType.GetUnionCases(union, true)
            |> Array.tryFind (fun (case: UnionCaseInfo) -> case.Name = caseName)
        with
        | None -> Error $"`%s{unionName}` has no case `%s{caseName}`."
        | Some case ->

        Ok(UnionCase(union, case.Name))
    | folders ->
        Error
            $"""`%s{String.concat "/" folders}` is too deep to name a node, which is `<Union>/<Case>/` or `<Node>/`: "Where a case goes" in the README lists the folders around it."""

/// The `FormatConfig` field behind every setting, by the name it is written under.
let private settingTypes: Map<string, System.Type> =
    FSharpType.GetRecordFields(typeof<FormatConfig>)
    |> Array.map (fun (field: PropertyInfo) -> toEditorConfigName field.Name, field.PropertyType)
    |> Map.ofArray

/// Whether a setting takes a value, as an `.editorconfig` would spell it.
let private isValue (key: string) (value: string) : bool =
    try
        parseOptionsFromEditorConfig Case.defaultConfig (readOnlyDict [ key, value ])
        |> snd
        |> List.isEmpty
    with _ ->
        false

/// The values of a setting that have a folder of their own: both of a switch, and every named value
/// a setting can take. A number has none, as there are too many to pick from.
let private folderValues (key: string) (settingType: System.Type) : string list =
    if settingType = typeof<bool> then
        [ "true"; "false" ]
    elif FSharpType.IsUnion settingType then
        FSharpType.GetUnionCases settingType
        |> Array.choose (fun (case: UnionCaseInfo) ->
            let value: string =
                Regex.Replace(case.Name, "(?<=[a-z])(?=[A-Z])", "_").ToLowerInvariant()

            if isValue key value then Some value else None
        )
        |> Array.toList
    else
        []

/// What a case's path claims about it.
[<NoComparison; NoEquality>]
type Claim =
    {
        /// The setting the case is about, and the value its value folder names when it has one: every
        /// switch and setting with named values has one, a number none.
        Setting: (string * string option) option
        /// The values the setting is tried at besides the case's own: the default for a case at another
        /// value, and every other value for a case at the default.
        OtherValues: string list
        Node: NodeFolder
        /// Whether the case sits in a `negative/` folder: one formatting must leave as it is, below a
        /// node, or one a setting must leave alone, below a setting. Such a case is its own gold.
        IsNegative: bool
    }

/// Read what a case's folders claim, or why they claim nothing that makes sense.
let claimOf (case: Case.Case) : Result<Claim, string> =
    let folders: string list = case.Folders

    // Below a node, `negative/` comes last: `oak/TypeDefn/Union/trivia/negative/`.
    let isNegativeLast: bool =
        List.tryHead folders = Some "oak" && List.tryLast folders = Some "negative"

    let folders: string list =
        if isNegativeLast then
            List.take (folders.Length - 1) folders
        else
            folders

    let isTrivia: bool = List.tryLast folders = Some "trivia"

    let nodeFolders (folders: string list) : string list =
        if isTrivia then
            List.take (folders.Length - 1) folders
        else
            folders

    match folders with
    // Inputs shaped by what users ran into, settings combined where they meet, in the folder of the
    // file of `Fantomas.Core.Tests` they come from. That file is about a topic, not one node or one
    // setting, so the folders claim nothing more.
    | "scenarios" :: _ ->
        Ok
            {
                Setting = None
                OtherValues = []
                Node = Unchecked "a scenario from Fantomas.Core.Tests, in the folder of the file it came from"
                IsNegative = List.tryLast folders = Some "negative"
            }
    | "oak" :: rest ->
        resolveNodeFolder (nodeFolders rest)
        |> Result.map (fun (node: NodeFolder) ->
            {
                Setting = None
                OtherValues = []
                Node = node
                IsNegative = isNegativeLast
            }
        )
    | "settings" :: key :: rest ->
        match Map.tryFind key settingTypes with
        | None -> Error $"`settings/%s{key}` names no setting."
        | Some settingType ->

        // A switch or a setting with named values has its value as the first folder.
        let values: string list = folderValues key settingType

        let value: Result<string option * string list, string> =
            match values, rest with
            | [], _ -> Ok(None, rest)
            | values, value :: afterValue when List.contains value values -> Ok(Some value, afterValue)
            | values, folder :: _ ->
                Error
                    $"""`%s{key}` has a folder per value, %s{values |> List.map (sprintf "`%s/`") |> String.concat ", "}, and `%s{folder}` is none of them."""
            | _, [] -> Error $"`%s{key}` has a folder per value, and the case is in none."

        match value with
        | Error reason -> Error reason
        | Ok(value, afterValue) ->

        let isNegative, nodePath =
            match afterValue with
            | "negative" :: nodePath -> true, nodePath
            | nodePath -> false, nodePath

        let defaultValue: string option =
            settingValues Case.defaultConfig
            |> List.tryFind (fun (name: string, _) -> name = key)
            |> Option.map snd

        let otherValues: string list =
            match value with
            | Some value when Some value = defaultValue -> values |> List.filter (fun (other: string) -> other <> value)
            | _ -> Option.toList defaultValue

        resolveNodeFolder (nodeFolders nodePath)
        |> Result.map (fun (node: NodeFolder) ->
            {
                Setting = Some(key, value)
                OtherValues = otherValues
                Node = node
                IsNegative = isNegative
            }
        )
    | top :: _ -> Error $"`%s{top}` is none of `oak`, `settings` and `scenarios`."
    | [] -> Error "The case is not in a folder."

let private propertyValue (case: Case.Case) (key: string) : string option =
    case.Properties
    |> List.tryFindBack (fun (written: string, _) -> String.Equals(written, key, StringComparison.OrdinalIgnoreCase))
    |> Option.map snd

/// What a case's folders ask of its input alone, whatever formatting gives: that they claim
/// something that makes sense, and that the setting they name is set, to its value folder when there
/// is one, and to other than its default when there is none.
let inputProblems (case: Case.Case) : Problem list =
    match claimOf case with
    | Error reason -> [ Problem.UnknownFolder reason ]
    | Ok claim ->

    match claim.Setting with
    | None -> []
    | Some(key, folderValue) ->

    match propertyValue case key, folderValue with
    | None, _ -> [ Problem.SettingNotSet key ]
    | Some written, Some folderValue when not (String.Equals(folderValue, written, StringComparison.OrdinalIgnoreCase)) ->
        [ Problem.SettingValueDiffers(key, folderValue, written) ]
    | Some written, None when Case.configOf [ key, written ] = Case.defaultConfig -> [ Problem.SettingAtDefault key ]
    | Some _, _ -> []

/// Check a case against the claim its path makes. `formatWith` formats the case with a given
/// configuration; it is only called for a case under `settings/`.
let check (case: Case.Case) (formatted: Formatting.Formatted) (formatWith: FormatConfig -> string) : Problem list =
    match claimOf case with
    | Error reason -> [ Problem.UnknownFolder reason ]
    | Ok claim ->

    let oaks: Oak list =
        formatted.Combinations
        |> List.map (fun (each: Formatting.FormattedCombination) -> each.Oak)

    let inputProblems: Problem list = inputProblems case

    let settingProblems: Problem list =
        match claim.Setting with
        | None -> []
        | Some(key, _) ->

        // A setting the input does not set the way its folders say is that one mistake, and not
        // also a setting that changes nothing.
        if not inputProblems.IsEmpty then
            []
        else

        // The setting has to matter: at another value, the result has to change.
        let others: (string * string) list =
            claim.OtherValues
            |> List.map (fun (other: string) ->
                let config: FormatConfig =
                    case.Properties
                    |> List.map (fun (written: string, value: string) ->
                        if String.Equals(written, key, StringComparison.OrdinalIgnoreCase) then
                            written, other
                        else
                            written, value
                    )
                    |> Case.configOf

                other, formatWith config
            )

        // A case under `negative/` is the opposite: its input comes back unchanged at every other
        // value too. That it comes back unchanged at its own is checked for every negative case.
        if claim.IsNegative then
            match others |> List.tryFind (fun (_, result: string) -> result <> case.Source) with
            | Some(other, result) when formatted.Merged = case.Source -> [ Problem.SettingApplies(key, other, result) ]
            | _ -> []
        elif others |> List.exists (fun (_, result: string) -> result <> formatted.Merged) then
            []
        else
            [ Problem.SettingHasNoEffect(key, claim.OtherValues) ]

    let nodeProblems: Problem list =
        match claim.Node with
        | Unchecked _ -> []
        | UnionCase(union, caseName) ->
            let holds: bool =
                oaks
                |> List.exists (fun (oak: Oak) -> OakFacts.unionCases oak |> List.contains (union, caseName))

            if holds then
                []
            else
                [ Problem.NodeMissing $"%s{union.Name}.%s{caseName}" ]
        | NodeClass nodeClass ->

        let visits: OakFacts.Visit list = oaks |> List.collect OakFacts.visits

        let containsNode: bool =
            visits
            |> List.exists (fun (visit: OakFacts.Visit) -> visit.Node.GetType() = nodeClass)

        if containsNode then
            []
        else
            [ Problem.NodeMissing nodeClass.Name ]

    // A negative case is its own gold, so its result must be its input. Any other case has to earn
    // its gold: a result that is the input unchanged says nothing a gold could add.
    // A result that only ends differently, with a final newline added say, earns no gold either,
    // unless ending a file is the point of the case: one about `insert_final_newline`, or one that
    // sets it to other than its default.
    let onlyEndChanged: bool =
        formatted.Merged <> case.Source
        && formatted.Merged.TrimEnd() = case.Source.TrimEnd()
        && claim.Setting
           |> Option.forall (fun (key: string, _) -> key <> "insert_final_newline")
        && case.Config.InsertFinalNewline = Case.defaultConfig.InsertFinalNewline

    let keptProblems: Problem list =
        match claim.IsNegative, formatted.Merged = case.Source with
        | true, false -> [ Problem.InputNotKept formatted.Merged ]
        | false, true -> [ Problem.AlreadyFormatted ]
        | false, false when onlyEndChanged -> [ Problem.OnlyEndChanged ]
        | _ -> []

    inputProblems @ keptProblems @ settingProblems @ nodeProblems
