/// Whether a case is where it says it is. The folders a case sits in are a claim about it: that
/// it contains a node, that it sets a setting and that the setting matters, that it attaches trivia
/// to a node. These checks hold every case to the claim its path makes.
module Fantomas.Core.SnapshotTests.Placement

open System
open System.Reflection
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
    /// A union case whose node class other union cases carry too, such as every case holding a
    /// bare `SingleTextNode`. Nothing in the Oak tells them apart, so only SyntaxOak coverage can
    /// say whether a case has one.
    | Unchecked of reason: string

let private nestedTypes: System.Type array =
    OakFacts.syntaxOakModule.GetNestedTypes(BindingFlags.Public ||| BindingFlags.NonPublic)

/// How many union cases, over every union in the Oak, carry a field of each type, by its full name.
let private unionCaseFieldUse: Map<string, int> =
    nestedTypes
    |> Array.filter FSharpType.IsUnion
    |> Array.collect (fun (union: System.Type) ->
        FSharpType.GetUnionCases(union, true)
        |> Array.collect (fun (case: UnionCaseInfo) ->
            case.GetFields()
            |> Array.map (fun (field: PropertyInfo) -> field.PropertyType.FullName)
        )
    )
    |> Array.countBy id
    |> Map.ofArray

/// Resolve the folders that name a node: `[ "TypeDefn"; "Union" ]` for a union case, or
/// `[ "UnionCase" ]` for a node class, which is the folder name with `Node` after it.
let resolveNodeFolder (folders: string list) : Result<NodeFolder, string> =
    match folders with
    | [] -> Error "The case is not in a folder that names a node."
    | [ name ] ->
        nestedTypes
        |> Array.tryFind (fun (t: System.Type) ->
            (t.Name = name + "Node" || t.Name = name) && typeof<Node>.IsAssignableFrom t
        )
        |> function
            | Some nodeClass -> Ok(NodeClass nodeClass)
            | None -> Error $"`%s{name}` names no node class: there is no `%s{name}Node` in SyntaxOak."
    | [ unionName; caseName ] ->
        match
            nestedTypes
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

        match case.GetFields() with
        | [| field |] when
            typeof<Node>.IsAssignableFrom field.PropertyType
            && Map.tryFind field.PropertyType.FullName unionCaseFieldUse = Some 1
            ->
            Ok(NodeClass field.PropertyType)
        | fields ->
            let carried: string =
                fields
                |> Array.map (fun (field: PropertyInfo) -> field.PropertyType.Name)
                |> String.concat ", "

            Ok(Unchecked $"`%s{unionName}.%s{caseName}` carries %s{carried}, which other union cases carry as well.")
    | folders -> Error $"""`%s{String.concat "/" folders}` is too deep to name a node."""

/// The `FormatConfig` field behind every setting, by the name it is written under.
let private settingTypes: Map<string, System.Type> =
    FSharpType.GetRecordFields(typeof<FormatConfig>)
    |> Array.map (fun (field: PropertyInfo) -> toEditorConfigName field.Name, field.PropertyType)
    |> Map.ofArray

/// What a case's path claims about it.
[<NoComparison; NoEquality>]
type Claim =
    {
        /// The setting the case is about, and the value its value folder names when it has one.
        Setting: (string * string option) option
        Node: NodeFolder
        /// Whether the case sits in a `trivia/` folder.
        IsTrivia: bool
    }

/// Read what a case's folders claim, or why they claim nothing that makes sense.
let claimOf (case: Case.Case) : Result<Claim, string> =
    let folders: string list = case.Folders

    let isTrivia: bool = List.tryLast folders = Some "trivia"

    let nodeFolders (folders: string list) : string list =
        if isTrivia then
            List.take (folders.Length - 1) folders
        else
            folders

    match folders with
    | "oak" :: rest ->
        resolveNodeFolder (nodeFolders rest)
        |> Result.map (fun (node: NodeFolder) ->
            {
                Setting = None
                Node = node
                IsTrivia = isTrivia
            }
        )
    | "settings" :: key :: rest ->
        match Map.tryFind key settingTypes with
        | None -> Error $"`settings/%s{key}` names no setting."
        | Some settingType ->

        let value, nodePath =
            match FSharpType.IsUnion settingType, rest with
            | true, value :: nodePath -> Some value, nodePath
            | _ -> None, rest

        resolveNodeFolder (nodeFolders nodePath)
        |> Result.map (fun (node: NodeFolder) ->
            {
                Setting = Some(key, value)
                Node = node
                IsTrivia = isTrivia
            }
        )
    | top :: _ -> Error $"`%s{top}` is neither `oak` nor `settings`."
    | [] -> Error "The case is not in a folder."

let private propertyValue (case: Case.Case) (key: string) : string option =
    case.Properties
    |> List.tryFindBack (fun (written: string, _) -> String.Equals(written, key, StringComparison.OrdinalIgnoreCase))
    |> Option.map snd

/// Check a case against the claim its path makes. `formatWith` formats the case with a given
/// configuration; it is only called for a case under `settings/`.
let check (case: Case.Case) (formatted: Formatting.Formatted) (formatWith: FormatConfig -> string) : Problem list =
    match claimOf case with
    | Error reason -> [ Problem.UnknownFolder reason ]
    | Ok claim ->

    let oaks: Oak list =
        formatted.Combinations
        |> List.map (fun (each: Formatting.ForDefines) -> each.Oak)

    let settingProblems: Problem list =
        match claim.Setting with
        | None -> []
        | Some(key, folderValue) ->

        match propertyValue case key with
        | None -> [ Problem.SettingNotSet key ]
        | Some written ->

        let valueProblems: Problem list =
            match folderValue with
            | Some folderValue when not (String.Equals(folderValue, written, StringComparison.OrdinalIgnoreCase)) ->
                [ Problem.SettingValueDiffers(key, folderValue, written) ]
            | _ -> []

        // The setting has to matter. Reset it to the default and the result has to change.
        let withoutSetting: FormatConfig =
            case.Properties
            |> List.filter (fun (written: string, _) ->
                not (String.Equals(written, key, StringComparison.OrdinalIgnoreCase))
            )
            |> Case.configOf

        let effectProblems: Problem list =
            if formatWith withoutSetting <> formatted.Merged then
                []
            else
                [ Problem.SettingHasNoEffect key ]

        valueProblems @ effectProblems

    let nodeProblems: Problem list =
        match claim.Node with
        | Unchecked _ -> []
        | NodeClass nodeClass ->

        let visits: OakFacts.Visit list = oaks |> List.collect OakFacts.visits

        let containsNode: bool =
            visits
            |> List.exists (fun (visit: OakFacts.Visit) -> visit.Node.GetType() = nodeClass)

        let containsProblems: Problem list =
            if containsNode then
                []
            else
                [ Problem.NodeMissing nodeClass.Name ]

        let triviaProblems: Problem list =
            if not claim.IsTrivia then
                []
            else

            let attachedHere: bool =
                oaks
                |> List.collect OakFacts.attachments
                |> List.exists (fun (attachment: OakFacts.Attachment) ->
                    attachment.Visit.Node.GetType() = nodeClass
                    || attachment.Visit.Parent
                       |> Option.exists (fun (parent: Node) -> parent.GetType() = nodeClass)
                )

            if attachedHere then
                []
            else
                [ Problem.TriviaNotAttached nodeClass.Name ]

        containsProblems @ triviaProblems

    settingProblems @ nodeProblems
