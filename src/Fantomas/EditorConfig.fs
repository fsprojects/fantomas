/// Implements loading formatting options from .editorconfig files.
module internal Fantomas.EditorConfig

open EditorConfig.Core

let private ecParser = EditorConfigParser()

let private readConfigFor fileName = ecParser.Parse [|fileName|] |> Seq.exactlyOne

let private tryGetIndentSize (cfg : FileConfiguration) =
    let style = Option.ofNullable cfg.IndentStyle
    let size = Option.ofObj cfg.IndentSize
    match style, size with
    | Some IndentStyle.Space, Some indentSize when not indentSize.UseTabWidth -> Option.ofNullable indentSize.NumberOfColumns
    | _ -> None

let private tryGetIndentSpaceNum (cfg : FileConfiguration) : (string * obj) array =
    match tryGetIndentSize cfg with
    | Some spaces -> [|("IndentSpaceNum", spaces :> obj)|]
    | _ -> [||]

/// Reads supported options from editorconfig's <see cref="FileConfiguration">FileConfiguration</see>.
let toFantomasOptions (cfg : FileConfiguration) : (string * obj) array =
    [|
        yield! (cfg |> tryGetIndentSpaceNum)
        // TODO: Read other options [JB]
    |]

/// Reads the formatting options for the given source file from related editorconfig files.
/// The first argument in the return tuple are the options. Listed as (key,value) where key is a member name of the FormatConfig record. Value is either a boolean or an int boxed as object.
/// The second argument in the return tuple are warnings. F.ex. invalid settings
let readOptionsFromEditorConfig sourceFileName : (string * obj) array * string array =
    let cfg = readConfigFor sourceFileName
    let options = toFantomasOptions cfg
    options, Array.empty
