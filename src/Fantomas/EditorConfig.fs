module internal Fantomas.EditorConfig

/// Similar to how to processing of the JSON file happens, this function should the options found in the editor config.
/// The first argument in the return tuple are the options. Listed as (key,value) where key is a member name of the FormatConfig record. Value is either a boolean or an int boxed as object.
/// The second argument in the return tuple are warnings. F.ex. invalid settings
let parseOptionsFromEditorConfig editorConfig : (string * obj) array * string array =
    Array.empty, Array.empty
