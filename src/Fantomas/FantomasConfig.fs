module Fantomas.FantomasConfig

open System
open System.IO
open Fantomas.FormatConfig
open Newtonsoft.Json

module Dto =

    /// DTO for FormatConfig (we want to keep this separated from FormatConfig in case we change the FormatConfig internal type)
    [<CLIMutable>]
    type FantomasConfig = 
        { IndentSpaceNum : Num
          PageWidth : Num
          PreserveEndOfLine : bool
          SemicolonAtEndOfLine : bool
          SpaceBeforeArgument : bool
          SpaceBeforeColon : bool
          SpaceAfterComma : bool
          SpaceAfterSemicolon : bool
          IndentOnTryWith : bool
          ReorderOpenDeclaration : bool
          SpaceAroundDelimiter : bool
          StrictMode : bool }

    /// Maps FantomasConfig -> FormatConfig
    let internal toFormatConfig (cfg: FantomasConfig) : FormatConfig =
        { IndentSpaceNum         = cfg.IndentSpaceNum
          PageWidth              = cfg.PageWidth
          PreserveEndOfLine      = cfg.PreserveEndOfLine
          SemicolonAtEndOfLine   = cfg.SemicolonAtEndOfLine
          SpaceBeforeArgument    = cfg.SpaceBeforeArgument
          SpaceBeforeColon       = cfg.SpaceBeforeColon
          SpaceAfterComma        = cfg.SpaceAfterComma
          SpaceAfterSemicolon    = cfg.SpaceAfterSemicolon
          IndentOnTryWith        = cfg.IndentOnTryWith
          ReorderOpenDeclaration = cfg.ReorderOpenDeclaration
          SpaceAroundDelimiter   = cfg.SpaceAroundDelimiter
          StrictMode             = cfg.StrictMode }

    /// Maps FormatConfig -> FantomasConfig
    let internal fromFormatConfig (cfg: FormatConfig) : FantomasConfig = 
        { IndentSpaceNum         = cfg.IndentSpaceNum
          PageWidth              = cfg.PageWidth
          PreserveEndOfLine      = cfg.PreserveEndOfLine
          SemicolonAtEndOfLine   = cfg.SemicolonAtEndOfLine
          SpaceBeforeArgument    = cfg.SpaceBeforeArgument
          SpaceBeforeColon       = cfg.SpaceBeforeColon
          SpaceAfterComma        = cfg.SpaceAfterComma
          SpaceAfterSemicolon    = cfg.SpaceAfterSemicolon
          IndentOnTryWith        = cfg.IndentOnTryWith
          ReorderOpenDeclaration = cfg.ReorderOpenDeclaration
          SpaceAroundDelimiter   = cfg.SpaceAroundDelimiter
          StrictMode             = cfg.StrictMode }

    /// The default configuration as a DTO
    let internal defaultConfig() =
        fromFormatConfig FormatConfig.Default

type LoadedConfig =
    {
        Warnings: string[]
        FileName: string
        FantomasConfig: Dto.FantomasConfig
        FormatConfig: FormatConfig
    }

/// The settings used for both serialization and deserialization
let internal settings = 
    JsonSerializerSettings(Formatting = Formatting.Indented, MissingMemberHandling = MissingMemberHandling.Error)

/// Makes a new JsonSerializerSettings with warning output to the console for missing members
let internal mkSettingsWithWarning missingMemberHandlerFn =
    let settings = JsonSerializerSettings(Formatting = Formatting.Indented, MissingMemberHandling = MissingMemberHandling.Error)
    settings.Error <- EventHandler<_>(fun _sender args -> 
        match args.ErrorContext.Error.Message.Contains "Could not find member" with
        | true -> 
            missingMemberHandlerFn args
            args.ErrorContext.Handled <- true
        | false ->
            ()
    )
    settings

/// Serializes any object to a string
let internal serialize o = 
    JsonConvert.SerializeObject(o, settings)

/// Deserializes any string to an object
let internal deserialize<'t> json =
    let warnings = ResizeArray<_>()
    let res = JsonConvert.DeserializeObject<'t>(json, mkSettingsWithWarning (fun x -> warnings.Add x.ErrorContext.Error.Message))
    res, warnings.ToArray()

/// Populates (mutates) an object with the specified json and then returns the mutated object
let internal populate<'t> (target: 't) json = 
    let warnings = ResizeArray<_>()
    JsonConvert.PopulateObject(json, target, mkSettingsWithWarning (fun x -> warnings.Add x.ErrorContext.Error.Message))
    target, warnings.ToArray()

/// Tries to load a config file from disk
let tryLoad fileName =
    try
        match File.Exists fileName with
        | true ->
            fileName
            |> File.ReadAllText 
            |> populate (Dto.defaultConfig())
            |> fun (cfg, warnings) -> 
                { FileName = fileName
                  Warnings = warnings
                  FantomasConfig = cfg
                  FormatConfig = Dto.toFormatConfig cfg }
            |> Ok
        | false -> 
            Error(sprintf "File does not exist '%s'" fileName)
    with ex -> 
        Error(ex.Message)

/// Unwraps tryLoad and throws an exception if there is an error
let load configFilename =
    match tryLoad configFilename with
    | Ok x -> x
    | Error x -> failwith x

/// Saves the specified config to the specified file name
let save configFilename config =
    let item = Dto.fromFormatConfig config
    let json = serialize item
    File.WriteAllText(configFilename, json)

/// Recursively tries to find a config file either by a file name or directory path
let rec tryFindConfig fileOrDirectoryPath =
    let fileName = Path.Combine(fileOrDirectoryPath, ".fantomas-config")
    match File.Exists fileName with
    | true -> Some fileName
    | false -> 
        match Path.GetDirectoryName(fileOrDirectoryPath) with
        | null -> None
        | dir -> tryFindConfig dir

/// Calls tryFindConfig and then load
let tryFindAndLoadConfig fileOrDirectoryPath =
    fileOrDirectoryPath
    |> tryFindConfig
    |> Option.map tryLoad
