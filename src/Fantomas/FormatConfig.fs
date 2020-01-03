module Fantomas.FormatConfig

open System

let SAT_SOLVE_MAX_STEPS = 100

type FormatException(msg : string) =
    inherit Exception(msg)

type Num = int

type FormatConfig = 
    { /// Number of spaces for each indentation
      IndentSpaceNum : Num
      /// The column where we break to new lines
      PageWidth : Num
      SemicolonAtEndOfLine : bool
      SpaceBeforeArgument : bool
      SpaceBeforeColon : bool
      SpaceAfterComma : bool
      SpaceAfterSemicolon : bool
      IndentOnTryWith : bool
      /// Reordering and deduplicating open statements
      ReorderOpenDeclaration : bool
      SpaceAroundDelimiter : bool
      KeepNewlineAfter : bool
      MaxIfThenElseShortWidth: Num
      /// Prettyprinting based on ASTs only
      StrictMode : bool }

    static member Default = 
        { IndentSpaceNum = 4
          PageWidth = 120
          SemicolonAtEndOfLine = false
          SpaceBeforeArgument = true
          SpaceBeforeColon = false
          SpaceAfterComma = true
          SpaceAfterSemicolon = true
          IndentOnTryWith = false
          ReorderOpenDeclaration = false
          SpaceAroundDelimiter = true
          KeepNewlineAfter = false
          MaxIfThenElseShortWidth = 40
          StrictMode = false }

    static member create(indentSpaceNum, pageWith, semicolonAtEndOfLine, 
                         spaceBeforeArgument, spaceBeforeColon, spaceAfterComma, 
                         spaceAfterSemicolon, indentOnTryWith, reorderOpenDeclaration) =
        { FormatConfig.Default with
              IndentSpaceNum = indentSpaceNum; 
              PageWidth = pageWith;
              SemicolonAtEndOfLine = semicolonAtEndOfLine; 
              SpaceBeforeArgument = spaceBeforeArgument; 
              SpaceBeforeColon = spaceBeforeColon;
              SpaceAfterComma = spaceAfterComma; 
              SpaceAfterSemicolon = spaceAfterSemicolon; 
              IndentOnTryWith = indentOnTryWith; 
              ReorderOpenDeclaration = reorderOpenDeclaration }

    static member create(indentSpaceNum, pageWith, semicolonAtEndOfLine, 
                         spaceBeforeArgument, spaceBeforeColon, spaceAfterComma, 
                         spaceAfterSemicolon, indentOnTryWith, reorderOpenDeclaration, spaceAroundDelimiter) =
        { FormatConfig.Default with
              IndentSpaceNum = indentSpaceNum; 
              PageWidth = pageWith;
              SemicolonAtEndOfLine = semicolonAtEndOfLine; 
              SpaceBeforeArgument = spaceBeforeArgument; 
              SpaceBeforeColon = spaceBeforeColon;
              SpaceAfterComma = spaceAfterComma; 
              SpaceAfterSemicolon = spaceAfterSemicolon; 
              IndentOnTryWith = indentOnTryWith; 
              ReorderOpenDeclaration = reorderOpenDeclaration;
              SpaceAroundDelimiter = spaceAroundDelimiter }

    static member create(indentSpaceNum, pageWith, semicolonAtEndOfLine, 
                         spaceBeforeArgument, spaceBeforeColon, spaceAfterComma, 
                         spaceAfterSemicolon, indentOnTryWith, reorderOpenDeclaration, 
                         spaceAroundDelimiter, strictMode) =
        { FormatConfig.Default with
              IndentSpaceNum = indentSpaceNum; 
              PageWidth = pageWith;
              SemicolonAtEndOfLine = semicolonAtEndOfLine; 
              SpaceBeforeArgument = spaceBeforeArgument; 
              SpaceBeforeColon = spaceBeforeColon;
              SpaceAfterComma = spaceAfterComma; 
              SpaceAfterSemicolon = spaceAfterSemicolon; 
              IndentOnTryWith = indentOnTryWith; 
              ReorderOpenDeclaration = reorderOpenDeclaration;
              SpaceAroundDelimiter = spaceAroundDelimiter;
              StrictMode = strictMode }

type FormatConfigFileParseResult =
    | Success of FormatConfig
    | PartialSuccess of config: FormatConfig * warnings: string list
    | Failure of exn

module internal ConfigFile =
    open System.IO
    open System.Text.RegularExpressions
    open Fantomas.Version

    let private defaultConfigurationFileName = "fantomas-config.json"

    let rec private getParentFolders acc current =
        let parent = Directory.GetParent(current) |> Option.ofObj
        match parent with
        | Some p -> getParentFolders (current::acc) p.FullName
        | None -> current::acc

    /// Returns all the found configuration files for the given path
    /// fileOrFolder can be a concrete json file or a directory path
    let rec findConfigurationFiles fileOrFolder : string list =
        let findConfigInFolder folderPath =
            let configFile = Path.Combine(folderPath, defaultConfigurationFileName)
            if File.Exists(configFile) then Some configFile else None

        if Path.GetExtension(fileOrFolder) = "" && Directory.Exists fileOrFolder then
            getParentFolders [] fileOrFolder
            |> List.choose findConfigInFolder
        elif File.Exists(fileOrFolder) then
            let parentFolder =
                Directory.GetParent(Path.GetDirectoryName(fileOrFolder))
                |> Option.ofObj
            match parentFolder with
            | Some pf -> findConfigurationFiles pf.FullName @ [fileOrFolder]
            | None -> [fileOrFolder]
        else
            []

    let private fantomasFields = Reflection.getRecordFields FormatConfig.Default |> Array.map fst
    let private (|FantomasSetting|_|) (s:string) =
        let s = s.Trim('\"')
        Array.tryFind ((=) s) fantomasFields

    let private (|Number|_|) d =
        match System.Int32.TryParse(d) with
        | true, d -> Some (box d)
        | _ -> None
    let private (|Boolean|_|) b =
        if b = "true" then Some (box true)
        elif b = "false" then Some (box false)
        else None

    let private parseOptionsFromJson json =
        let results =
            Regex.Replace(json, "\s|{|}", String.Empty).Split([|','|])
            |> Array.map (fun line -> line, line.Split([|':'|]))
            |> Array.filter (fun (_, pieces) -> Array.length pieces = 2 && pieces.[0] <> "$schema")
            |> Array.map(fun (line, pieces) ->
                match pieces with
                | [|FantomasSetting(fs); Number(v)|]
                | [|FantomasSetting(fs); Boolean(v)|] -> Ok (fs, v)
                | _ ->
                    let warning = sprintf "%s is no valid setting for Fantomas v%s" line (fantomasVersion.Value)
                    Error warning
            )

        let options = Array.choose (function | Ok r -> Some r | _ -> None) results
        let warnings = Array.choose (function | Error r -> Some r | _ -> None) results
        options, warnings

    let private formatConfigType = FormatConfig.Default.GetType()
    let applyOptionsToConfig currentConfig path =
        let json = System.IO.File.ReadAllText path
        let (options,warnings) = parseOptionsFromJson json

        let currentValues = Reflection.getRecordFields currentConfig
        let newValues =
            Array.fold (fun acc (k,v) ->
                Array.map (fun (fn, ev) -> if fn = k then (fn, v) else (fn,ev)) acc
            ) currentValues options
            |> Array.map snd
        let updatedConfig = Microsoft.FSharp.Reflection.FSharpValue.MakeRecord (formatConfigType, newValues) :?> FormatConfig
        let warningsInFile = Seq.map (fun w -> sprintf "%s, in %s" w path) warnings |> Seq.toList
        updatedConfig, warningsInFile




