module Fantomas.Plan

open System.IO.Abstractions
open Fantomas
open Fantomas.Arguments
open Fantomas.CommandResult
open Serilog

/// One file's worth of work. Whether a file is ignored is settled here, once, rather than asked
/// again at every layer that touches the file.
[<RequireQualifiedAccess>]
type WorkItem =
    | Ignored of file: string
    | Format of inputFile: string * outputFile: string

/// Turn the input and output paths into the list of files to work on, with each output path
/// already worked out. This is where a folder becomes its files, where an output tree is mirrored
/// onto an input tree, and where the ignore file is consulted.
val plan:
    fs: IFileSystem ->
    log: ILogger ->
    ignoreFile: IgnoreFile option ->
    inputPath: InputPath ->
    outputPath: OutputPath ->
        Result<WorkItem list, InputProblem>
