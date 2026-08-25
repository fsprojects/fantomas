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
///
/// The ignore file is asked for per file rather than once for the run, because the one that
/// governs a file is the nearest at or above it, which is what the daemon has always done.
///
/// A folder the ignore file names is never opened, so it produces no work item at all. It is
/// written to `log` at Debug on the way past, which is the only thing a run can say about it: what
/// is inside is as unknown as what is inside a folder that is not there.
val plan:
    fs: IFileSystem ->
    log: ILogger ->
    findIgnoreFile: (string -> IgnoreFile option) ->
    inputPath: InputPath ->
    outputPath: OutputPath ->
        Result<WorkItem list, InputProblem>
