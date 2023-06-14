module Program

open System.IO.Abstractions

/// Main entry point for any unit tests.
val mainAux: IFileSystem -> string array -> int

/// Main entry point using actual FileSystem.
val main: argv: string[] -> int
