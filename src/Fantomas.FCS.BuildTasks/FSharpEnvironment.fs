namespace Internal.Utilities

open System.Runtime.InteropServices

/// The part of the upstream FSharpEnvironment (src/Compiler/Facilities/CompilerLocation.fs) that the
/// vendored TaskEnvironmentPaths.fs refers to. The real module needs the generated UtilsStrings and
/// FSharp.BuildProperties, which this project does not have.
module internal FSharpEnvironment =

    let isWindows = RuntimeInformation.IsOSPlatform(OSPlatform.Windows)

    /// Only TaskEnvironmentPaths.defaultCompilerToolPath calls this, for the Fsc and Fsi tasks, and
    /// FSharpEmbedResourceText never does.
    let BinFolderOfDefaultFSharpCompilerUsingEnvironment
        (_getEnvironmentVariable: string -> string | null)
        (_probePoint: string option)
        : string option
        =
        None
