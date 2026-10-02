namespace AltCover.Recorder

open System

#if NET2
#else
[<System.Diagnostics.CodeAnalysis.ExcludeFromCodeCoverage>]
#endif
type internal Close =
    | DomainUnload
    | ProcessExit
    | Pause
    | Resume
