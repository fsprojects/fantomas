(*---
indent_size = 2
---*)
namespace AltCover.Recorder

open System

#if NET2
[<ProgIdAttribute("ExcludeFromCodeCoverage hack for OpenCover issue 615")>]
#else
[<System.Diagnostics.CodeAnalysis.ExcludeFromCodeCoverage>]
#endif
type internal Close =
  | DomainUnload
  | ProcessExit
  | Pause
  | Resume
