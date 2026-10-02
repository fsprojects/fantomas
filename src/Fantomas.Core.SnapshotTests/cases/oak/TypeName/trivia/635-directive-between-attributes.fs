(*---
# One case where the old suite had three tests: the merged result and one per define.
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
