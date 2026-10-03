(*---
fsharp_multiline_bracket_style = cramped
---*)
namespace Signature

type Range =
    { From : float
      To : float
      Name: string }
    member Length : unit -> int
