(*---
fsharp_space_before_colon = true
fsharp_space_before_semicolon = true
---*)
namespace X
type MyRecord =
    { Level: int
      Progress: string
      Bar: string
      Street: string
      Number: int }
    member Score : unit -> int
