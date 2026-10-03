(*---
fsharp_space_after_comma = false
fsharp_space_after_semicolon = false
fsharp_space_around_delimiter = false
fsharp_multiline_bracket_style = cramped
---*)
type F =
  abstract G : int list -> Map<int, int>

let x : F = { new F with member __.G _ = Map.empty }
x.G[].TryFind 3
