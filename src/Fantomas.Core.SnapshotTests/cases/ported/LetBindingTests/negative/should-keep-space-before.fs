(*---
fsharp_space_before_colon = true
---*)
let refl<'a> : Teq<'a, 'a> = Teq(id, id)
