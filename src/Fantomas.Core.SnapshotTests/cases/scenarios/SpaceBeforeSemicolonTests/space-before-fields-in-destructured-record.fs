(*---
fsharp_space_before_semicolon = true
fsharp_space_after_semicolon = false
---*)
let IsMatchByName record1 (name: string) =
    match record1 with
    | { MyRecord.Name = nameFound; ID = _ } when nameFound = name -> true
    | _ -> false
