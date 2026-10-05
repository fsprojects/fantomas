(*---
max_line_length = 80
---*)
let dotted ifaces =
    ifaces
    |> List.tryPick (fun (SynInterfaceImpl(interfaceTy = ty; withKeyword = withRange)) -> Some(ty, withRange))

let undotted ifaces =
    ifaces
    |> pickFromList (fun (SynInterfaceImpl(interfaceTy = ty; withKeyword = withRange)) -> Some(ty, withRange))
