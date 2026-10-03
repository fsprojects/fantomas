(*---
max_line_length = 90
fsharp_space_before_colon = true
---*)
  let internal UpdateStrongNaming (assembly : AssemblyDefinition) (key : StrongNameKeyPair option) =
    assembly.Name

  let UpdateStrongNamingX (assembly : AssemblyDefinition) (key : StrongNameKeyPair option) =
    assembly.Name
