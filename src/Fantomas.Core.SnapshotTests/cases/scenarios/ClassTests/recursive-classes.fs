(*---
fsharp_max_value_binding_width = 120
---*)
type Folder(pathIn: string) =
  let path = pathIn
  let filenameArray : string array = System.IO.Directory.GetFiles(path)
  member this.FileArray = Array.map (fun elem -> new File(elem, this)) filenameArray

and File(filename: string, containingFolder: Folder) =
   member __.Name = filename
   member __.ContainingFolder = containingFolder