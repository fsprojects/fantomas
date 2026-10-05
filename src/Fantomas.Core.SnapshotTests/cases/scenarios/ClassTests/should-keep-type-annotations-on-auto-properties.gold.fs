type Document(id: string, library: string, name: string option) =
    member val ID = id
    member val Library = library
    member val Name = name with get, set
    member val LibraryID: string option = None with get, set
