let tryDecompile (ty: FSharpEntity) =
  async {
    match ty.TryFullName with
    | Some fullName -> return decompile ty.Assembly.SimpleName externalSym
    | None ->
      // might be abbreviated type (like string)
      return!
        (if ty.IsFSharpAbbreviation then
           Some ty.AbbreviatedType
         else
           None)
        |> tryGetTypeDef
        |> tryGetSource
  }
