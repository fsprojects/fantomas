module X =
  let getValSignature displayContext (v: FSharpMemberOrFunctionOrValue) =
    let name =
      (if v.DisplayName.StartsWith "( " then
         v.LogicalName
       else
         v.DisplayName)
      |> PrettyNaming.QuoteIdentifierIfNeeded

    ()
