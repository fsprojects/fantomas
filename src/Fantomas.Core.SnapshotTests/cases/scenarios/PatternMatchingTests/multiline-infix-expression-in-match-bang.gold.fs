match!
  structuralTypes
  |> List.tryFind (
    fst
    >> checkIfFieldTypeSupportsComparison tycon
    >> not
  )
with
| _ -> ()
