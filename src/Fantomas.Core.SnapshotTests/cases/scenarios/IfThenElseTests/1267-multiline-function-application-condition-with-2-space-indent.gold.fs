let code =
  if
    System.Text.RegularExpressions.Regex.IsMatch(
      d.Name,
      """^[a-zA-Z][a-zA-Z0-9']+$"""
    )
  then
    d.Name
  elif d.NamespaceToOpen.IsSome then
    d.Name
  else
    PrettyNaming.QuoteIdentifierIfNeeded d.Name
