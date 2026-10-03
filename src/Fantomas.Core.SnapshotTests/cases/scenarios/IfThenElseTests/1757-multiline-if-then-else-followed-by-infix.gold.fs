let name =
  (if typ.GenericParameter.IsSolveAtCompileTime then
     "^"
   else
     "'")
  + typ.GenericParameter.Name
