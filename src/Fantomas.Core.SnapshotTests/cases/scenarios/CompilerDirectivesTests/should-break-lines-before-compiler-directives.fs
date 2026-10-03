let [<Literal>] private assemblyConfig() =
  #if TRACE
  let x = ""
  #else
  let x = "x"
  #endif
  x
