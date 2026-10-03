  let
#if DEBUG
#else
      inline
#endif
             internal Issue71Wrapper visits moduleId hitPointId context handler add =
    try
      add visits moduleId hitPointId context
    with x ->
      match x with
      | :? KeyNotFoundException
      | :? NullReferenceException
      | :? ArgumentNullException -> handler moduleId hitPointId context x
      | _ -> reraise()
