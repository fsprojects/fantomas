let getColl =
  define
    .Operation
    .ForContext(Context.toAuthenticatedContext)
    .GetCollection(fun _ parser ->
      let x = 2
      x)
