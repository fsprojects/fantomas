let a =
  (fun _ ->
    function
    | A ->
      ()
#if DEBUG
      f ()
#endif
    | B -> ())
