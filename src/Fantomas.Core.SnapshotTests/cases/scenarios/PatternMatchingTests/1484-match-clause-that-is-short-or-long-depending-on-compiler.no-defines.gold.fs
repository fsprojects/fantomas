let a =
  (fun _ ->
    function
    | A -> ()
    #if DEBUG
    #endif
    | B -> ())
