match x with
| _ when
    (try
        somethingDangerous ()
        true
     with ex ->
         false)
      ->
      {
          A = longTypeName
          B = someOtherVariable
          C = ziggyBarX
      }
