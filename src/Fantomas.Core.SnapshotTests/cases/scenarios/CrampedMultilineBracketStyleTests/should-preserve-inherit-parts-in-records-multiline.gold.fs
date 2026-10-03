type MyExc =
    inherit Exception

    new(msg) =
        { inherit Exception(msg)
          XXXXXXXXXXXXXXXXXXXXXXXX = 1
          YYYYYYYYYYYYYYYYYYYYYYYY = 2 }
