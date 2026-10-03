type MyExc =
    inherit Exception
    new(msg) = { inherit Exception(msg) ;X = 1 }
