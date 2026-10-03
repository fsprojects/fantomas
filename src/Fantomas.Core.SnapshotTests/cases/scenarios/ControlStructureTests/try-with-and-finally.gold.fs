let function1 x y =
    try
        try
            if x = y then
                raise (InnerError("inner"))
            else
                raise (OuterError("outer"))
        with
        | Failure _ -> ()
        | InnerError(str) -> printfn "Error1 %s" str
    finally
        printfn "Always print this."
