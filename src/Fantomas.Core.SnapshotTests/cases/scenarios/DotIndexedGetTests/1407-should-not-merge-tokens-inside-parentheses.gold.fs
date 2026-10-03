let inline (=??) x = (=!) x

let mySampleMethod () =
    let result = Ok {| Results = [] |}

    (Result.okValue result).Results.[0] |> Result.isOk
    =?? true
