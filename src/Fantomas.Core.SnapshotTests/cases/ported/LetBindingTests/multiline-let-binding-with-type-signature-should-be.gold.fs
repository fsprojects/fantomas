let foo (a: int) (b: string) : string =
    let c = a.ToString() + b
    sprintf "result: %s" c
