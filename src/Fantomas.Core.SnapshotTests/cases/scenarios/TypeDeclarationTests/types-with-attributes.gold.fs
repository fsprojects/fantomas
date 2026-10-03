type MyType() =
    let mutable myInt1 = 10

    [<DefaultValue; Test>]
    val mutable myInt2: int

    [<DefaultValue; Test>]
    val mutable myString: string

    member this.SetValsAndPrint(i: int, str: string) =
        myInt1 <- i
        this.myInt2 <- i + 1
        this.myString <- str
        printfn "%d %d %s" myInt1 (this.myInt2) (this.myString)
