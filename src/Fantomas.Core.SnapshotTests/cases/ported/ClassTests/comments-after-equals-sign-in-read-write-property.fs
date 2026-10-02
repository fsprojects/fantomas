type Foo() =
    member this.MyReadWriteProperty
        with get () =   //comment get 
            myInternalValue
        and set (value) =   // comment set
            myInternalValue <- value
