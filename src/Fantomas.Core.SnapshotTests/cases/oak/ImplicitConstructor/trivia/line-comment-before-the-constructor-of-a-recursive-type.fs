type A() =
    member _.X = 1

and [<AbstractClass>] B
    // comment
    (x:int, y:string)
    =
    inherit A()
