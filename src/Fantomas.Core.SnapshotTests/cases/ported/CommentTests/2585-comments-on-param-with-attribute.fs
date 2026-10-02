type MyType =
    member _.MyMethod
        (
            [<MyAttribute>] inputA: string, // my comment 1
            [<MyAttribute>] inputB: string // my comment 2
        ) =
        inputA

type MyType2 =
    member _.MyMethod
        (
            [<MyAttribute>] inputA: string // my comment 1
        ) =
        inputA
