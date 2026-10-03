// OK
let complexFunction (a: int) (b: int) c = a + b + c
let expensiveToCompute: int = 0 // Type annotation for let-bound value

type C() =
    member _.Property: int = 1

// Bad
let complexFunctionBad (a: int) (b: int) (c: int) = a + b + c
let expensiveToComputeBad1: int = 1
let expensiveToComputeBad2: int = 2
