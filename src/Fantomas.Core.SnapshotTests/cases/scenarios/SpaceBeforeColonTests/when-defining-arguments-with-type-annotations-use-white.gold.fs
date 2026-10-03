// OK
let complexFunction (a : int) (b : int) c = a + b + c

// Bad
let complexFunctionBad (a : int) (b : int) (c : int) = a + b + c

// OK
let expensiveToCompute : int = 0 // Type annotation for let-bound value
let myFun (a : decimal) b c : decimal = a + b + c // Type annotation for the return type of a function
// Bad
let expensiveToComputeBad1 : int = 1
let expensiveToComputeBad2 : int = 2
let myFunBad (a : decimal) b c : decimal = a + b + c
