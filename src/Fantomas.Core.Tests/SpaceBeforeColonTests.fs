module Fantomas.Core.Tests.SpaceBeforeColonTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelpers

// ref: https://docs.microsoft.com/en-us/dotnet/fsharp/style-guide/formatting#right-pad-value-and-function-argument-type-annotations

[<Test>]
let ``right-pad value and function argument type annotations`` () =
    formatSourceString
        """
// OK
let complexFunction (a: int) (b: int) c = a + b + c
let expensiveToCompute: int = 0 // Type annotation for let-bound value

type C() =
    member _.Property: int = 1

// Bad
let complexFunctionBad (a :int) (b :int) (c:int) = a + b + c
let expensiveToComputeBad1:int = 1
let expensiveToComputeBad2 :int = 2
"""
        config
    |> prepend newline
    |> should
        equal
        """
// OK
let complexFunction (a: int) (b: int) c = a + b + c
let expensiveToCompute: int = 0 // Type annotation for let-bound value

type C() =
    member _.Property: int = 1

// Bad
let complexFunctionBad (a: int) (b: int) (c: int) = a + b + c
let expensiveToComputeBad1: int = 1
let expensiveToComputeBad2: int = 2
"""

// ref: https://github.com/G-Research/fsharp-formatting-conventions#type-annotations

[<Test>]
let ``when defining arguments with type annotations, use white space before and after the : symbol:`` () =
    formatSourceString
        """
// OK
let complexFunction (a : int) (b : int) c = a + b + c

// Bad
let complexFunctionBad (a :int) (b: int) (c:int) = a + b + c

// OK
let expensiveToCompute : int = 0 // Type annotation for let-bound value
let myFun (a : decimal) b c : decimal = a + b + c // Type annotation for the return type of a function
// Bad
let expensiveToComputeBad1:int = 1
let expensiveToComputeBad2 :int = 2
let myFunBad (a: decimal) b c:decimal = a + b + c
"""
        { config with SpaceBeforeColon = true }
    |> prepend newline
    |> should
        equal
        """
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
"""

[<Test>]
let ``fsharp_space_before_colon not honored for return type info of explicit get, set, 2825`` () =
    formatSourceString
        """
type SomeType() =

    let mutable v: string = ""

    member val SomeAutoProp: float = 23.42 with get, set
    
    member this.MyProperty
        with get (): string = v
        and set (value: string): unit = v <- value
"""
        { config with SpaceBeforeColon = true }
    |> prepend newline
    |> should
        equal
        """
type SomeType() =

    let mutable v : string = ""

    member val SomeAutoProp : float = 23.42 with get, set

    member this.MyProperty
        with get () : string = v
        and set (value : string) : unit = v <- value
"""
