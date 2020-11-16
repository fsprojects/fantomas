module Fantomas.Tests.AlternativeMultilineLambdaTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper

let config =
    { config with
          AlternativeMultilineLambda = true }

[<Test>]
let ``function with single multiline lambda`` () =
    formatSourceString false """
List.collect (fun (a, element) ->
    let path' =
        path
        |> someFunctionToCalculateThing

    innerFunc<'a, 'b>
        path'
        elementNameThatHasThisRatherLongVariableNameToForceTheWholeThingOnMultipleLines
        (foo >> bar value >> List.item a)
        shape
)
"""
        { config with
              MaxInfixOperatorExpression = 35 }
    |> prepend newline
    |> should equal """
List.collect (fun (a, element) ->
    let path' =
        path
        |> someFunctionToCalculateThing

    innerFunc<'a, 'b>
        path'
        elementNameThatHasThisRatherLongVariableNameToForceTheWholeThingOnMultipleLines
        (foo >> bar value >> List.item a)
        shape
)
"""

[<Test>]
let ``parameter before multiline lambda`` () =
    formatSourceString false """
let mySuperFunction a =
    someOtherFunction a (fun b ->
        // doing some stuff her
       b * b
    )
"""  config
    |> prepend newline
    |> should equal """
let mySuperFunction a =
    someOtherFunction
        a
        (fun b ->
            // doing some stuff her
            b * b
        )
"""

[<Test>]
let ``parameter after multiline lambda`` () =
    formatSourceString false """
let mySuperFunction a =
    someOtherFunction (fun b ->
        // doing some stuff her
       b * b
    ) a
"""  config
    |> prepend newline
    |> should equal """
let mySuperFunction a =
    someOtherFunction
        (fun b ->
            // doing some stuff her
            b * b
        )
        a
"""

[<Test>]
let ``lambda without fun keyword`` () =
    formatSourceString false """
let printListWithOffset a list1 =
    List.iter (
        ((+) a)
        >> printfn "%d"
    ) list1
"""
        { config with
              MaxInfixOperatorExpression = 5 }
    |> prepend newline
    |> should equal """
let printListWithOffset a list1 =
    List.iter
        (((+) a)
         >> printfn "%d")
        list1
"""

[<Test>]
let ``desugared lambda`` () =
    formatSourceString false """
let printListWithOffset a list1 =
    List.iter(fun { ItemOne = a } ->
        // print
        printfn "%s" a
    ) list1
"""  config
    |> prepend newline
    |> should equal """
let printListWithOffset a list1 =
    List.iter
        (fun { ItemOne = a } ->
            // print
            printfn "%s" a
        )
        list1
"""

// Not sure if the result is accepted here

[<Test>]
let ``multiple multiline lambdas`` () =
    formatSourceString false """
let mySuperFunction v =
    someOtherFunction (fun  a  ->
        let meh = "foo"
        a
     ) (fun b ->
        // probably wrong
        42
     ) v
"""  config
    |> prepend newline
    |> should equal """
let mySuperFunction v =
    someOtherFunction
        (fun a ->
            let meh = "foo"
            a
        )
        (fun b ->
            // probably wrong
            42
        )
        v
"""

[<Test>]
let ``multiple multiline desugared lambdas`` () =
    formatSourceString false """
let myTopLevelFunction v =
    someOtherFunction (fun { A = a }  ->
        let meh = "foo"
        a
     ) (fun ({ B = b }) ->
        // probably wrong
        42
     ) v
"""  config
    |> prepend newline
    |> should equal """
let myTopLevelFunction v =
    someOtherFunction
        (fun { A = a } ->
            let meh = "foo"
            a
        )
        (fun { B = b } ->
            // probably wrong
            42
        )
        v
"""

[<Test>]
let ``lambda after pipe operator`` () =
    formatSourceString false """
let printListWithOffset a list1 =
    list1
    |> List.iter (fun elem ->
        // print stuff
        printfn "%d" (a + elem)
    )

let printListWithOffset a list1 =
    list1
    |> List.iter (
        ((+) a)
        >> printfn "%d"
    )
"""
        { config with
              MaxInfixOperatorExpression = 10 }
    |> prepend newline
    |> should equal """
let printListWithOffset a list1 =
    list1
    |> List.iter (fun elem ->
        // print stuff
        printfn "%d" (a + elem)
    )

let printListWithOffset a list1 =
    list1
    |> List.iter (
        ((+) a)
        >> printfn "%d"
    )
"""

[<Test>]
let ``custom infix operator with multiline lambda`` () =
    formatSourceString false """
let expr =
    genExpr astContext e
    +> col
        sepSpace
        es
        (fun e ->
            match e with
            | Paren (_, Lambda _, _) -> !- "lambda"
            | _ -> genExpr astContext e)
"""  config
    |> prepend newline
    |> should equal """
let expr =
    genExpr astContext e
    +> col
        sepSpace
        es
        (fun e ->
            match e with
            | Paren (_, Lambda _, _) -> !- "lambda"
            | _ -> genExpr astContext e
        )
"""

[<Test>]
let ``multiline infix operator samples`` () =
    formatSourceString false """
let printListWithOffset a list1 =
    list1
    |> List.iter (
        ((+) veryVeryVeryVeryVeryVeryVeryVeryVeryLongThing)
        >> printfn "%d"
    )

let printListWithOffset' a list1 =
    list1
    |> List.iter (((+) a) >> printfn "%d")

let foldList a list1 =
    list1
    |> List.fold (((+) a) >> printfn "%d") someVeryLongAccumulatorNameThatMakesTheWholeConstructMultilineBecauseOfTheLongName
"""
        { config with
              MaxInfixOperatorExpression = 35 }
    |> prepend newline
    |> should equal """
let printListWithOffset a list1 =
    list1
    |> List.iter (
        ((+) veryVeryVeryVeryVeryVeryVeryVeryVeryLongThing)
        >> printfn "%d"
    )

let printListWithOffset' a list1 =
    list1
    |> List.iter (((+) a) >> printfn "%d")

let foldList a list1 =
    list1
    |> List.fold
        (((+) a) >> printfn "%d")
        someVeryLongAccumulatorNameThatMakesTheWholeConstructMultilineBecauseOfTheLongName
"""


// TODO:
// - fsharp_space_before_lowercase_invocation
// - fsharp_space_before_uppercase_invocation
