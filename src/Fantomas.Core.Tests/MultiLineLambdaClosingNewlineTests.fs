module Fantomas.Core.Tests.MultiLineLambdaClosingNewlineTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelper

let defaultConfig = config

let config = { config with MultiLineLambdaClosingNewline = true }

[<Test>]
let ``function with single multiline lambda`` () =
    formatSourceString
        false
        """
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
        { config with MaxInfixOperatorExpression = 35 }
    |> prepend newline
    |> should
        equal
        """
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
    formatSourceString
        false
        """
let mySuperFunction a =
    someOtherFunction a (fun b ->
        // doing some stuff her
       b * b
    )
"""
        config
    |> prepend newline
    |> should
        equal
        """
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
    formatSourceString
        false
        """
let mySuperFunction a =
    someOtherFunction (fun b ->
        // doing some stuff her
       b * b
    ) a
"""
        config
    |> prepend newline
    |> should
        equal
        """
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
    formatSourceString
        false
        """
let printListWithOffset a list1 =
    List.iter (
        ((+) a)
        >> printfn "%d"
    ) list1
"""
        { defaultConfig with MaxInfixOperatorExpression = 5 }
    |> prepend newline
    |> should
        equal
        """
let printListWithOffset a list1 =
    List.iter
        (((+) a)
         >> printfn "%d")
        list1
"""

[<Test>]
let ``desugared lambda`` () =
    formatSourceString
        false
        """
let printListWithOffset a list1 =
    List.iter(fun { ItemOne = a } ->
        // print
        printfn "%s" a
    ) list1
"""
        config
    |> prepend newline
    |> should
        equal
        """
let printListWithOffset a list1 =
    List.iter
        (fun { ItemOne = a } ->
            // print
            printfn "%s" a
        )
        list1
"""

[<Test>]
let ``multiple multiline lambdas`` () =
    formatSourceString
        false
        """
let mySuperFunction v =
    someOtherFunction (fun  a  ->
        let meh = "foo"
        a
     ) (fun b ->
        // probably wrong
        42
     ) v
"""
        config
    |> prepend newline
    |> should
        equal
        """
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
    formatSourceString
        false
        """
let myTopLevelFunction v =
    someOtherFunction (fun { A = a }  ->
        let meh = "foo"
        a
     ) (fun ({ B = b }) ->
        // probably wrong
        42
     ) v
"""
        config
    |> prepend newline
    |> should
        equal
        """
let myTopLevelFunction v =
    someOtherFunction
        (fun { A = a } ->
            let meh = "foo"
            a
        )
        (fun ({ B = b }) ->
            // probably wrong
            42
        )
        v
"""

[<Test>]
let ``lambda after pipe operator`` () =
    formatSourceString
        false
        """
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
        { config with MaxInfixOperatorExpression = 10 }
    |> prepend newline
    |> should
        equal
        """
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
    formatSourceString
        false
        """
let expr =
    genExpr astContext e
    +> col
        sepSpace
        es
        (fun e ->
            match e with
            | Paren (_, Lambda _, _) -> !- "lambda"
            | _ -> genExpr astContext e)
"""
        config
    |> prepend newline
    |> should
        equal
        """
let expr =
    genExpr astContext e
    +> col
        sepSpace
        es
        (fun e ->
            match e with
            | Paren(_, Lambda _, _) -> !- "lambda"
            | _ -> genExpr astContext e
        )
"""

[<Test>]
let ``multiline infix operator samples`` () =
    formatSourceString
        false
        """
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
        { defaultConfig with MaxInfixOperatorExpression = 35 }
    |> prepend newline
    |> should
        equal
        """
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

[<Test>]
let ``no space before uppercase invocations`` () =
    formatSourceString
        false
        """
Foobar(fun x ->
    // going multiline
    x * x)

myValue.UppercaseMemberCall(fun x ->
    let y = x + 1
    x + y)
"""
        { config with SpaceBeforeUppercaseInvocation = false }
    |> prepend newline
    |> should
        equal
        """
Foobar(fun x ->
    // going multiline
    x * x
)

myValue.UppercaseMemberCall(fun x ->
    let y = x + 1
    x + y
)
"""

[<Test>]
let ``space before uppercase invocations`` () =
    formatSourceString
        false
        """
Foobar(fun x ->
    // going multiline
    x * x)

myValue.UppercaseMemberCall(fun x ->
    let y = x + 1
    x + y)
"""
        { config with SpaceBeforeUppercaseInvocation = true }
    |> prepend newline
    |> should
        equal
        """
Foobar (fun x ->
    // going multiline
    x * x
)

myValue.UppercaseMemberCall (fun x ->
    let y = x + 1
    x + y
)
"""

[<Test>]
let ``no space before lowercase invocations`` () =
    formatSourceString
        false
        """
foobar(fun x ->
    // going multiline
    x * x)

myValue.lowercaseMemberCall(fun x ->
    let y = x + 1
    x + y)
"""
        { config with SpaceBeforeLowercaseInvocation = false }
    |> prepend newline
    |> should
        equal
        """
foobar(fun x ->
    // going multiline
    x * x
)

myValue.lowercaseMemberCall(fun x ->
    let y = x + 1
    x + y
)
"""

[<Test>]
let ``space before lowercase invocations`` () =
    formatSourceString
        false
        """
foobar(fun x ->
    // going multiline
    x * x)

myValue.lowercaseMemberCall(fun x ->
    let y = x + 1
    x + y)
"""
        { config with SpaceBeforeLowercaseInvocation = true }
    |> prepend newline
    |> should
        equal
        """
foobar (fun x ->
    // going multiline
    x * x
)

myValue.lowercaseMemberCall (fun x ->
    let y = x + 1
    x + y
)
"""

[<Test>]
let ``comments after desugared lambda arrows`` () =
    formatSourceString
        false
        """
[]
|> List.map (fun { Foo = foo } -> // I use the name foo a lot
    foo + 1)

List.map(fun { Bar = bar } -> // same remark for bar
    bar + 2) []
"""
        config
    |> prepend newline
    |> should
        equal
        """
[]
|> List.map (fun { Foo = foo } -> // I use the name foo a lot
    foo + 1
)

List.map
    (fun { Bar = bar } -> // same remark for bar
        bar + 2
    )
    []
"""

[<Test>]
let ``comments after lambda arrows`` () =
    formatSourceString
        false
        """
[]
|> List.map (fun foo -> // I use the name foo a lot
    foo + 1)

List.map(fun bar -> // same remark for bar
    bar + 2) []
"""
        config
    |> prepend newline
    |> should
        equal
        """
[]
|> List.map (fun foo -> // I use the name foo a lot
    foo + 1
)

List.map
    (fun bar -> // same remark for bar
        bar + 2
    )
    []
"""

[<Test>]
let ``multiple lambda parameters, 1427`` () =
    formatSourceString
        false
        """
let choose chooser source =
    source
    |> Set.fold
        (fun set item ->
            chooser item
            |> Option.map (fun mappedItem -> Set.add mappedItem set)
            |> Option.defaultValue set)
        Set.empty
"""
        { config with SpaceBeforeLowercaseInvocation = false }
    |> prepend newline
    |> should
        equal
        """
let choose chooser source =
    source
    |> Set.fold
        (fun set item ->
            chooser item
            |> Option.map(fun mappedItem -> Set.add mappedItem set)
            |> Option.defaultValue set
        )
        Set.empty
"""

[<Test>]
let ``single line lambda, 1474`` () =
    formatSourceString
        false
        """
module Caching =
    type MainCache() =
        member __.GetLastCachedData (): CachedNetworkData =
            lock cacheFiles.CachedNetworkData (fun _ ->
                sessionCachedNetworkData
            )
"""
        { config with
            MaxLineLength = 80
            MultiLineLambdaClosingNewline = true }
    |> prepend newline
    |> should
        equal
        """
module Caching =
    type MainCache() =
        member __.GetLastCachedData() : CachedNetworkData =
            lock
                cacheFiles.CachedNetworkData
                (fun _ -> sessionCachedNetworkData)
"""

[<Test>]
let ``comment before paren function arg, 1607`` () =
    formatSourceString
        false
        """
namespace Bar

[<RequireQualifiedAccess>]
module Foo =
    /// Blah
    let bang<'a when 'a : equality> (a : Foo<'a>) (ans : ('a * System.TimeSpan) list) : bool =
        List.length x = List.length y
        &&
        List.forall2
        //
          (fun (a, ta) (b, tb) -> a.Equals b && ta = tb)
          x
          y
"""
        { config with
            MaxLineLength = 100
            SpaceBeforeUppercaseInvocation = true
            SpaceBeforeClassConstructor = true
            SpaceBeforeMember = true
            SpaceBeforeColon = true
            SpaceBeforeSemicolon = true
            MultilineBlockBracketsOnSameColumn = true
            MultiLineLambdaClosingNewline = true
            ExperimentalKeepIndentInBranch = true }
    |> prepend newline
    |> should
        equal
        """
namespace Bar

[<RequireQualifiedAccess>]
module Foo =
    /// Blah
    let bang<'a when 'a : equality> (a : Foo<'a>) (ans : ('a * System.TimeSpan) list) : bool =
        List.length x = List.length y
        && List.forall2
            //
            (fun (a, ta) (b, tb) -> a.Equals b && ta = tb)
            x
            y
"""

[<Test>]
let ``comment before paren function arg, idempotent`` () =
    formatSourceString
        false
        """
namespace Bar

[<RequireQualifiedAccess>]
module Foo =
    /// Blah
    let bang<'a when 'a : equality> (a : Foo<'a>) (ans : ('a * System.TimeSpan) list) : bool =
        List.length x = List.length y
        && List.forall2
            //
            (fun (a, ta) (b, tb) -> a.Equals b && ta = tb)
            x
            y
"""
        { config with
            MaxLineLength = 100
            SpaceBeforeUppercaseInvocation = true
            SpaceBeforeClassConstructor = true
            SpaceBeforeMember = true
            SpaceBeforeColon = true
            SpaceBeforeSemicolon = true
            MultilineBlockBracketsOnSameColumn = true
            MultiLineLambdaClosingNewline = true
            ExperimentalKeepIndentInBranch = true }
    |> prepend newline
    |> should
        equal
        """
namespace Bar

[<RequireQualifiedAccess>]
module Foo =
    /// Blah
    let bang<'a when 'a : equality> (a : Foo<'a>) (ans : ('a * System.TimeSpan) list) : bool =
        List.length x = List.length y
        && List.forall2
            //
            (fun (a, ta) (b, tb) -> a.Equals b && ta = tb)
            x
            y
"""

[<Test>]
let ``multiline infix application with piped match expression`` () =
    formatSourceString
        false
        """
module Foo =

    let bar =
        baz
        |> (
            // Hi!
            match false with
            | true -> id
            | false -> id
        )
"""
        config
    |> prepend newline
    |> should
        equal
        """
module Foo =

    let bar =
        baz
        |> (
            // Hi!
            match false with
            | true -> id
            | false -> id
        )
"""

[<Test>]
let ``inner let binding inside lambda, 1741`` () =
    formatSourceString
        false
        """
module Foo =

    let bar () =
        []
        |> Seq.iter(fun (a, b) ->
            let blah =
                fieldInfos
                |> Seq.groupBy (fun fi -> fi.Name)
                |> Seq.filter (fst >> foo >> not)
                |> Seq.choose (fun (name, fieldInfos) ->
                    let fieldTypes = fieldInfos |> Seq.map (fun fi -> TypeId fi.TypeInfo.Id) |> Seq.distinct |> Seq.toList
                    match fieldTypes with
                    | [ fieldType ] -> // hi!
                        let parents = fieldInfos |> Seq.cache
                        Some (name, fieldType, parents)
                    | _ -> // differing
                        None
                )
            ()
        )
"""
        { config with
            MultiLineLambdaClosingNewline = true
            ExperimentalKeepIndentInBranch = true }
    |> prepend newline
    |> should
        equal
        """
module Foo =

    let bar () =
        []
        |> Seq.iter (fun (a, b) ->
            let blah =
                fieldInfos
                |> Seq.groupBy (fun fi -> fi.Name)
                |> Seq.filter (fst >> foo >> not)
                |> Seq.choose (fun (name, fieldInfos) ->
                    let fieldTypes =
                        fieldInfos
                        |> Seq.map (fun fi -> TypeId fi.TypeInfo.Id)
                        |> Seq.distinct
                        |> Seq.toList

                    match fieldTypes with
                    | [ fieldType ] -> // hi!
                        let parents = fieldInfos |> Seq.cache
                        Some(name, fieldType, parents)
                    | _ -> // differing
                        None
                )

            ()
        )
"""

[<Test>]
let ``inner let binding inside lambda, multiple arguments`` () =
    formatSourceString
        false
        """
module Foo =

    let bar () =
        []
        |> Seq.fold (fun (a, b) ->
            let blah =
                fieldInfos
                |> Seq.groupBy (fun fi -> fi.Name)
                |> Seq.filter (fst >> foo >> not)
                |> Seq.choose (fun (name, fieldInfos) ->
                    let fieldTypes = fieldInfos |> Seq.map (fun fi -> TypeId fi.TypeInfo.Id) |> Seq.distinct |> Seq.toList
                    match fieldTypes with
                    | [ fieldType ] -> // hi!
                        let parents = fieldInfos |> Seq.cache
                        Some (name, fieldType, parents)
                    | _ -> // differing
                        None
                )
            ()
        ) meh
"""
        { config with
            MultiLineLambdaClosingNewline = true
            ExperimentalKeepIndentInBranch = true }
    |> prepend newline
    |> should
        equal
        """
module Foo =

    let bar () =
        []
        |> Seq.fold
            (fun (a, b) ->
                let blah =
                    fieldInfos
                    |> Seq.groupBy (fun fi -> fi.Name)
                    |> Seq.filter (fst >> foo >> not)
                    |> Seq.choose (fun (name, fieldInfos) ->
                        let fieldTypes =
                            fieldInfos
                            |> Seq.map (fun fi -> TypeId fi.TypeInfo.Id)
                            |> Seq.distinct
                            |> Seq.toList

                        match fieldTypes with
                        | [ fieldType ] -> // hi!
                            let parents = fieldInfos |> Seq.cache
                            Some(name, fieldType, parents)
                        | _ -> // differing
                            None
                    )

                ()
            )
            meh
"""

[<Test>]
let ``SynExpr.MatchLambda inside parenthesis as argument, 1823`` () =
    formatSourceString
        false
        """
module Foo =
    let bar =
        []
        |> List.choose (
            function
            | _ -> ""
        )
"""
        config
    |> prepend newline
    |> should
        equal
        """
module Foo =
    let bar =
        []
        |> List.choose (
            function
            | _ -> ""
        )
"""

[<Test>]
let ``comment after single lambda in parenthesis argument`` () =
    formatSourceString
        false
        """
module Foo =

    let blah =
        it
        |> List.iter (fun (_, output) ->
            thing
            |> Map.iter (fun key value ->
                match value with
                | Ok (TestResult.Failure f) -> failwith ""
                | Error e -> failwith ""
                | _ -> () // hi!
            )
        )
"""
        config
    |> prepend newline
    |> should
        equal
        """
module Foo =

    let blah =
        it
        |> List.iter (fun (_, output) ->
            thing
            |> Map.iter (fun key value ->
                match value with
                | Ok(TestResult.Failure f) -> failwith ""
                | Error e -> failwith ""
                | _ -> () // hi!
            )
        )
"""

[<Test>]
let ``lambda inside parenthesis without application, 1835`` () =
    formatSourceString
        false
        """
module Foo =
    let blah =
        printfn ""
        (fun bar ->
            printfn ""
            bar + "11111111111111111111111111111111"
        )
"""
        config
    |> prepend newline
    |> should
        equal
        """
module Foo =
    let blah =
        printfn ""

        (fun bar ->
            printfn ""
            bar + "11111111111111111111111111111111"
        )
"""

[<Test>]
let ``lambda inside parenthesis without application, trivia`` () =
    formatSourceString
        false
        """
module Foo =
    let blah =
        printfn ""
        // meh
        (fun bar ->  // foo
            printfn ""
            bar + "11111111111111111111111111111111"
            // bar
        ) // ziggy
"""
        config
    |> prepend newline
    |> should
        equal
        """
module Foo =
    let blah =
        printfn ""
        // meh
        (fun bar -> // foo
            printfn ""
            bar + "11111111111111111111111111111111"
        // bar
        ) // ziggy
"""

[<Test>]
let ``short lambda inside parenthesis`` () =
    formatSourceString
        false
        """
(fun x -> x)
"""
        config
    |> prepend newline
    |> should
        equal
        """
(fun x -> x)
"""

[<Test>]
let ``lambda at end of dot get`` () =
    formatSourceString
        false
        """
configuration
    .MinimumLevel
    .Debug()
    .WriteTo
    .Logger(fun loggerConfiguration ->
        loggerConfiguration
            .Enrich
            .WithProperty("host", Environment.MachineName)
            .Enrich.WithProperty("user", Environment.UserName)
            .Enrich.WithProperty("application", context.HostingEnvironment.ApplicationName)
        |> ignore
    )
"""
        config
    |> prepend newline
    |> should
        equal
        """
configuration
    .MinimumLevel
    .Debug()
    .WriteTo
    .Logger(fun loggerConfiguration ->
        loggerConfiguration
            .Enrich
            .WithProperty("host", Environment.MachineName)
            .Enrich.WithProperty("user", Environment.UserName)
            .Enrich.WithProperty("application", context.HostingEnvironment.ApplicationName)
        |> ignore
    )
"""

[<Test>]
let ``lambda at end of dot get, short lambda`` () =
    formatSourceString
        false
        """
configuration
    .MinimumLevel
    .Debug()
    .WriteTo
    .Logger(fun x -> x * x)
"""
        { config with MaxDotGetExpressionWidth = 50 }
    |> prepend newline
    |> should
        equal
        """
configuration
    .MinimumLevel
    .Debug()
    .WriteTo.Logger(fun x -> x * x)
"""

[<Test>]
let ``match lambda with other arguments`` () =
    formatSourceString
        false
        """
let a =
    Something.foo
        bar
        meh
        (function | Ok x -> true | Error err -> false)
"""
        config
    |> prepend newline
    |> should
        equal
        """
let a =
    Something.foo
        bar
        meh
        (function
        | Ok x -> true
        | Error err -> false
        )
"""

[<Test>]
let ``lambda with long list of arguments at end of dotget`` () =
    formatSourceString
        false
        """
configuration
    .MinimumLevel
    .Debug()
    .WriteTo
    .Logger(fun (a0: int) (a1: int) (a2: int) (a3: int) (a4: int) (a5: int) (a6: int) (a7: int) (a8: int) (a9: int) (a10: int) (a11: int) ->
        //
        ()
)
"""
        config
    |> prepend newline
    |> should
        equal
        """
configuration
    .MinimumLevel
    .Debug()
    .WriteTo
    .Logger(fun
                (a0: int)
                (a1: int)
                (a2: int)
                (a3: int)
                (a4: int)
                (a5: int)
                (a6: int)
                (a7: int)
                (a8: int)
                (a9: int)
                (a10: int)
                (a11: int) ->
        //
        ()
    )
"""

[<Test>]
let ``parameter after multiline lambda with long list of arguments`` () =
    formatSourceString
        false
        """
let mySuperFunction a =
    someOtherFunction (fun (a0: int) (a1: int) (a2: int) (a3: int) (a4: int) (a5: int) (a6: int) (a7: int) (a8: int) (a9: int) (a10: int) (a11: int) ->
        // doing some stuff her
       b * b
    ) a
"""
        config
    |> prepend newline
    |> should
        equal
        """
let mySuperFunction a =
    someOtherFunction
        (fun
            (a0: int)
            (a1: int)
            (a2: int)
            (a3: int)
            (a4: int)
            (a5: int)
            (a6: int)
            (a7: int)
            (a8: int)
            (a9: int)
            (a10: int)
            (a11: int) ->
            // doing some stuff her
            b * b
        )
        a
"""
