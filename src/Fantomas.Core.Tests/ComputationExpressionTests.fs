module Fantomas.Core.Tests.ComputationExpressionTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelper
open Fantomas.Core

[<Test>]
let ``async workflows`` () =
    formatSourceString
        false
        """
let fetchAsync(name, url:string) =
    async {
        try
            let uri = new System.Uri(url)
            let webClient = new WebClient()
            let! html = webClient.AsyncDownloadString(uri)
            printfn "Read %d characters for %s" html.Length name
        with
            | :? Exception -> ()
            | ex -> printfn "%s" (ex.Message);
    }
    """
        config
    |> prepend newline
    |> should
        equal
        """
let fetchAsync (name, url: string) =
    async {
        try
            let uri = new System.Uri(url)
            let webClient = new WebClient()
            let! html = webClient.AsyncDownloadString(uri)
            printfn "Read %d characters for %s" html.Length name
        with
        | :? Exception -> ()
        | ex -> printfn "%s" (ex.Message)
    }
"""

[<Test>]
let ``computation expressions`` () =
    formatSourceString
        false
        """
let comp =
    eventually { for x in 1 .. 2 do
                    printfn " x = %d" x
                 return 3 + 4 }"""
        config
    |> prepend newline
    |> should
        equal
        """
let comp =
    eventually {
        for x in 1..2 do
            printfn " x = %d" x

        return 3 + 4
    }
"""

[<Test>]
let ``sequence expressions`` () =
    formatSourceString
        false
        """
let s1 = seq { for i in 1 .. 10 -> i * i }
let s2 = seq { 0 .. 10 .. 100 }
let rec inorder tree =
    seq {
      match tree with
          | Tree(x, left, right) ->
               yield! inorder left
               yield x
               yield! inorder right
          | Leaf x -> yield x
    }
    """
        config
    |> prepend newline
    |> should
        equal
        """
let s1 = seq { for i in 1..10 -> i * i }
let s2 = seq { 0..10..100 }

let rec inorder tree =
    seq {
        match tree with
        | Tree(x, left, right) ->
            yield! inorder left
            yield x
            yield! inorder right
        | Leaf x -> yield x
    }
"""

[<Test>]
let ``range expressions`` () =
    formatSourceString
        false
        """
let factors number =
    {2L .. number / 2L}
    |> Seq.filter (fun x -> number % x = 0L)"""
        { config with
            MaxInfixOperatorExpression = 65
            MaxFunctionBindingWidth = 65 }
    |> prepend newline
    |> should
        equal
        """
let factors number = { 2L .. number / 2L } |> Seq.filter (fun x -> number % x = 0L)
"""

[<Test>]
let ``match bang`` () =
    formatSourceString
        false
        """
async {
    match! myAsyncFunction() with
    | Some x -> printfn "%A" x
    | None -> printfn "Function returned None!"
}"""
        config
    |> prepend newline
    |> should
        equal
        """
async {
    match! myAsyncFunction () with
    | Some x -> printfn "%A" x
    | None -> printfn "Function returned None!"
}
"""

[<Test>]
let ``sequence expression inside computation expression, 553`` () =
    formatSourceString
        false
        """let x = {3..7}
let y = async {
    return { 0.. 1 }
}
"""
        config
    |> prepend newline
    |> should
        equal
        """
let x = { 3..7 }
let y = async { return { 0..1 } }
"""

[<Test>]
let ``and! is supported`` () =
    formatSourceString
        false
        """
async {
    let! x = Async.Sleep 1.
    and! y = Async.Sleep 2.
    return 10
}
"""
        config
    |> prepend newline
    |> should
        equal
        """
async {
    let! x = Async.Sleep 1.
    and! y = Async.Sleep 2.
    return 10
}
"""

[<Test>]
let ``multiple and! is supported`` () =
    formatSourceString
        false
        """
// Reads the values of x, y and z concurrently, then applies f to them
``parallel`` {
    let! x = slowRequestX()
    and! y = slowRequestY()
    and! z = slowRequestZ()
    return f x y z
}
"""
        config
    |> prepend newline
    |> should
        equal
        """
// Reads the values of x, y and z concurrently, then applies f to them
``parallel`` {
    let! x = slowRequestX ()
    and! y = slowRequestY ()
    and! z = slowRequestZ ()
    return f x y z
}
"""

[<Test>]
let ``and! sample number 3`` () =
    formatSourceString
        false
        """
observable {
    let! a = foo
    and! b = bar
    return a + b
}
"""
        config
    |> prepend newline
    |> should
        equal
        """
observable {
    let! a = foo
    and! b = bar
    return a + b
}
"""

[<Test>]
let ``let bang should be formatted as regular let, 615`` () =
    formatSourceString
        false
        """
let f =
  async {
    // Without binding newline after assignment sign preserved, which is expected behavior
    let r =
      match 0 with
      | _ -> ()

    return r
  }

let f2 =
  async {
    // When binding, newline force-removed, which makes the whole expression
    // on the right side to be indented.
    let! r = match 0 with
             | _ -> () |> async.Return

    return r
  }
"""
        config
    |> prepend newline
    |> should
        equal
        """
let f =
    async {
        // Without binding newline after assignment sign preserved, which is expected behavior
        let r =
            match 0 with
            | _ -> ()

        return r
    }

let f2 =
    async {
        // When binding, newline force-removed, which makes the whole expression
        // on the right side to be indented.
        let! r =
            match 0 with
            | _ -> () |> async.Return

        return r
    }
"""

[<Test>]
let ``let bang, and bang should be formatted as regular let`` () =
    formatSourceString
        false
        """
let f2 =
  async {
    // When binding, newline force-removed, which makes the whole expression
    // on the right side to be indented.
    let! r = match 0 with
             | _ -> () |> async.Return
    and! s = match 0 with
             | _ -> () |> async.Return
    return r + s
  }
"""
        config
    |> prepend newline
    |> should
        equal
        """
let f2 =
    async {
        // When binding, newline force-removed, which makes the whole expression
        // on the right side to be indented.
        let! r =
            match 0 with
            | _ -> () |> async.Return

        and! s =
            match 0 with
            | _ -> () |> async.Return

        return r + s
    }
"""

[<Test>]
let ``computation expression with app identifier, 806`` () =
    formatSourceString
        false
        """
[<Tests>]
let tests =
  testList "tests"
    [
      test "test" {
        Expect.equal true true "unexpected"
      }
    ]
"""
        config
    |> prepend newline
    |> should
        equal
        """
[<Tests>]
let tests = testList "tests" [ test "test" { Expect.equal true true "unexpected" } ]
"""

[<Test>]
let ``multiline computation expression with SynExpr.App identifier, 835`` () =
    formatSourceString
        false
        """let meh =
    create [] {
        // foo
        // bar
        return 42
    }"""
        config
    |> prepend newline
    |> should
        equal
        """
let meh =
    create [] {
        // foo
        // bar
        return 42
    }
"""

[<Test>]
let ``multiline computation expression with SynExpr.App identifier and multiple expressions`` () =
    formatSourceString
        false
        """
let private validateLocation =
    createValidatorFor<LocationAdded> () {
        validate (fun l -> l.Id) [ isNotEmptyGuid ]
        validate (fun l -> l.Name)
            [ isNotEmpty
              hasMinLengthOf 3 ]
        validate (fun l -> fst l.Location) [ isValidLatitude ]
        validate (fun l -> snd l.Location) [ isValidLongitude ]
        validate (fun l -> l.Price) [ isGreaterThan 0. ]
        validate (fun l -> l.Date) [ isNotMinDate ]
        validate (fun l -> l.Creator) [ isNotEmpty ]
    }
"""
        config
    |> prepend newline
    |> should
        equal
        """
let private validateLocation =
    createValidatorFor<LocationAdded> () {
        validate (fun l -> l.Id) [ isNotEmptyGuid ]
        validate (fun l -> l.Name) [ isNotEmpty; hasMinLengthOf 3 ]
        validate (fun l -> fst l.Location) [ isValidLatitude ]
        validate (fun l -> snd l.Location) [ isValidLongitude ]
        validate (fun l -> l.Price) [ isGreaterThan 0. ]
        validate (fun l -> l.Date) [ isNotMinDate ]
        validate (fun l -> l.Creator) [ isNotEmpty ]
    }
"""

[<Test>]
let ``new line after multiline let bang, 819`` () =
    formatSourceString
        false
        """
let x data =
    async {
        let! bar =
            data
            |> Array.map id
            |> Array.filter ((=) 1)
            |> Array.countBy id
            |> async.Return
        return bar
    }

let y data =
    async {
        let bar =
            data
            |> Array.map id
            |> Array.filter ((=) 1)
            |> Array.countBy id
            |> async.Return
        return bar
    }

let z =
    let bar =
        data
        |> Array.map id
        |> Array.filter ((=) 1)
        |> Array.countBy id
    bar
"""
        { config with
            MaxInfixOperatorExpression = 40 }
    |> prepend newline
    |> should
        equal
        """
let x data =
    async {
        let! bar =
            data
            |> Array.map id
            |> Array.filter ((=) 1)
            |> Array.countBy id
            |> async.Return

        return bar
    }

let y data =
    async {
        let bar =
            data
            |> Array.map id
            |> Array.filter ((=) 1)
            |> Array.countBy id
            |> async.Return

        return bar
    }

let z =
    let bar =
        data
        |> Array.map id
        |> Array.filter ((=) 1)
        |> Array.countBy id

    bar
"""

[<Test>]
let ``normal let bindings before and after let bang`` () =
    formatSourceString
        false
        """
let fetchAsync(name, url:string) =
    async {
        let uri = new System.Uri(url)
        let webClient = new WebClient()
        let! html = webClient.AsyncDownloadString(uri)
        let title = html.CssSelect("title")
        return title
    }
"""
        config
    |> prepend newline
    |> should
        equal
        """
let fetchAsync (name, url: string) =
    async {
        let uri = new System.Uri(url)
        let webClient = new WebClient()
        let! html = webClient.AsyncDownloadString(uri)
        let title = html.CssSelect("title")
        return title
    }
"""

[<Test>]
let ``short expression with intertwined with newlines`` () =
    formatSourceString
        false
        """
async {
    let! a = aa

    and! b = bb

    and! c = cc

    return (a + b + c)
}
"""
        config
    |> prepend newline
    |> should
        equal
        """
async {
    let! a = aa

    and! b = bb

    and! c = cc

    return (a + b + c)
}
"""

[<Test>]
let ``add new line between one-liner and multiline expression, 838`` () =
    formatSourceString
        false
        """
let AddEvents (req: HttpRequest, log: ILogger) =
    task {
        let! user = req.Authenticate(log)
        let! response =
            user
            |> Result.mapError sendUnAuthorizedRequest
            |> Result.bind (decodeEvents req.Body)
            |> Result.bind (validateEvents)
            |> Result.bind (authenticateEvents)
            |> Result.map (persistEvents)
            |> Result.either
        return response
    }

let AddEventsX (req: HttpRequest, log: ILogger) =
        let user = req.Authenticate(log)
        let response =
            user
            |> Result.mapError sendUnAuthorizedRequest
            |> Result.bind (decodeEvents req.Body)
            |> Result.bind (validateEvents)
            |> Result.bind (authenticateEvents)
            |> Result.map (persistEvents)
            |> Result.either
        response
"""
        config
    |> prepend newline
    |> should
        equal
        """
let AddEvents (req: HttpRequest, log: ILogger) =
    task {
        let! user = req.Authenticate(log)

        let! response =
            user
            |> Result.mapError sendUnAuthorizedRequest
            |> Result.bind (decodeEvents req.Body)
            |> Result.bind (validateEvents)
            |> Result.bind (authenticateEvents)
            |> Result.map (persistEvents)
            |> Result.either

        return response
    }

let AddEventsX (req: HttpRequest, log: ILogger) =
    let user = req.Authenticate(log)

    let response =
        user
        |> Result.mapError sendUnAuthorizedRequest
        |> Result.bind (decodeEvents req.Body)
        |> Result.bind (validateEvents)
        |> Result.bind (authenticateEvents)
        |> Result.map (persistEvents)
        |> Result.either

    response
"""

[<Test>]
let ``mix of let and let! single line expressions`` () =
    formatSourceString
        false
        """let foo () =
    async {
        let! a = callA()
        let b = callB()
        let! c = callC()
        let d = callD()
        let! e = callE()
        return (a + b + c - e * d) }
"""
        config
    |> prepend newline
    |> should
        equal
        """
let foo () =
    async {
        let! a = callA ()
        let b = callB ()
        let! c = callC ()
        let d = callD ()
        let! e = callE ()
        return (a + b + c - e * d)
    }
"""

[<Test>]
let ``return from computation expression`` () =
    formatSourceString
        false
        """async { return 42 }
"""
        config
    |> prepend newline
    |> should
        equal
        """
async { return 42 }
"""

[<Test>]
let ``return from multiline computation expression`` () =
    formatSourceString
        false
        """async {
    // foo
    return 42 }
"""
        config
    |> prepend newline
    |> should
        equal
        """
async {
    // foo
    return 42
}
"""

[<Test>]
let ``let + return from ce`` () =
    formatSourceString
        false
        """async {
    let a = getA()
    return a
}
"""
        config
    |> prepend newline
    |> should
        equal
        """
async {
    let a = getA ()
    return a
}
"""

[<Test>]
let ``let rec + return from ce`` () =
    formatSourceString
        false
        """async {
    let rec a = getA()
    return a
}
"""
        config
    |> prepend newline
    |> should
        equal
        """
async {
    let rec a = getA ()
    return a
}
"""

[<Test>]
let ``two let + return from ce`` () =
    formatSourceString
        false
        """async {
    let a = getA()
    let b = getB ()
    return a
}
"""
        config
    |> prepend newline
    |> should
        equal
        """
async {
    let a = getA ()
    let b = getB ()
    return a
}
"""

[<Test>]
let ``let + let rec + let + return from ce`` () =
    formatSourceString
        false
        """async {
    let a = getA ()
    let rec b = getB ()
    let c = getC ()
    return a
}
"""
        config
    |> prepend newline
    |> should
        equal
        """
async {
    let a = getA ()
    let rec b = getB ()
    let c = getC ()
    return a
}
"""

[<Test>]
let ``multiline let + return from ce`` () =
    formatSourceString
        false
        """async {
    let a =
        // foo
        getA()
    return a
}
"""
        config
    |> prepend newline
    |> should
        equal
        """
async {
    let a =
        // foo
        getA ()

    return a
}
"""

[<Test>]
let ``do + return from ce`` () =
    formatSourceString
        false
        """async {
    do foo
    return bar
}
"""
        config
    |> prepend newline
    |> should
        equal
        """
async {
    do foo
    return bar
}
"""

[<Test>]
let ``do! + return from ce`` () =
    formatSourceString
        false
        """async {
    do! foo
    return bar
}
"""
        config
    |> prepend newline
    |> should
        equal
        """
async {
    do! foo
    return bar
}
"""

[<Test>]
let ``do! + let + return from ce`` () =
    formatSourceString
        false
        """async {
    do! foo
    let bar = getBar ()
    return bar
}
"""
        config
    |> prepend newline
    |> should
        equal
        """
async {
    do! foo
    let bar = getBar ()
    return bar
}
"""

[<Test>]
let ``let bang + newline + return`` () =
    formatSourceString
        false
        """async {
    let! bar = getBar ()

    return bar
}
"""
        config
    |> prepend newline
    |> should
        equal
        """
async {
    let! bar = getBar ()

    return bar
}
"""

[<Test>]
let ``let bang + and bang + newline + return`` () =
    formatSourceString
        false
        """async {
    let! bar = getBar ()

    and! foo = getFoo ()

    return bar
}
"""
        config
    |> prepend newline
    |> should
        equal
        """
async {
    let! bar = getBar ()

    and! foo = getFoo ()

    return bar
}
"""

[<Test>]
let ``custom method names`` () =
    formatSourceString
        false
        """let indexMachine =
    freyaMachine {
        methods [GET; HEAD; OPTIONS]
        handleOk Pages.home }
"""
        config
    |> prepend newline
    |> should
        equal
        """
let indexMachine =
    freyaMachine {
        methods [ GET; HEAD; OPTIONS ]
        handleOk Pages.home
    }
"""

[<Test>]
let ``let bang + multiline match in ce`` () =
    formatSourceString
        false
        """
let rec runPendingJobs () =
    task {
        let! jobToRun = checkForJob ()
        match jobToRun with
        | None -> return ()
        | Some pendingJob ->
            do! pendingJob ()
            return! runPendingJobs ()
    }

"""
        config
    |> prepend newline
    |> should
        equal
        """
let rec runPendingJobs () =
    task {
        let! jobToRun = checkForJob ()

        match jobToRun with
        | None -> return ()
        | Some pendingJob ->
            do! pendingJob ()
            return! runPendingJobs ()
    }
"""

[<Test>]
let ``let + let + let bang + if/then/else in ce`` () =
    formatSourceString
        false
        """let rec private appendToAzureTableStorage (cosmoEvents: EventWrite<JsonValue> seq) =
    task {
        let moreThanBatchLimit = Seq.length cosmoEvents > BatchLimit

        let batch =
            if moreThanBatchLimit then Seq.take BatchLimit cosmoEvents else cosmoEvents
            |> List.ofSeq

        let! _ = eventStore.AppendEvents EventStream Any batch

        if moreThanBatchLimit then
            let rest = Seq.skip BatchLimit cosmoEvents
            return! appendToAzureTableStorage rest
        else
            return ()
    }
"""
        { config with
            MaxIfThenElseShortWidth = 75 }
    |> prepend newline
    |> should
        equal
        """
let rec private appendToAzureTableStorage (cosmoEvents: EventWrite<JsonValue> seq) =
    task {
        let moreThanBatchLimit = Seq.length cosmoEvents > BatchLimit

        let batch =
            if moreThanBatchLimit then Seq.take BatchLimit cosmoEvents else cosmoEvents
            |> List.ofSeq

        let! _ = eventStore.AppendEvents EventStream Any batch

        if moreThanBatchLimit then
            let rest = Seq.skip BatchLimit cosmoEvents
            return! appendToAzureTableStorage rest
        else
            return ()
    }
"""

[<Test>]
let ``short do bang in ce`` () =
    formatSourceString
        false
        """let appendEvents userId (events: Event list) =
    let cosmoEvents = List.map (createEvent userId) events
    task { do! appendToAzureTableStorage cosmoEvents }
"""
        config
    |> prepend newline
    |> should
        equal
        """
let appendEvents userId (events: Event list) =
    let cosmoEvents = List.map (createEvent userId) events
    task { do! appendToAzureTableStorage cosmoEvents }
"""

[<Test>]
let ``let bang + let + return in ce`` () =
    formatSourceString
        false
        """let getEvents() =
    task {
        let! cosmoEvents = eventStore.GetEvents EventStream AllEvents
        let events = List.map (fun (ce: EventRead<JsonValue, _>) -> ce.Data) cosmoEvents
        return events
    }
"""
        config
    |> prepend newline
    |> should
        equal
        """
let getEvents () =
    task {
        let! cosmoEvents = eventStore.GetEvents EventStream AllEvents
        let events = List.map (fun (ce: EventRead<JsonValue, _>) -> ce.Data) cosmoEvents
        return events
    }
"""

[<Test>]
let ``let bang + do expression + let + return in ce`` () =
    formatSourceString
        false
        """
    task {
        let! config = manager.GetConfigurationAsync().ConfigureAwait(false)
        parameters.IssuerSigningKeys <- config.SigningKeys
        let user, _ = handler.ValidateToken((token: string), parameters)
        return Ok(user.Identity.Name, collectClaims user)
    }
"""
        { config with
            MaxDotGetExpressionWidth = 50 }
    |> prepend newline
    |> should
        equal
        """
task {
    let! config =
        manager
            .GetConfigurationAsync()
            .ConfigureAwait(false)

    parameters.IssuerSigningKeys <- config.SigningKeys
    let user, _ = handler.ValidateToken((token: string), parameters)
    return Ok(user.Identity.Name, collectClaims user)
}
"""

[<Test>]
let ``do bang + return in ce`` () =
    formatSourceString
        false
        """    let ((userId, _), events) = request
    task {
        do! EventStore.appendEvents userId events
        return sendText "Events persisted"
    }
"""
        config
    |> prepend newline
    |> should
        equal
        """
let ((userId, _), events) = request

task {
    do! EventStore.appendEvents userId events
    return sendText "Events persisted"
}
"""

[<Test>]
let ``yield bang + yield bang in ce`` () =
    formatSourceString
        false
        """
let squares =
    seq {
        for i in 1..3 -> i * i
    }

let cubes =
    seq {
        for i in 1..3 -> i * i * i
    }

let squaresAndCubes =
    seq {
        yield! squares
        yield! cubes
    }
"""
        config
    |> prepend newline
    |> should
        equal
        """
let squares = seq { for i in 1..3 -> i * i }

let cubes = seq { for i in 1..3 -> i * i * i }

let squaresAndCubes =
    seq {
        yield! squares
        yield! cubes
    }
"""

[<Test>]
let ``let bang + yield bang in ce`` () =
    formatSourceString
        false
        """let myCollection = seq {
    let! squares = getSquares()
    yield! (squares * level) }
"""
        config
    |> prepend newline
    |> should
        equal
        """
let myCollection =
    seq {
        let! squares = getSquares ()
        yield! (squares * level)
    }
"""

[<Test>]
let ``return bang in ce`` () =
    formatSourceString
        false
        """let req = // 'req' is of type is 'Async<data>'
    async {
        return! fetch url
    }

"""
        config
    |> prepend newline
    |> should
        equal
        """
let req = // 'req' is of type is 'Async<data>'
    async { return! fetch url }
"""

[<Test>]
let ``saturn router`` () =
    formatSourceString
        false
        """
module Router

open Saturn
open Giraffe.Core
open Giraffe.ResponseWriters


let browser = pipeline {
    plug acceptHtml
    plug putSecureBrowserHeaders
    plug fetchSession
    set_header "x-pipeline-type" "Browser"
}

let defaultView = router {
    get "/" (htmlView Index.layout)
    get "/index.html" (redirectTo false "/")
    get "/default.html" (redirectTo false "/")
}

let browserRouter = router {
    not_found_handler (htmlView NotFound.layout) //Use the default 404 webpage
    pipe_through browser //Use the default browser pipeline

    forward "" defaultView //Use the default view
}

let appRouter = router {
    // forward "/api" apiRouter
    forward "" browserRouter
}
"""
        config
    |> prepend newline
    |> should
        equal
        """
module Router

open Saturn
open Giraffe.Core
open Giraffe.ResponseWriters


let browser =
    pipeline {
        plug acceptHtml
        plug putSecureBrowserHeaders
        plug fetchSession
        set_header "x-pipeline-type" "Browser"
    }

let defaultView =
    router {
        get "/" (htmlView Index.layout)
        get "/index.html" (redirectTo false "/")
        get "/default.html" (redirectTo false "/")
    }

let browserRouter =
    router {
        not_found_handler (htmlView NotFound.layout) //Use the default 404 webpage
        pipe_through browser //Use the default browser pipeline

        forward "" defaultView //Use the default view
    }

let appRouter =
    router {
        // forward "/api" apiRouter
        forward "" browserRouter
    }
"""

[<Test>]
let ``freya api file`` () =
    formatSourceString
        false
        """module Api

open Freya.Core
open Freya.Machines.Http
open Freya.Types.Http
open Freya.Routers.Uri.Template

let name_ = Route.atom_ "name"

let name =
    freya {
        let! name = Freya.Optic.get name_

        match name with
        | Some name -> return name
        | None -> return "World" }

let sayHello =
    freya {
        let! name = name

        return Represent.text (sprintf "Hello, %s!" name) }

let helloMachine =
    freyaMachine {
        methods [GET; HEAD; OPTIONS]
        handleOk sayHello }

let root =
    freyaRouter { resource "/hello{/name}" helloMachine }
"""
        config
    |> prepend newline
    |> should
        equal
        """
module Api

open Freya.Core
open Freya.Machines.Http
open Freya.Types.Http
open Freya.Routers.Uri.Template

let name_ = Route.atom_ "name"

let name =
    freya {
        let! name = Freya.Optic.get name_

        match name with
        | Some name -> return name
        | None -> return "World"
    }

let sayHello =
    freya {
        let! name = name

        return Represent.text (sprintf "Hello, %s!" name)
    }

let helloMachine =
    freyaMachine {
        methods [ GET; HEAD; OPTIONS ]
        handleOk sayHello
    }

let root = freyaRouter { resource "/hello{/name}" helloMachine }
"""

[<Test>]
let ``use bang`` () =
    formatSourceString
        false
        """
let resource = promise {
    return new DisposableAction(fun () -> isDisposed := true)
}
promise {
    use! r = resource
    step1ok := not !isDisposed
}
"""
        { config with
            MaxValueBindingWidth = 90 }
    |> prepend newline
    |> should
        equal
        """
let resource = promise { return new DisposableAction(fun () -> isDisposed := true) }

promise {
    use! r = resource
    step1ok := not !isDisposed
}
"""

[<Test>]
let ``multiline let bang + return in ce`` () =
    formatSourceString
        false
        """
   let divideByWorkflow x y w z =
        maybe
            {
            let! a = x |> divideBy y
            let! b = a |> divideBy w
            let! c = b |> divideBy z
            return c
            }
"""
        config
    |> prepend newline
    |> should
        equal
        """
let divideByWorkflow x y w z =
    maybe {
        let! a = x |> divideBy y
        let! b = a |> divideBy w
        let! c = b |> divideBy z
        return c
    }
"""

[<Test>]
let ``giraffe handler example`` () =
    formatSourceString
        false
        """
let loginHandler =
    fun (next : HttpFunc) (ctx : HttpContext) ->
        task {
            let issuer = "http://localhost:5000"
            let claims =
                [
                    Claim(ClaimTypes.Name,      "John",  ClaimValueTypes.String, issuer)
                    Claim(ClaimTypes.Surname,   "Doe",   ClaimValueTypes.String, issuer)
                    Claim(ClaimTypes.Role,      "Admin", ClaimValueTypes.String, issuer)
                ]
            let identity = ClaimsIdentity(claims, authScheme)
            let user     = ClaimsPrincipal(identity)

            do! ctx.SignInAsync(authScheme, user)

            return! text "Successfully logged in" next ctx
        }
"""
        config
    |> prepend newline
    |> should
        equal
        """
let loginHandler =
    fun (next: HttpFunc) (ctx: HttpContext) ->
        task {
            let issuer = "http://localhost:5000"

            let claims =
                [ Claim(ClaimTypes.Name, "John", ClaimValueTypes.String, issuer)
                  Claim(ClaimTypes.Surname, "Doe", ClaimValueTypes.String, issuer)
                  Claim(ClaimTypes.Role, "Admin", ClaimValueTypes.String, issuer) ]

            let identity = ClaimsIdentity(claims, authScheme)
            let user = ClaimsPrincipal(identity)

            do! ctx.SignInAsync(authScheme, user)

            return! text "Successfully logged in" next ctx
        }
"""

[<Test>]
let ``all keywords`` () =
    formatSourceString
        false
        """
let valueOne =
    myCe {
        let a = getA()
        let! b= getB()
        and! bb = getBB()
        do c
        do! d
        return 42
    }

let valueTwo =
    myCe {
        let a = getA()
        let! b= getB()
        and! bb = getBB()
        do c
        do! d
        return! getE()
    }
"""
        config
    |> prepend newline
    |> should
        equal
        """
let valueOne =
    myCe {
        let a = getA ()
        let! b = getB ()
        and! bb = getBB ()
        do c
        do! d
        return 42
    }

let valueTwo =
    myCe {
        let a = getA ()
        let! b = getB ()
        and! bb = getBB ()
        do c
        do! d
        return! getE ()
    }
"""

[<Test>]
let ``use and let bang, 876`` () =
    formatSourceString
        false
        """let private getAST log (req: HttpRequest) =
        async {
            use stream = new StreamReader(req.Body)
            let! json = stream.ReadToEndAsync() |> Async.AwaitTask
            let parseRequest = Decoders.decodeInputRequest json

            match parseRequest with
            | Result.Ok input when (input.SourceCode.Length < Const.sourceSizeLimit) ->
                let! astResult = parseAST log input
                match astResult with
                | Result.Ok ast ->
                    let node =
                        match ast with
                        | ParsedInput.ImplFile (ParsedImplFileInput.ParsedImplFileInput (_, _, _, _, hds, mns, _)) ->
                            Fantomas.AstTransformer.astToNode hds mns

                        | ParsedInput.SigFile (ParsedSigFileInput.ParsedSigFileInput (_, _, _, _, mns)) ->
                            Fantomas.AstTransformer.sigAstToNode mns
                        |> Encoders.nodeEncoder

                    let responseJson =
                        Encoders.encodeResponse node (sprintf "%A" ast)
                        |> Thoth.Json.Net.Encode.toString 2

                    return sendJson responseJson

                | Error error -> return sendBadRequest (sprintf "%A" error)

            | Result.Ok _ -> return sendTooLargeError ()

            | Error err -> return sendInternalError (sprintf "%A" err)
        }
"""
        config
    |> prepend newline
    |> should
        equal
        """
let private getAST log (req: HttpRequest) =
    async {
        use stream = new StreamReader(req.Body)
        let! json = stream.ReadToEndAsync() |> Async.AwaitTask
        let parseRequest = Decoders.decodeInputRequest json

        match parseRequest with
        | Result.Ok input when (input.SourceCode.Length < Const.sourceSizeLimit) ->
            let! astResult = parseAST log input

            match astResult with
            | Result.Ok ast ->
                let node =
                    match ast with
                    | ParsedInput.ImplFile(ParsedImplFileInput.ParsedImplFileInput(_, _, _, _, hds, mns, _)) ->
                        Fantomas.AstTransformer.astToNode hds mns

                    | ParsedInput.SigFile(ParsedSigFileInput.ParsedSigFileInput(_, _, _, _, mns)) ->
                        Fantomas.AstTransformer.sigAstToNode mns
                    |> Encoders.nodeEncoder

                let responseJson =
                    Encoders.encodeResponse node (sprintf "%A" ast)
                    |> Thoth.Json.Net.Encode.toString 2

                return sendJson responseJson

            | Error error -> return sendBadRequest (sprintf "%A" error)

        | Result.Ok _ -> return sendTooLargeError ()

        | Error err -> return sendInternalError (sprintf "%A" err)
    }
"""

[<Test>]
let ``let rec + let bang`` () =
    formatSourceString
        false
        """let a =
    async {
        let rec foo a = foo a
        let! bar = async { return foo a }
        return bar
    }
"""
        config
    |> prepend newline
    |> should
        equal
        """
let a =
    async {
        let rec foo a = foo a
        let! bar = async { return foo a }
        return bar
    }
"""

[<Test>]
let ``new line between let and let bang, 879`` () =
    formatSourceString
        false
        """let rec loop () =
        async {
          let! msg = inbox.Receive()

          match msg with
          | Handle (eventSource,command,reply) ->
              let! stream = eventSource |> eventStore.GetStream

              let newEvents =
                stream |> Result.map (asEvents >> behaviour command >> enveloped eventSource)

              let! result =
                newEvents
                |> function
                    | Ok events -> eventStore.Append events
                    | Error err -> async { return Error err }

              do reply.Reply result

              return! loop ()
        }
"""
        { config with
            SpaceBeforeUppercaseInvocation = true
            IndentSize = 2
            SpaceAroundDelimiter = false
            MultilineBracketStyle = Aligned
            MaxInfixOperatorExpression = 50 }
    |> prepend newline
    |> should
        equal
        """
let rec loop () =
  async {
    let! msg = inbox.Receive ()

    match msg with
    | Handle (eventSource, command, reply) ->
      let! stream = eventSource |> eventStore.GetStream

      let newEvents =
        stream
        |> Result.map (
          asEvents
          >> behaviour command
          >> enveloped eventSource
        )

      let! result =
        newEvents
        |> function
          | Ok events -> eventStore.Append events
          | Error err -> async {return Error err}

      do reply.Reply result

      return! loop ()
  }
"""

[<Test>]
let ``trivia before closing brace, 977`` () =
    formatSourceString
        false
        """
    let initDb() =
        if not (File.Exists(dbFileName)) then
            let dbFile = File.Create(dbFileName)
            dbFile.Dispose() |> ignore
        let createSql = readSqlFile "create"
        using (connection()) (fun conn ->
            task {
                do! conn.OpenAsync()
                let! _ = conn.ExecuteAsync(createSql)
#if DEBUG
                let! hasClients = hasClients()
                if not (hasClients) then
                    let seedSql = readSqlFile "seed"
                    let! _ = conn.ExecuteAsync(seedSql)
                    ()
#else
                ()
#endif
            })
"""
        config
    |> prepend newline
    |> should
        equal
        """
let initDb () =
    if not (File.Exists(dbFileName)) then
        let dbFile = File.Create(dbFileName)
        dbFile.Dispose() |> ignore

    let createSql = readSqlFile "create"

    using (connection ()) (fun conn ->
        task {
            do! conn.OpenAsync()
            let! _ = conn.ExecuteAsync(createSql)
#if DEBUG
            let! hasClients = hasClients ()

            if not (hasClients) then
                let seedSql = readSqlFile "seed"
                let! _ = conn.ExecuteAsync(seedSql)
                ()
#else
            ()
#endif
        })
"""

[<Test>]
let ``keep newline before do bang`` () =
    formatSourceString
        false
        """
let private removeSubscription (log : ILogger) (req : HttpRequest) =
    log.LogInformation("Start remove-subscription")
    task {
        let origin = req.Headers.["Origin"].ToString()
        let user = Authentication.getUser log req
        let! endpoint = req.ReadAsStringAsync()
        let! managementToken = Authentication.getManagementAccessToken log
        let! existingSubscriptions = Authentication.getUserPushNotificationSubscriptions log managementToken user.Id

        do! filterSubscriptionsAndPersist managementToken user.Id existingSubscriptions origin endpoint

        return sendText "Subscription removed"
    }
"""
        { config with SpaceBeforeColon = true }
    |> prepend newline
    |> should
        equal
        """
let private removeSubscription (log : ILogger) (req : HttpRequest) =
    log.LogInformation("Start remove-subscription")

    task {
        let origin = req.Headers.["Origin"].ToString()
        let user = Authentication.getUser log req
        let! endpoint = req.ReadAsStringAsync()
        let! managementToken = Authentication.getManagementAccessToken log
        let! existingSubscriptions = Authentication.getUserPushNotificationSubscriptions log managementToken user.Id

        do! filterSubscriptionsAndPersist managementToken user.Id existingSubscriptions origin endpoint

        return sendText "Subscription removed"
    }
"""

[<Test>]
let ``don't add extra newline before do bang`` () =
    formatSourceString
        false
        """
            let sendPushNotifications =
                allSubscriptions
                |> List.map
                    (fun (user, subscriptions) ->
                        subscriptions
                        |> List.filter (fun s -> s.Origin = origin)
                        |> List.map (fun s ->
                            task {
                                try
                                    let ps =
                                        PushSubscription(s.Endpoint, s.P256DH, s.Auth)

                                    do! webPushClient.SendNotificationAsync(ps, payload, vapidDetails)
                                with :? WebPushException as wpex ->
                                    log.LogError(sprintf "Couldn't send notification to %s, %A" user.UserId wpex)
                                    do! filterSubscriptionsAndPersistLongLongLongLongLongLongLongLongLong
                                            managementToken
                                            user.UserId
                                            subscriptions
                                            s.Origin
                                            s.Endpoint
                            } :> Task)
                        |> Task.WhenAll)
"""
        config
    |> prepend newline
    |> should
        equal
        """
let sendPushNotifications =
    allSubscriptions
    |> List.map (fun (user, subscriptions) ->
        subscriptions
        |> List.filter (fun s -> s.Origin = origin)
        |> List.map (fun s ->
            task {
                try
                    let ps = PushSubscription(s.Endpoint, s.P256DH, s.Auth)

                    do! webPushClient.SendNotificationAsync(ps, payload, vapidDetails)
                with :? WebPushException as wpex ->
                    log.LogError(sprintf "Couldn't send notification to %s, %A" user.UserId wpex)

                    do!
                        filterSubscriptionsAndPersistLongLongLongLongLongLongLongLongLong
                            managementToken
                            user.UserId
                            subscriptions
                            s.Origin
                            s.Endpoint
            }
            :> Task)
        |> Task.WhenAll)
"""

[<Test>]
let ``multi line return expression should be indented, 1062`` () =
    formatSourceString
        false
        """
let f () =
  async {
    let x = 2
    return some rather long |> stuff that |> uses piping |> to' demonstrate |> the issue
  }
"""
        { config with
            MaxInfixOperatorExpression = 50 }
    |> prepend newline
    |> should
        equal
        """
let f () =
    async {
        let x = 2

        return
            some rather long
            |> stuff that
            |> uses piping
            |> to' demonstrate
            |> the issue
    }
"""

[<Test>]
let ``multi line return bang expression should be indented`` () =
    formatSourceString
        false
        """
let f () =
  async {
    let x = 2
    return! some rather long |> stuff that |> uses piping |> to' demonstrate |> the issue
  }
"""
        { config with
            MaxInfixOperatorExpression = 50 }
    |> prepend newline
    |> should
        equal
        """
let f () =
    async {
        let x = 2

        return!
            some rather long
            |> stuff that
            |> uses piping
            |> to' demonstrate
            |> the issue
    }
"""

[<Test>]
let ``add new line before multiline for loop, 1092`` () =
    formatSourceString
        false
        """
async {
    let! (msg: Msg) = inbox.Receive()
    for x in msg.Content do
        printfn "%s" x
    return ()
}

async {
    let! (msg: Msg) = inbox.Receive()

    for x in msg.Content do
        printfn "%s" x

    return ()
}
"""
        config
    |> prepend newline
    |> should
        equal
        """
async {
    let! (msg: Msg) = inbox.Receive()

    for x in msg.Content do
        printfn "%s" x

    return ()
}

async {
    let! (msg: Msg) = inbox.Receive()

    for x in msg.Content do
        printfn "%s" x

    return ()
}
"""

[<Test>]
let ``don't repeat new line trivia before closing brace, 1137`` () =
    formatSourceString
        false
        """
let create: Highlighter =
    fun searchTerm ->
        let regex = searchTerm |> SearchTerm.toRegex

        fun s ->
            match s |> String.length with
            | 0 -> [] |> FormattedText
            | _ ->
                seq {
                    let ms = regex.Matches(s)

                    if ms.Count = 0 then yield (TextSpan.normal s)
                    elif ms.[0].Index > 0 then yield TextSpan.normal (s.Substring(0, ms.[0].Index))

                    for i in 0 .. ms.Count - 1 do
                        yield TextSpan.highlight ms.[i].Value
                        let regStart = ms.[i].Index + ms.[i].Length

                        if i < ms.Count - 1
                        then yield TextSpan.normal (s.Substring(regStart, ms.[i + 1].Index - regStart))
                        elif regStart < s.Length
                        then yield TextSpan.normal (s.Substring(regStart))

                }
                |> List.ofSeq
                |> FormattedText.fromList
"""
        { config with
            MaxIfThenElseShortWidth = 80
            MaxIfThenShortWidth = 80 }
    |> prepend newline
    |> should
        equal
        """
let create: Highlighter =
    fun searchTerm ->
        let regex = searchTerm |> SearchTerm.toRegex

        fun s ->
            match s |> String.length with
            | 0 -> [] |> FormattedText
            | _ ->
                seq {
                    let ms = regex.Matches(s)

                    if ms.Count = 0 then yield (TextSpan.normal s)
                    elif ms.[0].Index > 0 then yield TextSpan.normal (s.Substring(0, ms.[0].Index))

                    for i in 0 .. ms.Count - 1 do
                        yield TextSpan.highlight ms.[i].Value
                        let regStart = ms.[i].Index + ms.[i].Length

                        if i < ms.Count - 1 then
                            yield TextSpan.normal (s.Substring(regStart, ms.[i + 1].Index - regStart))
                        elif regStart < s.Length then
                            yield TextSpan.normal (s.Substring(regStart))

                }
                |> List.ofSeq
                |> FormattedText.fromList
"""

[<Test>]
let ``applicative computation expression`` () =
    formatSourceString
        false
        """
// First, define a 'zip' function
module Result =
    let zip x1 x2 =
        match x1,x2 with
        | Ok x1res, Ok x2res -> Ok (x1res, x2res)
        | Error e, _ -> Error e
        | _, Error e -> Error e

// Next, define a builder with 'MergeSources' and 'BindReturn'
type ResultBuilder() =
    member _.MergeSources(t1: Result<'T,'U>, t2: Result<'T1,'U>) = Result.zip t1 t2
    member _.BindReturn(x: Result<'T,'U>, f) = Result.map f x

let result = ResultBuilder()

let run r1 r2 r3 =
    // And here is our applicative!
    let res1: Result<int, string> =
        result {
            let! a = r1
            and! b = r2
            and! c = r3
            return a + b - c
        }

    match res1 with
    | Ok x -> printfn "%s is: %d" (nameof res1) x
    | Error e -> printfn "%s is: %s" (nameof res1) e

let printApplicatives () =
    let r1 = Ok 2
    let r2 = Ok 3 // Error "fail!"
    let r3 = Ok 4

    run r1 r2 r3
    run r1 (Error "failure!") r3
"""
        config
    |> prepend newline
    |> should
        equal
        """
// First, define a 'zip' function
module Result =
    let zip x1 x2 =
        match x1, x2 with
        | Ok x1res, Ok x2res -> Ok(x1res, x2res)
        | Error e, _ -> Error e
        | _, Error e -> Error e

// Next, define a builder with 'MergeSources' and 'BindReturn'
type ResultBuilder() =
    member _.MergeSources(t1: Result<'T, 'U>, t2: Result<'T1, 'U>) = Result.zip t1 t2
    member _.BindReturn(x: Result<'T, 'U>, f) = Result.map f x

let result = ResultBuilder()

let run r1 r2 r3 =
    // And here is our applicative!
    let res1: Result<int, string> =
        result {
            let! a = r1
            and! b = r2
            and! c = r3
            return a + b - c
        }

    match res1 with
    | Ok x -> printfn "%s is: %d" (nameof res1) x
    | Error e -> printfn "%s is: %s" (nameof res1) e

let printApplicatives () =
    let r1 = Ok 2
    let r2 = Ok 3 // Error "fail!"
    let r3 = Ok 4

    run r1 r2 r3
    run r1 (Error "failure!") r3
"""

[<Test>]
let ``overloads of custom keywords in computation expressions`` () =
    formatSourceString
        false
        """
open System

type InputKind =
    | Text of placeholder:string option
    | Password of placeholder: string option

type InputOptions =
  { Label: string option
    Kind : InputKind
    Validators : (string -> bool) array }

type InputBuilder() =
    member t.Yield(_) =
      { Label = None
        Kind = Text None
        Validators = [||] }

    [<CustomOperation("text")>]
    member this.Text(io, ?placeholder) =
        { io with Kind = Text placeholder }

    [<CustomOperation("password")>]
    member this.Password(io, ?placeholder) =
        { io with Kind = Password placeholder }

    [<CustomOperation("label")>]
    member this.Label(io, label) =
        { io with Label = Some label }

    [<CustomOperation("with_validators")>]
    member this.Validators(io, [<ParamArray>] validators) =
        { io with Validators = validators }

let input = InputBuilder()

let name =
    input {
        label "Name"
        text
        with_validators
            (String.IsNullOrWhiteSpace >> not)
    }

let email =
    input {
        label "Email"
        text "Your email"
        with_validators
            (String.IsNullOrWhiteSpace >> not)
            (fun s -> s.Contains "@")
    }

let password =
    input {
        label "Password"
        password "Must contains at least 6 characters, one number and one uppercase"
        with_validators
            (String.exists Char.IsUpper)
            (String.exists Char.IsDigit)
            (fun s -> s.Length >= 6)
    }
"""
        config
    |> prepend newline
    |> should
        equal
        """
open System

type InputKind =
    | Text of placeholder: string option
    | Password of placeholder: string option

type InputOptions =
    { Label: string option
      Kind: InputKind
      Validators: (string -> bool) array }

type InputBuilder() =
    member t.Yield(_) =
        { Label = None
          Kind = Text None
          Validators = [||] }

    [<CustomOperation("text")>]
    member this.Text(io, ?placeholder) = { io with Kind = Text placeholder }

    [<CustomOperation("password")>]
    member this.Password(io, ?placeholder) = { io with Kind = Password placeholder }

    [<CustomOperation("label")>]
    member this.Label(io, label) = { io with Label = Some label }

    [<CustomOperation("with_validators")>]
    member this.Validators(io, [<ParamArray>] validators) = { io with Validators = validators }

let input = InputBuilder()

let name =
    input {
        label "Name"
        text
        with_validators (String.IsNullOrWhiteSpace >> not)
    }

let email =
    input {
        label "Email"
        text "Your email"
        with_validators (String.IsNullOrWhiteSpace >> not) (fun s -> s.Contains "@")
    }

let password =
    input {
        label "Password"
        password "Must contains at least 6 characters, one number and one uppercase"
        with_validators (String.exists Char.IsUpper) (String.exists Char.IsDigit) (fun s -> s.Length >= 6)
    }
"""

[<Test>]
let ``multiline do bang`` () =
    formatSourceString
        false
        """
type ProjectController(checker: FSharpChecker) =
  member x.LoadWorkspace (files: string list) (tfmForScripts: FSIRefs.TFM) onProjectLoaded (generateBinlog: bool) =
    async {
      match Environment.workspaceLoadDelay () with
      | delay when delay > TimeSpan.Zero ->
          do! Async.Sleep(
            Environment.workspaceLoadDelay().TotalMilliseconds
            |> int
          )
      | _ -> ()

      return true
    }

"""
        { config with
            IndentSize = 2
            MaxInfixOperatorExpression = 50 }
    |> prepend newline
    |> should
        equal
        """
type ProjectController(checker: FSharpChecker) =
  member x.LoadWorkspace (files: string list) (tfmForScripts: FSIRefs.TFM) onProjectLoaded (generateBinlog: bool) =
    async {
      match Environment.workspaceLoadDelay () with
      | delay when delay > TimeSpan.Zero ->
        do!
          Async.Sleep(
            Environment.workspaceLoadDelay().TotalMilliseconds
            |> int
          )
      | _ -> ()

      return true
    }
"""

[<Test>]
let ``multiline do`` () =
    formatSourceString
        false
        """
type ProjectController(checker: FSharpChecker) =
  member x.LoadWorkspace (files: string list) (tfmForScripts: FSIRefs.TFM) onProjectLoaded (generateBinlog: bool) =
    async {
      match Environment.workspaceLoadDelay () with
      | delay when delay > TimeSpan.Zero ->
          do NonAsync.Sleep( Environment.workspaceLoadDelay().TotalMilliseconds |> int )
      | _ -> ()

      return true
    }

"""
        { config with
            IndentSize = 2
            MaxInfixOperatorExpression = 50 }
    |> prepend newline
    |> should
        equal
        """
type ProjectController(checker: FSharpChecker) =
  member x.LoadWorkspace (files: string list) (tfmForScripts: FSIRefs.TFM) onProjectLoaded (generateBinlog: bool) =
    async {
      match Environment.workspaceLoadDelay () with
      | delay when delay > TimeSpan.Zero ->
        do
          NonAsync.Sleep(
            Environment.workspaceLoadDelay().TotalMilliseconds
            |> int
          )
      | _ -> ()

      return true
    }
"""

[<Test>]
let ``multiline do bang with parenthesis`` () =
    formatSourceString
        false
        """
let setup =
  meh {
    do!
      (let thing = Thing()
       thing.DoSomething()
       let value = 1
       value)
  }
"""
        config
    |> prepend newline
    |> should
        equal
        """
let setup =
    meh {
        do!
            (let thing = Thing()
             thing.DoSomething()
             let value = 1
             value)
    }
"""

[<Test>]
let ``keep new line before match bang, 1313`` () =
    formatSourceString
        false
        """
  /// a codefix that generates union cases for an incomplete match expression
  let generateUnionCases =
    ifDiagnosticByMessage
      (fun diagnostic codeActionParams ->
        asyncResult {
          let! (tyRes, line, lines) = getParseResultsForFile fileName pos

          match! generateCases tyRes pos lines line |> Async.map Ok with
          | CoreResponse.Res (insertString: string, insertPosition) ->
              return
                [ { SourceDiagnostic = Some diagnostic
                    File = codeActionParams.TextDocument
                    Title = "Generate union pattern match cases"
                    Edits = [| { Range = range; NewText = replaced } |]
                    Kind = Fix } ]

          | _ -> return []
        }
        |> AsyncResult.foldResult id (fun _ -> []))
      "Incomplete pattern matches on this expression. For example"
"""
        config
    |> prepend newline
    |> should
        equal
        """
/// a codefix that generates union cases for an incomplete match expression
let generateUnionCases =
    ifDiagnosticByMessage
        (fun diagnostic codeActionParams ->
            asyncResult {
                let! (tyRes, line, lines) = getParseResultsForFile fileName pos

                match! generateCases tyRes pos lines line |> Async.map Ok with
                | CoreResponse.Res(insertString: string, insertPosition) ->
                    return
                        [ { SourceDiagnostic = Some diagnostic
                            File = codeActionParams.TextDocument
                            Title = "Generate union pattern match cases"
                            Edits = [| { Range = range; NewText = replaced } |]
                            Kind = Fix } ]

                | _ -> return []
            }
            |> AsyncResult.foldResult id (fun _ -> []))
        "Incomplete pattern matches on this expression. For example"
"""

[<Test>]
let ``keep newline before multiline SynExpr.JoinIn, 1463`` () =
    formatSourceString
        false
        """
aggregateResult {
    apply id in someFunction
    also displayableId in AggregateResult.map (fun x -> string x.Z) g
    also person in getThing y |> AggregateResult.ofResult
    also more in AggregateResult.bind
                     (getLongfunctionNameWithLotsOfStuff
                      >> AggregateResult.ofResult)
                     mainThingThatHappens

    return
        { Id = id
          DisplayableId = displayableId
          More = more }
}

aggregateResult {
    apply id in someFunction
    also displayableId in AggregateResult.map (fun x -> string x.Z) g
    also person in getThing y |> AggregateResult.ofResult

    also more in AggregateResult.bind
                     (getLongfunctionNameWithLotsOfStuff
                      >> AggregateResult.ofResult)
                     mainThingThatHappens

    return
        { Id = id
          DisplayableId = displayableId
          More = more }
}
"""
        { config with
            MaxInfixOperatorExpression = 40
            MaxDotGetExpressionWidth = 50 }
    |> prepend newline
    |> should
        equal
        """
aggregateResult {
    apply id in someFunction
    also displayableId in AggregateResult.map (fun x -> string x.Z) g
    also person in getThing y |> AggregateResult.ofResult

    also more in AggregateResult.bind
                     (getLongfunctionNameWithLotsOfStuff
                      >> AggregateResult.ofResult)
                     mainThingThatHappens

    return
        { Id = id
          DisplayableId = displayableId
          More = more }
}

aggregateResult {
    apply id in someFunction
    also displayableId in AggregateResult.map (fun x -> string x.Z) g
    also person in getThing y |> AggregateResult.ofResult

    also more in AggregateResult.bind
                     (getLongfunctionNameWithLotsOfStuff
                      >> AggregateResult.ofResult)
                     mainThingThatHappens

    return
        { Id = id
          DisplayableId = displayableId
          More = more }
}
"""

[<Test>]
let ``line comment above SynExpr.LetOrUseBang`` () =
    formatSourceString
        false
        """
let x =
    async {
        // bar
        let! f =   foo()
        ()
    }
"""
        config
    |> prepend newline
    |> should
        equal
        """
let x =
    async {
        // bar
        let! f = foo ()
        ()
    }
"""

[<Test>]
let ``let bang + sequential, 1882`` () =
    formatSourceString
        false
        """
               async {
                 logger.Debug "some message"
                 let! token = Async.CancellationToken
                 let! model = sendRequest logger credentials token
                 return model.Prop }
"""
        config
    |> prepend newline
    |> should
        equal
        """
async {
    logger.Debug "some message"
    let! token = Async.CancellationToken
    let! model = sendRequest logger credentials token
    return model.Prop
}
"""

[<Test>]
let ``named ce with generic parameter, 2285`` () =
    formatSourceString
        false
        """
odata<Person> {
    count
    take 10
    skip 10
}
"""
        config
    |> prepend newline
    |> should
        equal
        """
odata<Person> {
    count
    take 10
    skip 10
}
"""

[<Test>]
let ``trivia after computation expression, 2466`` () =
    formatSourceString
        false
        """
                let errs =
                    (*[omit:(copying of errors omitted)]*)
                    seq {
                        for e in res.Errors ->
                            { StartColumn = e.StartColumn
                              StartLine = e.StartLine
                              Message = e.Message
                              IsError = e.Severity = Error
                              EndColumn = e.EndColumn
                              EndLine = e.EndLine }
                    } (*[/omit]*)
"""
        config
    |> prepend newline
    |> should
        equal
        """
let errs =
    (*[omit:(copying of errors omitted)]*)
    seq {
        for e in res.Errors ->
            { StartColumn = e.StartColumn
              StartLine = e.StartLine
              Message = e.Message
              IsError = e.Severity = Error
              EndColumn = e.EndColumn
              EndLine = e.EndLine }
    } (*[/omit]*)
"""

[<Test>]
let ``trivia after short computation expression`` () =
    formatSourceString
        false
        """
let zero =
    async { () } // foo
    |> ignore
"""
        config
    |> prepend newline
    |> should
        equal
        """
let zero =
    async { () } // foo
    |> ignore
"""
