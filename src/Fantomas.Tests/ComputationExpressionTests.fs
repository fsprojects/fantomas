module Fantomas.Tests.CodeFormatterExtTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper

[<Test>]
let ``async workflows``() =
    formatSourceString false """
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
    """ config
    |> prepend newline
    |> should equal """
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
let ``computation expressions``() =
    formatSourceString false """
let comp =
    eventually { for x in 1 .. 2 do
                    printfn " x = %d" x
                 return 3 + 4 }""" config
    |> prepend newline
    |> should equal """
let comp =
    eventually {
        for x in 1 .. 2 do
            printfn " x = %d" x

        return 3 + 4
    }
"""

[<Test>]
let ``sequence expressions``() =
    formatSourceString false """
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
    """ config
    |> prepend newline
    |> should equal """
let s1 = seq { for i in 1 .. 10 -> i * i }
let s2 = seq { 0 .. 10 .. 100 }

let rec inorder tree =
    seq {
        match tree with
        | Tree (x, left, right) ->
            yield! inorder left
            yield x
            yield! inorder right
        | Leaf x -> yield x
    }
"""

[<Test>]
let ``range expressions``() =
    formatSourceString false """
let factors number = 
    {2L .. number / 2L}
    |> Seq.filter (fun x -> number % x = 0L)""" ({ config with
                                                        MaxInfixOperatorExpression = 65
                                                        MaxFunctionBindingWidth = 65 })
    |> prepend newline
    |> should equal """
let factors number = { 2L .. number / 2L } |> Seq.filter (fun x -> number % x = 0L)
"""

[<Test>]
let ``match bang``() =
    formatSourceString false """
async { 
    match! myAsyncFunction() with
    | Some x -> printfn "%A" x
    | None -> printfn "Function returned None!"
}""" config
    |> prepend newline
    |> should equal """
async {
    match! myAsyncFunction () with
    | Some x -> printfn "%A" x
    | None -> printfn "Function returned None!"
}
"""

[<Test>]
let ``sequence expression inside computation expression, 553`` () =
    formatSourceString false """let x = {3..7}
let y = async {
    return { 0.. 1 }
}
"""  config
    |> prepend newline
    |> should equal """
let x = { 3 .. 7 }
let y = async { return { 0 .. 1 } }
"""

[<Test>]
let ``and! is supported`` () =
    formatSourceString false """
async {
    let! x = Async.Sleep 1.
    and! y = Async.Sleep 2.
    return 10
}
"""  config
   |> prepend newline
   |> should equal """
async {
    let! x = Async.Sleep 1.
    and! y = Async.Sleep 2.
    return 10
}
"""
[<Test>]
let ``multiple and! is supported`` () =
    formatSourceString false """
// Reads the values of x, y and z concurrently, then applies f to them
parallel {
    let! x = slowRequestX()
    and! y = slowRequestY()
    and! z = slowRequestZ()
    return f x y z
}
"""  config
   |> prepend newline
   |> should equal """
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
    formatSourceString false """
observable {
    let! a = foo
    and! b = bar
    return a + b
}
"""  config
   |> prepend newline
   |> should equal """
observable {
    let! a = foo
    and! b = bar
    return a + b
}
"""

[<Test>]
let ``let bang should be formatted as regular let, 615`` () =
    formatSourceString false """
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
"""  config
    |> prepend newline
    |> should equal """
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
    formatSourceString false """
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
"""  config
    |> prepend newline
    |> should equal """
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
    formatSourceString false """
[<Tests>]
let tests =
  testList "tests"
    [
      test "test" {
        Expect.equal true true "unexpected"
      }
    ]
"""  config
    |> prepend newline
    |> should equal """
[<Tests>]
let tests =
    testList "tests" [ test "test" { Expect.equal true true "unexpected" } ]
"""

[<Test>]
let ``multiline computation expression with SynExpr.App identifier, 835`` () =
    formatSourceString false """let meh =
    create [] {
        // foo
        // bar
        return 42
    }"""  config
    |> prepend newline
    |> should equal """
let meh =
    create [] {
        // foo
        // bar
        return 42
    }
"""

[<Test>]
let ``multiline computation expression with SynExpr.App identifier and multiple expressions`` () =
    formatSourceString false """
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
"""  config
    |> prepend newline
    |> should equal """
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
    formatSourceString false """
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
"""  config
    |> prepend newline
    |> should equal """
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
    formatSourceString false """
let fetchAsync(name, url:string) =
    async {
        let uri = new System.Uri(url)
        let webClient = new WebClient()
        let! html = webClient.AsyncDownloadString(uri)
        let title = html.CssSelect("title")
        return title
    }
"""  config
    |> prepend newline
    |> should equal """
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
    formatSourceString false """
async {
    let! a = aa

    and! b = bb

    and! c = cc

    return (a + b + c)
}
"""  config
    |> prepend newline
    |> should equal """
async {
    let! a = aa

    and! b = bb

    and! c = cc

    return (a + b + c)
}
"""

[<Test>]
let ``add new line between one-liner and multiline expression, 838`` () =
    formatSourceString false """
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
"""  config
    |> prepend newline
    |> should equal """
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
    formatSourceString false """let foo () =
    async {
        let! a = callA()
        let b = callB()
        let! c = callC()
        let d = callD()
        let! e = callE()
        return (a + b + c - e * d) }
"""  config
    |> prepend newline
    |> should equal """
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
    formatSourceString false """async { return 42 }
"""  config
    |> prepend newline
    |> should equal """
async { return 42 }
"""

[<Test>]
let ``return from multiline computation expression`` () =
    formatSourceString false """async {
    // foo
    return 42 }
"""  config
    |> prepend newline
    |> should equal """
async {
    // foo
    return 42
}
"""

[<Test>]
let ``let + return from ce`` () =
    formatSourceString false """async {
    let a = getA()
    return a
}
"""  config
    |> prepend newline
    |> should equal """
async {
    let a = getA ()
    return a
}
"""

[<Test>]
let ``let rec + return from ce`` () =
    formatSourceString false """async {
    let rec a = getA()
    return a
}
"""  config
    |> prepend newline
    |> should equal """
async {
    let rec a = getA ()
    return a
}
"""

[<Test>]
let ``two let + return from ce`` () =
    formatSourceString false """async {
    let a = getA()
    let b = getB ()
    return a
}
"""  config
    |> prepend newline
    |> should equal """
async {
    let a = getA ()
    let b = getB ()
    return a
}
"""

[<Test>]
let ``let + let rec + let + return from ce`` () =
    formatSourceString false """async {
    let a = getA ()
    let rec b = getB ()
    let c = getC ()
    return a
}
"""  config
    |> prepend newline
    |> should equal """
async {
    let a = getA ()
    let rec b = getB ()
    let c = getC ()
    return a
}
"""

[<Test>]
let ``multiline let + return from ce`` () =
    formatSourceString false """async {
    let a =
        // foo
        getA()
    return a
}
"""  config
    |> prepend newline
    |> should equal """
async {
    let a =
        // foo
        getA ()

    return a
}
"""

[<Test>]
let ``do + return from ce`` () =
    formatSourceString false """async {
    do foo
    return bar
}
"""  config
    |> prepend newline
    |> should equal """
async {
    do foo
    return bar
}
"""

[<Test>]
let ``do! + return from ce`` () =
    formatSourceString false """async {
    do! foo
    return bar
}
"""  config
    |> prepend newline
    |> should equal """
async {
    do! foo
    return bar
}
"""

[<Test>]
let ``do! + let + return from ce`` () =
    formatSourceString false """async {
    do! foo
    let bar = getBar ()
    return bar
}
"""  config
    |> prepend newline
    |> should equal """
async {
    do! foo
    let bar = getBar ()
    return bar
}
"""

[<Test>]
let ``let bang + newline + return`` () =
    formatSourceString false """async {
    let! bar = getBar ()

    return bar
}
"""  config
    |> prepend newline
    |> should equal """
async {
    let! bar = getBar ()

    return bar
}
"""

[<Test>]
let ``let bang + and bang + newline + return`` () =
    formatSourceString false """async {
    let! bar = getBar ()

    and! foo = getFoo ()

    return bar
}
"""  config
    |> prepend newline
    |> should equal """
async {
    let! bar = getBar ()

    and! foo = getFoo ()

    return bar
}
"""

[<Test>]
let ``custom method names`` () =
    formatSourceString false """let indexMachine =
    freyaMachine {
        methods [GET; HEAD; OPTIONS]
        handleOk Pages.home }
"""  config
    |> prepend newline
    |> should equal """
let indexMachine =
    freyaMachine {
        methods [ GET; HEAD; OPTIONS ]
        handleOk Pages.home
    }
"""

[<Test>]
let ``let bang + multiline match in ce`` () =
    formatSourceString false """
let rec runPendingJobs () =
    task {
        let! jobToRun = checkForJob ()
        match jobToRun with
        | None -> return ()
        | Some pendingJob ->
            do! pendingJob ()
            return! runPendingJobs ()
    }

"""  config
    |> prepend newline
    |> should equal """
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
    formatSourceString false """let rec private appendToAzureTableStorage (cosmoEvents: EventWrite<JsonValue> seq) =
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
"""  config
    |> prepend newline
    |> should equal """
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
    formatSourceString false """let appendEvents userId (events: Event list) =
    let cosmoEvents = List.map (createEvent userId) events
    task { do! appendToAzureTableStorage cosmoEvents }
"""  config
    |> prepend newline
    |> should equal """
let appendEvents userId (events: Event list) =
    let cosmoEvents = List.map (createEvent userId) events
    task { do! appendToAzureTableStorage cosmoEvents }
"""

[<Test>]
let ``let bang + let + return in ce`` () =
    formatSourceString false """let getEvents() =
    task {
        let! cosmoEvents = eventStore.GetEvents EventStream AllEvents
        let events = List.map (fun (ce: EventRead<JsonValue, _>) -> ce.Data) cosmoEvents
        return events
    }
"""  config
    |> prepend newline
    |> should equal """
let getEvents () =
    task {
        let! cosmoEvents = eventStore.GetEvents EventStream AllEvents

        let events =
            List.map (fun (ce: EventRead<JsonValue, _>) -> ce.Data) cosmoEvents

        return events
    }
"""

[<Test>]
let ``let bang + do expression + let + return in ce`` () =
    formatSourceString false """
    task {
        let! config = manager.GetConfigurationAsync().ConfigureAwait(false)
        parameters.IssuerSigningKeys <- config.SigningKeys
        let user, _ = handler.ValidateToken((token: string), parameters)
        return Ok(user.Identity.Name, collectClaims user)
    }
"""  config
    |> prepend newline
    |> should equal """
task {
    let! config = manager.GetConfigurationAsync().ConfigureAwait(false)
    parameters.IssuerSigningKeys <- config.SigningKeys

    let user, _ =
        handler.ValidateToken((token: string), parameters)

    return Ok(user.Identity.Name, collectClaims user)
}
"""

[<Test>]
let ``do bang + return in ce`` () =
    formatSourceString false """    let ((userId, _), events) = request
    task {
        do! EventStore.appendEvents userId events
        return sendText "Events persisted"
    }
"""  config
    |> prepend newline
    |> should equal """
let ((userId, _), events) = request

task {
    do! EventStore.appendEvents userId events
    return sendText "Events persisted"
}
"""

[<Test>]
let ``yield bang + yield bang in ce`` () =
    formatSourceString false """
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
"""  config
    |> prepend newline
    |> should equal """
let squares = seq { for i in 1 .. 3 -> i * i }

let cubes = seq { for i in 1 .. 3 -> i * i * i }

let squaresAndCubes =
    seq {
        yield! squares
        yield! cubes
    }
"""

[<Test>]
let ``let bang + yield bang in ce`` () =
    formatSourceString false """let myCollection = seq {
    let! squares = getSquares()
    yield! (squares * level) }
"""  config
    |> prepend newline
    |> should equal """
let myCollection =
    seq {
        let! squares = getSquares ()
        yield! (squares * level)
    }
"""

[<Test>]
let ``return bang in ce`` () =
    formatSourceString false """let req = // 'req' is of type is 'Async<data>'
    async {
        return! fetch url
    }

"""  config
    |> prepend newline
    |> should equal """
let req = // 'req' is of type is 'Async<data>'
    async { return! fetch url }
"""

[<Test>]
let ``saturn router`` () =
    formatSourceString false """
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
"""  config
    |> prepend newline
    |> should equal """
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
    formatSourceString false """module Api

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
    freyaRouter {
        resource "/hello{/name}" helloMachine }
"""  config
    |> prepend newline
    |> should equal """
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

let root =
    freyaRouter { resource "/hello{/name}" helloMachine }
"""

[<Test>]
let ``use bang`` () =
    formatSourceString false """
let resource = promise {
    return new DisposableAction(fun () -> isDisposed := true)
}
promise {
    use! r = resource
    step1ok := not !isDisposed
}
"""  config
    |> prepend newline
    |> should equal """
let resource =
    promise { return new DisposableAction(fun () -> isDisposed := true) }

promise {
    use! r = resource
    step1ok := not !isDisposed
}
"""

[<Test>]
let ``multiline let bang + return in ce`` () =
    formatSourceString false """
   let divideByWorkflow x y w z =
        maybe
            {
            let! a = x |> divideBy y
            let! b = a |> divideBy w
            let! c = b |> divideBy z
            return c
            }
"""  config
    |> prepend newline
    |> should equal """
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
    formatSourceString false """
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
"""  config
    |> prepend newline
    |> should equal """
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
    formatSourceString false """
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
"""  config
    |> prepend newline
    |> should equal """
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
    formatSourceString false """let private getAST log (req: HttpRequest) =
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
"""  config
    |> prepend newline
    |> should equal """
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

[<Test>]
let ``let rec + let bang`` () =
    formatSourceString false """let a =
    async {
        let rec foo a = foo a
        let! bar = async { return foo a }
        return bar
    }
"""  config
    |> prepend newline
    |> should equal """
let a =
    async {
        let rec foo a = foo a
        let! bar = async { return foo a }
        return bar
    }
"""

[<Test>]
let ``new line between let and let bang, 879`` () =
    formatSourceString false """let rec loop () =
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
"""  ({ config with
            SpaceBeforeUppercaseInvocation = true
            IndentSize = 2
            SpaceAroundDelimiter = false
            MultilineBlockBracketsOnSameColumn = true })
    |> prepend newline
    |> should equal """
let rec loop () =
  async {
    let! msg = inbox.Receive ()

    match msg with
    | Handle (eventSource, command, reply) ->
        let! stream = eventSource |> eventStore.GetStream

        let newEvents =
          stream
          |> Result.map
               (asEvents
                >> behaviour command
                >> enveloped eventSource)

        let! result =
          newEvents
          |> function
          | Ok events -> eventStore.Append events
          | Error err -> async { return Error err}

        do reply.Reply result

        return! loop ()
  }
"""