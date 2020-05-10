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
                                                        MaxLetBindingWidth = 65 })
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