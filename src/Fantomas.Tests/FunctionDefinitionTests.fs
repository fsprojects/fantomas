module Fantomas.Tests.FunctionDefinitionTests

open NUnit.Framework
open FsUnit

open Fantomas.CodeFormatter
open Fantomas.Tests.TestHelper

[<Test>]
let ``recursive functions``() =
    formatSourceString false """
    let rec f x = g x
    and g x = x""" config
    |> prepend newline
    |> should equal """
let rec f x = g x

and g x = x
"""

[<Test>]
let ``should keep mutually recursive functions``() =
    formatSourceString false """
let rec createJArray x = createJObject

and createJObject y = createJArray
    """ config
    |> should equal """let rec createJArray x = createJObject

and createJObject y = createJArray
"""

[<Test>]
let ``should keep mutually recursive functions in nested function``() =
    formatSourceString false """let f =
    let rec createJArray x = createJObject x

    and createJObject y = createJArray y
    createJArray
    """ config
    |> should equal """let f = 
    let rec createJArray x = createJObject x
    and createJObject y = createJArray y
    createJArray
"""

[<Test>]
let ``should keep identifiers with whitespace in double backticks``() =
    formatSourceString false """let ``should keep identifiers in double backticks``() = x
    """ config
    |> should equal """let ``should keep identifiers in double backticks``() = x
"""

[<Test>]
let ``should remove backticks from shouldn't identifier``() =
    formatSourceString false """let ``shouldn't``() = x
    """ config
    |> should equal """let shouldn't() = x
"""

[<Test>]
let ``should keep identifiers with + in double backticks``() =
    formatSourceString false """let ``Foo+Bar``() = x
    """ config
    |> should equal """let ``Foo+Bar``() = x
"""

[<Test>]
let ``let bindings with return types``() =
    formatSourceString false """
       let divide x y =
           let stream : System.IO.FileStream = System.IO.File.Create("test.txt")
           let writer : System.IO.StreamWriter = new System.IO.StreamWriter(stream)
           try
              writer.WriteLine("test1");
              Some( x / y )
           finally
              writer.Flush()
              printfn "Closing stream"
              stream.Close()""" config
    |> prepend newline
    |> should equal """
let divide x y = 
    let stream : System.IO.FileStream = System.IO.File.Create("test.txt")
    let writer : System.IO.StreamWriter = new System.IO.StreamWriter(stream)
    try 
        writer.WriteLine("test1")
        Some(x / y)
    finally
        writer.Flush()
        printfn "Closing stream"
        stream.Close()
"""

[<Test>]
let ``type constraints and inline``() =
    formatSourceString false """
let inline add(value1 : ^T when ^T : (static member (+) : ^T * ^T -> ^T), value2: ^T) =
    value1 + value2

let inline heterogenousAdd(value1 : ^T when (^T or ^U) : (static member (+) : ^T * ^U -> ^T), value2 : ^U) =
    value1 + value2""" config
    |> prepend newline
    |> should equal """
let inline add(value1 : ^T when ^T : (static member (+) : ^T * ^T -> ^T), 
               value2 : ^T) = value1 + value2
let inline heterogenousAdd(value1 : ^T when (^T or ^U) : (static member (+) : ^T * ^U
                                                               -> ^T), 
                           value2 : ^U) = value1 + value2
"""

[<Test>]
let ``should keep whitespace after function call``() =
    formatSourceString false """let relative = (toRelativePath fileName).TrimStart '.'
    """ config
    |> should equal """let relative = (toRelativePath fileName).TrimStart '.'
"""