#r "../../lib/FSharp.Compiler.dll"

#load "FormatConfig.fs"
#load "SourceParser.fs"
#load "CodePrinter.fs"
#load "CodeFormatter.fs"

open Fantomas.FormatConfig
open Fantomas.SourceParser
open Fantomas.CodePrinter
open Fantomas.CodeFormatter

let config = FormatConfig.Default

let t01 = """
    type MyClass2(dataIn) as self =
       let data = dataIn
       do
           self.PrintMessage()
       member this.PrintMessage() =
           printf "Creating MyClass2 with Data %d" data"""

let t02 = """
    type MyClass1(x: int, y: int) =
     do printfn "%d %d" x y
     new() = MyClass1(0, 0)"""

let t03 = """
    type Point2D =
       struct 
          val X: float
          val Y: float
          new(x: float, y: float) = { X = x; Y = y }
       end"""

let t04 = """
    type MyClassBase1() =
       let mutable z = 0
       abstract member function1 : int -> int
       default u.function1(a : int) = z <- z + a; z

    type MyClassDerived1() =
       inherit MyClassBase1()
       override u.function1(a: int) = a + 1"""

let t05 = """
    seq { for i in 1 .. 10 -> i * i }"""

let t06 = """
       let divide x y =
           let stream : System.IO.FileStream = System.IO.File.Create("test.txt")
           let writer : System.IO.StreamWriter = new System.IO.StreamWriter(stream)
           try
              writer.WriteLine("test1");
              Some( x / y )
           finally
              writer.Flush()
              printfn "Closing stream"
              stream.Close()"""

let t07 = """
let rangeTest testValue mid size =
    match testValue with
    | var1 when var1 >= mid - size/2 && var1 <= mid + size/2 -> printfn "The test value is in range."
    | _ -> printfn "The test value is out of range."
"""

let t08 = """
let fetchAsync(name, url:string) =
    async { 
        try 
            let uri = new System.Uri(url)
            let webClient = new WebClient()
            let! html = webClient.AsyncDownloadString(uri)
            printfn "Read %d characters for %s" html.Length name
        with
            | ex -> printfn "%s" (ex.Message);
    }
    """

let t09 = """
let comp =
    eventually { for x in 1 .. 2 do
                    printfn " x = %d" x
                 return 3 + 4 }"""

let t10 = """
let runAll() =
    urlList
    |> Seq.map fetchAsync
    |> Async.Parallel 
    |> Async.RunSynchronously
    |> ignore
    """;;

printfn "Result:\n%s" <| formatSourceString t01 config;;
printfn "Result:\n%s" <| formatSourceString t02 config;;
printfn "Result:\n%s" <| formatSourceString t03 config;;
printfn "Result:\n%s" <| formatSourceString t04 config;;
printfn "Result:\n%s" <| formatSourceString t05 config;;
printfn "Result:\n%s" <| formatSourceString t06 config;;
printfn "Result:\n%s" <| formatSourceString t07 config;;
printfn "Result:\n%s" <| formatSourceString t08 config;;
printfn "Result:\n%s" <| formatSourceString t09 config;;
printfn "Result:\n%s" <| formatSourceString t10 config;;