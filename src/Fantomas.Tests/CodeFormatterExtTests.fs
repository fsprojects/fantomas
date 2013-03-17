module Fantomas.Tests.CodeFormatterExtTests

open NUnit.Framework
open FsUnit

open Fantomas.FormatConfig
open Fantomas.CodeFormatter

let config = FormatConfig.Default
let newline = System.Environment.NewLine

let inline prepend s content = s + content
let inline append s content = content + s

[<Test>]
let ``line, file and path identifiers``() =
    formatSourceString """
    let printSourceLocation() =
        printfn "Line: %s" __LINE__
        printfn "Source Directory: %s" __SOURCE_DIRECTORY__
        printfn "Source File: %s" __SOURCE_FILE__
    printSourceLocation()
    """ config
    |> prepend newline
    |> should equal """
let printSourceLocation() = 
    printfn "Line: %s" __LINE__
    printfn "Source Directory: %s" __SOURCE_DIRECTORY__
    printfn "Source File: %s" __SOURCE_FILE__

printSourceLocation()"""

[<Test>]
let ``async workflows``() =
    formatSourceString """
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
    """ config
    |> prepend newline
    |> should equal """
let fetchAsync(name, url : string) = 
    async { 
        try 
            let uri = new System.Uri(url)
            let webClient = new WebClient()
            let! html = webClient.AsyncDownloadString(uri)
            printfn "Read %d characters for %s" html.Length name
        with
        | ex -> printfn "%s" (ex.Message) }
"""

[<Test>]
let ``computation expressions``() =
    formatSourceString """
let comp =
    eventually { for x in 1 .. 2 do
                    printfn " x = %d" x
                 return 3 + 4 }""" config
    |> prepend newline
    |> should equal """
let comp = 
    eventually { 
        for x in 1..2 do
            printfn " x = %d" x
        return 3 + 4 }
"""

[<Test>]
let ``sequence expressions``() =
    formatSourceString """
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
let s1 = seq { for i in 1..10 do yield i * i }

let s2 = seq { 0..10..100 }

let rec inorder tree = 
    seq { 
        match tree with
        | Tree(x, left, right) -> 
            yield! inorder left
            yield x
            yield! inorder right
        | Leaf x -> yield x }
"""

[<Test>]
let ``let bindings with return types``() =
    formatSourceString """
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
let ``when clauses and as patterns``() =
    formatSourceString """
let rangeTest testValue mid size =
    match testValue with
    | var1 when var1 >= mid - size/2 && var1 <= mid + size/2 -> printfn "The test value is in range."
    | _ -> printfn "The test value is out of range."

let (var1, var2) as tuple1 = (1, 2)
printfn "%d %d %A" var1 var2 tuple1""" config
    |> prepend newline
    |> should equal """
let rangeTest testValue mid size = 
    match testValue with
    | var1 when var1 >= mid - size / 2 && var1 <= mid + size / 2 -> printfn "The test value is in range."
    | _ -> printfn "The test value is out of range."

let (var1, var2) as tuple1 = (1, 2)

printfn "%d %d %A" var1 var2 tuple1"""

[<Test>]
let ``and & or patterns``() =
    formatSourceString """
let detectZeroOR point =
    match point with
    | (0, 0) | (0, _) | (_, 0) -> printfn "Zero found."
    | _ -> printfn "Both nonzero."

let detectZeroAND point =
    match point with
    | (0, 0) -> printfn "Both values zero."
    | (var1, var2) & (0, _) -> printfn "First value is 0 in (%d, %d)" var1 var2
    | (var1, var2)  & (_, 0) -> printfn "Second value is 0 in (%d, %d)" var1 var2
    | _ -> printfn "Both nonzero."
"""  config
    |> prepend newline
    |> should equal """
let detectZeroOR point = 
    match point with
    | (0, 0) | (0, _) | (_, 0) -> printfn "Zero found."
    | _ -> printfn "Both nonzero."

let detectZeroAND point = 
    match point with
    | (0, 0) -> printfn "Both values zero."
    | (var1, var2) & (0, _) -> printfn "First value is 0 in (%d, %d)" var1 var2
    | (var1, var2) & (_, 0) -> printfn "Second value is 0 in (%d, %d)" var1 var2
    | _ -> printfn "Both nonzero."
"""

[<Test>]
let ``cons and list patterns``() =
    formatSourceString """
let rec printList l =
    match l with
    | head :: tail -> printf "%d " head; printList tail
    | [] -> printfn ""

let listLength list =
    match list with
    | [] -> 0
    | [ _ ] -> 1
    | [ _; _ ] -> 2
    | [ _; _; _ ] -> 3
    | _ -> List.length list"""  config
    |> prepend newline
    |> should equal """
let rec printList l = 
    match l with
    | head :: tail -> 
        printf "%d " head
        printList tail
    | [] -> printfn ""

let listLength list = 
    match list with
    | [] -> 0
    | [_] -> 1
    | [_; _] -> 2
    | [_; _; _] -> 3
    | _ -> List.length list
"""

[<Test>]
let ``array patterns``() =
    formatSourceString """
let vectorLength vec =
    match vec with
    | [| var1 |] -> var1
    | [| var1; var2 |] -> sqrt (var1*var1 + var2*var2)
    | [| var1; var2; var3 |] -> sqrt (var1*var1 + var2*var2 + var3*var3)
    | _ -> failwith "vectorLength called with an unsupported array size of %d." (vec.Length)""" config
    |> prepend newline
    |> should equal """
let vectorLength vec = 
    match vec with
    | [|var1|] -> var1
    | [|var1; var2|] -> sqrt(var1 * var1 + var2 * var2)
    | [|var1; var2; var3|] -> sqrt(var1 * var1 + var2 * var2 + var3 * var3)
    | _ -> failwith "vectorLength called with an unsupported array size of %d." (vec.Length)
"""

[<Test>]
let ``paren and tuple patterns``() =
    formatSourceString """
let countValues list value =
    let rec checkList list acc =
       match list with
       | (elem1 & head) :: tail when elem1 = value -> checkList tail (acc + 1)
       | head :: tail -> checkList tail acc
       | [] -> acc
    checkList list 0

let detectZeroTuple point =
    match point with
    | (0, 0) -> printfn "Both values zero."
    | (0, var2) -> printfn "First value is 0 in (0, %d)" var2
    | (var1, 0) -> printfn "Second value is 0 in (%d, 0)" var1
    | _ -> printfn "Both nonzero."
"""  config
    |> prepend newline
    |> should equal """
let countValues list value = 
    let rec checkList list acc = 
        match list with
        | (elem1 & head) :: tail when elem1 = value -> checkList tail (acc + 1)
        | head :: tail -> checkList tail acc
        | [] -> acc
    checkList list 0

let detectZeroTuple point = 
    match point with
    | (0, 0) -> printfn "Both values zero."
    | (0, var2) -> printfn "First value is 0 in (0, %d)" var2
    | (var1, 0) -> printfn "Second value is 0 in (%d, 0)" var1
    | _ -> printfn "Both nonzero."
"""

[<Test>]
let ``type test and null patterns``() =
    formatSourceString """
let detect1 x =
    match x with
    | 1 -> printfn "Found a 1!"
    | (var1 : int) -> printfn "%d" var1

let RegisterControl(control:Control) =
    match control with
    | :? Button as button -> button.Text <- "Registered."
    | :? CheckBox as checkbox -> checkbox.Text <- "Registered."
    | _ -> ()

let ReadFromFile (reader : System.IO.StreamReader) =
    match reader.ReadLine() with
    | null -> printfn "\n"; false
    | line -> printfn "%s" line; true""" config
    |> prepend newline
    |> should equal """
let detect1 x = 
    match x with
    | 1 -> printfn "Found a 1!"
    | (var1 : int) -> printfn "%d" var1

let RegisterControl(control : Control) = 
    match control with
    | :? Button as button -> button.Text <- "Registered."
    | :? CheckBox as checkbox -> checkbox.Text <- "Registered."
    | _ -> ()

let ReadFromFile(reader : System.IO.StreamReader) = 
    match reader.ReadLine() with
    | null -> 
        printfn "\n"
        false
    | line -> 
        printfn "%s" line
        true
"""

[<Test>]
let ``record patterns``() =
    formatSourceString """
type MyRecord = { Name: string; ID: int }

let IsMatchByName record1 (name: string) =
    match record1 with
    | { MyRecord.Name = nameFound; ID = _; } when nameFound = name -> true
    | _ -> false """ config
    |> prepend newline
    |> should equal """
type MyRecord = 
    { Name : string;
      ID : int }

let IsMatchByName record1 (name : string) = 
    match record1 with
    | { MyRecord.Name = nameFound; ID = _ } when nameFound = name -> true
    | _ -> false
"""

[<Test>]
let ``active patterns``() =
    formatSourceString """
let (|Even|Odd|) input = if input % 2 = 0 then Even else Odd

let (|Integer|_|) (str: string) =
   let mutable intvalue = 0
   if System.Int32.TryParse(str, &intvalue) then Some(intvalue)
   else None

let (|ParseRegex|_|) regex str =
   let m = Regex(regex).Match(str)
   if m.Success
   then Some (List.tail [ for x in m.Groups -> x.Value ])
   else None""" config
    |> prepend newline
    |> should equal """
let (|Even|Odd|) input = 
    if input % 2 = 0 then Even
    else Odd

let (|Integer|_|)(str : string) = 
    let mutable intvalue = 0
    if System.Int32.TryParse(str, &intvalue) then Some(intvalue)
    else None

let (|ParseRegex|_|) regex str = 
    let m = Regex(regex).Match(str)
    if m.Success then Some(List.tail [for x in m.Groups do yield x.Value])
    else None
"""

[<Test>]
let ``if/then/else block``() =
    formatSourceString """
let rec tryFindMatch pred list =
    match list with
    | head :: tail -> if pred(head)
                        then Some(head)
                        else tryFindMatch pred tail
    | [] -> None

let test x y =
  if x = y then "equals" 
  elif x < y then "is less than" 
  else "is greater than"

if age < 10
then printfn "You are only %d years old and already learning F#? Wow!" age""" config
    |> prepend newline
    |> should equal """
let rec tryFindMatch pred list = 
    match list with
    | head :: tail -> 
        if pred(head) then Some(head)
        else tryFindMatch pred tail
    | [] -> None

let test x y = 
    if x = y then "equals"
    else
        if x < y then "is less than"
        else "is greater than"

if age < 10 then printfn "You are only %d years old and already learning F#? Wow!" age"""

[<Test>]
let ``records with update``() =
    formatSourceString """
type Car = {
    Make : string
    Model : string
    mutable Odometer : int
    }

let myRecord3 = { myRecord2 with Y = 100; Z = 2 }""" config
    |> prepend newline
    |> should equal """
type Car = 
    { Make : string;
      Model : string;
      mutable Odometer : int }

let myRecord3 = { myRecord2 with Y = 100; Z = 2 }
"""

[<Test>]
let ``enums conversion``() =
    formatSourceString """
type uColor =
   | Red = 0u
   | Green = 1u
   | Blue = 2u
let col3 = Microsoft.FSharp.Core.LanguagePrimitives.EnumOfValue<uint32, uColor>(2u)""" config
    |> prepend newline
    |> should equal """
type uColor = 
    | Red = 0u
    | Green = 1u
    | Blue = 2u

let col3 = Microsoft.FSharp.Core.LanguagePrimitives.EnumOfValue<uint32, uColor>(2u)
"""