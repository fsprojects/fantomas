module Fantomas.Tests.CodeFormatterExtExtTests

open NUnit.Framework
open FsUnit

open Fantomas.FormatConfig
open Fantomas.CodeFormatter

let config = FormatConfig.Default
let newline = System.Environment.NewLine

let inline prepend s content = s + content
let inline append s content = content + s

[<Test>]
let ``xml documentation``() =
    formatSourceString """
/// <summary>
/// Kill Weight Mud
/// </summary>
///<param name="sidpp">description</param>
///<param name="tvd">xdescription</param>
///<param name="omw">ydescription</param>
let kwm sidpp tvd omw =
    (sidpp / 0.052 / tvd) + omw

/// Kill Weight Mud
let kwm sidpp tvd omw = 1.0""" config
    |> prepend newline
    |> should equal """
/// <summary>
/// Kill Weight Mud
/// </summary>
///<param name="sidpp">description</param>
///<param name="tvd">xdescription</param>
///<param name="omw">ydescription</param>
let kwm sidpp tvd omw = (sidpp / 0.052 / tvd) + omw

/// Kill Weight Mud
let kwm sidpp tvd omw = 1.0
"""

[<Test>]
let ``type params``() =
    formatSourceString """
let genericSumUnits ( x : float<'u>) (y: float<'u>) = x + y
type vector3D<[<Measure>] 'u> = { x : float<'u>; y : float<'u>; z : float<'u>}""" config
    |> prepend newline
    |> should equal """
let genericSumUnits (x : float<'u>) (y : float<'u>) = x + y

type vector3D<[<Measure>] 'u> = 
    { x : float<'u>;
      y : float<'u>;
      z : float<'u> }
"""

[<Test>]
let ``classes and inheritance``() =
    formatSourceString """
type MyClassBase2(x: int) =
   let mutable z = x * x
   do for i in 1..z do printf "%d " i

type MyClassDerived2(y: int) =
   inherit MyClassBase2(y * 2)
   do for i in 1..y do printf "%d " i""" config
    |> prepend newline
    |> should equal """
type MyClassBase2(x : int) = 
    let mutable z = x * x
    do 
      for i in 1..z do
          printf "%d " i

type MyClassDerived2(y : int) = 
    inherit MyClassBase2(y * 2)
    do 
      for i in 1..y do
          printf "%d " i
"""

[<Test>]
let ``classes and implicit constructors``() =
    formatSourceString """
    type MyClass2(dataIn) as self =
       let data = dataIn
       do self.PrintMessage()
       member this.PrintMessage() =
           printf "Creating MyClass2 with Data %d" data""" config
    |> prepend newline
    |> should equal """
type MyClass2(dataIn) as self = 
    let data = dataIn
    do self.PrintMessage()
    member this.PrintMessage() = printf "Creating MyClass2 with Data %d" data
"""

[<Test>]
let ``interfaces and inheritance``() =
    formatSourceString """
type IPrintable =
   abstract member Print : unit -> unit

type SomeClass1(x: int, y: float) =
   interface IPrintable with 
      member this.Print() = printfn "%d %f" x y
type Interface3 =
    inherit Interface1
    inherit Interface2
    abstract member Method3 : int -> int""" config
    |> prepend newline
    |> should equal """
type IPrintable = 
    abstract member Print : unit -> unit

type SomeClass1(x : int, y : float) = 
    interface IPrintable with
        member this.Print() = printfn "%d %f" x y

type Interface3 = 
    inherit Interface1
    inherit Interface2
    abstract member Method3 : int -> int
"""

[<Test>]
let ``recursive classes``() =
    formatSourceString """
type Folder(pathIn: string) =
  let path = pathIn
  let filenameArray : string array = System.IO.Directory.GetFiles(path)
  member this.FileArray = Array.map (fun elem -> new File(elem, this)) filenameArray

and File(filename: string, containingFolder: Folder) = 
   member __.Name = filename
   member __.ContainingFolder = containingFolder""" config
    |> prepend newline
    |> should equal """
type Folder(pathIn : string) = 
    let path = pathIn
    let filenameArray : string array = System.IO.Directory.GetFiles(path)
    member this.FileArray = Array.map(fun elem -> new File(elem, this)) filenameArray

and File(filename : string, containingFolder : Folder) = 
    member __.Name = filename
    member __.ContainingFolder = containingFolder
"""


