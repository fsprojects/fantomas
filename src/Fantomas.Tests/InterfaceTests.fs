module Fantomas.Tests.InterfaceTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper

[<Test>]
let ``interfaces and inheritance``() =
    formatSourceString false """
type IPrintable =
   abstract member Print : unit -> unit

type SomeClass1(x: int, y: float) =
   interface IPrintable with 
      member this.Print() = printfn "%d %f" x y
type Interface3 =
    inherit Interface1
    inherit Interface2
    abstract member Method3 : int -> int""" { config with MaxFunctionBindingWidth = 120 }
    |> prepend newline
    |> should equal """
type IPrintable =
    abstract Print: unit -> unit

type SomeClass1(x: int, y: float) =
    interface IPrintable with
        member this.Print() = printfn "%d %f" x y

type Interface3 =
    inherit Interface1
    inherit Interface2
    abstract Method3: int -> int
"""

[<Test>]
let ``should not add with to interface definitions with no members``() =
    formatSourceString false """type Text(text : string) = 
    interface IDocument
        
    interface Infrastucture with
        member this.Serialize sb = sb.AppendFormat("\"{0}\"", escape v)
        member this.ToXml() = v :> obj
    """ { config with MaxValueBindingWidth = 120 }
    |> should equal """type Text(text: string) =
    interface IDocument

    interface Infrastucture with
        member this.Serialize sb = sb.AppendFormat("\"{0}\"", escape v)
        member this.ToXml() = v :> obj
"""

[<Test>]
let ``object expressions``() =
    formatSourceString false """let obj1 = { new System.Object() with member x.ToString() = "F#" }""" config
    |> prepend newline
    |> should equal """
let obj1 =
    { new System.Object() with
        member x.ToString() = "F#" }
"""

[<Test>]
let ``object expressions and interfaces``() =
    formatSourceString false """
    let implementer() = 
        { new ISecond with 
            member this.H() = ()
            member this.J() = ()
          interface IFirst with 
            member this.F() = ()
            member this.G() = () }""" config
    |> prepend newline
    |> should equal """
let implementer () =
    { new ISecond with
        member this.H() = ()
        member this.J() = ()
      interface IFirst with
          member this.F() = ()
          member this.G() = () }
"""

[<Test>]
let ``should not add with to interfaces with no members in object expressions``() =
    formatSourceString false """
let f () =       
    { new obj() with
        member x.ToString() = "INotifyEnumerableInternal"
      interface INotifyEnumerableInternal<'T>
      interface IEnumerable<_> with
        member x.GetEnumerator() = null }""" { config with MaxValueBindingWidth = 120 }
    |> prepend newline
    |> should equal """
let f () =
    { new obj() with
        member x.ToString() = "INotifyEnumerableInternal"
      interface INotifyEnumerableInternal<'T>
      interface IEnumerable<_> with
          member x.GetEnumerator() = null }
"""

[<Test>]
let ``should keep named arguments on abstract members``() =
    formatSourceString false """type IThing =
    abstract Foo : name:string * age:int -> bool
"""  config
    |> should equal """type IThing =
    abstract Foo: name:string * age:int -> bool
"""

[<Test>]
let ``should not skip 'with get()' in indexers``() =
    formatSourceString false """type Interface = 
    abstract Item : int -> char with get
"""  config
    |> should equal """type Interface =
    abstract Item: int -> char with get
"""

[<Test>]
let ``override keyword should be preserved`` () =
    formatSourceString false """open System
    
type T() =
    interface IDisposable with
        override x.Dispose() = ()""" config
    |> prepend newline
    |> should equal """
open System

type T() =
    interface IDisposable with
        override x.Dispose() = ()
"""

[<Test>]
let ``combination of override and member`` () =
    formatSourceString false """type LogInterface =
    abstract member Print: string -> unit
    abstract member GetLogFile: string -> string
    abstract member Info: unit -> unit
    abstract member Version: unit -> unit

type MyLogInteface() =
    interface LogInterface with
        member x.Print msg = printfn "%s" msg
        override x.GetLogFile environment =
            if environment = "DEV" then 
                "dev.log"
            else
                sprintf "date-%s.log" environment
        member x.Info () = ()
        override x.Version () = ()""" ({ config with MaxLineLength = 119; MaxFunctionBindingWidth = 120 })
    |> prepend newline
    |> should equal """
type LogInterface =
    abstract Print: string -> unit
    abstract GetLogFile: string -> string
    abstract Info: unit -> unit
    abstract Version: unit -> unit

type MyLogInteface() =
    interface LogInterface with
        member x.Print msg = printfn "%s" msg

        override x.GetLogFile environment =
            if environment = "DEV" then "dev.log" else sprintf "date-%s.log" environment

        member x.Info() = ()
        override x.Version() = ()
"""


[<Test>]
let ``Interface with comment after equal`` () =
    formatSourceString false """
/// Interface that must be implemented by all Argu template types
type IArgParserTemplate =
    /// returns a usage string for every union case
    abstract Usage : string
"""  config
    |> should equal """/// Interface that must be implemented by all Argu template types
type IArgParserTemplate =
    /// returns a usage string for every union case
    abstract Usage: string
"""


[<Test>]
let ``generic interface member should have space after name`` () =
    let source = """
type IFunc<'R> =
    abstract Invoke<'T> : unit -> 'R // without this space the code is invalid
"""

    formatSourceString false source config
    |> fun formatted -> formatSourceString false formatted config
    |> should equal """type IFunc<'R> =
    abstract Invoke<'T> : unit -> 'R // without this space the code is invalid
"""