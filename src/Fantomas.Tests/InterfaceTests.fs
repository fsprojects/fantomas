module Fantomas.Tests.InterfaceTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper

[<Test>]
let ``interfaces and inheritance`` () =
    formatSourceString
        false
        """
type IPrintable =
   abstract member Print : unit -> unit

type SomeClass1(x: int, y: float) =
   interface IPrintable with
      member this.Print() = printfn "%d %f" x y
type Interface3 =
    inherit Interface1
    inherit Interface2
    abstract member Method3 : int -> int"""
        { config with MaxFunctionBindingWidth = 120 }
    |> prepend newline
    |> should
        equal
        """
type IPrintable =
    abstract member Print: unit -> unit

type SomeClass1(x: int, y: float) =
    interface IPrintable with
        member this.Print() = printfn "%d %f" x y

type Interface3 =
    inherit Interface1
    inherit Interface2
    abstract member Method3: int -> int
"""

[<Test>]
let ``should not add with to interface definitions with no members`` () =
    formatSourceString
        false
        """type Text(text : string) =
    interface IDocument

    interface Infrastucture with
        member this.Serialize sb = sb.AppendFormat("\"{0}\"", escape v)
        member this.ToXml() = v :> obj
    """
        { config with MaxValueBindingWidth = 120 }
    |> should
        equal
        """type Text(text: string) =
    interface IDocument

    interface Infrastucture with
        member this.Serialize sb = sb.AppendFormat("\"{0}\"", escape v)
        member this.ToXml() = v :> obj
"""

[<Test>]
let ``object expressions`` () =
    formatSourceString false """let obj1 = { new System.Object() with member x.ToString() = "F#" }""" config
    |> prepend newline
    |> should
        equal
        """
let obj1 =
    { new System.Object() with
        member x.ToString() = "F#" }
"""

[<Test>]
let ``object expressions and interfaces`` () =
    formatSourceString
        false
        """
    let implementer() =
        { new ISecond with
            member this.H() = ()
            member this.J() = ()
          interface IFirst with
            member this.F() = ()
            member this.G() = () }"""
        config
    |> prepend newline
    |> should
        equal
        """
let implementer () =
    { new ISecond with
        member this.H() = ()
        member this.J() = ()
      interface IFirst with
          member this.F() = ()
          member this.G() = () }
"""

[<Test>]
let ``should not add with to interfaces with no members in object expressions`` () =
    formatSourceString
        false
        """
let f () =
    { new obj() with
        member x.ToString() = "INotifyEnumerableInternal"
      interface INotifyEnumerableInternal<'T>
      interface IEnumerable<_> with
        member x.GetEnumerator() = null }"""
        { config with MaxValueBindingWidth = 120 }
    |> prepend newline
    |> should
        equal
        """
let f () =
    { new obj() with
        member x.ToString() = "INotifyEnumerableInternal"
      interface INotifyEnumerableInternal<'T>
      interface IEnumerable<_> with
          member x.GetEnumerator() = null }
"""

[<Test>]
let ``should keep named arguments on abstract members`` () =
    formatSourceString
        false
        """type IThing =
    abstract Foo : name:string * age:int -> bool
"""
        config
    |> should
        equal
        """type IThing =
    abstract Foo: name: string * age: int -> bool
"""

[<Test>]
let ``should not skip 'with get()' in indexers`` () =
    formatSourceString
        false
        """type Interface =
    abstract Item : int -> char with get
"""
        config
    |> should
        equal
        """type Interface =
    abstract Item: int -> char with get
"""

[<Test>]
let ``override keyword should be preserved`` () =
    formatSourceString
        false
        """open System

type T() =
    interface IDisposable with
        override x.Dispose() = ()"""
        config
    |> prepend newline
    |> should
        equal
        """
open System

type T() =
    interface IDisposable with
        override x.Dispose() = ()
"""

[<Test>]
let ``combination of override and member`` () =
    formatSourceString
        false
        """type LogInterface =
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
        override x.Version () = ()"""
        { config with
            MaxLineLength = 119
            MaxFunctionBindingWidth = 120
            MaxIfThenElseShortWidth = 80 }
    |> prepend newline
    |> should
        equal
        """
type LogInterface =
    abstract member Print: string -> unit
    abstract member GetLogFile: string -> string
    abstract member Info: unit -> unit
    abstract member Version: unit -> unit

type MyLogInteface() =
    interface LogInterface with
        member x.Print msg = printfn "%s" msg

        override x.GetLogFile environment =
            if environment = "DEV" then "dev.log" else sprintf "date-%s.log" environment

        member x.Info() = ()
        override x.Version() = ()
"""

[<Test>]
let ``interface with comment after equal`` () =
    formatSourceString
        false
        """
/// Interface that must be implemented by all Argu template types
type IArgParserTemplate =
    /// returns a usage string for every union case
    abstract Usage : string
"""
        config
    |> should
        equal
        """/// Interface that must be implemented by all Argu template types
type IArgParserTemplate =
    /// returns a usage string for every union case
    abstract Usage: string
"""

[<Test>]
let ``generic interface member should have space after name`` () =
    formatSourceString
        false
        """
type IFunc<'R> =
    abstract Invoke<'T> : unit -> 'R // without this space the code is invalid
"""
        config
    |> fun formatted -> formatSourceString false formatted config
    |> should
        equal
        """type IFunc<'R> =
    abstract Invoke<'T> : unit -> 'R // without this space the code is invalid
"""

[<Test>]
let ``long abstract member definition, 435`` () =
    formatSourceString
        false
        """
type Test =
    abstract RunJobs: folder:string * ?jobs:string * ?ctm:string * ?createDuplicate:bool * ?hold:bool * ?ignoreCriteria:bool * ?independentFlow:bool * ?orderDate:string * ?orderIntoFolder:string * ?variables:Dictionary<string, string>[] * ?waitForOrderDate:bool
     -> string

    override this.RunJobs(folder: string, ?jobs: string, ?ctm: string, ?createDuplicate: bool, ?hold: bool,
                          ?ignoreCriteria: bool, ?independentFlow: bool, ?orderDate: string, ?orderIntoFolder: string,
                          ?variables: Dictionary<string, string>[], ?waitForOrderDate: bool) =
        ""
"""
        config
    |> prepend newline
    |> should
        equal
        """
type Test =
    abstract RunJobs:
        folder: string *
        ?jobs: string *
        ?ctm: string *
        ?createDuplicate: bool *
        ?hold: bool *
        ?ignoreCriteria: bool *
        ?independentFlow: bool *
        ?orderDate: string *
        ?orderIntoFolder: string *
        ?variables: Dictionary<string, string>[] *
        ?waitForOrderDate: bool ->
            string

    override this.RunJobs
        (
            folder: string,
            ?jobs: string,
            ?ctm: string,
            ?createDuplicate: bool,
            ?hold: bool,
            ?ignoreCriteria: bool,
            ?independentFlow: bool,
            ?orderDate: string,
            ?orderIntoFolder: string,
            ?variables: Dictionary<string, string>[],
            ?waitForOrderDate: bool
        ) =
        ""
"""

[<Test>]
let ``interface with get/set members`` () =
    formatSourceString
        false
        """
type IMyInterface =
    abstract MyProp : bool with get, set
    abstract MyMethod : unit -> unit
"""
        config
    |> prepend newline
    |> should
        equal
        """
type IMyInterface =
    abstract MyProp: bool with get, set
    abstract MyMethod: unit -> unit
"""

[<Test>]
let ``default interface member consumption`` () =
    formatSourceString
        false
        """
open CSharp

// You can implement the interface via a class
type MyType() =
    member _.M() = ()

    interface MyDim

let md = MyType() :> MyDim
printfn "DIM from C#: %d" md.Z

// You can also implement it via an object expression
let md' = { new MyDim }
printfn "DIM from C# but via Object Expression: %d" md'.Z
"""
        config
    |> prepend newline
    |> should
        equal
        """
open CSharp

// You can implement the interface via a class
type MyType() =
    member _.M() = ()

    interface MyDim

let md = MyType() :> MyDim
printfn "DIM from C#: %d" md.Z

// You can also implement it via an object expression
let md' = { new MyDim }
printfn "DIM from C# but via Object Expression: %d" md'.Z
"""

[<Test>]
let ``long member in type declaration, 1362`` () =
    formatSourceString
        false
        """
type IFoo =
    abstract Blah : foo : string -> bar : string -> baz : string -> int
"""
        { config with
            MaxLineLength = 50
            SpaceBeforeColon = true }
    |> prepend newline
    |> should
        equal
        """
type IFoo =
    abstract Blah :
        foo : string ->
        bar : string ->
        baz : string ->
            int
"""

[<Test>]
let ``long member in type declaration, not named`` () =
    formatSourceString
        false
        """
type IFoo =
    abstract Blah : string -> string -> string -> int -> string -> string
"""
        { config with
            MaxLineLength = 60
            SpaceBeforeColon = true }
    |> prepend newline
    |> should
        equal
        """
type IFoo =
    abstract Blah :
        string ->
        string ->
        string ->
        int ->
        string ->
            string
"""

[<Test>]
let ``long member with tuple in type declaration`` () =
    formatSourceString
        false
        """
type IFoo =
    abstract Bar : [<Path "bar">] bar : string  * [<Path "baz">] baz : string ->  Task<Foo>
"""
        { config with
            MaxLineLength = 60
            SpaceBeforeColon = true }
    |> prepend newline
    |> should
        equal
        """
type IFoo =
    abstract Bar :
        [<Path "bar">] bar : string *
        [<Path "baz">] baz : string ->
            Task<Foo>
"""

[<Test>]
let ``long member with mixed type declaration`` () =
    formatSourceString
        false
        """
type IFoo =
    abstract Bar : i : int -> a : string * foo : int -> string
"""
        { config with
            MaxLineLength = 50
            SpaceBeforeColon = true }
    |> prepend newline
    |> should
        equal
        """
type IFoo =
    abstract Bar :
        i : int ->
        a : string * foo : int ->
            string
"""

[<Test>]
let ``long member with mixed type declaration with a long name`` () =
    formatSourceString
        false
        """
type IFoo =
    abstract Bar : i : int -> a : string * foo : int * someReallyLongNameThatMakesTheTupleMultiLine : string -> string
"""
        { config with
            MaxLineLength = 60
            SpaceBeforeColon = true }
    |> prepend newline
    |> should
        equal
        """
type IFoo =
    abstract Bar :
        i : int ->
        a : string *
        foo : int *
        someReallyLongNameThatMakesTheTupleMultiLine : string ->
            string
"""

[<Test>]
let ``print trivia before object expression, 1388`` () =
    formatSourceString
        false
        """
let test () =
    let something = "something"

    { new IDisposable with
        override this.Dispose() = dispose somethingElse }
"""
        config
    |> prepend newline
    |> should
        equal
        """
let test () =
    let something = "something"

    { new IDisposable with
        override this.Dispose() = dispose somethingElse }
"""

[<Test>]
let ``attribute before interface member, 1668`` () =
    formatSourceString
        false
        """
type internal WorkingShard =
    | Started of ReactoKinesixShardProcessor
    | Stopped of StoppedReason

and ReactoKinesixApp
    private
    (
        kinesis: IAmazonKinesis,
        dynamoDB: IAmazonDynamoDB,
        appName: string,
        streamName: string,
        workerId: string,
        processorFactory: IRecordProcessorFactory,
        config: ReactoKinesixConfig
    ) as this =


    interface IReactoKinesixApp with
        [<CLIEvent>] member this.OnInitialized = initializedEvent.Publish
        [<CLIEvent>] member this.OnBatchProcessed = batchProcessedEvent.Publish
"""
        config
    |> prepend newline
    |> should
        equal
        """
type internal WorkingShard =
    | Started of ReactoKinesixShardProcessor
    | Stopped of StoppedReason

and ReactoKinesixApp
    private
    (
        kinesis: IAmazonKinesis,
        dynamoDB: IAmazonDynamoDB,
        appName: string,
        streamName: string,
        workerId: string,
        processorFactory: IRecordProcessorFactory,
        config: ReactoKinesixConfig
    ) as this =


    interface IReactoKinesixApp with
        [<CLIEvent>]
        member this.OnInitialized = initializedEvent.Publish

        [<CLIEvent>]
        member this.OnBatchProcessed = batchProcessedEvent.Publish
"""

[<Test>]
let ``recursive type where second type has interface with attributes on the members`` () =
    formatSourceString
        false
        """
type Foo = | Foo of string

and Bar =
    interface IMeh with
        [<SomeAttribute>] member this.Value = ()
        [<SomeAttribute>] member this.ValueWithReturnType : unit = ()
        [<SomeAttribute>] member this.SomeFunction (a: int) = 4 + a
        [<SomeAttribute>] member this.SomeFunctionWithReturnType (a: int) : int = a + 5
"""
        config
    |> prepend newline
    |> should
        equal
        """
type Foo = Foo of string

and Bar =
    interface IMeh with
        [<SomeAttribute>]
        member this.Value = ()

        [<SomeAttribute>]
        member this.ValueWithReturnType: unit = ()

        [<SomeAttribute>]
        member this.SomeFunction(a: int) = 4 + a

        [<SomeAttribute>]
        member this.SomeFunctionWithReturnType(a: int) : int = a + 5
"""
