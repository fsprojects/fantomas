module Fantomas.Core.Tests.TypeDeclarationTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelpers
open Fantomas.Core

[<Test>]
let ``exception declarations`` () =
    formatSourceString "exception Error2 of string * int" config
    |> should
        equal
        """exception Error2 of string * int
"""

[<Test>]
let ``exception declarations with members`` () =
    formatSourceString
        """/// An exception type to signal build errors.
exception BuildException of string*list<string>
  with
    override x.ToString() = x.Data0.ToString() + "\r\n" + (separated "\r\n" x.Data1)"""
        { config with
            MaxInfixOperatorExpression = 60
            MaxFunctionBindingWidth = 120 }
    |> should
        equal
        """/// An exception type to signal build errors.
exception BuildException of string * list<string> with
    override x.ToString() = x.Data0.ToString() + "\r\n" + (separated "\r\n" x.Data1)
"""

[<Test>]
let ``comment after with keyword in exception type`` () =
    formatSourceString
        """
exception FooException  with  // comment
    member this.Bar ()  =  ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
exception FooException with // comment
    member this.Bar() = ()
"""

[<Test>]
let ``comment after with keyword in exception type in signature files`` () =
    formatSignatureString
        """
namespace Moon

exception FooException  with  // comment
    member Bar: unit -> unit
"""
        config
    |> prepend newline
    |> should
        equal
        """
namespace Moon

exception FooException with // comment
    member Bar: unit -> unit
"""

[<Test>]
let ``type annotations`` () =
    formatSourceString
        """
    let iterate1 (f : unit -> seq<int>) =
        for e in f() do printfn "%d" e
    let iterate2 (f : unit -> #seq<int>) =
        for e in f() do printfn "%d" e"""
        config
    |> prepend newline
    |> should
        equal
        """
let iterate1 (f: unit -> seq<int>) =
    for e in f () do
        printfn "%d" e

let iterate2 (f: unit -> #seq<int>) =
    for e in f () do
        printfn "%d" e
"""

[<Test>]
let ``upcast and downcast`` () =
    formatSourceString
        """
    let base1 = d1 :> Base1
    let derived1 = base1 :?> Derived1"""
        config
    |> prepend newline
    |> should
        equal
        """
let base1 = d1 :> Base1
let derived1 = base1 :?> Derived1
"""

[<Test>]
let ``optional arguments`` () =
    formatSourceString
        """
type Connection(?rate0 : int, ?duplex0 : DuplexType, ?parity0 : bool) =
    let duplex = defaultArg duplex0 Full
    let parity = defaultArg parity0 false
    let mutable rate = match rate0 with
                        | Some rate1 -> rate1
                        | None -> match duplex with
                                  | Full -> 9600
                                  | Half -> 4800
    do printfn "Baud Rate: %d Duplex: %A Parity: %b" rate duplex parity"""
        config
    |> prepend newline
    |> should
        equal
        """
type Connection(?rate0: int, ?duplex0: DuplexType, ?parity0: bool) =
    let duplex = defaultArg duplex0 Full
    let parity = defaultArg parity0 false

    let mutable rate =
        match rate0 with
        | Some rate1 -> rate1
        | None ->
            match duplex with
            | Full -> 9600
            | Half -> 4800

    do printfn "Baud Rate: %d Duplex: %A Parity: %b" rate duplex parity
"""

[<Test>]
let ``method params`` () =
    formatSourceString
        """
type Test() =
    member this.Function1<'a>(x, y) =
        printfn "%A, %A" x y

    abstract AbstractMethod<'a, 'b> : 'a * 'b -> unit
    override this.AbstractMethod<'a, 'b>(x:'a, y:'b) =
         printfn "%A, %A" x y"""
        { config with
            MaxFunctionBindingWidth = 120 }
    |> prepend newline
    |> should
        equal
        """
type Test() =
    member this.Function1<'a>(x, y) = printfn "%A, %A" x y

    abstract AbstractMethod<'a, 'b> : 'a * 'b -> unit
    override this.AbstractMethod<'a, 'b>(x: 'a, y: 'b) = printfn "%A, %A" x y
"""

[<Test>]
let ``params arguments`` () =
    formatSourceString
        """
type X() =
    member this.F([<ParamArray>] args: Object[]) =
        for arg in args do
            printfn "%A" arg"""
        config
    |> prepend newline
    |> should
        equal
        """
type X() =
    member this.F([<ParamArray>] args: Object[]) =
        for arg in args do
            printfn "%A" arg
"""

[<Test>]
let ``generic types`` () =
    formatSourceString
        """
type public MyClass<'a> public (x, y) as this =
    static let PI = 3.14
    static do printfn "static constructor"
    let mutable z = x + y
    do  printfn "%s" (this.ToString())
        printfn "more constructor effects"
    internal new (a) = MyClass(a,a)
    static member StaticProp = PI
    static member StaticMethod a = a + 1
    member internal self.Prop1 = x
    member self.Prop2 with get() = z
                      and set(a) = z <- a
    member self.Method(a,b) = x + y + z + a + b"""
        { config with
            MaxFunctionBindingWidth = 120 }
    |> prepend newline
    |> should
        equal
        """
type public MyClass<'a> public (x, y) as this =
    static let PI = 3.14
    static do printfn "static constructor"
    let mutable z = x + y

    do
        printfn "%s" (this.ToString())
        printfn "more constructor effects"

    internal new(a) = MyClass(a, a)
    static member StaticProp = PI
    static member StaticMethod a = a + 1
    member internal self.Prop1 = x

    member self.Prop2
        with get () = z
        and set (a) = z <- a

    member self.Method(a, b) = x + y + z + a + b
"""

[<Test>]
let ``struct declaration`` () =
    formatSourceString
        """
    type Point2D =
       struct
          val X: float
          val Y: float
          new(x: float, y: float) = { X = x; Y = y }
       end"""
        config
    |> prepend newline
    |> should
        equal
        """
type Point2D =
    struct
        val X: float
        val Y: float
        new(x: float, y: float) = { X = x; Y = y }
    end
"""

[<Test>]
let ``abstract and override keywords`` () =
    formatSourceString
        """
    type MyClassBase1() =
       let mutable z = 0
       abstract member Function1 : int -> int
       default u.Function1(a : int) = z <- z + a; z

    type MyClassDerived1() =
       inherit MyClassBase1()
       override u.Function1(a: int) = a + 1"""
        config
    |> prepend newline
    |> should
        equal
        """
type MyClassBase1() =
    let mutable z = 0
    abstract member Function1: int -> int

    default u.Function1(a: int) =
        z <- z + a
        z

type MyClassDerived1() =
    inherit MyClassBase1()
    override u.Function1(a: int) = a + 1
"""

[<Test>]
let ``intrinsic type extensions`` () =
    formatSourceString
        """
type MyClass() =
      member this.F() = 100

type MyClass with
    member this.G() = 200"""
        { config with
            NewlineBetweenTypeDefinitionAndMembers = false }
    |> prepend newline
    |> should
        equal
        """
type MyClass() =
    member this.F() = 100

type MyClass with
    member this.G() = 200
"""

[<Test>]
let ``optional type extensions`` () =
    formatSourceString
        """
/// Define a new member method FromString on the type Int32.
type System.Int32 with
    member this.FromString( s : string ) =
       System.Int32.Parse(s)"""
        { config with
            MaxFunctionBindingWidth = 120
            NewlineBetweenTypeDefinitionAndMembers = false }
    |> prepend newline
    |> should
        equal
        """
/// Define a new member method FromString on the type Int32.
type System.Int32 with
    member this.FromString(s: string) = System.Int32.Parse(s)
"""

[<Test>]
let ``auto property`` () =
    formatSourceString
        """
type MyClass(property1 : int) =
    member val Property1 = property1
    member val Property2 = "" with get, set"""
        config
    |> prepend newline
    |> should
        equal
        """
type MyClass(property1: int) =
    member val Property1 = property1
    member val Property2 = "" with get, set
"""

[<Test>]
let ``property handling`` () =
    formatSourceString
        """
type Derived1() =
   inherit AbstractBase()
   let mutable value = 10
   override this.Property1 with get() = value and set(v : int) = value <- v"""
        config
    |> prepend newline
    |> should
        equal
        """
type Derived1() =
    inherit AbstractBase()
    let mutable value = 10

    override this.Property1
        with get () = value
        and set (v: int) = value <- v
"""

[<Test>]
let ``access modifiers on properties`` () =
    formatSourceString
        """
type Foo() =
    member x.Get with get () = 1
    member x.Set with private set (v : int) = value <- v
    member x.GetSet with internal get () = value and private set (v : bool) = value <- v
    member x.GetI with internal get (key1, key2) = false
    member x.SetI with private set (key1, key2) value = ()
    member x.GetSetI with internal get (key1, key2) = true and private set (key1, key2) value = ()"""
        config
    |> prepend newline
    |> should
        equal
        """
type Foo() =
    member x.Get = 1

    member x.Set
        with private set (v: int) = value <- v

    member x.GetSet
        with internal get () = value
        and private set (v: bool) = value <- v

    member x.GetI
        with internal get (key1, key2) = false

    member x.SetI
        with private set (key1, key2) value = ()

    member x.GetSetI
        with internal get (key1, key2) = true
        and private set (key1, key2) value = ()
"""

[<Test>]
let ``types with attributes`` () =
    formatSourceString
        """
type MyType() =
    let mutable myInt1 = 10
    [<DefaultValue; Test>] val mutable myInt2 : int
    [<DefaultValue; Test>] val mutable myString : string
    member this.SetValsAndPrint( i: int, str: string) =
       myInt1 <- i
       this.myInt2 <- i + 1
       this.myString <- str
       printfn "%d %d %s" myInt1 (this.myInt2) (this.myString)"""
        config
    |> prepend newline
    |> should
        equal
        """
type MyType() =
    let mutable myInt1 = 10

    [<DefaultValue; Test>]
    val mutable myInt2: int

    [<DefaultValue; Test>]
    val mutable myString: string

    member this.SetValsAndPrint(i: int, str: string) =
        myInt1 <- i
        this.myInt2 <- i + 1
        this.myString <- str
        printfn "%d %d %s" myInt1 (this.myInt2) (this.myString)
"""

[<Test>]
let ``named arguments`` () =
    formatSourceString
        """
type SpeedingTicket() =
    member this.GetMPHOver(speed: int, limit: int) = speed - limit

let CalculateFine (ticket : SpeedingTicket) =
    let delta = ticket.GetMPHOver(limit = 55, speed = 70)
    if delta < 20 then 50.0 else 100.0"""
        { config with
            MaxValueBindingWidth = 120 }
    |> prepend newline
    |> should
        equal
        """
type SpeedingTicket() =
    member this.GetMPHOver(speed: int, limit: int) = speed - limit

let CalculateFine (ticket: SpeedingTicket) =
    let delta = ticket.GetMPHOver(limit = 55, speed = 70)
    if delta < 20 then 50.0 else 100.0
"""

[<Test>]
let ``separate-indexed-properties, 2129`` () =
    formatSourceString
        """
type Foo() =
    member this.Item
        with get (name: string): obj option = None

    member this.Item
        with set (name: string) (v: obj option): unit =
            ()"""
        config
    |> prepend newline
    |> should
        equal
        """
type Foo() =
    member this.Item
        with get (name: string): obj option = None

    member this.Item
        with set (name: string) (v: obj option): unit = ()
"""

[<Test>]
let ``indexed properties`` () =
    formatSourceString
        """
type NumberStrings() =
   let mutable ordinals = [| "one"; |]
   let mutable cardinals = [| "first"; |]
   member this.Item
      with get index = ordinals.[index]
      and set index value = ordinals.[index] <- value
   member this.Ordinal
      with get(index) = ordinals.[index]
      and set index value = ordinals.[index] <- value
   member this.Cardinal
      with get(index) = cardinals.[index]
      and set index value = cardinals.[index] <- value"""
        config
    |> prepend newline
    |> should
        equal
        """
type NumberStrings() =
    let mutable ordinals = [| "one" |]
    let mutable cardinals = [| "first" |]

    member this.Item
        with get index = ordinals.[index]
        and set index value = ordinals.[index] <- value

    member this.Ordinal
        with get (index) = ordinals.[index]
        and set index value = ordinals.[index] <- value

    member this.Cardinal
        with get (index) = cardinals.[index]
        and set index value = cardinals.[index] <- value
"""

[<Test>]
let ``complex indexed properties`` () =
    formatSourceString
        """
open System.Collections.Generic
type SparseMatrix() =
    let mutable table = new Dictionary<int * int, float>()
    member this.Item
        with get(key1, key2) = table.[(key1, key2)]
        and set (key1, key2) value = table.[(key1, key2)] <- value

let matrix1 = new SparseMatrix()
for i in 1..1000 do
    matrix1.[i, i] <- float i * float i
    """
        config
    |> prepend newline
    |> should
        equal
        """
open System.Collections.Generic

type SparseMatrix() =
    let mutable table = new Dictionary<int * int, float>()

    member this.Item
        with get (key1, key2) = table.[(key1, key2)]
        and set (key1, key2) value = table.[(key1, key2)] <- value

let matrix1 = new SparseMatrix()

for i in 1..1000 do
    matrix1.[i, i] <- float i * float i
"""

[<Test>]
let ``type constraints simple`` () =
    formatSourceString
        """
type Class1<'T when 'T :> System.Exception> =
    class end

type Class2<'T when 'T :> System.IComparable> =
    class end

type Class3<'T when 'T : null> =
    class end

type Class8<'T when 'T : not struct> =
   class end

type Class9<'T when 'T : enum<uint32>> =
   class end

type Class10<'T when 'T : comparison> =
   class end

type Class11<'T when 'T : equality> =
   class end

type Class12<'T when 'T : delegate<obj * System.EventArgs, unit>> =
   class end

type Class13<'T when 'T : unmanaged> =
   class end

type Class14<'T,'U when 'T : equality and 'U : equality> =
    class end"""
        config
    |> prepend newline
    |> should
        equal
        """
type Class1<'T when 'T :> System.Exception> = class end

type Class2<'T when 'T :> System.IComparable> = class end

type Class3<'T when 'T: null> = class end

type Class8<'T when 'T: not struct> = class end

type Class9<'T when 'T: enum<uint32>> = class end

type Class10<'T when 'T: comparison> = class end

type Class11<'T when 'T: equality> = class end

type Class12<'T when 'T: delegate<obj * System.EventArgs, unit>> = class end

type Class13<'T when 'T: unmanaged> = class end

type Class14<'T, 'U when 'T: equality and 'U: equality> = class end
"""

[<Test>]
let ``then blocks after constructors`` () =
    formatSourceString
        """
type Person(nameIn : string, idIn : int) =
    let mutable name = nameIn
    let mutable id = idIn
    do printfn "Created a person object."
    member this.Name with get() = name and set(v) = name <- v
    member this.ID with get() = id and set(v) = id <- v
    new() =
        Person("Invalid Name", -1)
        then printfn "Created an invalid person object."
            """
        config
    |> prepend newline
    |> should
        equal
        """
type Person(nameIn: string, idIn: int) =
    let mutable name = nameIn
    let mutable id = idIn
    do printfn "Created a person object."

    member this.Name
        with get () = name
        and set (v) = name <- v

    member this.ID
        with get () = id
        and set (v) = id <- v

    new() =
        Person("Invalid Name", -1)
        then printfn "Created an invalid person object."
"""

[<Test>]
let ``associativity of types`` () =
    formatSourceString
        """
type Delegate1 = delegate of (int * int) * (int * int) -> int
type Delegate2 = delegate of int * int -> int
type Delegate3 = delegate of int -> (int -> int)
type Delegate4 = delegate of int -> int -> int
type U = U of (int * int)
    """
        config
    |> prepend newline
    |> should
        equal
        """
type Delegate1 = delegate of (int * int) * (int * int) -> int
type Delegate2 = delegate of int * int -> int
type Delegate3 = delegate of int -> (int -> int)
type Delegate4 = delegate of int -> int -> int
type U = U of (int * int)
"""

[<Test>]
let ``very long delegate type alias, 1514`` () =
    formatSourceString
        """
type SomeWin32Callback = delegate of NastyWinApi32Type * int * int * int * NastyWinApi32Type * int * int * int * int * NastyWinApi32Type * int * int -> bool
    """
        { config with MaxLineLength = 60 }
    |> prepend newline
    |> should
        equal
        """
type SomeWin32Callback =
    delegate of
        NastyWinApi32Type *
        int *
        int *
        int *
        NastyWinApi32Type *
        int *
        int *
        int *
        int *
        NastyWinApi32Type *
        int *
        int ->
            bool
"""

[<Test>]
let ``very long delegate type alias wrapped in parens, 1514`` () =
    formatSourceString
        """
type SomeWin32Callback = delegate of (NastyWinApi32Type * int * int * int * NastyWinApi32Type * int * int * int * int * NastyWinApi32Type * int * int) -> bool
    """
        { config with MaxLineLength = 60 }
    |> prepend newline
    |> should
        equal
        """
type SomeWin32Callback =
    delegate of
        (NastyWinApi32Type *
        int *
        int *
        int *
        NastyWinApi32Type *
        int *
        int *
        int *
        int *
        NastyWinApi32Type *
        int *
        int) ->
            bool
"""

[<Test>]
let ``should keep the ? in optional parameters`` () =
    formatSourceString
        """type Shell() =
    static member private GetParams(cmd, ?args) = doStuff
    static member Exec(cmd, ?args) =
        shellExec(Shell.GetParams(cmd, ?args = args))

    """
        { config with
            MaxFunctionBindingWidth = 120 }
    |> should
        equal
        """type Shell() =
    static member private GetParams(cmd, ?args) = doStuff
    static member Exec(cmd, ?args) = shellExec (Shell.GetParams(cmd, ?args = args))
"""

[<Test>]
let ``should add space before argument on given config`` () =
    formatSourceString
        """
let f(x: int) = x

type t(x : int) =
    class
    end
    """
        { config with SpaceBeforeColon = true }
    |> prepend newline
    |> should
        equal
        """
let f (x : int) = x

type t(x : int) = class end
"""

[<Test>]
let ``should keep brackets around type signatures`` () =
    formatSourceString
        """
let user_printers = ref([] : (string * (term -> unit)) list)
let the_interface = ref([] : (string * (string * hol_type)) list)
    """
        { config with
            MaxValueBindingWidth = 50 }
    |> prepend newline
    |> should
        equal
        """
let user_printers =
    ref ([]: (string * (term -> unit)) list)

let the_interface =
    ref ([]: (string * (string * hol_type)) list)
"""

[<Test>]
let ``should print named patterns on explicit constructors`` () =
    formatSourceString
        """
type StateMachine(makeAsync) =
    new(fileName, makeAsync, initState) as secondCtor =
        new StateMachine(makeAsync)
        then
            secondCtor.Init(fileName, initState)
    """
        config
    |> prepend newline
    |> should
        equal
        """
type StateMachine(makeAsync) =
    new(fileName, makeAsync, initState) as secondCtor =
        new StateMachine(makeAsync)
        then secondCtor.Init(fileName, initState)
"""

[<Test>]
let ``should not misrecognize sequential expressions as a then block`` () =
    formatSourceString
        """
type BlobHelper(Account : CloudStorageAccount) =
    new(configurationSettingName, hostedService) =
        CloudStorageAccount.SetConfigurationSettingPublisher(fun configName configSettingPublisher ->
            let connectionString =
                if hostedService then RoleEnvironment.GetConfigurationSettingValue(configName)
                else ConfigurationManager.ConnectionStrings.[configName].ConnectionString
            configSettingPublisher.Invoke(connectionString) |> ignore)
        BlobHelper(CloudStorageAccount.FromConfigurationSetting(configurationSettingName))
    """
        { config with
            MaxInfixOperatorExpression = 40 }
    |> prepend newline
    |> should
        equal
        """
type BlobHelper(Account: CloudStorageAccount) =
    new(configurationSettingName, hostedService) =
        CloudStorageAccount.SetConfigurationSettingPublisher(fun configName configSettingPublisher ->
            let connectionString =
                if hostedService then
                    RoleEnvironment.GetConfigurationSettingValue(configName)
                else
                    ConfigurationManager.ConnectionStrings.[configName].ConnectionString

            configSettingPublisher.Invoke(connectionString)
            |> ignore)

        BlobHelper(CloudStorageAccount.FromConfigurationSetting(configurationSettingName))
"""

[<Test>]
let ``^a needs spaces when used as a type parameter`` () =
    formatSourceString
        """
let inline tryAverage(seq: seq< ^a >): ^a option =  None"""
        config
    |> prepend newline
    |> should
        equal
        """
let inline tryAverage (seq: seq< ^a >) : ^a option = None
"""

[<Test>]
let ``multiple hats need spaces`` () =
    formatSourceString
        """
let inline tryAverage(map: Map< ^a,^b>): ^a option =  None"""
        config
    |> prepend newline
    |> should
        equal
        """
let inline tryAverage (map: Map< ^a, ^b >) : ^a option = None
"""

[<Test>]
let ``should preserve orders on field declarations`` () =
    formatSourceString
        """
type CustomGraphControl() =
    inherit UserControl()
    [<DefaultValue(false)>]
    static val mutable private GraphProperty : DependencyProperty
    """
        config
    |> prepend newline
    |> should
        equal
        """
type CustomGraphControl() =
    inherit UserControl()

    [<DefaultValue(false)>]
    static val mutable private GraphProperty: DependencyProperty
"""

[<Test>]
let ``should preserve orders on field declarations - multiple spaces between attribute args`` () =
    formatSourceString
        """
type CustomGraphControl() =
    inherit UserControl()
    [<DefaultValue      (false)>]
    static val mutable private GraphProperty : DependencyProperty
    """
        config
    |> prepend newline
    |> should
        equal
        """
type CustomGraphControl() =
    inherit UserControl()

    [<DefaultValue(false)>]
    static val mutable private GraphProperty: DependencyProperty
"""

[<Test>]
let ``should preserve orders on field declarations - attribute without parentheses`` () =
    formatSourceString
        """
type CustomGraphControl() =
    inherit UserControl()
    [<DefaultValue false>]
    static val mutable private GraphProperty : DependencyProperty
    """
        config
    |> prepend newline
    |> should
        equal
        """
type CustomGraphControl() =
    inherit UserControl()

    [<DefaultValue false>]
    static val mutable private GraphProperty: DependencyProperty
"""

[<Test>]
let ``should preserve orders on field declarations - attribute without parentheses and multiple spaces between attribute args``
    ()
    =
    formatSourceString
        """
type CustomGraphControl() =
    inherit UserControl()
    [<DefaultValue       false>]
    static val mutable private GraphProperty : DependencyProperty
    """
        config
    |> prepend newline
    |> should
        equal
        """
type CustomGraphControl() =
    inherit UserControl()

    [<DefaultValue false>]
    static val mutable private GraphProperty: DependencyProperty
"""

[<Test>]
let ``should indent properly on getters and setters`` () =
    formatSourceString
        """
type A() =
    override this.Address with set v =
        let x =
             match _kbytes.GetAddress(8) with
             | Some(x) -> x
             | None -> null
        ignore x"""
        config
    |> prepend newline
    |> should
        equal
        """
type A() =
    override this.Address
        with set v =
            let x =
                match _kbytes.GetAddress(8) with
                | Some(x) -> x
                | None -> null

            ignore x
"""

[<Test>]
let ``should go to new lines on long property bodies`` () =
    formatSourceString
        """
type A() =
    member x.B with set v = "[<System.Runtime.InteropServices.DllImport(\"user32.dll\")>] extern int GetWindowLong(System.IntPtr hwnd, int index)"
                            |> ignore"""
        config
    |> prepend newline
    |> should
        equal
        """
type A() =
    member x.B
        with set v =
            "[<System.Runtime.InteropServices.DllImport(\"user32.dll\")>] extern int GetWindowLong(System.IntPtr hwnd, int index)"
            |> ignore
"""

[<Test>]
let ``should not remove identifier on getter ... except '()'`` () =
    formatSourceString
        """
type Bar =
    member this.Item
        with get(i : int) =
            match mo with
            | Some(m) when m.Groups.[i].Success -> m.Groups.[i].Value
            | _ -> null

    member this.Item
        with get(i : string) =
            match mo with
            | Some (m) when m.Groups.[i].Success -> m.Groups.[i].Value
            | _ -> null"""
        config
    |> prepend newline
    |> should
        equal
        """
type Bar =
    member this.Item
        with get (i: int) =
            match mo with
            | Some(m) when m.Groups.[i].Success -> m.Groups.[i].Value
            | _ -> null

    member this.Item
        with get (i: string) =
            match mo with
            | Some(m) when m.Groups.[i].Success -> m.Groups.[i].Value
            | _ -> null
"""

[<Test>]
let ``should not add dubious new line inside call chains`` () =
    formatSourceString
        """
let x =
    JobCollectionCreateParameters
        (Label = "Test",
         IntrinsicSettings = JobCollectionIntrinsicSettings
                                 (Plan = JobCollectionPlan.Standard,
                                  Quota = new JobCollectionQuota(MaxJobCount = Nullable(50))))"""
        { config with MaxLineLength = 120 }
    |> prepend newline
    |> should
        equal
        """
let x =
    JobCollectionCreateParameters(
        Label = "Test",
        IntrinsicSettings =
            JobCollectionIntrinsicSettings(
                Plan = JobCollectionPlan.Standard,
                Quota = new JobCollectionQuota(MaxJobCount = Nullable(50))
            )
    )
"""

[<Test>]
let ``should preserve attributes on member parameters`` () =
    formatSourceString
        """
type ILogger =
    abstract DebugFormat : format:String * [<ParamArray>]args:Object[] -> unit"""
        config
    |> prepend newline
    |> should
        equal
        """
type ILogger =
    abstract DebugFormat: format: String * [<ParamArray>] args: Object[] -> unit
"""

[<Test>]
let ``should preserve brackets on type signatures`` () =
    formatSourceString
        """
type A =
    abstract member M : int -> (int -> unit)
    abstract member M : float -> int"""
        config
    |> prepend newline
    |> should
        equal
        """
type A =
    abstract member M: int -> (int -> unit)
    abstract member M: float -> int
"""

[<Test>]
let ``should preserve brackets on type signatures 2`` () =
    formatSourceString
        """
type A =
    abstract member M : (int -> int) -> unit
    abstract member M : float -> int"""
        config
    |> prepend newline
    |> should
        equal
        """
type A =
    abstract member M: (int -> int) -> unit
    abstract member M: float -> int
"""

[<Test>]
let ``should handle overridden auto properties`` () =
    formatSourceString
        """
type Entity() =
    abstract Id : int with get, set
    default val Id = 0 with get, set"""
        config
    |> prepend newline
    |> should
        equal
        """
type Entity() =
    abstract Id: int with get, set
    default val Id = 0 with get, set
"""

[<Test>]
let ``type abbreviation augmentation`` () =
    formatSourceString
        """type T2 = T2 with
    member __.X = ()
"""
        { config with
            NewlineBetweenTypeDefinitionAndMembers = false }
    |> should
        equal
        """type T2 = T2
    with
        member __.X = ()
"""

[<Test>]
let ``operator in words should not print to symbol, 409`` () =
    formatSourceString
        """type T() =
    static member op_LessThan(a, b) = a < b"""
        { config with
            SpaceBeforeMember = true
            MaxFunctionBindingWidth = 120 }
    |> should
        equal
        """type T() =
    static member op_LessThan (a, b) = a < b
"""

[<Test>]
let ``operator in words in let binding`` () =
    formatSourceString """let op_PipeRight2  = ()""" config
    |> should
        equal
        """let op_PipeRight2 = ()
"""

[<Test>]
let ``operator in words in member`` () =
    formatSourceString
        """type A() =
    member this.B(op_Inequality : string) = ()"""
        { config with
            MaxFunctionBindingWidth = 120 }
    |> should
        equal
        """type A() =
    member this.B(op_Inequality: string) = ()
"""

[<Test>]
let ``attributes on extension methods should not add newlines, 473`` () =
    formatSourceString
        """
[<Extension>]
type TestExtensions =

    [<Extension>]
    static member SomeExtension(x) = ""

    [<Extension>]
    static member SomeOtherExtension(x) = ""
"""
        { config with
            MaxValueBindingWidth = 120 }
    |> prepend newline
    |> should
        equal
        """
[<Extension>]
type TestExtensions =

    [<Extension>]
    static member SomeExtension(x) = ""

    [<Extension>]
    static member SomeOtherExtension(x) = ""
"""

[<Test>]
let ``F# 4.7 syntax relaxation in member declaration`` () =
    formatSourceString
        """
type C'() =
    member _.M() = ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
type C'() =
    member _.M() = ()
"""

[<Test>]
let ``don't add additional newlines between recursive type declarations, 520`` () =
    formatSourceString
        """module Game

type Details =
    { Name: string
      Description: string }

type Item =
    { Details: Details }

type Exit =
    | Passable of Details * desitnation: Room
    | Locked of Details * key: Item * next: Exit
    | NoExit of Details option

and Exits =
    { North: Exit
      South: Exit
      East: Exit
      West: Exit }

and Room =
    { Details: Details
      Items: Item list
      Exits: Exits }
"""
        config
    |> prepend newline
    |> should
        equal
        """
module Game

type Details = { Name: string; Description: string }

type Item = { Details: Details }

type Exit =
    | Passable of Details * desitnation: Room
    | Locked of Details * key: Item * next: Exit
    | NoExit of Details option

and Exits =
    { North: Exit
      South: Exit
      East: Exit
      West: Exit }

and Room =
    { Details: Details
      Items: Item list
      Exits: Exits }
"""

[<Test>]
let ``don't add additional newlines between recursive type declarations with attributes, 520`` () =
    formatSourceString
        """module Game

type Exit =
    | Passable of Details * desitnation: Room
    | Locked of Details * key: Item * next: Exit
    | NoExit of Details option

and Exits =
    { North: Exit
      South: Exit
      East: Exit
      West: Exit }

and [<Marker()>] Room =
    { Details: Details
      Items: Item list
      Exits: Exits }
"""
        config
    |> prepend newline
    |> should
        equal
        """
module Game

type Exit =
    | Passable of Details * desitnation: Room
    | Locked of Details * key: Item * next: Exit
    | NoExit of Details option

and Exits =
    { North: Exit
      South: Exit
      East: Exit
      West: Exit }

and [<Marker>] Room =
    { Details: Details
      Items: Item list
      Exits: Exits }
"""

[<Test>]
let ``trivia newlines between letbinding of type, 709`` () =
    formatSourceString
        """
open Xunit
open FSharp.Core
open Swensen.Unquote

type FormattingSpecs() =

    [<Fact>]
    let ``true is true``() = test <@ true = true @>

    [<Fact>]
    let ``false is false``() = test <@ false = false @>
"""
        config
    |> prepend newline
    |> should
        equal
        """
open Xunit
open FSharp.Core
open Swensen.Unquote

type FormattingSpecs() =

    [<Fact>]
    let ``true is true`` () = test <@ true = true @>

    [<Fact>]
    let ``false is false`` () = test <@ false = false @>
"""

[<Test>]
let ``line comment above single line abstract slot should not make it multiline, 757`` () =
    formatSourceString
        """[<AllowNullLiteral>]
type Graph2dOptions =
    abstract zoomMin: float option with get, set
    // abstract moment: MomentConstructor option with get, set
    abstract maxHeight: HeightWidthType option with get, set
    abstract zIndex: float option with get, set
"""
        config
    |> prepend newline
    |> should
        equal
        """
[<AllowNullLiteral>]
type Graph2dOptions =
    abstract zoomMin: float option with get, set
    // abstract moment: MomentConstructor option with get, set
    abstract maxHeight: HeightWidthType option with get, set
    abstract zIndex: float option with get, set
"""

[<Test>]
let ``long type members should have parameters on separate lines, 719`` () =
    formatSourceString
        """type C () =
    member __.LongMethodWithLotsOfParameters(aVeryLongType: AVeryLongTypeThatYouNeedToUse, aSecondVeryLongType: AVeryLongTypeThatYouNeedToUse, aThirdVeryLongType: AVeryLongTypeThatYouNeedToUse) =  aVeryLongType aSecondVeryLongType aThirdVeryLongType
"""
        { config with
            SpaceBeforeClassConstructor = true }
    |> prepend newline
    |> should
        equal
        """
type C () =
    member __.LongMethodWithLotsOfParameters
        (
            aVeryLongType: AVeryLongTypeThatYouNeedToUse,
            aSecondVeryLongType: AVeryLongTypeThatYouNeedToUse,
            aThirdVeryLongType: AVeryLongTypeThatYouNeedToUse
        ) =
        aVeryLongType aSecondVeryLongType aThirdVeryLongType
"""

[<Test>]
let ``long type member with return type should have parameters on separate lines`` () =
    formatSourceString
        """type C () =
    member __.LongMethodWithLotsOfParameters(aVeryLongType: AVeryLongTypeThatYouNeedToUse, aSecondVeryLongType: AVeryLongTypeThatYouNeedToUse, aThirdVeryLongType: AVeryLongTypeThatYouNeedToUse) : int =  aVeryLongType aSecondVeryLongType aThirdVeryLongType
"""
        { config with
            SpaceBeforeClassConstructor = true }
    |> prepend newline
    |> should
        equal
        """
type C () =
    member __.LongMethodWithLotsOfParameters
        (
            aVeryLongType: AVeryLongTypeThatYouNeedToUse,
            aSecondVeryLongType: AVeryLongTypeThatYouNeedToUse,
            aThirdVeryLongType: AVeryLongTypeThatYouNeedToUse
        ) : int =
        aVeryLongType aSecondVeryLongType aThirdVeryLongType
"""

[<Test>]
let ``long constructors should have parameters on separate lines`` () =
    formatSourceString
        """type C (aVeryLongType : AVeryLongTypeThatYouNeedToUse, aSecondVeryLongType : AVeryLongTypeThatYouNeedToUse, aThirdVeryLongType : AVeryLongTypeThatYouNeedToUse) =
    member this.X = 42
"""
        { config with
            SpaceBeforeClassConstructor = true }
    |> prepend newline
    |> should
        equal
        """
type C
    (
        aVeryLongType: AVeryLongTypeThatYouNeedToUse,
        aSecondVeryLongType: AVeryLongTypeThatYouNeedToUse,
        aThirdVeryLongType: AVeryLongTypeThatYouNeedToUse
    ) =
    member this.X = 42
"""

[<Test>]
let ``preserve abstract keyword`` () =
    formatSourceString
        """namespace Foo

type internal Blah =
  abstract Baz : unit
"""
        config
    |> prepend newline
    |> should
        equal
        """
namespace Foo

type internal Blah =
    abstract Baz: unit
"""

[<Test>]
let ``keep correct indentation after multiline member definition, 845`` () =
    formatSourceString
        """type SomeType() =
    member SomeMember(looooooooooooooooooooooooooooooooooong1: A, looooooooooooooooooooooooooooooooooong2: A) =
        printfn "a"
        "a"

    member SomeOtherMember () =
        printfn "b"
"""
        { config with
            MaxLineLength = 80
            MaxFunctionBindingWidth = 120 }
    |> prepend newline
    |> should
        equal
        """
type SomeType() =
    member SomeMember
        (
            looooooooooooooooooooooooooooooooooong1: A,
            looooooooooooooooooooooooooooooooooong2: A
        ) =
        printfn "a"
        "a"

    member SomeOtherMember() = printfn "b"
"""

[<Test>]
let ``keep correct indentation after multiline typed member definition`` () =
    formatSourceString
        """type SomeType() =
    member SomeMember(looooooooooooooooooooooooooooooooooong1: A, looooooooooooooooooooooooooooooooooong2: A) : string =
        printfn "a"
        "a"

    member SomeOtherMember () =
        printfn "b"
"""
        { config with
            MaxLineLength = 80
            MaxFunctionBindingWidth = 120 }
    |> prepend newline
    |> should
        equal
        """
type SomeType() =
    member SomeMember
        (
            looooooooooooooooooooooooooooooooooong1: A,
            looooooooooooooooooooooooooooooooooong2: A
        ) : string =
        printfn "a"
        "a"

    member SomeOtherMember() = printfn "b"
"""

[<Test>]
let ``split multiple parameters over multiple lines`` () =
    formatSourceString
        """
type SomeType =
    static member SomeMember (looooooooooooooooooooooooooooooooooooooooooooooooooooooooooooong1: string) (looooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooong2: string) : string =
     printfn "a"
     "b"
"""
        config
    |> prepend newline
    |> should
        equal
        """
type SomeType =
    static member SomeMember
        (looooooooooooooooooooooooooooooooooooooooooooooooooooooooooooong1: string)
        (looooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooong2: string)
        : string =
        printfn "a"
        "b"
"""

[<Test>]
let ``split multiple parameters over multiple lines and have correct indentation afterwards`` () =
    formatSourceString
        """
type SomeType =
    static member SomeMember (looooooooooooooooooooooooooooooooooooooooooooooooooooooooooooong1: string) (looooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooong2: string) : string =
     printfn "a"
     "b"

    static member SomeOtherMember () = printfn "c"
"""
        { config with
            MaxFunctionBindingWidth = 120 }
    |> prepend newline
    |> should
        equal
        """
type SomeType =
    static member SomeMember
        (looooooooooooooooooooooooooooooooooooooooooooooooooooooooooooong1: string)
        (looooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooong2: string)
        : string =
        printfn "a"
        "b"

    static member SomeOtherMember() = printfn "c"
"""

[<Test>]
let ``member with one long parameter and return type, 850`` () =
    formatSourceString
        """
type SomeType =
    static member SomeMember loooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooong1 : string =
     printfn "a"
     "b"
"""
        config
    |> prepend newline
    |> should
        equal
        """
type SomeType =
    static member SomeMember
        loooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooong1
        : string =
        printfn "a"
        "b"
"""

[<Test>]
let ``member with one long parameter and no return type, 850`` () =
    formatSourceString
        """
type SomeType =
    static member SomeMember loooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooong1 =
     printfn "a"
     "b"
"""
        config
    |> prepend newline
    |> should
        equal
        """
type SomeType =
    static member SomeMember
        loooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooong1
        =
        printfn "a"
        "b"
"""

[<Test>]
let ``multiple members with one long parameter`` () =
    formatSourceString
        """
type SomeType =
    static member SomeMember loooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooong1 =
     printfn "a"
     "b"

    static member Serialize (loooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooong2: SomeType) = Encode.string v.Meh
    static member Deserialize (loooooooooooooooooooooooooooooooooooooooooooooooooooooonnnnnnnnnnnnnnnnnnnnngggggggggggJsonVaaaaalueeeeeeeeeeeeeeee) : SomeType = Decode.SomeType loooooooooooooooooooooooooooooooooooooooooooooooooooooonnnnnnnnnnnnnnnnnnnnngggggggggggJsonVaaaaalueeeeeeeeeeeeeeee
"""
        config
    |> prepend newline
    |> should
        equal
        """
type SomeType =
    static member SomeMember
        loooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooong1
        =
        printfn "a"
        "b"

    static member Serialize
        (loooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooong2:
            SomeType)
        =
        Encode.string v.Meh

    static member Deserialize
        (loooooooooooooooooooooooooooooooooooooooooooooooooooooonnnnnnnnnnnnnnnnnnnnngggggggggggJsonVaaaaalueeeeeeeeeeeeeeee)
        : SomeType =
        Decode.SomeType
            loooooooooooooooooooooooooooooooooooooooooooooooooooooonnnnnnnnnnnnnnnnnnnnngggggggggggJsonVaaaaalueeeeeeeeeeeeeeee
"""

[<Test>]
let ``access modifier before long constructor`` () =
    formatSourceString
        """
type INotifications<'a,'b,'c,'d,'e> =
    class
    end
type DeviceNotificationHandler<'Notification, 'CallbackId, 'RegisterInputData, 'RegisterOutputData, 'UnregisterOutputData> private (client: INotifications<'Notification, 'CallbackId, 'RegisterInputData, 'RegisterOutputData, 'UnregisterOutputData>, callbackId: 'CallbackId, validateUnregisterOutputData: 'UnregisterOutputData -> unit) =
    let a = 5
"""
        config
    |> prepend newline
    |> should
        equal
        """
type INotifications<'a, 'b, 'c, 'd, 'e> = class end

type DeviceNotificationHandler<'Notification, 'CallbackId, 'RegisterInputData, 'RegisterOutputData, 'UnregisterOutputData>
    private
    (
        client:
            INotifications<'Notification, 'CallbackId, 'RegisterInputData, 'RegisterOutputData, 'UnregisterOutputData>,
        callbackId: 'CallbackId,
        validateUnregisterOutputData: 'UnregisterOutputData -> unit
    ) =
    let a = 5
"""

[<Test>]
let ``long type members should be in multiple lines, 868`` () =
    formatSourceString
        """
type C() =
    member _.LongMethodWithLotsOfParameters(aVeryLongType: int, aSecondVeryLongType: int, aThirdVeryLongType: int) : int =
        aVeryLongType + aSecondVeryLongType + aThirdVeryLongType
"""
        { config with
            MaxLineLength = 80
            SpaceBeforeColon = true
            MaxInfixOperatorExpression = 80 }
    |> prepend newline
    |> should
        equal
        """
type C() =
    member _.LongMethodWithLotsOfParameters
        (
            aVeryLongType : int,
            aSecondVeryLongType : int,
            aThirdVeryLongType : int
        ) : int =
        aVeryLongType + aSecondVeryLongType + aThirdVeryLongType
"""

[<Test>]
let ``long type members should be in multiple lines, no return type`` () =
    formatSourceString
        """
type C() =
    member _.LongMethodWithLotsOfParameters(aVeryLongType: int, aSecondVeryLongType: int, aThirdVeryLongType: int) =
        aVeryLongType + aSecondVeryLongType + aThirdVeryLongType
"""
        { config with
            MaxLineLength = 80
            SpaceBeforeColon = true
            MaxInfixOperatorExpression = 80 }
    |> prepend newline
    |> should
        equal
        """
type C() =
    member _.LongMethodWithLotsOfParameters
        (
            aVeryLongType : int,
            aSecondVeryLongType : int,
            aThirdVeryLongType : int
        ) =
        aVeryLongType + aSecondVeryLongType + aThirdVeryLongType
"""

[<Test>]
let ``long type constructors should be in multiple lines, 868`` () =
    formatSourceString
        """
type VersionMismatchDuringDeserializationException(message: string, innerException: System.Exception) =
    inherit System.Exception(message, innerException)
"""
        { config with
            MaxLineLength = 55
            SpaceBeforeColon = true }
    |> prepend newline
    |> should
        equal
        """
type VersionMismatchDuringDeserializationException
    (
        message : string,
        innerException : System.Exception
    ) =
    inherit System.Exception(message, innerException)
"""

[<Test>]
let ``tuple typed abbreviation`` () =
    formatSourceString
        """type A = (int * int)
"""
        config
    |> prepend newline
    |> should
        equal
        """
type A = (int * int)
"""

[<Test>]
let ``function signature type abbreviation`` () =
    formatSourceString
        """type A = (int -> int -> int)
"""
        config
    |> prepend newline
    |> should
        equal
        """
type A = (int -> int -> int)
"""

let ``type record declaration with attributes, 910`` () =
    formatSourceString
        """type Commenter =
    { [<JsonProperty("display_name")>]
      DisplayName: string }

type Message =
    { [<JsonProperty("body")>]
      Body: string }
"""
        config
    |> prepend newline
    |> should
        equal
        """
type Commenter =
    { [<JsonProperty("display_name")>]
      DisplayName: string }

type Message =
    { [<JsonProperty("body")>]
      Body: string }
"""

[<Test>]
let ``attribute on abstract member followed by type with attribute, 933`` () =
    formatSourceString
        """
[<AllowNullLiteral>]
type SubGroupStackOptions =
    [<Emit "$0[$1]{{=$2}}">]
    abstract Item: name:string -> bool with get, set

[<AllowNullLiteral>]
type DataGroup =
    abstract className: string option with get, set
"""
        config
    |> prepend newline
    |> should
        equal
        """
[<AllowNullLiteral>]
type SubGroupStackOptions =
    [<Emit "$0[$1]{{=$2}}">]
    abstract Item: name: string -> bool with get, set

[<AllowNullLiteral>]
type DataGroup =
    abstract className: string option with get, set
"""

[<Test>]
let ``attribute on abstract member followed by let binding with attribute`` () =
    formatSourceString
        """
[<AllowNullLiteral>]
type SubGroupStackOptions =
    [<Emit "$0[$1]{{=$2}}">]
    abstract Item: name:string -> bool with get, set

[<AllowNullLiteral>]
let foo bar = zero
"""
        config
    |> prepend newline
    |> should
        equal
        """
[<AllowNullLiteral>]
type SubGroupStackOptions =
    [<Emit "$0[$1]{{=$2}}">]
    abstract Item: name: string -> bool with get, set

[<AllowNullLiteral>]
let foo bar = zero
"""

[<Test>]
let ``type constraint on type definition, 887`` () =
    formatSourceString
        """
type OuterType =
    abstract Apply<'r>
        : InnerType<'r>
        -> 'r when 'r : comparison
"""
        { config with SpaceBeforeColon = true }
    |> prepend newline
    |> should
        equal
        """
type OuterType =
    abstract Apply<'r> : InnerType<'r> -> 'r when 'r : comparison
"""

[<Test>]
let ``attribute on type and abstract member followed by type, 949`` () =
    formatSourceString
        """
[<AllowNullLiteral>]
type TimelineOptionsGroupCallbackFunction =
    [<Emit "$0($1...)">]
    abstract Invoke: group:TimelineGroup * callback:(TimelineGroup option -> unit) -> unit

type TimelineOptionsGroupEditableType = U2<bool, TimelineGroupEditableOption>
"""
        config
    |> prepend newline
    |> should
        equal
        """
[<AllowNullLiteral>]
type TimelineOptionsGroupCallbackFunction =
    [<Emit "$0($1...)">]
    abstract Invoke: group: TimelineGroup * callback: (TimelineGroup option -> unit) -> unit

type TimelineOptionsGroupEditableType = U2<bool, TimelineGroupEditableOption>
"""

[<Test>]
let ``attribute on type and abstract member followed by let binding`` () =
    formatSourceString
        """
[<AllowNullLiteral>]
type TimelineOptionsGroupCallbackFunction =
    [<Emit "$0($1...)">]
    abstract Invoke: group:TimelineGroup * callback:(TimelineGroup option -> unit) -> unit

let myBinding a = 7
"""
        config
    |> prepend newline
    |> should
        equal
        """
[<AllowNullLiteral>]
type TimelineOptionsGroupCallbackFunction =
    [<Emit "$0($1...)">]
    abstract Invoke: group: TimelineGroup * callback: (TimelineGroup option -> unit) -> unit

let myBinding a = 7
"""

[<Test>]
let ``comments before access modifier, 885`` () =
    formatSourceString
        """
type TestType =
    // Here is some comment about the type
    // Some more comments
    private
        {
            Foo : int
        }
"""
        config
    |> prepend newline
    |> should
        equal
        """
type TestType =
    // Here is some comment about the type
    // Some more comments
    private
        { Foo: int }
"""

[<Test>]
let ``comments before access modifier and multiline record type`` () =
    formatSourceString
        """
type OlapCube =
    // Here is some comment about the type
    // Some more comments
    private
        {
            OneDimension : int
            TwoDimension : int
            ThreeDimension : int
        }
"""
        config
    |> prepend newline
    |> should
        equal
        """
type OlapCube =
    // Here is some comment about the type
    // Some more comments
    private
        { OneDimension: int
          TwoDimension: int
          ThreeDimension: int }
"""

[<Test>]
let ``alternative long member definition`` () =
    formatSourceString
        """
type C () =
    member __.LongMethodWithLotsOfParameters(aVeryLongType : AVeryLongTypeThatYouNeedToUse, aSecondVeryLongType : AVeryLongTypeThatYouNeedToUse,aThirdVeryLongType : AVeryLongTypeThatYouNeedToUse) =
        someImplementation aVeryLongType aSecondVeryLongType aThirdVeryLongType
"""
        { config with
            SpaceBeforeClassConstructor = true
            SpaceBeforeColon = true
            AlternativeLongMemberDefinitions = true }
    |> prepend newline
    |> should
        equal
        """
type C () =
    member __.LongMethodWithLotsOfParameters
        (
            aVeryLongType : AVeryLongTypeThatYouNeedToUse,
            aSecondVeryLongType : AVeryLongTypeThatYouNeedToUse,
            aThirdVeryLongType : AVeryLongTypeThatYouNeedToUse
        )
        =
        someImplementation aVeryLongType aSecondVeryLongType aThirdVeryLongType
"""

[<Test>]
let ``alternative long member definition with return type`` () =
    formatSourceString
        """
type C () =
    member __.LongMethodWithLotsOfParameters(aVeryLongType : AVeryLongTypeThatYouNeedToUse, aSecondVeryLongType : AVeryLongTypeThatYouNeedToUse,aThirdVeryLongType : AVeryLongTypeThatYouNeedToUse) : int =
        someImplementation aVeryLongType aSecondVeryLongType aThirdVeryLongType
"""
        { config with
            SpaceBeforeClassConstructor = true
            SpaceBeforeColon = true
            AlternativeLongMemberDefinitions = true }
    |> prepend newline
    |> should
        equal
        """
type C () =
    member __.LongMethodWithLotsOfParameters
        (
            aVeryLongType : AVeryLongTypeThatYouNeedToUse,
            aSecondVeryLongType : AVeryLongTypeThatYouNeedToUse,
            aThirdVeryLongType : AVeryLongTypeThatYouNeedToUse
        )
        : int
        =
        someImplementation aVeryLongType aSecondVeryLongType aThirdVeryLongType
"""

// See https://github.com/dotnet/docs/issues/18806#issuecomment-655281219

[<Test>]
let ``member, tuple (non-curried), with return type:`` () =
    formatSourceString
        """
type MyClass() =
    member _.LongMethodWithLotsOfParameters(aVeryLongType: AVeryLongTypeThatYouNeedToUse, aSecondVeryLongType: AVeryLongTypeThatYouNeedToUse, aThirdVeryLongType: AVeryLongTypeThatYouNeedToUse) : AVeryLongReturnType =
        someFunction aVeryLongType aSecondVeryLongType aThirdVeryLongType
"""
        config
    |> prepend newline
    |> should
        equal
        """
type MyClass() =
    member _.LongMethodWithLotsOfParameters
        (
            aVeryLongType: AVeryLongTypeThatYouNeedToUse,
            aSecondVeryLongType: AVeryLongTypeThatYouNeedToUse,
            aThirdVeryLongType: AVeryLongTypeThatYouNeedToUse
        ) : AVeryLongReturnType =
        someFunction aVeryLongType aSecondVeryLongType aThirdVeryLongType
"""

[<Test>]
let ``member, tuple (non-curried), with no return type:`` () =
    formatSourceString
        """
type MyClass() =
    member _.LongMethodWithLotsOfParameters(aVeryLongType: AVeryLongTypeThatYouNeedToUse, aSecondVeryLongType: AVeryLongTypeThatYouNeedToUse, aThirdVeryLongType: AVeryLongTypeThatYouNeedToUse) =
        someFunction aVeryLongType aSecondVeryLongType aThirdVeryLongType
"""
        config
    |> prepend newline
    |> should
        equal
        """
type MyClass() =
    member _.LongMethodWithLotsOfParameters
        (
            aVeryLongType: AVeryLongTypeThatYouNeedToUse,
            aSecondVeryLongType: AVeryLongTypeThatYouNeedToUse,
            aThirdVeryLongType: AVeryLongTypeThatYouNeedToUse
        ) =
        someFunction aVeryLongType aSecondVeryLongType aThirdVeryLongType
"""

[<Test>]
let ``member, curried (non-tuple), with return type:`` () =
    formatSourceString
        """
type MyClass() =
    member _.LongMethodWithLotsOfParameters(aVeryLongType: AVeryLongTypeThatYouNeedToUse) (aSecondVeryLongType: AVeryLongTypeThatYouNeedToUse) (aThirdVeryLongType: AVeryLongTypeThatYouNeedToUse) : AVeryLongReturnType =
        someFunction aVeryLongType aSecondVeryLongType aThirdVeryLongType
"""
        config
    |> prepend newline
    |> should
        equal
        """
type MyClass() =
    member _.LongMethodWithLotsOfParameters
        (aVeryLongType: AVeryLongTypeThatYouNeedToUse)
        (aSecondVeryLongType: AVeryLongTypeThatYouNeedToUse)
        (aThirdVeryLongType: AVeryLongTypeThatYouNeedToUse)
        : AVeryLongReturnType =
        someFunction aVeryLongType aSecondVeryLongType aThirdVeryLongType
"""

[<Test>]
let ``member, curried (non-tuple), with no return type:`` () =
    formatSourceString
        """
type MyClass() =
    member _.LongMethodWithLotsOfParameters(aVeryLongType: AVeryLongTypeThatYouNeedToUse) (aSecondVeryLongType: AVeryLongTypeThatYouNeedToUse) (aThirdVeryLongType: AVeryLongTypeThatYouNeedToUse) =
        someFunction aVeryLongType aSecondVeryLongType aThirdVeryLongType
"""
        config
    |> prepend newline
    |> should
        equal
        """
type MyClass() =
    member _.LongMethodWithLotsOfParameters
        (aVeryLongType: AVeryLongTypeThatYouNeedToUse)
        (aSecondVeryLongType: AVeryLongTypeThatYouNeedToUse)
        (aThirdVeryLongType: AVeryLongTypeThatYouNeedToUse)
        =
        someFunction aVeryLongType aSecondVeryLongType aThirdVeryLongType
"""

[<Test>]
let ``alternative long class constructor`` () =
    formatSourceString
        """
type C(aVeryLongType: AVeryLongTypeThatYouNeedToUse,
       aSecondVeryLongType: AVeryLongTypeThatYouNeedToUse,
       aThirdVeryLongType: AVeryLongTypeThatYouNeedToUse) =
    class
    end
"""
        { config with
            AlternativeLongMemberDefinitions = true
            SpaceBeforeColon = true }
    |> prepend newline
    |> should
        equal
        """
type C
    (
        aVeryLongType : AVeryLongTypeThatYouNeedToUse,
        aSecondVeryLongType : AVeryLongTypeThatYouNeedToUse,
        aThirdVeryLongType : AVeryLongTypeThatYouNeedToUse
    )
    = class end
"""

[<Test>]
let ``alternative long class constructor with access modifier`` () =
    formatSourceString
        """
type C internal (aVeryLongType: AVeryLongTypeThatYouNeedToUse,
       aSecondVeryLongType: AVeryLongTypeThatYouNeedToUse,
       aThirdVeryLongType: AVeryLongTypeThatYouNeedToUse) =
    class
    end
"""
        { config with
            AlternativeLongMemberDefinitions = true
            SpaceBeforeColon = true }
    |> prepend newline
    |> should
        equal
        """
type C
    internal
    (
        aVeryLongType : AVeryLongTypeThatYouNeedToUse,
        aSecondVeryLongType : AVeryLongTypeThatYouNeedToUse,
        aThirdVeryLongType : AVeryLongTypeThatYouNeedToUse
    )
    = class end
"""

[<Test>]
let ``trivia before properties, 1009`` () =
    formatSourceString
        """
type Box() =
    let mutable color : string = null

    // A Box has a color property with get and set.
    member x.Color
        with get() = color
        and set(c) = color <- c

    member x.Color2
        // A Box has a color property with get and set.
        with get() = color
        and set(c) = color <- c

    // If there's no get/set, the comment is preserved
    member x.hello = "world"
"""
        config
    |> prepend newline
    |> should
        equal
        """
type Box() =
    let mutable color: string = null

    // A Box has a color property with get and set.
    member x.Color
        with get () = color
        and set (c) = color <- c

    member x.Color2
        // A Box has a color property with get and set.
        with get () = color
        and set (c) = color <- c

    // If there's no get/set, the comment is preserved
    member x.hello = "world"
"""

[<Test>]
let ``don't add additional newline before record instance return value`` () =
    formatSourceString
        """
type Auth0User =
    { UserId : string
      AppMetaData : AppMetaData }

    static member Decoder : Decoder<Auth0User> =
        Decode.object (fun get ->
            let userId =
                get.Required.Field "user_id" Decode.string

            let metaData =
                get.Optional.Field "app_metadata" AppMetaData.Decoder
                |> Option.defaultValue ({ PushNotificationSubscriptions = [] })

            { UserId = userId
              AppMetaData = metaData })
"""
        { config with SpaceBeforeColon = true }
    |> prepend newline
    |> should
        equal
        """
type Auth0User =
    { UserId : string
      AppMetaData : AppMetaData }

    static member Decoder : Decoder<Auth0User> =
        Decode.object (fun get ->
            let userId = get.Required.Field "user_id" Decode.string

            let metaData =
                get.Optional.Field "app_metadata" AppMetaData.Decoder
                |> Option.defaultValue ({ PushNotificationSubscriptions = [] })

            { UserId = userId
              AppMetaData = metaData })
"""

[<Test>]
let ``generic recursive types`` () =
    formatSourceString
        """
type ViewBinding<'model,'msg> = string * Variable<'model,'msg>
and ViewBindings<'model,'msg> = ViewBinding<'model,'msg> list
and Variable<'model,'msg> =
    | Bind of Getter<'model>
    | BindTwoWay of Getter<'model> * Setter<'model,'msg>
    | BindTwoWayValidation of Getter<'model> * ValidSetter<'model,'msg>
    | BindCmd of Execute<'model,'msg> * CanExecute<'model>
    | BindModel of Getter<'model> * ViewBindings<'model,'msg>
    | BindMap of Getter<'model> * (obj -> obj)
"""
        config
    |> prepend newline
    |> should
        equal
        """
type ViewBinding<'model, 'msg> = string * Variable<'model, 'msg>
and ViewBindings<'model, 'msg> = ViewBinding<'model, 'msg> list

and Variable<'model, 'msg> =
    | Bind of Getter<'model>
    | BindTwoWay of Getter<'model> * Setter<'model, 'msg>
    | BindTwoWayValidation of Getter<'model> * ValidSetter<'model, 'msg>
    | BindCmd of Execute<'model, 'msg> * CanExecute<'model>
    | BindModel of Getter<'model> * ViewBindings<'model, 'msg>
    | BindMap of Getter<'model> * (obj -> obj)
"""

[<Test>]
let ``union type with constraint`` () =
    formatSourceString """type 'a t when 'a :> IDisposable = T  of  'a option""" config
    |> should
        equal
        """type 'a t when 'a :> IDisposable = T of 'a option
"""

[<Test>]
let ``add newline and indent for multiline internal record definition, 658`` () =
    formatSourceString
        """
type RequestParser<'ctx, 'a> = internal {
  consumedFields: Set<ConsumedFieldName>
  parse: 'ctx -> Request ->  Async<Result<'a, Error list>>
  prohibited: ProhibitedRequestGetter list
}
"""
        config
    |> prepend newline
    |> should
        equal
        """
type RequestParser<'ctx, 'a> =
    internal
        { consumedFields: Set<ConsumedFieldName>
          parse: 'ctx -> Request -> Async<Result<'a, Error list>>
          prohibited: ProhibitedRequestGetter list }
"""

[<Test>]
let ``generic nameof`` () =
    formatSourceString
        """
#r "nuget: FSharp.SystemTextJson"

open System.Text.Json
open System.Text.Json.Serialization
open System.Runtime.CompilerServices

nameof(+) // gives '+'
nameof op_Addition // gives 'op_Addition'

type C<'TType> =
    member _.TypeName = nameof<'TType> // Nameof with a generic type parameter via 'nameof<>'

/// Simplified version of EventStore's API
[<Struct; IsByRefLike>]
type RecordedEvent = { EventType: string; Data: ReadOnlySpan<byte> }

/// My concrete type:
type MyEvent =
    | AData of int
    | BData of string

// use 'nameof' instead of the string literal in the match expression
let deserialize (e: RecordedEvent) : MyEvent =
    match e.EventType with
    | nameof AData -> AData (JsonSerializer.Deserialize<int> e.Data)
    | nameof BData -> BData (JsonSerializer.Deserialize<string> e.Data)
    | t -> failwithf "Invalid EventType: %s" t
"""
        config
    |> prepend newline
    |> should
        equal
        """
#r "nuget: FSharp.SystemTextJson"

open System.Text.Json
open System.Text.Json.Serialization
open System.Runtime.CompilerServices

nameof (+) // gives '+'
nameof op_Addition // gives 'op_Addition'

type C<'TType> =
    member _.TypeName = nameof<'TType> // Nameof with a generic type parameter via 'nameof<>'

/// Simplified version of EventStore's API
[<Struct; IsByRefLike>]
type RecordedEvent =
    { EventType: string
      Data: ReadOnlySpan<byte> }

/// My concrete type:
type MyEvent =
    | AData of int
    | BData of string

// use 'nameof' instead of the string literal in the match expression
let deserialize (e: RecordedEvent) : MyEvent =
    match e.EventType with
    | nameof AData -> AData(JsonSerializer.Deserialize<int> e.Data)
    | nameof BData -> BData(JsonSerializer.Deserialize<string> e.Data)
    | t -> failwithf "Invalid EventType: %s" t
"""

[<Test>]
let ``member constraint on next line should have extra indent, 1394`` () =
    formatSourceString
        """
type Bar = | Bar of int
and Foo<'ret> = abstract Barrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr<'a> : 'a -> 'ret when 'a : comparison
"""
        { config with MaxLineLength = 80 }
    |> prepend newline
    |> should
        equal
        """
type Bar = Bar of int

and Foo<'ret> =
    abstract Barrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrrr<'a> :
        'a -> 'ret when 'a: comparison
"""

[<Test>]
let ``member constraint on next line with long return type`` () =
    formatSourceString
        """
type Foo =
    abstract Baaaaaaaaaaaaaarrrrrrr<'a> : 'a -> int -> string -> string -> bool when 'a : comparison
"""
        { config with MaxLineLength = 60 }
    |> prepend newline
    |> should
        equal
        """
type Foo =
    abstract Baaaaaaaaaaaaaarrrrrrr<'a> :
        'a -> int -> string -> string -> bool
            when 'a: comparison
"""

[<Test>]
let ``member with val keyword with multiline expression, 1426`` () =
    formatSourceString
        """
type public Foo() =

    // Here it generates valid code
    static member FooBarThing1 =
        new TypedThingDefinition(
            "StringA",
            SomeLongThing.SomeProperty,
            IsMandatory = new Nullable<bool>(true),
            Blablablabla = moreStuff
        )

    // With "member val" it generates invalid code
    static member val FooBarThing2 =
        new TypedThingDefinition(
            "StringA",
            SomeLongThing.SomeProperty,
            IsMandatory = new Nullable<bool>(true),
            Blablablabla = moreStuff
        )
"""
        config
    |> prepend newline
    |> should
        equal
        """
type public Foo() =

    // Here it generates valid code
    static member FooBarThing1 =
        new TypedThingDefinition(
            "StringA",
            SomeLongThing.SomeProperty,
            IsMandatory = new Nullable<bool>(true),
            Blablablabla = moreStuff
        )

    // With "member val" it generates invalid code
    static member val FooBarThing2 =
        new TypedThingDefinition(
            "StringA",
            SomeLongThing.SomeProperty,
            IsMandatory = new Nullable<bool>(true),
            Blablablabla = moreStuff
        )
"""

[<Test>]
let ``blank line before with keyword should be preserved`` () =
    formatSourceString
        """
type A =
  | B of int
  | C

  with
    member this.GetB =
      match this with
      | B x -> x
      | _ -> failwith "shouldn't happen"
"""
        config
    |> prepend newline
    |> should
        equal
        """
type A =
    | B of int
    | C

    member this.GetB =
        match this with
        | B x -> x
        | _ -> failwith "shouldn't happen"
"""

[<Test>]
let ``member inside compiler define using with keyword, 1503`` () =
    formatSourceString
        """
type A =
  | B of int
  | C

#if DEBUG
  with
    member this.GetB =
      match this with
      | B x -> x
      | _ -> failwith "shouldn't happen"
#endif
"""
        { config with IndentSize = 2 }
    |> prepend newline
    |> should
        equal
        """
type A =
  | B of int
  | C

#if DEBUG
  member this.GetB =
    match this with
    | B x -> x
    | _ -> failwith "shouldn't happen"
#endif
"""

[<Test>]
let ``const keyword in StaticConstantExpr should be preserved, 1574`` () =
    formatSourceString
        """
type T = SomeTypeProvider<const(" string literal " + REUSED_ LITERAL_STRING)>
"""
        config
    |> prepend newline
    |> should
        equal
        """
type T = SomeTypeProvider<const(" string literal " + REUSED_ LITERAL_STRING)>
"""

[<Test>]
let ``comment above constructor, 1286`` () =
    formatSourceString
        """
/// This is the type
type SomeType
      /// This is the implicit constructor
      (a: int, b: int) =

    /// This is the member
    member _.Sum() = a + b
"""
        config
    |> prepend newline
    |> should
        equal
        """
/// This is the type
type SomeType
    /// This is the implicit constructor
    (a: int, b: int) =

    /// This is the member
    member _.Sum() = a + b
"""

[<Test>]
let ``comment above multiline constructor`` () =
    formatSourceString
        """
/// This is the type
type SomeTypeWithQuiteTheLongNameThere
      /// This is the implicit constructor
      (a: int, b: int, c: string, d: DateTimeOffset, e: SomeReallyLongTypeNameOhBoyOhBoy, f: EvenLongerLongerTypeNameThanThatOtherTypeName) =

    /// This is the member
    member _.Sum() = a + b
"""
        config
    |> prepend newline
    |> should
        equal
        """
/// This is the type
type SomeTypeWithQuiteTheLongNameThere
    /// This is the implicit constructor
    (
        a: int,
        b: int,
        c: string,
        d: DateTimeOffset,
        e: SomeReallyLongTypeNameOhBoyOhBoy,
        f: EvenLongerLongerTypeNameThanThatOtherTypeName
    ) =

    /// This is the member
    member _.Sum() = a + b
"""

[<Test>]
let ``short or long member depending on compiler define, 1589`` () =
    formatSourceString
        """
type X =
    /// Indicates if the entity is a generated provided type definition, i.e. not erased.
    member x.IsProvidedGeneratedTycon =
        match x.TypeReprInfo with
        | TProvidedTypeExtensionPoint info -> info.IsGenerated
        | _ -> false

    /// Indicates if the entity is erased, either a measure definition, or an erased provided type definition
    member x.IsErased =
        x.IsMeasureableReprTycon
#if !NO_EXTENSIONTYPING
        || x.IsProvidedErasedTycon
#endif
"""
        config
    |> prepend newline
    |> should
        equal
        """
type X =
    /// Indicates if the entity is a generated provided type definition, i.e. not erased.
    member x.IsProvidedGeneratedTycon =
        match x.TypeReprInfo with
        | TProvidedTypeExtensionPoint info -> info.IsGenerated
        | _ -> false

    /// Indicates if the entity is erased, either a measure definition, or an erased provided type definition
    member x.IsErased =
        x.IsMeasureableReprTycon
#if !NO_EXTENSIONTYPING
        || x.IsProvidedErasedTycon
#endif
"""

[<Test>]
let ``short or long member depending on compiler define, followed by next member`` () =
    formatSourceString
        """
type X =
    /// Indicates if the entity is a generated provided type definition, i.e. not erased.
    member x.IsProvidedGeneratedTycon =
        match x.TypeReprInfo with
        | TProvidedTypeExtensionPoint info -> info.IsGenerated
        | _ -> false

    /// Indicates if the entity is erased, either a measure definition, or an erased provided type definition
    member x.IsErased =
        x.IsMeasureableReprTycon
#if !NO_EXTENSIONTYPING
        || x.IsProvidedErasedTycon
#endif

    /// Get a blob of data indicating how this type is nested inside other namespaces, modules and types.
    member x.CompilationPathOpt = x.entity_cpath
"""
        config
    |> prepend newline
    |> should
        equal
        """
type X =
    /// Indicates if the entity is a generated provided type definition, i.e. not erased.
    member x.IsProvidedGeneratedTycon =
        match x.TypeReprInfo with
        | TProvidedTypeExtensionPoint info -> info.IsGenerated
        | _ -> false

    /// Indicates if the entity is erased, either a measure definition, or an erased provided type definition
    member x.IsErased =
        x.IsMeasureableReprTycon
#if !NO_EXTENSIONTYPING
        || x.IsProvidedErasedTycon
#endif

    /// Get a blob of data indicating how this type is nested inside other namespaces, modules and types.
    member x.CompilationPathOpt = x.entity_cpath
"""

[<Test>]
let ``multiline type function signature`` () =
    formatSourceString
        """
namespace Test

module OrderProcessing =
  type ValidateOrder =
    CheckProductCodeExists    // dependency
      -> CheckAddressExists   // dependency
      -> UnvalidatedOrder     // input
      -> Result<ValidatedOrder,ValidationError>  // output (Result b/c one of deps returns a Result)
"""
        config
    |> prepend newline
    |> should
        equal
        """
namespace Test

module OrderProcessing =
    type ValidateOrder =
        CheckProductCodeExists // dependency
            -> CheckAddressExists // dependency
            -> UnvalidatedOrder // input
            -> Result<ValidatedOrder, ValidationError> // output (Result b/c one of deps returns a Result)
"""

[<Test>]
let ``generic type arguments in function invocation, 1637`` () =
    formatSourceString
        """
[<NoEquality ; NoComparison>]
type Foo<'context, 'a> =
    | Apply of ApplyCrate<'context, 'a>

and [<CustomEquality ; NoComparison>] Bar<'context, 'a> =
    internal {
        Hash : int
        Foo : Foo<'a, 'b>
    }
    member this.InnerEquals<'innerContextLongLongLong, 'd, 'e> (a : Foo<'innerContextLongLongLong, 'd>) (b : Foo<'innerContext, 'd>) (cont : bool -> 'e) : 'e =
        if a.Hash <> b.Hash then cont false
        else
            match a.Foo, b.Foo with
            | Foo.Apply a, Foo.Apply b ->
                a.Apply { new ApplyEval<_, _, _> with
                    member __.Eval<'bb> (a : Foo<'innerContextLongLongLong, 'bb -> 'b> * Foo<'innerContextLongLongLong, 'bb>) =
                        let (af, av) = a
                        b.Apply { new ApplyEval<_, _, _> with
                            member __.Eval<'cb> (b : Foo<'innerContextLongLongLong, 'cb -> 'b> * Foo<'innerContextLongLongLong, 'bc>) =
                                let (bf, bv) = b
                                if typeof<'bb> = typeof<'cb> then
                                    let bv = unbox<Foo<'innerContextLongLongLong, 'bb>> bv
                                    this.InnerEquals av bv (fun inner ->
                                        if inner then
                                            let bv = unbox<Foo<'innerContextLongLongLong, 'bb -> 'b>> bf
                                            this.InnerEquals af bf cont
                                        else cont false
                                    )
                                else cont false
                        }
                }
"""
        { config with
            MaxLineLength = 100
            SpaceBeforeUppercaseInvocation = true
            SpaceBeforeClassConstructor = true
            SpaceBeforeMember = true
            SpaceBeforeColon = true
            SpaceBeforeSemicolon = true
            MultilineBracketStyle = Aligned
            AlignFunctionSignatureToIndentation = true
            AlternativeLongMemberDefinitions = true
            MultiLineLambdaClosingNewline = true
            NewlineBetweenTypeDefinitionAndMembers = false }
    |> prepend newline
    |> should
        equal
        """
[<NoEquality ; NoComparison>]
type Foo<'context, 'a> = Apply of ApplyCrate<'context, 'a>

and [<CustomEquality ; NoComparison>] Bar<'context, 'a> =
    internal
        {
            Hash : int
            Foo : Foo<'a, 'b>
        }
    member this.InnerEquals<'innerContextLongLongLong, 'd, 'e>
        (a : Foo<'innerContextLongLongLong, 'd>)
        (b : Foo<'innerContext, 'd>)
        (cont : bool -> 'e)
        : 'e
        =
        if a.Hash <> b.Hash then
            cont false
        else
            match a.Foo, b.Foo with
            | Foo.Apply a, Foo.Apply b ->
                a.Apply
                    { new ApplyEval<_, _, _> with
                        member __.Eval<'bb>
                            (
                                a :
                                    Foo<'innerContextLongLongLong, 'bb -> 'b> *
                                    Foo<'innerContextLongLongLong, 'bb>
                            )
                            =
                            let (af, av) = a

                            b.Apply
                                { new ApplyEval<_, _, _> with
                                    member __.Eval<'cb>
                                        (
                                            b :
                                                Foo<'innerContextLongLongLong, 'cb -> 'b> *
                                                Foo<'innerContextLongLongLong, 'bc>
                                        )
                                        =
                                        let (bf, bv) = b

                                        if typeof<'bb> = typeof<'cb> then
                                            let bv = unbox<Foo<'innerContextLongLongLong, 'bb>> bv

                                            this.InnerEquals
                                                av
                                                bv
                                                (fun inner ->
                                                    if inner then
                                                        let bv =
                                                            unbox<
                                                                Foo<
                                                                    'innerContextLongLongLong,
                                                                    'bb -> 'b
                                                                 >
                                                             >
                                                                bf

                                                        this.InnerEquals af bf cont
                                                    else
                                                        cont false
                                                )
                                        else
                                            cont false
                                }
                    }
"""

[<Test>]
let ``multiple nested generic types`` () =
    formatSourceString
        """
let bv =
    unbox<
        Fooadfadadfdadfadfadfadfadfadfsfdsfadfadadfada<
            Foo<
                innerContextLongLongLong,
                bb
             >
         >
     >
        bf
"""
        { config with MaxLineLength = 10 }
    |> prepend newline
    |> should
        equal
        """
let bv =
    unbox<
        Fooadfadadfdadfadfadfadfadfadfsfdsfadfadadfada<
            Foo<
                innerContextLongLongLong,
                bb
             >
         >
     >
        bf
"""

[<Test>]
let ``Trivia inside multiline generic type parameters`` () =
    formatSourceString
        """
type X =
    Teq< //
        int
     //
     >
"""
        config
    |> prepend newline
    |> should
        equal
        """
type X =
    Teq< //
        int
     //
    >
"""

[<Test>]
let ``a huge amount of type declarations`` () =
    let sourceCode =
        List.init 1000 (sprintf "type FooBar%i = class end")
        |> String.concat "\n"
        |> sprintf
            """module A.Whole.Lot.Of.Types

%s
        """

    let formatted = formatSourceString sourceCode config

    // the result is less important here,
    // the point of this unit test is to verify if a stackoverflow problem at genModuleDeclList has been resolved.
    formatted |> should not' (equal EmptyString)

[<Test>]
let ``a huge amount of type declarations, signature file`` () =
    let sourceCode =
        List.init 1000 (sprintf "type FooBar%i = class end")
        |> String.concat "\n"
        |> sprintf
            """module A.Whole.Lot.Of.Types

%s
        """

    let formatted = formatSignatureString sourceCode config

    formatted |> should not' (equal EmptyString)

[<Test>]
let ``a huge amount of member bindings`` () =
    let sourceCode =
        List.init 1000 (sprintf "        member this.Bar%i () = ()")
        |> String.concat "\n"
        |> sprintf
            """module A.Whole.Lot.Of.MemberBindings

type FooBarry =
    interface Lorem with
%s
"""

    let formatted = formatSourceString sourceCode config

    formatted |> should not' (equal EmptyString)

[<Test>]
let ``a huge amount of member bindings, object expression`` () =
    let sourceCode =
        List.init 1000 (sprintf "        member this.Bar%i () = ()")
        |> String.concat "\n"
        |> sprintf
            """module A.Whole.Lot.Of.MemberBindings

let leBarry =
    { new SomeLargeInterface with
%s }
"""

    let formatted = formatSourceString sourceCode config

    formatted |> should not' (equal EmptyString)

[<Test>]
let ``trivia after parenthesis in syn type`` () =
    formatSourceString
        """
type A = ( (* string *) int * int )
"""
        config
    |> prepend newline
    |> should
        equal
        """
type A = ( (* string *) int * int)
"""

[<Test>]
let ``type abbreviation with comparison constraint, 2075`` () =
    formatSourceString
        """
type Graph<'a> when 'a:comparison = Set<'a * 'a>
"""
        config
    |> prepend newline
    |> should
        equal
        """
type Graph<'a> when 'a: comparison = Set<'a * 'a>
"""

[<Test>]
let ``type abbreviation with comparison constraint inside generic parameter scope`` () =
    formatSourceString
        """
type Graph<'a when 'a:comparison> = Set<'a * 'a>
"""
        config
    |> prepend newline
    |> should
        equal
        """
type Graph<'a when 'a: comparison> = Set<'a * 'a>
"""

[<Test>]
let ``comment after equals sign in type defn, 2001`` () =
    formatSourceString
        """
type V = // comment
    { X: SomeFieldType
      Y: OhSomethingElse
      Z: ALongTypeName }
"""
        config
    |> prepend newline
    |> should
        equal
        """
type V = // comment
    { X: SomeFieldType
      Y: OhSomethingElse
      Z: ALongTypeName }
"""

[<Test>]
let ``trivia between xml doc and member, 2147`` () =
    formatSignatureString
        """
// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

module internal FSharp.Compiler.Infos

type MethInfo =
    | FSMeth of tcGlobals: TcGlobals
    
/// Get the information about provided static parameters, if any
// #if NO_EXTENSIONTYPING
    member ProvidedStaticParameterInfo: obj option
// #else
//     member ProvidedStaticParameterInfo: (Tainted<ProvidedMethodBase> * Tainted<ProvidedParameterInfo> []) option
// #endif
"""
        config
    |> prepend newline
    |> should
        equal
        """
// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

module internal FSharp.Compiler.Infos

type MethInfo =
    | FSMeth of tcGlobals: TcGlobals

    /// Get the information about provided static parameters, if any
    // #if NO_EXTENSIONTYPING
    member ProvidedStaticParameterInfo: obj option
// #else
//     member ProvidedStaticParameterInfo: (Tainted<ProvidedMethodBase> * Tainted<ProvidedParameterInfo> []) option
// #endif
"""

[<Test>]
let ``hash directive between xml doc and member`` () =
    formatSignatureString
        """
// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

module internal FSharp.Compiler.Infos

type MethInfo =
    | FSMeth of tcGlobals: TcGlobals
    
/// Get the information about provided static parameters, if any
#if NO_EXTENSIONTYPING
    member ProvidedStaticParameterInfo: obj option
#else
    member ProvidedStaticParameterInfo: (Tainted<ProvidedMethodBase> * Tainted<ProvidedParameterInfo>[]) option
#endif
"""
        config
    |> prepend newline
    |> should
        equal
        """
// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

module internal FSharp.Compiler.Infos

type MethInfo =
    | FSMeth of tcGlobals: TcGlobals

    /// Get the information about provided static parameters, if any
#if NO_EXTENSIONTYPING
    member ProvidedStaticParameterInfo: obj option
#else
    member ProvidedStaticParameterInfo: (Tainted<ProvidedMethodBase> * Tainted<ProvidedParameterInfo>[]) option
#endif
"""

[<Test>]
let ``long type argument with constraints, 2266`` () =
    formatSourceString
        """
type Event<'Delegate, 'Args when 'Delegate: delegate<'Args, unit> and 'Delegate :> System.Delegate and 'Delegate: not struct> () =
                        class end
"""
        config
    |> prepend newline
    |> should
        equal
        """
type Event<'Delegate, 'Args
    when 'Delegate: delegate<'Args, unit> and 'Delegate :> System.Delegate and 'Delegate: not struct>() = class end
"""

[<Test>]
let ``long type argument with constraints, short max_line_length`` () =
    formatSourceString
        """
type Event<'Delegate, 'Args when 'Delegate: delegate<'Args, unit> and 'Delegate :> System.Delegate and 'Delegate: not struct> () =
                        class end
"""
        { config with MaxLineLength = 80 }
    |> prepend newline
    |> should
        equal
        """
type Event<'Delegate, 'Args
    when 'Delegate: delegate<'Args, unit>
    and 'Delegate :> System.Delegate
    and 'Delegate: not struct>() = class end
"""

[<Test>]
let ``unit of measure should keep idempotency after formatting, 2264`` () =
    formatSourceString
        """
[<Measure>] type herth = / second
"""
        config
    |> prepend newline
    |> should
        equal
        """
[<Measure>]
type herth = / second
"""

[<Test>]
let ``generic constraint placement in ML-style generic definitions with multiple type parameters, 1868`` () =
    formatSourceString
        """
type ('key, 'value) map when 'key: comparison = Map<'key, 'value>
"""
        config
    |> prepend newline
    |> should
        equal
        """
type ('key, 'value) map when 'key: comparison = Map<'key, 'value>
"""

[<Test>]
let ``don't add additional space before equals sign in long alternative constructor`` () =
    formatSourceString
        """
type LdapClaimsTransformation(
                                 ldapSearcher : ILdapSearcher,
                                 options : ILdapClaimsTransformationOptions
                             ) =

    interface IClaimsTransformation with
        member __.TransformAsync principle =
            3
"""
        { config with
            MaxLineLength = 60
            AlternativeLongMemberDefinitions = true }
    |> prepend newline
    |> should
        equal
        """
type LdapClaimsTransformation
    (
        ldapSearcher: ILdapSearcher,
        options: ILdapClaimsTransformationOptions
    )
    =

    interface IClaimsTransformation with
        member __.TransformAsync principle = 3
"""

[<Test>]
let ``multiline return type`` () =
    formatSourceString
        """
type Meh =
    static member AsBeginEnd<'Arg, 'T>
        (computation: ('Arg -> Async<'T>))
        // The 'Begin' member
        : ('Arg * System.AsyncCallback * obj -> System.IAsyncResult) * (System.IAsyncResult -> 'T) * (System.IAsyncResult -> unit) =
        let beginAction =
            fun (a1, callback, state) -> AsBeginEndHelpers.beginAction ((computation a1), callback, state)

        beginAction, AsBeginEndHelpers.endAction<'T>, AsBeginEndHelpers.cancelAction<'T>
"""
        config
    |> prepend newline
    |> should
        equal
        """
type Meh =
    static member AsBeginEnd<'Arg, 'T>
        (computation: ('Arg -> Async<'T>))
        // The 'Begin' member
        : ('Arg * System.AsyncCallback * obj -> System.IAsyncResult) *
          (System.IAsyncResult -> 'T) *
          (System.IAsyncResult -> unit)
        =
        let beginAction =
            fun (a1, callback, state) -> AsBeginEndHelpers.beginAction ((computation a1), callback, state)

        beginAction, AsBeginEndHelpers.endAction<'T>, AsBeginEndHelpers.cancelAction<'T>
"""

[<Test>]
let ``alternativeLongMemberDefinitions breaks spacing around type Blah as this, 2598`` () =
    formatSourceString
        """
type Server<'a>
    (
        clusterSize : int,
        persistentState : IPersistentState<'a>,
        messageChannel : int<ServerId> -> Message<'a> -> unit
    ) as this =
    let mutable i = 0
    member this.Blah = 0
"""
        { config with
            AlternativeLongMemberDefinitions = true
            MaxLineLength = 80 }
    |> prepend newline
    |> should
        equal
        """
type Server<'a>
    (
        clusterSize: int,
        persistentState: IPersistentState<'a>,
        messageChannel: int<ServerId> -> Message<'a> -> unit
    )
    as this =
    let mutable i = 0
    member this.Blah = 0
"""

[<Test>]
let ``conditional directive around access modifier, 628`` () =
    formatSourceString
        """
type
#if DEBUG
#else
     internal
#endif
              ReportFormat =
  | NCover = 0
  | OpenCover = 1
  | OpenCoverWithTracking = 2
"""
        config
    |> prepend newline
    |> should
        equal
        """
type
#if DEBUG
#else
    internal
#endif
    ReportFormat =
    | NCover = 0
    | OpenCover = 1
    | OpenCoverWithTracking = 2
"""

[<Test>]
let ``block comment between type and identifier`` () =
    formatSourceString
        """
type (* foo *)  Bar = int
"""
        config
    |> prepend newline
    |> should
        equal
        """
type (* foo *) Bar = int
"""

[<Test>]
let ``comment between empty implicit constructor, 1872`` () =
    formatSourceString
        """
  type MyType
    (
      (* some comment *)
    ) = 
    let x = 5
"""
        config
    |> prepend newline
    |> should
        equal
        """
type MyType
    (
    (* some comment *)
    ) =
    let x = 5
"""

[<Test>]
let ``trivia before attributes in recursive type, 2361 `` () =
    formatSourceString
        """
module Primitives =
    type BlockHeight =
        | BlockHeight of uint32 

    and
#if !NoDUsAsStructs
        [<Struct>]
#endif
        BlockHeightOffset16 =
            | BlockHeightOffset16 of uint16

"""
        config
    |> prepend newline
    |> should
        equal
        """
module Primitives =
    type BlockHeight = BlockHeight of uint32

    and
#if !NoDUsAsStructs
        [<Struct>]
#endif
        BlockHeightOffset16 = BlockHeightOffset16 of uint16
"""

[<Test>]
let ``xml doc before recursive type, 2360`` () =
    formatSourceString
        """
module Primitives =
    type BlockHeight =
        | BlockHeight of uint32 

    /// **Description**
    ///
    /// 16bit relative block height used for `OP_CSV` locks,
    /// Since OP_CSV allow only block number of 0 ~ 65535, it is safe
    /// to restrict into the range smaller than BlockHeight
    and
#if !NoDUsAsStructs
        [<Struct>]
#endif
        BlockHeightOffset16 =
            | BlockHeightOffset16 of uint16
"""
        config
    |> prepend newline
    |> should
        equal
        """
module Primitives =
    type BlockHeight = BlockHeight of uint32

    /// **Description**
    ///
    /// 16bit relative block height used for `OP_CSV` locks,
    /// Since OP_CSV allow only block number of 0 ~ 65535, it is safe
    /// to restrict into the range smaller than BlockHeight
    and
#if !NoDUsAsStructs
        [<Struct>]
#endif
        BlockHeightOffset16 = BlockHeightOffset16 of uint16
"""

[<Test>]
let ``trivia before constructor parameter, 2692`` () =
    formatSourceString
        """
type SingleAppParenLambda
    (
        // Expr could be a single identifier or TypeApp
        functionName: Expr, parenLambda: ExprParenLambdaNode, range
    ) =
    inherit NodeBase(range)
    override this.Children = [| yield Expr.Node functionName; yield parenLambda |]
    member x.FunctionName = functionName
    member x.ParenLambda = parenLambda
"""
        config
    |> prepend newline
    |> should
        equal
        """
type SingleAppParenLambda
    (
        // Expr could be a single identifier or TypeApp
        functionName: Expr, parenLambda: ExprParenLambdaNode, range
    ) =
    inherit NodeBase(range)
    override this.Children = [| yield Expr.Node functionName; yield parenLambda |]
    member x.FunctionName = functionName
    member x.ParenLambda = parenLambda
"""

[<Test>]
let ``optional constructor arguments with attributes, 2718`` () =
    formatSourceString
        """
type CsvFile
    private
    (
        readerFunc: Func<TextReader>,
        [<Optional>] ?separators,
        [<Optional>] ?quote,
        [<Optional>] ?hasHeaders,
        [<Optional>] ?ignoreErrors,
        [<Optional>] ?skipRows
    ) as this =
    inherit CsvFile<CsvRow>
        (
            Func<_, _, _>(fun this columns -> CsvRow(this :?> CsvFile, columns)),
            Func<_, _>(fun row -> row.Columns),
            readerFunc,
            defaultArg separators "",
            defaultArg quote '"',
            defaultArg hasHeaders true,
            defaultArg ignoreErrors false,
            defaultArg skipRows 0
        )
"""
        config
    |> prepend newline
    |> should
        equal
        """
type CsvFile
    private
    (
        readerFunc: Func<TextReader>,
        [<Optional>] ?separators,
        [<Optional>] ?quote,
        [<Optional>] ?hasHeaders,
        [<Optional>] ?ignoreErrors,
        [<Optional>] ?skipRows
    ) as this =
    inherit
        CsvFile<CsvRow>(
            Func<_, _, _>(fun this columns -> CsvRow(this :?> CsvFile, columns)),
            Func<_, _>(fun row -> row.Columns),
            readerFunc,
            defaultArg separators "",
            defaultArg quote '"',
            defaultArg hasHeaders true,
            defaultArg ignoreErrors false,
            defaultArg skipRows 0
        )
"""

[<Test>]
let ``optional parameter with ticks, 2731`` () =
    formatSourceString
        """
type [<AllowNullLiteral>] ArrayBuffer =
    abstract byteLength: int
    abstract slice: ``begin``: int * ?``end``: int -> ArrayBuffer
"""
        config
    |> prepend newline
    |> should
        equal
        """
[<AllowNullLiteral>]
type ArrayBuffer =
    abstract byteLength: int
    abstract slice: ``begin``: int * ?``end``: int -> ArrayBuffer
"""

[<Test>]
let ``optional member parameter with ticks, 2954`` () =
    formatSourceString
        """
type C() =
    static member foo(?``type``) = ``type``
"""
        config
    |> prepend newline
    |> should
        equal
        """
type C() =
    static member foo(?``type``) = ``type``
"""

[<Test>]
let ``trivia before comma in primary constructor`` () =
    formatSourceString
        """
type Meh
    (
        a,
        b
#if !NO_TYPEPROVIDERS
        , c
#endif
    ) =
        class end
"""
        config
    |> prepend newline
    |> should
        equal
        """
type Meh
    (
        a,
        b
#if !NO_TYPEPROVIDERS
        ,
        c
#endif
    ) = class end
"""

[<Test>]
let ``multi tuple setter with indexer, 2971`` () =
    formatSourceString
        """
type MyArray3 () = 
    member _.Item
        with get (x: int, y: int, z: int) =
            ()
        and set (x: int, y: int, z: int) v =
            ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
type MyArray3() =
    member _.Item
        with get (x: int, y: int, z: int) = ()
        and set (x: int, y: int, z: int) v = ()
"""

[<Test>]
let ``intersection constraint, 2984`` () =
    formatSourceString
        """
let typographyLabel<'msg, 'marker & #IFabLabel>() = ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
let typographyLabel<'msg, 'marker & #IFabLabel> () = ()
"""

[<Test>]
let ``multiple intersection constraint, 2984`` () =
    formatSourceString
        """
let typographyLabel<'msg, 'marker & #IFabLabel & #IFoo & #Bar>() = ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
let typographyLabel<'msg, 'marker & #IFabLabel & #IFoo & #Bar> () = ()
"""

[<Test>]
let ``trivia after mutable keyword, 3005`` () =
    formatSourceString
        """
type R =
    {
        F1: int
        mutable // voobar
            F2: int
        F3: int
    }
"""
        config
    |> prepend newline
    |> should
        equal
        """
type R =
    { F1: int
      mutable // voobar
          F2: int
      F3: int }
"""

[<Test>]
let ``long tupled member where tuple fits on single line but return type doesn't, 3041`` () =
    formatSourceString
        """
type FSharpChecker with

    member this.ParseAndCheckDocument
        (
            filePath: string,
            sourceText: string,
            options: FSharpProjectOptions,
            allowStaleResults: bool
        ) : Async<(FSharpParseFileResults * ParsedInput * FSharpCheckFileResults) option> =
        ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
type FSharpChecker with

    member this.ParseAndCheckDocument
        (filePath: string, sourceText: string, options: FSharpProjectOptions, allowStaleResults: bool)
        : Async<(FSharpParseFileResults * ParsedInput * FSharpCheckFileResults) option> =
        ()
"""

[<Test>]
let ``single member union extensions without pipe, 3102`` () =
    formatSourceString
        """
type X = X
    with
        static member x = 1
    """
        config
    |> prepend newline
    |> should
        equal
        """
type X = X
    with

        static member x = 1
"""

[<Test>]
let ``single member union extensions without pipe idempotent, 3102`` () =
    formatSourceString
        """
type X = X
    with

        static member x = 1
    """
        config
    |> prepend newline
    |> should
        equal
        """
type X = X
    with

        static member x = 1
"""

[<Test>]
let ``type without body but with comment produces superfluous newline, 3145`` () =
    formatSourceString
        """
type X // oh dear

23
    """
        config
    |> prepend newline
    |> should
        equal
        """
type X // oh dear

23
"""
