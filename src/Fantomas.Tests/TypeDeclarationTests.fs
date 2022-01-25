module Fantomas.Tests.TypeDeclarationTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper

[<Test>]
let ``exception declarations`` () =
    formatSourceString false "exception Error2 of string * int" config
    |> should
        equal
        """exception Error2 of string * int
"""

[<Test>]
let ``exception declarations with members`` () =
    formatSourceString
        false
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
let ``type annotations`` () =
    formatSourceString
        false
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
        false
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
        false
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
        false
        """
type Test() =
    member this.Function1<'a>(x, y) =
        printfn "%A, %A" x y

    abstract AbstractMethod<'a, 'b> : 'a * 'b -> unit
    override this.AbstractMethod<'a, 'b>(x:'a, y:'b) =
         printfn "%A, %A" x y"""
        { config with MaxFunctionBindingWidth = 120 }
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
        false
        """
type X() =
    member this.F([<ParamArray>] args: Object []) =
        for arg in args do
            printfn "%A" arg"""
        config
    |> prepend newline
    |> should
        equal
        """
type X() =
    member this.F([<ParamArray>] args: Object []) =
        for arg in args do
            printfn "%A" arg
"""

[<Test>]
let ``generic types`` () =
    formatSourceString
        false
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
        { config with MaxFunctionBindingWidth = 120 }
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
        false
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
        false
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
        false
        """
type MyClass() =
      member this.F() = 100

type MyClass with
    member this.G() = 200"""
        config
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
        false
        """
/// Define a new member method FromString on the type Int32.
type System.Int32 with
    member this.FromString( s : string ) =
       System.Int32.Parse(s)"""
        { config with MaxFunctionBindingWidth = 120 }
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
        false
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
        false
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
        false
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
        false
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
        false
        """
type SpeedingTicket() =
    member this.GetMPHOver(speed: int, limit: int) = speed - limit

let CalculateFine (ticket : SpeedingTicket) =
    let delta = ticket.GetMPHOver(limit = 55, speed = 70)
    if delta < 20 then 50.0 else 100.0"""
        { config with MaxValueBindingWidth = 120 }
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
let ``indexed properties`` () =
    formatSourceString
        false
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
        false
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
        false
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
type Class1<'T when 'T :> System.Exception> =
    class
    end

type Class2<'T when 'T :> System.IComparable> =
    class
    end

type Class3<'T when 'T: null> =
    class
    end

type Class8<'T when 'T: not struct> =
    class
    end

type Class9<'T when 'T: enum<uint32>> =
    class
    end

type Class10<'T when 'T: comparison> =
    class
    end

type Class11<'T when 'T: equality> =
    class
    end

type Class12<'T when 'T: delegate<obj * System.EventArgs, unit>> =
    class
    end

type Class13<'T when 'T: unmanaged> =
    class
    end

type Class14<'T, 'U when 'T: equality and 'U: equality> =
    class
    end
"""

[<Test>]
let ``then blocks after constructors`` () =
    formatSourceString
        false
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
        false
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
let ``should keep the ? in optional parameters`` () =
    formatSourceString
        false
        """type Shell() =
    static member private GetParams(cmd, ?args) = doStuff
    static member Exec(cmd, ?args) =
        shellExec(Shell.GetParams(cmd, ?args = args))

    """
        { config with MaxFunctionBindingWidth = 120 }
    |> should
        equal
        """type Shell() =
    static member private GetParams(cmd, ?args) = doStuff
    static member Exec(cmd, ?args) = shellExec (Shell.GetParams(cmd, ?args = args))
"""

[<Test>]
let ``should add space before argument on given config`` () =
    formatSourceString
        false
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

type t(x : int) =
    class
    end
"""

[<Test>]
let ``should keep brackets around type signatures`` () =
    formatSourceString
        false
        """
let user_printers = ref([] : (string * (term -> unit)) list)
let the_interface = ref([] : (string * (string * hol_type)) list)
    """
        { config with MaxValueBindingWidth = 50 }
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
        false
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
        false
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
        config
    |> prepend newline
    |> should
        equal
        """
type BlobHelper(Account: CloudStorageAccount) =
    new(configurationSettingName, hostedService) =
        CloudStorageAccount.SetConfigurationSettingPublisher (fun configName configSettingPublisher ->
            let connectionString =
                if hostedService then
                    RoleEnvironment.GetConfigurationSettingValue(configName)
                else
                    ConfigurationManager.ConnectionStrings.[configName]
                        .ConnectionString

            configSettingPublisher.Invoke(connectionString)
            |> ignore)

        BlobHelper(CloudStorageAccount.FromConfigurationSetting(configurationSettingName))
"""

[<Test>]
let ``^a needs spaces when used as a type parameter`` () =
    formatSourceString
        false
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
        false
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
        false
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
        false
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
        false
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
        false
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
        false
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
                | Some (x) -> x
                | None -> null

            ignore x
"""

[<Test>]
let ``should go to new lines on long property bodies`` () =
    formatSourceString
        false
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
        false
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
            | Some (m) when m.Groups.[i].Success -> m.Groups.[i].Value
            | _ -> null

    member this.Item
        with get (i: string) =
            match mo with
            | Some (m) when m.Groups.[i].Success -> m.Groups.[i].Value
            | _ -> null
"""

[<Test>]
let ``should not add dubious new line inside call chains`` () =
    formatSourceString
        false
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
        false
        """
type ILogger =
    abstract DebugFormat : format:String * [<ParamArray>]args:Object [] -> unit"""
        config
    |> prepend newline
    |> should
        equal
        """
type ILogger =
    abstract DebugFormat: format: String * [<ParamArray>] args: Object [] -> unit
"""

[<Test>]
let ``should preserve brackets on type signatures`` () =
    formatSourceString
        false
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
        false
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
        false
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
    override val Id = 0 with get, set
"""

[<Test>]
let ``type abbreviation augmentation`` () =
    formatSourceString
        false
        """type T2 = T2 with
    member __.X = ()
"""
        config
    |> should
        equal
        """type T2 = T2
    with
        member __.X = ()
"""

[<Test>]
let ``operator in words should not print to symbol, 409`` () =
    formatSourceString
        false
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
    formatSourceString false """let op_PipeRight2  = ()""" config
    |> should
        equal
        """let op_PipeRight2 = ()
"""

[<Test>]
let ``operator in words in member`` () =
    formatSourceString
        false
        """type A() =
    member this.B(op_Inequality : string) = ()"""
        { config with MaxFunctionBindingWidth = 120 }
    |> should
        equal
        """type A() =
    member this.B(op_Inequality: string) = ()
"""

[<Test>]
let ``attributes on extension methods should not add newlines, 473`` () =
    formatSourceString
        false
        """
[<Extension>]
type TestExtensions =

    [<Extension>]
    static member SomeExtension(x) = ""

    [<Extension>]
    static member SomeOtherExtension(x) = ""
"""
        { config with MaxValueBindingWidth = 120 }
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
        false
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
        false
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
        false
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
        false
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
        false
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
        false
        """type C () =
    member __.LongMethodWithLotsOfParameters(aVeryLongType: AVeryLongTypeThatYouNeedToUse, aSecondVeryLongType: AVeryLongTypeThatYouNeedToUse, aThirdVeryLongType: AVeryLongTypeThatYouNeedToUse) =  aVeryLongType aSecondVeryLongType aThirdVeryLongType
"""
        { config with SpaceBeforeClassConstructor = true }
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
        false
        """type C () =
    member __.LongMethodWithLotsOfParameters(aVeryLongType: AVeryLongTypeThatYouNeedToUse, aSecondVeryLongType: AVeryLongTypeThatYouNeedToUse, aThirdVeryLongType: AVeryLongTypeThatYouNeedToUse) : int =  aVeryLongType aSecondVeryLongType aThirdVeryLongType
"""
        { config with SpaceBeforeClassConstructor = true }
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
        false
        """type C (aVeryLongType : AVeryLongTypeThatYouNeedToUse, aSecondVeryLongType : AVeryLongTypeThatYouNeedToUse, aThirdVeryLongType : AVeryLongTypeThatYouNeedToUse) =
    member this.X = 42
"""
        { config with SpaceBeforeClassConstructor = true }
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
        false
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
        false
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
        false
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
        false
        """type SomeType =
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
        false
        """type SomeType =
    static member SomeMember (looooooooooooooooooooooooooooooooooooooooooooooooooooooooooooong1: string) (looooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooong2: string) : string =
    printfn "a"
    "b"

    static member SomeOtherMember () = printfn "c"
"""
        { config with MaxFunctionBindingWidth = 120 }
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
        false
        """type SomeType =
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
        false
        """type SomeType =
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
        false
        """type SomeType =
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
        (loooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooong2: SomeType)
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
        false
        """type INotifications<'a,'b,'c,'d,'e> =
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
type INotifications<'a, 'b, 'c, 'd, 'e> =
    class
    end

type DeviceNotificationHandler<'Notification, 'CallbackId, 'RegisterInputData, 'RegisterOutputData, 'UnregisterOutputData>
    private
    (
        client: INotifications<'Notification, 'CallbackId, 'RegisterInputData, 'RegisterOutputData, 'UnregisterOutputData>,
        callbackId: 'CallbackId,
        validateUnregisterOutputData: 'UnregisterOutputData -> unit
    ) =
    let a = 5
"""


[<Test>]
let ``long type members should be in multiple lines, 868`` () =
    formatSourceString
        false
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
        false
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
        false
        """
type VersionMismatchDuringDeserializationException(message: string, innerException: System.Exception) =
    inherit System.Exception(message, innerException)
"""
        { config with
            MaxLineLength = 80
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
        false
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
        false
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
        false
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
        false
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
        false
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
        false
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
        false
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
        false
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
        false
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
    private { Foo: int }
"""

[<Test>]
let ``comments before access modifier and multiline record type`` () =
    formatSourceString
        false
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
        false
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
        false
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
        false
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
        false
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
        false
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
        false
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
        false
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
    =
    class
    end
"""

[<Test>]
let ``alternative long class constructor with access modifier`` () =
    formatSourceString
        false
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
    =
    class
    end
"""

[<Test>]
let ``trivia before properties, 1009`` () =
    formatSourceString
        false
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
        false
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
        false
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
    formatSourceString false """type 'a t when 'a :> IDisposable = T  of  'a option""" config
    |> should
        equal
        """type 'a t when 'a :> IDisposable = T of 'a option
"""

[<Test>]
let ``add newline and indent for multiline internal record definition, 658`` () =
    formatSourceString
        false
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
        false
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
        false
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
        false
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
        false
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
        false
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
        false
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
        false
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
        false
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
        false
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
        false
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
        false
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
        false
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
        false
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
            MultilineBlockBracketsOnSameColumn = true
            KeepIfThenInSameLine = true
            AlignFunctionSignatureToIndentation = true
            AlternativeLongMemberDefinitions = true
            MultiLineLambdaClosingNewline = true }
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
                            (a : Foo<'innerContextLongLongLong, 'bb -> 'b> * Foo<'innerContextLongLongLong, 'bb>)
                            =
                            let (af, av) = a

                            b.Apply
                                { new ApplyEval<_, _, _> with
                                    member __.Eval<'cb>
                                        (b : Foo<'innerContextLongLongLong, 'cb -> 'b> * Foo<'innerContextLongLongLong, 'bc>)
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
                                                            unbox<Foo<'innerContextLongLongLong, 'bb
                                                                          -> 'b>>
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
let ``a huge amount of type declarations`` () =
    let sourceCode =
        List.init 1000 (sprintf "type FooBar%i = class end")
        |> String.concat "\n"
        |> sprintf
            """module A.Whole.Lot.Of.Types

%s
        """

    let formatted = formatSourceString false sourceCode config

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

    let formatted = formatSourceString true sourceCode config

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

    let formatted = formatSourceString false sourceCode config

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

    let formatted = formatSourceString false sourceCode config

    formatted |> should not' (equal EmptyString)

[<Test>]
let ``trivia after parenthesis in syn type`` () =
    formatSourceString
        false
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
