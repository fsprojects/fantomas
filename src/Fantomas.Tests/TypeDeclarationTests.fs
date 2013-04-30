module Fantomas.Tests.TypeDeclarationTests

open NUnit.Framework
open FsUnit

open Fantomas.CodeFormatter
open Fantomas.Tests.TestHelper

[<Test>]
let ``exception declations``() =
    formatSourceString false "exception Error2 of string * int" config
    |> should equal """exception Error2 of string * int
"""

[<Test>]
let ``type annotations``() =
    formatSourceString false """
    let iterate1 (f : unit -> seq<int>) =
        for e in f() do printfn "%d" e
    let iterate2 (f : unit -> #seq<int>) =
        for e in f() do printfn "%d" e""" config
    |> prepend newline
    |> should equal """
let iterate1(f : unit -> seq<int>) = 
    for e in f() do
        printfn "%d" e

let iterate2(f : unit -> #seq<int>) = 
    for e in f() do
        printfn "%d" e
"""

[<Test>]
let ``upcast and downcast``() =
    formatSourceString false """
    let base1 = d1 :> Base1
    let derived1 = base1 :?> Derived1""" config
    |> prepend newline
    |> should equal """
let base1 = d1 :> Base1

let derived1 = base1 :?> Derived1
"""

[<Test>]
let ``optional arguments``() =
    formatSourceString false """
type Connection(?rate0 : int, ?duplex0 : DuplexType, ?parity0 : bool) =
    let duplex = defaultArg duplex0 Full
    let parity = defaultArg parity0 false 
    let mutable rate = match rate0 with
                        | Some rate1 -> rate1
                        | None -> match duplex with
                                  | Full -> 9600
                                  | Half -> 4800
    do printfn "Baud Rate: %d Duplex: %A Parity: %b" rate duplex parity""" config
    |> prepend newline
    |> should equal """
type Connection(?rate0 : int, ?duplex0 : DuplexType, ?parity0 : bool) = 
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
let ``method params``() =
    formatSourceString false """
type Test() =
    member this.Function1<'a>(x, y) =
        printfn "%A, %A" x y

    abstract abstractMethod<'a, 'b> : 'a * 'b -> unit
    override this.abstractMethod<'a, 'b>(x:'a, y:'b) =
         printfn "%A, %A" x y""" config
    |> prepend newline
    |> should equal """
type Test() = 
    member this.Function1<'a>(x, y) = printfn "%A, %A" x y
    abstract abstractMethod<'a, 'b> : 'a * 'b -> unit
    override this.abstractMethod<'a, 'b>(x : 'a, y : 'b) = printfn "%A, %A" x y
"""

[<Test>]
let ``params arguments``() =
    formatSourceString false """
type X() =
    member this.F([<ParamArray>] args: Object[]) =
        for arg in args do
            printfn "%A" arg""" config
    |> prepend newline
    |> should equal """
type X() = 
    member this.F([<ParamArray>] args : Object[]) = 
        for arg in args do
            printfn "%A" arg
"""

[<Test>]
let ``generic types``() =
    formatSourceString false """
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
    member self.Method(a,b) = x + y + z + a + b""" config
    |> prepend newline
    |> should equal """
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
let ``struct declaration``() =
    formatSourceString false """
    type Point2D =
       struct 
          val X: float
          val Y: float
          new(x: float, y: float) = { X = x; Y = y }
       end""" config
    |> prepend newline
    |> should equal """
type Point2D = 
    struct
        val X : float
        val Y : float
        new(x : float, y : float) = 
            { X = x;
              Y = y }
    end
"""

[<Test>]
let ``abstract and override keywords``() =
    formatSourceString false """
    type MyClassBase1() =
       let mutable z = 0
       abstract member function1 : int -> int
       default u.function1(a : int) = z <- z + a; z

    type MyClassDerived1() =
       inherit MyClassBase1()
       override u.function1(a: int) = a + 1""" config
    |> prepend newline
    |> should equal """
type MyClassBase1() = 
    let mutable z = 0
    abstract function1 : int -> int
    override u.function1(a : int) = 
        z <- z + a
        z

type MyClassDerived1() = 
    inherit MyClassBase1()
    override u.function1(a : int) = a + 1
"""

[<Test>]
let ``intrinsic type extensions``() =
    formatSourceString false """
type MyClass() =
      member this.F() = 100

type MyClass with 
    member this.G() = 200""" config
    |> prepend newline
    |> should equal """
type MyClass() = 
    member this.F() = 100

type MyClass with
    member this.G() = 200
"""

[<Test>]
let ``optional type extensions``() =
    formatSourceString false """
/// Define a new member method FromString on the type Int32. 
type System.Int32 with 
    member this.FromString( s : string ) =
       System.Int32.Parse(s)""" config
    |> prepend newline
    |> should equal """
/// Define a new member method FromString on the type Int32. 
type System.Int32 with
    member this.FromString(s : string) = System.Int32.Parse(s)
"""

[<Test>]
let ``auto property``() =
    formatSourceString false """
type MyClass(property1 : int) =
    member val Property1 = property1
    member val Property2 = "" with get, set""" config
    |> prepend newline
    |> should equal """
type MyClass(property1 : int) = 
    member val Property1 = property1
    member val Property2 = "" with get, set
"""

[<Test>]
let ``property handling``() =
    formatSourceString false """
type Derived1() =
   inherit AbstractBase()
   let mutable value = 10 
   override this.Property1 with get() = value and set(v : int) = value <- v""" config
    |> prepend newline
    |> should equal """
type Derived1() = 
    inherit AbstractBase()
    let mutable value = 10
    override this.Property1 
        with get () = value
        and set (v : int) = value <- v
"""

[<Test>]
let ``types with attributes``() =
    formatSourceString false """
type MyType() =
    let mutable myInt1 = 10
    [<DefaultValue>] val mutable myInt2 : int
    [<DefaultValue>] val mutable myString : string
    member this.SetValsAndPrint( i: int, str: string) =
       myInt1 <- i
       this.myInt2 <- i + 1
       this.myString <- str
       printfn "%d %d %s" myInt1 (this.myInt2) (this.myString)""" config
    |> prepend newline
    |> should equal """
type MyType() = 
    let mutable myInt1 = 10
    [<DefaultValue>] val mutable myInt2 : int
    [<DefaultValue>] val mutable myString : string
    member this.SetValsAndPrint(i : int, str : string) = 
        myInt1 <- i
        this.myInt2 <- i + 1
        this.myString <- str
        printfn "%d %d %s" myInt1 (this.myInt2) (this.myString)
"""

[<Test>]
let ``named arguments``() =
    formatSourceString false """
type SpeedingTicket() =
    member this.GetMPHOver(speed: int, limit: int) = speed - limit

let CalculateFine (ticket : SpeedingTicket) =
    let delta = ticket.GetMPHOver(limit = 55, speed = 70)
    if delta < 20 then 50.0 else 100.0""" config
    |> prepend newline
    |> should equal """
type SpeedingTicket() = 
    member this.GetMPHOver(speed : int, limit : int) = speed - limit

let CalculateFine(ticket : SpeedingTicket) = 
    let delta = ticket.GetMPHOver(limit = 55, speed = 70)
    if delta < 20
    then 50.0
    else 100.0
"""

[<Test>]
let ``indexed properties``() =
    formatSourceString false """
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
      and set index value = cardinals.[index] <- value""" config
    |> prepend newline
    |> should equal """
type NumberStrings() = 
    let mutable ordinals = [|"one"|]
    let mutable cardinals = [|"first"|]
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
let ``complex indexed properties``() =
    formatSourceString false """
open System.Collections.Generic
type SparseMatrix() =
    let mutable table = new Dictionary<int * int, float>()
    member this.Item
        with get(key1, key2) = table.[(key1, key2)]
        and set (key1, key2) value = table.[(key1, key2)] <- value

let matrix1 = new SparseMatrix()
for i in 1..1000 do
    matrix1.[i, i] <- float i * float i
    """ config
    |> prepend newline
    |> should equal """
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
let ``type constraints simple``() =
    formatSourceString false """
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
    class end""" config
    |> prepend newline
    |> should equal """
type Class1<'T when 'T :> System.Exception> = 
    class
    end

type Class2<'T when 'T :> System.IComparable> = 
    class
    end

type Class3<'T when 'T : null> = 
    class
    end

type Class8<'T when 'T : not struct> = 
    class
    end

type Class9<'T when 'T : enum<uint32>> = 
    class
    end

type Class10<'T when 'T : comparison> = 
    class
    end

type Class11<'T when 'T : equality> = 
    class
    end

type Class12<'T when 'T : delegate<obj * System.EventArgs, unit>> = 
    class
    end

type Class13<'T when 'T : unmanaged> = 
    class
    end

type Class14<'T, 'U when 'T : equality and 'U : equality> = 
    class
    end
"""

[<Test>]
let ``then blocks after constructors``() =
    formatSourceString false """
type Person(nameIn : string, idIn : int) =
    let mutable name = nameIn
    let mutable id = idIn
    do printfn "Created a person object." 
    member this.Name with get() = name and set(v) = name <- v
    member this.ID with get() = id and set(v) = id <- v
    new() = 
        Person("Invalid Name", -1)
        then
            printfn "Created an invalid person object."
            """ config
    |> prepend newline
    |> should equal """
type Person(nameIn : string, idIn : int) = 
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
let ``associativity of types``() =
    formatSourceString false """
type Delegate1 = delegate of (int * int) * (int * int) -> int
type Delegate2 = delegate of int * int -> int
type Delegate3 = delegate of int -> (int -> int)
type Delegate4 = delegate of int -> int -> int
type U = U of (int * int)
    """ config
    |> prepend newline
    |> should equal """
type Delegate1 = delegate of (int * int) * (int * int) -> int
type Delegate2 = delegate of int * int -> int
type Delegate3 = delegate of int -> (int -> int)
type Delegate4 = delegate of int -> int -> int
type U = 
    | U of (int * int)
"""

[<Test>]
let ``should keep the ? in optional parameters``() =
    formatSourceString false """type Shell() = 
    static member private GetParams(cmd, ?args) = doStuff
    static member Exec(cmd, ?args) = 
        shellExec(Shell.GetParams(cmd, ?args = args))

    """ config
    |> should equal """type Shell() = 
    static member private GetParams(cmd, ?args) = doStuff
    static member Exec(cmd, ?args) = 
        shellExec(Shell.GetParams(cmd, ?args = args))
"""

[<Test>]
let ``should add space before argument on given config``() =
    formatSourceString false """
let f(x: int) = x

type t(x : int) = 
    class
    end
    """ { config with SpaceBeforeArgument = true; SpaceBeforeColon = false }
    |> prepend newline
    |> should equal """
let f (x: int) = x

type t (x: int) = 
    class
    end
"""