module Fantomas.Tests.CodeFormatterExtExtTests

open NUnit.Framework
open FsUnit

open Fantomas.CodeFormatter
open Fantomas.Tests.TestHelper


[<Test>]
let ``classes and inheritance``() =
    formatSourceString false """
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
    formatSourceString false """
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
            { X = x; Y = y }
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
    override this.Property1 with get () = value
    override this.Property1 with set (v : int) = value <- v
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
    member self.Prop2 with get () = z
    member self.Prop2 with set (a) = z <- a
    member self.Method(a, b) = x + y + z + a + b
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

let inline heterogenousAdd(
                           value1 : ^T when (^T or ^U) : (static member (+) : ^T * ^U
                                                               -> ^T), 
                           value2 : ^U) = value1 + value2
"""