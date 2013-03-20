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
    abstract Print : unit -> unit

type SomeClass1(x : int, y : float) = 
    interface IPrintable with
        member this.Print() = printfn "%d %f" x y

type Interface3 = 
    inherit Interface1
    inherit Interface2
    abstract Method3 : int -> int
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

[<Test>]
let ``class declaration``() =
    formatSourceString """
type BaseClass = class
    val string1 : string
    new (str) = { string1 = str }
    new () = { string1 = "" }
end

type DerivedClass =
    inherit BaseClass
    val string2 : string
    new (str1, str2) = { inherit BaseClass(str1); string2 = str2 }
    new (str2) = { inherit BaseClass(); string2 = str2 }""" config
    |> prepend newline
    |> should equal """
type BaseClass = 
    class
        val string1 : string
        new(str) = { string1 = str }
        new() = { string1 = "" }
    end

type DerivedClass = 
    inherit BaseClass
    val string2 : string
    new(str1, str2) = { string2 = str2 }
    new(str2) = { string2 = str2 }
"""

[<Test>]
let ``struct declaration``() =
    formatSourceString """
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
        new(x : float, y : float) = { X = x; Y = y }
    end
"""

[<Test>]
let ``abstract and override keywords``() =
    formatSourceString """
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
    formatSourceString """
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
    formatSourceString """
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
    formatSourceString """
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
    formatSourceString """
type Derived1() =
   inherit AbstractBase()
   let mutable value = 10 
   override this.Property1 with get() = value and set(v : int) = value <- v""" config
    |> prepend newline
    |> should equal """
type Derived1() = 
    inherit AbstractBase()
    let mutable value = 10
    override this.Property1 = value
    override this.Property1 with set (v : int) = value <- v
"""

[<Test>]
let ``types with attributes``() =
    formatSourceString """
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
    formatSourceString """
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
    formatSourceString """
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
    member self.Prop2 = z
    member self.Prop2 with set (a) = z <- a
    member self.Method(a, b) = x + y + z + a + b
"""

[<Test>]
let ``optional arguments``() =
    formatSourceString """
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
    formatSourceString """
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
let ``abstract classes``() =
    formatSourceString """
[<AbstractClass>]
type Shape2D(x0 : float, y0 : float) =
    let mutable x, y = x0, y0
    let mutable rotAngle = 0.0

    member this.CenterX with get() = x and set xval = x <- xval
    member this.CenterY with get() = y and set yval = y <- yval

    abstract Area : float with get
    abstract Perimeter : float  with get
    abstract Name : string with get

    member this.Move dx dy =
       x <- x + dx
       y <- y + dy

    abstract member Rotate: float -> unit
    default this.Rotate(angle) = rotAngle <- rotAngle + angle
    """ config
    |> prepend newline
    |> should equal """
[<AbstractClass>]
type Shape2D(x0 : float, y0 : float) = 
    let mutable x, y = (x0, y0)
    let mutable rotAngle = 0.0
    member this.CenterX = x
    member this.CenterX with set xval = x <- xval
    member this.CenterY = y
    member this.CenterY with set yval = y <- yval
    abstract Area : float with get
    abstract Perimeter : float with get
    abstract Name : string with get
    member this.Move dx dy = 
        x <- x + dx
        y <- y + dy
    abstract Rotate : float -> unit
    override this.Rotate(angle) = rotAngle <- rotAngle + angle
"""

[<Test>]
let ``type constraints simple``() =
    formatSourceString """
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
let ``type constraints complex``() =
    formatSourceString """
type Class4<'T when 'T : (static member staticMethod1 : unit -> 'T) > =
    class end

type Class5<'T when 'T : (member Method1 : 'T -> int)> =
    class end

type Class6<'T when 'T : (member Property1 : int)> =
    class end

type Class7<'T when 'T : (new : unit -> 'T)>() =
   member val Field = new 'T()
    """ config
    |> prepend newline
    |> should equal """
type Class4<'T when 'T : (static member staticMethod1 : unit -> 'T)> = 
    class
    end

type Class5<'T when 'T : (member Method1 : 'T -> int)> = 
    class
    end

type Class6<'T when 'T : (member Property1 : int)> = 
    class
    end

type Class7<'T when 'T : (new : unit -> 'T)>() = 
    member val Field = new 'T()
"""

[<Test>]
let ``type constraints and inline``() =
    formatSourceString """
let inline add(value1 : ^T when ^T : (static member (+) : ^T * ^T -> ^T), value2: ^T) =
    value1 + value2

let inline heterogenousAdd(value1 : ^T when (^T or ^U) : (static member (+) : ^T * ^U -> ^T), value2 : ^U) =
    value1 + value2""" config
    |> prepend newline
    |> should equal """
let inline add(value1 : ^T when ^T : (static member (+) : ^T * ^T -> ^T), value2 : ^T) = value1 + value2

let inline heterogenousAdd(value1 : ^T when (^T or ^U) : (static member (+) : ^T * ^U -> ^T), value2 : ^U) = value1 + value2
"""