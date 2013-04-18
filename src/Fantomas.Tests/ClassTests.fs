module Fantomas.Tests.ClassTests

open NUnit.Framework
open FsUnit

open Fantomas.CodeFormatter
open Fantomas.Tests.TestHelper

[<Test>]
let ``class signatures``() =
    formatSourceString true """
module Heap

type Heap<'T when 'T : comparison> =
    class
    new : capacity:int -> Heap<'T>
    member Clear : unit -> unit
    member ExtractMin : unit -> 'T
    member Insert : k:'T -> unit
    member IsEmpty : unit -> bool
    member PeekMin : unit -> 'T
    override ToString : unit -> string
    member Count : int
    end""" config
    |> prepend newline
    |> should equal """
module Heap

type Heap<'T when 'T : comparison> = 
    class
        new : capacity:int -> Heap<'T>
        member Clear : unit -> unit
        member ExtractMin : unit -> 'T
        member Insert : k:'T -> unit
        member IsEmpty : unit -> bool
        member PeekMin : unit -> 'T
        override ToString : unit -> string
        member Count : int
    end
"""

[<Test>]
let ``type constraints complex``() =
    formatSourceString false """
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
let ``abstract classes``() =
    formatSourceString false """
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
    let mutable x, y = x0, y0
    let mutable rotAngle = 0.0
    member this.CenterX with get () = x
    member this.CenterX with set xval = x <- xval
    member this.CenterY with get () = y
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
let ``class declaration``() =
    formatSourceString false """
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
let ``recursive classes``() =
    formatSourceString false """
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
    member this.FileArray = 
        Array.map (fun elem -> new File(elem, this)) filenameArray

and File(filename : string, containingFolder : Folder) = 
    member __.Name = filename
    member __.ContainingFolder = containingFolder
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
let ``should keep parens in class definition in the right place``() =
    formatSourceString false """type DGMLClass() = class   
    let mutable currentState = System.String.Empty
    """ config
    |> should equal """type DGMLClass() = 
    class
        let mutable currentState = System.String.Empty
    end
"""

[<Test>]
let ``should keep parens in class inheritance in the right place``() =
    formatSourceString false """type StateMachine(makeAsync) as this = class
    inherit DGMLClass()

    let functions = System.Collections.Generic.Dictionary<string, IState>()
    """ config
    |> should equal """type StateMachine(makeAsync) as this = 
    class
        inherit DGMLClass()
        let functions = System.Collections.Generic.Dictionary<string, IState>()
    end
"""