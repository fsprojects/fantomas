module Fantomas.Core.Tests.DallasTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelper

[<Test>]
let ``proof of concept`` () =
    formatSourceString
        false
        """
let a =   1
"""
        config
    |> prepend newline
    |> should
        equal
        """
let a = 1
"""

[<Test>]
let ``named module with let binding`` () =
    formatSourceString
        false
        """
module A.B
let a =   1
"""
        config
    |> prepend newline
    |> should
        equal
        """
module A.B

let a = 1
"""

[<Test>]
let ``basic comment above let binding`` () =
    formatSourceString
        false
        """
let a =  0

// foobar
let b =  1
"""
        config
    |> prepend newline
    |> should
        equal
        """
let a = 0

// foobar
let b = 1
"""

[<Test>]
let ``single open`` () =
    formatSourceString
        false
        """
open  Foo
"""
        config
    |> prepend newline
    |> should
        equal
        """
open Foo
"""

[<Test>]
let ``two opens`` () =
    formatSourceString
        false
        """
open  Foo
open  Bar

let a =  0
"""
        config
    |> prepend newline
    |> should
        equal
        """
open Foo
open Bar

let a = 0
"""

[<Test>]
let ``type alias`` () =
    formatSourceString
        false
        """
type A =   int
"""
        config
    |> prepend newline
    |> should
        equal
        """
type A = int
"""

[<Test>]
let ``function with parameters`` () =
    formatSourceString
        false
        """
let x y z  = 0
"""
        config
    |> prepend newline
    |> should
        equal
        """
let x y z = 0
"""

[<Test>]
let ``ident expr with backticks`` () =
    formatSourceString
        false
        """
let x =  ``y``
"""
        config
    |> prepend newline
    |> should
        equal
        """
let x = ``y``
"""

[<Test>]
let ``horsing around`` () =
    formatSourceString
        false
        """
null
<@@  x @@>
<@  y @>
x :>  int
new T()
new T(t1)
let null =  0
let _ =  0
let () = 0
let 4 = 1
let a (b) = c
let a,b = 0
let a ?b = c
let a ([<Foo>] b) = 0
let (a | b) = 0
let (a & b & c) = 0
let x (a: int) = 0
let (x as y) = 0
let (x :: y) = 0
let X(y = 0) = 0
let (X(y,z)) = 0
let (struct (a,b)) = 0
let [ a; b ] = 0
let <@ a @> = 0
let (:? a) = 0
let { A = a; B.B = b } = 0
"""
        config
    |> prepend newline
    |> should
        equal
        """
null
<@@ x @@>
<@ y @>
x :> int
new T()
new T(t1)
let null = 0
let _ = 0
let () = 0
let 4 = 1
let a (b) = c
let a, b = 0
let a ?b = c
let a ([<Foo>] b) = 0
let (a | b) = 0
let (a & b & c) = 0
let x (a: int) = 0
let (x as y) = 0
let (x :: y) = 0
let X(y = 0) = 0
let (X(y, z)) = 0
let (struct (a, b)) = 0
let [ a; b ] = 0
let <@ a @> = 0
let (:? a) = 0
let { A = a; B.B = b } = 0
"""

[<Test>]
let ``some types`` () =
    formatSourceString
        false
        """
let a (b: (int)) = 0
let x (y : int[,,]) = 0
type t = int -> int
type A = int * int
type B = h / s
let _: struct (int * int) = ()
let a (b: #int) = 0
let a (b: const int) = 0
let a: 't = 0
let b: ^t = 0
let x : int list = 0
let y : int list array = 0
let z : Task<int> = 0
let p : Prefix.Task<int> = 0
"""
        config
    |> prepend newline
    |> should
        equal
        """
let a (b: (int)) = 0
let x (y: int[,,]) = 0
type t = int -> int
type A = int * int
type B = h / s
let _: struct (int * int) = ()
let a (b: #int) = 0
let a (b: const int) = 0
let a: 't = 0
let b: ^t = 0
let x: int list = 0
let y: int list array = 0
let z: Task<int> = 0
let p: Prefix.Task<int> = 0
"""

[<Test>]
let ``attributes`` () =
    formatSourceString
        false
        """
[<Foo>]
[<Bar1;Bar2>]
do ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
[<Foo>]
[<Bar1; Bar2>]
do ()
"""

[<Test>]
let ``some expressions`` () =
    formatSourceString
        false
        """
let a = [| 2;3 |]
let l = [ 4 ; 5 ;6 ]
"""
        config
    |> prepend newline
    |> should
        equal
        """
let a = [| 2; 3 |]
let l = [ 4; 5; 6 ]
"""

[<Test>]
let ``some type definitions`` () =
    formatSourceString
        false
        """
type A = | B = 1 | C = 2
type D = | D of int * e:string
type X
type Y = { a:int ; b:int }
"""
        config
    |> prepend newline
    |> should
        equal
        """
type A =
    | B = 1
    | C = 2

type D = D of int * e: string
type X
type Y = { a: int; b: int }
"""

[<Test>]
let ``explicit test`` () =
    formatSourceString
        false
        """
type Foo 
    /// Foo
    [<Attr>] private () =
    class
        member x.Bar = ()
    end
    member x.Foo = ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
type Foo
    /// Foo
    [<Attr>]
    private () =
    class
        member x.Bar = ()
    end

    member x.Foo = ()
"""

[<Test>]
let ``long implicit ctor`` () =
    formatSourceString
        false
        """
type Foo 
    /// Foo
    [<Attr>] private (x: Looooooooooooooooong, y: Looooooooooooooooong, z: Looooooooooooooooong, a: Looooooooooooooooong, b: Looooooooooooooooong, c) =
    class
        member x.Bar = ()
    end
    member x.Foo = ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
type Foo
    /// Foo
    [<Attr>]
    private
    (
        x: Looooooooooooooooong,
        y: Looooooooooooooooong,
        z: Looooooooooooooooong,
        a: Looooooooooooooooong,
        b: Looooooooooooooooong,
        c
    ) =
    class
        member x.Bar = ()
    end

    member x.Foo = ()
"""

[<Test>]
let ``long implicit ctor, alternative`` () =
    formatSourceString
        false
        """
type Foo 
    /// Foo
    [<Attr>] private (x: Looooooooooooooooong, y: Looooooooooooooooong, z: Looooooooooooooooong, a: Looooooooooooooooong, b: Looooooooooooooooong, c) =
    class
        member x.Bar = ()
    end
    member x.Foo = ()
"""
        { config with AlternativeLongMemberDefinitions = true }
    |> prepend newline
    |> should
        equal
        """
type Foo
    /// Foo
    [<Attr>]
    private
    (
        x: Looooooooooooooooong,
        y: Looooooooooooooooong,
        z: Looooooooooooooooong,
        a: Looooooooooooooooong,
        b: Looooooooooooooooong,
        c
    )
    =
    class
        member x.Bar = ()
    end

    member x.Foo = ()
"""

[<Test>]
let ``trivia inside implicit constructor`` () =
    formatSourceString
        false
        """
  type MyType
    (
      (* some comment *)
    ) = 
    class end
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
    class
    end
"""

[<Test>]
let ``inherit record`` () =
    formatSourceString
        false
        """
{ inherit   X  }
"""
        config
    |> prepend newline
    |> should
        equal
        """
{ inherit X }
"""

[<Test>]
let ``moar records`` () =
    formatSourceString
        false
        """
{ X = y; Z = 1 }
{ x with Y = 0 }
"""
        config
    |> prepend newline
    |> should
        equal
        """
{ X = y; Z = 1 }
{ x with Y = 0 }
"""

[<Test>]
let ``augmentation type`` () =
    formatSourceString
        false
        """
type Foo with
    member x.Bar = ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
type Foo with

    member x.Bar = ()
"""

[<Test>]
let ``long delegate stuff`` () =
    formatSourceString
        false
        """
type X =
    delegate of
            VreeeeeeeeeeeeeeLaaaaaaaaaaaaaaanngggType *
                    Fooooooooooooooooooooooooooooo ->
                        A ->
                        B * C ->
                            Y ->
                                        X

type A = int
"""
        { config with MaxLineLength = 60 }
    |> prepend newline
    |> should
        equal
        """
type X =
    delegate of
        VreeeeeeeeeeeeeeLaaaaaaaaaaaaaaanngggType *
        Fooooooooooooooooooooooooooooo ->
            A ->
            B * C ->
                Y ->
                    X

type A = int
"""

[<Test>]
let ``unspecified type with`` () =
    formatSourceString
        false
        """
type A() =
    member x.B () = ()
    with
        member x.C () = ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
type A() =
    member x.B () = ()
    member x.C () = ()
"""

[<Test>]
let ``xx dfe`` () =
    formatSourceString
        false
        """
type A() =
    class
        member x.B () = ()
    end
"""
        config
    |> prepend newline
    |> should
        equal
        """
type A() =
    class
        member x.B () = ()
    end
"""

[<Test>]
let ``implicit inherit member`` () =
    formatSourceString
        false
        """
type A() =
    inherit B(
        x
    )
"""
        config
    |> prepend newline
    |> should
        equal
        """
type A() =
    inherit B(x)
"""

[<Test>]
let ``val field`` () =
    formatSourceString
        false
        """
type A =
    val X: int
"""
        config
    |> prepend newline
    |> should
        equal
        """
type A =
    val X: int
"""

[<Test>]
let ``do expr in member`` () =
    formatSourceString
        false
        """
type A =
    do
        // x
        y
"""
        config
    |> prepend newline
    |> should
        equal
        """
type A =
    do
        // x
        y
"""

[<Test>]
let ``let bindings in type`` () =
    formatSourceString
        false
        """
type A =
    let b x = 0
    and c y = 1
"""
        config
    |> prepend newline
    |> should
        equal
        """
type A =
    let b x = 0
    and c y = 1
"""

[<Test>]
let ``second ctor`` () =
    formatSourceString
        false
        """
type StateMachine() =
    new() as secondCtor =
        new StateMachine()
            then
                        ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
type StateMachine() =
    new() as secondCtor =
        new StateMachine()
        then ()
"""

[<Test>]
let ``long prop get`` () =
    formatSourceString
        false
        """
type StateMachine() =
    member x.X with get() = y
"""
        config
    |> prepend newline
    |> should
        equal
        """
type StateMachine() =
    member x.X = y
"""

[<Test>]
let ``interface member`` () =
    formatSourceString
        false
        """
type Y =
    interface Z with
        member x.A = ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
type Y =
    interface Z with
        member x.A = ()
"""

[<Test>]
let ``auto property`` () =
    formatSourceString
        false
        """
type X =
    member val Y: int = 0
"""
        config
    |> prepend newline
    |> should
        equal
        """
type X =
    member val Y: int = 0
"""

[<Test>]
let ``auto property get/set`` () =
    formatSourceString
        false
        """
type X(y) =
    member val Y = y with get,set 
"""
        config
    |> prepend newline
    |> should
        equal
        """
type X(y) =
    member val Y = y with get, set
"""

[<Test>]
let ``abstract slot`` () =
    formatSourceString
        false
        """
type X =
    abstract Y : int
    abstract member Z : int
"""
        config
    |> prepend newline
    |> should
        equal
        """
type X =
    abstract Y: int
    abstract member Z: int
"""

[<Test>]
let ``get / set`` () =
    formatSourceString
        false
        """
type X =
    member this.Y 
        with get() = y
        and set(newY) = ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
type X =
    member this.Y
        with get () = y
        and set (newY) = ()
"""

[<Test>]
let ``set with two parameters`` () =
    formatSourceString
        false
        """
type X =
    member this.Item
            with get (name: string): obj option = None
            and set (name: string) (v: obj option): unit = ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
type X =
    member this.Item
        with get (name: string): obj option = None
        and set (name: string) (v: obj option): unit = ()
"""

[<Test>]
let ``tuple`` () =
    formatSourceString
        false
        """
x,y,z
a, //
b, c
"""
        config
    |> prepend newline
    |> should
        equal
        """
x, y, z

a, //
b,
c
"""

[<Test>]
let ``struct tuple`` () =
    formatSourceString
        false
        """
struct (x,y,z)
"""
        config
    |> prepend newline
    |> should
        equal
        """
struct (x, y, z)
"""

[<Test>]
let ``anon record expr`` () =
    formatSourceString
        false
        """
{| 
    x = y
    a = //
        b
|}
"""
        config
    |> prepend newline
    |> should
        equal
        """
{| x = y
   a = //
    b |}
"""

[<Test>]
let ``obj expr`` () =
    formatSourceString
        false
        """
{   new IDisposable
    interface Meh with
        member x.Blur = () }
"""
        config
    |> prepend newline
    |> should
        equal
        """
{ new IDisposable

  interface Meh with
      member x.Blur = () }
"""

[<Test>]
let ``while expr`` () =
    formatSourceString
        false
        """
while a do b
"""
        config
    |> prepend newline
    |> should
        equal
        """
while a do
    b
"""

[<Test>]
let ``for expr`` () =
    formatSourceString
        false
        """
for i = 0 to 10 do 
    y
"""
        config
    |> prepend newline
    |> should
        equal
        """
for i = 0 to 10 do
    y
"""

[<Test>]
let ``foreach expr`` () =
    formatSourceString
        false
        """
for x in y do 
    ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
for x in y do
    ()
"""
