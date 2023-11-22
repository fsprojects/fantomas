module Fantomas.Core.Tests.DallasTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelpers
open Fantomas.Core

[<Test>]
let ``proof of concept`` () =
    formatSourceString
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
        """
type Foo 
    /// Foo
    [<Attr>] private (x: Looooooooooooooooong, y: Looooooooooooooooong, z: Looooooooooooooooong, a: Looooooooooooooooong, b: Looooooooooooooooong, c) =
    class
        member x.Bar = ()
    end
    member x.Foo = ()
"""
        { config with
            AlternativeLongMemberDefinitions = true }
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
    ) = class end
"""

[<Test>]
let ``inherit record`` () =
    formatSourceString
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
    member x.B() = ()
    member x.C() = ()
"""

[<Test>]
let ``xx dfe`` () =
    formatSourceString
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
        member x.B() = ()
    end
"""

[<Test>]
let ``implicit inherit member`` () =
    formatSourceString
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
        """
type A =
    let rec b x = 0
    and c y = 1
"""
        config
    |> prepend newline
    |> should
        equal
        """
type A =
    let rec b x = 0
    and c y = 1
"""

[<Test>]
let ``second ctor`` () =
    formatSourceString
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

[<Test>]
let ``named computation expr`` () =
    formatSourceString
        """
async { x  } 
"""
        config
    |> prepend newline
    |> should
        equal
        """
async { x }
"""

[<Test>]
let ``let bang in comp expr`` () =
    formatSourceString
        """
async {
    let! x = 0
    and! blah = 0
    ()
}
"""
        config
    |> prepend newline
    |> should
        equal
        """
async {
    let! x = 0
    and! blah = 0
    ()
}
"""

[<Test>]
let ``nested let or use`` () =
    formatSourceString
        """
do
    let x = 1
    let y = 2
    ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
do
    let x = 1
    let y = 2
    ()
"""

[<Test>]
let ``join in`` () =
    formatSourceString
        """
seq { x    
            in //    
                y }
"""
        config
    |> prepend newline
    |> should
        equal
        """
seq {
    x in y //
}
"""

[<Test>]
let ``paren lambda`` () =
    formatSourceString
        """
(fun _ -> //
                     a)
"""
        config
    |> prepend newline
    |> should
        equal
        """
(fun _ -> //
    a)
"""

[<Test>]
let ``paren with closing lambda`` () =
    formatSourceString
        """
(fun _ -> //
                     a)
"""
        { config with
            MultiLineLambdaClosingNewline = true }
    |> prepend newline
    |> should
        equal
        """
(fun _ -> //
    a
)
"""

[<Test>]
let ``paren lambda, long list of parameters`` () =
    formatSourceString
        """
(fun a b c d e f 
        // comment
        g -> 
        //
        ()
        //
        )
"""
        { config with
            MultiLineLambdaClosingNewline = true }
    |> prepend newline
    |> should
        equal
        """
(fun
    a
    b
    c
    d
    e
    f
    // comment
    g ->
    //
    ()
//
)
"""

[<Test>]
let ``single lambda`` () =
    formatSourceString
        """
fun a b c -> d
"""
        config
    |> prepend newline
    |> should
        equal
        """
fun a b c -> d
"""

[<Test>]
let ``match lambda`` () =
    formatSourceString
        """
function 
| X -> X
| Y -> y
"""
        config
    |> prepend newline
    |> should
        equal
        """
function
| X -> X
| Y -> y
"""

[<Test>]
let ``nested or pattern in clause`` () =
    formatSourceString
        """
function 
| X
| Y
| Z -> ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
function
| X
| Y
| Z -> ()
"""

[<Test>]
let ``nested or in alias pattern`` () =
    formatSourceString
        """
function 
| X _
| Y _
| Z _ as meh -> ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
function
| X _
| Y _
| Z _ as meh -> ()
"""

[<Test>]
let ``when expr in clause`` () =
    formatSourceString
        """
function 
| X _ when someBoolThing ->
    // comment
    ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
function
| X _ when someBoolThing ->
    // comment
    ()
"""

[<Test>]
let ``match expr`` () =
    formatSourceString
        """
match x with
| Y x -> ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
match x with
| Y x -> ()
"""

[<Test>]
let ``short infix operator`` () =
    formatSourceString
        """
1 +  1
"""
        config
    |> prepend newline
    |> should
        equal
        """
1 + 1
"""

[<Test>]
let ``multiline same infix operator`` () =
    formatSourceString
        """
x +>
    y +>
        z +>
            a0
"""
        { config with
            MaxInfixOperatorExpression = 0 }
    |> prepend newline
    |> should
        equal
        """
x
+> y
+> z
+> a0
"""

[<Test>]
let ``index without dot`` () =
    formatSourceString
        """
a[ b ]
"""
        config
    |> prepend newline
    |> should
        equal
        """
a[b]
"""

[<Test>]
let ``if then`` () =
    formatSourceString
        """
if a then  b
"""
        config
    |> prepend newline
    |> should
        equal
        """
if a then
    b
"""

[<Test>]
let ``long ident expr`` () =
    formatSourceString
        """
(++)
?a.b
?c
"""
        config
    |> prepend newline
    |> should
        equal
        """
(++)
?a.b
?c
"""

[<Test>]
let ``dotNamedIndexedPropertySet`` () =
    formatSourceString
        """
(a).b() <- c
"""
        config
    |> prepend newline
    |> should
        equal
        """
(a).b() <- c
"""

[<Test>]
let ``interpolated string`` () =
    formatSourceString
        """
$"{x}y{z}"
"""
        config
    |> prepend newline
    |> should
        equal
        """
$"{x}y{z}"
"""

[<Test>]
let ``triple zero`` () =
    formatSourceString
        """
[ 0.   ..   0. ..  0. ]
"""
        config
    |> prepend newline
    |> should
        equal
        """
[ 0. .. 0. .. 0. ]
"""

[<Test>]
let ``AppDotGetTypeApp`` () =
    formatSourceString
        """
Result<int, string>.Ok 42
"""
        config
    |> prepend newline
    |> should
        equal
        """
Result<int, string>.Ok 42
"""

[<Test>]
let ``temp`` () =
    formatSourceString
        """
A.B<string>
"""
        config
    |> prepend newline
    |> should
        equal
        """
A.B<string>
"""

[<Test>]
let ``DotGetAppDotGetAppParenLambda`` () =
    formatSourceString
        """
Foo(fun x -> x).Bar().Meh
"""
        config
    |> prepend newline
    |> should
        equal
        """
Foo(fun x -> x).Bar().Meh
"""

[<Test>]
let ``DotGetAppDotGetAppParenLambda, type app`` () =
    formatSourceString
        """
Hej.Barry.Foo<a,b>(fun x -> x).Bar().Meh
"""
        config
    |> prepend newline
    |> should
        equal
        """
Hej.Barry.Foo<a, b>(fun x -> x).Bar().Meh
"""

[<Test>]
let ``DotGetAppParen`` () =
    formatSourceString
        """
Foo().Bar
"""
        config
    |> prepend newline
    |> should
        equal
        """
Foo().Bar
"""

[<Test>]
let ``app tests`` () =
    formatSourceString
        """
fn a b
UpperFn(x)
lowerFn(y)
"""
        config
    |> prepend newline
    |> should
        equal
        """
fn a b
UpperFn(x)
lowerFn (y)
"""

[<Test>]
let ``union type in signature`` () =
    formatSignatureString
        """
namespace X

module Y =
    type A =
        | B of int
        | C of string

    type D = E
"""
        config
    |> prepend newline
    |> should
        equal
        """
namespace X

module Y =
    type A =
        | B of int
        | C of string

    type D = E
"""

[<Test>]
let ``app single paren arg`` () =
    formatSourceString
        """
a(
    //
    b,
    c)
    
fn (fun x ->
    // foo
    ()
)
"""
        config
    |> prepend newline
    |> should
        equal
        """
a (
    //
    b,
    c
)

fn (fun x ->
    // foo
    ())
"""

[<Test>]
let ``app single paren arg + fsharp_multi_line_lambda_closing_newline `` () =
    formatSourceString
        """
fn (fun x ->
    // foo
    ())
"""
        { config with
            MultiLineLambdaClosingNewline = true }
    |> prepend newline
    |> should
        equal
        """
fn (fun x ->
    // foo
    ()
)
"""

[<Test>]
let ``generic arg in function`` () =
    formatSourceString
        """
let a<'t> b c = ()
let x<'t when 't :> null> b c = ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
let a<'t> b c = ()
let x<'t when 't :> null> b c = ()
"""

[<Test>]
let ``ExprDotGetAppWithParenLambdaNode tests`` () =
    formatSourceString
        """
A.B(fun x -> x).x<s>()
Foo(fun x -> x).Bar()
"""
        config
    |> prepend newline
    |> should
        equal
        """
A.B(fun x -> x).x<s> ()
Foo(fun x -> x).Bar()
"""

[<Test>]
let ``DotGetApp test`` () =
    formatSourceString
        """
 Foo().Bar().Meh()
"""
        config
    |> prepend newline
    |> should
        equal
        """
Foo().Bar().Meh()
"""

[<Test>]
let ``two let bindings followed by a lambda`` () =
    formatSourceString
        """
let shortExpr = genExpr e +> genSynLongIdent true sli
let longExpr = genExpr e +> indentSepNlnUnindent (genSynLongIdentMultiline true sli)

fun ctx ->
    isShortExpression
        ctx.Config.MaxDotGetExpressionWidth
        shortExpr
        longExpr
        ctx
"""
        config
    |> prepend newline
    |> should
        equal
        """
let shortExpr = genExpr e +> genSynLongIdent true sli
let longExpr = genExpr e +> indentSepNlnUnindent (genSynLongIdentMultiline true sli)

fun ctx -> isShortExpression ctx.Config.MaxDotGetExpressionWidth shortExpr longExpr ctx
"""

[<Test>]
let ``multiple always break infix operators`` () =
    formatSourceString
        """
(sli.Dots, tail)
||> List.zip
|> List.collect (fun (dot, ident) ->
    [ IdentifierOrDot.KnownDot(stn "." dot)
      IdentifierOrDot.Ident(mkSynIdent ident) ])
"""
        config
    |> prepend newline
    |> should
        equal
        """
(sli.Dots, tail)
||> List.zip
|> List.collect (fun (dot, ident) ->
    [ IdentifierOrDot.KnownDot(stn "." dot)
      IdentifierOrDot.Ident(mkSynIdent ident) ])
"""

[<Test>]
let ``value binding with multiline type annotation`` () =
    formatSourceString
        """
let autoCompleteItems: cmap<DeclName, DeclarationListItem * Position * string<LocalPath> * (Position -> option<string>) * FSharp.Compiler.Syntax.ParsedInput> =
    cmap ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
let autoCompleteItems
    : cmap<DeclName, DeclarationListItem *
      Position *
      string<LocalPath> *
      (Position -> option<string>) *
      FSharp.Compiler.Syntax.ParsedInput> =
    cmap ()
"""

[<Test>]
let ``recursive type with implicit constructor and attributes`` () =
    formatSourceString
        """
type x
and [<Sealed>] MapDebugView<'Key, 'Value when 'Key: comparison>(v: Map<'Key, 'Value>) =

    [<DebuggerBrowsable(DebuggerBrowsableState.RootHidden)>]
    member x.Items =
        v |> Seq.truncate 10000 |> Seq.map KeyValuePairDebugFriendly |> Seq.toArray
"""
        config
    |> prepend newline
    |> should
        equal
        """
type x

and [<Sealed>] MapDebugView<'Key, 'Value when 'Key: comparison>(v: Map<'Key, 'Value>) =

    [<DebuggerBrowsable(DebuggerBrowsableState.RootHidden)>]
    member x.Items =
        v |> Seq.truncate 10000 |> Seq.map KeyValuePairDebugFriendly |> Seq.toArray
"""

[<Test>]
let ``long ident app type`` () =
    formatSourceString
        """
let create size : ImmutableArray<'T>.Builder = ImmutableArray.CreateBuilder(size)
"""
        config
    |> prepend newline
    |> should
        equal
        """
let create size : ImmutableArray<'T>.Builder = ImmutableArray.CreateBuilder(size)
"""

[<Test>]
let ``type with implicit constructor and generic type constraints`` () =
    formatSourceString
        """
[<System.Diagnostics.DebuggerDisplay "Count = {Count}">]
[<Sealed>]
type internal Set<'T, 'ComparerTag> when 'ComparerTag :> IComparer<'T>(comparer: IComparer<'T>, tree: SetTree<'T>) =

    static let refresh (s: Set<_, _>) t =
        Set<_, _>(comparer = s.Comparer, tree = t)
"""
        config
    |> prepend newline
    |> should
        equal
        """
[<System.Diagnostics.DebuggerDisplay "Count = {Count}">]
[<Sealed>]
type internal Set<'T, 'ComparerTag> when 'ComparerTag :> IComparer<'T>(comparer: IComparer<'T>, tree: SetTree<'T>) =

    static let refresh (s: Set<_, _>) t =
        Set<_, _>(comparer = s.Comparer, tree = t)
"""

[<Test>]
let ``comment before inherit member definition`` () =
    formatSourceString
        """
type ILModuleReader =
    abstract ILModuleDef: ILModuleDef
    abstract ILAssemblyRefs: ILAssemblyRef list

    // ILModuleReader objects only need to be explicitly disposed if memory mapping is used, i.e. reduceMemoryUsage = false
    inherit IDisposable
"""
        config
    |> prepend newline
    |> should
        equal
        """
type ILModuleReader =
    abstract ILModuleDef: ILModuleDef
    abstract ILAssemblyRefs: ILAssemblyRef list

    // ILModuleReader objects only need to be explicitly disposed if memory mapping is used, i.e. reduceMemoryUsage = false
    inherit IDisposable
"""

[<Test>]
let ``multiline infix operator with three let in bindings`` () =
    formatSourceString
        """
Ok
<| let a = 1 in
   let b = 2 in
   let c = 3 in
   ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
Ok
<| let a = 1 in
   let b = 2 in
   let c = 3 in
   ()
"""

[<Test>]
let ``type alias with trivia`` () =
    formatSourceString
        """
(* 1 *) type (* 2 *) A (* 3 *) = (* 4 *) int (* 5 *)
"""
        config
    |> prepend newline
    |> should
        equal
        """
(* 1 *) type (* 2 *) A (* 3 *) = (* 4 *) int (* 5 *)
"""

[<Test>]
let ``trivia before equals in multiline implicit constructor`` () =
    formatSourceString
        """
type TypeDefnUnionNode
    (
        typeNameNode,
        accessibility: SingleTextNode option,
        unionCases: UnionCaseNode list,
        members: MemberDefn list,
        range
    )

 =
    inherit NodeBase(range)
"""
        config
    |> prepend newline
    |> should
        equal
        """
type TypeDefnUnionNode
    (typeNameNode, accessibility: SingleTextNode option, unionCases: UnionCaseNode list, members: MemberDefn list, range)

    =
    inherit NodeBase(range)
"""

[<Test>]
let ``comment above single parameter application, 2594`` () =
    formatSourceString
        """
let test1 param =
    doSomething
        // my comment
        (param)
"""
        config
    |> prepend newline
    |> should
        equal
        """
let test1 param =
    doSomething
        // my comment
        (param)
"""

[<Test>]
let ``comment above multiline single parentheses application`` () =
    formatSourceString
        """
myFunction
    // my comment
    (arg1,
     arg2,
     // another comment
     arg3)
"""
        config
    |> prepend newline
    |> should
        equal
        """
myFunction
    // my comment
    (
        arg1,
        arg2,
        // another comment
        arg3
    )
"""

[<Test>]
let ``block comment after pattern constant`` () =
    formatSourceString
        """
match tag with
            | 0 (* None *)  -> getInstancePropertyInfos (typ, [||], bindingFlags)
            | 1 (* Some *)  -> getInstancePropertyInfos (typ, [| "Value" |], bindingFlags)
            | _ -> failwith "fieldsPropsOfUnionCase"
"""
        config
    |> prepend newline
    |> should
        equal
        """
match tag with
| 0 (* None *) -> getInstancePropertyInfos (typ, [||], bindingFlags)
| 1 (* Some *) -> getInstancePropertyInfos (typ, [| "Value" |], bindingFlags)
| _ -> failwith "fieldsPropsOfUnionCase"
"""

[<Test>]
let ``mutable private value, 2646`` () =
    formatSourceString
        """
let mutable private myMutable = 5
"""
        config
    |> prepend newline
    |> should
        equal
        """
let mutable private myMutable = 5
"""

[<Test>]
let ``named computation expression with an argument and stroustrup, 2648`` () =
    formatSourceString
        """
let someTest input1 input2 =
    test "This can contain a quite long description of what the test exactly does and why it exists" {
        Expect.equal input1 input2 "didn't equal"
    }
"""
        { config with
            MultilineBracketStyle = Stroustrup }
    |> prepend newline
    |> should
        equal
        """
let someTest input1 input2 =
    test "This can contain a quite long description of what the test exactly does and why it exists" {
        Expect.equal input1 input2 "didn't equal"
    }
"""

[<Test>]
let ``comments after chained dotgetapp, 2649`` () =
    formatSourceString
        """
app
    .UseX("X") // Comment.
    .UseY("X") // Comment.
    .UseZ("X") // Comment.

app
    .UseX(x) // Comment.
    .UseY() // Comment.
    .UseZ() // Comment.
"""
        config
    |> prepend newline
    |> should
        equal
        """
app
    .UseX("X") // Comment.
    .UseY("X") // Comment.
    .UseZ("X") // Comment.

app
    .UseX(x) // Comment.
    .UseY() // Comment.
    .UseZ() // Comment.
"""

[<Test>]
let ``lambda as right-hand-side of infix application, 2650`` () =
    formatSourceString
        """
let answerToUniverse =
    question
    |> fun value ->
        TransformersModule.tryTransformToAnswerToUniverse value
        |> Option.defaultValue 42
"""
        config
    |> prepend newline
    |> should
        equal
        """
let answerToUniverse =
    question
    |> fun value ->
        TransformersModule.tryTransformToAnswerToUniverse value
        |> Option.defaultValue 42
"""
