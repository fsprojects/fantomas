module Fantomas.Tests.UnionsTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper

[<Test>]
let ``enums declaration``() =
    formatSourceString false """
    type FontVariant =
    | [<Description("small-caps")>] SmallCaps = 0""" config
    |> prepend newline
    |> should equal """
type FontVariant =
    | [<Description("small-caps")>] SmallCaps = 0
"""

[<Test>]
let ``discriminated unions declaration``() =
    formatSourceString false "type X = private | A of AParameters | B" config
    |> prepend newline
    |> should equal """
type X =
    private
    | A of AParameters
    | B
"""

[<Test>]
let ``enums conversion``() =
    shouldNotChangeAfterFormat """
type uColor =
    | Red = 0u
    | Green = 1u
    | Blue = 2u

let col3 =
    Microsoft.FSharp.Core.LanguagePrimitives.EnumOfValue<uint32, uColor>(2u)
"""

[<Test>]
let ``discriminated unions with members``() =
    formatSourceString false """
type Type
    = TyLam of Type * Type
    | TyVar of string
    | TyCon of string * Type list
    with override this.ToString() =
            match this with
            | TyLam (t1, t2) -> sprintf "(%s -> %s)" (t1.ToString()) (t2.ToString())
            | TyVar a -> a
            | TyCon (s, ts) -> s""" config
    |> prepend newline
    |> should equal """
type Type =
    | TyLam of Type * Type
    | TyVar of string
    | TyCon of string * Type list
    override this.ToString() =
        match this with
        | TyLam (t1, t2) -> sprintf "(%s -> %s)" (t1.ToString()) (t2.ToString())
        | TyVar a -> a
        | TyCon (s, ts) -> s
"""

[<Test>]
let ``newline between discriminated unions and members``() =
    formatSourceString false """
type Type
    = TyLam of Type * Type
    | TyVar of string
    | TyCon of string * Type list
    with override this.ToString() =
            match this with
            | TyLam (t1, t2) -> sprintf "(%s -> %s)" (t1.ToString()) (t2.ToString())
            | TyVar a -> a
            | TyCon (s, ts) -> s""" ({ config with NewlineBetweenTypeDefinitionAndMembers = true })
    |> prepend newline
    |> should equal """
type Type =
    | TyLam of Type * Type
    | TyVar of string
    | TyCon of string * Type list

    override this.ToString() =
        match this with
        | TyLam (t1, t2) -> sprintf "(%s -> %s)" (t1.ToString()) (t2.ToString())
        | TyVar a -> a
        | TyCon (s, ts) -> s
"""

[<Test>]
let ``should keep attributes on union cases``() =
    formatSourceString false """
type Argument = 
  | [<MandatoryAttribute>] Action of string
  | [<MandatoryAttribute>] ProjectFile of string
  | PackageId of string
  | Version of string""" config
    |> prepend newline
    |> should equal """
type Argument =
    | [<MandatoryAttribute>] Action of string
    | [<MandatoryAttribute>] ProjectFile of string
    | PackageId of string
    | Version of string
"""

[<Test>]
let ``should be able to define named unions``() =
    formatSourceString false """
type Thing =
| Human of Name:string * Age:int
| Cat of Name:string * HoursSleptADay:int

type Strategy =
    | Adaptive
    | Fundamental
    | ShortAR of p:int // F# 3.1 syntax
    | BuyHold""" config
    |> prepend newline
    |> should equal """
type Thing =
    | Human of Name: string * Age: int
    | Cat of Name: string * HoursSleptADay: int

type Strategy =
    | Adaptive
    | Fundamental
    | ShortAR of p: int // F# 3.1 syntax
    | BuyHold
"""

[<Test>]
let ``should be able to pattern match on unions``() =
    formatSourceString false """
type TestUnion = Test of A : int * B : int
[<EntryPoint>]
let main argv =
   let d = Test(B = 1, A = 2)
   match d with
   | Test(A = a; B = b) -> a + b
   | _ -> 0""" config
    |> prepend newline
    |> should equal """
type TestUnion = Test of A: int * B: int

[<EntryPoint>]
let main argv =
    let d = Test(B = 1, A = 2)
    match d with
    | Test (A = a; B = b) -> a + b
    | _ -> 0
"""

[<Test>]
let ``enums conversion with strict mode``() =
    formatSourceString false """
type uColor =
   | Red = 0u
   | Green = 1u
   | Blue = 2u
let col3 = Microsoft.FSharp.Core.LanguagePrimitives.EnumOfValue<uint32, uColor>(2u)""" { config with StrictMode = true }
    |> prepend newline
    |> should equal """
type uColor =
    | Red = 0u
    | Green = 1u
    | Blue = 2u

let col3 =
    Microsoft.FSharp.Core.LanguagePrimitives.EnumOfValue<uint32, uColor>(2u)
"""

[<Test>]
let ``Single case DUs on same line`` () =
    formatSourceString false """
type CustomerId = 
    | CustomerId of int
    """ config
    |> prepend newline
    |> should equal """
type CustomerId = CustomerId of int
"""

[<Test>]
let ``Single case DU with private access modifier`` () =
   formatSourceString false """
type CustomerId =
   private 
   | CustomerId of int
   """ config
   |> prepend newline
   |> should equal """
type CustomerId = private CustomerId of int
"""

[<Test>]
let ``single case DU with member should be on a newline`` () =
    formatSourceString false """
type CustomerId =
    | CustomerId of int
    member this.Test() =
        printfn "%A" this
    """ { config with MaxFunctionBindingWidth = 120 }
    |> prepend newline
    |> should equal """
type CustomerId =
    | CustomerId of int
    member this.Test() = printfn "%A" this
"""

[<Test>]
let ``Generic type style should be respected`` () =
    formatSourceString false """
type 'a Foo = Foo of 'a
    """ config
    |> prepend newline
    |> should equal """
type 'a Foo = Foo of 'a
"""

[<Test>]
let ``Generic multiple param type style should be respected`` () =
    formatSourceString false """
type ('a, 'b) Foo = Foo of 'a
    """ config
    |> prepend newline
    |> should equal """
type ('a, 'b) Foo = Foo of 'a
"""

[<Test>]
let ``preserve pipe after access modified, 561`` () =
    formatSourceString false """type Foo = private | Bar""" config
    |> should equal """type Foo = private | Bar
"""

[<Test>]
let ``preserve pipe after access modified in sig file, 561`` () =
    formatSourceString true """namespace meh

type internal Foo = private | Bar
"""  config
    |> should equal """namespace meh

type internal Foo = private | Bar
"""

[<Test>]
let ``preserve pipe when single choice contains attribute, 596`` () =
    formatSourceString false """type [<StringEnum>] [<RequireQualifiedAccess>] PayableFilters =
    | [<CompiledName "statusSelector">] Status
"""  config
    |> prepend newline
    |> should equal """
[<StringEnum>]
[<RequireQualifiedAccess>]
type PayableFilters = | [<CompiledName "statusSelector">] Status
"""

[<Test>]
let ``preserve pipe when single choice contains attribute, sig file`` () =
    formatSourceString true """namespace Meh

type [<StringEnum>] [<RequireQualifiedAccess>] PayableFilters = | [<CompiledName "statusSelector">] Status
"""  config
    |> prepend newline
    |> should equal """
namespace Meh

[<StringEnum>]
[<RequireQualifiedAccess>]
type PayableFilters = | [<CompiledName "statusSelector">] Status
"""

[<Test>]
let ``single case DU with comment above clause, 567`` () =
    formatSourceString false """type 'a MyGenericType =
  ///
  Foo
"""  config
    |> prepend newline
    |> should equal """
type 'a MyGenericType =
    ///
    Foo
"""

[<Test>]
let ``single case DU should keep a pipe after formatting, 641`` () =
    formatSourceString false """type Record = { Name: string }
type DU = | Record
"""  config
    |> prepend newline
    |> should equal """
type Record = { Name: string }
type DU = | Record
"""

[<Test>]
let ``single case DU with fields should not have a pipe after formatting`` () =
    formatSourceString false """type DU = Record of string"""  config
    |> prepend newline
    |> should equal """
type DU = Record of string
"""

[<Test>]
let ``single case DU with private fields should not have a pipe after formatting`` () =
    formatSourceString false """type String50 = private String50 of string"""  config
    |> prepend newline
    |> should equal """
type String50 = private String50 of string
"""

[<Test>]
let ``single case DU, no UnionCaseFields in signature file`` () =
    formatSourceString true """namespace meh

type DU = | Record
"""  config
    |> prepend newline
    |> should equal """
namespace meh

type DU = | Record
"""

[<Test>]
let ``enum with back ticks, 626`` () =
    formatSourceString false """type MyEnum =
  | ``test-one`` = 0
"""  config
    |> prepend newline
    |> should equal """
type MyEnum =
    | ``test-one`` = 0
"""

[<Test>]
let ``enum with back ticks in signature file`` () =
    formatSourceString true """namespace foo

type MyEnum =
  | ``test-one`` = 0
"""  config
    |> prepend newline
    |> should equal """
namespace foo

type MyEnum =
    | ``test-one`` = 0
"""

[<Test>]
let ``discriminated union with back ticks`` () =
    formatSourceString false """type MyEnum =
  | ``test-one`` of int
  | ``test-two`` of string
"""  config
    |> prepend newline
    |> should equal """
type MyEnum =
    | ``test-one`` of int
    | ``test-two`` of string
"""

[<Test>]
let ``discriminated union with back ticks in signature file`` () =
    formatSourceString true """namespace foo
type MyEnum =
  | ``test-one`` of int
  | ``test-two`` of string
"""  config
    |> prepend newline
    |> should equal """
namespace foo

type MyEnum =
    | ``test-one`` of int
    | ``test-two`` of string
"""
