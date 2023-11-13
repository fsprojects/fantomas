module Fantomas.Core.Tests.InterfaceStaticMethodTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelpers

[<Test>]
let ``static member in constraint`` () =
    formatSourceString
        """
// Check that "property" and "get_ method" constraints are considered logically equivalent
let inline f_StaticProperty<'T when 'T : (static member StaticProperty: int) >() : int = 'T.StaticProperty

let inline f_StaticMethod<'T when 'T : (static member StaticMethod: int -> int) >() : int = 'T.StaticMethod(3)

let inline f_set_StaticProperty<'T when 'T : (static member StaticProperty: int with set) >() = 'T.set_StaticProperty(3)

let inline f_InstanceMethod<'T when 'T : (member InstanceMethod: int -> int) >(x: 'T) : int = x.InstanceMethod(3)

let inline f_Length<'T when 'T : (member Length: int) >(x: 'T) = x.Length

let inline f_set_Length<'T when 'T : (member Length: int with set) >(x: 'T) = x.set_Length(3)

let inline f_Item<'T when 'T : (member Item: int -> string with get) >(x: 'T) = x.get_Item(3)
"""
        config
    |> prepend newline
    |> should
        equal
        """
// Check that "property" and "get_ method" constraints are considered logically equivalent
let inline f_StaticProperty<'T when 'T: (static member StaticProperty: int)> () : int = 'T.StaticProperty

let inline f_StaticMethod<'T when 'T: (static member StaticMethod: int -> int)> () : int = 'T.StaticMethod(3)

let inline f_set_StaticProperty<'T when 'T: (static member StaticProperty: int with set)> () = 'T.set_StaticProperty (3)

let inline f_InstanceMethod<'T when 'T: (member InstanceMethod: int -> int)> (x: 'T) : int = x.InstanceMethod(3)

let inline f_Length<'T when 'T: (member Length: int)> (x: 'T) = x.Length

let inline f_set_Length<'T when 'T: (member Length: int with set)> (x: 'T) = x.set_Length (3)

let inline f_Item<'T when 'T: (member Item: int -> string with get)> (x: 'T) = x.get_Item (3)
"""

[<Test>]
let ``self constrained type parameter`` () =
    formatSourceString
        """
open System
open Types

module CheckSelfConstrainedIWSAM =

    let f_IWSAM_explicit_operator_name<'T when IAdditionOperator<'T>>(x: 'T, y: 'T) =
        'T.op_Addition(x, y)

    let f_IWSAM_pretty_operator_name<'T when IAdditionOperator<'T>>(x: 'T, y: 'T) =
        'T.(+)(x, y)

    let f_IWSAM_StaticProperty<'T when IStaticProperty<'T>>() =
        'T.StaticProperty

    let f_IWSAM_declared_StaticMethod<'T when IStaticMethod<'T>>(x: 'T) =
        'T.StaticMethod(x)

    let f_IWSAM_declared_UnitMethod<'T when IUnitMethod<'T>>() =
        'T.UnitMethod()

    let f_IWSAM_declared_UnitMethod_list<'T when IUnitMethod<'T>>() =
        let v = 'T.UnitMethod()
        [ v ]

    let inline f3<'T when IAdditionOperator<'T>>(x: 'T, y: 'T) =
        'T.op_Addition(x,y)

    type WithStaticProperty<'T when 'T : (static member StaticProperty: int)> = 'T
    type WithStaticMethod<'T when 'T : (static member StaticMethod: int -> int)> = 'T
    type WithBoth<'T when WithStaticProperty<'T> and WithStaticMethod<'T>> = 'T

    let inline f_StaticProperty<'T when WithStaticProperty<'T>>() = 'T.StaticProperty
    let inline f_StaticMethod<'T when WithStaticMethod<'T>>() = 'T.StaticMethod(3)
    let inline f_Both<'T when WithBoth<'T> >() =
        let v1 = 'T.StaticProperty
        let v2 = 'T.StaticMethod(3)
        v1 + v2

    let inline f_OK1<'T when WithBoth<'T>>() =
        'T.StaticMethod(3) |> ignore
        'T.StaticMethod(3)

    let inline f_OK2<'T when WithBoth<'T>>() =
        'T.StaticMethod(3) |> ignore
        'T.StaticMethod(3)

    let inline f_OK3<'T when WithBoth<'T>>() =
        printfn ""
        'T.StaticMethod(3)

    printfn ""
"""
        config
    |> prepend newline
    |> should
        equal
        """
open System
open Types

module CheckSelfConstrainedIWSAM =

    let f_IWSAM_explicit_operator_name<'T when IAdditionOperator<'T>> (x: 'T, y: 'T) = 'T.op_Addition (x, y)

    let f_IWSAM_pretty_operator_name<'T when IAdditionOperator<'T>> (x: 'T, y: 'T) = 'T.(+) (x, y)

    let f_IWSAM_StaticProperty<'T when IStaticProperty<'T>> () = 'T.StaticProperty

    let f_IWSAM_declared_StaticMethod<'T when IStaticMethod<'T>> (x: 'T) = 'T.StaticMethod(x)

    let f_IWSAM_declared_UnitMethod<'T when IUnitMethod<'T>> () = 'T.UnitMethod()

    let f_IWSAM_declared_UnitMethod_list<'T when IUnitMethod<'T>> () =
        let v = 'T.UnitMethod()
        [ v ]

    let inline f3<'T when IAdditionOperator<'T>> (x: 'T, y: 'T) = 'T.op_Addition (x, y)

    type WithStaticProperty<'T when 'T: (static member StaticProperty: int)> = 'T
    type WithStaticMethod<'T when 'T: (static member StaticMethod: int -> int)> = 'T
    type WithBoth<'T when WithStaticProperty<'T> and WithStaticMethod<'T>> = 'T

    let inline f_StaticProperty<'T when WithStaticProperty<'T>> () = 'T.StaticProperty
    let inline f_StaticMethod<'T when WithStaticMethod<'T>> () = 'T.StaticMethod(3)

    let inline f_Both<'T when WithBoth<'T>> () =
        let v1 = 'T.StaticProperty
        let v2 = 'T.StaticMethod(3)
        v1 + v2

    let inline f_OK1<'T when WithBoth<'T>> () =
        'T.StaticMethod(3) |> ignore
        'T.StaticMethod(3)

    let inline f_OK2<'T when WithBoth<'T>> () =
        'T.StaticMethod(3) |> ignore
        'T.StaticMethod(3)

    let inline f_OK3<'T when WithBoth<'T>> () =
        printfn ""
        'T.StaticMethod(3)

    printfn ""
"""
