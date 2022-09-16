module Fantomas.Core.Tests.InterfaceStaticMethodTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelper

[<Test>]
let ``static member in constraint`` () =
    formatSourceString
        false
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
