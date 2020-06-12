module Fantomas.Tests.StructTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper

[<Test>]
let ``struct type``() =
    formatSourceString false """
type NameStruct =
    struct
        val Name : string
        new (name) = { Name = name }

        member x.Upper() =
            x.Name.ToUpper()
        
        member x.Lower() =
            x.Name.ToLower()
    end

let n = new NameStruct("Hippo")""" { config with MaxValueBindingWidth = 120 }
    |> prepend newline
    |> should equal """
type NameStruct =
    struct
        val Name: string
        new(name) = { Name = name }

        member x.Upper() = x.Name.ToUpper()

        member x.Lower() = x.Name.ToLower()
    end

let n = new NameStruct("Hippo")
"""

[<Test>]
let ``struct type retains members outside struct-end``() =
    formatSourceString false """
type NameStruct =
    struct
        val Name : string
        new (name) = { Name = name }
    end

    member x.Upper() =
        x.Name.ToUpper()
        
    member x.Lower() =
        x.Name.ToLower()
        
let n = new NameStruct("Hippo")""" config
    |> prepend newline
    |> should equal """
type NameStruct =
    struct
        val Name: string
        new(name) = { Name = name }
    end

    member x.Upper() = x.Name.ToUpper()

    member x.Lower() = x.Name.ToLower()

let n = new NameStruct("Hippo")
"""

[<Test>]
let ``struct tuple``() =
    formatSourceString false """
type S = S of struct (int * int)
let g : struct (int*int) = struct (1,1)
let z = fun (S (struct (u, v)): S) -> u + v
let t = struct (1,2)
match t with
| struct (x,y) -> ()""" config
    |> prepend newline
    |> should equal """
type S = S of struct (int * int)
let g: struct (int * int) = struct (1, 1)
let z = fun ((S (struct (u, v))): S) -> u + v
let t = struct (1, 2)

match t with
| struct (x, y) -> ()
"""

[<Test>]
let ``struct tuple type abbreviation, 605`` () =
    formatSourceString false "type TupleStruct = (struct (string * string))"  config
    |> prepend newline
    |> should equal """
type TupleStruct = (struct (string * string))
"""

[<Test>]
let ``struct tuple type abbreviation, sigfile`` () =
    formatSourceString true """namespace meh

type TupleStruct = (struct (string * string))"""  config
    |> prepend newline
    |> should equal """
namespace meh

type TupleStruct = (struct (string * string))
"""