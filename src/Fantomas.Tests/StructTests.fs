module Fantomas.Tests.StructTests

open NUnit.Framework
open FsUnit

open Fantomas.CodeFormatter
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

let n = new NameStruct("Hippo")""" config
    |> prepend newline
    |> should equal """
type NameStruct =
    struct
        val Name : string
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
        val Name : string
        new(name) = { Name = name }
    end
    member x.Upper() = x.Name.ToUpper()
    member x.Lower() = x.Name.ToLower()

let n = new NameStruct("Hippo")
"""
