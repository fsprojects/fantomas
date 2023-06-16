module Fantomas.Core.Tests.StructTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelpers

[<Test>]
let ``struct type`` () =
    formatSourceString
        false
        """
type NameStruct =
    struct
        val Name : string
        new (name) = { Name = name }

        member x.Upper() =
            x.Name.ToUpper()

        member x.Lower() =
            x.Name.ToLower()
    end

let n = new NameStruct("Hippo")"""
        { config with
            MaxValueBindingWidth = 120 }
    |> prepend newline
    |> should
        equal
        """
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
let ``struct type retains members outside struct-end`` () =
    formatSourceString
        false
        """
type NameStruct =
    struct
        val Name : string
        new (name) = { Name = name }
    end

    member x.Upper() =
        x.Name.ToUpper()

    member x.Lower() =
        x.Name.ToLower()

let n = new NameStruct("Hippo")"""
        config
    |> prepend newline
    |> should
        equal
        """
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
let ``struct tuple`` () =
    formatSourceString
        false
        """
type S = S of struct (int * int)
let g : struct (int*int) = struct (1,1)
let z = fun (S (struct (u, v)): S) -> u + v
let t = struct (1,2)
match t with
| struct (x,y) -> ()"""
        config
    |> prepend newline
    |> should
        equal
        """
type S = S of struct (int * int)
let g: struct (int * int) = struct (1, 1)
let z = fun (S(struct (u, v)): S) -> u + v
let t = struct (1, 2)

match t with
| struct (x, y) -> ()
"""

[<Test>]
let ``struct tuple type abbreviation, 605`` () =
    formatSourceString false "type TupleStruct = (struct (string * string))" config
    |> prepend newline
    |> should
        equal
        """
type TupleStruct = (struct (string * string))
"""

[<Test>]
let ``struct tuple type abbreviation, sigfile`` () =
    formatSourceString
        true
        """namespace meh

type TupleStruct = (struct (string * string))"""
        config
    |> prepend newline
    |> should
        equal
        """
namespace meh

type TupleStruct = (struct (string * string))
"""

[<Test>]
let ``struct empty type, 2592`` () =
    formatSourceString
        false
        """
type NameStruct = struct end
"""
        config
    |> prepend newline
    |> should
        equal
        """
type NameStruct =
    struct
    end
"""

[<Test>]
let ``struct empty type with ctor`` () =
    formatSourceString
        false
        """
type NameStruct() =
    struct
    end
"""
        config
    |> prepend newline
    |> should
        equal
        """
type NameStruct() =
    struct
    end
"""

[<Test>]
let ``anonymous struct record with trivia`` () =
    formatSourceString
        false
        """
struct // 1
    {| // 2
        // 3
        X = 4
    // 5       
    |} // 6 
"""
        config
    |> prepend newline
    |> should
        equal
        """
struct // 1
    {| // 2
       // 3
       X = 4
    // 5
    |} // 6
"""

[<Test>]
let ``second binding does not contain accessibility, `` () =
    formatSourceString
        false
        """
module Telplin

type T =
    struct
        member private this.X with get () : int = 1 and set (_:int) = ()
    end
"""
        config
    |> prepend newline
    |> should
        equal
        """
module Telplin

type T =
    struct
        member private this.X
            with get (): int = 1
            and set (_: int) = ()
    end
"""

// A double access modifier, like on the member and on the get or set binding
// will lead to "Multiple accessibilities given for property getter or setter"
// When both get and set have accessibility, the getter binding seems to win.
// See: https://github.com/dotnet/fsharp/issues/15423

[<Test>]
let ``different accessibility on setter`` () =
    formatSourceString
        false
        """
module Telplin

type T =
    struct
        member this.X with get () : int = 1 and private set (_:int) = ()
        member this.Y with internal get () : int = 1 and private set (_:int) = ()
        member private this.Z with get () : int = 1 and set (_:int) = ()
        member this.S with internal set (_:int) = () and private get () : int = 1
    end
"""
        config
    |> prepend newline
    |> should
        equal
        """
module Telplin

type T =
    struct
        member this.X
            with get (): int = 1
            and set (_: int) = ()

        member internal this.Y
            with get (): int = 1
            and set (_: int) = ()

        member private this.Z
            with get (): int = 1
            and set (_: int) = ()

        member private this.S
            with set (_: int) = ()
            and get (): int = 1
    end
"""
