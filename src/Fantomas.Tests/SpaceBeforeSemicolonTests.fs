module Fantomas.Tests.SpaceBeforeSemicolonTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper

let beforeConfig = { config with
                        SpaceBeforeSemicolon = true
                        SpaceAfterSemicolon = false }

[<Test>]
let ``space before attributelist`` () =
    formatSourceString false """[<Foo;Bar;Meh>]
let f a : int = 7"""  beforeConfig
    |> prepend newline
    |> should equal """
[<Foo ;Bar ;Meh>]
let f a: int = 7
"""

[<Test>]
let ``space before array/list`` () =
    formatSourceString false """let a = [ 1;2;3]
let b = [|4;5;6|]
"""  beforeConfig
    |> prepend newline
    |> should equal """
let a = [ 1 ;2 ;3 ]
let b = [| 4 ;5 ;6 |]
"""

[<Test>]
let ``space before inherit expression`` () =
    formatSourceString false """type MyExc =
    inherit Exception
    new(msg) = { inherit Exception(msg); X = 1; }
"""  beforeConfig
    |> prepend newline
    |> should equal """
type MyExc =
    inherit Exception
    new(msg) = { inherit Exception(msg) ;X = 1 }
"""

[<Test>]
let ``space before member in anonymous record type alias`` () =
    formatSourceString false "type Foo = {| Bar:int; Meh:string |}" beforeConfig
    |> should equal "type Foo = {| Bar: int ;Meh: string |}
"

[<Test>]
let ``space before fields in destructured record`` () =
    formatSourceString false """let IsMatchByName record1 (name: string) =
    match record1 with
    | { MyRecord.Name = nameFound; ID = _ } when nameFound = name -> true
    | _ -> false
"""  beforeConfig
    |> prepend newline
    |> should equal """
let IsMatchByName record1 (name: string) =
    match record1 with
    | { MyRecord.Name = nameFound ;ID = _ } when nameFound = name -> true
    | _ -> false
"""

let beforeAndAfterConfig = { config with SpaceBeforeSemicolon = true }

[<Test>]
let ``space before and after attributelist`` () =
    formatSourceString false """[<Foo;Bar;Meh>]
let f a : int = 7"""  beforeAndAfterConfig
    |> prepend newline
    |> should equal """
[<Foo ; Bar ; Meh>]
let f a: int = 7
"""

[<Test>]
let ``space before and after array/list`` () =
    formatSourceString false """let a = [ 1;2;3]
let b = [|4;5;6|]
"""  beforeAndAfterConfig
    |> prepend newline
    |> should equal """
let a = [ 1 ; 2 ; 3 ]
let b = [| 4 ; 5 ; 6 |]
"""

[<Test>]
let ``space before and after inherit expression`` () =
    formatSourceString false """type MyExc =
    inherit Exception
    new(msg) = { inherit Exception(msg); X = 1; }
"""  beforeAndAfterConfig
    |> prepend newline
    |> should equal """
type MyExc =
    inherit Exception
    new(msg) = { inherit Exception(msg) ; X = 1 }
"""

[<Test>]
let ``space before and after member in anonymous record type alias`` () =
    formatSourceString false "type Foo = {| Bar:int; Meh:string |}" beforeAndAfterConfig
    |> should equal "type Foo = {| Bar: int ; Meh: string |}
"

[<Test>]
let ``space before and after fields in destructured record`` () =
    formatSourceString false """let IsMatchByName record1 (name: string) =
    match record1 with
    | { MyRecord.Name = nameFound; ID = _ } when nameFound = name -> true
    | _ -> false
"""  beforeAndAfterConfig
    |> prepend newline
    |> should equal """
let IsMatchByName record1 (name: string) =
    match record1 with
    | { MyRecord.Name = nameFound ; ID = _ } when nameFound = name -> true
    | _ -> false
"""