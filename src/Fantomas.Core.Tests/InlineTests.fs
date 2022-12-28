module Fantomas.Core.Tests.InlineTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelper

[<Test>]
let ``trivia around inline keyword, 2017`` () =
    formatSourceString
        false
        """
    let 
#if !DEBUG
        inline
#endif
        map f ar = Async.map (Result.map f) ar
"""
        config
    |> prepend newline
    |> should
        equal
        """
let
#if !DEBUG
    inline
#endif
    map
        f
        ar
        =
    Async.map (Result.map f) ar
"""

[<Test>]
let ``inline in plain member`` () =
    formatSourceString
        false
        """
type X =
    member inline x.Y () = ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
type X =
    member inline x.Y() = ()
"""

[<Test>]
let ``inline in get/set member`` () =
    formatSourceString
        false
        """
type X =
    member inline x.Y 
        with inline get () = 4
        and inline set y = ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
type X =
    member inline x.Y
        with inline get () = 4
        and inline set y = ()
"""

[<Test>]
let ``inline in val`` () =
    formatSourceString
        true
        """
val inline meh: int -> int
"""
        config
    |> prepend newline
    |> should
        equal
        """
val inline meh: int -> int
"""
