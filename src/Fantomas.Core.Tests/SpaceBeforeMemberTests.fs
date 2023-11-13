module Fantomas.Core.Tests.SpaceBeforeMemberTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelpers

let spaceBeforeConfig = { config with SpaceBeforeMember = true }

[<Test>]
let ``default config should not add a space before a type member`` () =
    formatSourceString
        """
type Person() =
    member this.Walk (distance:int) = ()
    member this.Sleep() = ignore
    member __.singAlong () = ()
    member __.swim (duration:TimeSpan) = ()
"""
        { config with
            MaxFunctionBindingWidth = 120 }
    |> prepend newline
    |> should
        equal
        """
type Person() =
    member this.Walk(distance: int) = ()
    member this.Sleep() = ignore
    member __.singAlong() = ()
    member __.swim(duration: TimeSpan) = ()
"""

[<Test>]
let ``spaceBeforeMember should add a space before a type member`` () =
    formatSourceString
        """
type Person() =
    member this.Walk (distance:int) = ()
    member this.Sleep() = ignore
    member __.singAlong () = ()
    member __.swim (duration:TimeSpan) = ()
"""
        { spaceBeforeConfig with
            MaxFunctionBindingWidth = 120 }
    |> prepend newline
    |> should
        equal
        """
type Person() =
    member this.Walk (distance: int) = ()
    member this.Sleep () = ignore
    member __.singAlong () = ()
    member __.swim (duration: TimeSpan) = ()
"""
