module Fantomas.Core.Tests.InlineTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelpers

[<Test>]
let ``trivia around inline keyword, 2017`` () =
    formatSourceString
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
    formatSignatureString
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

[<Test>]
let ``trivia around inline keyword with access modifier, 640`` () =
    formatSourceString
        """
  let
#if DEBUG
#else
      inline
#endif
             internal Issue71Wrapper visits moduleId hitPointId context handler add =
    try
      add visits moduleId hitPointId context
    with x ->
      match x with
      | :? KeyNotFoundException
      | :? NullReferenceException
      | :? ArgumentNullException -> handler moduleId hitPointId context x
      | _ -> reraise()
"""
        config
    |> prepend newline
    |> should
        equal
        """
let
#if DEBUG
#else
    inline
#endif
    internal Issue71Wrapper
        visits
        moduleId
        hitPointId
        context
        handler
        add
        =
    try
        add visits moduleId hitPointId context
    with x ->
        match x with
        | :? KeyNotFoundException
        | :? NullReferenceException
        | :? ArgumentNullException -> handler moduleId hitPointId context x
        | _ -> reraise ()
"""
