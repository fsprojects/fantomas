module Fantomas.Tests.SynConstTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper

// https://fsharp.github.io/FSharp.Compiler.Service/reference/fsharp-compiler-ast-synconst.html
[<Test>]
let ``All known SynConst with trivia`` () =
    formatSourceString false """
let a = true
let b = 13uy
let c = "abc"B
let d = 'a'
let e = 23.4M
let f = 1.40e10
let g = 13s
let h = 0x4000s
let i = 0o0777
let j = 13L
let k = 13n
[<Measure>]
type cm
let l = 7<cm>
let m = 1.0<cm>
let n = 1.0m<cm>
let o = 13y
let p = 0xFFy
let q = 1.30f
let r = "meh"
let s = 13us
let t = 0x4000u
let u = 13UL
let v = 17un
let x = ()
let y = 1Q
let z = 1Z
let a2 = 1R
let b2 = 1N
let c2 = 1G
"""  config
    |> should equal """let a = true
let b = 13uy
let c = "abc"B
let d = 'a'
let e = 23.4M
let f = 1.40e10
let g = 13s
let h = 0x4000s
let i = 0o0777
let j = 13L
let k = 13n

[<Measure>]
type cm

let l = 7<cm>
let m = 1.0<cm>
let n = 1.0m<cm>
let o = 13y
let p = 0xFFy
let q = 1.30f
let r = "meh"
let s = 13us
let t = 0x4000u
let u = 13UL
let v = 17un
let x = ()
let y = 1Q
let z = 1Z
let a2 = 1R
let b2 = 1N
let c2 = 1G
"""

[<Test>]
let ``unicode in characters`` () =
    formatSourceString false """
namespace SomeNamespace

module SomeModule =
    let backspace = '\b'
    let formFeed = '\f'
"""  config
    |> prepend newline
    |> should equal """
namespace SomeNamespace

module SomeModule =
    let backspace = '\b'
    let formFeed = '\f'
"""

[<Test>]
let ``escape unicode null, 632`` () =
    formatSourceString false """let nulchar = '\u0000'
let nullstr = "\u0000"
"""  config
    |> prepend newline
    |> should equal """
let nulchar = '\u0000'
let nullstr = "\u0000"
"""