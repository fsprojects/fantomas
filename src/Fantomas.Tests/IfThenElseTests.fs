module Fantomas.Tests.IfThenElseTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper

[<Test>]
let ``single line if without else`` () =
    formatSourceString false "if foo then bar" config
    |> prepend newline
    |> should equal """
if foo then bar
"""

[<Test>]
let ``single line if/then/else`` () =
    formatSourceString false "if a then b else c" config
    |> prepend newline
    |> should equal """
if a then b else c
"""

[<Test>]
let ``single line if/then/elif/then/else`` () =
    formatSourceString false "if a then b elif c then d else e" config
    |> prepend newline
    |> should equal """
if a then b elif c then d else e
"""

[<Test>]
let ``single line if/then/else if/then/else`` () =
    formatSourceString false "if a then b else if c then d else e" config
    |> prepend newline
    |> should equal """
if a then b else if c then d else e
"""

[<Test>]
let ``single line if/then/else if/elif/then/else`` () =
    formatSourceString false "if a then b else if c then d elif e then f else g" config
    |> prepend newline
    |> should equal """
if a then b else if c then d elif e then f else g
"""

[<Test>]
let ``longer condition, not multi-line`` () =
    formatSourceString false """if aaaaaaaaaBBBBBBBBBBccccccccccDDDDDDDDDeeeeeeeeeeeeeFFFFFFFFFFFggggggggg then 1 else 0
"""  ({ config with PageWidth = 80 })
    |> prepend newline
    |> should equal """
if aaaaaaaaaBBBBBBBBBBccccccccccDDDDDDDDDeeeeeeeeeeeeeFFFFFFFFFFFggggggggg then 1
else 0
"""

[<Test>]
let ``longer ifBranch, not multi-line`` () =
    formatSourceString false """if x then aaaaaaaaaBBBBBBBBBBccccccccccDDDDDDDDDeeeeeeeeeeeeeFFFFFFFFFFFggggggggg else 0
"""  ({ config with PageWidth = 80 })
    |> prepend newline
    |> should equal """
if x then aaaaaaaaaBBBBBBBBBBccccccccccDDDDDDDDDeeeeeeeeeeeeeFFFFFFFFFFFggggggggg
else 0
"""

[<Test>]
let ``longer else branch, not multi-line`` () =
    formatSourceString false """if x then 1 else aaaaaaaaaBBBBBBBBBBccccccccccDDDDDDDDDeeeeeeeeeeeeeFFFFFFFFFFFggggggggg
"""  ({ config with PageWidth = 80 })
    |> prepend newline
    |> should equal """
if x then 1
else aaaaaaaaaBBBBBBBBBBccccccccccDDDDDDDDDeeeeeeeeeeeeeFFFFFFFFFFFggggggggg
"""

[<Test>]
let ``longer if else branch, not multi-line`` () =
    formatSourceString false """if aaaaaaaaaaaa then bbbbbbbbbbbb else if cccccccccccc then ddddddddddd else eeeeeee
"""  ({ config with PageWidth = 80 })
    |> prepend newline
    |> should equal """
if aaaaaaaaaaaa then bbbbbbbbbbbb
else if cccccccccccc then ddddddddddd
else eeeeeee
"""

[<Test>]
let ``longer if else branch, longer elif branch, not multi-line`` () =
    formatSourceString false """if aaaaaa then bbbbbb else if ccccccc then ddddddd elif eeeee then ffffff else gggggg
"""  ({ config with PageWidth = 80 })
    |> prepend newline
    |> should equal """
if aaaaaa then bbbbbb
else if ccccccc then ddddddd
elif eeeee then ffffff
else gggggg
"""

[<Test>]
let ``multiline condition`` () =
    formatSourceString false """if (aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa && bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb) then
    x else y
"""  ({ config with PageWidth = 80 })
    |> prepend newline
    |> should equal """
if (aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
    && bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb) then
    x
else
    y
"""

[<Test>]
let ``multiline if branch`` () =
    formatSourceString false """if a then
    let x = 2
    x + 2
else y
"""  ({ config with PageWidth = 80 })
    |> prepend newline
    |> should equal """
if a then
    let x = 2
    x + 2
else
    y
"""

[<Test>]
let ``multiline else branch`` () =
    formatSourceString false """if a then
    x
else
    let y = 7;
    y + 9
"""  ({ config with PageWidth = 80 })
    |> prepend newline
    |> should equal """
if a then
    x
else
    let y = 7
    y + 9
"""

[<Test>]
let ``multiline else if branch`` () =
    formatSourceString false """if a then
    x else if b then
                let y = 7;
                y + 9
    else
        99
"""  ({ config with PageWidth = 80 })
    |> prepend newline
    |> should equal """
if a then
    x
else if b then
    let y = 7
    y + 9
else
    99
"""

[<Test>]
let ``multiline else if branch, multiline elif branch`` () =
    formatSourceString false """if a then
    x else if b then
                let y = 7;
                y + 9
    elif c then
        let z = 8
        z - 7
    else
        99
"""  ({ config with PageWidth = 80 })
    |> prepend newline
    |> should equal """
if a then
    x
else if b then
    let y = 7
    y + 9
elif c then
    let z = 8
    z - 7
else
    99
"""

[<Test>]
let ``comment after if`` () =
    formatSourceString false """if // meh
    x then 0 else 1
"""  config
    |> prepend newline
    |> should equal """
if // meh
    x then
    0
else
    1
"""

[<Test>]
let ``comment after if branch`` () =
    formatSourceString false """if  x // meh
    then 0 else 1
"""  config
    |> prepend newline
    |> should equal """
if x // meh
then
    0
else
    1
"""

[<Test>]
let ``comment after if branch then keyword`` () =
    formatSourceString false """if  x then // meh
    0 else 1
"""  config
    |> prepend newline
    |> should equal """
if x then // meh
    0
else
    1
"""

[<Test>]
let ``comment after if branch expression`` () =
    formatSourceString false """if  x then
    0  // meh
    else 1
"""  config
    |> prepend newline
    |> should equal """
if x then
    0 // meh
else
    1
"""