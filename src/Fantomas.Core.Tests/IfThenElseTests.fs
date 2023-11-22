module Fantomas.Core.Tests.IfThenElseTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelpers

[<Test>]
let ``single line if without else`` () =
    formatSourceString
        "if foo then bar"
        { config with
            InsertFinalNewline = false
            MaxIfThenShortWidth = 15 }
    |> should equal "if foo then bar"

[<Test>]
let ``if without else, if is longer`` () =
    formatSourceString
        """
if foooooooooooooooooooooooooooooooooooooooooooo
then bar
"""
        config
    |> prepend newline
    |> should
        equal
        """
if foooooooooooooooooooooooooooooooooooooooooooo then
    bar
"""

[<Test>]
let ``if without else, then is longer`` () =
    formatSourceString
        """
if foo then baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaar
"""
        config
    |> prepend newline
    |> should
        equal
        """
if foo then
    baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaar
"""

[<Test>]
let ``short if then without else`` () =
    formatSourceString
        """
if a then b
"""
        { config with MaxIfThenShortWidth = 12 }
    |> prepend newline
    |> should
        equal
        """
if a then b
"""

[<Test>]
let ``multiline if without else`` () =
    formatSourceString
        """
if foo && bar && meh then aha
"""
        { config with
            MaxInfixOperatorExpression = 5 }
    |> prepend newline
    |> should
        equal
        """
if
    foo
    && bar
    && meh
then
    aha
"""

[<Test>]
let ``single line if/then/else`` () =
    formatSourceString "if a then b else c" config
    |> prepend newline
    |> should
        equal
        """
if a then b else c
"""

[<Test>]
let ``single line if/then/elif/then/else`` () =
    formatSourceString "if a then b elif c then d else e" config
    |> prepend newline
    |> should
        equal
        """
if a then b
elif c then d
else e
"""

[<Test>]
let ``single line if/then/else if/then/else`` () =
    formatSourceString "if a then b else if c then d else e" config
    |> prepend newline
    |> should
        equal
        """
if a then b
else if c then d
else e
"""

[<Test>]
let ``single line if/then/else if/elif/then/else`` () =
    formatSourceString "if a then b else if c then d elif e then f else g" config
    |> prepend newline
    |> should
        equal
        """
if a then b
else if c then d
elif e then f
else g
"""

[<Test>]
let ``longer condition, not multi-line`` () =
    formatSourceString
        """if aaaaaaaaaBBBBBBBBBBccccccccccDDDDDDDDDeeeeeeeeeeeeeFFFFFFFFFFFggggggggg then 1 else 0
"""
        { config with MaxLineLength = 80 }
    |> prepend newline
    |> should
        equal
        """
if aaaaaaaaaBBBBBBBBBBccccccccccDDDDDDDDDeeeeeeeeeeeeeFFFFFFFFFFFggggggggg then
    1
else
    0
"""

[<Test>]
let ``longer ifBranch, not multi-line`` () =
    formatSourceString
        """if x then aaaaaaaaaBBBBBBBBBBccccccccccDDDDDDDDDeeeeeeeeeeeeeFFFFFFFFFFFggggggggg else 0
"""
        { config with MaxLineLength = 80 }
    |> prepend newline
    |> should
        equal
        """
if x then
    aaaaaaaaaBBBBBBBBBBccccccccccDDDDDDDDDeeeeeeeeeeeeeFFFFFFFFFFFggggggggg
else
    0
"""

[<Test>]
let ``longer else branch, not multi-line`` () =
    formatSourceString
        """if x then 1 else aaaaaaaaaBBBBBBBBBBccccccccccDDDDDDDDDeeeeeeeeeeeeeFFFFFFFFFFFggggggggg
"""
        { config with MaxLineLength = 80 }
    |> prepend newline
    |> should
        equal
        """
if x then
    1
else
    aaaaaaaaaBBBBBBBBBBccccccccccDDDDDDDDDeeeeeeeeeeeeeFFFFFFFFFFFggggggggg
"""

[<Test>]
let ``longer if else branch, not multi-line`` () =
    formatSourceString
        """if aaaaaaaaaaaa then bbbbbbbbbbbb else if cccccccccccc then ddddddddddd else eeeeeee
"""
        { config with MaxLineLength = 80 }
    |> prepend newline
    |> should
        equal
        """
if aaaaaaaaaaaa then bbbbbbbbbbbb
else if cccccccccccc then ddddddddddd
else eeeeeee
"""

[<Test>]
let ``longer if else branch, longer elif branch, not multi-line`` () =
    formatSourceString
        """if aaaaaa then bbbbbb else if ccccccc then ddddddd elif eeeee then ffffff else gggggg
"""
        { config with MaxLineLength = 80 }
    |> prepend newline
    |> should
        equal
        """
if aaaaaa then bbbbbb
else if ccccccc then ddddddd
elif eeeee then ffffff
else gggggg
"""

[<Test>]
let ``multiline condition`` () =
    formatSourceString
        """if (aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa && bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb) then
    x else y
"""
        { config with MaxLineLength = 80 }
    |> prepend newline
    |> should
        equal
        """
if
    (aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
     && bbbbbbbbbbbbbbbbbbbbbbbbbbbbbbbb)
then
    x
else
    y
"""

[<Test>]
let ``multiline if branch`` () =
    formatSourceString
        """if a then
    let x = 2
    x + 2
else y
"""
        { config with MaxLineLength = 80 }
    |> prepend newline
    |> should
        equal
        """
if a then
    let x = 2
    x + 2
else
    y
"""

[<Test>]
let ``multiline else branch`` () =
    formatSourceString
        """if a then
    x
else
    let y = 7;
    y + 9
"""
        { config with MaxLineLength = 80 }
    |> prepend newline
    |> should
        equal
        """
if a then
    x
else
    let y = 7
    y + 9
"""

[<Test>]
let ``multiline else if branch`` () =
    formatSourceString
        """if a then
    x else if b then
                let y = 7;
                y + 9
    else
        99
"""
        { config with MaxLineLength = 80 }
    |> prepend newline
    |> should
        equal
        """
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
    formatSourceString
        """if a then
    x else if b then
                let y = 7;
                y + 9
    elif c then
        let z = 8
        z - 7
    else
        99
"""
        { config with MaxLineLength = 80 }
    |> prepend newline
    |> should
        equal
        """
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
    formatSourceString
        """if // meh
    x then 0 else 1
"""
        config
    |> prepend newline
    |> should
        equal
        """
if // meh
    x
then
    0
else
    1
"""

[<Test>]
let ``comment after if branch`` () =
    formatSourceString
        """if  x // meh
    then 0 else 1
"""
        config
    |> prepend newline
    |> should
        equal
        """
if
    x // meh
then
    0
else
    1
"""

[<Test>]
let ``comment after if branch then keyword`` () =
    formatSourceString
        """if  x then // meh
    0 else 1
"""
        config
    |> prepend newline
    |> should
        equal
        """
if x then // meh
    0
else
    1
"""

[<Test>]
let ``comment after if branch expression`` () =
    formatSourceString
        """if  x then
    0  // meh
    else 1
"""
        config
    |> prepend newline
    |> should
        equal
        """
if x then
    0 // meh
else
    1
"""

[<Test>]
let ``comment after else keyword`` () =
    formatSourceString
        """if  x then 0 else // meh
    1
"""
        config
    |> prepend newline
    |> should
        equal
        """
if x then
    0
else // meh
    1
"""

[<Test>]
let ``comment after else branch expression`` () =
    formatSourceString
        """if  x then 0 else 1 // meh
"""
        config
    |> prepend newline
    |> should
        equal
        """
if x then 0 else 1 // meh
"""

[<Test>]
let ``comment after else keyword before if keyword`` () =
    formatSourceString
        """if  a then b else // meh
    if c then d else e
"""
        config
    |> prepend newline
    |> should
        equal
        """
if a then
    b
else if // meh
    c
then
    d
else
    e
"""

[<Test>]
let ``comment after else if keyword`` () =
    formatSourceString
        """if  a then b else if // meh
    c then d else e
"""
        config
    |> prepend newline
    |> should
        equal
        """
if a then
    b
else if // meh
    c
then
    d
else
    e
"""

[<Test>]
let ``comment after elif keyword`` () =
    formatSourceString
        """if  a then b elif // meh
    c then d else e
"""
        config
    |> prepend newline
    |> should
        equal
        """
if a then
    b
elif // meh
    c
then
    d
else
    e
"""

[<Test>]
let ``comment after else if boolean expression`` () =
    formatSourceString
        """if  a then b else if
    c // meh
    then d else e
"""
        config
    |> prepend newline
    |> should
        equal
        """
if a then
    b
else if
    c // meh
then
    d
else
    e
"""

[<Test>]
let ``comment after elif boolean expression`` () =
    formatSourceString
        """if  a then b elif
    c // meh
    then d else e
"""
        config
    |> prepend newline
    |> should
        equal
        """
if a then
    b
elif
    c // meh
then
    d
else
    e
"""

[<Test>]
let ``comment after else if then keyword`` () =
    formatSourceString
        """if  a then b else if
    c  then // meh
    d else e
"""
        config
    |> prepend newline
    |> should
        equal
        """
if a then b
else if c then d // meh
else e
"""

[<Test>]
let ``comment after elif then keyword`` () =
    formatSourceString
        """if  a then b elif
    c  then // meh
    d else e
"""
        config
    |> prepend newline
    |> should
        equal
        """
if a then b
elif c then d // meh
else e
"""

[<Test>]
let ``comment after else if branch expression`` () =
    formatSourceString
        """if  a then b else if
    c  then
    d // meh
    else e
"""
        config
    |> prepend newline
    |> should
        equal
        """
if a then b
else if c then d // meh
else e
"""

[<Test>]
let ``comment after multi line else  branch expression`` () =
    formatSourceString
        """
if  a then b
else if c  then d
else
    e // meh
    f
"""
        config
    |> prepend newline
    |> should
        equal
        """
if a then
    b
else if c then
    d
else
    e // meh
    f
"""

[<Test>]
let ``comment after multi line elif  branch expression`` () =
    formatSourceString
        """
if  a then b
elif c  then
    d
    e // meh
else
    f
"""
        config
    |> prepend newline
    |> should
        equal
        """
if a then
    b
elif c then
    d
    e // meh
else
    f
"""

[<Test>]
let ``comment after multi line else if  branch expression`` () =
    formatSourceString
        """
if  a then b
else if c  then
    d
    e // meh
else
    f
"""
        config
    |> prepend newline
    |> should
        equal
        """
if a then
    b
else if c then
    d
    e // meh
else
    f
"""

[<Test>]
let ``comment after else & if keyword`` () =
    formatSourceString
        """
if  a then b
else // foo
if // bar
    c  then d else e
"""
        config
    |> prepend newline
    |> should
        equal
        """
if a then
    b
else if // foo
    // bar
    c
then
    d
else
    e
"""

[<Test>]
let ``block comment if keyword`` () =
    formatSourceString
        """
if (* meh *) a then b
else c
"""
        config
    |> prepend newline
    |> should
        equal
        """
if (* meh *) a then b else c
"""

[<Test>]
let ``block comment if bool expr`` () =
    formatSourceString
        """
if  a  (* meh *)   then b
else c
"""
        config
    |> prepend newline
    |> should
        equal
        """
if a (* meh *) then b else c
"""

[<Test>]
let ``block comment then keyword`` () =
    formatSourceString
        """
if  a   then (* meh *)   b
else c
"""
        config
    |> prepend newline
    |> should
        equal
        """
if a then (* meh *) b else c
"""

[<Test>]
let ``block comment if branch expr`` () =
    formatSourceString
        """
if  a   then    b (* meh *)
else c
"""
        config
    |> prepend newline
    |> should
        equal
        """
if a then b (* meh *) else c
"""

[<Test>]
let ``block comment else keyword`` () =
    formatSourceString
        """
if  a   then    b
else  (* meh *)   c
"""
        config
    |> prepend newline
    |> should
        equal
        """
if a then b else (* meh *) c
"""

[<Test>]
let ``block comment else branch expr`` () =
    formatSourceString
        """
if  a   then    b
else     c  (* meh *)
"""
        config
    |> prepend newline
    |> should
        equal
        """
if a then b else c (* meh *)
"""

[<Test>]
let ``block comment between else and if keyword`` () =
    formatSourceString
        """
if  a   then    b
else (* meh *) if c then d
else     e
"""
        config
    |> prepend newline
    |> should
        equal
        """
if a then b
else if (* meh *) c then d
else e
"""

[<Test>]
let ``block comment after else if keyword`` () =
    formatSourceString
        """
if  a   then    b
else  if (* meh *)   c then d
else     e
"""
        config
    |> prepend newline
    |> should
        equal
        """
if a then b
else if (* meh *) c then d
else e
"""

[<Test>]
let ``block comment after elif keyword`` () =
    formatSourceString
        """
if  a   then    b
elif (* meh *)   c then d
else     e
"""
        config
    |> prepend newline
    |> should
        equal
        """
if a then b
elif (* meh *) c then d
else e
"""

[<Test>]
let ``block comment after elif branch expr`` () =
    formatSourceString
        """
if  a   then    b
elif c  (* meh *)  then d
else     e
"""
        config
    |> prepend newline
    |> should
        equal
        """
if a then b
elif c (* meh *) then d
else e
"""

[<Test>]
let ``block comment after else if branch expr`` () =
    formatSourceString
        """
if  a   then    b
else if c  (* meh *)  then d
else     e
"""
        config
    |> prepend newline
    |> should
        equal
        """
if a then b
else if c (* meh *) then d
else e
"""

[<Test>]
let ``line comment after all fragments of IfThenElse expr`` () =
    formatSourceString
        """
if // c1
  a // c2
then // c3
  b // c4
else // c5
if // c6
  c // c7
  then // c8
  d // c9
else // c10
  e // c11
"""
        config
    |> prepend newline
    |> should
        equal
        """
if // c1
    a // c2
then // c3
    b // c4
else if // c5
    // c6
    c // c7
then // c8
    d // c9
else // c10
    e // c11
"""

[<Test>]
let ``newlines before comment on elif`` () =
    formatSourceString
        """
if strA.Length = 0 && strB.Length = 0 then 0

// OPTIMIZATION : If the substrings have the same (identical) underlying string
// and offset, the comparison value will depend only on the length of the substrings.
elif strA.String == strB.String && strA.Offset = strB.Offset then
    compare strA.Length strB.Length

else
    -1
"""
        { config with
            MaxInfixOperatorExpression = 55 }
    |> prepend newline
    |> should
        equal
        """
if strA.Length = 0 && strB.Length = 0 then
    0

// OPTIMIZATION : If the substrings have the same (identical) underlying string
// and offset, the comparison value will depend only on the length of the substrings.
elif strA.String == strB.String && strA.Offset = strB.Offset then
    compare strA.Length strB.Length

else
    -1
"""

[<Test>]
let ``simple if/else with long identifiers`` () =
    formatSourceString
        """
if someveryveryveryverylongexpression then
            someveryveryveryveryveryverylongexpression
else someveryveryveryverylongexpression
"""
        { config with MaxLineLength = 80 }
    |> prepend newline
    |> should
        equal
        """
if someveryveryveryverylongexpression then
    someveryveryveryveryveryverylongexpression
else
    someveryveryveryverylongexpression
"""

[<Test>]
let ``longer if branch, nothing multiline`` () =
    formatSourceString
        """
   if m.Success then Some (List.tail [ for x in m.Groups -> x.Value ]) else None
"""
        config
    |> prepend newline
    |> should
        equal
        """
if m.Success then
    Some(List.tail [ for x in m.Groups -> x.Value ])
else
    None
"""

[<Test>]
let ``almost longer if branch where the whole if/else is indented by letbinding`` () =
    formatSourceString
        """
let (|Integer|_|) (str: string) =
   let mutable intvalue = 0
   if System.Int32.TryParse(str, &intvalue) then Some(intvalue)
   else None
"""
        { config with
            MaxIfThenElseShortWidth = 70 }
    |> prepend newline
    |> should
        equal
        """
let (|Integer|_|) (str: string) =
    let mutable intvalue = 0
    if System.Int32.TryParse(str, &intvalue) then Some(intvalue) else None
"""

[<Test>]
let ``longer elif condition`` () =
    formatSourceString
        """if a then b elif somethingABitLongerToForceDifferentStyle then c else d
"""
        { config with
            MaxIfThenElseShortWidth = 53 }
    |> prepend newline
    |> should
        equal
        """
if a then b
elif somethingABitLongerToForceDifferentStyle then c
else d
"""

[<Test>]
let ``impact of MaxIfThenElseShortWidth setting, longer bool expression`` () =
    let source = """if (tare + netWeight) = 10000 then a else b"""

    formatSourceString
        source
        { config with
            MaxIfThenElseShortWidth = 10 }
    |> prepend newline
    |> should
        equal
        """
if (tare + netWeight) = 10000 then
    a
else
    b
"""

    formatSourceString source config
    |> prepend newline
    |> should
        equal
        """
if (tare + netWeight) = 10000 then a else b
"""

[<Test>]
let ``impact of MaxIfThenElseShortWidth setting, longer if branch`` () =
    let source = """if a then (tare + netWeight) + 10000 else 0"""

    formatSourceString source config
    |> prepend newline
    |> should
        equal
        """
if a then (tare + netWeight) + 10000 else 0
"""

    formatSourceString
        source
        { config with
            MaxIfThenElseShortWidth = 40 }
    |> prepend newline
    |> should
        equal
        """
if a then
    (tare + netWeight) + 10000
else
    0
"""

[<Test>]
let ``impact of MaxIfThenElseShortWidth setting, longer else branch`` () =
    let source = """if a then 0 else (tare + netWeight) + 10"""

    formatSourceString source config
    |> prepend newline
    |> should
        equal
        """
if a then 0 else (tare + netWeight) + 10
"""

    formatSourceString
        source
        { config with
            MaxIfThenElseShortWidth = 20 }
    |> prepend newline
    |> should
        equal
        """
if a then
    0
else
    (tare + netWeight) + 10
"""

[<Test>]
let ``else if with newline in between, 675`` () =
    formatSourceString
        """namespace Fantomas

module String =
    let merge a b =
            if la <> lb then
                if la > lb then a' else b'
            else
                if String.length a' < String.length b' then a' else b'
"""
        { config with
            MaxIfThenElseShortWidth = 100 }
    |> prepend newline
    |> should
        equal
        """
namespace Fantomas

module String =
    let merge a b =
        if la <> lb then
            if la > lb then a' else b'
        else if String.length a' < String.length b' then
            a'
        else
            b'
"""

[<Test>]
let ``second else if where else and if are on separate lines, 713`` () =
    formatSourceString
        """if v1 < v2 then
    -1
elif v1 > v2 then
    1
else
    if t1 < t2 then
        -1
    elif t1 > t2 then
        1
    else
        0
"""
        config
    |> prepend newline
    |> should
        equal
        """
if v1 < v2 then -1
elif v1 > v2 then 1
else if t1 < t2 then -1
elif t1 > t2 then 1
else 0
"""

[<Test>]
let ``newline between else if,  prior by elif`` () =
    formatSourceString
        """
module String =
    let merge a b =
            if la <> lb then
                if la > lb then a' else b'
            elif la = lb then a'
            else
                if String.length a' < String.length b' then a' else b'
"""
        config
    |> prepend newline
    |> should
        equal
        """
module String =
    let merge a b =
        if la <> lb then
            if la > lb then a' else b'
        elif la = lb then
            a'
        else if String.length a' < String.length b' then
            a'
        else
            b'
"""

[<Test>]
let ``newline between else if, followed by else if`` () =
    formatSourceString
        """
module String =
    let merge a b =
            if la <> lb then
                if la > lb then a' else b'
            else
                if String.length a' < String.length b' then a' else if String.length a' > String.length b' then b' else b'
"""
        config
    |> prepend newline
    |> should
        equal
        """
module String =
    let merge a b =
        if la <> lb then
            if la > lb then a' else b'
        else if String.length a' < String.length b' then
            a'
        else if String.length a' > String.length b' then
            b'
        else
            b'
"""

[<Test>]
let ``comment after then in if/then, 730`` () =
    formatSourceString
        """if true then // comment
    ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
if true then // comment
    ()
"""

[<Test>]
let ``don't add additional new line before nested if/then, 1035`` () =
    formatSourceString
        """
            if ast.ParseHadErrors then
                let errors =
                    ast.Errors
                    |> Array.filter (fun e -> e.Severity = FSharpErrorSeverity.Error)

                if not <| Array.isEmpty errors
                then log.LogError(sprintf "Parsing failed with errors: %A\nAnd options: %A" errors checkOptions)

                return Error ast.Errors
            else
                match ast.ParseTree with
                | Some tree -> return Result.Ok tree
                | _ -> return Error Array.empty // Not sure this branch can be reached.
"""
        { config with
            MaxValueBindingWidth = 50
            MaxFunctionBindingWidth = 50
            MaxInfixOperatorExpression = 50 }
    |> prepend newline
    |> should
        equal
        """
if ast.ParseHadErrors then
    let errors =
        ast.Errors
        |> Array.filter (fun e -> e.Severity = FSharpErrorSeverity.Error)

    if not <| Array.isEmpty errors then
        log.LogError(sprintf "Parsing failed with errors: %A\nAnd options: %A" errors checkOptions)

    return Error ast.Errors
else
    match ast.ParseTree with
    | Some tree -> return Result.Ok tree
    | _ -> return Error Array.empty // Not sure this branch can be reached.
"""

[<Test>]
let ``comment after if expression, 1019`` () =
    formatSourceString
        """
let foo result total =
    if result = 0 // there's a comment here
    then total // and another one
    else result
"""
        config
    |> prepend newline
    |> should
        equal
        """
let foo result total =
    if
        result = 0 // there's a comment here
    then
        total // and another one
    else
        result
"""

[<Test>]
let ``if/then/elif without else, 1211`` () =
    formatSourceString
        """
let a =
        // check if the current # char is part of an define expression
        // if so add to defines
        let captureHashDefine idx =
                if trimmed.StartsWith("#if")
                then defines.Add(processLine "#if" trimmed lineNumber offset)
                elif trimmed.StartsWith("#elseif")
                then defines.Add(processLine "#elseif" trimmed lineNumber offset)
                elif trimmed.StartsWith("#else")
                then defines.Add(processLine "#else" trimmed lineNumber offset)
                elif trimmed.StartsWith("#endif")
                then defines.Add(processLine "#endif" trimmed lineNumber offset)

        for idx in [ 0 .. lastIndex ] do
            let zero = sourceCode.[idx]
            let plusOne = sourceCode.[idx + 1]
            let plusTwo = sourceCode.[idx + 2]
            ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
let a =
    // check if the current # char is part of an define expression
    // if so add to defines
    let captureHashDefine idx =
        if trimmed.StartsWith("#if") then
            defines.Add(processLine "#if" trimmed lineNumber offset)
        elif trimmed.StartsWith("#elseif") then
            defines.Add(processLine "#elseif" trimmed lineNumber offset)
        elif trimmed.StartsWith("#else") then
            defines.Add(processLine "#else" trimmed lineNumber offset)
        elif trimmed.StartsWith("#endif") then
            defines.Add(processLine "#endif" trimmed lineNumber offset)

    for idx in [ 0..lastIndex ] do
        let zero = sourceCode.[idx]
        let plusOne = sourceCode.[idx + 1]
        let plusTwo = sourceCode.[idx + 2]
        ()
"""

[<Test>]
let ``multiline if/then/elif without else, 1187`` () =
    formatSourceString
        """
let fn () =
    if true then
        DoSomething ()
        DoSomethingElse()
    elif shouldWe then
        Whatever()

let  nextfunc () =
    printf "Hi"
"""
        config
    |> prepend newline
    |> should
        equal
        """
let fn () =
    if true then
        DoSomething()
        DoSomethingElse()
    elif shouldWe then
        Whatever()

let nextfunc () = printf "Hi"
"""

[<Test>]
let ``nested if/then/else in short mode, 1243`` () =
    formatSourceString
        """
            let funcs =
                fse.MembersFunctionsAndValues
                |> Seq.sortWith (fun n1 n2 ->
                    let modifierScore (f : FSharpMemberOrFunctionOrValue) =
                        if f.IsProperty then
                            if f.IsInstanceMember then
                                if f.IsDispatchSlot then 9 else 1
                            else 8
                        elif f.IsMember then
                            if f.IsInstanceMember then
                                if f.IsDispatchSlot then 11 else 2
                            else 10
                        else 3
                    let n1Score = modifierScore n1
                    let n2Score = modifierScore n2
                    if n1Score = n2Score then
                        n1.DisplayName.CompareTo n2.DisplayName
                    else
                        n1Score.CompareTo n2Score
                )
"""
        config
    |> prepend newline
    |> should
        equal
        """
let funcs =
    fse.MembersFunctionsAndValues
    |> Seq.sortWith (fun n1 n2 ->
        let modifierScore (f: FSharpMemberOrFunctionOrValue) =
            if f.IsProperty then
                if f.IsInstanceMember then
                    if f.IsDispatchSlot then 9 else 1
                else
                    8
            elif f.IsMember then
                if f.IsInstanceMember then
                    if f.IsDispatchSlot then 11 else 2
                else
                    10
            else
                3

        let n1Score = modifierScore n1
        let n2Score = modifierScore n2

        if n1Score = n2Score then
            n1.DisplayName.CompareTo n2.DisplayName
        else
            n1Score.CompareTo n2Score)
"""

[<Test>]
let ``multiline then clause`` () =
    formatSourceString
        """
[<EntryPoint>]
let main argv =
    let fileToFile (inFile: string) (outFile: string) =
        try
            use buffer =
                if hasByteOrderMark
                then new StreamWriter(new FileStream(outFile, FileMode.OpenOrCreate, FileAccess.ReadWrite),
                                      Encoding.UTF8)
                else new StreamWriter(outFile)

            buffer.Flush()
        with exn ->
            eprintfn "The following exception occurred while formatting %s: %O" inFile exn

    0
"""
        config
    |> prepend newline
    |> should
        equal
        """
[<EntryPoint>]
let main argv =
    let fileToFile (inFile: string) (outFile: string) =
        try
            use buffer =
                if hasByteOrderMark then
                    new StreamWriter(
                        new FileStream(outFile, FileMode.OpenOrCreate, FileAccess.ReadWrite),
                        Encoding.UTF8
                    )
                else
                    new StreamWriter(outFile)

            buffer.Flush()
        with exn ->
            eprintfn "The following exception occurred while formatting %s: %O" inFile exn

    0
"""

[<Test>]
let ``multiline function application condition with 2 space indent, 1267`` () =
    formatSourceString
        "
let code =
    if System.Text.RegularExpressions.Regex.IsMatch(
        d.Name,
        \"\"\"^[a-zA-Z][a-zA-Z0-9']+$\"\"\") then
        d.Name
    elif d.NamespaceToOpen.IsSome then
        d.Name
    else
        PrettyNaming.QuoteIdentifierIfNeeded d.Name
"
        { config with
            MaxLineLength = 60
            IndentSize = 2 }
    |> prepend newline
    |> should
        equal
        "
let code =
  if
    System.Text.RegularExpressions.Regex.IsMatch(
      d.Name,
      \"\"\"^[a-zA-Z][a-zA-Z0-9']+$\"\"\"
    )
  then
    d.Name
  elif d.NamespaceToOpen.IsSome then
    d.Name
  else
    PrettyNaming.QuoteIdentifierIfNeeded d.Name
"

[<Test>]
let ``cond, e1 and e2 are short`` () =
    formatSourceString
        """
if cond then e1 else e2
"""
        config
    |> prepend newline
    |> should
        equal
        """
if cond then e1 else e2
"""

[<Test>]
let ``e1 is if/then/else`` () =
    formatSourceString
        """
if cond then
    if a then b else c
else
    e2
"""
        config
    |> prepend newline
    |> should
        equal
        """
if cond then
    if a then b else c
else
    e2
"""

[<Test>]
let ``e2 is if/then/else`` () =
    formatSourceString
        """
if cond then
    e1
else
    if a then b else c
"""
        config
    |> prepend newline
    |> should
        equal
        """
if cond then e1
else if a then b
else c
"""

[<Test>]
let ``multiple elifs where one condition is longer than configuration value`` () =
    formatSourceString
        """
if a then
    b
elif cccccccccccccccccccccccc then d
else f
"""
        { config with
            MaxIfThenElseShortWidth = 20 }
    |> prepend newline
    |> should
        equal
        """
if a then
    b
elif cccccccccccccccccccccccc then
    d
else
    f
"""

[<Test>]
let ``multiline if/then/else piped to function, 1324`` () =
    formatSourceString
        """
        let tryDecompile (ty: FSharpEntity) = async {
            match ty.TryFullName with
            | Some fullName ->
                return decompile ty.Assembly.SimpleName externalSym
            | None ->
                // might be abbreviated type (like string)
                return!
                  if ty.IsFSharpAbbreviation then Some ty.AbbreviatedType else None
                  |> tryGetTypeDef
                  |> tryGetSource
          }
"""
        { config with IndentSize = 2 }
    |> prepend newline
    |> should
        equal
        """
let tryDecompile (ty: FSharpEntity) =
  async {
    match ty.TryFullName with
    | Some fullName -> return decompile ty.Assembly.SimpleName externalSym
    | None ->
      // might be abbreviated type (like string)
      return!
        (if ty.IsFSharpAbbreviation then
           Some ty.AbbreviatedType
         else
           None)
        |> tryGetTypeDef
        |> tryGetSource
  }
"""

[<Test>]
let ``if then else followed by pipe, 1327`` () =
    formatSourceString
        """
module X =
  let getValSignature displayContext (v: FSharpMemberOrFunctionOrValue) =
    let name =
      if v.DisplayName.StartsWith "( "
      then v.LogicalName
      else v.DisplayName
      |> PrettyNaming.QuoteIdentifierIfNeeded

    ()
"""
        { config with IndentSize = 2 }
    |> prepend newline
    |> should
        equal
        """
module X =
  let getValSignature displayContext (v: FSharpMemberOrFunctionOrValue) =
    let name =
      (if v.DisplayName.StartsWith "( " then
         v.LogicalName
       else
         v.DisplayName)
      |> PrettyNaming.QuoteIdentifierIfNeeded

    ()
"""

[<Test>]
let ``if expression with SynExpr.DotGet inside, 1329`` () =
    formatSourceString
        """
let private tryGetUrlWithExactMatch (pathPattern: string<SourcelinkPattern>) (urlPattern: string<Url>) (document: Document) =
    if (UMX.untag pathPattern).Equals(UMX.untag document.Name, System.StringComparison.Ordinal)
    then Some (urlPattern, normalizeRepoPath (UMX.cast<SourcelinkPattern, RepoPathSegment> pathPattern), document) else None

let private tryGetUrlWithExactMatch
  (pathPattern: string<SourcelinkPattern>)
  (urlPattern: string<Url>)
  (document: Document)
  =
  if (UMX.untag pathPattern)
       .Equals(UMX.untag document.Name, System.StringComparison.Ordinal) then
    Some(urlPattern, normalizeRepoPath (UMX.cast<SourcelinkPattern, RepoPathSegment> pathPattern), document)
  else
    None
"""
        { config with IndentSize = 2 }
    |> prepend newline
    |> should
        equal
        """
let private tryGetUrlWithExactMatch
  (pathPattern: string<SourcelinkPattern>)
  (urlPattern: string<Url>)
  (document: Document)
  =
  if
    (UMX.untag pathPattern)
      .Equals(UMX.untag document.Name, System.StringComparison.Ordinal)
  then
    Some(urlPattern, normalizeRepoPath (UMX.cast<SourcelinkPattern, RepoPathSegment> pathPattern), document)
  else
    None

let private tryGetUrlWithExactMatch
  (pathPattern: string<SourcelinkPattern>)
  (urlPattern: string<Url>)
  (document: Document)
  =
  if
    (UMX.untag pathPattern)
      .Equals(UMX.untag document.Name, System.StringComparison.Ordinal)
  then
    Some(urlPattern, normalizeRepoPath (UMX.cast<SourcelinkPattern, RepoPathSegment> pathPattern), document)
  else
    None
"""

[<Test>]
let ``infix equals expression in if condition expression, 1241`` () =
    formatSourceString
        """
namespace SomeNamespace

module SomeModule =

    let SomeFunc () =
        let someLocalFunc someVeryLooooooooooooooooooooooooooooooooooooooooooooooooongParam =
            async {
                if (someVeryLooooooooooooooooooooooooooooooooooooooooooooooooongParam = 1) then
                    return failwith "xxx"
            }
        ()

"""
        config
    |> prepend newline
    |> should
        equal
        """
namespace SomeNamespace

module SomeModule =

    let SomeFunc () =
        let someLocalFunc someVeryLooooooooooooooooooooooooooooooooooooooooooooooooongParam =
            async {
                if (someVeryLooooooooooooooooooooooooooooooooooooooooooooooooongParam = 1) then
                    return failwith "xxx"
            }

        ()
"""

[<Test>]
let ``multiline function application inside parenthesis in if expression, 1374`` () =
    formatSourceString
        """
module UtxoCoinAccount =
    let internal SendPayment
        (account: NormalUtxoAccount)
        (txMetadata: TransactionMetadata)
        (destination: string)
        (amount: TransferAmount)
        (password: string)
        =
        if (baseAccount.PublicAddress.Equals (destination, StringComparison.InvariantCultureIgnoreCase)) then
            raise DestinationEqualToOrigin
"""
        { config with MaxLineLength = 80 }
    |> prepend newline
    |> should
        equal
        """
module UtxoCoinAccount =
    let internal SendPayment
        (account: NormalUtxoAccount)
        (txMetadata: TransactionMetadata)
        (destination: string)
        (amount: TransferAmount)
        (password: string)
        =
        if
            (baseAccount.PublicAddress.Equals(
                destination,
                StringComparison.InvariantCultureIgnoreCase
            ))
        then
            raise DestinationEqualToOrigin
"""

[<Test>]
let ``multiline function application inside parenthesis in if expression must remain idempotent, 1349`` () =
    formatSourceString
        """
// Original input:
let x =
    if not (f aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa) then
        1
    else
        2

// Formatted output of the above, in for a second format:
let x =
    if (not (
            f aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
        )) then
        1
    else
        2
"""
        config
    |> prepend newline
    |> should
        equal
        """
// Original input:
let x =
    if
        not (
            f aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
        )
    then
        1
    else
        2

// Formatted output of the above, in for a second format:
let x =
    if
        (not (
            f aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa
        ))
    then
        1
    else
        2
"""

[<Test>]
let ``multiline pattern match inside if expression, 1481`` () =
    formatSourceString
        """
namespace AltCover

open System
open System.Diagnostics.CodeAnalysis
open System.IO
open System.Reflection
open System.Xml
open System.Xml.Linq
open System.Xml.Schema
open System.Xml.XPath

module Configuration =
  let SaveSchemaDir (s : string) =
    let file, config = ensureFile()

    let node =
      config.XPathSelectElements("AltCover.Visualizer")
      |> Seq.toList
      |> Seq.head
    if match (node.Attribute(XName.Get "GSettingsSchemaDir"), String.IsNullOrWhiteSpace s) with
       | (null, false) ->
           node.Add(XAttribute(XName.Get "GSettingsSchemaDir", s))
           true
       | (a, false) ->
           a.Value <- s
           true
       | (null, true) -> false
       | (a, true) ->
           a.Remove()
           true
    then config.Save file
"""
        { config with
            MaxInfixOperatorExpression = 50 }
    |> prepend newline
    |> should
        equal
        """
namespace AltCover

open System
open System.Diagnostics.CodeAnalysis
open System.IO
open System.Reflection
open System.Xml
open System.Xml.Linq
open System.Xml.Schema
open System.Xml.XPath

module Configuration =
    let SaveSchemaDir (s: string) =
        let file, config = ensureFile ()

        let node =
            config.XPathSelectElements("AltCover.Visualizer")
            |> Seq.toList
            |> Seq.head

        if
            match (node.Attribute(XName.Get "GSettingsSchemaDir"), String.IsNullOrWhiteSpace s) with
            | (null, false) ->
                node.Add(XAttribute(XName.Get "GSettingsSchemaDir", s))
                true
            | (a, false) ->
                a.Value <- s
                true
            | (null, true) -> false
            | (a, true) ->
                a.Remove()
                true
        then
            config.Save file
"""

[<Test>]
let ``multiline pattern match inside if expression, match bang`` () =
    formatSourceString
        """
namespace AltCover

open System
open System.Diagnostics.CodeAnalysis
open System.IO
open System.Reflection
open System.Xml
open System.Xml.Linq
open System.Xml.Schema
open System.Xml.XPath

module Configuration =
  let SaveSchemaDir (s : string) =
    let file, config = ensureFile()

    let node =
      config.XPathSelectElements("AltCover.Visualizer")
      |> Seq.toList
      |> Seq.head
    if match! (node.Attribute(XName.Get "GSettingsSchemaDir"), String.IsNullOrWhiteSpace s) with
       | (null, false) ->
           node.Add(XAttribute(XName.Get "GSettingsSchemaDir", s))
           true
       | (a, false) ->
           a.Value <- s
           true
       | (null, true) -> false
       | (a, true) ->
           a.Remove()
           true
    then config.Save file
"""
        { config with
            MaxInfixOperatorExpression = 50 }
    |> prepend newline
    |> should
        equal
        """
namespace AltCover

open System
open System.Diagnostics.CodeAnalysis
open System.IO
open System.Reflection
open System.Xml
open System.Xml.Linq
open System.Xml.Schema
open System.Xml.XPath

module Configuration =
    let SaveSchemaDir (s: string) =
        let file, config = ensureFile ()

        let node =
            config.XPathSelectElements("AltCover.Visualizer")
            |> Seq.toList
            |> Seq.head

        if
            match! (node.Attribute(XName.Get "GSettingsSchemaDir"), String.IsNullOrWhiteSpace s) with
            | (null, false) ->
                node.Add(XAttribute(XName.Get "GSettingsSchemaDir", s))
                true
            | (a, false) ->
                a.Value <- s
                true
            | (null, true) -> false
            | (a, true) ->
                a.Remove()
                true
        then
            config.Save file
"""

[<Test>]
let ``multiline yield bang in then expression, 1185`` () =
    formatSourceString
        """
let lessonsForm (f:ValidatedForm<Request.CreateLessons>) dispatch =

    Html.div [
        Bulma.field.div [
            Bulma.fieldBody [
                Bulma.button.button [
                    color.isPrimary
                    prop.text "Přidat lekce"
                    if f.IsLoading then yield! [ button.isLoading; prop.disabled true ]
                    prop.onClick (fun _ -> CreateLessons |> dispatch)
                ]
            ]
        ]
    ]
"""
        config
    |> prepend newline
    |> should
        equal
        """
let lessonsForm (f: ValidatedForm<Request.CreateLessons>) dispatch =

    Html.div
        [ Bulma.field.div
              [ Bulma.fieldBody
                    [ Bulma.button.button
                          [ color.isPrimary
                            prop.text "Přidat lekce"
                            if f.IsLoading then
                                yield! [ button.isLoading; prop.disabled true ]
                            prop.onClick (fun _ -> CreateLessons |> dispatch) ] ] ] ]
"""

[<Test>]
let ``infix operation with long function call, 1564`` () =
    formatSourceString
        """
if Uri.Compare(foo, bar, UriComponents.Host ||| UriComponents.Path, UriFormat.UriEscaped, StringComparison.CurrentCulture) = 0 then
    ()
else ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
if
    Uri.Compare(
        foo,
        bar,
        UriComponents.Host ||| UriComponents.Path,
        UriFormat.UriEscaped,
        StringComparison.CurrentCulture
    ) = 0
then
    ()
else
    ()
"""

[<Test>]
let ``multiline infix expression in if expression, 1584`` () =
    formatSourceString
        """
        if sourceCode.EndsWith("\n")
               && not
                  <| formattedSourceCode.EndsWith(Environment.NewLine) then
                return formattedSourceCode + Environment.NewLine
            elif
                not <| sourceCode.EndsWith("\n")
                && formattedSourceCode.EndsWith(Environment.NewLine)
            then
                return formattedSourceCode.TrimEnd('\r', '\n')
            else
                return formattedSourceCode
"""
        { config with
            MaxInfixOperatorExpression = 50 }
    |> prepend newline
    |> should
        equal
        """
if
    sourceCode.EndsWith("\n")
    && not
       <| formattedSourceCode.EndsWith(Environment.NewLine)
then
    return formattedSourceCode + Environment.NewLine
elif
    not <| sourceCode.EndsWith("\n")
    && formattedSourceCode.EndsWith(Environment.NewLine)
then
    return formattedSourceCode.TrimEnd('\r', '\n')
else
    return formattedSourceCode
"""

[<Test>]
[<Ignore "Tracked in https://github.com/fsprojects/fantomas/issues/1588">]
let ``line comment before multiline if expression, 1588`` () =
    formatSourceString
        """
            if
              // Don't support implicit [<ReflectedDefinition>] on generated members, except the implicit members
              // for 'let' bound functions in classes.
              (not v.IsCompilerGenerated || v.IsIncrClassGeneratedMember) &&

              (// Check the attributes on any enclosing module
               env.reflect ||
               // Check the attributes on the value
               HasFSharpAttribute g g.attrib_ReflectedDefinitionAttribute v.Attribs ||
               // Also check the enclosing type for members - for historical reasons, in the TAST member values
               // are stored in the entity that encloses the type, hence we will not have noticed the ReflectedDefinition
               // on the enclosing type at this point.
               HasFSharpAttribute g g.attrib_ReflectedDefinitionAttribute v.TopValDeclaringEntity.Attribs) then

                ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
if
    // Don't support implicit [<ReflectedDefinition>] on generated members, except the implicit members
    // for 'let' bound functions in classes.
    (not v.IsCompilerGenerated
     || v.IsIncrClassGeneratedMember)
    &&

    (env // Check the attributes on any enclosing module
     .reflect
     ||
     // Check the attributes on the value
     HasFSharpAttribute g g.attrib_ReflectedDefinitionAttribute v.Attribs
     ||
     // Also check the enclosing type for members - for historical reasons, in the TAST member values
     // are stored in the entity that encloses the type, hence we will not have noticed the ReflectedDefinition
     // on the enclosing type at this point.
     HasFSharpAttribute g g.attrib_ReflectedDefinitionAttribute v.TopValDeclaringEntity.Attribs)
then

    ()
"""

[<Test>]
let ``comment after then of else if, 1606`` () =
    formatSourceString
        """
type internal Foo private () =
    static member Bar : int option =
        if thing = 1 then
            printfn "hi"
        else if
            veryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryLong |> Seq.forall (fun (u : VeryVeryVeryVeryVeryVeryVeryLong) -> u.Length = 0) //
            then
              printfn "hi"
        else failwith ""

type internal Foo2 private () =
    static member Bar : int option =
        if thing = 1 then
            printfn "hi"
        else if veryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryLong
                |> Seq.forall (fun (u: VeryVeryVeryVeryVeryVeryVeryLong) -> u.Length = 0) //
        then
            printfn "hi"
        else
            failwith ""
"""
        config
    |> prepend newline
    |> should
        equal
        """
type internal Foo private () =
    static member Bar: int option =
        if thing = 1 then
            printfn "hi"
        else if
            veryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryLong
            |> Seq.forall (fun (u: VeryVeryVeryVeryVeryVeryVeryLong) -> u.Length = 0) //
        then
            printfn "hi"
        else
            failwith ""

type internal Foo2 private () =
    static member Bar: int option =
        if thing = 1 then
            printfn "hi"
        else if
            veryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryLong
            |> Seq.forall (fun (u: VeryVeryVeryVeryVeryVeryVeryLong) -> u.Length = 0) //
        then
            printfn "hi"
        else
            failwith ""
"""

[<Test>]
let ``multiline else if expression with nested if/then/else in body`` () =
    formatSourceString
        """
if result.LaunchSuccess && result.ExitCode = 0 then
    Ok r
else if result.ExitCode = 1 then
    let stdout, stderr =
        output
        |> List.map
            (function
            | StdErr e -> Error e
            | StdOut l -> Ok l)
        |> Result.partition

    if not stderr.IsEmpty then
        failwithf "Got stderr bad bad bad"

    match stdout with
    | [] -> failwithf "Got no stdout :("
    | xs when
        xs
        |> List.exists (fun i -> i.Contains "magic string goes here!")
        ->
        Error(Ok r)
    | _ -> Error(Error r)
else
    failwith ""
"""
        { config with
            MaxInfixOperatorExpression = 50 }
    |> prepend newline
    |> should
        equal
        """
if result.LaunchSuccess && result.ExitCode = 0 then
    Ok r
else if result.ExitCode = 1 then
    let stdout, stderr =
        output
        |> List.map (function
            | StdErr e -> Error e
            | StdOut l -> Ok l)
        |> Result.partition

    if not stderr.IsEmpty then
        failwithf "Got stderr bad bad bad"

    match stdout with
    | [] -> failwithf "Got no stdout :("
    | xs when
        xs
        |> List.exists (fun i -> i.Contains "magic string goes here!")
        ->
        Error(Ok r)
    | _ -> Error(Error r)
else
    failwith ""
"""

[<Test>]
let ``multiline elif expression with nested if/then/else in body`` () =
    formatSourceString
        """
if result.LaunchSuccess && result.ExitCode = 0 then
    Ok r
elif result.ExitCode = 1 then
    let stdout, stderr =
        output
        |> List.map
            (function
            | StdErr e -> Error e
            | StdOut l -> Ok l)
        |> Result.partition

    if not stderr.IsEmpty then
        failwithf "Got stderr bad bad bad"

    match stdout with
    | [] -> failwithf "Got no stdout :("
    | xs when
        xs
        |> List.exists (fun i -> i.Contains "magic string goes here!")
        ->
        Error(Ok r)
    | _ -> Error(Error r)
else
    failwith ""
"""
        { config with
            MaxInfixOperatorExpression = 50 }
    |> prepend newline
    |> should
        equal
        """
if result.LaunchSuccess && result.ExitCode = 0 then
    Ok r
elif result.ExitCode = 1 then
    let stdout, stderr =
        output
        |> List.map (function
            | StdErr e -> Error e
            | StdOut l -> Ok l)
        |> Result.partition

    if not stderr.IsEmpty then
        failwithf "Got stderr bad bad bad"

    match stdout with
    | [] -> failwithf "Got no stdout :("
    | xs when
        xs
        |> List.exists (fun i -> i.Contains "magic string goes here!")
        ->
        Error(Ok r)
    | _ -> Error(Error r)
else
    failwith ""
"""

[<Test>]
let ``multiple multiline elifs`` () =
    formatSourceString
        """
        if startWithMember sel then
            (String.Join(String.Empty, "type T = ", Environment.NewLine, String(' ', startCol), sel), TypeMember)
        elif String.startsWithOrdinal "and" (sel.TrimStart()) then
            // Replace "and" by "type" or "let rec"
            if startLine = endLine then
                (pattern.Replace(sel, replacement, 1), p)
            else
                (String(' ', startCol)
                 + pattern.Replace(sel, replacement, 1),
                 p)
        elif startLine = endLine then
            (sel, Nothing)
        else
            failAndExit ()
"""
        { config with
            MaxInfixOperatorExpression = 50 }
    |> prepend newline
    |> should
        equal
        """
if startWithMember sel then
    (String.Join(String.Empty, "type T = ", Environment.NewLine, String(' ', startCol), sel), TypeMember)
elif String.startsWithOrdinal "and" (sel.TrimStart()) then
    // Replace "and" by "type" or "let rec"
    if startLine = endLine then
        (pattern.Replace(sel, replacement, 1), p)
    else
        (String(' ', startCol)
         + pattern.Replace(sel, replacement, 1),
         p)
elif startLine = endLine then
    (sel, Nothing)
else
    failAndExit ()
"""

[<Test>]
let ``multiline but not that special if expression`` () =
    formatSourceString
        """
if
    List.exists
        (function
        | CompExpr _ -> true
        | _ -> false)
        es
then
    shortExpression ctx
else
    expressionFitsOnRestOfLine shortExpression longExpression ctx
"""
        config
    |> prepend newline
    |> should
        equal
        """
if
    List.exists
        (function
        | CompExpr _ -> true
        | _ -> false)
        es
then
    shortExpression ctx
else
    expressionFitsOnRestOfLine shortExpression longExpression ctx
"""

[<Test>]
let ``multiline dotget chain in if expression, 1712`` () =
    formatSourceString
        """
module Foo =
    let bar =
        if Regex("long long long long long long long long long").Match(s).Success then
            None
        else Some "hi"
"""
        { config with
            MaxDotGetExpressionWidth = 50 }
    |> prepend newline
    |> should
        equal
        """
module Foo =
    let bar =
        if
            Regex("long long long long long long long long long")
                .Match(s)
                .Success
        then
            None
        else
            Some "hi"
"""

[<Test>]
let ``multiline if/then/else followed by infix, 1757`` () =
    formatSourceString
        """
           let name =
                if typ.GenericParameter.IsSolveAtCompileTime then "^" else "'"
                + typ.GenericParameter.Name
"""
        { config with IndentSize = 2 }
    |> prepend newline
    |> should
        equal
        """
let name =
  (if typ.GenericParameter.IsSolveAtCompileTime then
     "^"
   else
     "'")
  + typ.GenericParameter.Name
"""

[<Test>]
let ``multiline if/then/else followed by infix, no parenthesis needed`` () =
    formatSourceString
        """
           let name =
                if typ.GenericParameter.IsSolveAtCompileTime then "^" else "'"
                + typ.GenericParameter.Name
"""
        { config with
            IndentSize = 2
            MaxIfThenElseShortWidth = 9000 }
    |> prepend newline
    |> should
        equal
        """
let name =
  if typ.GenericParameter.IsSolveAtCompileTime then "^" else "'"
  + typ.GenericParameter.Name
"""

[<Test>]
let ``short function application in infix expression, 1795`` () =
    formatSourceString
        """
if
        FOOQueryUserToken (uint32 activeSessionId, &token) <> 0u
      then
        Some x
      else
        None
"""
        { config with
            MaxInfixOperatorExpression = 50 }
    |> prepend newline
    |> should
        equal
        """
if
    FOOQueryUserToken(uint32 activeSessionId, &token)
    <> 0u
then
    Some x
else
    None
"""

[<Test>]
let ``short function application in infix expression, reversed`` () =
    formatSourceString
        """
if
      0u   <> FOOQueryUserToken (uint32 activeSessionId, &token)
      then
        Some x
      else
        None
"""
        { config with
            MaxInfixOperatorExpression = 50 }
    |> prepend newline
    |> should
        equal
        """
if
    0u
    <> FOOQueryUserToken(uint32 activeSessionId, &token)
then
    Some x
else
    None
"""

[<Test>]
let ``multiline if expression with multiline infix operations, 1775`` () =
    formatSourceString
        """
if hasClassAttr && not (match k with SynTypeDefnKind.Class -> true | _ -> false) || 
    hasMeasureAttr && not (match k with SynTypeDefnKind.Class | SynTypeDefnKind.Abbrev | SynTypeDefnKind.Opaque -> true | _ -> false) || 
    hasInterfaceAttr && not (match k with SynTypeDefnKind.Interface -> true | _ -> false) || 
    hasStructAttr && not (match k with SynTypeDefnKind.Struct | SynTypeDefnKind.Record | SynTypeDefnKind.Union -> true | _ -> false) then 
    error(Error(FSComp.SR.tcKindOfTypeSpecifiedDoesNotMatchDefinition(), m))
k
"""
        config
    |> prepend newline
    |> should
        equal
        """
if
    hasClassAttr
    && not (
        match k with
        | SynTypeDefnKind.Class -> true
        | _ -> false
    )
    || hasMeasureAttr
       && not (
           match k with
           | SynTypeDefnKind.Class
           | SynTypeDefnKind.Abbrev
           | SynTypeDefnKind.Opaque -> true
           | _ -> false
       )
    || hasInterfaceAttr
       && not (
           match k with
           | SynTypeDefnKind.Interface -> true
           | _ -> false
       )
    || hasStructAttr
       && not (
           match k with
           | SynTypeDefnKind.Struct
           | SynTypeDefnKind.Record
           | SynTypeDefnKind.Union -> true
           | _ -> false
       )
then
    error (Error(FSComp.SR.tcKindOfTypeSpecifiedDoesNotMatchDefinition (), m))

k
"""

[<Test>]
let ``trivia after else in else if in multiline condition, 2449 `` () =
    formatSourceString
        """
if true then
    printfn "a"
else // this is a comment
    if
        false
    then
        printfn "b"
    else
        printfn "c"

printfn "d"
"""
        config
    |> prepend newline
    |> should
        equal
        """
if true then
    printfn "a"
else if // this is a comment
    false
then
    printfn "b"
else
    printfn "c"

printfn "d"
"""

[<Test>]
let ``duplicate newline after shifting trivia between else if, 2752`` () =
    formatSourceString
        """
// check for match
if nargTs <> haveArgTs.Length then
    false (* method argument length mismatch *)
else

// If a known-number-of-arguments-including-object-argument has been given then check that
if
    (match knownArgCount with
        | ValueNone -> false
        | ValueSome n -> n <> (if methInfo.IsStatic then 0 else 1) + nargTs)
then
    false
else

    let res = typesEqual (resT :: argTs) (haveResT :: haveArgTs)
    res
"""
        { config with
            KeepMaxNumberOfBlankLines = 1 }
    |> prepend newline
    |> should
        equal
        """
// check for match
if nargTs <> haveArgTs.Length then
    false (* method argument length mismatch *)
else if

    // If a known-number-of-arguments-including-object-argument has been given then check that
    (match knownArgCount with
     | ValueNone -> false
     | ValueSome n -> n <> (if methInfo.IsStatic then 0 else 1) + nargTs)
then
    false
else

    let res = typesEqual (resT :: argTs) (haveResT :: haveArgTs)
    res
"""
