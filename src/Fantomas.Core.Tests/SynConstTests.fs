module Fantomas.Core.Tests.SynConstTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelpers

// https://fsharp.github.io/FSharp.Compiler.Service/reference/fsharp-compiler-ast-synconst.html
[<Test>]
let ``all known SynConst with trivia`` () =
    formatSourceString
        false
        """
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
"""
        config
    |> should
        equal
        """let a = true
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
    formatSourceString
        false
        """
namespace SomeNamespace

module SomeModule =
    let backspace = '\b'
    let formFeed = '\f'
"""
        config
    |> prepend newline
    |> should
        equal
        """
namespace SomeNamespace

module SomeModule =
    let backspace = '\b'
    let formFeed = '\f'
"""

[<Test>]
let ``escape unicode null, 632`` () =
    formatSourceString
        false
        """let nulchar = '\u0000'
let nullstr = "\u0000"
"""
        config
    |> prepend newline
    |> should
        equal
        """
let nulchar = '\u0000'
let nullstr = "\u0000"
"""

[<Test>]
let ``line comment after custom measure type, 598`` () =
    formatSourceString
        false
        """namespace Krach

module Runner =

    let mPerSecond = 1000<m/second> // foo

    [<Measure>]
    type ProcessId =
        class
        end
"""
        config
    |> prepend newline
    |> should
        equal
        """
namespace Krach

module Runner =

    let mPerSecond = 1000<m / second> // foo

    [<Measure>]
    type ProcessId =
        class
        end
"""

[<Test>]
let ``formats negate of RationalConst without leading space in front of the const, 2265`` () =
    formatSourceString
        false
        """
type becquerel = second^-1
"""
        config
    |> prepend newline
    |> should
        equal
        """
type becquerel = second^-1
"""

[<Test>]
let ``array literals of BigInteger, 682`` () =
    formatSourceString false "let input = [| 1I;0I;-1I |]" config
    |> should
        equal
        "let input = [| 1I; 0I; -1I |]
"

[<Test>]
let ``negative single floating-point number, 785`` () =
    formatSourceString false "let num = -3.213f" config
    |> should
        equal
        "let num = -3.213f
"

[<Test>]
let ``string content ends at string token, 646`` () =
    formatSourceString
        false
        """"Yarn" ==> "Format"

"Yarn" ==> "CheckCodeFormat"

Target.runOrDefault "CheckCodeFormat"
"""
        config
    |> prepend newline
    |> should
        equal
        """
"Yarn" ==> "Format"

"Yarn" ==> "CheckCodeFormat"

Target.runOrDefault "CheckCodeFormat"
"""

[<Test>]
let ``hexadecimal numbers in match clause should be preserved, 995`` () =
    formatSourceString
        false
        """
let f (a: int) =
    match a, b with
    | 0x55 -> Some ()
    | _ -> None
"""
        config
    |> prepend newline
    |> should
        equal
        """
let f (a: int) =
    match a, b with
    | 0x55 -> Some()
    | _ -> None
"""

[<Test>]
let ``preserve underscore in int64, 1120`` () =
    formatSourceString false "let x = 60_000L" config
    |> should
        equal
        "let x = 60_000L
"

[<Test>]
let ``spaces before hash define inside string, 1290`` () =
    formatSourceString
        false
        "
[<Test>]
let ``defines inside string, escaped quote`` () =
    let source = \"
let a = \\\"\\\\\\\"
#if FOO
  #if BAR
  #endif
#endif
\\\"
\"

    getDefines source == []
"
        config
    |> prepend newline
    |> should
        equal
        "
[<Test>]
let ``defines inside string, escaped quote`` () =
    let source =
        \"
let a = \\\"\\\\\\\"
#if FOO
  #if BAR
  #endif
#endif
\\\"
\"

    getDefines source == []
"

[<Test>]
let ``escaped single quote`` () =
    formatSourceString
        false
        """
let x = !-(sprintf "\'%s\'" escapedChar)
"""
        config
    |> prepend newline
    |> should
        equal
        """
let x = !-(sprintf "\'%s\'" escapedChar)
"""

[<Test>]
let ``character literal patterns should be preserved, 1372`` () =
    formatSourceString
        false
        """
let f (c: char) =
    match c with
    | '\''
    | '\"'
    | '\x00'
    | '\u0000'
    | _ -> ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
let f (c: char) =
    match c with
    | '\''
    | '\"'
    | '\x00'
    | '\u0000'
    | _ -> ()
"""

[<Test>]
let ``hex escape in string literal should be preserved, 1508`` () =
    formatSourceString
        false
        """let hexEscape = "\x00"
"""
        config
    |> should
        equal
        """let hexEscape = "\x00"
"""

[<Test>]
let ``trivia after SynConst.Boolean, 1518`` () =
    formatSourceString
        false
        """
    match ast with
    | ParsedInput.SigFile _input ->
        // There is not much to explore in signature files
        true
    | ParsedInput.ImplFile input -> validateImplFileInput input

    match t with
    | TTuple _ -> not node.IsEmpty
    | TFun _ -> true // Fun is grouped by brackets inside 'genType astContext true t'
    | _ -> false

    let condition e =
        match e with
        | ElIf _
        | SynExpr.Lambda _ -> true
        | _ -> false // "if .. then .. else" have precedence over ","

    let x = 9
"""
        config
    |> prepend newline
    |> should
        equal
        """
match ast with
| ParsedInput.SigFile _input ->
    // There is not much to explore in signature files
    true
| ParsedInput.ImplFile input -> validateImplFileInput input

match t with
| TTuple _ -> not node.IsEmpty
| TFun _ -> true // Fun is grouped by brackets inside 'genType astContext true t'
| _ -> false

let condition e =
    match e with
    | ElIf _
    | SynExpr.Lambda _ -> true
    | _ -> false // "if .. then .. else" have precedence over ","

let x = 9
"""

[<Test>]
let ``trivia after SynConst.Char`` () =
    formatSourceString
        false
        """
let c = 'r' // meh
let x = 1
"""
        config
    |> prepend newline
    |> should
        equal
        """
let c = 'r' // meh
let x = 1
"""

[<Test>]
let ``trivia after SynConst.Bytes`` () =
    formatSourceString
        false
        """
let bytes = "meh"B // meh
let x = 1
"""
        config
    |> prepend newline
    |> should
        equal
        """
let bytes = "meh"B // meh
let x = 1
"""

[<Test>]
let ``bytes string with escaped character`` () =
    formatSourceString
        false
        """
let bytes = "meh\n"B
"""
        config
    |> prepend newline
    |> should
        equal
        """
let bytes = "meh\n"B
"""

[<Test>]
let ``trivia after SynConst.String, 1518`` () =
    formatSourceString
        false
        "
    let source = \"\"\"printfn foo

printfn bar\"\"\" // difference is the 4 spaces on line 188

    let x = 9
"
        config
    |> prepend newline
    |> should
        equal
        "
let source =
    \"\"\"printfn foo

printfn bar\"\"\" // difference is the 4 spaces on line 188

let x = 9
"

[<Test>]
let ``multiline string in let value binding, 1556`` () =
    formatSourceString
        false
        "
let foo = \"\"\"moo,
long
triple quotes string thing
\"\"\"
"
        config
    |> prepend newline
    |> should
        equal
        "
let foo =
    \"\"\"moo,
long
triple quotes string thing
\"\"\"
"

[<Test>]
let ``multiline string in let value binding, return type`` () =
    formatSourceString
        false
        "
let foo: string = \"\"\"moo,
long
triple quotes string thing
\"\"\"
"
        config
    |> prepend newline
    |> should
        equal
        "
let foo: string =
    \"\"\"moo,
long
triple quotes string thing
\"\"\"
"

[<Test>]
let ``multiline string in let function binding, no return type`` () =
    formatSourceString
        false
        "
let foo () = \"\"\"moo,
long
triple quotes string thing
\"\"\"
"
        config
    |> prepend newline
    |> should
        equal
        "
let foo () =
    \"\"\"moo,
long
triple quotes string thing
\"\"\"
"

[<Test>]
let ``multiline string in let function binding, return type`` () =
    formatSourceString
        false
        "
let foo () : string = \"\"\"moo,
long
triple quotes string thing
\"\"\"
"
        config
    |> prepend newline
    |> should
        equal
        "
let foo () : string =
    \"\"\"moo,
long
triple quotes string thing
\"\"\"
"

[<Test>]
let ``collect keyword string as separate trivia`` () =
    formatSourceString
        false
        """
__SOURCE_DIRECTORY__
"""
        config
    |> prepend newline
    |> should
        equal
        """
__SOURCE_DIRECTORY__
"""

[<Test>]
let ``escape sequences in strings are preserved`` () =
    formatSourceString
        false
        """let alert = "Hello\aWorld"
let backspace = "Hello\bWorld"
let formFeed = "Hello\fWorld"
let newline = "Hello\nWorld"
let carriageReturn = "Hello\rWorld"
let tab = "Hello\tWorld"
let verticalTab = "Hello\vWorld"
"""
        config
    |> should
        equal
        """let alert = "Hello\aWorld"
let backspace = "Hello\bWorld"
let formFeed = "Hello\fWorld"
let newline = "Hello\nWorld"
let carriageReturn = "Hello\rWorld"
let tab = "Hello\tWorld"
let verticalTab = "Hello\vWorld"
"""

[<Test>]
let ``concatenation of multi-line triple quote strings, 639`` () =
    formatSourceString
        false
        "
  let PrepareReadMe packingCopyright =
    let readme = Path.getFullName \"README.md\"
    let document = File.ReadAllText readme
    let markdown = Markdown()
    let docHtml = \"\"\"<?xml version=\"1.0\"  encoding=\"utf-8\"?>
<!DOCTYPE html>
<html lang=\"en\">
<head>
<title>AltCover README</title>
<style>
body, html {
color: #000; background-color: #eee;
font-family: 'Segoe UI', 'Open Sans', Calibri, verdana, helvetica, arial, sans-serif;
position: absolute; top: 0px; width: 50em;margin: 1em; padding:0;
}
a {color: #444; text-decoration: none; font-weight: bold;}
a:hover {color: #ecc;}
</style>
</head>
<body>
\"\"\"               + markdown.Transform document + \"\"\"
<footer><p style=\"text-align: center\">\"\"\" + packingCopyright + \"\"\"</p>
</footer>
</body>
</html>
\"\"\"
    let xmlform = XDocument.Parse docHtml
    let body = xmlform.Descendants(XName.Get \"body\")
    let eliminate = [ \"Continuous Integration\"; \"Building\"; \"Thanks to\" ]
    let keep = ref true

    let kill =
      body.Elements()
      |> Seq.map (fun x ->
           match x.Name.LocalName with
           | \"h2\" ->
               keep
               := (List.tryFind (fun e -> e = String.Concat(x.Nodes())) eliminate)
                  |> Option.isNone
           | \"footer\" -> keep := true
           | _ -> ()
           if !keep then None else Some x)
      |> Seq.toList
    kill
    |> Seq.iter (fun q ->
         match q with
         | Some x -> x.Remove()
         | _ -> ())
    let packable = Path.getFullName \"./_Binaries/README.html\"
    xmlform.Save packable
"
        { config with
            MaxDotGetExpressionWidth = 50
            MaxInfixOperatorExpression = 50
            MaxArrayOrListWidth = 40 }
    |> prepend newline
    |> should
        equal
        "
let PrepareReadMe packingCopyright =
    let readme = Path.getFullName \"README.md\"
    let document = File.ReadAllText readme
    let markdown = Markdown()

    let docHtml =
        \"\"\"<?xml version=\"1.0\"  encoding=\"utf-8\"?>
<!DOCTYPE html>
<html lang=\"en\">
<head>
<title>AltCover README</title>
<style>
body, html {
color: #000; background-color: #eee;
font-family: 'Segoe UI', 'Open Sans', Calibri, verdana, helvetica, arial, sans-serif;
position: absolute; top: 0px; width: 50em;margin: 1em; padding:0;
}
a {color: #444; text-decoration: none; font-weight: bold;}
a:hover {color: #ecc;}
</style>
</head>
<body>
\"\"\"
        + markdown.Transform document
        + \"\"\"
<footer><p style=\"text-align: center\">\"\"\"
        + packingCopyright
        + \"\"\"</p>
</footer>
</body>
</html>
\"\"\"

    let xmlform = XDocument.Parse docHtml
    let body = xmlform.Descendants(XName.Get \"body\")

    let eliminate =
        [ \"Continuous Integration\"
          \"Building\"
          \"Thanks to\" ]

    let keep = ref true

    let kill =
        body.Elements()
        |> Seq.map (fun x ->
            match x.Name.LocalName with
            | \"h2\" ->
                keep
                := (List.tryFind (fun e -> e = String.Concat(x.Nodes())) eliminate)
                   |> Option.isNone
            | \"footer\" -> keep := true
            | _ -> ()

            if !keep then None else Some x)
        |> Seq.toList

    kill
    |> Seq.iter (fun q ->
        match q with
        | Some x -> x.Remove()
        | _ -> ())

    let packable = Path.getFullName \"./_Binaries/README.html\"
    xmlform.Save packable
"

[<Test>]
let ``verbatim string in AST is preserved, 560`` () =
    formatAST
        false
        """
let s = @"\"
"""
        config
    |> prepend newline
    |> should
        equal
        """
let s = @"\"
"""

[<Test>]
let ``single digit constant`` () =
    formatSourceString
        false
        "1"
        { config with
            InsertFinalNewline = false }
    |> should equal "1"

[<Test>]
let ``left out lhs in SynMeasure.Divide should not be restored as SynMeasure.One, 2926`` () =
    formatSourceString
        false
        """
234</kg>
"""
        config
    |> prepend newline
    |> should
        equal
        """
234< / kg>
"""

[<Test>]
let ``explicit SynMeasure.One in SynMeasure.Divide should be preserved`` () =
    formatSourceString
        false
        """
234<1/kg>
"""
        config
    |> prepend newline
    |> should
        equal
        """
234<1 / kg>
"""

[<Test>]
let ``block comments in measure are lost or restored twice and in wrong place, 2927`` () =
    formatSourceString
        false
        """
234<(* foo *)kg(* bar *)>
"""
        config
    |> prepend newline
    |> should
        equal
        """
234< (* foo *) kg (* bar *) >
"""

[<Test>]
let ``block comment in Rational between numerator and / is lost, 2931`` () =
    formatSourceString
        false
        """
234<kg^(2(* foo *)/3)>
"""
        config
    |> prepend newline
    |> should
        equal
        """
234<kg^(2 (* foo *) /3)>
"""

[<Test>]
let ``block comment between ^- and exponent in SynMeasure.Power is lost, 2937`` () =
    formatSourceString
        false
        """
234<m^-(* bar *)2>
"""
        config
    |> prepend newline
    |> should
        equal
        """
234<m^- (* bar *) 2>
"""
