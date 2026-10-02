/// Inputs large enough that formatting them once overflowed the stack. What they format to does not
/// matter, only that formatting finishes.
module Fantomas.Core.Tests.StackOverflowTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelpers

[<Test>]
let ``very long triple-quoted strings do not cause the interpolated string active pattern to stack overflow, 1837`` () =
    let loremIpsum =
        String.replicate
            1000
            "Lorem ipsum dolor sit amet, consectetur adipiscing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua.\n\n"

    formatSourceString $"let value = \"\"\"%s{loremIpsum}\"\"\"" config
    |> should
        equal
        $"let value =
    \"\"\"%s{loremIpsum}\"\"\"
"

[<Test>]
let ``a huge amount of inner let bindings`` () =
    let sourceCode =
        List.init 1000 (fun i -> sprintf "    let x%i = %i\n    printfn \"%i\" x%i" i i i i)
        |> String.concat "\n"
        |> sprintf
            """module A.Whole.Lot.Of.InnerLetBindings

let v =
%s
"""

    let formatted = formatSourceString sourceCode config

    formatted |> should not' (equal EmptyString)

[<Test>]
let ``a huge amount of type declarations`` () =
    let sourceCode =
        List.init 1000 (sprintf "type FooBar%i = class end")
        |> String.concat "\n"
        |> sprintf
            """module A.Whole.Lot.Of.Types

%s
        """

    let formatted = formatSourceString sourceCode config

    // the result is less important here,
    // the point of this unit test is to verify if a stackoverflow problem at genModuleDeclList has been resolved.
    formatted |> should not' (equal EmptyString)

[<Test>]
let ``a huge amount of type declarations, signature file`` () =
    let sourceCode =
        List.init 1000 (sprintf "type FooBar%i = class end")
        |> String.concat "\n"
        |> sprintf
            """module A.Whole.Lot.Of.Types

%s
        """

    let formatted = formatSignatureString sourceCode config

    formatted |> should not' (equal EmptyString)

[<Test>]
let ``a huge amount of member bindings`` () =
    let sourceCode =
        List.init 1000 (sprintf "        member this.Bar%i () = ()")
        |> String.concat "\n"
        |> sprintf
            """module A.Whole.Lot.Of.MemberBindings

type FooBarry =
    interface Lorem with
%s
"""

    let formatted = formatSourceString sourceCode config

    formatted |> should not' (equal EmptyString)

[<Test>]
let ``a huge amount of member bindings, object expression`` () =
    let sourceCode =
        List.init 1000 (sprintf "        member this.Bar%i () = ()")
        |> String.concat "\n"
        |> sprintf
            """module A.Whole.Lot.Of.MemberBindings

let leBarry =
    { new SomeLargeInterface with
%s }
"""

    let formatted = formatSourceString sourceCode config

    formatted |> should not' (equal EmptyString)
