module Fantomas.Tests.AttributeTests

open NUnit.Framework
open FsUnit

open Fantomas.CodeFormatter
open Fantomas.Tests.TestHelper

// the current behavior results in an indentation error
[<Test>]
let ``should keep the attribute on top of the function``() =
    formatSourceString false """[<Extension>]
type Funcs = 
    [<Extension>]
    static member ToFunc (f: Action<_,_,_>) =
        Func<_,_,_,_>(fun a b c -> f.Invoke(a,b,c))
    """ config
    |> should equal """[<Extension>]
type Funcs = 
    [<Extension>]
    static member ToFunc(f : Action<_, _, _>) = 
        Func<_, _, _, _>(fun a b c -> f.Invoke(a, b, c))
"""
