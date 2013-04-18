module Fantomas.Tests.ClassTests

open NUnit.Framework
open FsUnit

open Fantomas.CodeFormatter
open Fantomas.Tests.TestHelper


// the current behavior results in a compile error since parens in "DGMLClass()" are moved to a wrong place
[<Test>]
let ``should keep parens in class definition in the right place``() =
    formatSourceString false """type DGMLClass() = class   
    let mutable currentState = System.String.Empty
    """ config
    |> should equal """type DGMLClass() = class   
    let mutable currentState = System.String.Empty
"""