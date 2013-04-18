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
    |> should equal """type DGMLClass() = 
    class
        let mutable currentState = System.String.Empty
    end
"""

// the current behavior results in a compile error since parens in (makeAsync) is moved to a wrong place
[<Test>]
let ``should keep parens in class inheritance in the right place``() =
    formatSourceString false """type StateMachine(makeAsync) as this = class
    inherit DGMLClass()

    let functions = System.Collections.Generic.Dictionary<string, IState>()
    """ config
    |> should equal """type StateMachine(makeAsync) as this = 
    class
        inherit DGMLClass()
        let functions = System.Collections.Generic.Dictionary<string, IState>()
    end
"""