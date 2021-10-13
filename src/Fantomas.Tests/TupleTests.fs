module Fantomas.Tests.TupleTests

open NUnit.Framework
open FsUnit

open Fantomas.Tests.TestHelper

[<Test>]
let ``tuple with lambda should add parenthesis`` () =
    formatSourceString
        false
        """
let private carouselSample =
    FunctionComponent.Of<obj>(fun _ ->
        fragment [] []
    ,"CarouselSample")
"""
        { config with
              MaxValueBindingWidth = 75 }
    |> should
        equal
        """let private carouselSample = FunctionComponent.Of<obj>((fun _ -> fragment [] []), "CarouselSample")
"""

[<Test>]
let ``multiline item in tuple - paren on its line`` () =
    formatSourceString
        false
        """(x,
 if longExpressionMakingTheIfElseMultiline && a then answerWhenTheConditionIsTrue
 else answerWhenTheConditionIsFalse)
"""
        config
    |> prepend newline
    |> should
        equal
        """
(x,
 if longExpressionMakingTheIfElseMultiline && a then
     answerWhenTheConditionIsTrue
 else
     answerWhenTheConditionIsFalse)
"""

[<Test>]
let ``multiline SynPat.Tuple should have parenthesis, 824`` () =
    formatSourceString
        false
        """
namespace GWallet.Backend.Tests

module Foo =

    let someOtherFunc () =
        let var1withAVeryLongLongLongLongLongLongName, var2withAVeryLongLongLongLongLongLongName =
            someFunc 1, someFunc 2

        ()
"""
        { config with MaxLineLength = 80 }
    |> prepend newline
    |> should
        equal
        """
namespace GWallet.Backend.Tests

module Foo =

    let someOtherFunc () =
        let (var1withAVeryLongLongLongLongLongLongName,
             var2withAVeryLongLongLongLongLongLongName) =
            someFunc 1, someFunc 2

        ()
"""

[<Test>]
let ``multiline SynPat.Tuple with existing parenthesis should not add additional parenthesis`` () =
    formatSourceString
        false
        """
namespace GWallet.Backend.Tests

module Foo =

    let someOtherFunc () =
        let (var1withAVeryLongLongLongLongLongLongName,
             var2withAVeryLongLongLongLongLongLongName) =
            someFunc 1, someFunc 2

        ()
"""
        { config with MaxLineLength = 80 }
    |> prepend newline
    |> should
        equal
        """
namespace GWallet.Backend.Tests

module Foo =

    let someOtherFunc () =
        let (var1withAVeryLongLongLongLongLongLongName,
             var2withAVeryLongLongLongLongLongLongName) =
            someFunc 1, someFunc 2

        ()
"""

[<Test>]
let ``long tuple containing match must be formatted with comma on the next line`` () =
    formatSourceString
        false
        """
match "Hello" with
    | "first" -> 1
    | "second" -> 2
    , []
"""
        { config with MaxLineLength = 80 }
    |> prepend newline
    |> should
        equal
        """
match "Hello" with
| "first" -> 1
| "second" -> 2
, []
"""

[<Test>]
let ``long tuple containing lambda must be formatted with comma on the next line`` () =
    formatSourceString
        false
        """
fun x ->
    let y = x + 3
    if y > 2 then y + 1 else y - 1
, []
"""
        { config with MaxLineLength = 80 }
    |> prepend newline
    |> should
        equal
        """
fun x ->
    let y = x + 3
    if y > 2 then y + 1 else y - 1
, []
"""

[<Test>]
let ``all lines should start with comma if tuple contains match`` () =
    formatSourceString
        false
        """
match "Hello" with
    | "first" -> 1
    | "second" -> 2
    , []
    , "Hello"
    , 1
"""
        { config with MaxLineLength = 80 }
    |> prepend newline
    |> should
        equal
        """
match "Hello" with
| "first" -> 1
| "second" -> 2
, []
, "Hello"
, 1
"""

[<Test>]
let ``add comma at the back when match is not follow by another expression in tuple`` () =
    formatSourceString
        false
        """
1
, "Hello"
, match "Hello" with
  | "first" -> 1
  | "second" -> 2
  | _ -> 3
"""
        config
    |> prepend newline
    |> should
        equal
        """
1,
"Hello",
match "Hello" with
| "first" -> 1
| "second" -> 2
| _ -> 3
"""

[<Test>]
let ``infix lambda followed by constant, 966`` () =
    formatSourceString
        false
        """
let f =
    5
    |> fun i -> i + 1
    , 6
"""
        { config with
              MaxInfixOperatorExpression = 5 }
    |> prepend newline
    |> should
        equal
        """
let f =
    5
    |> fun i -> i + 1
    , 6
"""

[<Test>]
let ``destructed tuple with comment after equals`` () =
    formatSourceString
        false
        """
        let var1withAVeryLongLongLongLongLongLongName, var2withAVeryLongLongLongLongLongLongName = // foo
            someFunc 1, someFunc 2
"""
        { config with MaxLineLength = 60 }
    |> prepend newline
    |> should
        equal
        """
let (var1withAVeryLongLongLongLongLongLongName,
     var2withAVeryLongLongLongLongLongLongName) = // foo
    someFunc 1, someFunc 2
"""

[<Test>]
let ``tuple with if/then/else, 1319`` () =
    formatSourceString
        false
        """
let y =
    if String.IsNullOrWhiteSpace(args) then ""
    elif args.StartsWith("(") then args
    elif v.CurriedParameterGroups.Count > 1 && (not verboseMode) then " " + args
    else sprintf "(%s)" args
    , namesWithIndices
"""
        config
    |> prepend newline
    |> should
        equal
        """
let y =
    (if String.IsNullOrWhiteSpace(args) then
         ""
     elif args.StartsWith("(") then
         args
     elif v.CurriedParameterGroups.Count > 1
          && (not verboseMode) then
         " " + args
     else
         sprintf "(%s)" args),
    namesWithIndices
"""

[<Test>]
let ``comment on first tuple argument is preserved`` () =
    formatSourceString
        false
        """
let func (a, b) = a + b

func(
        // abc
        0,
        1
)
"""
        config
    |> prepend newline
    |> should
        equal
        """
let func (a, b) = a + b

func (
    // abc
    0,
    1
)
"""

[<Test>]
let ``comment trivias on tuple arguments are preserved`` () =
    formatSourceString
        false
        """
let func (a, b) = a + b

func(
        // abc
        0, // def
        // ghi
        1 // jkl
        // mno
)
"""
        config
    |> prepend newline
    |> should
        equal
        """
let func (a, b) = a + b

func (
    // abc
    0, // def
    // ghi
    1 // jkl
        // mno
)
"""
