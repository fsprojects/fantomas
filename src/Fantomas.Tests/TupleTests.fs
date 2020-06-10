module Fantomas.Tests.TupleTests

open NUnit.Framework
open FsUnit

open Fantomas.Tests.TestHelper

[<Test>]
let ``tuple with lamba should add parenthesis`` () =
    formatSourceString false """
let private carouselSample =
    FunctionComponent.Of<obj>(fun _ ->
        fragment [] []
    ,"CarouselSample")
"""  ({ config with MaxValueBindingWidth = 75 })
    |> should equal """let private carouselSample = FunctionComponent.Of<obj>((fun _ -> fragment [] []), "CarouselSample")
"""

[<Test>]
let ``multiline item in tuple - paren on its line`` () =
    formatSourceString false """(x,
 if longExpressionMakingTheIfElseMultiline && a then answerWhenTheConditionIsTrue
 else answerWhenTheConditionIsFalse)
"""  config
    |> should equal """(x,
 (if longExpressionMakingTheIfElseMultiline && a
  then answerWhenTheConditionIsTrue
  else answerWhenTheConditionIsFalse))
"""