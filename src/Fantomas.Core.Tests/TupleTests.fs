module Fantomas.Core.Tests.TupleTests

open NUnit.Framework
open FsUnit

open Fantomas.Core.Tests.TestHelpers

[<Test>]
let ``tuple with lambda should add parenthesis`` () =
    formatSourceString
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
        """let private carouselSample =
    FunctionComponent.Of<obj>((fun _ -> fragment [] []), "CarouselSample")
"""

[<Test>]
let ``multiline item in tuple - paren on its line`` () =
    formatSourceString
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
        """
let y =
    if String.IsNullOrWhiteSpace(args) then ""
    elif args.StartsWith("(") then args
    elif v.CurriedParameterGroups.Count > 1 && (not verboseMode) then " " + args
    else sprintf "(%s)" args
    , namesWithIndices
"""
        { config with
            MaxInfixOperatorExpression = 50 }
    |> prepend newline
    |> should
        equal
        """
let y =
    (if String.IsNullOrWhiteSpace(args) then
         ""
     elif args.StartsWith("(") then
         args
     elif
         v.CurriedParameterGroups.Count > 1
         && (not verboseMode)
     then
         " " + args
     else
         sprintf "(%s)" args),
    namesWithIndices
"""

[<Test>]
let ``comment on first tuple argument is preserved`` () =
    formatSourceString
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

[<Test>]
let ``comma should not move and change type signature, 2381`` () =
    formatSourceString
        """
let cts = new CancellationTokenSource()

let mb =
    MailboxProcessor.Start(
        fun inbox ->
            let rec messageLoop _ = async { return! messageLoop () }

            messageLoop ()
        , cts.Token
    )
"""
        config
    |> prepend newline
    |> should
        equal
        """
let cts = new CancellationTokenSource()

let mb =
    MailboxProcessor.Start(
        fun inbox ->
            let rec messageLoop _ = async { return! messageLoop () }

            messageLoop ()
        , cts.Token
    )
"""

[<Test>]
let ``comma placement should not cause compiler warnings, 2159`` () =
    formatSourceString
        """
let f x =
    React.useEffect (fun () ->
        if length x > 5 && length x < 10 then
            doX x
        else
            doY x
    , [| x |])

    ()
"""
        { config with
            MaxIfThenElseShortWidth = 40 }
    |> prepend newline
    |> should
        equal
        """
let f x =
    React.useEffect (
        fun () ->
            if length x > 5 && length x < 10 then
                doX x
            else
                doY x
        , [| x |]
    )

    ()
"""

[<Test>]
let ``comma should not break with lambda as tuple, 2771`` () =
    formatSourceString
        """
let shiftTimes localDate (start: Utc, duration) =
    ZonedDate.create TimeZone.current localDate
    |> Time.ZonedDate.startOf
    |> fun dayStart -> start + dayStart.Duration - refDay.StartTime.Duration
    , duration
"""
        config
    |> prepend newline
    |> should
        equal
        """
let shiftTimes localDate (start: Utc, duration) =
    ZonedDate.create TimeZone.current localDate
    |> Time.ZonedDate.startOf
    |> fun dayStart -> start + dayStart.Duration - refDay.StartTime.Duration
    , duration
"""

[<Test>]
let ``if then expression inside object instantiation breaks when formatted, 2819`` () =
    formatSourceString
        """
type Chapter() =
    member val Title: string option = Unchecked.defaultof<_> with get, set
    member val Url: string = Unchecked.defaultof<_> with get, set

let c =
    Chapter(
        Title =
            if true then
                Some ""
            else
                None
            ,
        Url = ""
    )
"""
        config
    |> prepend newline
    |> should
        equal
        """
type Chapter() =
    member val Title: string option = Unchecked.defaultof<_> with get, set
    member val Url: string = Unchecked.defaultof<_> with get, set

let c = Chapter(Title = (if true then Some "" else None), Url = "")
"""

[<Test>]
let ``object instantiation with ifthenelse in tuple`` () =
    formatSourceString
        """
type Chapter() =
    member val Title: string option = Unchecked.defaultof<_> with get, set
    member val Url: string = Unchecked.defaultof<_> with get, set

let c =
    Chapter(
        if true then
            Some ""
        else
            None
        ,
        ""
    )
"""
        config
    |> prepend newline
    |> should
        equal
        """
type Chapter() =
    member val Title: string option = Unchecked.defaultof<_> with get, set
    member val Url: string = Unchecked.defaultof<_> with get, set

let c = Chapter((if true then Some "" else None), "")
"""

[<Test>]
let ``infixapp with ifthen rhs in tuple`` () =
    formatSourceString
        """
let a = 0
let b = true
let c = 1
let d = 2

let _ =
    try
        a <> if b then c else d
        ,
        b
    with ex ->
        false, false
"""
        config
    |> prepend newline
    |> should
        equal
        """
let a = 0
let b = true
let c = 1
let d = 2

let _ =
    try
        a <> (if b then c else d), b
    with ex ->
        false, false
"""

[<Test>]
let ``infixapp with lambda rhs in tuple`` () =
    formatSourceString
        """
a |> fun b -> if b then 0 else 1
,
2
"""
        config
    |> prepend newline
    |> should
        equal
        """
a |> (fun b -> if b then 0 else 1), 2
"""
