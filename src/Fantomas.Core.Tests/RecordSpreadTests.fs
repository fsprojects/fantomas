module Fantomas.Core.Tests.RecordSpreadTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelpers
open Fantomas.Core

// Record spreads, RFC FS-1151, dotnet/fsharp#18927.
// The spread reaches the syntax tree in three distinct places:
//
//   SynTypeDefnSimpleRepr.Record   holds SynFieldOrSpread,                carrying SynTypeSpread
//   SynExpr.Record                 holds SynExprRecordFieldOrSpread,      carrying SynExprSpread
//   SynExpr.AnonRecd               holds SynExprAnonRecordFieldOrSpread,  carrying SynExprSpread
//
// Each of those is exercised below, in implementation and signature files where the construct
// can appear in both.

// -------------------------------------------------------------------------------------------
// Record type definition, SynFieldOrSpread.Spread
// -------------------------------------------------------------------------------------------

[<Test>]
let ``type definition with only a spread`` () =
    formatSourceString """type T = { ...Src }""" config
    |> should
        equal
        """type T = { ...Src }
"""

[<Test>]
let ``type definition with a spread before a field`` () =
    formatSourceString """type T = { ...Src; A: int }""" config
    |> should
        equal
        """type T = { ...Src; A: int }
"""

[<Test>]
let ``type definition with a spread after a field`` () =
    formatSourceString """type T = { A: int; ...Src }""" config
    |> should
        equal
        """type T = { A: int; ...Src }
"""

[<Test>]
let ``type definition with a spread between fields`` () =
    formatSourceString """type T = { A: int; ...Src; B: string }""" config
    |> should
        equal
        """type T = { A: int; ...Src; B: string }
"""

[<Test>]
let ``type definition with multiple spreads`` () =
    formatSourceString """type T = { ...First; ...Second }""" config
    |> should
        equal
        """type T = { ...First; ...Second }
"""

[<Test>]
let ``type definition with a generic spread source`` () =
    formatSourceString """type T = { ...Src<int, string>; A: int }""" config
    |> should
        equal
        """type T = { ...Src<int, string>; A: int }
"""

[<Test>]
let ``type definition with a long identifier spread source`` () =
    formatSourceString """type T = { ...Some.Nested.Module.Src; A: int }""" config
    |> should
        equal
        """type T = { ...Some.Nested.Module.Src; A: int }
"""

[<Test>]
let ``multiline type definition with a spread`` () =
    formatSourceString
        """
type LongerRecordName =
    {
        ...SomeSourceRecordType
        FirstAdditionalField: int
        SecondAdditionalField: string
    }
"""
        config
    |> prepend newline
    |> should
        equal
        """
type LongerRecordName =
    {
        ...SomeSourceRecordType
        FirstAdditionalField: int
        SecondAdditionalField: string
    }
"""

[<Test>]
let ``multiline type definition with a trailing spread`` () =
    formatSourceString
        """
type LongerRecordName =
    {
        FirstAdditionalField: int
        SecondAdditionalField: string
        ...SomeSourceRecordType
    }
"""
        config
    |> prepend newline
    |> should
        equal
        """
type LongerRecordName =
    {
        FirstAdditionalField: int
        SecondAdditionalField: string
        ...SomeSourceRecordType
    }
"""

[<Test>]
let ``type definition with a spread and an attribute`` () =
    formatSourceString
        """
[<CLIMutable>]
type T = { ...Src; C: int }
"""
        config
    |> prepend newline
    |> should
        equal
        """
[<CLIMutable>]
type T = { ...Src; C: int }
"""

[<Test>]
let ``type definition with a spread and an xml doc`` () =
    formatSourceString
        """
/// Some documentation
type T = { ...Src; C: int }
"""
        config
    |> prepend newline
    |> should
        equal
        """
/// Some documentation
type T = { ...Src; C: int }
"""

[<Test>]
let ``type definition with a spread and a member`` () =
    formatSourceString
        """
type T =
    {
        ...Src
        C: int
    }

    member this.Total = this.C
"""
        config
    |> prepend newline
    |> should
        equal
        """
type T =
    {
        ...Src
        C: int
    }

    member this.Total = this.C
"""

[<Test>]
let ``type definition with a spread, stroustrup`` () =
    formatSourceString
        """
type LongerRecordName = {
    ...SomeSourceRecordType
    FirstAdditionalField: int
    SecondAdditionalField: string
}
"""
        { config with
            MultilineBracketStyle = Stroustrup }
    |> prepend newline
    |> should
        equal
        """
type LongerRecordName = {
    ...SomeSourceRecordType
    FirstAdditionalField: int
    SecondAdditionalField: string
}
"""

[<Test>]
let ``type definition with a comment before the spread`` () =
    formatSourceString
        """
type T =
    { // comment before the spread
        ...Src
        A: int
    }
"""
        config
    |> prepend newline
    |> should
        equal
        """
type T =
    { // comment before the spread
        ...Src
        A: int
    }
"""

[<Test>]
let ``type definition with a comment after the spread`` () =
    formatSourceString
        """
type T =
    {
        ...Src // comment after the spread
        A: int
    }
"""
        config
    |> prepend newline
    |> should
        equal
        """
type T =
    {
        ...Src // comment after the spread
        A: int
    }
"""

[<Test>]
let ``type definition with a comment between the spread and a field`` () =
    formatSourceString
        """
type T =
    {
        ...Src
        // comment between
        A: int
    }
"""
        config
    |> prepend newline
    |> should
        equal
        """
type T =
    {
        ...Src
        // comment between
        A: int
    }
"""

// -------------------------------------------------------------------------------------------
// Record type definition in a signature file
// -------------------------------------------------------------------------------------------

[<Test>]
let ``signature file, type definition with only a spread`` () =
    formatSignatureString
        """
module Foo

type T = { ...Src }
"""
        config
    |> prepend newline
    |> should
        equal
        """
module Foo

type T = { ...Src }
"""

[<Test>]
let ``signature file, type definition with a spread and fields`` () =
    formatSignatureString
        """
module Foo

type T = { ...Src; A: int }
"""
        config
    |> prepend newline
    |> should
        equal
        """
module Foo

type T = { ...Src; A: int }
"""

[<Test>]
let ``signature file, multiline type definition with a spread`` () =
    formatSignatureString
        """
module Foo

type LongerRecordName =
    {
        ...SomeSourceRecordType
        FirstAdditionalField: int
        SecondAdditionalField: string
    }
"""
        config
    |> prepend newline
    |> should
        equal
        """
module Foo

type LongerRecordName =
    {
        ...SomeSourceRecordType
        FirstAdditionalField: int
        SecondAdditionalField: string
    }
"""

[<Test>]
let ``signature file, type definition with a spread and a member`` () =
    formatSignatureString
        """
module Foo

type T =
    {
        ...Src
        C: int
    }

    member Total: int
"""
        config
    |> prepend newline
    |> should
        equal
        """
module Foo

type T =
    {
        ...Src
        C: int
    }

    member Total: int
"""

[<Test>]
let ``signature file, type definition with a comment before the spread`` () =
    formatSignatureString
        """
module Foo

type T =
    { // comment before the spread
        ...Src
        A: int
    }
"""
        config
    |> prepend newline
    |> should
        equal
        """
module Foo

type T =
    { // comment before the spread
        ...Src
        A: int
    }
"""

[<Test>]
let ``signature file, type definition with a comment after the spread`` () =
    formatSignatureString
        """
module Foo

type T =
    {
        ...Src // comment after the spread
        A: int
    }
"""
        config
    |> prepend newline
    |> should
        equal
        """
module Foo

type T =
    {
        ...Src // comment after the spread
        A: int
    }
"""

// -------------------------------------------------------------------------------------------
// Record expression, SynExprRecordFieldOrSpread.Spread
// -------------------------------------------------------------------------------------------

[<Test>]
let ``record expression with only a spread`` () =
    formatSourceString """let r = { ...source }""" config
    |> should
        equal
        """let r = { ...source }
"""

[<Test>]
let ``record expression with a spread before a field`` () =
    formatSourceString """let r = { ...source; B = 2 }""" config
    |> should
        equal
        """let r = { ...source; B = 2 }
"""

[<Test>]
let ``record expression with a spread after a field`` () =
    formatSourceString """let r = { A = 1; ...source }""" config
    |> should
        equal
        """let r = { A = 1; ...source }
"""

[<Test>]
let ``record expression with multiple spreads`` () =
    formatSourceString """let r = { ...first; ...second }""" config
    |> should
        equal
        """let r = { ...first; ...second }
"""

[<Test>]
let ``record expression with a record literal as spread source`` () =
    formatSourceString """let r = { ...{ A = 1; B = 2 }; B = 9 }""" config
    |> should
        equal
        """let r = { ...{ A = 1; B = 2 }; B = 9 }
"""

[<Test>]
let ``record expression with an application as spread source`` () =
    formatSourceString """let r = { ...makeSource arg; B = 9 }""" config
    |> should
        equal
        """let r = { ...makeSource arg; B = 9 }
"""

[<Test>]
let ``record expression with a parenthesized property get as spread source`` () =
    formatSourceString """let r = { ...(Holder()).P; B = 9 }""" config
    |> should
        equal
        """let r = { ...(Holder()).P; B = 9 }
"""

[<Test>]
let ``multiline record expression with a spread`` () =
    formatSourceString
        """
let r =
    {
        ...someRatherLongSourceExpression
        FirstAdditionalField = 1
        SecondAdditionalField = "two"
    }
"""
        config
    |> prepend newline
    |> should
        equal
        """
let r =
    {
        ...someRatherLongSourceExpression
        FirstAdditionalField = 1
        SecondAdditionalField = "two"
    }
"""

[<Test>]
let ``multiline record expression with a trailing spread`` () =
    formatSourceString
        """
let r =
    {
        FirstAdditionalField = 1
        SecondAdditionalField = "two"
        ...someRatherLongSourceExpression
    }
"""
        config
    |> prepend newline
    |> should
        equal
        """
let r =
    {
        FirstAdditionalField = 1
        SecondAdditionalField = "two"
        ...someRatherLongSourceExpression
    }
"""

[<Test>]
let ``record expression with a multiline application as spread source`` () =
    formatSourceString
        """
let r =
    {
        ...someRatherLongFunctionName firstArgument secondArgument thirdArgument fourthArgument
        B = 2
    }
"""
        config
    |> prepend newline
    |> should
        equal
        """
let r =
    {
        ...someRatherLongFunctionName firstArgument secondArgument thirdArgument fourthArgument
        B = 2
    }
"""

[<Test>]
let ``record expression with a conditional as spread source`` () =
    formatSourceString
        """
let r =
    {
        ...(if useDefaults then defaultSource else customSource)
        B = 2
    }
"""
        config
    |> prepend newline
    |> should
        equal
        """
let r =
    {
        ...(if useDefaults then defaultSource else customSource)
        B = 2
    }
"""

[<Test>]
let ``record expression with a spread source that has to break`` () =
    formatSourceString
        """
let r =
    {
        ...(someRatherLongFunctionName
                aFairlyLongArgumentName
                anotherFairlyLongArgumentName
                aThirdFairlyLongArgumentName)
        B = 2
    }
"""
        config
    |> prepend newline
    |> should
        equal
        """
let r =
    {
        ...(someRatherLongFunctionName
                aFairlyLongArgumentName
                anotherFairlyLongArgumentName
                aThirdFairlyLongArgumentName)
        B = 2
    }
"""

[<Test>]
let ``copy and update record expression with a spread`` () =
    formatSourceString """let r = { original with ...source }""" config
    |> should
        equal
        """let r = { original with ...source }
"""

[<Test>]
let ``record expression with a spread, stroustrup`` () =
    formatSourceString
        """
let r = {
    ...someRatherLongSourceExpression
    FirstAdditionalField = 1
    SecondAdditionalField = "two"
}
"""
        { config with
            MultilineBracketStyle = Stroustrup }
    |> prepend newline
    |> should
        equal
        """
let r = {
    ...someRatherLongSourceExpression
    FirstAdditionalField = 1
    SecondAdditionalField = "two"
}
"""

// -------------------------------------------------------------------------------------------
// Anonymous record expression, SynExprAnonRecordFieldOrSpread.Spread
// -------------------------------------------------------------------------------------------

[<Test>]
let ``anonymous record expression with only a spread`` () =
    formatSourceString """let r = {| ...source |}""" config
    |> should
        equal
        """let r = {| ...source |}
"""

[<Test>]
let ``anonymous record expression with a spread before a field`` () =
    formatSourceString """let r = {| ...source; B = 2 |}""" config
    |> should
        equal
        """let r = {| ...source; B = 2 |}
"""

[<Test>]
let ``anonymous record expression with a spread after a field`` () =
    formatSourceString """let r = {| A = 1; ...source |}""" config
    |> should
        equal
        """let r = {| A = 1; ...source |}
"""

[<Test>]
let ``anonymous record expression with multiple spreads`` () =
    formatSourceString """let r = {| ...first; ...second |}""" config
    |> should
        equal
        """let r = {| ...first; ...second |}
"""

[<Test>]
let ``struct anonymous record expression with a spread`` () =
    formatSourceString """let r = struct {| ...source; B = 2 |}""" config
    |> should
        equal
        """let r = struct {| ...source; B = 2 |}
"""

[<Test>]
let ``anonymous record expression with an anonymous record as spread source`` () =
    formatSourceString """let r = {| ...{| A = 5; B = 6 |}; A = 7 |}""" config
    |> should
        equal
        """let r = {| ...{| A = 5; B = 6 |}; A = 7 |}
"""

[<Test>]
let ``multiline anonymous record expression with a spread`` () =
    formatSourceString
        """
let r =
    {|
        ...someRatherLongSourceExpression
        FirstAdditionalField = 1
        SecondAdditionalField = "two"
    |}
"""
        config
    |> prepend newline
    |> should
        equal
        """
let r =
    {|
        ...someRatherLongSourceExpression
        FirstAdditionalField = 1
        SecondAdditionalField = "two"
    |}
"""

[<Test>]
let ``anonymous record expression with a multiline application as spread source`` () =
    formatSourceString
        """
let r =
    {|
        ...someRatherLongFunctionName firstArgument secondArgument thirdArgument fourthArg
        B = 2
    |}
"""
        config
    |> prepend newline
    |> should
        equal
        """
let r =
    {|
        ...someRatherLongFunctionName firstArgument secondArgument thirdArgument fourthArg
        B = 2
    |}
"""

[<Test>]
let ``anonymous record expression with a spread, stroustrup`` () =
    formatSourceString
        """
let r = {|
    ...someRatherLongSourceExpression
    FirstAdditionalField = 1
    SecondAdditionalField = "two"
|}
"""
        { config with
            MultilineBracketStyle = Stroustrup }
    |> prepend newline
    |> should
        equal
        """
let r = {|
    ...someRatherLongSourceExpression
    FirstAdditionalField = 1
    SecondAdditionalField = "two"
|}
"""

// -------------------------------------------------------------------------------------------
// Spreads nested in other constructs
// -------------------------------------------------------------------------------------------

[<Test>]
let ``spread inside a computation expression`` () =
    formatSourceString
        """
let xs = seq { for i in 1..2 -> { ...b; A = i } }
"""
        config
    |> prepend newline
    |> should
        equal
        """
let xs = seq { for i in 1..2 -> { ...b; A = i } }
"""

[<Test>]
let ``spread inside a lambda`` () =
    formatSourceString """let f = fun x -> { ...x; A = 1 }""" config
    |> should
        equal
        """let f = fun x -> { ...x; A = 1 }
"""

[<Test>]
let ``spread inside a quotation`` () =
    formatSourceString """let q = <@ { ...p; Y = 3 } @>""" config
    |> should
        equal
        """let q = <@ { ...p; Y = 3 } @>
"""

[<Test>]
let ``record expression with a comment after the spread`` () =
    formatSourceString
        """
let r =
    {
        ...source // comment after the spread
        B = 2
    }
"""
        config
    |> prepend newline
    |> should
        equal
        """
let r =
    {
        ...source // comment after the spread
        B = 2
    }
"""

[<Test>]
let ``anonymous record expression with a comment before the spread`` () =
    formatSourceString
        """
let r =
    {| // comment before the spread
        ...source
        B = 2
    |}
"""
        config
    |> prepend newline
    |> should
        equal
        """
let r =
    {| // comment before the spread
        ...source
        B = 2
    |}
"""

[<Test>]
let ``anonymous record expression with a comment after the spread`` () =
    formatSourceString
        """
let r =
    {|
        ...source // comment after the spread
        B = 2
    |}
"""
        config
    |> prepend newline
    |> should
        equal
        """
let r =
    {|
        ...source // comment after the spread
        B = 2
    |}
"""

[<Test>]
let ``record expression with a comment before the spread`` () =
    formatSourceString
        """
let r =
    { // leading comment
        ...source
        B = 2
    }
"""
        config
    |> prepend newline
    |> should
        equal
        """
let r =
    { // leading comment
        ...source
        B = 2
    }
"""
