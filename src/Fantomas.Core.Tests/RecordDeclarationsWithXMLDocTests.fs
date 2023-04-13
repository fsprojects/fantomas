module Fantomas.Core.Tests.RecordDeclarationsWithXMLDocTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelpers

[<Test>]
let ``each record field has xml comment`` () =
    formatSourceString
        false
        """
/// Represents additional information for SynExpr.TryWith
[<NoEquality; NoComparison>]
type SynExprTryWithTrivia =
    { /// The syntax range of the `try` keyword.
      TryKeyword: range

      /// The syntax range from the beginning of the `try` keyword till the end of the `with` keyword.
      TryToWithRange: range

      /// The syntax range of the `with` keyword
      WithKeyword: range

      /// The syntax range from the beginning of the `with` keyword till the end of the TryWith expression.
      WithToEndRange: range }
"""
        config
    |> prepend newline
    |> should
        equal
        """
/// Represents additional information for SynExpr.TryWith
[<NoEquality; NoComparison>]
type SynExprTryWithTrivia =
    {
        /// The syntax range of the `try` keyword.
        TryKeyword: range

        /// The syntax range from the beginning of the `try` keyword till the end of the `with` keyword.
        TryToWithRange: range

        /// The syntax range of the `with` keyword
        WithKeyword: range

        /// The syntax range from the beginning of the `with` keyword till the end of the TryWith expression.
        WithToEndRange: range
    }
"""

[<Test>]
let ``each record field has xml comment in signature file`` () =
    formatSourceString
        true
        """
/// Represents additional information for SynExpr.TryWith
[<NoEquality; NoComparison>]
type SynExprTryWithTrivia =
    { /// The syntax range of the `try` keyword.
      TryKeyword: range

      /// The syntax range from the beginning of the `try` keyword till the end of the `with` keyword.
      TryToWithRange: range

      /// The syntax range of the `with` keyword
      WithKeyword: range

      /// The syntax range from the beginning of the `with` keyword till the end of the TryWith expression.
      WithToEndRange: range }
"""
        config
    |> prepend newline
    |> should
        equal
        """
/// Represents additional information for SynExpr.TryWith
[<NoEquality; NoComparison>]
type SynExprTryWithTrivia =
    {
        /// The syntax range of the `try` keyword.
        TryKeyword: range

        /// The syntax range from the beginning of the `try` keyword till the end of the `with` keyword.
        TryToWithRange: range

        /// The syntax range of the `with` keyword
        WithKeyword: range

        /// The syntax range from the beginning of the `with` keyword till the end of the TryWith expression.
        WithToEndRange: range
    }
"""

[<Test>]
let ``a single record field has xml doc comment`` () =
    formatSourceString
        false
        """
type SynExprTryWithTrivia =
    { TryKeyword: range
      /// The syntax range from the beginning of the `try` keyword till the end of the `with` keyword.
      TryToWithRange: range
      WithKeyword: range
      WithToEndRange: range }
"""
        config
    |> prepend newline
    |> should
        equal
        """
type SynExprTryWithTrivia =
    {
        TryKeyword: range
        /// The syntax range from the beginning of the `try` keyword till the end of the `with` keyword.
        TryToWithRange: range
        WithKeyword: range
        WithToEndRange: range
    }
"""

[<Test>]
let ``a single record field has xml doc comment in signature file`` () =
    formatSourceString
        true
        """
type SynExprTryWithTrivia =
    { TryKeyword: range
      /// The syntax range from the beginning of the `try` keyword till the end of the `with` keyword.
      TryToWithRange: range
      WithKeyword: range
      WithToEndRange: range }
"""
        config
    |> prepend newline
    |> should
        equal
        """
type SynExprTryWithTrivia =
    {
        TryKeyword: range
        /// The syntax range from the beginning of the `try` keyword till the end of the `with` keyword.
        TryToWithRange: range
        WithKeyword: range
        WithToEndRange: range
    }
"""

[<Test>]
let ``no xml docs, should be cramped style`` () =
    formatSourceString
        false
        """
type SynExprTryWithTrivia =
    {
        TryKeyword: range
        TryToWithRange: range
        WithKeyword: range
        WithToEndRange: range
    }
"""
        config
    |> prepend newline
    |> should
        equal
        """
type SynExprTryWithTrivia =
    { TryKeyword: range
      TryToWithRange: range
      WithKeyword: range
      WithToEndRange: range }
"""

[<Test>]
let ``no xml docs, should be cramped style in signature file`` () =
    formatSourceString
        true
        """
type SynExprTryWithTrivia =
    {
        TryKeyword: range
        TryToWithRange: range
        WithKeyword: range
        WithToEndRange: range
    }
"""
        config
    |> prepend newline
    |> should
        equal
        """
type SynExprTryWithTrivia =
    { TryKeyword: range
      TryToWithRange: range
      WithKeyword: range
      WithToEndRange: range }
"""
