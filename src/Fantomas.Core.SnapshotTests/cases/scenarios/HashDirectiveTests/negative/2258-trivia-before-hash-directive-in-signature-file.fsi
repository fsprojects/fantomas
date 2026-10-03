(*---
fsharp_multiline_bracket_style = cramped
---*)
// Copyright (c) Microsoft Corporation.  All Rights Reserved.  See License.txt in the project root for license information.

namespace FSharp.Compiler.Tokenization

open System
open System.Threading
open FSharp.Compiler
open FSharp.Compiler.Text

#nowarn "57"

/// Represents encoded information for the end-of-line continuation of lexing
[<Struct; CustomEquality; NoComparison>]
type FSharpTokenizerLexState =
    { PosBits: int64
      OtherBits: int64 }

    static member Initial: FSharpTokenizerLexState

    member Equals: FSharpTokenizerLexState -> bool

/// Represents stable information for the state of the lexing engine at the end of a line
type FSharpTokenizerColorState =
    | Token = 1
    | IfDefSkip = 3
    | String = 4
    | Comment = 5
    | StringInComment = 6
    | VerbatimStringInComment = 7
    | CamlOnly = 8
    | VerbatimString = 9
    | SingleLineComment = 10
    | EndLineThenSkip = 11
    | EndLineThenToken = 12
    | TripleQuoteString = 13
    | TripleQuoteStringInComment = 14
    | InitialState = 0
