module Fantomas.Core.Tests.BarBeforeDiscriminatedUnionDeclarationTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelper

let defaultConfig = config

let config = { config with BarBeforeDiscriminatedUnionDeclaration = true }

[<Test>]
let ``single DU without fields`` () =
    formatSourceString
        false
        """
type A = | A
"""
        config
    |> prepend newline
    |> should
        equal
        """
type A = | A
"""

[<Test>]
let ``single DU with fields`` () =
    formatSourceString
        false
        """
type A = | A of int
"""
        config
    |> prepend newline
    |> should
        equal
        """
type A = | A of int
"""

[<Test>]
let ``single DU with access modifier`` () =
    formatSourceString
        false
        """
type Foo =  private | Foo of int
"""
        config
    |> prepend newline
    |> should
        equal
        """
type Foo = private | Foo of int
"""

[<Test>]
let ``multiline DU case`` () =
    formatSourceString
        false
        """
[<NoEquality; NoComparison>]
type SynBinding =
    SynBinding of
                        accessibility: SynAccess option *
                        kind: SynBindingKind *
                        mustInline: bool *
                        isMutable: bool *
                        attributes: SynAttributes *
                        xmlDoc: PreXmlDoc *
                        valData: SynValData *
                        headPat: SynPat *
                        returnInfo: SynBindingReturnInfo option *
                        expr: SynExpr *
                        range: range *
                        seqPoint: DebugPointAtBinding
"""
        config
    |> prepend newline
    |> should
        equal
        """
[<NoEquality; NoComparison>]
type SynBinding =
    | SynBinding of
        accessibility: SynAccess option *
        kind: SynBindingKind *
        mustInline: bool *
        isMutable: bool *
        attributes: SynAttributes *
        xmlDoc: PreXmlDoc *
        valData: SynValData *
        headPat: SynPat *
        returnInfo: SynBindingReturnInfo option *
        expr: SynExpr *
        range: range *
        seqPoint: DebugPointAtBinding
"""

[<Test>]
let ``in signature file`` () =
    formatSourceString
        true
        """namespace meh

type Foo = | Bar of int
"""
        config
    |> should
        equal
        """namespace meh

type Foo = | Bar of int
"""

[<Test>]
let ``exception type does not have bar`` () =
    formatSourceString
        false
        """
exception LoadedSourceNotFoundIgnoring of string * range (*filename*)
"""
        config
    |> prepend newline
    |> should
        equal
        """
exception LoadedSourceNotFoundIgnoring of string * range (*filename*)
"""

[<Test>]
let ``attribute before single case`` () =
    formatSourceString
        false
        """
type Foo =   | [<SomeAttributeHere>] Foo of int
"""
        config
    |> prepend newline
    |> should
        equal
        """
type Foo = | [<SomeAttributeHere>] Foo of int
"""
