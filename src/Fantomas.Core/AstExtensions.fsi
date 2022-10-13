module internal Fantomas.Core.AstExtensions

open FSharp.Compiler.Text
open FSharp.Compiler.Syntax

type ParsedInput with

    member FullRange: range

type SynModuleOrNamespace with

    member FullRange: range

type SynModuleOrNamespaceSig with

    member FullRange: range

type SynIdent with

    member FullRange: range

type SynLongIdent with

    member FullRange: range

type SynTyparDecl with

    member FullRange: range

type SynInterpolatedStringPart with

    member FullRange: range

type SynExprRecordField with

    member FullRange: range

type SynField with

    member FullRange: range

val longIdentFullRange: li: LongIdent -> Range

type SynBinding with

    member FullRange: range
