module Fantomas.Core.Tests.TestHelpers

open Fantomas.Core
open NUnit.Framework

[<RequireQualifiedAccess>]
module String =
    val normalizeNewLine: str: string -> string

val config: FormatConfig
val newline: string
val formatSourceString: isFsiFile: bool -> s: string -> config: FormatConfig -> string
/// The `source` will first be parsed to AST.
val formatAST: isFsiFile: bool -> source: string -> config: FormatConfig -> string
val formatSourceStringWithDefines: defines: string list -> s: string -> config: FormatConfig -> string
val isValidFSharpCode: isFsiFile: bool -> s: string -> bool
/// A wrapper around FsUnit's equal that also normalizes newlines.
val equal: x: 'a -> Constraints.EqualConstraint
val inline prepend: s: ^a -> content: ^b -> 'c when (^a or ^b): (static member (+): ^a * ^b -> 'c)
val (==): actual: 'a -> expected: 'a -> unit when 'a: equality
val fail: unit -> unit
val pass: unit -> unit
