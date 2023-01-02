module internal Fantomas.Core.Trivia

open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open Fantomas.Core.FormatConfig
open Fantomas.Core.SyntaxOak

val findNodeWhereRangeFitsIn: root: Node -> range: range -> Node option
val enrichTree: config: FormatConfig -> sourceText: ISourceText -> ast: ParsedInput -> tree: Oak -> Oak
