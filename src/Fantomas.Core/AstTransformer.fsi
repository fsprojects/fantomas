module internal Fantomas.Core.AstTransformer

open FSharp.Compiler.Text

val astToNode:
    range: range ->
    hds: FSharp.Compiler.Syntax.ParsedHashDirective list ->
    mdls: FSharp.Compiler.Syntax.SynModuleOrNamespace list ->
        TriviaTypes.TriviaNode

val sigAstToNode: range: range -> ast: FSharp.Compiler.Syntax.SynModuleOrNamespaceSig list -> TriviaTypes.TriviaNode
