module internal Fantomas.Core.AstTransformer

open FSharp.Compiler.Text

val astToNode:
    range: range ->
    hds: FSharp.Compiler.Syntax.ParsedHashDirective list ->
    mdls: FSharp.Compiler.Syntax.SynModuleOrNamespace list ->
        TriviaTypes.TriviaNodeAssigner

val sigAstToNode:
    range: range -> ast: FSharp.Compiler.Syntax.SynModuleOrNamespaceSig list -> TriviaTypes.TriviaNodeAssigner
