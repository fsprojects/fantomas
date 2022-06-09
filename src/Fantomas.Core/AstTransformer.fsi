module internal Fantomas.Core.AstTransformer

val astToNode:
    hds: FSharp.Compiler.Syntax.ParsedHashDirective list ->
    mdls: FSharp.Compiler.Syntax.SynModuleOrNamespace list ->
        TriviaTypes.TriviaNodeAssigner list

val sigAstToNode: ast: FSharp.Compiler.Syntax.SynModuleOrNamespaceSig list -> TriviaTypes.TriviaNodeAssigner list
