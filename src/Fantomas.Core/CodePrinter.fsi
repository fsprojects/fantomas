module internal Fantomas.Core.CodePrinter

/// This type is a relic from the past but referenced all over the place.
/// The maintainers will remove this at a convenient time.
[<Class>]
type ASTContext =
    static member Default: ASTContext

val genParsedInput:
    astContext: ASTContext -> ast: FSharp.Compiler.Syntax.ParsedInput -> (Context.Context -> Context.Context)
