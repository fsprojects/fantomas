module internal Fantomas.Core.CodePrinter

val genParsedInput: ast: FSharp.Compiler.Syntax.ParsedInput -> (Context.Context -> Context.Context)
