#r "../../lib/FSharp.Compiler.dll"

#load "Utils.fs"
#load "Ast.fs"
#load "Converter.fs"
#load "Parser.fs"

let result = Fantomas.Parser.parse (__SOURCE_DIRECTORY__ + "/Example.fs");;