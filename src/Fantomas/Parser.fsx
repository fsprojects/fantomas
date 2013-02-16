#r "../../lib/FSharp.Compiler.dll"

#load "Utils.fs"
#load "Ast.fs"
#load "Converter.fs"
#load "Parser.fs"

open Fantomas.Parser

let result = parse (__SOURCE_DIRECTORY__ + "/Example.fs");;