#r "../../lib/FSharp.Compiler.dll"

#load "Utils.fs"
#load "Ast.fs"
#load "Converter.fs"
#load "Parser.fs"

open Fantomas.Parser

let result = parseFromFile (__SOURCE_DIRECTORY__ + "/Example.fs");;

let a = parseExps "let x = 42\n\
     let y = 24";;