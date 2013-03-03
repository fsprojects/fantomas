#r "../../lib/FSharp.Compiler.dll"

#load "Utils.fs"
#load "Ast.fs"
#load "Converter.fs"
#load "Parser.fs"

open Fantomas.Parser

let result = parseFromFile (__SOURCE_DIRECTORY__ + "/Example.fsx");;

let a = parseExps """try Foo() with
                 | :? System.ArgumentException
                 | :? System.ArgumentNullException -> 42"""