module Fantomas.Tests.ParserTests

open NUnit.Framework
open FsUnit

open Fantomas.Ast
open Fantomas.Parser

[<Test>]
let ``literals``() =
   parseExps "42" |> should equal [[(Lit (Int 42))]]
   parseExps "-42" |> should equal [[(Lit (Int -42))]]
   parseExps "0.05" |> should equal [[(Lit (Double 0.05))]]
   parseExps "-12.5" |> should equal [[(Lit (Double -12.5))]]
   parseExps "2.0015" |> should equal [[(Lit (Double 2.0015))]]
   parseExps "'f'" |> should equal [[(Lit (Char 'f'))]]
   parseExps "'g'" |> should equal [[(Lit (Char 'g'))]]
   parseExps "true" |> should equal [[(Lit (Bool true))]]
   parseExps "false" |> should equal [[(Lit (Bool false))]]

[<Test>]
let ``simple declarations``() =
    parseExps """
    let x = 42
    let y = 24""" 
    |> should equal [[Let (false,[(PVar "x", Lit (Int 42))],Lit Unit)];
                     [Let (false,[(PVar "y", Lit (Int 24))],Lit Unit)]]

[<Test>]
let ``function declarations``() =   
    parseExps "let f x y z = z" 
    |> should equal [[Let(false,[PApp(PApp(PApp(PVar "f", PVar "x"), PVar "y"), PVar "z"), Var "z"], Lit(Unit))]]

[<Test>]
let ``nested declarations``() =  
    let exp = """
    let x = let y = 12 
            let z = 21
            z"""
    parseExps exp 
    |> should equal [[Let(false,
                            [PVar "x", 
                                    Let(false,
                                            [PVar "y",Lit (Int 12)],
                                                Let(false,
                                                    [PVar "z",Lit (Int 21)],
                                                        Var "z"))],  Lit Unit)]]
    parseExps exp |> should equal (parseExps "let x = let y = 12 in let z = 21 in z")

[<Test>]
let ``infix operators``() =
    parseExps "x + y + z" 
    |> should equal [[App(App (Var "op_Addition", App(App (Var "op_Addition", Var "x"), Var "y")), Var "z")]]
    parseExps "(+) ((+) ((+) x y) z) k" |> should equal (parse "((x + y) + z) + k")

[<Test>]
let ``applications associate to the left``() =
    parseExps "f g h j"
    |> should equal [[(App(App(App(Var "f", Var "g"), Var "h"), Var "j"))]]

[<Test>]
let ``parentheses modify associations``() =
    parseExps "f (g (h j))"
    |> should equal [[(App(Var "f", Paren(App(Var "g", Paren(App(Var "h", Var "j"))))))]]

[<Test>]
let currying() =
    parseExps "let squares = map square numbers"
    |> should equal [[(Let(false,[PVar "squares", App (App(Var "map", Var "square"), Var "numbers")],  Lit Unit))]]

[<Test>]
let lambdas() =
    parseExps "fun x -> fun y -> fun z -> z"
    |> should equal [[Lam([PVar "x"], Lam([PVar "y"], Lam([PVar "z"], Var "z")))]]
    parseExps "fun x y z -> z" |> should equal (parseExps "fun x -> fun y -> fun z -> z")

[<Test>]
let tuples() =
    parseExps "let x = (42, 24)"
    |> should equal [[Let(false,[PVar "x", Paren(Tuple [Lit(Int 42);Lit(Int 24)])], Lit(Unit))]]
    parseExps """let x = (42, ("Hello", y))"""
    |> should equal [[Let(false,[PVar "x", Paren(Tuple [Lit(Int 42); Paren(Tuple [Lit(String "Hello"); Var "y"])])], Lit(Unit))]]

[<Test>]
let lists() =
    parseExps "let x = [42; y; z]"
    |> should equal [[Let(false,[PVar "x", List [Lit(Int 42); List[Var "y"; Var "z"]]], Lit(Unit))]]

[<Test>]
let ``option values``() =
    parseExps "let x = Some 42"
    |> should equal [[Let(false,[PVar "x", App (Var "Some", Lit(Int 42))], Lit(Unit))]]
    parseExps "let x = None"
    |> should equal [[Let(false,[PVar "x", Var "None"], Lit(Unit))]]

[<Test>]
let ``discriminated unions``() =
    parseTypes "type Exp = Var of string"
    |> should equal [[DisUnion("Exp", ["Var"])]]

