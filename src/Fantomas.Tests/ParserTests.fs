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
   // What about generic literal G
   parseExps "256I" |> should equal [[(Lit (BigInt 256I))]]

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
    parseExps "(+) ((+) ((+) x y) z) k" |> should equal (parseExps "((x + y) + z) + k")

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
let ``type declarations``() =
    parseTypes "type Exp = Var of string"
    |> should equal [[DisUnion("Exp", ["Var"])]]

[<Test>]
let ``pattern matching``() =
    parseExps "let f (x, y) = x"
    |> should equal [[Let(false,[PApp(PVar "f", PParen(PTuple [PVar "x"; PVar "y"])), Var "x"], Lit(Unit))]]
    parseExps "let _ = x"
    |> should equal [[Let(false,[PWild, Var "x"], Lit(Unit))]]
    parseExps "let f x = match x with True -> 42"
    |> should equal [[Let(false,[PApp(PVar "f", PVar "x"), Match(Var "x", [Clause(PVar("True"), Lit(Int 42))])], Lit(Unit))]]
    parseExps "let f p = match p with (x, y) -> x"
    |> should equal [[Let(false,[PApp(PVar "f", PVar "p"), Match(Var "p", [Clause(PParen (PTuple [PVar "x"; PVar "y"]), Var "x")])], Lit(Unit))]]

[<Test>]
let ``sequence expressions``() =
    parseExps "let xs = seq { 1..10 }"
    |> should equal [[Let(false,[PVar "xs", App (Var "seq",App (App (Var "op_Range",Lit (Int 1)),Lit (Int 10)))], Lit Unit)]]
    parseExps "let xs = seq { for i in 1..5 do yield i }"
    |> should equal [[Let(false,[PVar "xs", App (Var "seq",ForEach(PVar "i", App (App (Var "op_Range",Lit (Int 1)), Lit (Int 5)), YieldOrReturn (Var "i")))], Lit Unit)]]

[<Test>]
let ``module handling``() =
    parse "open System"
    |> should equal [Open ["System"]]
    parse """
    open System
    open System.IO"""
    |> should equal [Open ["System"]; Open ["System";"IO"]]
    parse "let xs = List.head [1..5]"
    |> should equal 
          [Exp
             [Let
                (false,
                 [(PVar "xs",
                   App
                     (Var "List.head",
                      App (App (Var "op_Range",Lit (Int 1)),Lit (Int 5))))],Lit Unit)]]
    parse "module MyModule = let x = 42"
    |> should equal [NestedModule (["MyModule"], [Exp [Let(false,[PVar "x",Lit (Int 42)],Lit Unit)]])]

[<Test>]
let ``Record alias`` () =
    parse """
        type AParameters = { a : int }
        type X = | A of AParameters | B
        let f (r : X) =
            match r with
            | X.A ( { a = aValue } as t )-> aValue
            | X.B -> 0"""
    |> should equal [Types [Record ("AParameters",[Some "a"],[])]; Types [DisUnion ("X",["A"; "B"])];
                         Exp [Let (false, [(PApp (PVar "f", PParen(PVar "r")),
                                                 Match (Var "r",
                                                    [Clause (PApp (PLongVar [PVar "X"; PVar "A"], 
                                                                PParen(PNamed (PRecord [("a", PVar "aValue")],PVar "t"))), Var "aValue");
                                                     Clause (PLongVar [PVar "X"; PVar "B"],Lit (Int 0))]))],Lit Unit)]]
