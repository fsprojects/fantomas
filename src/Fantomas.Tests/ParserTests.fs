module Fantomas.Tests.ParserTests

open NUnit.Framework
open FsUnit

open Fantomas.Ast
open Fantomas.Parser

[<Test>]
let ``literals``() =
   parseExps "42" |> should equal [[(Lit (Int 42) : Exp<string>)]]
   parseExps "-42" |> should equal [[(Lit (Int -42) : Exp<string>)]]
   parseExps "0.05" |> should equal [[(Lit (Double 0.05) : Exp<string>)]]
   parseExps "-12.5" |> should equal [[(Lit (Double -12.5) : Exp<string>)]]
   parseExps "2.0015" |> should equal [[(Lit (Double 2.0015) : Exp<string>)]]
   parseExps "'f'" |> should equal [[(Lit (Char 'f') : Exp<string>)]]
   parseExps "'g'" |> should equal [[(Lit (Char 'g') : Exp<string>)]]
   parseExps "true" |> should equal [[(Lit (Bool true) : Exp<string>)]]
   parseExps "false" |> should equal [[(Lit (Bool false) : Exp<string>)]]
   // What about generic literal G
   parseExps "256I" |> should equal [[(Lit (BigInt 256I) : Exp<string>)]]

[<Test>]
let``signed byte``() =
    parseExps "let x = 0y"
    |> should equal [[Let(false,[PVar "x", Lit(SByte 0y)], Lit(Unit))]]

[<Test>]
let``64 bit Int``() =
    parseExps "let x = 0L"
    |> should equal [[Let(false,[PVar "x", Lit(Int64 (0L))], Lit(Unit))]]

[<Test>]
let``unsigned 64 bit Int``() =
    parseExps "let x = 0UL"
    |> should equal [[Let(false,[PVar "x", Lit(UInt64 (0UL))], Lit(Unit))]]

[<Test>]
let``single``() =
    parseExps "let x = 0.0f"
    |> should equal [[Let(false,[PVar "x", Lit(Single (0.0f))], Lit(Unit))]]

[<Test>]
let ``support for Int16, UInt16 and Byte``() =
    parseExps "let x = 0us" |> should equal [[Let (false,[(PVar "x", Lit (UInt16 0us))],Lit Unit)]]
    parseExps "let x = 0s" |> should equal [[Let (false,[(PVar "x", Lit (Int16  0s))],Lit Unit)]]
    parseExps "let x = 0uy" |> should equal [[Let (false,[(PVar "x", Lit (Byte 0uy))],Lit Unit)]]

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
    |> should equal 
              [[Let
                  (false,
                   [(PVar "x",
                     Paren
                       (Tuple
                          [Lit (Int 42); Paren (Tuple [Lit (String "Hello"); Var "y"])]))],
                   Lit Unit)]]

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
    parseExps "let f p = match p with (x, y) -> x"
    |> should equal
              [[Let
                  (false,
                   [(PApp (PVar "f",PVar "p"),
                     Match
                       (Var "p",[Clause (PParen (PTuple [PVar "x"; PVar "y"]),Var "x")]))],
                   Lit Unit)]]
[<Test>]
let ``sequence expressions``() =
    parseExps "let xs = seq { 1..10 }"
    |> should equal 
              [[Let
                  (false,
                   [(PVar "xs",
                     App (Var "seq",App (App (Var "op_Range",Lit (Int 1)),Lit (Int 10))))],
                   Lit Unit)]]

    parseExps "let xs = seq { for i in 1..5 do yield i }"
    |> should equal 
              [[Let
                  (false,
                   [(PVar "xs",
                     App
                       (Var "seq",
                        ForEach
                          (PVar "i",App (App (Var "op_Range",Lit (Int 1)),Lit (Int 5)),
                           YieldOrReturn (Var "i"))))],Lit Unit)]]

[<Test>]
let ``module handling``() =
    parse "open System"
    |> should equal [Module<string>.Open ["System"]]
    parse """
    open System
    open System.IO"""
    |> should equal [Module<string>.Open ["System"]; Open ["System";"IO"]]
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
    |> should equal [Module<string>.NestedModule (["MyModule"], [Exp [Let(false,[PVar "x",Lit (Int 42)],Lit Unit)]])]

[<Test>]
let ``record alias``() =
    parse """
        type AParameters = { a : int }
        type X = | A of AParameters | B
        let f (r : X) =
            match r with
            | X.A ( { a = aValue } as t )-> aValue
            | X.B -> 0"""
    |> should equal 
              [Types [Record ("AParameters",[Some "a"],[])];
               Types [DisUnion ("X",["A"; "B"])];
               Exp
                 [Let
                    (false,
                     [(PApp (PVar "f",PParen (PVar "r")),
                       Match
                         (Var "r",
                          [Clause
                             (PApp
                                (PLongVar [PVar "X"; PVar "A"],
                                 PParen (PNamed (PRecord [("a", PVar "aValue")],PVar "t"))),
                              Var "aValue");
                           Clause (PLongVar [PVar "X"; PVar "B"],Lit (Int 0))]))],Lit Unit)]]

[<Test>]
let ``dot indexed set``() =
    parseExps "twoDimensionalArray.[0, 1] <- 1.0"
    |> should equal [[DotIndexedSet (Var "twoDimensionalArray", [Tuple [Lit (Int 0); Lit (Int 1)]], Lit (Double 1.0))]]
                    

[<Test>]
let ``dot indexed get``() =
    parseExps "let x = twoDimensionalArray.[0,1]"
    |> should equal   [[Let
                          (false,
                           [(PVar "x",
                             DotIndexedGet
                               (Var "twoDimensionalArray",[Tuple [Lit (Int 0); Lit (Int 1)]]))],
                           Lit Unit)]]

[<Test>]
let ``record type definition``() =
    parseTypes "type Point = { X : int; Y : int }"
    |> should equal [[Record("Point", [Some "X"; Some "Y"], [])]]

[<Test>]
let ``record usage``() =
    parseExps "let p = { X = 2; Y = 32 }"
    |> should equal [[Let (false,[PVar "p", Exp.Record ["X", Lit(Int 2); "Y", Lit(Int 32)]], Lit Unit)]]
                    

[<Test>]
let ``class definition``() =
    parseTypes """
        type Point (x : int, y : int) =
            member this.X = x
            member this.Y = y"""
    |> should equal [[Class("Point", [ImplicitCtor [PVar "x"; PVar "y"]; 
                                        Member (true, PLongVar [PVar "this"; PVar "X"], Var "x"); 
                                        Member (true, PLongVar [PVar "this"; PVar "Y"], Var "y")])]]

[<Test>]
let ``exception declaration``() =
    parse "exception Empty"
    |> should equal [(Exception (ExceptionDef ("Empty",[])) : Module<string>)]

[<Test>]
let ``exception declaration with static members``() =
    parse "exception Empty with static member Foo = 42"
    |> should equal [Exception (ExceptionDef ("Empty",[Member (false, PVar "Foo",Lit (Int 42))]))] 
                    

[<Test>]
let ``tuple type in type constraint``() =
    parseExps "let xs : 'a * 'b = ys"
    |> should equal [[Let (false, [(PVar "xs", Typed (Var "ys",TTuple [TVar (TIdent "a"); TVar (TIdent "b")]))], Lit Unit)]]

[<Test>]
let ``record pattern``() =
    parseExps "let { FirstName = x; LastName = y } = ret"
    |> should equal [[Let (false, [(PRecord [("FirstName", PVar "x"); ("LastName", PVar "y")], Var "ret")], Lit Unit)]]
                    
[<Test>]
let ``anonymous type constraint``() =
    parseExps "let x : IDictionary<_, _> = dict"
    |> should equal   [[Let
                          (false,
                           [(PVar "x",
                             Typed
                               (Var "dict",TApp (TLongIdent [TIdent "IDictionary"],[TAnon; TAnon])))],
                           Lit Unit)]]

