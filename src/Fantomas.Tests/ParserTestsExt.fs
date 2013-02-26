module Fantomas.Tests.ParserTestsExtLongIdent

open NUnit.Framework
open FsUnit

open Fantomas.Ast
open Fantomas.Parser

[<Test>]
let ``Type test``() =
    parse "let x = 2 :? double"
    |> should equal [Let (false,[(PVar "x", TypeTest (Lit (Int 2),TLongIdent [TIdent "double"]))], Lit Unit)]

[<Test>]
let ``Dot set``() =
    parse "(List.head xs).Value <- 42"
    |> should equal [DotSet (Paren(App (Var "List.head",Var "xs")), Var "Value", Lit (Int 42))]

[<Test>]
let ``Interface implementation with no members``() =
    parse """
        type SomeClass =
            interface IFoo"""
    |> should equal [Types [Class ("SomeClass",[Interface (TLongIdent [TIdent "IFoo"], Option.None)])]]

[<Test>]
let ``Generic measure type instantiation``() =
    parse "let f<'a> = 42<'a> "
    |> should equal [Exp [Let (false,[(PVar "f", Measure (Lit (Int 42),Seq [MVar "a"]))],Lit Unit)]]

[<Test>]
let ``Anonymous measure type instantiation``() =
    parse "let f = 0.0<_>"
    |> should equal [Exp [Let (false,[(PVar "f", Measure (Lit (Double 0.0), Anon))],Lit Unit)]]

[<Test>]
let ``And patterns``() = 
    parse """
        let detectZeroAND point =
            match point with
            | (0, 0) -> 0
            | (var1, var2) & (0, _) -> 1
            | (var1, var2)  & (_, 0) -> 2
            | _ -> 3"""
   |> should equal [Exp [Let (false, [(PApp (PVar "detectZeroAND",PVar "point"),
                                             Match
                                               (Var "point",
                                                [Clause
                                                   (PParen(PTuple [PLit (Int 0); PLit (Int 0)]),Lit (Int 0));
                                                 Clause
                                                   (PAnds
                                                      [PParen(PTuple [PVar "var1"; PVar "var2"]);
                                                       PParen(PTuple [PLit (Int 0); PWild])],Lit (Int 1));
                                                 Clause
                                                   (PAnds
                                                      [PParen(PTuple [PVar "var1"; PVar "var2"]);
                                                       PParen(PTuple [PWild; PLit (Int 0)])],Lit (Int 2));
                                                 Clause (PWild,Lit (Int 3))]))],Lit Unit)]]

[<Test>]
let ``Statically resolved type constraints``() = 
    parse """
        let inline joinM b m =
        let (>>=) m f = (^x: (member Bind: ^m -> (^n -> ^n) -> ^n) b, m, f)
        m >>= id"""   
    |> should equal [Exp [Let (false, [(PApp (PApp (PVar "joinM",PVar "b"),PVar "m"),
                                             Let (false, [(PApp (PApp (PVar "op_GreaterGreaterEquals",PVar "m"),PVar "f"),
                                                              TraitCall (["x"], 
                                                                MemberSig (TFun (TVar (TIdent "m"), 
                                                                               TFun (TFun (TVar (TIdent "n"),TVar (TIdent "n")), 
                                                                                    TVar (TIdent "n")))),
                                                                                        Tuple [Var "b"; Var "m"; Var "f"]))],
                                                            App (App (Var "op_GreaterGreaterEquals",Var "m"),Var "id")))],
                                                       Lit Unit)]]

[<Test>]
let ``Support for DiscardAfterError``() = 
    parse "let product = List."
    |> should equal [Exp [Let (false,[(PVar "product", ArbitraryAfterError)],Lit Unit)]]

[<Test>]
let ``Support for bigint literals`` () = /// What about generic literal G
    parse "let v = 256I"
    |> should equal [Exp [Let (false,[(PVar "v", Lit (Literal.BigInt 256I))],Lit Unit)]] 

[<Test>]
let ``Type extension``() =
    parse "type A () = do () with member x.A b c d = b + c * d"
    |> should equal [Types [Class ("A", [ImplicitCtor [];
                                            LetBindings [Let (false,[(PLit Unit, Lit Unit)],Lit Unit)];
                                            Member (true, PApp (PApp (PApp (PLongVar [PVar "x"; PVar "A"],PVar "b"),PVar "c"), PVar "d"),
                                                    App (App (Var "op_Addition",Var "b"), App (App (Var "op_Multiply",Var "c"),Var "d")))])]]

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

[<Test>]
let ``Optional arguments in class members``() =
    parse "type Foo() = let Foo ?x = defaultArg x 42"
    |> should equal [Types [Class ("Foo", [ImplicitCtor []; 
                                           Member (true, PApp (PLongVar [PVar "this"; PVar "Foo"],PVar "x"), App (App (Var "defaultArg",Var "x"),Lit (Int 42)))])]]
                         
[<Test>]
let ``Long name identifier``() =
    parse "let ``my function`` ``this value`` = ``this value``"
    |> should equal [Exp [Let (false,[(PApp (PVar "my function",PVar "this value"), Var "this value")], Lit Unit)]]

[<Test>]
let ``Params array attribute``() =
    parse """
        [<AttributeUsage(AttributeTargets.Method, AllowMultiple = true)>]
        type TestAttribute([<ParamArray>] parameters: obj[])  =
            inherit Attribute()
            let Parameters = parameters"""
    |> should equal [Types [Class
                                ("TestAttribute",
                                [ImplicitCtor [PVar "parameters"];
                                ImplicitInherit (TLongIdent [TIdent "Attribute"],Lit Unit, Option.None);
                                Member (true, PLongVar [PVar "this"; PVar "Parameters"],Var "parameters")])]]

[<Test>]
let ``Hash constraint``() =
    parseExps "let something: #IEnumerable option = None"
    |> should equal [[Let (false, [(PVar "something", Typed (Var "None", TApp (TLongIdent [TIdent "option"],[TLongIdent [TIdent "IEnumerable"]])))], Lit Unit)]]

[<Test>]
let ``Unit of Measure products support``() =
    parse """
        [<Measure>] type m
        [<Measure>] type kg
        [<Measure>] type s
        [<Measure>] type N = kg m / s^2
        [<Measure>] type Pa = N / m^2
        let a = 1.<m/Pa*s>"""
    |> should equal [   Types [Nothing "m"]
                        Types [Nothing "kg"]
                        Types [Nothing "s"]
                        Types
                          [Abbrev
                             ("N",
                              TTuple
                                [TApp (TLongIdent [TIdent "m"],[TLongIdent [TIdent "kg"]]);
                                 TMeasurePower (TLongIdent [TIdent "s"],2)])]
                        Types
                          [Abbrev
                             ("Pa",
                              TTuple [TLongIdent [TIdent "N"]; TMeasurePower (TLongIdent [TIdent "m"],2)])]
                        Exp
                          [Let
                             (false,
                              [(PVar "a",
                                Measure
                                  (Lit (Double 1.0),
                                   Product
                                     (Divide
                                        (Seq [Named (TLongIdent [TIdent "m"])],
                                         Seq [Named (TLongIdent [TIdent "Pa"])]),
                                      Seq [Named (TLongIdent [TIdent "s"])])))],Lit Unit)]]

