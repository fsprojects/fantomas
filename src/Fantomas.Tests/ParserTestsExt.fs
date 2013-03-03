module Fantomas.Tests.ParserTestsExt

open NUnit.Framework
open FsUnit

open Fantomas.Ast
open Fantomas.Parser

[<Test>]
let ``exception pattern matching``() =
    parseExps """try Foo() with
                 | :? System.ArgumentException
                 | :? System.ArgumentNullException -> 42"""
    |> should equal
              [[TryWith
                  (App (Var "Foo",Lit Unit),
                   [Clause
                      (POr
                         (PIsInst
                            (TLongIdent [TIdent "System"; TIdent "ArgumentException"]),
                          PIsInst
                            (TLongIdent [TIdent "System"; TIdent "ArgumentNullException"])),
                       Lit (Int 42))])]]

[<Test>]
let ``for loop``() =
    parseExps """
        for i = 0 to 5 do
            printf "%i" i """
    |> should equal [[For (PVar "i",Lit (Int 0),Lit (Int 5), App (App (Var "printf",Lit (String "%i")),Var "i"))]]

[<Test>]
let ``extern method arguments attributes``() =
    parse """
        extern bool private HeapSetInformation( 
            UIntPtr _HeapHandle, 
            UInt32  _HeapInformationClass, 
            UIntPtr _HeapInformation, 
            UIntPtr _HeapInformationLength)"""
    |> should equal [Exp [Let (false,
                                [(PApp
                                    (PVar "HeapSetInformation",
                                    PTuple
                                        [PAttribute (PVar "_HeapHandle",[]);
                                        PAttribute (PVar "_HeapInformationClass",[]);
                                        PAttribute (PVar "_HeapInformation",[]);
                                        PAttribute (PVar "_HeapInformationLength",[])]),
                                  Typed
                                    (App
                                        (Var "failwith",
                                        Lit (String "extern was not given a DllImport attribute")),
                                    TApp (TLongIdent [TIdent "bool"],[])))],Lit Unit)]]

[<Test>]
let ``assert keyword``() =
    parseExps "let _ = assert (posNbits <= 32)"
    |> should equal
              [[Let
                  (false,
                   [(PWild,
                     Assert
                       (Paren
                          (App
                             (App (Var "op_LessThanOrEqual",Var "posNbits"),Lit (Int 32)))))],
                   Lit Unit)]]

[<Test>]
let ``val keyword``() =
    parseTypes """
        type MyType() =
            [<DefaultValue>] val mutable myInt2 : int"""
    |> should equal 
              [[Class
                  ("MyType",
                   [ImplicitCtor [];
                    ValField (Some (TIdent "myInt2"),TLongIdent [TIdent "int"])])]]

[<Test>]
let ``while loop``() =
    parseExps """
        while x > 0 do
            foo x """
    |> should equal [[While (App (App (Var "op_GreaterThan",Var "x"),Lit (Int 0)), App (Var "foo",Var "x"))]]

[<Test>]
let ``lazy keyword``() =
    parseExps "let f = lazy 42"
    |> should equal [[Let (false,[(PVar "f", Lazy (Lit (Int 42)))],Lit Unit)]]

[<Test>]
let ``enum support``() =
    parseTypes """
        type Choice = 
            | Yes = 0
            | No  = 1"""
    |> should equal [[Enum ("Choice",[("Yes", Int 0); ("No", Int 1)])]]

[<Test>]
let ``inferred downcast``() =
    parseExps "let x : string = downcast foo()"
    |> should equal
              [[Let
                  (false,
                   [(PVar "x",
                     Typed
                       (InferredDowncast (App (Var "foo",Lit Unit)),
                        TLongIdent [TIdent "string"]))],Lit Unit)]]
[<Test>]
let ``quoted identifier``() =      
    parseExps "let x' = 42"  
    |> should equal [[Let(false,[PVar "x'", Lit(Int 42)], Lit(Unit))]]

[<Test>]
let ``inheriting a type``() =
    parseTypes ("type IPartialEqualityComparer<'T> = inherit IEqualityComparer<'T>")
    |> should equal 
            [[Class
                  ("IPartialEqualityComparer",
                   [Inherit
                      (TApp (TLongIdent [TIdent "IEqualityComparer"],[TVar (TIdent "T")]),
                       None)])]]  

[<Test>]
let ``assembly level attribute``() =
    parse """
        [<Dependency("FSharp.Compiler", LoadHint.Always)>]
        do ()"""
    |> should equal 
            [Attributes
                 [Attribute
                    (Paren (Tuple [Lit (String "FSharp.Compiler"); Var "LoadHint.Always"]))];
             Exp [Do (Lit Unit)]]

[<Test>]
let ``Implicit inherit``() =
    parseTypes "type MyClassDerived() = inherit MyClassBase()"
    |> should equal
              [[Class
                  ("MyClassDerived",
                   [ImplicitCtor [];
                    ImplicitInherit (TLongIdent [TIdent "MyClassBase"],Lit Unit,None)])]]

[<Test>]
let ``module abbreviation``() =
    // FsUnit can't infer correct types
    parse "module ES = Microsoft.FSharp.Quotations.ExprShape"
    |> should equal [Module<string>.ModuleAbbrev ("ES",["Microsoft"; "FSharp"; "Quotations"; "ExprShape"])]

[<Test>]
let ``try finally``() =
    parseExps """
        let divide x y =
            try
                x / y 
            finally
                printfn "Always print this"
                """
    |> should equal [[Let (false,  [(PApp (PApp (PVar "divide",PVar "x"),PVar "y"),  
                                     TryFinally (App (App (Var "op_Division",Var "x"),Var "y"), 
                                                 App (Var "printfn",Lit (String "Always print this"))))],Lit Unit)]]

[<Test>]
let``typed quotation``() =
    parseExps "let x = <@ 2 + 3 @>"
    |> should equal 
            [[Let
                  (false,
                   [(PVar "x",
                     Quote
                       (Var "op_Quotation",
                        App (App (Var "op_Addition",Lit (Int 2)),Lit (Int 3))))],Lit Unit)]]

[<Test>]
let``untyped quotation``() =
    parseExps "let x = <@@ 2 + 3 @@>"
    |> should equal
            [[Let
                  (false,
                   [(PVar "x",
                     Quote
                       (Var "op_QuotationUntyped",
                        App (App (Var "op_Addition",Lit (Int 2)),Lit (Int 3))))],Lit Unit)]]

[<Test>]
let ``inferred upcast``() =
    parse "let x = upcast y"
    |> should equal [Exp [Let (false,[(PVar "x", InferredUpcast (Var "y"))],Lit Unit)]]

[<Test>]
let ``type test``() =
    parse "let x = 2 :? double"
    |> should equal
          [Exp
             [Let
                (false,
                 [(PVar "x", TypeTest (Lit (Int 2),TLongIdent [TIdent "double"]))],
                 Lit Unit)]]

[<Test>]
let ``dot set``() =
    parse "(List.head xs).Value <- 42"
    |> should equal [Exp [DotSet (Paren (App (Var "List.head",Var "xs")),Var "Value",Lit (Int 42))]]

[<Test>]
let ``interface implementation with no members``() =
    parse """
        type SomeClass =
            interface IFoo"""
    |> should equal [Types [Class ("SomeClass",[Interface (TLongIdent [TIdent "IFoo"], None)])]]

[<Test>]
let ``generic measure type instantiation``() =
    parse "let f<'a> = 42<'a>"
    |> should equal [Exp [Let (false,[(PVar "f", Measure (Lit (Int 42),Seq [MVar "a"]))],Lit Unit)]]

[<Test>]
let ``anonymous measure type instantiation``() =
    parse "let f = 0.0<_>"
    |> should equal [Exp [Let (false,[(PVar "f", Measure (Lit (Double 0.0), Anon))],Lit Unit)]]

[<Test>]
let ``and patterns``() = 
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
let ``statically resolved type constraints``() = 
    parse """
        let inline joinM b m =
            let (>>=) m f = (^x: (member Bind: ^m -> (^n -> ^n) -> ^n) b, m, f)
            m >>= id"""   
    |> should equal
          [Exp
             [Let
                (false,
                 [(PApp (PApp (PVar "joinM",PVar "b"),PVar "m"),
                   Let
                     (false,
                      [(PApp (PApp (PVar "op_GreaterGreaterEquals",PVar "m"),PVar "f"),
                        Paren
                          (TraitCall
                             (["x"],
                              MemberSig
                                (TFun
                                   (TVar (TIdent "m"),
                                    TFun
                                      (TFun (TVar (TIdent "n"),TVar (TIdent "n")),
                                       TVar (TIdent "n")))),
                              Tuple [Var "b"; Var "m"; Var "f"])))],
                      App (App (Var "op_GreaterGreaterEquals",Var "m"),Var "id")))],
                 Lit Unit)]]

[<Test>]
let ``support for DiscardAfterError``() = 
    parse "let product = List."
    |> should equal [Exp [Let (false,[(PVar "product", ArbitraryAfterError)],Lit Unit)]]

[<Test>]
let ``Type extension``() =
    parse "type A () = do () with member x.A b c d = b + c * d"
    |> should equal 
          [Types
             [Class
                ("A",
                 [ImplicitCtor [];
                  LetBindings [Let (false,[(PLit Unit, Lit Unit)],Lit Unit)];
                  Member
                    (true,
                     PApp
                       (PApp (PApp (PLongVar [PVar "x"; PVar "A"],PVar "b"),PVar "c"),
                        PVar "d"),
                     App
                       (App (Var "op_Addition",Var "b"),
                        App (App (Var "op_Multiply",Var "c"),Var "d")))])]]

[<Test>]
let ``optional arguments in class members``() =
    parse "type Foo() = member this.Foo ?x = defaultArg x 42"
    |> should equal 
          [Types
             [Class
                ("Foo",
                 [ImplicitCtor [];
                  Member
                    (true,PApp (PLongVar [PVar "this"; PVar "Foo"],PVar "x"),
                     App (App (Var "defaultArg",Var "x"),Lit (Int 42)))])]]   
                                
[<Test>]
let ``long name identifier``() =
    parse "let ``my function`` ``this value`` = ``this value``"
    |> should equal [Exp [Let (false,[(PApp (PVar "my function",PVar "this value"), Var "this value")], Lit Unit)]]

[<Test>]
let ``params array attribute``() =
    parse """
        [<AttributeUsage(AttributeTargets.Method, AllowMultiple = true)>]
        type TestAttribute([<ParamArray>] parameters: obj[])  =
            inherit Attribute()
            member this.Parameters = parameters"""
    |> should equal 
         [Types
             [Class
                ("TestAttribute",
                 [ImplicitCtor [PVar "parameters"];
                  ImplicitInherit (TLongIdent [TIdent "Attribute"],Lit Unit,None);
                  Member
                    (true,PLongVar [PVar "this"; PVar "Parameters"],Var "parameters")])]]

[<Test>]
let ``hash constraint``() =
    parse "let something: #IEnumerable option = None"
    |> should equal
          [Exp
             [Let
                (false,
                 [(PVar "something",
                   Typed
                     (Var "None",
                      TApp
                        (TLongIdent [TIdent "option"],
                         [TLongIdent [TIdent "IEnumerable"]])))],Lit Unit)]]

[<Test>]
let ``unit of measure support``() =
    parse """
        [<Measure>] type m
        [<Measure>] type kg
        [<Measure>] type s
        [<Measure>] type N = kg m / s^2
        [<Measure>] type Pa = N / m^2
        let a = 1.<m/Pa*s>"""
    |> should equal 
      [Types [Nothing "m"]; 
       Types [Nothing "kg"]; 
       Types [Nothing "s"];
       Types
         [Abbrev
            ("N",
             TTuple
               [TApp (TLongIdent [TIdent "m"],[TLongIdent [TIdent "kg"]]);
                TMeasurePower (TLongIdent [TIdent "s"],2)])];
       Types
         [Abbrev
            ("Pa",
             TTuple
               [TLongIdent [TIdent "N"]; TMeasurePower (TLongIdent [TIdent "m"],2)])];
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
