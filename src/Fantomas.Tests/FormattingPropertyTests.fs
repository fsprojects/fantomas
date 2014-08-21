module Fantomas.Tests.FormattingPropertyTests

open NUnit.Framework
open System
open Fantomas.CodeFormatter
open Fantomas.FormatConfig
open FsCheck
open FsCheck.NUnit
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Range

let generateSynMeasure =
    Gen.constant SynMeasure.One

let zero = range.Zero

let rec generateSynConst size =
    Gen.oneof 
        [ 
            Gen.constant SynConst.Unit
            Gen.map SynConst.Bool Arb.generate<_>
            Gen.map SynConst.SByte Arb.generate<_>
            Gen.map SynConst.Byte Arb.generate<_>
            Gen.map SynConst.Int16 Arb.generate<_>
            Gen.map SynConst.UInt16 Arb.generate<_>
            Gen.map SynConst.Int32 Arb.generate<_>
            Gen.map SynConst.UInt32 Arb.generate<_>
            Gen.map SynConst.Int64 Arb.generate<_>
            Gen.map SynConst.UInt64 Arb.generate<_>
            Gen.map SynConst.IntPtr Arb.generate<_>
            Gen.map SynConst.UIntPtr Arb.generate<_>
            Gen.map SynConst.Single Arb.generate<_>
            Gen.map SynConst.Double Arb.generate<_>
            Gen.map SynConst.Char Arb.generate<_>
            Gen.map SynConst.Decimal Arb.generate<_>
            Gen.map SynConst.UserNum Arb.generate<_>
            Gen.map (fun x -> SynConst.String(x, zero)) Arb.generate<_>
            Gen.map (fun x -> SynConst.Bytes(x, zero)) Arb.generate<_>
            Gen.map SynConst.UInt16s Arb.generate<_>
            (if size <= 2 then Gen.constant SynConst.Unit 
             else Gen.map2 (fun x y -> SynConst.Measure(x, y)) (generateSynConst (size/2)) generateSynMeasure) 
        ]

let alphaFreqList =
    [ 
        (26, Gen.elements <| seq {'a'..'z'});
        (26, Gen.elements <| seq {'A'..'Z'});
        (1, Gen.elements <| seq ['_'])
    ]

let digitFreqList = [ (10, Gen.elements <| seq {'0'..'9'}) ]

let letter = Gen.frequency alphaFreqList
let letterOrDigit = Gen.frequency <| alphaFreqList @ digitFreqList

let generateIdent size =
    (letter, Gen.listOfLength size letterOrDigit)
    ||> Gen.map2 (fun c cs -> String(c::cs |> List.toArray))

let generateLongIdentWithDots size =
    Gen.map (fun s -> LongIdentWithDots([Ident(s, zero)], [zero])) (generateIdent size)

let generateSynType size =
    Gen.oneof 
        [ 
            Gen.map SynType.LongIdent (generateLongIdentWithDots (size/2))
        ]

let rec generateSynPat size = 
    Gen.oneof 
        [ 
            Gen.constant (SynPat.Wild zero)
            Gen.map (fun c -> SynPat.Const(c, zero)) (generateSynConst (size/2))
        ]

and generateSynSimplePats size =
    Gen.map (fun pat -> fst <| SimplePatsOfPat (SynArgNameGenerator()) pat) (generateSynPat (size/2))

and generateSynMatchClause size =
    Gen.oneof
        [
            Gen.map2 (fun pat expr -> SynMatchClause.Clause(pat, None, expr, zero, SequencePointAtTarget)) (generateSynPat (size/2)) (generateSynExpr (size/2))
            Gen.map3 (fun pat expr1 expr2 -> SynMatchClause.Clause(pat, Some expr1, expr2, zero, SequencePointAtTarget)) (generateSynPat (size/2)) (generateSynExpr (size/2)) (generateSynExpr (size/2))
        ]

and generateIdentExpr size =
    Gen.map (fun s -> SynExpr.Ident(Ident(s, zero))) (generateIdent (size/2))

and generateSynExpr size =
    if size <= 2 then
        generateIdentExpr size
    else
        Gen.oneof 
            [ 
                Gen.map (fun c -> SynExpr.Const(c, zero)) (generateSynConst size)
                Gen.map2 (fun expr typ -> SynExpr.Typed(expr, typ, zero)) (generateSynExpr (size/2)) (generateSynType (size/2))
                Gen.map (fun exprs -> SynExpr.Tuple(exprs, exprs |> List.map (fun _ -> zero), zero)) (Gen.listOf (generateSynExpr (size/2)))
                Gen.map2 (fun b exprs -> SynExpr.ArrayOrList(b, exprs, zero)) Arb.generate<_> (Gen.listOf (generateSynExpr (size/2)))
                Gen.map3 (fun b typ expr -> SynExpr.New(b, typ, SynExpr.Paren(expr, zero, None, zero), zero)) Arb.generate<_> (generateSynType (size/2)) (generateSynExpr (size/2))
                Gen.map2 (fun expr1 expr2 -> SynExpr.While(NoSequencePointAtWhileLoop, expr1, expr2, zero)) (generateSynExpr (size/2)) (generateSynExpr (size/2))
                Gen.map2 (fun b expr -> SynExpr.ArrayOrListOfSeqExpr(b, expr, zero)) Arb.generate<_> (generateSynExpr (size/2))
                Gen.map2 (fun b expr -> SynExpr.CompExpr(b, ref true, expr, zero)) Arb.generate<_> (generateSynExpr (size/2))
                Gen.map (fun expr -> SynExpr.Do(expr, zero)) (generateSynExpr (size/2))
                Gen.map (fun expr -> SynExpr.Assert(expr, zero)) (generateSynExpr (size/2))
                Gen.map (fun expr -> SynExpr.Paren(expr, zero, None, zero)) (generateSynExpr (size/2))
                generateIdentExpr size
                Gen.map2 (fun b expr -> SynExpr.AddressOf(b, expr, zero, zero)) Arb.generate<_> (generateIdentExpr (size/2))
                Gen.constant (SynExpr.Null zero)
                Gen.map (fun expr -> SynExpr.InferredDowncast(expr, zero)) (generateIdentExpr (size/2))
                Gen.map (fun expr -> SynExpr.InferredUpcast(expr, zero)) (generateIdentExpr (size/2))
                Gen.map2 (fun expr typ -> SynExpr.Upcast(expr, typ, zero)) (generateIdentExpr (size/2)) (generateSynType (size/2))
                Gen.map2 (fun expr typ -> SynExpr.Downcast(expr, typ, zero)) (generateIdentExpr (size/2)) (generateSynType (size/2))
                Gen.map2 (fun expr typ -> SynExpr.TypeTest(expr, typ, zero)) (generateIdentExpr (size/2)) (generateSynType (size/2))
                Gen.map2 (fun expr1 expr2 -> SynExpr.DotIndexedGet(expr1, [SynIndexerArg.One expr2], zero, zero)) (generateSynExpr (size/2)) (generateSynExpr (size/2))
                Gen.map3 (fun expr1 expr2 expr3 -> SynExpr.DotIndexedSet(expr1, [SynIndexerArg.One expr3], expr2, zero, zero, zero)) (generateSynExpr (size/2)) (generateSynExpr (size/2)) (generateSynExpr (size/2))
                Gen.map2 (fun expr longIdent -> SynExpr.DotGet(expr, zero, longIdent, zero)) (generateSynExpr (size/2)) (generateLongIdentWithDots (size/2))
                Gen.map3 (fun expr1 expr2 longIdent -> SynExpr.DotSet(expr1, longIdent, expr2, zero)) (generateSynExpr (size/2)) (generateSynExpr (size/2)) (generateLongIdentWithDots (size/2))
                Gen.map2 (fun expr longIdent -> SynExpr.LongIdentSet(longIdent, expr, zero)) (generateSynExpr (size/2)) (generateLongIdentWithDots (size/2))
                Gen.map2 (fun b longIdent -> SynExpr.LongIdent(b, longIdent, None, zero)) Arb.generate<_> (generateLongIdentWithDots (size/2))
                Gen.map3 (fun expr1 expr2 expr3 -> SynExpr.IfThenElse(expr1, expr2, Some expr3, NoSequencePointAtDoBinding, false, zero, zero)) (generateSynExpr (size/2)) (generateSynExpr (size/2)) (generateSynExpr (size/2))
                Gen.map2 (fun expr1 expr2 -> SynExpr.Sequential(SequencePointsAtSeq, true, expr1, expr2, zero)) (generateSynExpr (size/2)) (generateSynExpr (size/2))
                Gen.map (fun expr -> SynExpr.Lazy(expr, zero)) (generateSynExpr (size/2))
                Gen.map2 (fun expr1 expr2 -> SynExpr.TryFinally(expr1, expr2, zero, NoSequencePointAtTry, NoSequencePointAtFinally)) (generateSynExpr (size/2)) (generateSynExpr (size/2))
                Gen.map2 (fun expr clauses -> SynExpr.TryWith(expr, zero, clauses, zero, zero, NoSequencePointAtTry, NoSequencePointAtWith)) (generateSynExpr (size/2)) (Gen.listOf (generateSynMatchClause (size/2)))
                Gen.map2 (fun expr typs -> SynExpr.TypeApp(expr, zero, typs, typs |> List.map (fun _ -> zero), None, zero, zero)) (generateSynExpr (size/2)) (Gen.listOf (generateSynType (size/2)))
                Gen.map3 (fun b expr1 expr2 -> SynExpr.App(ExprAtomicFlag.NonAtomic, b, expr1, expr2, zero)) Arb.generate<_> (generateSynExpr (size/2)) (generateSynExpr (size/2))
                Gen.map2 (fun expr clauses -> SynExpr.Match(NoSequencePointAtDoBinding, expr, clauses, false, zero)) (generateSynExpr (size/2)) (Gen.listOf (generateSynMatchClause (size/2)))
                Gen.map2 (fun b clauses -> SynExpr.MatchLambda(b, zero, clauses, NoSequencePointAtDoBinding, zero)) Arb.generate<_> (Gen.listOf (generateSynMatchClause (size/2)))
                Gen.map3 (fun b pat expr -> SynExpr.Lambda(b, false, pat, expr, zero)) Arb.generate<_> (generateSynSimplePats (size/2)) (generateSynExpr (size/2))
                Gen.map5 (fun b expr1 expr2 expr3 s -> SynExpr.For(NoSequencePointAtForLoop, Ident(s, zero), expr1, b, expr2, expr3, zero)) Arb.generate<_> (generateSynExpr (size/2)) (generateSynExpr (size/2)) (generateSynExpr (size/2)) (generateIdent (size/2))
                Gen.map5 (fun b1 b2 expr1 expr2 pat -> SynExpr.ForEach(NoSequencePointAtForLoop, SeqExprOnly b1, b2, pat, expr1, expr2, zero)) Arb.generate<_> Arb.generate<_> (generateSynExpr (size/2)) (generateSynExpr (size/2)) (generateSynPat (size/2))
            ]
    
let generateParsedInput =
    let generateAST expr =
        let ident = Ident("Tmp", zero)
        ParsedInput.ImplFile
            (ParsedImplFileInput
               ("/tmp.fs", false,
                QualifiedNameOfFile ident, [], [],
                [SynModuleOrNamespace
                   ([ident], true,
                    [SynModuleDecl.DoExpr(NoSequencePointAtDoBinding, expr, zero)], PreXmlDocEmpty, [], None,
                    zero)], false))
    Gen.sized <| fun size -> Gen.map generateAST (generateSynExpr size)

type Input = Input of string

let formatConfig = { FormatConfig.Default with StrictMode = true }

let tryFormatAST ast sourceCode config =
    try
        formatAST ast sourceCode config
    with _ ->
        ""

let generateInput = 
    Gen.map (fun ast -> Input (tryFormatAST ast None formatConfig)) generateParsedInput

type Generators = 
    static member ParsedInput() = 
        { new Arbitrary<ParsedInput>() with
              member __.Generator = generateParsedInput
              member __.Shrinker _ = Seq.empty }
    static member Input() = 
        { new Arbitrary<Input>() with
              member __.Generator = generateInput
              member __.Shrinker _ = Seq.empty }

let quickConf = { Config.Quick with MaxTest = 100; EndSize = 20 }
let verboseConf = { Config.Verbose with MaxTest = 100; EndSize = 20 }

[<TestFixtureSetUp>]
let registerFsCheckGenerators() =
    Arb.register<Generators>() |> ignore

[<Test>]
let ``running formatting ASTs twice should produce the same results``() =
    Check.One(quickConf, 
        fun ast ->
            let formatted = formatAST ast None formatConfig
            Console.WriteLine("Tentative output:\n{0}", formatted)
            formatAST ast None formatConfig = formatted)

let tryFormatSourceString isFsi sourceCode config =
    try
        if sourceCode = null then sourceCode
        else formatSourceString isFsi sourceCode config
    with _ ->
        sourceCode

[<Test>]
let ``running formatting twice should produce the same results``() =
    Check.One(verboseConf, 
        fun (Input sourceCode) ->
            let formatted = tryFormatSourceString false sourceCode formatConfig
            tryFormatSourceString false formatted formatConfig = formatted)


         

