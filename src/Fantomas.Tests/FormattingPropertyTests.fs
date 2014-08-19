module Fantomas.Tests.FormattingPropertyTests

open NUnit.Framework
open System
open Fantomas.CodeFormatter
open Fantomas.FormatConfig
open FsCheck
open FsCheck.NUnit
open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Range

let generateRange() =
    Gen.constant range.Zero

let generateSynMeasure() =
    Gen.constant SynMeasure.One

let rec generateSynConst() =
    Gen.oneof [ Gen.constant SynConst.Unit
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
                Gen.map2 (fun x y -> SynConst.String(x, y)) Arb.generate<_> (generateRange())
                Gen.map2 (fun x y -> SynConst.Bytes(x, y)) Arb.generate<_> (generateRange())
                Gen.map SynConst.UInt16s Arb.generate<_>
                Gen.map2 (fun x y -> SynConst.Measure(x, y)) (generateSynConst()) (generateSynMeasure()) ]

let generateSynExpr() =
    Gen.map2 (fun x y -> SynExpr.Const(x, y)) (generateSynConst()) (generateRange())
    
let generateParsedInput() =
    let generateAST expr r =
        let ident = Ident("Tmp", r)
        ParsedInput.ImplFile
            (ParsedImplFileInput
               ("/tmp.fs", false,
                QualifiedNameOfFile ident, [], [],
                [SynModuleOrNamespace
                   ([ident], true,
                    [SynModuleDecl.DoExpr(NoSequencePointAtDoBinding, expr, r)] ,PreXmlDocEmpty, [], None,
                    r)], false))
    Gen.map2 generateAST (generateSynExpr()) (generateRange())

type RangeGenerators =
  static member Range() =
      { new Arbitrary<range>() with
          override x.Generator = generateRange()
          override x.Shrinker t = Seq.empty }

type ParseInputGenerators() =
  static member ParseInput() =
      { new Arbitrary<ParsedInput>() with
          override x.Generator = generateParsedInput()
          override x.Shrinker t = Seq.empty }

[<TestFixtureSetUp>]
let registerFsCheckGenerators =
    Arb.register<RangeGenerators>() |> ignore
    Arb.register<ParseInputGenerators>() |> ignore

let runDoubleFormatAST ast =
    let formatted = formatAST ast "" FormatConfig.Default
    formatAST ast "" FormatConfig.Default = formatted

[<Test>]
let ``running formatting ASTs twice should produce the same results``() =
    Check.Verbose(runDoubleFormatAST)

//[<Property(Verbose = true, Arbitrary = [|typeof<ParseInputGenerators>|])>]
//let ``running formatting ASTs twice should produce the same results 2`` ast =
//    runDoubleFormatAST ast

//let tryFormatSourceString isFsi sourceCode config =
//    try
//        if sourceCode = null then sourceCode
//        else formatSourceString isFsi sourceCode config
//    with :? FormatException ->
//        sourceCode
//
//[<Property(Verbose = true); Ignore>]
//let ``running formatting twice should produce the same results`` sourceCode =
//          let formatted = tryFormatSourceString false sourceCode FormatConfig.Default
//          tryFormatSourceString false formatted FormatConfig.Default = formatted


         

