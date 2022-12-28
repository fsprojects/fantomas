module Fantomas.Core.Tests.FangornTests

open Fantomas.Core
open Fantomas.Core.FormatConfig
open NUnit.Framework
open FSharp.Compiler.Text
open FSharp.Compiler.Xml
open FSharp.Compiler.Syntax
open FSharp.Compiler.SyntaxTrivia

[<Test>]
let ``avoid stack-overflow in long array/list, 2485`` () =
    let mkStringExpr () =
        SynExpr.Const(
            SynConst.String((System.Guid.NewGuid().ToString("N"), SynStringKind.Regular, Range.Zero)),
            Range.Zero
        )

    let longArrayExpr: SynExpr =
        let rec mkArray count childExpr =
            if count = 20_000 then
                childExpr
            else
                mkArray
                    (count + 1)
                    (SynExpr.Sequential(
                        DebugPointAtSequential.SuppressNeither,
                        true,
                        mkStringExpr (),
                        childExpr,
                        Range.Zero
                    ))

        SynExpr.ArrayOrListComputed(true, mkArray 0 (mkStringExpr ()), Range.Zero)

    let ast =
        ParsedInput.ImplFile(
            ParsedImplFileInput(
                "filename.fsx",
                true,
                QualifiedNameOfFile(Ident("", Range.Zero)),
                [],
                [],
                [ SynModuleOrNamespace(
                      [],
                      false,
                      SynModuleOrNamespaceKind.AnonModule,
                      [ SynModuleDecl.Expr(longArrayExpr, Range.Zero) ],
                      PreXmlDoc.Empty,
                      [],
                      None,
                      Range.Zero,
                      { LeadingKeyword = SynModuleOrNamespaceLeadingKeyword.None }
                  ) ],
                (false, false),
                { ConditionalDirectives = []
                  CodeComments = [] },
                Set.empty
            )
        )

    let _rootNode = Fangorn.mkOak FormatConfig.Default None ast
    Assert.Pass()
