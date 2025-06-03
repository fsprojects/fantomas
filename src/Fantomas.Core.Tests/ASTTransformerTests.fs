module Fantomas.Core.Tests.ASTTransformerTests

open NUnit.Framework
open Fantomas.FCS.Text
open Fantomas.FCS.Xml
open Fantomas.FCS.Syntax
open Fantomas.FCS.SyntaxTrivia
open Fantomas.Core

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
                        Range.Zero,
                        SynExprSequentialTrivia.Zero
                    ))

        SynExpr.ArrayOrListComputed(true, mkArray 0 (mkStringExpr ()), Range.Zero)

    let ast =
        ParsedInput.ImplFile(
            ParsedImplFileInput(
                "filename.fsx",
                true,
                QualifiedNameOfFile(Ident("", Range.Zero)),
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
                ParsedInputTrivia.Empty,
                Set.empty
            )
        )

    let _rootNode = ASTTransformer.mkOak None ast
    Assert.Pass()
