module Fantomas.AstTransformer

open FSharp.Compiler.Text
open FSharp.Compiler.Syntax
open Fantomas.TriviaTypes
open Fantomas.AstExtensions
open Fantomas.RangePatterns
open Fantomas

type Id = { Ident: string; Range: Range }

module Helpers =
    let mkNode (t: FsAstType) (r: range) = TriviaNodeAssigner(MainNode(t), r)
    let mkNodeOption (t: FsAstType) (r: range option) : TriviaNodeAssigner list = Option.toList r |> List.map (mkNode t)

module private Ast =
    open Helpers

    let rec visitSynModuleOrNamespace (modOrNs: SynModuleOrNamespace) : TriviaNodeAssigner list =
        match modOrNs with
        | SynModuleOrNamespace (longIdent, _, kind, decls, _, attrs, _, _range) ->
            let longIdentNodes =
                match kind, decls with
                | SynModuleOrNamespaceKind.AnonModule, _ :: _ -> []
                | _ -> visitLongIdentIncludingFullRange longIdent

            [ yield! longIdentNodes
              yield! (visitSynAttributeLists attrs)
              yield! (decls |> List.collect visitSynModuleDecl) ]

    and visitSynModuleDecl (ast: SynModuleDecl) : TriviaNodeAssigner list =
        let rec visit
            (ast: SynModuleDecl)
            (finalContinuation: TriviaNodeAssigner list -> TriviaNodeAssigner list)
            : TriviaNodeAssigner list =
            match ast with
            | SynModuleDecl.ModuleAbbrev (_, _, range) ->
                [ mkNode SynModuleDecl_ModuleAbbrev range ]
                |> finalContinuation
            | SynModuleDecl.NestedModule (sci, _, decls, _, range) ->
                let continuations: ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                    decls |> List.map visit

                let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                    let afterAttributesBeforeNestedModule =
                        ast.AfterAttributesBeforeNestedModuleName
                        |> Option.map (mkNode SynModuleDecl_NestedModule_AfterAttributesBeforeModuleName)
                        |> Option.toList

                    [ mkNode SynModuleDecl_NestedModule range
                      yield! afterAttributesBeforeNestedModule
                      yield! visitSynComponentInfo sci
                      yield! (List.collect id nodes) ]
                    |> finalContinuation

                Continuation.sequence continuations finalContinuation
            | SynModuleDecl.Let (_, bindings, range) ->
                mkNode SynModuleDecl_Let range
                :: (bindings |> List.collect visitSynBinding)
                |> finalContinuation
            | SynModuleDecl.DoExpr (_, expr, range) ->
                mkNode SynModuleDecl_DoExpr range
                :: visitSynExpr expr
                |> finalContinuation
            | SynModuleDecl.Types (typeDefs, range) ->
                mkNode SynModuleDecl_Types range
                :: (typeDefs |> List.collect visitSynTypeDefn)
                |> finalContinuation
            | SynModuleDecl.Exception (exceptionDef, range) ->
                mkNode SynModuleDecl_Exception range
                :: (visitSynExceptionDefn exceptionDef)
                |> finalContinuation
            | SynModuleDecl.Open (target, parentRange) ->
                // we use the parent ranges here to match up with the trivia parsed
                match target with
                | SynOpenDeclTarget.ModuleOrNamespace (_, _range) ->
                    mkNode SynModuleDecl_Open parentRange
                    |> List.singleton
                    |> finalContinuation
                | SynOpenDeclTarget.Type (synType, _range) ->
                    mkNode SynModuleDecl_OpenType parentRange
                    :: (visitSynType synType)
                    |> finalContinuation
            | SynModuleDecl.Attributes (attrs, range) ->
                mkNode SynModuleDecl_Attributes range
                :: (visitSynAttributeLists attrs)
                |> finalContinuation
            | SynModuleDecl.HashDirective (hash, range) ->
                [ mkNode SynModuleDecl_HashDirective range
                  visitParsedHashDirective hash ]
                |> finalContinuation
            | SynModuleDecl.NamespaceFragment moduleOrNamespace ->
                visitSynModuleOrNamespace moduleOrNamespace
                |> finalContinuation

        visit ast id

    and visitSynExpr (synExpr: SynExpr) : TriviaNodeAssigner list =
        let rec visit
            (synExpr: SynExpr)
            (finalContinuation: TriviaNodeAssigner list -> TriviaNodeAssigner list)
            : TriviaNodeAssigner list =
            match synExpr with
            | SynExpr.Paren (expr, lpr, rpr, range) ->
                visit expr (fun nodes ->
                    [ yield mkNode SynExpr_Paren range
                      yield mkNode SynExpr_Paren_OpeningParenthesis lpr
                      yield! nodes
                      yield! mkNodeOption SynExpr_Paren_ClosingParenthesis rpr ]
                    |> finalContinuation)
            | SynExpr.Quote (operator, _, quotedSynExpr, _, range) ->
                let continuations: ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                    [ visit operator; visit quotedSynExpr ]

                let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                    mkNode SynExpr_Quote range
                    :: (List.collect id nodes)
                    |> finalContinuation

                Continuation.sequence continuations finalContinuation
            | SynExpr.Const (constant, range) -> visitSynConst range constant |> finalContinuation
            | SynExpr.Typed (expr, typeName, _) ->
                visit expr (fun nodes -> nodes @ visitSynType typeName |> finalContinuation)
            | SynExpr.Tuple (_, exprs, _, range) ->
                let continuations: ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                    exprs |> List.map visit

                let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                    mkNode SynExpr_Tuple range
                    :: (List.collect id nodes)
                    |> finalContinuation

                Continuation.sequence continuations finalContinuation
            | SourceParser.ArrayOrList (startRange, _isArray, exprs, endRange, range) ->
                let continuations: ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                    exprs |> List.map visit

                let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                    [ yield mkNode SynExpr_ArrayOrList range
                      yield mkNode SynExpr_ArrayOrList_OpeningDelimiter startRange
                      yield! List.collect id nodes
                      yield mkNode SynExpr_ArrayOrList_ClosingDelimiter endRange ]
                    |> finalContinuation

                Continuation.sequence continuations finalContinuation
            // Captured above
            | SynExpr.ArrayOrList _
            | SynExpr.ArrayOrListComputed _ -> []
            | SynExpr.Record (_, _, recordFields, StartEndRange 1 (openingBrace, range, closingBrace)) ->
                [ yield mkNode SynExpr_Record range
                  yield mkNode SynExpr_Record_OpeningBrace openingBrace
                  yield! List.collect visitRecordField recordFields
                  yield mkNode SynExpr_Record_ClosingBrace closingBrace ]
                |> finalContinuation
            | SynExpr.AnonRecd (_, _, recordFields, range) ->
                mkNode SynExpr_AnonRecd range
                :: (List.collect visitAnonRecordField recordFields)
                |> finalContinuation
            | SynExpr.New (_, typeName, expr, range) ->
                visit expr (fun nodes ->
                    [ mkNode SynExpr_New range
                      yield! nodes
                      yield! visitSynType typeName ]
                    |> finalContinuation)
            | SynExpr.ObjExpr (objType, argOptions, bindings, extraImpls, _, range) ->
                [ yield mkNode SynExpr_ObjExpr range
                  yield! visitSynType objType
                  if argOptions.IsSome then
                      yield! visitArgsOption argOptions.Value
                  yield! extraImpls |> List.collect visitSynInterfaceImpl
                  yield! bindings |> List.collect visitSynBinding ]
                |> finalContinuation
            | SynExpr.While (_, whileExpr, doExpr, range) ->
                let continuations: ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                    [ visit whileExpr; visit doExpr ]

                let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                    mkNode SynExpr_While range
                    :: (List.collect id nodes)
                    |> finalContinuation

                Continuation.sequence continuations finalContinuation
            | SynExpr.For (_, _, identBody, _, toBody, doBody, range) ->
                let continuations: ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                    [ visit identBody
                      visit toBody
                      visit doBody ]

                let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                    mkNode SynExpr_For range
                    :: (List.collect id nodes)
                    |> finalContinuation

                Continuation.sequence continuations finalContinuation
            | SynExpr.ForEach (_, SeqExprOnly _, _, pat, enumExpr, bodyExpr, range) ->
                let continuations: ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                    [ visit enumExpr; visit bodyExpr ]

                let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                    [ yield mkNode SynExpr_ForEach range
                      yield! visitSynPat pat
                      yield! (List.collect id nodes) ]
                    |> finalContinuation

                Continuation.sequence continuations finalContinuation
            | SynExpr.ComputationExpr (_, expr, StartEndRange 1 (openingBrace, _, closingBrace)) ->
                [ yield mkNode SynExpr_ComputationExpr_OpeningBrace openingBrace
                  yield! visit expr finalContinuation
                  yield mkNode SynExpr_ComputationExpr_ClosingBrace closingBrace ]
            | SynExpr.Lambda (_, _, args, arrowRange, body, _parsedData, range) ->
                visit body (fun nodes ->
                    [ yield mkNode SynExpr_Lambda range
                      yield! visitSynSimplePats args
                      yield!
                          (Option.toList arrowRange
                           |> List.map (mkNode SynExpr_Lambda_Arrow))
                      yield! nodes ]
                    |> finalContinuation)
            | SynExpr.MatchLambda (_, keywordRange, matchClauses, _, range) ->
                mkNode SynExpr_MatchLambda range
                :: mkNode SynExpr_MatchLambda_Function keywordRange
                   :: (List.collect visitSynMatchClause matchClauses)
                |> finalContinuation
            | SynExpr.Match (_, expr, clauses, range) ->
                visit expr (fun nodes ->
                    [ yield mkNode SynExpr_Match range
                      yield! nodes
                      yield! (List.collect visitSynMatchClause clauses) ]
                    |> finalContinuation)
            | SynExpr.Do (expr, range) ->
                visit expr (fun nodes ->
                    mkNode SynExpr_Do range :: nodes
                    |> finalContinuation)
            | SynExpr.Assert (expr, range) ->
                visit expr (fun nodes ->
                    mkNode SynExpr_Assert range :: nodes
                    |> finalContinuation)
            | SynExpr.App (_, _, funcExpr, argExpr, range) ->
                let continuations: ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                    [ visit funcExpr; visit argExpr ]

                let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                    mkNode SynExpr_App range
                    :: (List.collect id nodes)
                    |> finalContinuation

                Continuation.sequence continuations finalContinuation
            | SynExpr.TypeApp (expr, _, typeNames, _, _, _, range) ->
                visit expr (fun nodes ->
                    [ yield mkNode SynExpr_TypeApp range
                      yield! nodes
                      yield! (List.collect visitSynType typeNames) ]
                    |> finalContinuation)
            | SynExpr.LetOrUse (_, _, bindings, body, _) ->
                visit body (fun nodes ->
                    [ yield! (List.collect visitSynBinding bindings)
                      yield! nodes ]
                    |> finalContinuation)
            | SynExpr.TryWith (tryExpr, _, withCases, _, range, _, _) ->
                visit tryExpr (fun nodes ->
                    [ yield mkNode SynExpr_TryWith range
                      yield! nodes
                      yield! withCases |> List.collect visitSynMatchClause ]
                    |> finalContinuation)
            | SynExpr.TryFinally (tryExpr, finallyExpr, range, _, _) ->
                let continuations: ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                    [ visit tryExpr; visit finallyExpr ]

                let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                    mkNode SynExpr_TryFinally range
                    :: (List.collect id nodes)
                    |> finalContinuation

                Continuation.sequence continuations finalContinuation
            | SynExpr.Lazy (ex, range) ->
                visit ex (fun nodes ->
                    mkNode SynExpr_Lazy range :: nodes
                    |> finalContinuation)
            | SynExpr.Sequential (_, _, expr1, expr2, _) ->
                visit expr2 (fun nodes1 -> visit expr1 (fun nodes2 -> nodes1 @ nodes2 |> finalContinuation))
            | SynExpr.SequentialOrImplicitYield (_, expr1, expr2, ifNotStmt, range) ->
                let continuations: ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                    [ visit expr1
                      visit expr2
                      visit ifNotStmt ]

                let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                    mkNode SynExpr_SequentialOrImplicitYield range
                    :: (List.collect id nodes)
                    |> finalContinuation

                Continuation.sequence continuations finalContinuation
            // don't collect nested elif expression as nodes.
            // the ranges are often incorrect and using the else if or elif token is more reliable.
            | SourceParser.ElIf ((_leadingElseKw, ifKw, isElif, ifExpr, thenKw, thenExpr) :: es,
                                 (elseKw, elseExpr),
                                 range) ->
                let elifs =
                    es
                    |> List.collect (fun (_, _, _, e1, _, e2) -> [ visit e1; visit e2 ])

                let continuations: ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                    [ yield visit ifExpr
                      yield visit thenExpr
                      yield! elifs
                      yield! (Option.toList elseExpr |> List.map visit) ]

                let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                    let elseNode elseKw =
                        Option.map (mkNode SynExpr_IfThenElse_Else) elseKw
                        |> Option.toList

                    let elifKeywords =
                        es
                        |> List.collect (fun (elseKw, ifKw, isElif, _, thenKw, _) ->
                            [ yield! elseNode elseKw
                              yield
                                  mkNode
                                      (if isElif then
                                           SynExpr_IfThenElse_Elif
                                       else
                                           SynExpr_IfThenElse_If)
                                      ifKw
                              yield mkNode SynExpr_IfThenElse_Then thenKw ])

                    [ yield mkNode SynExpr_IfThenElse range
                      yield
                          mkNode
                              (if isElif then
                                   SynExpr_IfThenElse_Elif
                               else
                                   SynExpr_IfThenElse_If)
                              ifKw
                      yield mkNode SynExpr_IfThenElse_Then thenKw
                      yield! elifKeywords
                      yield! elseNode elseKw
                      yield! (List.collect id nodes) ]
                    |> finalContinuation

                Continuation.sequence continuations finalContinuation

            | SynExpr.IfThenElse (ifKw, isElif, ifExpr, thenKw, thenExpr, elseKw, elseExpr, _, _, _, range) ->
                let continuations: ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                    [ yield visit ifExpr
                      yield visit thenExpr
                      yield! (Option.toList elseExpr |> List.map visit) ]

                let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                    [ yield mkNode SynExpr_IfThenElse range
                      yield
                          mkNode
                              (if isElif then
                                   SynExpr_IfThenElse_Elif
                               else
                                   SynExpr_IfThenElse_If)
                              ifKw
                      yield mkNode SynExpr_IfThenElse_Then thenKw
                      yield!
                          (Option.map (mkNode SynExpr_IfThenElse_Else) elseKw
                           |> Option.toList)
                      yield! (List.collect id nodes) ]
                    |> finalContinuation

                Continuation.sequence continuations finalContinuation
            | SynExpr.Ident id ->
                mkNode SynExpr_Ident id.idRange
                |> List.singleton
                |> finalContinuation
            | SynExpr.LongIdent (_, longDotId, _, range) ->
                mkNode SynExpr_LongIdent range
                :: (visitLongIdentWithDots longDotId)
                |> finalContinuation
            | SynExpr.LongIdentSet (_, expr, range) ->
                visit expr (fun nodes ->
                    mkNode SynExpr_LongIdentSet range :: nodes
                    |> finalContinuation)
            | SynExpr.DotGet (expr, _, longDotId, range) ->
                visit expr (fun nodes ->
                    [ yield mkNode SynExpr_DotGet range
                      yield! nodes
                      // Idents are collected as children here to deal with unit test ``Fluent api with comments should remain on same lines``
                      yield! (visitLongIdentWithDots longDotId) ]
                    |> finalContinuation)
            | SynExpr.DotSet (expr, _, e2, range) ->
                let continuations: ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                    [ visit expr; visit e2 ]

                let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                    mkNode SynExpr_DotSet range
                    :: (List.collect id nodes)
                    |> finalContinuation

                Continuation.sequence continuations finalContinuation
            | SynExpr.Set (e1, e2, range) ->
                let continuations: ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                    [ visit e1; visit e2 ]

                let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                    mkNode SynExpr_Set range
                    :: (List.collect id nodes)
                    |> finalContinuation

                Continuation.sequence continuations finalContinuation
            | SynExpr.DotIndexedGet (objectExpr, indexArgs, _, range) ->
                visit objectExpr (fun nodes ->
                    [ yield mkNode SynExpr_DotIndexedGet range
                      yield! nodes
                      yield! visitSynExpr indexArgs ]
                    |> finalContinuation)
            | SynExpr.DotIndexedSet (objectExpr, indexArgs, valueExpr, _, _, range) ->
                let continuations: ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                    [ visit objectExpr; visit valueExpr ]

                let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                    [ yield mkNode SynExpr_DotIndexedSet range
                      yield! (List.collect id nodes)
                      yield! visitSynExpr indexArgs ]
                    |> finalContinuation

                Continuation.sequence continuations finalContinuation
            | SynExpr.NamedIndexedPropertySet (_, e1, e2, range) ->
                let continuations: ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                    [ visit e1; visit e2 ]

                let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                    mkNode SynExpr_NamedIndexedPropertySet range
                    :: (List.collect id nodes)
                    |> finalContinuation

                Continuation.sequence continuations finalContinuation
            | SynExpr.DotNamedIndexedPropertySet (expr, _, e1, e2, range) ->
                let continuations: ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                    [ visit expr; visit e1; visit e2 ]

                let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                    mkNode SynExpr_DotNamedIndexedPropertySet range
                    :: (List.collect id nodes)
                    |> finalContinuation

                Continuation.sequence continuations finalContinuation
            | SynExpr.TypeTest (expr, typeName, range) ->
                visit expr (fun nodes ->
                    [ yield mkNode SynExpr_TypeTest range
                      yield! nodes
                      yield! visitSynType typeName ]
                    |> finalContinuation)
            | SynExpr.Upcast (expr, typeName, range) ->
                visit expr (fun nodes ->
                    [ yield mkNode SynExpr_Upcast range
                      yield! nodes
                      yield! visitSynType typeName ]
                    |> finalContinuation)
            | SynExpr.Downcast (expr, typeName, range) ->
                visit expr (fun nodes ->
                    [ yield mkNode SynExpr_Downcast range
                      yield! nodes
                      yield! visitSynType typeName ]
                    |> finalContinuation)
            | SynExpr.InferredUpcast (expr, range) ->
                visit expr (fun nodes ->
                    mkNode SynExpr_InferredUpcast range :: nodes
                    |> finalContinuation)
            | SynExpr.InferredDowncast (expr, range) ->
                visit expr (fun nodes ->
                    mkNode SynExpr_InferredDowncast range :: nodes
                    |> finalContinuation)
            | SynExpr.Null range ->
                mkNode SynExpr_Null range
                |> List.singleton
                |> finalContinuation
            | SynExpr.AddressOf (_, expr, _, range) ->
                visit expr (fun nodes ->
                    mkNode SynExpr_AddressOf range :: nodes
                    |> finalContinuation)
            | SynExpr.TraitCall (_typars, sign, expr, range) ->
                visit expr (fun nodes ->
                    [ yield mkNode SynExpr_TraitCall range
                      yield! visitSynMemberSig sign
                      yield! nodes ]
                    |> finalContinuation)
            | SynExpr.JoinIn (expr, _, expr2, range) ->
                let continuations: ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                    [ visit expr; visit expr2 ]

                let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                    mkNode SynExpr_JoinIn range
                    :: (List.collect id nodes)
                    |> finalContinuation

                Continuation.sequence continuations finalContinuation
            | SynExpr.ImplicitZero range ->
                mkNode SynExpr_ImplicitZero range
                |> List.singleton
                |> finalContinuation
            | SynExpr.YieldOrReturn (_, expr, range) ->
                visit expr (fun nodes ->
                    mkNode SynExpr_YieldOrReturn range :: nodes
                    |> finalContinuation)
            | SynExpr.YieldOrReturnFrom (_, expr, range) ->
                visit expr (fun nodes ->
                    mkNode SynExpr_YieldOrReturnFrom range :: nodes
                    |> finalContinuation)
            | SynExpr.LetOrUseBang (_, _, _, pat, rhsExpr, andBangs, body, range) ->
                let continuations: ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                    [ yield visit rhsExpr
                      yield visit body
                      yield! (List.map (fun (_, _, _, _, body, _) -> visit body) andBangs) ]

                let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                    [ yield mkNode SynExpr_LetOrUseBang range
                      yield! visitSynPat pat
                      yield! (List.collect id nodes)
                      yield!
                          andBangs
                          |> List.collect (fun (_, _, _, pat, _, _) -> visitSynPat pat) ]
                    |> finalContinuation

                Continuation.sequence continuations finalContinuation
            | SynExpr.MatchBang (_, expr, clauses, range) ->
                visit expr (fun nodes ->
                    [ yield mkNode SynExpr_MatchBang range
                      yield! nodes
                      yield! clauses |> List.collect visitSynMatchClause ]
                    |> finalContinuation)
            | SynExpr.DoBang (expr, range) ->
                visit expr (fun nodes ->
                    mkNode SynExpr_DoBang range :: nodes
                    |> finalContinuation)
            | SynExpr.LibraryOnlyILAssembly (_, _, _, _, range) ->
                mkNode SynExpr_LibraryOnlyILAssembly range
                |> List.singleton
                |> finalContinuation
            | SynExpr.LibraryOnlyStaticOptimization (_, _, _, range) ->
                mkNode SynExpr_LibraryOnlyStaticOptimization range
                |> List.singleton
                |> finalContinuation
            | SynExpr.LibraryOnlyUnionCaseFieldGet (expr, _, _, range) ->
                visit expr (fun nodes ->
                    mkNode SynExpr_LibraryOnlyUnionCaseFieldGet range
                    :: nodes
                    |> finalContinuation)
            | SynExpr.LibraryOnlyUnionCaseFieldSet (e1, _, _, e2, range) ->
                let continuations: ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                    [ visit e1; visit e2 ]

                let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                    mkNode SynExpr_LibraryOnlyUnionCaseFieldSet range
                    :: (List.collect id nodes)
                    |> finalContinuation

                Continuation.sequence continuations finalContinuation
            | SynExpr.ArbitraryAfterError (_, range) ->
                mkNode SynExpr_ArbitraryAfterError range
                |> List.singleton
                |> finalContinuation
            | SynExpr.FromParseError (expr, range) ->
                visit expr (fun nodes ->
                    mkNode SynExpr_FromParseError range :: nodes
                    |> finalContinuation)
            | SynExpr.DiscardAfterMissingQualificationAfterDot (expr, range) ->
                visit expr (fun nodes ->
                    mkNode SynExpr_DiscardAfterMissingQualificationAfterDot range
                    :: nodes
                    |> finalContinuation)
            | SynExpr.Fixed (expr, range) ->
                visit expr (fun nodes ->
                    mkNode SynExpr_Fixed range :: nodes
                    |> finalContinuation)
            | SynExpr.InterpolatedString (parts, _, range) ->
                mkNode SynExpr_InterpolatedString range
                :: (List.collect visitSynInterpolatedStringPart parts)
                |> finalContinuation
            | SynExpr.IndexRange (e1, _, e2, _, _, range) ->
                [ yield mkNode SynExpr_IndexRange range
                  yield! (e1 |> Option.toList |> List.collect visitSynExpr)
                  yield! (e2 |> Option.toList |> List.collect visitSynExpr) ]
                |> finalContinuation
            | SynExpr.IndexFromEnd (e, range) ->
                [ yield mkNode SynExpr_IndexFromEnd range
                  yield! visitSynExpr e ]

        visit synExpr id

    and visitSynInterpolatedStringPart (synInterpolatedStringPart: SynInterpolatedStringPart) =
        match synInterpolatedStringPart with
        | SynInterpolatedStringPart.String (_, range) ->
            mkNode SynInterpolatedStringPart_String range
            |> List.singleton
        | SynInterpolatedStringPart.FillExpr (expr, ident) ->
            visitSynExpr expr
            @ (Option.toList ident |> List.map visitIdent)

    and visitRecordField ((longId, _): RecordFieldName, expr: SynExpr option, _: BlockSeparator option) =
        mkNode RecordField_ longId.Range
        :: (match expr with
            | Some e -> visitSynExpr e
            | None -> [])

    and visitAnonRecordField (_: Ident, expr: SynExpr) = visitSynExpr expr

    and visitAnonRecordTypeField (_: Ident, t: SynType) = visitSynType t

    and visitSynMemberSig (ms: SynMemberSig) : TriviaNodeAssigner list =
        match ms with
        | SynMemberSig.Member (valSig, _, range) ->
            mkNode SynMemberSig_Member range
            :: (visitSynValSig valSig)
        | SynMemberSig.Interface (typeName, range) ->
            mkNode SynMemberSig_Interface range
            :: (visitSynType typeName)
        | SynMemberSig.Inherit (typeName, range) ->
            mkNode SynMemberSig_Inherit range
            :: (visitSynType typeName)
        | SynMemberSig.ValField (f, range) ->
            mkNode SynMemberSig_ValField range
            :: (visitSynField f)
        | SynMemberSig.NestedType (typedef, range) ->
            mkNode SynMemberSig_NestedType range
            :: (visitSynTypeDefnSig typedef)

    and visitSynMatchClause (mc: SynMatchClause) : TriviaNodeAssigner list =
        match mc with
        | SynMatchClause (pat, e1, arrowRange, e2, _range, _) ->
            mkNode SynMatchClause_ mc.Range // _range is the same range as pat, see https://github.com/dotnet/fsharp/issues/10877
            :: [ yield! visitSynPat pat
                 if e1.IsSome then
                     yield! visitSynExpr e1.Value
                 yield!
                     (Option.toList arrowRange
                      |> List.map (mkNode SynMatchClause_Arrow))
                 yield! visitSynExpr e2 ]

    and visitArgsOption (expr: SynExpr, _: Ident option) = visitSynExpr expr

    and visitSynInterfaceImpl (ii: SynInterfaceImpl) : TriviaNodeAssigner list =
        match ii with
        | SynInterfaceImpl (typ, bindings, range) ->
            [ yield mkNode SynInterfaceImpl_ range
              yield! visitSynType typ
              yield! (bindings |> List.collect visitSynBinding) ]

    and visitSynTypeDefn (td: SynTypeDefn) =
        match td with
        | SynTypeDefn (sci, stdr, members, implicitConstructor, range) ->
            let afterAttributesBeforeRepr =
                match td.AfterAttributesBeforeComponentInfo with
                | None -> []
                | Some r ->
                    mkNode SynTypeDefn_AfterAttributesBeforeComponentInfo r
                    |> List.singleton

            // TODO process implicitConstructor ??
            [ yield mkNode SynTypeDefn_ range
              yield! visitSynComponentInfo sci
              yield! afterAttributesBeforeRepr
              yield! visitSynTypeDefnRepr stdr
              yield! (members |> List.collect visitSynMemberDefn) ]

    and visitSynTypeDefnSig (typeDefSig: SynTypeDefnSig) : TriviaNodeAssigner list =
        match typeDefSig with
        | SynTypeDefnSig (sci, synTypeDefnSigReprs, memberSig, _) ->
            [ yield mkNode SynTypeDefnSig_ typeDefSig.Range
              yield! visitSynComponentInfo sci
              yield! visitSynTypeDefnSigRepr synTypeDefnSigReprs
              yield! (memberSig |> List.collect visitSynMemberSig) ]

    and visitSynTypeDefnSigRepr (stdr: SynTypeDefnSigRepr) : TriviaNodeAssigner list =
        match stdr with
        | SynTypeDefnSigRepr.ObjectModel (kind, members, _) ->
            visitSynTypeDefnKind kind
            @ (members |> List.collect visitSynMemberSig)
        | SynTypeDefnSigRepr.Simple (simpleRepr, _) -> (visitSynTypeDefnSimpleRepr simpleRepr)
        | SynTypeDefnSigRepr.Exception exceptionRepr -> visitSynExceptionDefnRepr exceptionRepr

    and visitSynMemberDefn (mbrDef: SynMemberDefn) : TriviaNodeAssigner list =
        match mbrDef with
        | SynMemberDefn.Open (target, parentRange) ->
            // we use the parent ranges here to match up with the trivia parsed
            match target with
            | SynOpenDeclTarget.ModuleOrNamespace (_, _range) ->
                mkNode SynMemberDefn_Open parentRange
                |> List.singleton
            | SynOpenDeclTarget.Type (synType, _range) ->
                mkNode SynMemberDefn_OpenType parentRange
                :: (visitSynType synType)
        | SynMemberDefn.Member (memberDefn, range) ->
            mkNode SynMemberDefn_Member range
            :: (visitSynBinding memberDefn)
        | SynMemberDefn.ImplicitCtor (_, attrs, ctorArgs, _, _xmlDoc, range) ->
            [ yield mkNode SynMemberDefn_ImplicitCtor range
              yield! (visitSynAttributeLists attrs)
              yield! visitSynSimplePats ctorArgs ]
        | SynMemberDefn.ImplicitInherit (inheritType, inheritArgs, _, range) ->
            [ yield mkNode SynMemberDefn_ImplicitInherit range
              yield! visitSynType inheritType
              yield! visitSynExpr inheritArgs ]
        | SynMemberDefn.LetBindings (bindings, _, _, range) ->
            mkNode SynMemberDefn_LetBindings range
            :: (List.collect visitSynBinding bindings)
        | SynMemberDefn.AbstractSlot (valSig, _, range) ->
            mkNode SynMemberDefn_AbstractSlot range
            :: (visitSynValSig valSig)
        | SynMemberDefn.Interface (typ, members, range) ->
            [ yield mkNode SynMemberDefn_Interface range
              yield! visitSynType typ
              if members.IsSome then
                  yield! members.Value |> List.collect visitSynMemberDefn ]
        | SynMemberDefn.Inherit (typ, _, range) ->
            mkNode SynMemberDefn_Inherit range
            :: (visitSynType typ)
        | SynMemberDefn.ValField (fld, range) ->
            mkNode SynMemberDefn_ValField range
            :: (visitSynField fld)
        | SynMemberDefn.NestedType (typeDefn, _, range) ->
            mkNode SynMemberDefn_NestedType range
            :: (visitSynTypeDefn typeDefn)
        | SynMemberDefn.AutoProperty (attrs, _, _, typeOpt, _, _, _, _, synExpr, _, range) ->
            [ yield mkNode SynMemberDefn_AutoProperty range
              yield! (visitSynAttributeLists attrs)
              if typeOpt.IsSome then
                  yield! visitSynType typeOpt.Value
              yield! visitSynExpr synExpr ]

    and visitSynSimplePat (sp: SynSimplePat) : TriviaNodeAssigner list =
        let rec visit
            (sp: SynSimplePat)
            (continuation: TriviaNodeAssigner list -> TriviaNodeAssigner list)
            : TriviaNodeAssigner list =
            match sp with
            | SynSimplePat.Id (_, _, _, _, _, range) ->
                mkNode SynSimplePat_Id range
                |> List.singleton
                |> continuation
            | SynSimplePat.Typed (simplePat, typ, range) ->
                visit simplePat (fun nodes ->
                    [ yield mkNode SynSimplePat_Typed range
                      yield! nodes
                      yield! visitSynType typ ]
                    |> continuation)
            | SynSimplePat.Attrib (simplePat, attrs, range) ->
                visit simplePat (fun nodes ->
                    [ yield mkNode SynSimplePat_Attrib range
                      yield! nodes
                      yield! (visitSynAttributeLists attrs) ]
                    |> continuation)

        visit sp id

    and visitSynSimplePats (sp: SynSimplePats) : TriviaNodeAssigner list =
        let rec visit (sp: SynSimplePats) (continuation: TriviaNodeAssigner list -> TriviaNodeAssigner list) =
            match sp with
            | SynSimplePats.SimplePats (pats, range) ->
                mkNode SynSimplePats_SimplePats range
                :: (List.collect visitSynSimplePat pats)
                |> continuation
            | SynSimplePats.Typed (pats, typ, range) ->
                visit pats (fun nodes ->
                    [ yield mkNode SynSimplePat_Typed range
                      yield! nodes
                      yield! visitSynType typ ]
                    |> continuation)

        visit sp id

    and visitSynBinding (binding: SynBinding) : TriviaNodeAssigner list =
        match binding with
        | SynBinding (_, kind, _, _, attrs, _, valData, headPat, returnInfo, expr, _range, _) ->
            let t =
                match kind with
                | SynBindingKind.StandaloneExpression -> SynBindingKind_StandaloneExpression
                | SynBindingKind.Normal -> SynBindingKind_Normal
                | SynBindingKind.Do -> SynBindingKind_Do

            let afterAttributesBeforeHeadPattern =
                match binding.AfterAttributesBeforeHeadPattern with
                | Some r ->
                    mkNode SynBinding_AfterAttributes_BeforeHeadPattern r
                    |> List.singleton
                | None -> []

            let headPatNodes =
                match kind with
                | SynBindingKind.Do -> []
                | _ -> visitSynPat headPat

            [ yield mkNode t binding.RangeOfBindingWithRhs
              yield! visitSynAttributeLists attrs
              yield! afterAttributesBeforeHeadPattern
              yield! visitSynValData valData
              yield! headPatNodes
              yield!
                  (match returnInfo with
                   | Some ri -> visitSynBindingReturnInfo ri
                   | None -> [])
              yield! visitSynExpr expr ]

    and visitSynValData (svd: SynValData) : TriviaNodeAssigner list =
        match svd with
        | SynValData (_, svi, _) -> visitSynValInfo svi

    and visitSynValSig (svs: SynValSig) : TriviaNodeAssigner list =
        match svs with
        | SynValSig (attrs, ident, explicitValDecls, synType, arity, _, _, _, _, expr, range) ->
            [ yield mkNode SynValSig_ range
              yield visitIdent ident
              yield! (visitSynAttributeLists attrs)
              yield! visitSynValTyparDecls explicitValDecls
              yield! visitSynType synType
              yield! visitSynValInfo arity
              if expr.IsSome then
                  yield! visitSynExpr expr.Value ]

    and visitSynValTyparDecls (valTypeDecl: SynValTyparDecls) : TriviaNodeAssigner list =
        match valTypeDecl with
        | SynValTyparDecls (Some typardecl, _) -> visitSynTyparDecls typardecl
        | _ -> []

    and visitSynTyparDecl (std: SynTyparDecl) : TriviaNodeAssigner list =
        match std with
        | SynTyparDecl (attrs, _) -> [ yield! (visitSynAttributeLists attrs) ]

    and visitSynBindingReturnInfo (returnInfo: SynBindingReturnInfo) : TriviaNodeAssigner list =
        match returnInfo with
        | SynBindingReturnInfo (typeName, range, attrs) ->
            [ yield mkNode SynBindingReturnInfo_ range
              yield! visitSynType typeName
              yield! (visitSynAttributeLists attrs) ]

    and visitSynPat (sp: SynPat) : TriviaNodeAssigner list =
        let rec visit
            (sp: SynPat)
            (finalContinuation: TriviaNodeAssigner list -> TriviaNodeAssigner list)
            : TriviaNodeAssigner list =
            match sp with
            | SynPat.Const (sc, range) -> visitSynConst range sc |> finalContinuation
            | SynPat.Wild range ->
                mkNode SynPat_Wild range
                |> List.singleton
                |> finalContinuation
            | SynPat.Named (ident, _, _, range) ->
                [ mkNode SynPat_Named range
                  visitIdent ident ]
                |> finalContinuation
            | SynPat.As (synPat, synPat2, range) ->
                let continuations: ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                    [ visit synPat; visit synPat2 ]

                let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                    mkNode SynPat_As range :: (List.collect id nodes)
                    |> finalContinuation

                Continuation.sequence continuations finalContinuation
            | SynPat.Typed (synPat, synType, range) ->
                visit synPat (fun nodes ->
                    mkNode SynPat_Typed range
                    :: (nodes @ visitSynType synType)
                    |> finalContinuation)
            | SynPat.Attrib (synPat, attrs, range) ->
                visit synPat (fun nodes ->
                    [ yield mkNode SynPat_Attrib range
                      yield! nodes
                      yield! (visitSynAttributeLists attrs) ]
                    |> finalContinuation)
            | SynPat.Or (synPat, synPat2, _range) ->
                let continuations: ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                    [ visit synPat; visit synPat2 ]

                let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                    List.collect id nodes |> finalContinuation

                Continuation.sequence continuations finalContinuation
            | SynPat.Ands (pats, range) ->
                let continuations: ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                    pats |> List.map visit

                let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                    mkNode SynPat_Ands range
                    :: (List.collect id nodes)
                    |> finalContinuation

                Continuation.sequence continuations finalContinuation
            | SynPat.LongIdent (_, _, svtd, ctorArgs, _, range) ->
                [ yield mkNode SynPat_LongIdent range
                  if svtd.IsSome then
                      yield! visitSynValTyparDecls svtd.Value
                  yield! visitSynConstructorArgs ctorArgs ]
                |> finalContinuation
            | SynPat.Tuple (_, pats, range) ->
                let continuations: ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                    pats |> List.map visit

                let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                    mkNode SynPat_Tuple range
                    :: (List.collect id nodes)
                    |> finalContinuation

                Continuation.sequence continuations finalContinuation
            | SynPat.Paren (pat, StartEndRange 1 (lpr, range, rpr)) ->
                visit pat (fun nodes ->
                    [ yield mkNode SynPat_Paren range
                      yield mkNode SynPat_Paren_OpeningParenthesis lpr
                      yield! nodes
                      yield mkNode SynPat_Paren_ClosingParenthesis rpr ]
                    |> finalContinuation)

            | SynPat.ArrayOrList (_, pats, range) ->
                let continuations: ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                    pats |> List.map visit

                let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                    mkNode SynPat_ArrayOrList range
                    :: (List.collect id nodes)
                    |> finalContinuation

                Continuation.sequence continuations finalContinuation

            | SynPat.Record (pats, range) ->
                let continuations: ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                    pats |> List.map (snd >> visit)

                let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                    mkNode SynPat_Record range
                    :: (List.collect id nodes)
                    |> finalContinuation

                Continuation.sequence continuations finalContinuation
            | SynPat.Null range ->
                mkNode SynPat_Null range
                |> List.singleton
                |> finalContinuation
            | SynPat.OptionalVal (_, range) ->
                mkNode SynPat_OptionalVal range
                |> List.singleton
                |> finalContinuation
            | SynPat.IsInst (typ, range) ->
                mkNode SynPat_IsInst range :: visitSynType typ
                |> finalContinuation
            | SynPat.QuoteExpr (expr, range) ->
                mkNode SynPat_QuoteExpr range :: visitSynExpr expr
                |> finalContinuation
            | SynPat.DeprecatedCharRange (_, _, range) ->
                mkNode SynPat_DeprecatedCharRange range
                |> List.singleton
                |> finalContinuation
            | SynPat.InstanceMember (_, _, _, _, range) ->
                mkNode SynPat_InstanceMember range
                |> List.singleton
                |> finalContinuation
            | SynPat.FromParseError (pat, range) ->
                visit pat (fun nodes ->
                    mkNode SynPat_FromParseError range :: nodes
                    |> finalContinuation)

        visit sp id

    and visitSynConstructorArgs (ctorArgs: SynArgPats) : TriviaNodeAssigner list =
        match ctorArgs with
        | SynArgPats.Pats pats -> List.collect visitSynPat pats
        | SynArgPats.NamePatPairs (pats, range) ->
            mkNode SynArgPats_NamePatPairs range
            :: (List.collect (snd >> visitSynPat) pats)

    and visitSynComponentInfo (sci: SynComponentInfo) : TriviaNodeAssigner list =
        match sci with
        | SynComponentInfo (attribs, typeParams, _, _, _, _, _, range) ->
            [ yield mkNode SynComponentInfo_ range
              yield! (visitSynAttributeLists attribs)
              yield!
                  (Option.map visitSynTyparDecls typeParams
                   |> Option.defaultValue []) ]

    and visitSynTyparDecls (decls: SynTyparDecls) : TriviaNodeAssigner list =
        match decls with
        | SynTyparDecls.PostfixList (decls, _constraints, range) ->
            [ yield mkNode SynTyparDecls_PostfixList range
              yield! (List.collect visitSynTyparDecl decls) ]
        | SynTyparDecls.PrefixList (decls, range) ->
            [ yield mkNode SynTyparDecls_PrefixList range
              yield! (List.collect visitSynTyparDecl decls) ]
        | SynTyparDecls.SinglePrefix (decl, range) ->
            [ yield mkNode SynTyparDecls_SinglePrefix range
              yield! (visitSynTyparDecl decl) ]

    and visitSynTypeDefnRepr (stdr: SynTypeDefnRepr) : TriviaNodeAssigner list =
        match stdr with
        | SynTypeDefnRepr.ObjectModel (kind, members, _) ->
            visitSynTypeDefnKind kind
            @ (members |> List.collect visitSynMemberDefn)
        | SynTypeDefnRepr.Simple (simpleRepr, _) -> visitSynTypeDefnSimpleRepr simpleRepr
        | SynTypeDefnRepr.Exception exceptionRepr -> visitSynExceptionDefnRepr exceptionRepr

    and visitSynTypeDefnKind (kind: SynTypeDefnKind) : TriviaNodeAssigner list =
        match kind with
        | SynTypeDefnKind.Unspecified
        | SynTypeDefnKind.Class
        | SynTypeDefnKind.Interface
        | SynTypeDefnKind.Struct
        | SynTypeDefnKind.Record
        | SynTypeDefnKind.Abbrev
        | SynTypeDefnKind.Opaque
        | SynTypeDefnKind.Augmentation
        | SynTypeDefnKind.Union
        | SynTypeDefnKind.IL -> []
        | SynTypeDefnKind.Delegate (typ, valinfo) -> visitSynType typ @ visitSynValInfo valinfo

    and visitSynTypeDefnSimpleRepr (arg: SynTypeDefnSimpleRepr) =
        match arg with
        | SynTypeDefnSimpleRepr.None range ->
            mkNode SynTypeDefnSimpleRepr_None range
            |> List.singleton
        | SynTypeDefnSimpleRepr.Union (_, unionCases, range) ->
            mkNode SynTypeDefnSimpleRepr_Union range
            :: (List.collect visitSynUnionCase unionCases)
        | SynTypeDefnSimpleRepr.Enum (enumCases, range) ->
            mkNode SynTypeDefnSimpleRepr_Enum range
            :: (List.collect visitSynEnumCase enumCases)
        | SynTypeDefnSimpleRepr.Record (_, recordFields, StartEndRange 1 (openingBrace, range, closingBrace)) ->
            [ yield mkNode SynTypeDefnSimpleRepr_Record range
              yield mkNode SynTypeDefnSimpleRepr_Record_OpeningBrace openingBrace
              yield! List.collect visitSynField recordFields
              yield mkNode SynTypeDefnSimpleRepr_Record_ClosingBrace closingBrace ]
        | SynTypeDefnSimpleRepr.General (_, _, _, _, _, _, _, range) ->
            mkNode SynTypeDefnSimpleRepr_General range
            |> List.singleton
        | SynTypeDefnSimpleRepr.LibraryOnlyILAssembly (_, range) ->
            mkNode SynTypeDefnSimpleRepr_LibraryOnlyILAssembly range
            |> List.singleton
        | SynTypeDefnSimpleRepr.TypeAbbrev (_, typ, range) ->
            mkNode SynTypeDefnSimpleRepr_TypeAbbrev range
            :: (visitSynType typ)
        | SynTypeDefnSimpleRepr.Exception edr -> visitSynExceptionDefnRepr edr

    and visitSynExceptionDefn (exceptionDef: SynExceptionDefn) : TriviaNodeAssigner list =
        match exceptionDef with
        | SynExceptionDefn (sedr, members, range) ->
            [ yield mkNode SynExceptionDefn_ range
              yield! visitSynExceptionDefnRepr sedr
              yield! (members |> List.collect visitSynMemberDefn) ]

    and visitSynExceptionDefnRepr (sedr: SynExceptionDefnRepr) : TriviaNodeAssigner list =
        match sedr with
        | SynExceptionDefnRepr (attrs, unionCase, _, _, _, _range) ->
            let fullRange = sedr.FullRange

            [ yield mkNode SynExceptionDefnRepr_ fullRange
              yield! (visitSynAttributeLists attrs)
              yield! visitSynUnionCase unionCase ]

    and visitSynAttribute (attr: SynAttribute) : TriviaNodeAssigner list =
        mkNode SynAttribute_ attr.Range
        :: (visitSynExpr attr.ArgExpr)

    and visitSynAttributeLists (attrs: SynAttributeList list) : TriviaNodeAssigner list =
        List.collect visitSynAttributeList attrs
    //        match attrs with
//        | [ h ] -> visitSynAttributeList h
//        | _ :: tail ->
//            let aRanges =
//                tail
//                |> List.map (fun a -> a.Range)
//                |> fun r -> r @ [ parentRange ]
//
//            List.zip attrs aRanges
//            |> List.collect (fun (a, r) -> visitSynAttributeList r a)
//        | [] -> []

    and visitSynAttributeList (attrs: SynAttributeList) : TriviaNodeAssigner list =
        TriviaNodeAssigner(MainNode(SynAttributeList_), attrs.Range)
        :: (List.collect visitSynAttribute attrs.Attributes)

    and visitSynUnionCase (uc: SynUnionCase) : TriviaNodeAssigner list =
        match uc with
        | SynUnionCase (attrs, _, uct, _, _, range) ->
            [ yield mkNode SynUnionCase_ range
              yield! visitSynUnionCaseType uct
              yield! (visitSynAttributeLists attrs) ]

    and visitSynUnionCaseType (uct: SynUnionCaseKind) =
        match uct with
        | SynUnionCaseKind.Fields fields -> List.collect visitSynField fields
        | SynUnionCaseKind.FullType (stype, valInfo) -> visitSynType stype @ visitSynValInfo valInfo

    and visitSynEnumCase (sec: SynEnumCase) : TriviaNodeAssigner list =
        match sec with
        | SynEnumCase (attrs, ident, value, valueRange, _, range) ->
            [ yield mkNode SynEnumCase_ range
              yield! (visitSynAttributeLists attrs)
              yield visitIdent ident
              yield! visitSynConst valueRange value ]

    and visitSynField (sfield: SynField) : TriviaNodeAssigner list =
        match sfield with
        | SynField (attrs, _, _ident, typ, _, _, _, range) ->
            let afterAttributesBeforeIdentifier =
                match sfield.AfterAttributesBeforeIdentifier with
                | None -> []
                | Some r ->
                    mkNode SynField_AfterAttributesBeforeIdentifier r
                    |> List.singleton

            [ yield mkNode SynField_ range
              yield! (visitSynAttributeLists attrs)
              yield! afterAttributesBeforeIdentifier
              yield! visitSynType typ ]

    and visitSynType (st: SynType) =
        let rec visit
            (st: SynType)
            (finalContinuation: TriviaNodeAssigner list -> TriviaNodeAssigner list)
            : TriviaNodeAssigner list =
            match st with
            | SynType.LongIdent li -> visitLongIdentWithDots li |> finalContinuation
            | SynType.App (typeName, _, typeArgs, _, _, _, range) ->
                let continuations: ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                    [ yield! (List.map visit typeArgs)
                      yield visit typeName ]

                let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                    mkNode SynType_App range
                    :: (List.collect id nodes)
                    |> finalContinuation

                Continuation.sequence continuations finalContinuation
            | SynType.LongIdentApp (typeName, _, _, typeArgs, _, _, range) ->
                let continuations: ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                    [ yield! (List.map visit typeArgs)
                      yield visit typeName ]

                let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                    mkNode SynType_LongIdentApp range
                    :: (List.collect id nodes)
                    |> finalContinuation

                Continuation.sequence continuations finalContinuation
            | SynType.Tuple (_, typeNames, range) ->
                let continuations: ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                    List.map (snd >> visit) typeNames

                let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                    mkNode SynType_Tuple range
                    :: (List.collect id nodes)
                    |> finalContinuation

                Continuation.sequence continuations finalContinuation
            | SynType.Array (_, elementType, range) ->
                visit elementType (fun nodes ->
                    mkNode SynType_Array range :: nodes
                    |> finalContinuation)
            | SynType.Fun (argType, returnType, range) ->
                let continuations: ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                    [ visit argType; visit returnType ]

                let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                    mkNode SynType_Fun range
                    :: (List.collect id nodes)
                    |> finalContinuation

                Continuation.sequence continuations finalContinuation
            | SynType.Var (_, range) ->
                mkNode SynType_Var range
                |> List.singleton
                |> finalContinuation
            | SynType.Anon range ->
                mkNode SynType_Anon range
                |> List.singleton
                |> finalContinuation
            | SynType.WithGlobalConstraints (typeName, _, range) ->
                visit typeName (fun nodes ->
                    mkNode SynType_WithGlobalConstraints range
                    :: nodes
                    |> finalContinuation)
            | SynType.HashConstraint (synType, range) ->
                visit synType (fun nodes ->
                    mkNode SynType_HashConstraint range :: nodes
                    |> finalContinuation)
            | SynType.MeasureDivide (dividendType, divisorType, range) ->
                let continuations: ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                    [ visit dividendType
                      visit divisorType ]

                let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                    mkNode SynType_MeasureDivide range
                    :: (List.collect id nodes)
                    |> finalContinuation

                Continuation.sequence continuations finalContinuation
            | SynType.MeasurePower (measureType, _, range) ->
                visit measureType (fun nodes ->
                    mkNode SynType_MeasurePower range :: nodes
                    |> finalContinuation)
            | SynType.StaticConstant (constant, range) ->
                [ yield mkNode SynType_StaticConstant range
                  yield! visitSynConst range constant ]
                |> finalContinuation
            | SynType.StaticConstantExpr (expr, range) ->
                mkNode SynType_StaticConstantExpr range
                :: (visitSynExpr expr)
                |> finalContinuation
            | SynType.StaticConstantNamed (expr, typ, range) ->
                let continuations: ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                    [ visit expr; visit typ ]

                let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                    mkNode SynType_StaticConstantNamed range
                    :: (List.collect id nodes)
                    |> finalContinuation

                Continuation.sequence continuations finalContinuation
            | SynType.AnonRecd (_, typeNames, range) ->
                mkNode SynType_AnonRecd range
                :: (List.collect visitAnonRecordTypeField typeNames)
                |> finalContinuation
            | SynType.Paren (innerType, StartEndRange 1 (lpr, range, rpr)) ->
                visit innerType (fun nodes ->
                    [ yield mkNode SynType_Paren range
                      yield mkNode SynType_Paren_OpeningParenthesis lpr
                      yield! nodes
                      yield mkNode SynType_Paren_ClosingParenthesis rpr ]
                    |> finalContinuation)

        visit st id

    and visitSynConst (parentRange: Range) (sc: SynConst) : TriviaNodeAssigner list =
        let t sc =
            match sc with
            | SynConst.Bool _ -> SynConst_Bool
            | SynConst.Unit -> SynConst_Unit
            | SynConst.SByte _ -> SynConst_SByte
            | SynConst.Byte _ -> SynConst_Byte
            | SynConst.Int16 _ -> SynConst_Int16
            | SynConst.UInt16 _ -> SynConst_UInt16
            | SynConst.Int32 _ -> SynConst_Int32
            | SynConst.UInt32 _ -> SynConst_UInt32
            | SynConst.Int64 _ -> SynConst_Int64
            | SynConst.UInt64 _ -> SynConst_UInt64
            | SynConst.IntPtr _ -> SynConst_IntPtr
            | SynConst.UIntPtr _ -> SynConst_UIntPtr
            | SynConst.Single _ -> SynConst_Single
            | SynConst.Double _ -> SynConst_Double
            | SynConst.Char _ -> SynConst_Char
            | SynConst.Decimal _ -> SynConst_Decimal
            | SynConst.UserNum _ -> SynConst_UserNum
            | SynConst.String _ -> SynConst_String
            | SynConst.Bytes _ -> SynConst_Bytes
            | SynConst.UInt16s _ -> SynConst_UInt16s
            | SynConst.Measure _ -> SynConst_Measure
            | SynConst.SourceIdentifier _ -> SynConst_SourceIdentifier

        match sc with
        | SynConst.Measure (n, numberRange, _) -> [ mkNode (t n) numberRange ]
        | SynConst.Unit ->
            match parentRange with
            | StartEndRange 1 (lpr, range, rpr) ->
                [ mkNode SynConst_Unit range
                  mkNode SynConst_Unit_OpeningParenthesis lpr
                  mkNode SynConst_Unit_ClosingParenthesis rpr ]
        | _ -> [ mkNode (t sc) (sc.Range parentRange) ]

    and visitSynValInfo (svi: SynValInfo) =
        match svi with
        | SynValInfo (args, arg) ->
            (List.collect (List.collect visitSynArgInfo) args)
            @ visitSynArgInfo arg

    and visitSynArgInfo (sai: SynArgInfo) : TriviaNodeAssigner list =
        match sai with
        | SynArgInfo (attrs, _, _ident) -> visitSynAttributeLists attrs

    and visitParsedHashDirective (hash: ParsedHashDirective) : TriviaNodeAssigner =
        match hash with
        | ParsedHashDirective (_, _, range) -> mkNode ParsedHashDirective_ range

    and visitSynModuleOrNamespaceSig (modOrNs: SynModuleOrNamespaceSig) : TriviaNodeAssigner list =
        match modOrNs with
        | SynModuleOrNamespaceSig (longIdent, _, kind, decls, _, attrs, _, _range) ->
            let longIdentNodes =
                match kind, decls with
                | SynModuleOrNamespaceKind.AnonModule, _ :: _ -> []
                | _ -> visitLongIdentIncludingFullRange longIdent

            [ yield! longIdentNodes
              yield! (visitSynAttributeLists attrs)
              yield! (decls |> List.collect visitSynModuleSigDecl) ]

    and visitSynModuleSigDecl (ast: SynModuleSigDecl) : TriviaNodeAssigner list =
        let rec visit
            (ast: SynModuleSigDecl)
            (finalContinuation: TriviaNodeAssigner list -> TriviaNodeAssigner list)
            : TriviaNodeAssigner list =
            match ast with
            | SynModuleSigDecl.ModuleAbbrev (_, _, range) ->
                mkNode SynModuleSigDecl_ModuleAbbrev range
                |> List.singleton
                |> finalContinuation
            | SynModuleSigDecl.NestedModule (sci, _, decls, range) ->
                let continuations: ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                    List.map visit decls

                let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                    let afterAttributesBeforeNestedModule =
                        ast.AfterAttributesBeforeNestedModuleName
                        |> Option.map (mkNode SynModuleSigDecl_NestedModule_AfterAttributesBeforeModuleName)
                        |> Option.toList

                    [ yield mkNode SynModuleSigDecl_NestedModule range
                      yield! afterAttributesBeforeNestedModule
                      yield! visitSynComponentInfo sci
                      yield! (List.collect id nodes) ]
                    |> finalContinuation

                Continuation.sequence continuations finalContinuation
            | SynModuleSigDecl.Val (SynValSig.SynValSig _ as node, _) -> visitSynValSig node |> finalContinuation
            | SynModuleSigDecl.Types (typeDefs, range) ->
                mkNode SynModuleSigDecl_Types range
                :: (List.collect visitSynTypeDefnSig typeDefs)
                |> finalContinuation
            | SynModuleSigDecl.Open (target, parentRange) ->
                // we use the parent ranges here to match up with the trivia parsed
                match target with
                | SynOpenDeclTarget.ModuleOrNamespace (_, _range) ->
                    mkNode SynModuleSigDecl_Open parentRange
                    |> List.singleton
                    |> finalContinuation
                | SynOpenDeclTarget.Type (synType, _range) ->
                    mkNode SynModuleSigDecl_OpenType parentRange
                    :: (visitSynType synType)
                    |> finalContinuation
            | SynModuleSigDecl.HashDirective (hash, range) ->
                [ mkNode SynModuleSigDecl_HashDirective range
                  (visitParsedHashDirective hash) ]
                |> finalContinuation
            | SynModuleSigDecl.NamespaceFragment moduleOrNamespace ->
                visitSynModuleOrNamespaceSig moduleOrNamespace
                |> finalContinuation
            | SynModuleSigDecl.Exception (synExceptionSig, _range) ->
                let fullRange = ast.FullRange

                mkNode SynModuleSigDecl_Exception fullRange
                :: (visitSynExceptionSig synExceptionSig)
                |> finalContinuation

        visit ast id

    and visitSynExceptionSig (exceptionDef: SynExceptionSig) : TriviaNodeAssigner list =
        match exceptionDef with
        | SynExceptionSig (sedr, members, _range) ->
            let fullRange = exceptionDef.FullRange

            [ yield mkNode SynExceptionSig_ fullRange
              yield! visitSynExceptionDefnRepr sedr
              yield! (members |> List.collect visitSynMemberSig) ]

    and visitLongIdentWithDots (lid: LongIdentWithDots) : TriviaNodeAssigner list =
        match lid with
        | LongIdentWithDots (ids, _) -> List.map visitIdent ids

    and visitLongIdentIncludingFullRange (li: LongIdent) : TriviaNodeAssigner list =
        // LongIdent is a bit of an artificial AST node
        // meant to be used as namespace or module identifier
        mkNode LongIdent_ (longIdentFullRange li)
        :: List.map visitIdent li

    and visitIdent (ident: Ident) : TriviaNodeAssigner = mkNode Ident_ ident.idRange

let astToNode (hds: ParsedHashDirective list) (mdls: SynModuleOrNamespace list) : TriviaNodeAssigner list =
    [ yield! List.collect Ast.visitSynModuleOrNamespace mdls
      yield! List.map Ast.visitParsedHashDirective hds ]

let sigAstToNode (ast: SynModuleOrNamespaceSig list) : TriviaNodeAssigner list =
    List.collect Ast.visitSynModuleOrNamespaceSig ast
