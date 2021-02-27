module Fantomas.AstTransformer

open FSharp.Compiler.Text
open FSharp.Compiler.SyntaxTree
open Fantomas.TriviaTypes
open Fantomas

type Id = { Ident: string; Range: Range }

module Helpers =
    let i (id: Ident) : Id =
        { Ident = id.idText
          Range = id.idRange }

    let li (id: LongIdent) = id |> List.map i

    let lid (id: LongIdentWithDots) = li id.Lid
    let mkNode (t: FsAstType) (r: range) = TriviaNodeAssigner(MainNode(t), r)

module private Ast =
    open Helpers

    let rec visitSynModuleOrNamespace (modOrNs: SynModuleOrNamespace) : TriviaNodeAssigner list =
        match modOrNs with
        | SynModuleOrNamespace (longIdent, _, synModuleOrNamespaceKind, decls, _, attrs, _, range) ->
            let collectIdents (idents: LongIdent) =
                idents
                |> List.map (fun ident -> mkNode Ident_ ident.idRange)

            let typeName =
                match synModuleOrNamespaceKind with
                | SynModuleOrNamespaceKind.AnonModule -> SynModuleOrNamespace_AnonModule
                | SynModuleOrNamespaceKind.NamedModule -> SynModuleOrNamespace_NamedModule
                | SynModuleOrNamespaceKind.DeclaredNamespace -> SynModuleOrNamespace_DeclaredNamespace
                | SynModuleOrNamespaceKind.GlobalNamespace -> SynModuleOrNamespace_GlobalNamespace

            [
              // LongIdent inside Namespace is being processed as children.
              if typeName <> SynModuleOrNamespace_DeclaredNamespace then
                  mkNode typeName range
              yield!
                  if synModuleOrNamespaceKind = SynModuleOrNamespaceKind.DeclaredNamespace then
                      collectIdents longIdent
                  else
                      []
              yield! (visitSynAttributeLists range attrs)
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
                let continuations : ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                    decls |> List.map visit

                let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                    [ mkNode SynModuleDecl_NestedModule range
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
                :: (visitSynAttributeLists range attrs)
                |> finalContinuation
            | SynModuleDecl.HashDirective (hash, range) ->
                [ mkNode SynModuleDecl_HashDirective range
                  visitParsedHashDirective hash ]
                |> finalContinuation
            | SynModuleDecl.NamespaceFragment (moduleOrNamespace) ->
                visitSynModuleOrNamespace moduleOrNamespace
                |> finalContinuation

        visit ast id

    and visitSynExpr (synExpr: SynExpr) : TriviaNodeAssigner list =
        let rec visit
            (synExpr: SynExpr)
            (finalContinuation: TriviaNodeAssigner list -> TriviaNodeAssigner list)
            : TriviaNodeAssigner list =
            match synExpr with
            | SynExpr.Paren (expr, _, _, range) ->
                visit
                    expr
                    (fun nodes ->
                        [ mkNode SynExpr_Paren range
                          yield! nodes ]
                        |> finalContinuation)
            | SynExpr.Quote (operator, _, quotedSynExpr, _, range) ->
                let continuations : ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                    [ visit operator; visit quotedSynExpr ]

                let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                    [ mkNode SynExpr_Quote range
                      yield! (List.collect id nodes) ]
                    |> finalContinuation

                Continuation.sequence continuations finalContinuation
            | SynExpr.Const (constant, range) ->
                [ mkNode SynExpr_Const range
                  visitSynConst range constant ]
                |> finalContinuation
            | SynExpr.Typed (expr, typeName, _) ->
                visit
                    expr
                    (fun nodes ->
                        //                        { Type = SynExpr_Typed
//                           Range = r range
//                           Properties = Map.empty
//                            }
                        nodes @ visitSynType typeName |> finalContinuation)
            | SynExpr.Tuple (_, exprs, _, range) ->
                let continuations : ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                    exprs |> List.map visit

                let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                    [ mkNode SynExpr_Tuple range
                      yield! (List.collect id nodes) ]
                    |> finalContinuation

                Continuation.sequence continuations finalContinuation
            | SynExpr.ArrayOrList (_, exprs, range) ->
                let continuations : ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                    exprs |> List.map visit

                let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                    [ mkNode SynExpr_ArrayOrList range
                      yield! (List.collect id nodes) ]
                    |> finalContinuation

                Continuation.sequence continuations finalContinuation
            | SynExpr.Record (_, _, recordFields, range) ->
                mkNode SynExpr_Record range
                :: (List.collect visitRecordField recordFields)
                |> finalContinuation
            | SynExpr.AnonRecd (_, _, recordFields, range) ->
                mkNode SynExpr_AnonRecd range
                :: (List.collect visitAnonRecordField recordFields)
                |> finalContinuation
            | SynExpr.New (_, typeName, expr, range) ->
                visit
                    expr
                    (fun nodes ->
                        [ mkNode SynExpr_New range
                          yield! nodes
                          yield! visitSynType typeName ]
                        |> finalContinuation)
            | SynExpr.ObjExpr (objType, argOptions, bindings, extraImpls, _, range) ->
                mkNode SynExpr_ObjExpr range
                :: [ yield! visitSynType objType
                     if argOptions.IsSome then
                         yield! visitArgsOption argOptions.Value
                     yield! extraImpls |> List.collect visitSynInterfaceImpl
                     yield! bindings |> List.collect visitSynBinding ]
                |> finalContinuation
            | SynExpr.While (_, whileExpr, doExpr, range) ->
                let continuations : ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                    [ visit whileExpr; visit doExpr ]

                let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                    mkNode SynExpr_While range
                    :: (List.collect id nodes)
                    |> finalContinuation

                Continuation.sequence continuations finalContinuation
            | SynExpr.For (_, _, identBody, _, toBody, doBody, range) ->
                let continuations : ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                    [ visit identBody
                      visit toBody
                      visit doBody ]

                let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                    mkNode SynExpr_For range
                    :: (List.collect id nodes)
                    |> finalContinuation

                Continuation.sequence continuations finalContinuation
            | SynExpr.ForEach (_, (SeqExprOnly _), _, pat, enumExpr, bodyExpr, range) ->
                let continuations : ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                    [ visit enumExpr; visit bodyExpr ]

                let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                    [ mkNode SynExpr_ForEach range
                      yield! visitSynPat pat
                      yield! (List.collect id nodes) ]
                    |> finalContinuation

                Continuation.sequence continuations finalContinuation
            | SynExpr.ArrayOrListOfSeqExpr (_, expr, range) ->
                visit
                    expr
                    (fun nodes ->
                        mkNode SynExpr_ArrayOrListOfSeqExpr range :: nodes
                        |> finalContinuation)
            | SynExpr.CompExpr (_, _, expr, _) -> visit expr finalContinuation
            //                        { Type = SynExpr_CompExpr
//                          Range = r range
//                          Properties = Map.empty }
            | SynExpr.Lambda (_, _, args, body, _parsedData, range) ->
                visit
                    body
                    (fun nodes ->
                        [ mkNode SynExpr_Lambda range
                          yield! visitSynSimplePats args
                          yield! nodes ]
                        |> finalContinuation)
            | SynExpr.MatchLambda (_, _, matchClauses, _, range) ->
                mkNode SynExpr_MatchLambda range
                :: (List.collect visitSynMatchClause matchClauses)
                |> finalContinuation
            | SynExpr.Match (_, expr, clauses, range) ->
                visit
                    expr
                    (fun nodes ->
                        [ mkNode SynExpr_Match range
                          yield! nodes
                          yield! (List.collect visitSynMatchClause clauses) ]
                        |> finalContinuation)
            | SynExpr.Do (expr, range) ->
                visit
                    expr
                    (fun nodes ->
                        mkNode SynExpr_Do range :: nodes
                        |> finalContinuation)
            | SynExpr.Assert (expr, range) ->
                visit
                    expr
                    (fun nodes ->
                        mkNode SynExpr_Assert range :: nodes
                        |> finalContinuation)
            | SynExpr.App (_, _, funcExpr, argExpr, range) ->
                let continuations : ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                    [ visit funcExpr; visit argExpr ]

                let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                    mkNode SynExpr_App range
                    :: (List.collect id nodes)
                    |> finalContinuation

                Continuation.sequence continuations finalContinuation
            | SynExpr.TypeApp (expr, _, typeNames, _, _, _, range) ->
                visit
                    expr
                    (fun nodes ->
                        [ mkNode SynExpr_TypeApp range
                          yield! nodes
                          yield! (List.collect visitSynType typeNames) ]
                        |> finalContinuation)
            | SynExpr.LetOrUse (_, _, bindings, body, _) ->
                visit
                    body
                    (fun nodes ->
                        //                          { Type = SynExpr_LetOrUse
//                            Range = r range
//                            Properties =
//                                p [ "isRecursive" ==> isRecursive
//                                    "isUse" ==> isUse ]
//                             }
                        (List.collect visitSynBinding bindings) @ nodes
                        |> finalContinuation)
            | SynExpr.TryWith (tryExpr, _, withCases, _, range, _, _) ->
                visit
                    tryExpr
                    (fun nodes ->
                        [ mkNode SynExpr_TryWith range
                          yield! nodes
                          yield! withCases |> List.collect visitSynMatchClause ]
                        |> finalContinuation)
            | SynExpr.TryFinally (tryExpr, finallyExpr, range, _, _) ->
                let continuations : ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                    [ visit tryExpr; visit finallyExpr ]

                let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                    mkNode SynExpr_TryFinally range
                    :: (List.collect id nodes)
                    |> finalContinuation

                Continuation.sequence continuations finalContinuation
            | SynExpr.Lazy (ex, range) ->
                visit
                    ex
                    (fun nodes ->
                        mkNode SynExpr_Lazy range :: nodes
                        |> finalContinuation)
            | SynExpr.Sequential (_, _, expr1, expr2, _) ->
                let continuations : ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                    [ visit expr1; visit expr2 ]

                let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                    //                    { Type = SynExpr_Sequential
//                      Range = r range
//                      Properties = Map.empty
//                       }
                    (List.collect id nodes) |> finalContinuation

                Continuation.sequence continuations finalContinuation
            | SynExpr.SequentialOrImplicitYield (_, expr1, expr2, ifNotStmt, range) ->
                let continuations : ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                    [ visit expr1
                      visit expr2
                      visit ifNotStmt ]

                let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                    mkNode SynExpr_SequentialOrImplicitYield range
                    :: (List.collect id nodes)
                    |> finalContinuation

                Continuation.sequence continuations finalContinuation
            | SynExpr.IfThenElse (ifExpr, thenExpr, elseExpr, _, _, _, range) ->
                let continuations : ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                    [ visit ifExpr
                      visit thenExpr
                      yield! (Option.toList elseExpr |> List.map visit) ]

                let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                    mkNode SynExpr_IfThenElse range
                    :: (List.collect id nodes)
                    |> finalContinuation

                Continuation.sequence continuations finalContinuation
            | SynExpr.Ident (id) ->
                mkNode SynExpr_Ident (i id).Range
                |> List.singleton
                |> finalContinuation
            | SynExpr.LongIdent (_, longDotId, _, range) ->
                mkNode SynExpr_LongIdent range
                :: (visitLongIdentWithDots longDotId)
                |> finalContinuation
            | SynExpr.LongIdentSet (_, expr, range) ->
                visit
                    expr
                    (fun nodes ->
                        mkNode SynExpr_LongIdentSet range :: nodes
                        |> finalContinuation)
            | SynExpr.DotGet (expr, _, longDotId, range) ->
                visit
                    expr
                    (fun nodes ->
                        [ mkNode SynExpr_DotGet range
                          yield! nodes
                          // Idents are collected as children here to deal with unit test ``Fluent api with comments should remain on same lines``
                          yield! (visitLongIdentWithDots longDotId) ]
                        |> finalContinuation)
            | SynExpr.DotSet (expr, _, e2, range) ->
                let continuations : ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                    [ visit expr; visit e2 ]

                let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                    mkNode SynExpr_DotSet range
                    :: (List.collect id nodes)
                    |> finalContinuation

                Continuation.sequence continuations finalContinuation
            | SynExpr.Set (e1, e2, range) ->
                let continuations : ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                    [ visit e1; visit e2 ]

                let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                    mkNode SynExpr_Set range
                    :: (List.collect id nodes)
                    |> finalContinuation

                Continuation.sequence continuations finalContinuation
            | SynExpr.DotIndexedGet (objectExpr, indexExprs, _, range) ->
                visit
                    objectExpr
                    (fun nodes ->
                        [ mkNode SynExpr_DotIndexedGet range
                          yield! nodes
                          yield! indexExprs |> List.collect visitSynIndexerArg ]
                        |> finalContinuation)
            | SynExpr.DotIndexedSet (objectExpr, indexExprs, valueExpr, _, _, range) ->
                let continuations : ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                    [ visit objectExpr; visit valueExpr ]

                let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                    [ mkNode SynExpr_DotIndexedSet range
                      yield! (List.collect id nodes)
                      yield! indexExprs |> List.collect visitSynIndexerArg ]
                    |> finalContinuation

                Continuation.sequence continuations finalContinuation
            | SynExpr.NamedIndexedPropertySet (_, e1, e2, range) ->
                let continuations : ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                    [ visit e1; visit e2 ]

                let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                    mkNode SynExpr_NamedIndexedPropertySet range
                    :: (List.collect id nodes)
                    |> finalContinuation

                Continuation.sequence continuations finalContinuation
            | SynExpr.DotNamedIndexedPropertySet (expr, _, e1, e2, range) ->
                let continuations : ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                    [ visit expr; visit e1; visit e2 ]

                let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                    mkNode SynExpr_DotNamedIndexedPropertySet range
                    :: (List.collect id nodes)
                    |> finalContinuation

                Continuation.sequence continuations finalContinuation
            | SynExpr.TypeTest (expr, typeName, range) ->
                visit
                    expr
                    (fun nodes ->
                        [ mkNode SynExpr_TypeTest range
                          yield! nodes
                          yield! visitSynType typeName ]
                        |> finalContinuation)
            | SynExpr.Upcast (expr, typeName, range) ->
                visit
                    expr
                    (fun nodes ->
                        [ mkNode SynExpr_Upcast range
                          yield! nodes
                          yield! visitSynType typeName ]
                        |> finalContinuation)
            | SynExpr.Downcast (expr, typeName, range) ->
                visit
                    expr
                    (fun nodes ->
                        [ mkNode SynExpr_Downcast range
                          yield! nodes
                          yield! visitSynType typeName ]
                        |> finalContinuation)
            | SynExpr.InferredUpcast (expr, range) ->
                visit
                    expr
                    (fun nodes ->
                        mkNode SynExpr_InferredUpcast range :: nodes
                        |> finalContinuation)
            | SynExpr.InferredDowncast (expr, range) ->
                visit
                    expr
                    (fun nodes ->
                        mkNode SynExpr_InferredDowncast range :: nodes
                        |> finalContinuation)
            | SynExpr.Null (range) ->
                mkNode SynExpr_Null range
                |> List.singleton
                |> finalContinuation
            | SynExpr.AddressOf (_, expr, _, range) ->
                visit
                    expr
                    (fun nodes ->
                        mkNode SynExpr_AddressOf range :: nodes
                        |> finalContinuation)
            | SynExpr.TraitCall (typars, sign, expr, range) ->
                visit
                    expr
                    (fun nodes ->
                        [ mkNode SynExpr_TraitCall range
                          yield! typars |> List.collect visitSynTypar
                          yield! visitSynMemberSig sign
                          yield! nodes ]
                        |> finalContinuation)
            | SynExpr.JoinIn (expr, _, expr2, range) ->
                let continuations : ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                    [ visit expr; visit expr2 ]

                let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                    mkNode SynExpr_JoinIn range
                    :: (List.collect id nodes)
                    |> finalContinuation

                Continuation.sequence continuations finalContinuation
            | SynExpr.ImplicitZero (range) ->
                mkNode SynExpr_ImplicitZero range
                |> List.singleton
                |> finalContinuation
            | SynExpr.YieldOrReturn (_, expr, range) ->
                visit
                    expr
                    (fun nodes ->
                        mkNode SynExpr_YieldOrReturn range :: nodes
                        |> finalContinuation)
            | SynExpr.YieldOrReturnFrom (_, expr, range) ->
                visit
                    expr
                    (fun nodes ->
                        mkNode SynExpr_YieldOrReturnFrom range :: nodes
                        |> finalContinuation)
            | SynExpr.LetOrUseBang (_, _, _, pat, rhsExpr, andBangs, body, range) ->
                let continuations : ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                    [ visit rhsExpr
                      visit body
                      yield! (List.map (fun (_, _, _, _, body, _) -> visit body) andBangs) ]

                let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                    [ mkNode SynExpr_LetOrUseBang range
                      yield! visitSynPat pat
                      yield! (List.collect id nodes)
                      yield!
                          andBangs
                          |> List.collect (fun (_, _, _, pat, _, _) -> visitSynPat pat) ]
                    |> finalContinuation

                Continuation.sequence continuations finalContinuation
            | SynExpr.MatchBang (_, expr, clauses, range) ->
                visit
                    expr
                    (fun nodes ->
                        [ mkNode SynExpr_MatchBang range
                          yield! nodes
                          yield! clauses |> List.collect visitSynMatchClause ]
                        |> finalContinuation)
            | SynExpr.DoBang (expr, range) ->
                visit
                    expr
                    (fun nodes ->
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
                visit
                    expr
                    (fun nodes ->
                        mkNode SynExpr_LibraryOnlyUnionCaseFieldGet range
                        :: nodes
                        |> finalContinuation)
            | SynExpr.LibraryOnlyUnionCaseFieldSet (e1, _, _, e2, range) ->
                let continuations : ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
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
                visit
                    expr
                    (fun nodes ->
                        mkNode SynExpr_FromParseError range :: nodes
                        |> finalContinuation)
            | SynExpr.DiscardAfterMissingQualificationAfterDot (expr, range) ->
                visit
                    expr
                    (fun nodes ->
                        mkNode SynExpr_DiscardAfterMissingQualificationAfterDot range
                        :: nodes
                        |> finalContinuation)
            | SynExpr.Fixed (expr, range) ->
                visit
                    expr
                    (fun nodes ->
                        mkNode SynExpr_Fixed range :: nodes
                        |> finalContinuation)
            | SynExpr.InterpolatedString (parts, range) ->
                mkNode SynExpr_InterpolatedString range
                :: (List.collect visitSynInterpolatedStringPart parts)
                |> finalContinuation

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

    and visitSynIndexerArg (ia: SynIndexerArg) : TriviaNodeAssigner list =
        match ia with
        | SynIndexerArg.One (e, _fromEnd, _) -> visitSynExpr e
        | SynIndexerArg.Two (e1, _fromEnd1, e2, _fromEnd2, _, _) -> visitSynExpr e1 @ visitSynExpr e2

    and visitSynMatchClause (mc: SynMatchClause) : TriviaNodeAssigner list =
        match mc with
        | SynMatchClause.Clause (pat, e1, e2, _range, _) ->
            mkNode SynMatchClause_Clause mc.Range // _range is the same range as pat, see https://github.com/dotnet/fsharp/issues/10877
            :: [ yield! visitSynPat pat
                 if e1.IsSome then
                     yield! visitSynExpr e1.Value
                 yield! visitSynExpr e2 ]

    and visitArgsOption (expr: SynExpr, _: Ident option) = visitSynExpr expr

    and visitSynInterfaceImpl (ii: SynInterfaceImpl) : TriviaNodeAssigner list =
        match ii with
        | InterfaceImpl (typ, bindings, range) ->
            [ mkNode InterfaceImpl_ range
              yield! visitSynType typ
              yield! (bindings |> List.collect visitSynBinding) ]

    and visitSynTypeDefn (td: SynTypeDefn) =
        match td with
        | TypeDefn (sci, stdr, members, range) ->
            [ mkNode TypeDefn_ range
              yield! visitSynComponentInfo sci
              yield! visitSynTypeDefnRepr stdr
              yield! (members |> List.collect visitSynMemberDefn) ]

    and visitSynTypeDefnSig (typeDefSig: SynTypeDefnSig) : TriviaNodeAssigner list =
        match typeDefSig with
        | TypeDefnSig (sci, synTypeDefnSigReprs, memberSig, _) ->
            //            { Type = TypeDefnSig_
//              Range = r range
//              Properties = Map.empty
//              FsAstNode = typeDefSig }
            [ yield! visitSynComponentInfo sci
              yield! visitSynTypeDefnSigRepr synTypeDefnSigReprs
              yield! (memberSig |> List.collect visitSynMemberSig) ]

    and visitSynTypeDefnSigRepr (stdr: SynTypeDefnSigRepr) : TriviaNodeAssigner list =
        match stdr with
        | SynTypeDefnSigRepr.ObjectModel (kind, members, _) ->
            //            { Type = SynTypeDefnSigRepr_ObjectModel
//              Range = r range
//              Properties = Map.empty
//              FsAstNode = stdr }
            visitSynTypeDefnKind kind
            @ (members |> List.collect visitSynMemberSig)
        | SynTypeDefnSigRepr.Simple (simpleRepr, _) ->
            //            { Type = SynTypeDefnSigRepr_ObjectModel
//              Range = r range
//              Properties = Map.empty
//              FsAstNode = stdr }
            (visitSynTypeDefnSimpleRepr simpleRepr)
        | SynTypeDefnSigRepr.Exception (exceptionRepr) -> visitSynExceptionDefnRepr exceptionRepr

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
            [ mkNode SynMemberDefn_ImplicitCtor range
              yield! (visitSynAttributeLists range attrs)
              yield! visitSynSimplePats ctorArgs ]
        | SynMemberDefn.ImplicitInherit (inheritType, inheritArgs, _, range) ->
            [ mkNode SynMemberDefn_ImplicitInherit range
              yield! visitSynType inheritType
              yield! visitSynExpr inheritArgs ]
        | SynMemberDefn.LetBindings (bindings, _, _, range) ->
            mkNode SynMemberDefn_LetBindings range
            :: (List.collect visitSynBinding bindings)
        | SynMemberDefn.AbstractSlot (valSig, _, range) ->
            mkNode SynMemberDefn_AbstractSlot range
            :: (visitSynValSig valSig)
        | SynMemberDefn.Interface (typ, members, range) ->
            [ mkNode SynMemberDefn_Interface range
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
            [ mkNode SynMemberDefn_AutoProperty range
              yield! (visitSynAttributeLists range attrs)
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
                visit
                    simplePat
                    (fun nodes ->
                        [ mkNode SynSimplePat_Typed range
                          yield! nodes
                          yield! visitSynType typ ]
                        |> continuation)
            | SynSimplePat.Attrib (simplePat, attrs, range) ->
                visit
                    simplePat
                    (fun nodes ->
                        [ mkNode SynSimplePat_Attrib range
                          yield! nodes
                          yield! (visitSynAttributeLists range attrs) ]
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
                visit
                    pats
                    (fun nodes ->
                        [ mkNode SynSimplePat_Typed range
                          yield! nodes
                          yield! visitSynType typ ]
                        |> continuation)

        visit sp id

    and visitSynBinding (binding: SynBinding) : TriviaNodeAssigner list =
        match binding with
        | Binding (_, kind, _, _, attrs, _, valData, headPat, returnInfo, expr, range, _) ->
            let t =
                match kind with
                | SynBindingKind.StandaloneExpression -> StandaloneExpression_
                | SynBindingKind.NormalBinding -> NormalBinding_
                | SynBindingKind.DoBinding -> DoBinding_

            [ mkNode t binding.RangeOfBindingAndRhs
              yield! visitSynAttributeLists range attrs
              yield! visitSynValData valData
              yield! visitSynPat headPat
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
        | ValSpfn (attrs, _, explicitValDecls, synType, arity, _, _, _, _, expr, range) ->
            [ mkNode ValSpfn_ range
              yield! (visitSynAttributeLists range attrs)
              yield! visitSynValTyparDecls explicitValDecls
              yield! visitSynType synType
              yield! visitSynValInfo arity
              if expr.IsSome then
                  yield! visitSynExpr expr.Value ]

    and visitSynValTyparDecls (valTypeDecl: SynValTyparDecls) : TriviaNodeAssigner list =
        match valTypeDecl with
        | SynValTyparDecls (typardecls, _, _) -> List.collect visitSynTyparDecl typardecls

    and visitSynTyparDecl (std: SynTyparDecl) : TriviaNodeAssigner list =
        match std with
        | TyparDecl (attrs, typar) ->
            [ yield! (visitSynAttributeLists typar.Range attrs)
              yield! visitSynTypar typar ]

    and visitSynTypar (typar: SynTypar) : TriviaNodeAssigner list =
        match typar with
        | Typar _ -> []

    and visitTyparStaticReq (tsr: TyparStaticReq) =
        match tsr with
        | NoStaticReq -> "NoStaticReq"
        | HeadTypeStaticReq -> "HeadTypeStaticReq"

    and visitSynBindingReturnInfo (returnInfo: SynBindingReturnInfo) : TriviaNodeAssigner list =
        match returnInfo with
        | SynBindingReturnInfo (typeName, range, attrs) ->
            [ mkNode SynBindingReturnInfo_ range
              yield! visitSynType typeName
              yield! (visitSynAttributeLists range attrs) ]

    and visitSynPat (sp: SynPat) : TriviaNodeAssigner list =
        let rec visit
            (sp: SynPat)
            (finalContinuation: TriviaNodeAssigner list -> TriviaNodeAssigner list)
            : TriviaNodeAssigner list =
            match sp with
            | SynPat.Const (sc, range) ->
                [ mkNode SynPat_Const range
                  visitSynConst range sc ]
                |> finalContinuation
            | SynPat.Wild (range) ->
                mkNode SynPat_Wild range
                |> List.singleton
                |> finalContinuation
            | SynPat.Named (synPat, _, _, _, range) ->
                visit
                    synPat
                    (fun nodes ->
                        mkNode SynPat_Named range :: nodes
                        |> finalContinuation)
            | SynPat.Typed (synPat, synType, range) ->
                visit
                    synPat
                    (fun nodes ->
                        mkNode SynPat_Typed range
                        :: (nodes @ visitSynType synType)
                        |> finalContinuation)
            | SynPat.Attrib (synPat, attrs, range) ->
                visit
                    synPat
                    (fun nodes ->
                        [ mkNode SynPat_Attrib range
                          yield! nodes
                          yield! (visitSynAttributeLists range attrs) ]
                        |> finalContinuation)
            | SynPat.Or (synPat, synPat2, range) ->
                let continuations : ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                    [ visit synPat; visit synPat2 ]

                let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                    mkNode SynPat_Or range :: (List.collect id nodes)
                    |> finalContinuation

                Continuation.sequence continuations finalContinuation
            | SynPat.Ands (pats, range) ->
                let continuations : ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                    pats |> List.map visit

                let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                    mkNode SynPat_Ands range
                    :: (List.collect id nodes)
                    |> finalContinuation

                Continuation.sequence continuations finalContinuation
            | SynPat.LongIdent (_, _, svtd, ctorArgs, _, range) ->
                [ mkNode SynPat_LongIdent range
                  if svtd.IsSome then
                      yield! visitSynValTyparDecls svtd.Value
                  yield! visitSynConstructorArgs ctorArgs ]
                |> finalContinuation
            | SynPat.Tuple (_, pats, range) ->
                let continuations : ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                    pats |> List.map visit

                let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                    mkNode SynPat_Tuple range
                    :: (List.collect id nodes)
                    |> finalContinuation

                Continuation.sequence continuations finalContinuation
            | SynPat.Paren (pat, range) ->
                visit
                    pat
                    (fun nodes ->
                        mkNode SynPat_Paren range :: nodes
                        |> finalContinuation)
            | SynPat.ArrayOrList (_, pats, range) ->
                let continuations : ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                    pats |> List.map visit

                let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                    mkNode SynPat_ArrayOrList range
                    :: (List.collect id nodes)
                    |> finalContinuation

                Continuation.sequence continuations finalContinuation
            | SynPat.Record (pats, range) ->
                let continuations : ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                    pats |> List.map (snd >> visit)

                let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                    mkNode SynPat_Record range
                    :: (List.collect id nodes)
                    |> finalContinuation

                Continuation.sequence continuations finalContinuation
            | SynPat.Null (range) ->
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
                visit
                    pat
                    (fun nodes ->
                        mkNode SynPat_FromParseError range :: nodes
                        |> finalContinuation)

        visit sp id

    and visitSynConstructorArgs (ctorArgs: SynArgPats) : TriviaNodeAssigner list =
        match ctorArgs with
        | Pats (pats) -> List.collect visitSynPat pats
        | NamePatPairs (pats, range) ->
            mkNode NamePatPairs_ range
            :: (List.collect (snd >> visitSynPat) pats)

    and visitSynComponentInfo (sci: SynComponentInfo) : TriviaNodeAssigner list =
        match sci with
        | ComponentInfo (attribs, typeParams, _, _, _, _, _, range) ->
            [ mkNode ComponentInfo_ range
              yield! (visitSynAttributeLists range attribs)
              yield! (typeParams |> List.collect (visitSynTyparDecl)) ]

    and visitSynTypeDefnRepr (stdr: SynTypeDefnRepr) : TriviaNodeAssigner list =
        match stdr with
        | SynTypeDefnRepr.ObjectModel (kind, members, _) ->
            //            { Type = SynTypeDefnRepr_ObjectModel
//              Range = r range
//              Properties = Map.empty
//              FsAstNode = stdr }
            visitSynTypeDefnKind kind
            @ (members |> List.collect visitSynMemberDefn)
        | SynTypeDefnRepr.Simple (simpleRepr, _) ->
            //            { Type = SynTypeDefnRepr_Simple
//              Range = r range
//              Properties = Map.empty
//              FsAstNode = stdr }
            visitSynTypeDefnSimpleRepr simpleRepr
        | SynTypeDefnRepr.Exception (exceptionRepr) -> visitSynExceptionDefnRepr exceptionRepr

    and visitSynTypeDefnKind (kind: SynTypeDefnKind) : TriviaNodeAssigner list =
        match kind with
        | TyconUnspecified
        | TyconClass
        | TyconInterface
        | TyconStruct
        | TyconRecord
        | TyconAbbrev
        | TyconHiddenRepr
        | TyconAugmentation
        | TyconUnion
        | TyconILAssemblyCode -> []
        | TyconDelegate (typ, valinfo) -> visitSynType typ @ visitSynValInfo valinfo

    and visitSynTypeDefnSimpleRepr (arg: SynTypeDefnSimpleRepr) =
        match arg with
        | SynTypeDefnSimpleRepr.None (range) ->
            mkNode SynTypeDefnSimpleRepr_None range
            |> List.singleton
        | SynTypeDefnSimpleRepr.Union (_, unionCases, range) ->
            mkNode SynTypeDefnSimpleRepr_Union range
            :: (List.collect visitSynUnionCase unionCases)
        | SynTypeDefnSimpleRepr.Enum (enumCases, range) ->
            mkNode SynTypeDefnSimpleRepr_Enum range
            :: (List.collect visitSynEnumCase enumCases)
        | SynTypeDefnSimpleRepr.Record (_, recordFields, range) ->
            mkNode SynTypeDefnSimpleRepr_Record range
            :: (List.collect visitSynField recordFields)
        | SynTypeDefnSimpleRepr.General (_, _, _, _, _, _, _, range) ->
            mkNode SynTypeDefnSimpleRepr_General range
            |> List.singleton
        | SynTypeDefnSimpleRepr.LibraryOnlyILAssembly (_, range) ->
            mkNode SynTypeDefnSimpleRepr_LibraryOnlyILAssembly range
            |> List.singleton
        | SynTypeDefnSimpleRepr.TypeAbbrev (_, typ, range) ->
            mkNode SynTypeDefnSimpleRepr_TypeAbbrev range
            :: (visitSynType typ)
        | SynTypeDefnSimpleRepr.Exception (edr) -> visitSynExceptionDefnRepr edr

    and visitSynExceptionDefn (exceptionDef: SynExceptionDefn) : TriviaNodeAssigner list =
        match exceptionDef with
        | SynExceptionDefn (sedr, members, range) ->
            [ mkNode SynExceptionDefn_ range
              yield! visitSynExceptionDefnRepr sedr
              yield! (members |> List.collect visitSynMemberDefn) ]

    and visitSynExceptionDefnRepr (sedr: SynExceptionDefnRepr) : TriviaNodeAssigner list =
        match sedr with
        | SynExceptionDefnRepr (attrs, unionCase, _, _, _, range) ->
            [ mkNode SynExceptionDefnRepr_ range
              yield! (visitSynAttributeLists range attrs)
              yield! visitSynUnionCase unionCase ]

    and visitSynAttribute (attr: SynAttribute) : TriviaNodeAssigner list =
        mkNode SynAttribute_ attr.Range
        :: (visitSynExpr attr.ArgExpr)

    and visitSynAttributeLists (parentRange: Range) (attrs: SynAttributeList list) : TriviaNodeAssigner list =
        match attrs with
        | [ h ] -> visitSynAttributeList parentRange h
        | _ :: tail ->
            let aRanges =
                tail
                |> List.map (fun a -> a.Range)
                |> fun r -> r @ [ parentRange ]

            List.zip attrs aRanges
            |> List.collect (fun (a, r) -> visitSynAttributeList r a)
        | [] -> []

    and visitSynAttributeList (parentRange: Range) (attrs: SynAttributeList) : TriviaNodeAssigner list =
        TriviaNodeAssigner(MainNode(SynAttributeList_), attrs.Range, parentRange.StartLine - attrs.Range.EndLine - 1)
        :: (List.collect visitSynAttribute attrs.Attributes)

    and visitSynUnionCase (uc: SynUnionCase) : TriviaNodeAssigner list =
        match uc with
        | UnionCase (attrs, _, uct, _, _, range) ->
            [ mkNode UnionCase_ range
              yield! visitSynUnionCaseType uct
              yield! (visitSynAttributeLists range attrs) ]

    and visitSynUnionCaseType (uct: SynUnionCaseType) =
        match uct with
        | UnionCaseFields (cases) -> List.collect visitSynField cases
        | UnionCaseFullType (stype, valInfo) -> visitSynType stype @ visitSynValInfo valInfo

    and visitSynEnumCase (sec: SynEnumCase) : TriviaNodeAssigner list =
        match sec with
        | EnumCase (attrs, ident, _, _, range) ->
            [ yield mkNode EnumCase_ range
              yield! (visitSynAttributeLists range attrs)
              yield visitIdent ident ]

    and visitSynField (sfield: SynField) : TriviaNodeAssigner list =
        match sfield with
        | Field (attrs, _, ident, typ, _, _, _, range) ->
            let parentRange =
                Option.map (fun (i: Ident) -> i.idRange) ident
                |> Option.defaultValue range

            [ mkNode Field_ range
              yield! (visitSynAttributeLists parentRange attrs)
              yield! visitSynType typ ]

    and visitSynType (st: SynType) =
        let rec visit
            (st: SynType)
            (finalContinuation: TriviaNodeAssigner list -> TriviaNodeAssigner list)
            : TriviaNodeAssigner list =
            match st with
            | SynType.LongIdent (li) -> visitLongIdentWithDots li |> finalContinuation
            | SynType.App (typeName, _, typeArgs, _, _, _, range) ->
                let continuations : ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                    [ yield! (List.map visit typeArgs)
                      visit typeName ]

                let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                    mkNode SynType_App range
                    :: (List.collect id nodes)
                    |> finalContinuation

                Continuation.sequence continuations finalContinuation
            | SynType.LongIdentApp (typeName, _, _, typeArgs, _, _, range) ->
                let continuations : ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                    [ yield! (List.map visit typeArgs)
                      visit typeName ]

                let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                    mkNode SynType_LongIdentApp range
                    :: (List.collect id nodes)
                    |> finalContinuation

                Continuation.sequence continuations finalContinuation
            | SynType.Tuple (_, typeNames, range) ->
                let continuations : ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                    List.map (snd >> visit) typeNames

                let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                    mkNode SynType_Tuple range
                    :: (List.collect id nodes)
                    |> finalContinuation

                Continuation.sequence continuations finalContinuation
            | SynType.Array (_, elementType, range) ->
                visit
                    elementType
                    (fun nodes ->
                        mkNode SynType_Array range :: nodes
                        |> finalContinuation)
            | SynType.Fun (argType, returnType, range) ->
                let continuations : ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                    [ visit argType; visit returnType ]

                let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                    mkNode SynType_Fun range
                    :: (List.collect id nodes)
                    |> finalContinuation

                Continuation.sequence continuations finalContinuation
            | SynType.Var (genericName, range) ->
                mkNode SynType_Var range
                :: (visitSynTypar genericName)
                |> finalContinuation
            | SynType.Anon (range) ->
                mkNode SynType_Anon range
                |> List.singleton
                |> finalContinuation
            | SynType.WithGlobalConstraints (typeName, _, range) ->
                visit
                    typeName
                    (fun nodes ->
                        mkNode SynType_WithGlobalConstraints range
                        :: nodes
                        |> finalContinuation)
            | SynType.HashConstraint (synType, range) ->
                visit
                    synType
                    (fun nodes ->
                        mkNode SynType_HashConstraint range :: nodes
                        |> finalContinuation)
            | SynType.MeasureDivide (dividendType, divisorType, range) ->
                let continuations : ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                    [ visit dividendType
                      visit divisorType ]

                let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                    mkNode SynType_MeasureDivide range
                    :: (List.collect id nodes)
                    |> finalContinuation

                Continuation.sequence continuations finalContinuation
            | SynType.MeasurePower (measureType, _, range) ->
                visit
                    measureType
                    (fun nodes ->
                        mkNode SynType_MeasurePower range :: nodes
                        |> finalContinuation)
            | SynType.StaticConstant (constant, range) ->
                [ mkNode SynType_StaticConstant range
                  visitSynConst range constant ]
                |> finalContinuation
            | SynType.StaticConstantExpr (expr, range) ->
                mkNode SynType_StaticConstantExpr range
                :: (visitSynExpr expr)
                |> finalContinuation
            | SynType.StaticConstantNamed (expr, typ, range) ->
                let continuations : ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
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
            | SynType.Paren (innerType, range) ->
                visit
                    innerType
                    (fun nodes ->
                        mkNode SynType_Paren range :: nodes
                        |> finalContinuation)

        visit st id

    and visitSynConst (parentRange: Range) (sc: SynConst) : TriviaNodeAssigner =
        let t =
            match sc with
            | SynConst.Bool _ -> SynConst_Bool
            | SynConst.Unit _ -> SynConst_Unit
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

        mkNode t (sc.Range parentRange)

    and visitSynValInfo (svi: SynValInfo) =
        match svi with
        | SynValInfo (args, arg) ->
            (List.collect (List.collect visitSynArgInfo) args)
            @ visitSynArgInfo arg

    and visitSynArgInfo (sai: SynArgInfo) : TriviaNodeAssigner list =
        match sai with
        | SynArgInfo (attrs, _, ident) ->
            let parentRange =
                ident
                |> Option.map (fun i -> i.idRange)
                |> Option.defaultValue range.Zero

            visitSynAttributeLists parentRange attrs

    and visitSynAccess (a: SynAccess) =
        match a with
        | SynAccess.Private -> SynAccess_Private
        | SynAccess.Internal -> SynAccess_Internal
        | SynAccess.Public -> SynAccess_Public

    and visitSynBindingKind (kind: SynBindingKind) =
        match kind with
        | SynBindingKind.DoBinding -> "Do Binding"
        | SynBindingKind.StandaloneExpression -> "Standalone Expression"
        | SynBindingKind.NormalBinding -> "Normal Binding"

    and visitMemberKind (mk: MemberKind) =
        match mk with
        | MemberKind.ClassConstructor -> "ClassConstructor"
        | MemberKind.Constructor -> "Constructor"
        | MemberKind.Member -> "Member"
        | MemberKind.PropertyGet -> "PropertyGet"
        | MemberKind.PropertySet -> "PropertySet"
        | MemberKind.PropertyGetSet -> "PropertyGetSet"

    and visitParsedHashDirective (hash: ParsedHashDirective) : TriviaNodeAssigner =
        match hash with
        | ParsedHashDirective (_, _, range) -> mkNode ParsedHashDirective_ range

    and visitSynModuleOrNamespaceSig (modOrNs: SynModuleOrNamespaceSig) : TriviaNodeAssigner list =
        match modOrNs with
        | SynModuleOrNamespaceSig (longIdent, _, synModuleOrNamespaceKind, decls, _, attrs, _, range) ->
            let typeName =
                match synModuleOrNamespaceKind with
                | SynModuleOrNamespaceKind.AnonModule -> SynModuleOrNamespaceSig_AnonModule
                | SynModuleOrNamespaceKind.NamedModule -> SynModuleOrNamespaceSig_NamedModule
                | SynModuleOrNamespaceKind.DeclaredNamespace -> SynModuleOrNamespaceSig_DeclaredNamespace
                | SynModuleOrNamespaceKind.GlobalNamespace -> SynModuleOrNamespaceSig_GlobalNamespace

            [ // LongIdent inside Namespace is being processed as children.
              if typeName
                 <> SynModuleOrNamespaceSig_DeclaredNamespace then
                  mkNode typeName range
              yield!
                  if synModuleOrNamespaceKind = SynModuleOrNamespaceKind.DeclaredNamespace then
                      visitLongIdent longIdent
                  else
                      []
              yield! (visitSynAttributeLists range attrs)
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
                let continuations : ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                    List.map visit decls

                let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                    [ mkNode SynModuleSigDecl_NestedModule range
                      yield! visitSynComponentInfo sci
                      yield! (List.collect id nodes) ]
                    |> finalContinuation

                Continuation.sequence continuations finalContinuation
            | SynModuleSigDecl.Val (SynValSig.ValSpfn _ as node, _) -> visitSynValSig node |> finalContinuation
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
            | SynModuleSigDecl.NamespaceFragment (moduleOrNamespace) ->
                visitSynModuleOrNamespaceSig moduleOrNamespace
                |> finalContinuation
            | SynModuleSigDecl.Exception (synExceptionSig, range) ->
                mkNode SynModuleSigDecl_Exception range
                :: (visitSynExceptionSig synExceptionSig)
                |> finalContinuation

        visit ast id

    and visitSynExceptionSig (exceptionDef: SynExceptionSig) : TriviaNodeAssigner list =
        match exceptionDef with
        | SynExceptionSig (sedr, members, range) ->
            [ mkNode SynExceptionSig_ range
              yield! visitSynExceptionDefnRepr sedr
              yield! (members |> List.collect visitSynMemberSig) ]

    and visitLongIdentWithDots (lid: LongIdentWithDots) : TriviaNodeAssigner list =
        match lid with
        | LongIdentWithDots (ids, _) -> List.map visitIdent ids

    and visitLongIdent (li: LongIdent) : TriviaNodeAssigner list = List.map visitIdent li

    and visitIdent (ident: Ident) : TriviaNodeAssigner = mkNode Ident_ ident.idRange

let astToNode (hds: ParsedHashDirective list) (mdls: SynModuleOrNamespace list) : TriviaNodeAssigner list =
    let children =
        [ yield! List.collect Ast.visitSynModuleOrNamespace mdls
          yield! List.map Ast.visitParsedHashDirective hds ]

    children

let sigAstToNode (ast: SynModuleOrNamespaceSig list) : TriviaNodeAssigner list =
    let children =
        List.collect Ast.visitSynModuleOrNamespaceSig ast

    children
