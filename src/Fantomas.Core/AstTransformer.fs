module internal Fantomas.Core.AstTransformer

open FSharp.Compiler.Text
open FSharp.Compiler.Syntax
open Fantomas.Core.AstExtensions
open Fantomas.Core.TriviaTypes
open Fantomas.Core.RangePatterns
open Fantomas.Core

let sortChildren =
    Array.sortBy (fun ({ Range = range }: TriviaNode) -> range.StartLine, range.StartColumn)

let mkNode (t: FsAstType) (r: range) =
    { Range = r
      Type = t
      Children = Array.empty
      FSharpASTNode = None }

let mkNodeWithChildren (t: FsAstType) (r: range) (children: TriviaNode array) =
    { Range = r
      Type = t
      Children = children
      FSharpASTNode = None }

let mkSynModuleDeclNode (t: FsAstType) (astNode: SynModuleDecl) (r: range) (children: TriviaNode array) =
    { Range = r
      Type = t
      Children = children
      FSharpASTNode = Some(FSharpASTNode.ModuleDecl astNode) }

let mkSynModuleSigDeclNode (t: FsAstType) (astNode: SynModuleSigDecl) (r: range) (children: TriviaNode array) =
    { Range = r
      Type = t
      Children = children
      FSharpASTNode = Some(FSharpASTNode.ModuleSigDecl astNode) }

let mkSynExprNode (t: FsAstType) (astNode: SynExpr) (r: range) (children: TriviaNode array) =
    { Range = r
      Type = t
      Children = children
      FSharpASTNode = Some(FSharpASTNode.Expr astNode) }

let mkSynValSig (astNode: SynValSig) (r: range) (children: TriviaNode array) =
    { Range = r
      Type = SynValSig_
      Children = children
      FSharpASTNode = Some(FSharpASTNode.ValSig astNode) }

let mkNodeOption (t: FsAstType) (r: range option) : TriviaNode option = Option.map (mkNode t) r

// We only need the let/type keyword anchor if there are xml docs or attributes present that have space between itself and the let/type keyword
let mkNodeForRangeAfterXmlAndAttributes
    (t: FsAstType)
    (SourceParser.PreXmlDoc (xml, xmlRange))
    (attrs: SynAttributeList list)
    (r: range option)
    : TriviaNode list =
    if Array.isEmpty xml && attrs.IsEmpty then
        []
    else
        match r with
        | None -> []
        | Some keyword ->
            let lastLine =
                let lastAttrLine =
                    match List.tryLast attrs with
                    | None -> 0
                    | Some a -> a.Range.EndLine

                System.Math.Max(xmlRange.EndLine, lastAttrLine)

            if keyword.StartLine > lastLine + 1 then
                // In this scenario there is trivia underneath the attributes or xmldoc
                // f.ex.:
                // [<AttributeOne()>]
                // // some comment
                // let a = 0
                [ mkNode t keyword ]
            else
                []

let rec visitSynModuleOrNamespace
    (SourceParser.ModuleOrNamespace (attrs, _, moduleKeyword, namespaceKeyword, _, longIdent, decls, _, kind, fullRange))
    =
    let astType, moduleOrNamespace =
        match kind with
        | SynModuleOrNamespaceKind.DeclaredNamespace ->
            SynModuleOrNamespace_DeclaredNamespace, mkNodeOption SynModuleOrNamespace_Namespace namespaceKeyword
        | SynModuleOrNamespaceKind.GlobalNamespace ->
            SynModuleOrNamespace_GlobalNamespace, mkNodeOption SynModuleOrNamespace_Namespace namespaceKeyword
        | SynModuleOrNamespaceKind.NamedModule ->
            SynModuleOrNamespace_NamedModule, mkNodeOption SynModuleOrNamespace_Module moduleKeyword
        | SynModuleOrNamespaceKind.AnonModule -> SynModuleOrNamespace_AnonModule, None

    let longIdentNodes =
        match kind, decls with
        | SynModuleOrNamespaceKind.AnonModule, _ :: _ -> None
        | _ -> Some(visitLongIdentIncludingFullRange longIdent)

    { Range = fullRange
      Type = astType
      FSharpASTNode = None
      Children =
        sortChildren
            [| yield! Option.toList moduleOrNamespace
               yield! Option.toList longIdentNodes
               yield! visitSynAttributeLists attrs
               yield! (List.map visitSynModuleDecl decls) |] }

and visitSynModuleDecl (ast: SynModuleDecl) : TriviaNode =
    let rec visit (ast: SynModuleDecl) (finalContinuation: TriviaNode -> TriviaNode) : TriviaNode =
        match ast with
        | SynModuleDecl.ModuleAbbrev (_, _, range) ->
            mkSynModuleDeclNode SynModuleDecl_ModuleAbbrev ast range Array.empty
            |> finalContinuation
        | SynModuleDecl.NestedModule (SynComponentInfo (xmlDoc = preXmlDoc; attributes = attrs) as sci,
                                      _,
                                      decls,
                                      _,
                                      range,
                                      trivia) ->
            let continuations: ((TriviaNode -> TriviaNode) -> TriviaNode) list =
                List.map visit decls

            let moduleNode =
                mkNodeForRangeAfterXmlAndAttributes
                    SynModuleDecl_NestedModule_Module
                    preXmlDoc
                    attrs
                    trivia.ModuleKeyword

            let finalContinuation (nodes: TriviaNode list) : TriviaNode =
                mkSynModuleDeclNode
                    SynModuleDecl_NestedModule
                    ast
                    range
                    (sortChildren
                        [| yield! moduleNode
                           yield! visitSynComponentInfo sci
                           yield! Option.toList (mkNodeOption SynModuleDecl_NestedModule_Equals trivia.EqualsRange)
                           yield! nodes |])
                |> finalContinuation

            Continuation.sequence continuations finalContinuation
        | SynModuleDecl.Let (_, bindings, range) ->
            mkSynModuleDeclNode
                SynModuleDecl_Let
                ast
                range
                (sortChildren [| yield! List.map visitSynBinding bindings |])
            |> finalContinuation
        | SynModuleDecl.Expr (expr, range) ->
            mkSynModuleDeclNode SynModuleDecl_Expr ast range (sortChildren [| yield! visitSynExpr expr |])
            |> finalContinuation
        | SynModuleDecl.Types (typeDefs, range) ->
            mkSynModuleDeclNode
                SynModuleDecl_Types
                ast
                range
                (sortChildren [| yield! List.map visitSynTypeDefn typeDefs |])
            |> finalContinuation
        | SynModuleDecl.Exception (exceptionDef, range) ->
            mkSynModuleDeclNode SynModuleDecl_Exception ast range [| visitSynExceptionDefn exceptionDef |]
            |> finalContinuation
        | SynModuleDecl.Open (target, parentRange) ->
            // we use the parent ranges here to match up with the trivia parsed
            match target with
            | SynOpenDeclTarget.ModuleOrNamespace (longId, _range) ->
                mkSynModuleDeclNode SynModuleDecl_Open ast parentRange (sortChildren [| visitSynLongIdent longId |])
                |> finalContinuation
            | SynOpenDeclTarget.Type (synType, _range) ->
                mkSynModuleDeclNode SynModuleDecl_OpenType ast parentRange (sortChildren [| visitSynType synType |])
                |> finalContinuation
        | SynModuleDecl.Attributes (attrs, range) ->
            mkSynModuleDeclNode
                SynModuleDecl_Attributes
                ast
                range
                (sortChildren [| yield! visitSynAttributeLists attrs |])
            |> finalContinuation
        | SynModuleDecl.HashDirective (hash, range) ->
            mkSynModuleDeclNode SynModuleDecl_HashDirective ast range [| visitParsedHashDirective hash |]
            |> finalContinuation
        | SynModuleDecl.NamespaceFragment moduleOrNamespace ->
            visitSynModuleOrNamespace moduleOrNamespace |> finalContinuation

    visit ast id

and visitSynExpr (synExpr: SynExpr) : TriviaNode list =
    let processSequence
        (finalContinuation: TriviaNode list -> TriviaNode list)
        (continuations: ((TriviaNode list -> TriviaNode list) -> TriviaNode list) list)
        (mkNode: TriviaNode list -> TriviaNode)
        : TriviaNode list =
        let finalContinuation (nodes: TriviaNode list list) : TriviaNode list =
            mkNode (List.collect id nodes) |> List.singleton |> finalContinuation

        Continuation.sequence continuations finalContinuation

    let rec visit (synExpr: SynExpr) (finalContinuation: TriviaNode list -> TriviaNode list) : TriviaNode list =
        match synExpr with
        | SourceParser.Sequentials xs ->
            let continuations: ((TriviaNode list -> TriviaNode list) -> TriviaNode list) list =
                List.map visit xs

            let finalContinuation (nodes: TriviaNode list list) : TriviaNode list =
                List.collect id nodes |> finalContinuation

            Continuation.sequence continuations finalContinuation
        | SynExpr.Paren (expr, lpr, rpr, range) ->
            visit expr (fun nodes ->
                mkSynExprNode
                    SynExpr_Paren
                    synExpr
                    range
                    [| yield mkNode SynExpr_Paren_OpeningParenthesis lpr
                       yield! nodes
                       yield! Option.toList (mkNodeOption SynExpr_Paren_ClosingParenthesis rpr) |]
                |> List.singleton
                |> finalContinuation)
        | SynExpr.Quote (_, _, quotedSynExpr, _, range) ->
            visit quotedSynExpr (fun nodes ->
                mkSynExprNode SynExpr_Quote synExpr range (sortChildren [| yield! nodes |])
                |> List.singleton
                |> finalContinuation)
        | SynExpr.Const (constant, range) ->
            mkSynExprNode SynExpr_Const synExpr range [| visitSynConst range constant |]
            |> List.singleton
            |> finalContinuation
        | SynExpr.Typed (expr, typeName, range) ->
            visit expr (fun nodes ->
                mkSynExprNode SynExpr_Typed synExpr range (sortChildren [| yield! nodes; yield visitSynType typeName |])
                |> List.singleton
                |> finalContinuation)
        | SynExpr.Tuple (_, exprs, _, range) ->
            let continuations: ((TriviaNode list -> TriviaNode list) -> TriviaNode list) list =
                List.map visit exprs

            processSequence finalContinuation continuations (fun nodes ->
                mkSynExprNode SynExpr_Tuple synExpr range (sortChildren [| yield! nodes |]))
        | SourceParser.ArrayOrList (startRange, _isArray, exprs, endRange, range) ->
            let continuations: ((TriviaNode list -> TriviaNode list) -> TriviaNode list) list =
                exprs |> List.map visit

            processSequence finalContinuation continuations (fun nodes ->
                mkSynExprNode
                    SynExpr_ArrayOrList
                    synExpr
                    range
                    (sortChildren
                        [| yield mkNode SynExpr_ArrayOrList_OpeningDelimiter startRange
                           yield! nodes
                           yield mkNode SynExpr_ArrayOrList_ClosingDelimiter endRange |]))
        // Captured above
        | SynExpr.ArrayOrList _
        | SynExpr.ArrayOrListComputed _ -> [ mkNode SynExpr_ArrayOrList synExpr.Range ]
        | SynExpr.Record (_, _, recordFields, StartEndRange 1 (openingBrace, range, closingBrace)) ->
            mkSynExprNode
                SynExpr_Record
                synExpr
                range
                (sortChildren
                    [| yield mkNode SynExpr_Record_OpeningBrace openingBrace
                       yield! List.map visitRecordField recordFields
                       yield mkNode SynExpr_Record_ClosingBrace closingBrace |])
            |> List.singleton
            |> finalContinuation
        | SynExpr.AnonRecd (_, _, recordFields, range) ->
            mkSynExprNode
                SynExpr_AnonRecd
                synExpr
                range
                (sortChildren [| yield! List.map visitAnonRecordField recordFields |])
            |> List.singleton
            |> finalContinuation
        | SynExpr.New (_, typeName, expr, range) ->
            visit expr (fun nodes ->
                mkSynExprNode SynExpr_New synExpr range (sortChildren [| yield! nodes; yield visitSynType typeName |])
                |> List.singleton
                |> finalContinuation)
        | SynExpr.ObjExpr (objType, argOptions, withRange, bindings, members, extraImpls, _, range) ->
            mkSynExprNode
                SynExpr_ObjExpr
                synExpr
                range
                (sortChildren
                    [| yield visitSynType objType
                       yield! visitOptSynExpr (Option.map fst argOptions)
                       yield! Option.toList (mkNodeOption SynExpr_ObjExpr_With withRange)
                       yield! List.map visitSynBinding bindings
                       yield! visitSynMemberDefns members
                       yield! List.map visitSynInterfaceImpl extraImpls |])
            |> List.singleton
            |> finalContinuation
        | SynExpr.While (_, whileExpr, doExpr, range) ->
            let continuations: ((TriviaNode list -> TriviaNode list) -> TriviaNode list) list =
                [ visit whileExpr; visit doExpr ]

            processSequence finalContinuation continuations (fun nodes ->
                mkSynExprNode SynExpr_While synExpr range (sortChildren [| yield! nodes |]))
        | SynExpr.For (_, _, _, equalsRange, identBody, _, toBody, doBody, range) ->
            let continuations: ((TriviaNode list -> TriviaNode list) -> TriviaNode list) list =
                [ visit identBody; visit toBody; visit doBody ]

            processSequence finalContinuation continuations (fun nodes ->
                mkSynExprNode
                    SynExpr_For
                    synExpr
                    range
                    (sortChildren
                        [| yield! Option.toList (mkNodeOption SynExpr_For_Equals equalsRange)
                           yield! nodes |]))
        | SynExpr.ForEach (_, _, SeqExprOnly _, _, pat, enumExpr, bodyExpr, range) ->
            let continuations: ((TriviaNode list -> TriviaNode list) -> TriviaNode list) list =
                [ visit enumExpr; visit bodyExpr ]

            processSequence finalContinuation continuations (fun nodes ->
                mkSynExprNode SynExpr_ForEach synExpr range (sortChildren [| yield visitSynPat pat; yield! nodes |]))
        | SynExpr.ComputationExpr (_, expr, StartEndRange 1 (openingBrace, _, closingBrace)) ->
            visit expr (fun nodes ->
                mkSynExprNode
                    SynExpr_ComputationExpr
                    synExpr
                    synExpr.Range
                    (sortChildren
                        [| yield mkNode SynExpr_ComputationExpr_OpeningBrace openingBrace
                           yield! nodes
                           yield mkNode SynExpr_ComputationExpr_ClosingBrace closingBrace |])
                |> List.singleton
                |> finalContinuation)
        | SourceParser.Lambda (pats, arrowRange, expr, range) ->
            visit expr (fun nodes ->
                mkSynExprNode
                    SynExpr_Lambda
                    synExpr
                    range
                    (sortChildren
                        [| yield! List.map visitSynPat pats
                           yield! Option.toList (mkNodeOption SynExpr_Lambda_Arrow arrowRange)
                           yield! nodes |])
                |> List.singleton
                |> finalContinuation)
        | SynExpr.Lambda _ -> failwith "should be tackled above"
        | SynExpr.MatchLambda (_, keywordRange, matchClauses, _, range) ->
            mkSynExprNode
                SynExpr_MatchLambda
                synExpr
                range
                (sortChildren
                    [| yield mkNode SynExpr_MatchLambda_Function keywordRange
                       yield! List.map visitSynMatchClause matchClauses |])
            |> List.singleton
            |> finalContinuation
        | SynExpr.Match (_, expr, clauses, range, trivia) ->
            visit expr (fun nodes ->
                mkSynExprNode
                    SynExpr_Match
                    synExpr
                    range
                    (sortChildren
                        [| yield mkNode SynExpr_Match_Match trivia.MatchKeyword
                           yield! nodes
                           yield mkNode SynExpr_Match_With trivia.WithKeyword
                           yield! List.map visitSynMatchClause clauses |])
                |> List.singleton
                |> finalContinuation)
        | SynExpr.Do (expr, StartRange 2 (doKeyword, range)) ->
            visit expr (fun nodes ->
                mkSynExprNode
                    SynExpr_Do
                    synExpr
                    range
                    (sortChildren [| yield mkNode SynExpr_Do_Do doKeyword; yield! nodes |])
                |> List.singleton
                |> finalContinuation)
        | SynExpr.Assert (expr, StartRange 6 (assertKeyword, range)) ->
            visit expr (fun nodes ->
                mkSynExprNode
                    SynExpr_Assert
                    synExpr
                    range
                    (sortChildren [| yield mkNode SynExpr_Assert_Assert assertKeyword; yield! nodes |])
                |> List.singleton
                |> finalContinuation)
        | SourceParser.NewlineInfixApps (e, es)
        | SourceParser.SameInfixApps (e, es) ->
            let continuations: ((TriviaNode list -> TriviaNode list) -> TriviaNode list) list =
                [ yield visit e; yield! List.map (fun (_, _, e) -> visit e) es ]

            processSequence finalContinuation continuations (fun nodes ->
                let operators = List.map (fun (_, operator, _) -> visitSynLongIdent operator) es
                mkSynExprNode SynExpr_App synExpr synExpr.Range (sortChildren [| yield! nodes; yield! operators |]))
        | SourceParser.InfixApp (_, sli, e1, e2, range) ->
            let continuations: ((TriviaNode list -> TriviaNode list) -> TriviaNode list) list =
                [ visit e1; visit e2 ]

            processSequence finalContinuation continuations (fun nodes ->
                mkSynExprNode SynExpr_App synExpr range (sortChildren [| yield visitSynLongIdent sli; yield! nodes |]))
        | SynExpr.App (_, _, funcExpr, argExpr, range) ->
            let continuations: ((TriviaNode list -> TriviaNode list) -> TriviaNode list) list =
                [ visit funcExpr; visit argExpr ]

            processSequence finalContinuation continuations (fun nodes ->
                mkSynExprNode SynExpr_App synExpr range (sortChildren [| yield! nodes |]))
        | SynExpr.TypeApp (expr, lessRange, typeNames, _, greaterRange, _, range) ->
            visit expr (fun nodes ->
                mkSynExprNode
                    SynExpr_TypeApp
                    synExpr
                    range
                    (sortChildren
                        [| yield mkNode SynExpr_TypeApp_Less lessRange
                           yield! nodes
                           yield! (List.map visitSynType typeNames)
                           yield! Option.toList (mkNodeOption SynExpr_TypeApp_Greater greaterRange) |])
                |> List.singleton
                |> finalContinuation)
        | SynExpr.LetOrUse (_, _, bindings, body, _, trivia) ->
            visit body (fun nodes ->
                [ yield! (List.map visitSynBinding bindings)
                  yield! Option.toList (mkNodeOption SynExpr_LetOrUse_In trivia.InKeyword)
                  yield! nodes ]
                |> finalContinuation)
        | SynExpr.TryWith (tryExpr,
                           withCases,
                           range,
                           _,
                           _,
                           { TryKeyword = tryKeyword
                             WithKeyword = withKeyword }) ->
            visit tryExpr (fun nodes ->
                mkSynExprNode
                    SynExpr_TryWith
                    synExpr
                    range
                    (sortChildren
                        [| yield mkNode SynExpr_TryWith_Try tryKeyword
                           yield! nodes
                           yield mkNode SynExpr_TryWith_With withKeyword
                           yield! List.map visitSynMatchClause withCases |])
                |> List.singleton
                |> finalContinuation)
        | SynExpr.TryFinally (tryExpr, finallyExpr, range, _, _, trivia) ->
            let continuations: ((TriviaNode list -> TriviaNode list) -> TriviaNode list) list =
                [ visit tryExpr; visit finallyExpr ]

            processSequence finalContinuation continuations (fun nodes ->
                mkSynExprNode
                    SynExpr_TryFinally
                    synExpr
                    range
                    (sortChildren
                        [| yield mkNode SynExpr_TryFinally_Try trivia.TryKeyword
                           yield mkNode SynExpr_TryFinally_Finally trivia.FinallyKeyword
                           yield! nodes |]))
        | SynExpr.Lazy (ex, StartRange 4 (lazyKeyword, range)) ->
            visit ex (fun nodes ->
                mkSynExprNode
                    SynExpr_Lazy
                    synExpr
                    range
                    (sortChildren [| yield mkNode SynExpr_Lazy_Lazy lazyKeyword; yield! nodes |])
                |> List.singleton
                |> finalContinuation)
        | SynExpr.Sequential _ -> failwith "SynExpr.Sequential should have been captured before"
        | SynExpr.SequentialOrImplicitYield (_, expr1, expr2, ifNotStmt, range) ->
            let continuations: ((TriviaNode list -> TriviaNode list) -> TriviaNode list) list =
                [ visit expr1; visit expr2; visit ifNotStmt ]

            processSequence finalContinuation continuations (fun nodes ->
                mkSynExprNode SynExpr_SequentialOrImplicitYield synExpr range (sortChildren [| yield! nodes |]))
        | SourceParser.ElIf ((_leadingElseKw, ifKw, isElif, ifExpr, thenKw, thenExpr) :: es, elseInfo, range) ->
            let elifs = es |> List.collect (fun (_, _, _, e1, _, e2) -> [ visit e1; visit e2 ])

            let elseExprNodes, elseKeywordNodes =
                match elseInfo with
                | None -> [], []
                | Some (elseKw, elseExpr) -> [ visit elseExpr ], [ mkNode SynExpr_IfThenElse_Else elseKw ]

            let continuations: ((TriviaNode list -> TriviaNode list) -> TriviaNode list) list =
                [ yield visit ifExpr; yield visit thenExpr; yield! elifs; yield! elseExprNodes ]

            processSequence finalContinuation continuations (fun nodes ->
                let elifKeywords =
                    es
                    |> List.collect (fun (elseKw, ifKw, isElif, _, thenKw, _) ->
                        [ yield! (mkNodeOption SynExpr_IfThenElse_Else elseKw |> Option.toList)
                          yield
                              mkNode
                                  (if isElif then
                                       SynExpr_IfThenElse_Elif
                                   else
                                       SynExpr_IfThenElse_If)
                                  ifKw
                          yield mkNode SynExpr_IfThenElse_Then thenKw ])

                mkSynExprNode
                    SynExpr_IfThenElse
                    synExpr
                    range
                    (sortChildren
                        [| yield
                               mkNode
                                   (if isElif then
                                        SynExpr_IfThenElse_Elif
                                    else
                                        SynExpr_IfThenElse_If)
                                   ifKw
                           yield mkNode SynExpr_IfThenElse_Then thenKw
                           yield! elifKeywords
                           yield! elseKeywordNodes
                           yield! nodes |]))

        | SynExpr.IfThenElse (ifExpr, thenExpr, elseExpr, _, _, range, trivia) ->
            let continuations: ((TriviaNode list -> TriviaNode list) -> TriviaNode list) list =
                [ yield visit ifExpr
                  yield visit thenExpr
                  yield! (Option.toList elseExpr |> List.map visit) ]

            processSequence finalContinuation continuations (fun nodes ->
                mkSynExprNode
                    SynExpr_IfThenElse
                    synExpr
                    range
                    (sortChildren
                        [| yield
                               mkNode
                                   (if trivia.IsElif then
                                        SynExpr_IfThenElse_Elif
                                    else
                                        SynExpr_IfThenElse_If)
                                   trivia.IfKeyword
                           yield mkNode SynExpr_IfThenElse_Then trivia.ThenKeyword
                           yield! Option.toList (mkNodeOption SynExpr_IfThenElse_Else trivia.ElseKeyword)
                           yield! nodes |]))
        | SynExpr.Ident id ->
            mkSynExprNode SynExpr_Ident synExpr id.idRange [| visitIdent id |]
            |> List.singleton
            |> finalContinuation
        | SynExpr.LongIdent (_, longDotId, _, range) ->
            mkSynExprNode SynExpr_LongIdent synExpr range (sortChildren [| visitSynLongIdent longDotId |])
            |> List.singleton
            |> finalContinuation
        | SynExpr.LongIdentSet (_, expr, range) ->
            visit expr (fun nodes ->
                mkSynExprNode SynExpr_LongIdentSet synExpr range (sortChildren [| yield! nodes |])
                |> List.singleton
                |> finalContinuation)
        | SynExpr.DotGet (expr, _, longDotId, range) ->
            visit expr (fun nodes ->
                mkSynExprNode
                    SynExpr_DotGet
                    synExpr
                    range
                    (sortChildren
                        [| yield! nodes
                           // Idents are collected as children here to deal with unit test ``Fluent api with comments should remain on same lines``
                           yield visitSynLongIdent longDotId |])

                |> List.singleton
                |> finalContinuation)
        | SynExpr.DotSet (expr, _, e2, range) ->
            let continuations: ((TriviaNode list -> TriviaNode list) -> TriviaNode list) list =
                [ visit expr; visit e2 ]

            processSequence finalContinuation continuations (fun nodes ->
                mkSynExprNode SynExpr_DotSet synExpr range (sortChildren [| yield! nodes |]))
        | SynExpr.Set (e1, e2, range) ->
            let continuations: ((TriviaNode list -> TriviaNode list) -> TriviaNode list) list =
                [ visit e1; visit e2 ]

            processSequence finalContinuation continuations (fun nodes ->
                mkSynExprNode SynExpr_Set synExpr range (sortChildren [| yield! nodes |]))
        | SynExpr.DotIndexedGet (objectExpr, indexArgs, _, range) ->
            let continuations: ((TriviaNode list -> TriviaNode list) -> TriviaNode list) list =
                [ visit objectExpr; visit indexArgs ]

            processSequence finalContinuation continuations (fun nodes ->
                mkSynExprNode SynExpr_DotIndexedGet synExpr range (sortChildren [| yield! nodes |]))
        | SynExpr.DotIndexedSet (objectExpr, indexArgs, valueExpr, _, _, range) ->
            let continuations: ((TriviaNode list -> TriviaNode list) -> TriviaNode list) list =
                [ visit objectExpr; visit indexArgs; visit valueExpr ]

            processSequence finalContinuation continuations (fun nodes ->
                mkSynExprNode SynExpr_DotIndexedSet synExpr range (sortChildren [| yield! nodes |]))
        | SynExpr.NamedIndexedPropertySet (_, e1, e2, range) ->
            let continuations: ((TriviaNode list -> TriviaNode list) -> TriviaNode list) list =
                [ visit e1; visit e2 ]

            processSequence finalContinuation continuations (fun nodes ->
                mkSynExprNode SynExpr_NamedIndexedPropertySet synExpr range (sortChildren [| yield! nodes |]))
        | SynExpr.DotNamedIndexedPropertySet (expr, _, e1, e2, range) ->
            let continuations: ((TriviaNode list -> TriviaNode list) -> TriviaNode list) list =
                [ visit expr; visit e1; visit e2 ]

            processSequence finalContinuation continuations (fun nodes ->
                mkSynExprNode SynExpr_DotNamedIndexedPropertySet synExpr range (sortChildren [| yield! nodes |]))
        | SynExpr.TypeTest (expr, typeName, range) ->
            visit expr (fun nodes ->
                mkSynExprNode SynExpr_TypeTest synExpr range (sortChildren [| yield! nodes; visitSynType typeName |])
                |> List.singleton
                |> finalContinuation)
        | SynExpr.Upcast (expr, typeName, range) ->
            visit expr (fun nodes ->
                mkSynExprNode SynExpr_Upcast synExpr range (sortChildren [| yield! nodes; visitSynType typeName |])
                |> List.singleton
                |> finalContinuation)
        | SynExpr.Downcast (expr, typeName, range) ->
            visit expr (fun nodes ->
                mkSynExprNode SynExpr_Downcast synExpr range (sortChildren [| yield! nodes; visitSynType typeName |])
                |> List.singleton
                |> finalContinuation)
        | SynExpr.InferredUpcast (expr, StartRange 6 (upcastKeyword, range)) ->
            visit expr (fun nodes ->
                mkSynExprNode
                    SynExpr_InferredUpcast
                    synExpr
                    range
                    (sortChildren [| yield mkNode SynExpr_InferredUpcast_Upcast upcastKeyword; yield! nodes |])
                |> List.singleton
                |> finalContinuation)
        | SynExpr.InferredDowncast (expr, StartRange 8 (downcastKeyword, range)) ->
            visit expr (fun nodes ->
                mkSynExprNode
                    SynExpr_InferredDowncast
                    synExpr
                    range
                    (sortChildren
                        [| yield mkNode SynExpr_InferredDowncast_Downcast downcastKeyword
                           yield! nodes |])
                |> List.singleton
                |> finalContinuation)
        | SynExpr.Null range ->
            mkSynExprNode SynExpr_Null synExpr range Array.empty
            |> List.singleton
            |> finalContinuation
        | SynExpr.AddressOf (isByRef, expr, _, range) ->
            let ampersandRange, ampersandType =
                if isByRef then
                    RangeHelpers.mkStartRange 1 range, SynExpr_AddressOf_SingleAmpersand
                else
                    RangeHelpers.mkStartRange 2 range, SynExpr_AddressOf_DoubleAmpersand

            visit expr (fun nodes ->
                mkSynExprNode
                    SynExpr_AddressOf
                    synExpr
                    range
                    (sortChildren [| yield mkNode ampersandType ampersandRange; yield! nodes |])
                |> List.singleton
                |> finalContinuation)
        | SynExpr.TraitCall (_typars, sign, expr, range) ->
            visit expr (fun nodes ->
                mkSynExprNode
                    SynExpr_TraitCall
                    synExpr
                    range
                    (sortChildren [| yield visitSynMemberSig sign; yield! nodes |])
                |> List.singleton
                |> finalContinuation)
        | SynExpr.JoinIn (expr, _, expr2, range) ->
            let continuations: ((TriviaNode list -> TriviaNode list) -> TriviaNode list) list =
                [ visit expr; visit expr2 ]

            processSequence finalContinuation continuations (fun nodes ->
                mkSynExprNode SynExpr_JoinIn synExpr range (sortChildren [| yield! nodes |]))
        | SynExpr.ImplicitZero range ->
            mkSynExprNode SynExpr_ImplicitZero synExpr range Array.empty
            |> List.singleton
            |> finalContinuation
        | SynExpr.YieldOrReturn ((isYield, _), expr, range) ->
            let keywordType, keywordRange =
                if isYield then
                    SynExpr_YieldOrReturn_Yield, RangeHelpers.mkStartRange 5 range
                else
                    SynExpr_YieldOrReturn_Return, RangeHelpers.mkStartRange 6 range

            visit expr (fun nodes ->
                mkSynExprNode
                    SynExpr_YieldOrReturn
                    synExpr
                    range
                    (sortChildren [| yield mkNode keywordType keywordRange; yield! nodes |])
                |> List.singleton
                |> finalContinuation)
        | SynExpr.YieldOrReturnFrom ((isYield, _), expr, range) ->
            let keywordType, keywordRange =
                if isYield then
                    SynExpr_YieldOrReturnFrom_YieldBang, RangeHelpers.mkStartRange 6 range
                else
                    SynExpr_YieldOrReturnFrom_ReturnBang, RangeHelpers.mkStartRange 7 range

            visit expr (fun nodes ->
                mkSynExprNode
                    SynExpr_YieldOrReturnFrom
                    synExpr
                    range
                    (sortChildren [| yield mkNode keywordType keywordRange; yield! nodes |])
                |> List.singleton
                |> finalContinuation)
        | SynExpr.LetOrUseBang (_, _, _, pat, rhsExpr, andBangs, body, range, trivia) ->
            let continuations: ((TriviaNode list -> TriviaNode list) -> TriviaNode list) list =
                [ yield visit rhsExpr; yield visit body ]

            processSequence finalContinuation continuations (fun nodes ->
                mkSynExprNode
                    SynExpr_LetOrUseBang
                    synExpr
                    range
                    (sortChildren
                        [| yield visitSynPat pat
                           yield! Option.toList (mkNodeOption SynExpr_LetOrUseBang_Equals trivia.EqualsRange)
                           yield! nodes
                           yield! List.map visitSynExprAndBang andBangs |]))
        | SynExpr.MatchBang (_, expr, clauses, range, trivia) ->
            visit expr (fun nodes ->
                mkSynExprNode
                    SynExpr_MatchBang
                    synExpr
                    range
                    (sortChildren
                        [| yield mkNode SynExpr_MatchBang_Match trivia.MatchBangKeyword
                           yield! nodes
                           yield mkNode SynExpr_MatchBang_With trivia.WithKeyword
                           yield! List.map visitSynMatchClause clauses |])
                |> List.singleton
                |> finalContinuation)
        | SynExpr.DoBang (expr, StartRange 3 (doBangKeyword, range)) ->
            visit expr (fun nodes ->
                mkSynExprNode
                    SynExpr_DoBang
                    synExpr
                    range
                    (sortChildren [| yield mkNode SynExpr_DoBang_DoBang doBangKeyword; yield! nodes |])
                |> List.singleton
                |> finalContinuation)
        | SynExpr.LibraryOnlyILAssembly (_, _, _, _, range) ->
            mkSynExprNode SynExpr_LibraryOnlyILAssembly synExpr range Array.empty
            |> List.singleton
            |> finalContinuation
        | SynExpr.LibraryOnlyStaticOptimization (_, _, _, range) ->
            mkSynExprNode SynExpr_LibraryOnlyStaticOptimization synExpr range Array.empty
            |> List.singleton
            |> finalContinuation
        | SynExpr.LibraryOnlyUnionCaseFieldGet (expr, _, _, range) ->
            visit expr (fun nodes ->
                mkSynExprNode SynExpr_LibraryOnlyUnionCaseFieldGet synExpr range [| yield! nodes |]
                |> List.singleton
                |> finalContinuation)
        | SynExpr.LibraryOnlyUnionCaseFieldSet (e1, _, _, e2, range) ->
            let continuations: ((TriviaNode list -> TriviaNode list) -> TriviaNode list) list =
                [ visit e1; visit e2 ]

            processSequence finalContinuation continuations (fun nodes ->
                mkSynExprNode SynExpr_LibraryOnlyUnionCaseFieldSet synExpr range (sortChildren [| yield! nodes |]))
        | SynExpr.ArbitraryAfterError (_, range) ->
            mkSynExprNode SynExpr_ArbitraryAfterError synExpr range Array.empty
            |> List.singleton
            |> finalContinuation
        | SynExpr.FromParseError (expr, range) ->
            visit expr (fun nodes ->
                mkSynExprNode SynExpr_FromParseError synExpr range [| yield! nodes |]
                |> List.singleton
                |> finalContinuation)
        | SynExpr.DiscardAfterMissingQualificationAfterDot (expr, range) ->
            visit expr (fun nodes ->
                mkSynExprNode SynExpr_DiscardAfterMissingQualificationAfterDot synExpr range [| yield! nodes |]
                |> List.singleton
                |> finalContinuation)
        | SynExpr.Fixed (expr, StartRange 5 (fixedKeyword, range)) ->
            visit expr (fun nodes ->
                mkSynExprNode
                    SynExpr_Fixed
                    synExpr
                    range
                    (sortChildren [| yield mkNode SynExpr_Fixed_Fixed fixedKeyword; yield! nodes |])
                |> List.singleton
                |> finalContinuation)
        | SynExpr.InterpolatedString (parts, _, range) ->
            mkSynExprNode
                SynExpr_InterpolatedString
                synExpr
                range
                (sortChildren [| yield! List.map visitSynInterpolatedStringPart parts |])
            |> List.singleton
            |> finalContinuation
        | SynExpr.IndexRange (e1, _, e2, _, _, range) ->
            let continuations: ((TriviaNode list -> TriviaNode list) -> TriviaNode list) list =
                [ yield! (e1 |> Option.map visit |> Option.toList)
                  yield! (e2 |> Option.map visit |> Option.toList) ]

            processSequence finalContinuation continuations (fun nodes ->
                mkSynExprNode SynExpr_Dynamic synExpr range (sortChildren [| yield! nodes |]))
        | SynExpr.IndexFromEnd (e, range) ->
            visit e (fun nodes ->
                mkSynExprNode SynExpr_IndexFromEnd synExpr range [| yield! nodes |]
                |> List.singleton
                |> finalContinuation)
        | SynExpr.DebugPoint (innerExpr = expr) -> visit expr finalContinuation
        | SynExpr.Dynamic (funcExpr, _, argExpr, range) ->
            let continuations: ((TriviaNode list -> TriviaNode list) -> TriviaNode list) list =
                [ visit funcExpr; visit argExpr ]

            processSequence finalContinuation continuations (fun nodes ->
                mkSynExprNode SynExpr_Dynamic synExpr range (sortChildren [| yield! nodes |]))
        | SynExpr.Typar (typar, range) ->
            mkSynExprNode SynExpr_Typar synExpr range [| visitSynTypar typar |]
            |> List.singleton
            |> finalContinuation

    visit synExpr id

and visitSynInterpolatedStringPart (synInterpolatedStringPart: SynInterpolatedStringPart) : TriviaNode =
    match synInterpolatedStringPart with
    | SynInterpolatedStringPart.String (_, range) -> mkNode SynInterpolatedStringPart_String range
    | SynInterpolatedStringPart.FillExpr (expr, ident) ->
        mkNodeWithChildren
            SynInterpolatedStringPart_FillExpr
            synInterpolatedStringPart.FullRange
            (sortChildren
                [| yield! visitSynExpr expr
                   yield! Option.toList (Option.map visitIdent ident) |])

and visitRecordField (SynExprRecordField ((fieldName, _), equalsRange, synExprOption, _blockSeparator) as rf) =
    mkNodeWithChildren
        SynExprRecordField_
        rf.FullRange
        (sortChildren
            [| yield visitSynLongIdent fieldName
               yield! Option.toList (mkNodeOption SynExprRecordField_Equals equalsRange)
               yield! (Option.map visitSynExpr >> Option.toList >> List.collect id) synExprOption |])

and visitAnonRecordField (SourceParser.AnonRecordFieldName (ident, equalsRange, expr, range)) =
    mkNodeWithChildren
        SynExpr_AnonRecd_Field
        range
        (sortChildren
            [| yield visitIdent ident
               yield! Option.toList (mkNodeOption SynExpr_AnonRecd_Field_Equals equalsRange)
               yield! visitSynExpr expr |])

and visitAnonRecordTypeField (_: Ident, t: SynType) = visitSynType t

and visitSynMemberSig (ms: SynMemberSig) : TriviaNode =
    match ms with
    | SynMemberSig.Member (valSig, mf, range) ->
        let valSigNode = visitSynValSig valSig

        mkNodeWithChildren
            SynMemberSig_Member
            range
            (sortChildren [| yield! visitSynMemberFlags mf; yield! valSigNode.Children |])
    | SynMemberSig.Interface (typeName, range) ->
        mkNodeWithChildren SynMemberSig_Interface range [| visitSynType typeName |]
    | SynMemberSig.Inherit (typeName, range) ->
        mkNodeWithChildren SynMemberSig_Inherit range [| visitSynType typeName |]
    | SynMemberSig.ValField (f, range) -> mkNodeWithChildren SynMemberSig_ValField range [| visitSynField f |]
    | SynMemberSig.NestedType (typedef, range) ->
        mkNodeWithChildren SynMemberSig_NestedType range [| visitSynTypeDefnSig typedef |]

and visitSynMatchClause (mc: SynMatchClause) : TriviaNode =
    match mc with
    | SourceParser.Clause (barRange, pat, eo, arrowRange, e, range) ->
        mkNodeWithChildren
            SynMatchClause_
            range
            (sortChildren
                [| yield visitSynPat pat
                   yield! Option.toList (mkNodeOption SynMatchClause_Bar barRange)
                   yield! (Option.map visitSynExpr >> Option.toList >> List.collect id) eo
                   yield! Option.toList (mkNodeOption SynMatchClause_Arrow arrowRange)
                   yield! visitSynExpr e |])

and visitSynExprAndBang (SynExprAndBang (_, _, _, pat, body, range, trivia)) : TriviaNode =
    mkNodeWithChildren
        SynExprAndBang_
        range
        (sortChildren
            [| yield visitSynPat pat
               yield mkNode SynExprAndBang_Equals trivia.EqualsRange
               yield! visitSynExpr body |])

and visitSynInterfaceImpl (ii: SynInterfaceImpl) : TriviaNode =
    match ii with
    | SynInterfaceImpl (typ, withKeyword, bindings, members, range) ->
        mkNodeWithChildren
            SynInterfaceImpl_
            range
            (sortChildren
                [| yield! Option.toList (mkNodeOption SynInterfaceImpl_With withKeyword)
                   yield visitSynType typ
                   yield! List.map visitSynBinding bindings
                   yield! visitSynMemberDefns members |])

and visitSynTypeDefn (td: SynTypeDefn) : TriviaNode =
    match td with
    | SynTypeDefn (SynComponentInfo (xmlDoc = preXmlDoc; attributes = attrs) as sci,
                   stdr,
                   members,
                   _implicitConstructor,
                   range,
                   trivia) ->
        let typeKeyword =
            mkNodeForRangeAfterXmlAndAttributes SynTypeDefn_Type preXmlDoc attrs trivia.TypeKeyword

        mkNodeWithChildren
            SynTypeDefn_
            range
            (sortChildren
                [| yield! typeKeyword
                   yield! Option.toList (mkNodeOption SynTypeDefn_Equals trivia.EqualsRange)
                   yield! visitSynComponentInfo sci
                   yield! visitSynTypeDefnRepr stdr
                   yield! Option.toList (mkNodeOption SynTypeDefn_With trivia.WithKeyword)
                   yield! visitSynMemberDefns members |])

and visitSynTypeDefnSig (typeDefSig: SynTypeDefnSig) : TriviaNode =
    match typeDefSig with
    | SynTypeDefnSig (sci, synTypeDefnSigReprs, memberSig, _, trivia) ->
        mkNodeWithChildren
            SynTypeDefnSig_
            typeDefSig.Range
            (sortChildren
                [| yield! Option.toList (mkNodeOption SynTypeDefnSig_Type trivia.TypeKeyword)
                   yield! visitSynComponentInfo sci
                   yield! Option.toList (mkNodeOption SynTypeDefnSig_Equals trivia.EqualsRange)
                   yield! visitSynTypeDefnSigRepr synTypeDefnSigReprs
                   yield! Option.toList (mkNodeOption SynTypeDefnSig_With trivia.WithKeyword)
                   yield! List.map visitSynMemberSig memberSig |])

and visitSynTypeDefnSigRepr (stdr: SynTypeDefnSigRepr) : TriviaNode list =
    match stdr with
    | SynTypeDefnSigRepr.ObjectModel (kind, members, _range) ->
        [ yield! Option.toList (visitSynTypeDefnKind kind)
          yield! List.map visitSynMemberSig members ]
    | SynTypeDefnSigRepr.Simple (simpleRepr, _range) -> visitSynTypeDefnSimpleRepr simpleRepr
    | SynTypeDefnSigRepr.Exception exceptionRepr -> [ visitSynExceptionDefnRepr exceptionRepr ]

and visitSynMemberDefn (mbrDef: SynMemberDefn) : TriviaNode =
    match mbrDef with
    | SynMemberDefn.Open (target, parentRange) ->
        // we use the parent ranges here to match up with the trivia parsed
        match target with
        | SynOpenDeclTarget.ModuleOrNamespace (longIdent, _range) ->
            mkNodeWithChildren SynMemberDefn_Open parentRange [| visitSynLongIdent longIdent |]
        | SynOpenDeclTarget.Type (synType, _range) ->
            mkNodeWithChildren SynMemberDefn_OpenType parentRange [| visitSynType synType |]
    | SynMemberDefn.Member (memberDefn, range) ->
        mkNodeWithChildren SynMemberDefn_Member range [| visitSynBinding memberDefn |]
    | SynMemberDefn.ImplicitCtor (_, attrs, ctorArgs, _, _xmlDoc, range) ->
        mkNodeWithChildren
            SynMemberDefn_ImplicitCtor
            range
            (sortChildren [| yield! (visitSynAttributeLists attrs); yield visitSynSimplePats ctorArgs |])
    | SynMemberDefn.ImplicitInherit (inheritType, inheritArgs, _, range) ->
        mkNodeWithChildren
            SynMemberDefn_ImplicitInherit
            range
            (sortChildren [| yield visitSynType inheritType; yield! visitSynExpr inheritArgs |])
    | SynMemberDefn.LetBindings (bindings, _, _, range) ->
        mkNodeWithChildren SynMemberDefn_LetBindings range (sortChildren [| yield! List.map visitSynBinding bindings |])
    | SynMemberDefn.AbstractSlot (valSig, memberFlags, range) ->
        let valSigNode = visitSynValSig valSig

        mkNodeWithChildren
            SynMemberDefn_AbstractSlot
            range
            (sortChildren [| yield! visitSynMemberFlags memberFlags; yield! valSigNode.Children |])
    | SynMemberDefn.Interface (typ, withKeyword, members, range) ->
        let ms =
            match members with
            | None -> []
            | Some ms -> visitSynMemberDefns ms

        mkNodeWithChildren
            SynMemberDefn_Interface
            range
            [| yield! Option.toList (mkNodeOption SynMemberDefn_Interface_With withKeyword)
               yield visitSynType typ
               yield! ms |]
    | SynMemberDefn.Inherit (typ, _, range) -> mkNodeWithChildren SynMemberDefn_Inherit range [| visitSynType typ |]
    | SynMemberDefn.ValField (fld, range) -> mkNodeWithChildren SynMemberDefn_ValField range [| visitSynField fld |]
    | SynMemberDefn.NestedType (typeDefn, _, range) ->
        mkNodeWithChildren SynMemberDefn_NestedType range [| visitSynTypeDefn typeDefn |]
    | SynMemberDefn.AutoProperty (attrs, _, _, typeOpt, _, _, _, _, _, equalsRange, synExpr, withKeyword, _, range) ->
        mkNodeWithChildren
            SynMemberDefn_AutoProperty
            range
            (sortChildren
                [| yield mkNode SynMemberDefn_AutoProperty_Equals equalsRange
                   yield! Option.toList (mkNodeOption SynMemberDefn_AutoProperty_With withKeyword)
                   yield! (visitSynAttributeLists attrs)
                   yield! (Option.map visitSynType >> Option.toList) typeOpt
                   yield! visitSynExpr synExpr |])
    | SynMemberDefn.GetSetMember (getBinding, setBinding, range, trivia) ->
        let visitBinding = Option.toList >> List.map visitSynBinding
        let mkKeyword t r = mkNodeOption t r |> Option.toList

        let keywords =
            [| yield mkNode SynMemberDefn_GetSetMember_With trivia.WithKeyword
               yield! mkKeyword SynMemberDefn_GetSetMember_Get trivia.GetKeyword
               yield! mkKeyword SynMemberDefn_GetSetMember_And trivia.AndKeyword
               yield! mkKeyword SynMemberDefn_GetSetMember_Set trivia.SetKeyword |]

        mkNodeWithChildren
            SynMemberDefn_GetSetMember
            range
            (sortChildren
                [| yield! visitBinding getBinding
                   yield! visitBinding setBinding
                   yield! keywords |])

and visitSynMemberDefns (ms: SynMemberDefn list) : TriviaNode list = List.map visitSynMemberDefn ms

and visitSynSimplePat (sp: SynSimplePat) : TriviaNode =
    let rec visit (sp: SynSimplePat) (continuation: TriviaNode -> TriviaNode) : TriviaNode =
        match sp with
        | SynSimplePat.Id (_, _, _, _, _, range) -> mkNode SynSimplePat_Id range |> continuation
        | SynSimplePat.Typed (simplePat, typ, range) ->
            visit simplePat (fun node ->
                mkNodeWithChildren SynSimplePat_Typed range (sortChildren [| node; visitSynType typ |])
                |> continuation)
        | SynSimplePat.Attrib (simplePat, attrs, range) ->
            visit simplePat (fun node ->
                mkNodeWithChildren
                    SynSimplePat_Attrib
                    range
                    (sortChildren [| yield node; yield! visitSynAttributeLists attrs |])
                |> continuation)

    visit sp id

and visitSynSimplePats (sp: SynSimplePats) : TriviaNode =
    let rec visit (sp: SynSimplePats) (continuation: TriviaNode -> TriviaNode) =
        match sp with
        | SynSimplePats.SimplePats (pats, range) ->
            mkNodeWithChildren
                SynSimplePats_SimplePats
                range
                (sortChildren [| yield! List.map visitSynSimplePat pats |])
            |> continuation
        | SynSimplePats.Typed (pats, typ, range) ->
            visit pats (fun node ->
                mkNodeWithChildren SynSimplePat_Typed range (sortChildren [| node; visitSynType typ |])
                |> continuation)

    visit sp id

and visitSynBinding (binding: SynBinding) : TriviaNode =
    match binding with
    | SynBinding (_, kind, _, _, attrs, preXml, valData, headPat, returnInfo, expr, _range, _, trivia) ->
        let t =
            match kind with
            | SynBindingKind.StandaloneExpression -> SynBindingKind_StandaloneExpression
            | SynBindingKind.Normal -> SynBindingKind_Normal
            | SynBindingKind.Do -> SynBindingKind_Do

        let headPatNodes =
            match kind with
            | SynBindingKind.Do -> None
            | _ -> Some(visitSynPat headPat)

        let keywordNode =
            let triviaType, range =
                match trivia.ExternKeyword with
                | Some _ -> SynBinding_Extern, trivia.ExternKeyword
                | None -> SynBinding_Let, trivia.LetKeyword

            mkNodeForRangeAfterXmlAndAttributes triviaType preXml attrs range

        let expr = SourceParser.parseExpressionInSynBinding returnInfo expr
        let returnInfo = Option.map visitSynBindingReturnInfo returnInfo

        mkNodeWithChildren
            t
            binding.RangeOfBindingWithRhs
            (sortChildren
                [| yield! visitSynAttributeLists attrs
                   yield! keywordNode
                   yield! visitSynValData valData
                   yield! Option.toList headPatNodes
                   yield! Option.toList returnInfo
                   yield! Option.toList (mkNodeOption SynBinding_Equals trivia.EqualsRange)
                   yield! visitSynExpr expr |])

and visitSynValData (svd: SynValData) : TriviaNode list =
    match svd with
    | SynValData (Some memberFlags, _, _) -> visitSynMemberFlags memberFlags
    | _ -> []

and visitSynMemberFlags (memberFlags: SynMemberFlags) : TriviaNode list =
    [ yield! Option.toList (mkNodeOption SynMemberFlags_Static memberFlags.Trivia.StaticRange)
      yield! Option.toList (mkNodeOption SynMemberFlags_Member memberFlags.Trivia.MemberRange)
      yield! Option.toList (mkNodeOption SynMemberFlags_Abstract memberFlags.Trivia.AbstractRange) ]

and visitSynValSig (svs: SynValSig) : TriviaNode =
    match svs with
    | SynValSig (attrs, ident, explicitValDecls, synType, _, _, _, _, _, expr, range, trivia) ->
        mkSynValSig
            svs
            range
            (sortChildren
                [| yield! Option.toList (mkNodeOption SynValSig_Val trivia.ValKeyword)
                   yield visitSynIdent ident
                   yield! (visitSynAttributeLists attrs)
                   yield! Option.toList (visitSynValTyparDecls explicitValDecls)
                   yield visitSynType synType
                   yield! visitOptSynExpr expr
                   yield! Option.toList (mkNodeOption SynValSig_With trivia.ValKeyword) |])

and visitSynValTyparDecls (valTypeDecl: SynValTyparDecls) : TriviaNode option =
    match valTypeDecl with
    | SynValTyparDecls (Some typardecl, _) -> Some(visitSynTyparDecls typardecl)
    | _ -> None

and visitSynTyparDecl (std: SynTyparDecl) : TriviaNode =
    match std with
    | SourceParser.TyparDecl (attrs, synTypar, fullRange) ->
        mkNodeWithChildren
            SynTyparDecl_
            fullRange
            (sortChildren [| yield! (visitSynAttributeLists attrs); yield visitSynTypar synTypar |])

and visitSynTypar (SynTypar (ident, _typarStaticReq, _isCompGen)) = mkNode Ident_ ident.idRange

and visitSynBindingReturnInfo (returnInfo: SynBindingReturnInfo) : TriviaNode =
    match returnInfo with
    | SynBindingReturnInfo (typeName, range, attrs) ->
        mkNodeWithChildren
            SynBindingReturnInfo_
            range
            (sortChildren [| yield visitSynType typeName; yield! (visitSynAttributeLists attrs) |])

and visitSynPat (sp: SynPat) : TriviaNode =
    let rec visit (sp: SynPat) (finalContinuation: TriviaNode -> TriviaNode) : TriviaNode =
        match sp with
        | SynPat.Const (sc, range) -> visitSynConst range sc |> finalContinuation
        | SynPat.Wild range -> mkNode SynPat_Wild range |> finalContinuation
        | SynPat.Named (si, _, _, range) ->
            mkNodeWithChildren SynPat_Named range [| visitSynIdent si |]
            |> finalContinuation
        | SynPat.As (synPat, synPat2, range) ->
            let continuations: ((TriviaNode -> TriviaNode) -> TriviaNode) list =
                [ visit synPat; visit synPat2 ]

            let finalContinuation (nodes: TriviaNode list) : TriviaNode =
                mkNodeWithChildren SynPat_As range (sortChildren [| yield! nodes |])
                |> finalContinuation

            Continuation.sequence continuations finalContinuation
        | SynPat.Typed (synPat, synType, range) ->
            visit synPat (fun node ->
                mkNodeWithChildren SynPat_Typed range (sortChildren [| node; visitSynType synType |])
                |> finalContinuation)
        | SynPat.Attrib (synPat, attrs, range) ->
            visit synPat (fun node ->
                mkNodeWithChildren
                    SynPat_Attrib
                    range
                    (sortChildren [| yield node; yield! (visitSynAttributeLists attrs) |])
                |> finalContinuation)
        | SynPat.Or (synPat, synPat2, range, trivia) ->
            let continuations: ((TriviaNode -> TriviaNode) -> TriviaNode) list =
                [ visit synPat; visit synPat2 ]

            let finalContinuation (nodes: TriviaNode list) : TriviaNode =
                mkNodeWithChildren
                    SynPat_Or
                    range
                    (sortChildren [| yield! nodes; yield mkNode SynPat_Or_Bar trivia.BarRange |])
                |> finalContinuation

            Continuation.sequence continuations finalContinuation

        | SynPat.ListCons (p1, p2, range, trivia) ->
            let continuations: ((TriviaNode -> TriviaNode) -> TriviaNode) list =
                [ visit p1; visit p2 ]

            let finalContinuation (nodes: TriviaNode list) : TriviaNode =
                mkNodeWithChildren
                    SynPat_ListCons
                    range
                    (sortChildren
                        [| yield! nodes
                           yield mkNode SynPat_ListCons_ColonColon trivia.ColonColonRange |])
                |> finalContinuation

            Continuation.sequence continuations finalContinuation
        | SynPat.Ands (pats, range) ->
            let continuations: ((TriviaNode -> TriviaNode) -> TriviaNode) list =
                pats |> List.map visit

            let finalContinuation (nodes: TriviaNode list) : TriviaNode =
                mkNodeWithChildren SynPat_Ands range (sortChildren [| yield! nodes |])
                |> finalContinuation

            Continuation.sequence continuations finalContinuation
        | SynPat.LongIdent (_, _, svtd, ctorArgs, _, range) ->
            mkNodeWithChildren
                SynPat_LongIdent
                range
                (sortChildren
                    [| yield! Option.toList (Option.bind visitSynValTyparDecls svtd)
                       yield! visitSynConstructorArgs ctorArgs |])
            |> finalContinuation
        | SynPat.Tuple (_, pats, range) ->
            let continuations: ((TriviaNode -> TriviaNode) -> TriviaNode) list =
                pats |> List.map visit

            let finalContinuation (nodes: TriviaNode list) : TriviaNode =
                mkNodeWithChildren SynPat_Tuple range (sortChildren [| yield! nodes |])
                |> finalContinuation

            Continuation.sequence continuations finalContinuation
        | SynPat.Paren (pat, StartEndRange 1 (lpr, range, rpr)) ->
            visit pat (fun node ->
                mkNodeWithChildren
                    SynPat_Paren
                    range
                    (sortChildren
                        [| yield mkNode SynPat_Paren_OpeningParenthesis lpr
                           yield node
                           yield mkNode SynPat_Paren_ClosingParenthesis rpr |])
                |> finalContinuation)

        | SynPat.ArrayOrList (_, pats, range) ->
            let continuations: ((TriviaNode -> TriviaNode) -> TriviaNode) list =
                List.map visit pats

            let finalContinuation (nodes: TriviaNode list) : TriviaNode =
                mkNodeWithChildren SynPat_ArrayOrList range (sortChildren [| yield! nodes |])
                |> finalContinuation

            Continuation.sequence continuations finalContinuation

        | SynPat.Record (pats, range) ->
            let continuations: ((TriviaNode -> TriviaNode) -> TriviaNode) list =
                pats |> List.map (fun (_, _, pat) -> visit pat)

            let finalContinuation (nodes: TriviaNode list) : TriviaNode =
                mkNodeWithChildren SynPat_Record range (sortChildren [| yield! nodes |])
                |> finalContinuation

            Continuation.sequence continuations finalContinuation
        | SynPat.Null range -> mkNode SynPat_Null range |> finalContinuation
        | SynPat.OptionalVal (_, range) -> mkNode SynPat_OptionalVal range |> finalContinuation
        | SynPat.IsInst (typ, range) ->
            mkNodeWithChildren SynPat_IsInst range [| visitSynType typ |]
            |> finalContinuation
        | SynPat.QuoteExpr (expr, range) ->
            mkNodeWithChildren SynPat_QuoteExpr range [| yield! visitSynExpr expr |]
            |> finalContinuation
        | SynPat.DeprecatedCharRange (_, _, range) -> mkNode SynPat_DeprecatedCharRange range |> finalContinuation
        | SynPat.InstanceMember (_, _, _, _, range) -> mkNode SynPat_InstanceMember range |> finalContinuation
        | SynPat.FromParseError (pat, range) ->
            visit pat (fun node -> mkNodeWithChildren SynPat_FromParseError range [| node |] |> finalContinuation)

    visit sp id

and visitSynConstructorArgs (ctorArgs: SynArgPats) : TriviaNode list =
    match ctorArgs with
    | SynArgPats.Pats pats -> List.map visitSynPat pats
    | SynArgPats.NamePatPairs (pats, _, { ParenRange = StartEndRange 1 (lpr, range, rpr) }) ->
        let children =
            pats
            |> List.map (fun (ident, eqRange, pat) ->
                mkNodeWithChildren
                    SynArgPats_NamePatPair
                    (Range.unionRanges ident.idRange pat.Range)
                    [| visitIdent ident
                       mkNode SynArgPats_NamePatPairs_Equals eqRange
                       visitSynPat pat |])

        [ mkNodeWithChildren
              SynArgPats_NamePatPairs
              range
              (sortChildren
                  [| yield mkNode SynArgPats_NamePatPairs_OpeningParenthesis lpr
                     yield! children
                     yield mkNode SynArgPats_NamePatPairs_ClosingParenthesis rpr |]) ]

and visitSynComponentInfo (sci: SynComponentInfo) : TriviaNode list =
    match sci with
    | SynComponentInfo (attributes = attribs; typeParams = typeParams) ->
        [ yield! visitSynAttributeLists attribs
          yield! (Option.map visitSynTyparDecls >> Option.toList) typeParams ]

and visitSynTyparDecls (decls: SynTyparDecls) : TriviaNode =
    match decls with
    | SourceParser.PostfixList (gt, decls, _constraints, lt, range) ->
        mkNodeWithChildren
            SynTyparDecls_PostfixList
            range
            (sortChildren
                [| yield mkNode SynTyparDecls_PostfixList_Greater gt
                   yield! List.map visitSynTyparDecl decls
                   yield mkNode SynTyparDecls_PostfixList_Lesser lt |])
    | SynTyparDecls.PostfixList (decls, _constraints, range) ->
        mkNodeWithChildren SynTyparDecls_PostfixList range (sortChildren [| yield! List.map visitSynTyparDecl decls |])
    | SynTyparDecls.PrefixList (decls, range) ->
        mkNodeWithChildren SynTyparDecls_PrefixList range (sortChildren [| yield! List.map visitSynTyparDecl decls |])
    | SynTyparDecls.SinglePrefix (decl, range) ->
        mkNodeWithChildren SynTyparDecls_SinglePrefix range [| visitSynTyparDecl decl |]

and visitSynTypeDefnRepr (stdr: SynTypeDefnRepr) : TriviaNode list =
    match stdr with
    | SynTypeDefnRepr.ObjectModel (kind, members, _range) ->
        [ yield! Option.toList (visitSynTypeDefnKind kind)
          yield! visitSynMemberDefns members ]
    | SynTypeDefnRepr.Simple (simpleRepr, _) -> visitSynTypeDefnSimpleRepr simpleRepr
    | SynTypeDefnRepr.Exception exceptionRepr -> [ visitSynExceptionDefnRepr exceptionRepr ]

and visitSynTypeDefnKind (kind: SynTypeDefnKind) : TriviaNode option =
    match kind with
    | SynTypeDefnKind.Unspecified
    | SynTypeDefnKind.Class
    | SynTypeDefnKind.Interface
    | SynTypeDefnKind.Struct
    | SynTypeDefnKind.Record
    | SynTypeDefnKind.Abbrev
    | SynTypeDefnKind.Opaque
    | SynTypeDefnKind.Union
    | SynTypeDefnKind.IL -> None
    | SynTypeDefnKind.Augmentation withRange -> Some(mkNode SynTypeDefnKind_Augmentation_With withRange)

    | SynTypeDefnKind.Delegate (typ, _valInfo) ->
        Some(mkNodeWithChildren SynTypeDefnKind_Delegate typ.Range (sortChildren [| visitSynType typ |]))

and visitSynTypeDefnSimpleRepr (arg: SynTypeDefnSimpleRepr) : TriviaNode list =
    match arg with
    | SynTypeDefnSimpleRepr.None _range -> []
    | SynTypeDefnSimpleRepr.Union (vis, unionCases, _range) ->
        [ yield! visitOptSynAccess vis; yield! List.map visitSynUnionCase unionCases ]
    | SynTypeDefnSimpleRepr.Enum (enumCases, _range) -> List.map visitSynEnumCase enumCases
    | SynTypeDefnSimpleRepr.Record (vis, recordFields, StartEndRange 1 (openingBrace, _range, closingBrace)) ->
        [ yield! visitOptSynAccess vis
          yield mkNode SynTypeDefnSimpleRepr_Record_OpeningBrace openingBrace
          yield! List.map visitSynField recordFields
          yield mkNode SynTypeDefnSimpleRepr_Record_ClosingBrace closingBrace ]
    | SynTypeDefnSimpleRepr.General (_, _, _, _, _, _, _, _range) -> []
    | SynTypeDefnSimpleRepr.LibraryOnlyILAssembly (_, _range) -> []
    | SynTypeDefnSimpleRepr.TypeAbbrev (_, typ, _range) -> [ visitSynType typ ]
    | SynTypeDefnSimpleRepr.Exception edr -> [ visitSynExceptionDefnRepr edr ]

and visitSynExceptionDefn (exceptionDef: SynExceptionDefn) : TriviaNode =
    match exceptionDef with
    | SynExceptionDefn (sedr, withKeyword, members, range) ->
        mkNodeWithChildren
            SynExceptionDefn_
            range
            (sortChildren
                [| yield visitSynExceptionDefnRepr sedr
                   yield! Option.toList (mkNodeOption SynExceptionDefn_With withKeyword)
                   yield! visitSynMemberDefns members |])

and visitSynExceptionDefnRepr (sedr: SynExceptionDefnRepr) : TriviaNode =
    match sedr with
    | SynExceptionDefnRepr (attrs, unionCase, longId, _, _, range) ->
        mkNodeWithChildren
            SynExceptionDefnRepr_
            range
            (sortChildren
                [| yield! (visitSynAttributeLists attrs)
                   yield visitSynUnionCase unionCase
                   yield! (Option.map visitLongIdentIncludingFullRange >> Option.toList) longId |])

and visitSynAttribute (attr: SynAttribute) : TriviaNode =
    { Range = attr.Range
      Type = SynAttribute_
      Children = [| yield! visitSynExpr attr.ArgExpr |]
      FSharpASTNode = None }

and visitSynAttributeLists (attrs: SynAttributeList list) : TriviaNode list = List.map visitSynAttributeList attrs

and visitSynAttributeList (attrs: SynAttributeList) : TriviaNode =
    { Range = attrs.Range
      Type = SynAttributeList_
      Children = sortChildren [| yield! List.map visitSynAttribute attrs.Attributes |]
      FSharpASTNode = None }

and visitSynUnionCase (uc: SynUnionCase) : TriviaNode =
    match uc with
    | SourceParser.UnionCase (attrs, _, barRange, _, synIdent, uct, range) ->
        mkNodeWithChildren
            SynUnionCase_
            range
            (sortChildren
                [| yield! Option.toList (mkNodeOption SynUnionCase_Bar barRange)
                   yield visitSynIdent synIdent
                   yield! visitSynUnionCaseType uct
                   yield! (visitSynAttributeLists attrs) |])

and visitSynUnionCaseType (uct: SynUnionCaseKind) : TriviaNode list =
    match uct with
    | SynUnionCaseKind.Fields fields -> List.map visitSynField fields
    | SynUnionCaseKind.FullType (stype, _) -> [ visitSynType stype ]

and visitSynEnumCase (sec: SynEnumCase) : TriviaNode =
    match sec with
    | SourceParser.EnumCase (attrs, barRange, _, synIdent, equalsRange, value, valueRange, range) ->
        mkNodeWithChildren
            SynEnumCase_
            range
            (sortChildren
                [| yield! Option.toList (mkNodeOption SynEnumCase_Bar barRange)
                   yield! (visitSynAttributeLists attrs)
                   yield visitSynIdent synIdent
                   yield mkNode SynEnumCase_Equals equalsRange
                   yield visitSynConst valueRange value |])

and visitSynField (sfield: SynField) : TriviaNode =
    match sfield with
    | SynField (attrs, _, ident, typ, _, preXmlDoc, _, _range) ->
        let innerNode =
            match ident with
            | None -> []
            | Some ident ->
                Range.unionRanges ident.idRange typ.Range
                |> Some
                |> mkNodeForRangeAfterXmlAndAttributes SynField_IdentifierAndType preXmlDoc attrs

        mkNodeWithChildren
            SynField_
            sfield.FullRange
            (sortChildren
                [| yield! (visitSynAttributeLists attrs)
                   yield! innerNode
                   yield visitSynType typ |])

and visitSynType (st: SynType) : TriviaNode =
    let rec visit (st: SynType) (finalContinuation: TriviaNode -> TriviaNode) : TriviaNode =
        match st with
        | SynType.LongIdent li -> visitSynLongIdent li |> finalContinuation
        | SynType.App (typeName, lessRange, typeArgs, _, greaterRange, _, range) ->
            let continuations: ((TriviaNode -> TriviaNode) -> TriviaNode) list =
                [ yield! (List.map visit typeArgs); yield visit typeName ]

            let finalContinuation (nodes: TriviaNode list) : TriviaNode =
                mkNodeWithChildren
                    SynType_App
                    range
                    (sortChildren
                        [| yield! Option.toList (mkNodeOption SynType_App_Less lessRange)
                           yield! nodes
                           yield! Option.toList (mkNodeOption SynType_App_Greater greaterRange) |])
                |> finalContinuation

            Continuation.sequence continuations finalContinuation
        | SynType.LongIdentApp (typeName, _, _, typeArgs, _, _, range) ->
            let continuations: ((TriviaNode -> TriviaNode) -> TriviaNode) list =
                [ yield visit typeName; yield! (List.map visit typeArgs) ]

            let finalContinuation (nodes: TriviaNode list) : TriviaNode =
                mkNodeWithChildren SynType_LongIdentApp range (sortChildren [| yield! nodes |])
                |> finalContinuation

            Continuation.sequence continuations finalContinuation
        | SynType.Tuple (_, path, range) ->
            let continuations: ((TriviaNode -> TriviaNode) -> TriviaNode) list =
                List.choose
                    (fun t ->
                        match t with
                        | SynTupleTypeSegment.Type t -> Some(visit t)
                        | _ -> None)
                    path

            let finalContinuation (nodes: TriviaNode list) : TriviaNode =
                mkNodeWithChildren SynType_Tuple range (sortChildren [| yield! nodes |])
                |> finalContinuation

            Continuation.sequence continuations finalContinuation
        | SynType.Array (_, elementType, range) ->
            visit elementType (fun node -> mkNodeWithChildren SynType_Array range [| node |] |> finalContinuation)
        | SourceParser.TFuns (ts, returnType) ->
            let continuations: ((TriviaNode -> TriviaNode) -> TriviaNode) list =
                [ yield! List.map (fst >> visit) ts; yield visit returnType ]

            let finalContinuation (nodes: TriviaNode list) : TriviaNode =
                mkNodeWithChildren
                    SynType_Fun
                    st.Range
                    (sortChildren
                        [| yield! nodes
                           yield! List.map (fun (_, arrow) -> mkNode SynType_Fun_Arrow arrow) ts |])
                |> finalContinuation

            Continuation.sequence continuations finalContinuation
        | SynType.Fun (argType, returnType, range, { ArrowRange = arrow }) ->
            let continuations: ((TriviaNode -> TriviaNode) -> TriviaNode) list =
                [ visit argType; visit returnType ]

            let finalContinuation (nodes: TriviaNode list) : TriviaNode =
                mkNodeWithChildren
                    SynType_Fun
                    range
                    (sortChildren [| yield! nodes; yield mkNode SynType_Fun_Arrow arrow |])
                |> finalContinuation

            Continuation.sequence continuations finalContinuation
        | SynType.Var (_, range) -> mkNode SynType_Var range |> finalContinuation
        | SynType.Anon range -> mkNode SynType_Anon range |> finalContinuation
        | SynType.WithGlobalConstraints (typeName, _, range) ->
            visit typeName (fun node ->
                mkNodeWithChildren SynType_WithGlobalConstraints range [| node |]
                |> finalContinuation)
        | SynType.HashConstraint (synType, range) ->
            visit synType (fun node -> mkNodeWithChildren SynType_HashConstraint range [| node |] |> finalContinuation)
        | SynType.MeasureDivide (dividendType, divisorType, range) ->
            let continuations: ((TriviaNode -> TriviaNode) -> TriviaNode) list =
                [ visit dividendType; visit divisorType ]

            let finalContinuation (nodes: TriviaNode list) : TriviaNode =
                mkNodeWithChildren SynType_MeasureDivide range (sortChildren [| yield! nodes |])
                |> finalContinuation

            Continuation.sequence continuations finalContinuation
        | SynType.MeasurePower (measureType, _, range) ->
            visit measureType (fun node -> mkNodeWithChildren SynType_MeasurePower range [| node |] |> finalContinuation)
        | SynType.StaticConstant (constant, range) ->
            mkNodeWithChildren SynType_StaticConstant range [| visitSynConst range constant |]
            |> finalContinuation
        | SynType.StaticConstantExpr (expr, range) ->
            mkNodeWithChildren SynType_StaticConstantExpr range [| yield! visitSynExpr expr |]
            |> finalContinuation
        | SynType.StaticConstantNamed (expr, typ, range) ->
            let continuations: ((TriviaNode -> TriviaNode) -> TriviaNode) list =
                [ visit expr; visit typ ]

            let finalContinuation (nodes: TriviaNode list) : TriviaNode =
                mkNodeWithChildren SynType_StaticConstantNamed range (sortChildren [| yield! nodes |])
                |> finalContinuation

            Continuation.sequence continuations finalContinuation
        | SynType.AnonRecd (_, typeNames, range) ->
            mkNodeWithChildren SynType_AnonRecd range [| yield! List.map visitAnonRecordTypeField typeNames |]
            |> finalContinuation
        | SynType.Paren (innerType, StartEndRange 1 (lpr, range, rpr)) ->
            visit innerType (fun node ->
                mkNodeWithChildren
                    SynType_Paren
                    range
                    (sortChildren
                        [| yield mkNode SynType_Paren_OpeningParenthesis lpr
                           yield node
                           yield mkNode SynType_Paren_ClosingParenthesis rpr |])
                |> finalContinuation)
        | SynType.SignatureParameter (attrs, _, identOpt, t, range) ->
            visit t (fun node ->
                mkNodeWithChildren
                    SynType_SignatureParameter
                    range
                    (sortChildren
                        [| yield! (List.map visitSynAttributeList attrs)
                           yield! (Option.toList >> List.map visitIdent) identOpt
                           yield node |])
                |> finalContinuation)
        | SynType.Or (lhs, rhs, range, trivia) ->
            let continuations: ((TriviaNode -> TriviaNode) -> TriviaNode) list =
                [ visit lhs; visit rhs ]

            let finalContinuation (nodes: TriviaNode list) : TriviaNode =
                mkNodeWithChildren
                    SynType_Or
                    range
                    (sortChildren [| yield! nodes; yield mkNode SynType_Or_Or trivia.OrKeyword |])
                |> finalContinuation

            Continuation.sequence continuations finalContinuation

    visit st id

and visitSynConst (parentRange: Range) (sc: SynConst) : TriviaNode =
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
    | SynConst.Measure (n, numberRange, _) -> mkNode (t n) numberRange
    | SynConst.Unit ->
        match parentRange with
        | StartEndRange 1 (lpr, range, rpr) ->
            mkNodeWithChildren
                SynConst_Unit
                range
                [| mkNode SynConst_Unit_OpeningParenthesis lpr
                   mkNode SynConst_Unit_ClosingParenthesis rpr |]
    | _ -> mkNode (t sc) (sc.Range parentRange)

and visitParsedHashDirective (hash: ParsedHashDirective) : TriviaNode =
    match hash with
    | ParsedHashDirective (_, _, range) -> mkNode ParsedHashDirective_ range

and visitSynModuleOrNamespaceSig
    (SourceParser.SigModuleOrNamespace (attrs,
                                        _,
                                        moduleKeyword,
                                        namespaceKeyword,
                                        _,
                                        longIdent,
                                        decls,
                                        _,
                                        kind,
                                        fullRange))
    =
    let astType, moduleOrNamespace =
        match kind with
        | SynModuleOrNamespaceKind.DeclaredNamespace ->
            SynModuleOrNamespaceSig_DeclaredNamespace, mkNodeOption SynModuleOrNamespace_Namespace namespaceKeyword
        | SynModuleOrNamespaceKind.GlobalNamespace ->
            SynModuleOrNamespaceSig_GlobalNamespace, mkNodeOption SynModuleOrNamespace_Namespace namespaceKeyword
        | SynModuleOrNamespaceKind.NamedModule ->
            SynModuleOrNamespaceSig_NamedModule, mkNodeOption SynModuleOrNamespace_Module moduleKeyword
        | SynModuleOrNamespaceKind.AnonModule -> SynModuleOrNamespaceSig_AnonModule, None

    let longIdentNodes =
        match kind, decls with
        | SynModuleOrNamespaceKind.AnonModule, _ :: _ -> None
        | _ -> Some(visitLongIdentIncludingFullRange longIdent)

    { Range = fullRange
      Type = astType
      FSharpASTNode = None
      Children =
        sortChildren
            [| yield! Option.toList moduleOrNamespace
               yield! Option.toList longIdentNodes
               yield! visitSynAttributeLists attrs
               yield! (List.map visitSynModuleSigDecl decls) |] }

and visitSynModuleSigDecl (ast: SynModuleSigDecl) : TriviaNode =
    let rec visit (ast: SynModuleSigDecl) (finalContinuation: TriviaNode -> TriviaNode) : TriviaNode =
        match ast with
        | SynModuleSigDecl.ModuleAbbrev (_, _, range) ->
            mkSynModuleSigDeclNode SynModuleSigDecl_ModuleAbbrev ast range Array.empty
            |> finalContinuation
        | SynModuleSigDecl.NestedModule (SynComponentInfo (xmlDoc = preXmlDoc; attributes = attrs) as sci,
                                         _,
                                         decls,
                                         range,
                                         trivia) ->
            let continuations: ((TriviaNode -> TriviaNode) -> TriviaNode) list =
                List.map visit decls

            let moduleKeyword =
                mkNodeForRangeAfterXmlAndAttributes
                    SynModuleSigDecl_NestedModule_Module
                    preXmlDoc
                    attrs
                    trivia.ModuleKeyword

            let finalContinuation (nodes: TriviaNode list) : TriviaNode =
                mkSynModuleSigDeclNode
                    SynModuleSigDecl_NestedModule
                    ast
                    range
                    (sortChildren
                        [| yield! moduleKeyword
                           yield! visitSynComponentInfo sci
                           yield! Option.toList (mkNodeOption SynModuleSigDecl_NestedModule_Equals trivia.EqualsRange)
                           yield! nodes |])
                |> finalContinuation

            Continuation.sequence continuations finalContinuation
        | SynModuleSigDecl.Val (SynValSig.SynValSig _ as node, range) ->
            mkSynModuleSigDeclNode SynModuleSigDecl_Val ast range [| visitSynValSig node |]
            |> finalContinuation
        | SynModuleSigDecl.Types (typeDefs, range) ->
            mkSynModuleSigDeclNode
                SynModuleSigDecl_Types
                ast
                range
                (sortChildren [| yield! List.map visitSynTypeDefnSig typeDefs |])
            |> finalContinuation
        | SynModuleSigDecl.Open (target, parentRange) ->
            // we use the parent ranges here to match up with the trivia parsed
            match target with
            | SynOpenDeclTarget.ModuleOrNamespace (longId, _range) ->
                mkSynModuleSigDeclNode
                    SynModuleSigDecl_Open
                    ast
                    parentRange
                    (sortChildren [| visitSynLongIdent longId |])
                |> finalContinuation
            | SynOpenDeclTarget.Type (synType, _range) ->
                mkSynModuleSigDeclNode
                    SynModuleSigDecl_OpenType
                    ast
                    parentRange
                    (sortChildren [| visitSynType synType |])
                |> finalContinuation
        | SynModuleSigDecl.HashDirective (hash, range) ->
            mkSynModuleSigDeclNode SynModuleSigDecl_HashDirective ast range [| visitParsedHashDirective hash |]
            |> finalContinuation
        | SynModuleSigDecl.NamespaceFragment moduleOrNamespace ->
            visitSynModuleOrNamespaceSig moduleOrNamespace |> finalContinuation
        | SynModuleSigDecl.Exception (synExceptionSig, range) ->
            mkSynModuleSigDeclNode SynModuleSigDecl_Exception ast range [| visitSynExceptionSig synExceptionSig |]
            |> finalContinuation

    visit ast id

and visitSynExceptionSig (exceptionDef: SynExceptionSig) : TriviaNode =
    match exceptionDef with
    | SynExceptionSig (sedr, withKeyword, members, range) ->
        mkNodeWithChildren
            SynExceptionSig_
            range
            (sortChildren
                [| yield visitSynExceptionDefnRepr sedr
                   yield! Option.toList (mkNodeOption SynExceptionSig_With withKeyword)
                   yield! List.map visitSynMemberSig members |])

and visitSynLongIdent (lid: SynLongIdent) : TriviaNode =
    mkNodeWithChildren
        SynLongIdent_
        lid.FullRange
        (sortChildren [| yield! List.map visitSynIdent lid.IdentsWithTrivia |])

and visitLongIdentIncludingFullRange (li: LongIdent) : TriviaNode =
    mkNodeWithChildren LongIdent_ (longIdentFullRange li) [| yield! List.map visitIdent li |]

and visitIdent (ident: Ident) : TriviaNode = mkNode Ident_ ident.idRange

and visitSynIdent (si: SynIdent) = mkNode SynIdent_ si.FullRange

and visitOptSynExpr (synExpr: SynExpr option) : TriviaNode list =
    match Option.map visitSynExpr synExpr with
    | None -> []
    | Some xs -> xs

and visitOptSynAccess (synAccess: SynAccess option) : TriviaNode list =
    match synAccess with
    | None -> []
    | Some vis -> [ visitSynAccess vis ]

and visitSynAccess (synAccess: SynAccess) : TriviaNode =
    match synAccess with
    | SynAccess.Private r -> mkNode SynAccess_Private r
    | SynAccess.Internal r -> mkNode SynAccess_Internal r
    | SynAccess.Public r -> mkNode SynAccess_Public r

let astToNode (range: range) (hds: ParsedHashDirective list) (mdls: SynModuleOrNamespace list) : TriviaNode =
    { Range = range
      Type = ParsedInput_
      Children =
        sortChildren
            [| yield! List.map visitSynModuleOrNamespace mdls
               yield! List.map visitParsedHashDirective hds |]
      FSharpASTNode = None }

let sigAstToNode (range: range) (ast: SynModuleOrNamespaceSig list) : TriviaNode =
    { Range = range
      Type = ParsedInput_
      Children = sortChildren [| yield! List.map visitSynModuleOrNamespaceSig ast |]
      FSharpASTNode = None }
