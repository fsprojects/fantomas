module internal Fantomas.Core.AstTransformer

open FSharp.Compiler.Text
open FSharp.Compiler.Syntax
open Fantomas.Core.AstExtensions
open Fantomas.Core.TriviaTypes
open Fantomas.Core.RangePatterns
open Fantomas.Core

let sortChildren =
    Array.sortBy (fun ({ Range = range }: TriviaNodeAssigner) -> range.StartLine, range.StartColumn)

let mkNode (t: FsAstType) (r: range) = TriviaNodeAssigner(t, r)

let mkSynModuleDeclNode (t: FsAstType) (astNode: SynModuleDecl) (r: range) =
    TriviaNodeAssigner(t, r, Choice1Of2 astNode)

let mkSynExprNode (t: FsAstType) (astNode: SynExpr) (r: range) =
    TriviaNodeAssigner(t, r, Choice2Of2 astNode)

let mkNodeOption (t: FsAstType) (r: range option) : TriviaNodeAssigner list = Option.toList r |> List.map (mkNode t)

// We only need the let/type keyword anchor if there are xml docs or attributes present that have space between itself and the let/type keyword
let mkNodeForRangeAfterXmlAndAttributes
    (t: FsAstType)
    (SourceParser.PreXmlDoc (xml, xmlRange))
    (attrs: SynAttributeList list)
    (r: range option)
    : TriviaNodeAssigner list =
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

// TODO: introduce a couple of more parent nodes in trivia types
let rec visitSynModuleOrNamespace (modOrNs: SynModuleOrNamespace) : TriviaNodeAssigner =
    match modOrNs with
    | SynModuleOrNamespace (longIdent, _, kind, decls, _, attrs, _, range, trivia) ->
        let moduleOrNamespace =
            match kind with
            | SynModuleOrNamespaceKind.DeclaredNamespace ->
                [ yield mkNode SynModuleOrNamespace_DeclaredNamespace range
                  yield! mkNodeOption SynModuleOrNamespace_Namespace trivia.NamespaceKeyword ]
            | SynModuleOrNamespaceKind.GlobalNamespace ->
                [ yield mkNode SynModuleOrNamespace_GlobalNamespace range
                  yield! mkNodeOption SynModuleOrNamespace_Namespace trivia.NamespaceKeyword ]
            | SynModuleOrNamespaceKind.NamedModule ->
                [ yield mkNode SynModuleOrNamespace_NamedModule range
                  yield! mkNodeOption SynModuleOrNamespace_Module trivia.ModuleKeyword ]
            | SynModuleOrNamespaceKind.AnonModule -> []

        let longIdentNodes =
            match kind, decls with
            | SynModuleOrNamespaceKind.AnonModule, _ :: _ -> []
            | _ -> visitLongIdentIncludingFullRange longIdent

        [ yield! moduleOrNamespace
          yield! longIdentNodes
          yield! (visitSynAttributeLists attrs)
          yield! (List.collect visitSynModuleDecl decls) ]

and visitSynModuleDecl (ast: SynModuleDecl) : TriviaNodeAssigner list =
    let rec visit
        (ast: SynModuleDecl)
        (finalContinuation: TriviaNodeAssigner list -> TriviaNodeAssigner list)
        : TriviaNodeAssigner list =
        match ast with
        | SynModuleDecl.ModuleAbbrev (_, _, range) ->
            [ mkSynModuleDeclNode SynModuleDecl_ModuleAbbrev ast range ]
            |> finalContinuation
        | SynModuleDecl.NestedModule (SynComponentInfo (xmlDoc = preXmlDoc; attributes = attrs) as sci,
                                      _,
                                      decls,
                                      _,
                                      range,
                                      trivia) ->
            let continuations: ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                decls |> List.map visit

            let moduleNode =
                mkNodeForRangeAfterXmlAndAttributes
                    SynModuleDecl_NestedModule_Module
                    preXmlDoc
                    attrs
                    trivia.ModuleKeyword

            let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                [ mkSynModuleDeclNode SynModuleDecl_NestedModule ast range
                  yield! moduleNode
                  yield! visitSynComponentInfo sci
                  yield! mkNodeOption SynModuleDecl_NestedModule_Equals trivia.EqualsRange
                  yield! (List.collect id nodes) ]
                |> finalContinuation

            Continuation.sequence continuations finalContinuation
        | SynModuleDecl.Let (_, bindings, range) ->
            mkSynModuleDeclNode SynModuleDecl_Let ast range
            :: (bindings |> List.collect visitSynBinding)
            |> finalContinuation
        | SynModuleDecl.Expr (expr, range) ->
            mkSynModuleDeclNode SynModuleDecl_Expr ast range
            :: visitSynExpr expr
            |> finalContinuation
        | SynModuleDecl.Types (typeDefs, range) ->
            mkSynModuleDeclNode SynModuleDecl_Types ast range
            :: (typeDefs |> List.collect visitSynTypeDefn)
            |> finalContinuation
        | SynModuleDecl.Exception (exceptionDef, range) ->
            mkSynModuleDeclNode SynModuleDecl_Exception ast range
            :: (visitSynExceptionDefn exceptionDef)
            |> finalContinuation
        | SynModuleDecl.Open (target, parentRange) ->
            // we use the parent ranges here to match up with the trivia parsed
            match target with
            | SynOpenDeclTarget.ModuleOrNamespace (_, _range) ->
                mkSynModuleDeclNode SynModuleDecl_Open ast parentRange
                |> List.singleton
                |> finalContinuation
            | SynOpenDeclTarget.Type (synType, _range) ->
                mkSynModuleDeclNode SynModuleDecl_OpenType ast parentRange
                :: (visitSynType synType)
                |> finalContinuation
        | SynModuleDecl.Attributes (attrs, range) ->
            mkSynModuleDeclNode SynModuleDecl_Attributes ast range
            :: (visitSynAttributeLists attrs)
            |> finalContinuation
        | SynModuleDecl.HashDirective (hash, range) ->
            [ mkSynModuleDeclNode SynModuleDecl_HashDirective ast range
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
                [ yield mkSynExprNode SynExpr_Paren synExpr range
                  yield mkNode SynExpr_Paren_OpeningParenthesis lpr
                  yield! nodes
                  yield! mkNodeOption SynExpr_Paren_ClosingParenthesis rpr ]
                |> finalContinuation)
        | SynExpr.Quote (operator, _, quotedSynExpr, _, range) ->
            let continuations: ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                [ visit operator; visit quotedSynExpr ]

            let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                mkSynExprNode SynExpr_Quote synExpr range
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
                mkSynExprNode SynExpr_Tuple synExpr range
                :: (List.collect id nodes)
                |> finalContinuation

            Continuation.sequence continuations finalContinuation
        | SourceParser.ArrayOrList (startRange, _isArray, exprs, endRange, range) ->
            let continuations: ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                exprs |> List.map visit

            let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                [ yield mkSynExprNode SynExpr_ArrayOrList synExpr range
                  yield mkNode SynExpr_ArrayOrList_OpeningDelimiter startRange
                  yield! List.collect id nodes
                  yield mkNode SynExpr_ArrayOrList_ClosingDelimiter endRange ]
                |> finalContinuation

            Continuation.sequence continuations finalContinuation
        // Captured above
        | SynExpr.ArrayOrList _
        | SynExpr.ArrayOrListComputed _ -> []
        | SynExpr.Record (_, _, recordFields, StartEndRange 1 (openingBrace, range, closingBrace)) ->
            [ yield mkSynExprNode SynExpr_Record synExpr range
              yield mkNode SynExpr_Record_OpeningBrace openingBrace
              yield! List.collect visitRecordField recordFields
              yield mkNode SynExpr_Record_ClosingBrace closingBrace ]
            |> finalContinuation
        | SynExpr.AnonRecd (_, _, recordFields, range) ->
            mkSynExprNode SynExpr_AnonRecd synExpr range
            :: (List.collect visitAnonRecordField recordFields)
            |> finalContinuation
        | SynExpr.New (_, typeName, expr, range) ->
            visit expr (fun nodes ->
                [ yield mkSynExprNode SynExpr_New synExpr range
                  yield! nodes
                  yield! visitSynType typeName ]
                |> finalContinuation)
        | SynExpr.ObjExpr (objType, argOptions, withRange, bindings, members, extraImpls, _, range) ->
            [ yield mkSynExprNode SynExpr_ObjExpr synExpr range
              yield! visitSynType objType
              if argOptions.IsSome then
                  yield! visitArgsOption argOptions.Value
              yield! mkNodeOption SynExpr_ObjExpr_With withRange
              yield! bindings |> List.collect visitSynBinding
              yield! members |> List.collect visitSynMemberDefn
              yield! extraImpls |> List.collect visitSynInterfaceImpl ]
            |> finalContinuation
        | SynExpr.While (_, whileExpr, doExpr, range) ->
            let continuations: ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                [ visit whileExpr; visit doExpr ]

            let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                mkSynExprNode SynExpr_While synExpr range
                :: (List.collect id nodes)
                |> finalContinuation

            Continuation.sequence continuations finalContinuation
        | SynExpr.For (_, _, _, equalsRange, identBody, _, toBody, doBody, range) ->
            let continuations: ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                [ visit identBody
                  visit toBody
                  visit doBody ]

            let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                [ yield mkSynExprNode SynExpr_For synExpr range
                  yield! mkNodeOption SynExpr_For_Equals equalsRange
                  yield! (List.collect id nodes) ]
                |> finalContinuation

            Continuation.sequence continuations finalContinuation
        | SynExpr.ForEach (_, _, SeqExprOnly _, _, pat, enumExpr, bodyExpr, range) ->
            let continuations: ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                [ visit enumExpr; visit bodyExpr ]

            let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                [ yield mkSynExprNode SynExpr_ForEach synExpr range
                  yield! visitSynPat pat
                  yield! (List.collect id nodes) ]
                |> finalContinuation

            Continuation.sequence continuations finalContinuation
        | SynExpr.ComputationExpr (_, expr, StartEndRange 1 (openingBrace, _, closingBrace)) ->
            [ yield mkNode SynExpr_ComputationExpr_OpeningBrace openingBrace
              yield! visit expr finalContinuation
              yield mkNode SynExpr_ComputationExpr_ClosingBrace closingBrace ]
        | SynExpr.Lambda (_, _, args, body, _parsedData, range, { ArrowRange = arrowRange }) ->
            visit body (fun nodes ->
                [ yield mkSynExprNode SynExpr_Lambda synExpr range
                  yield! visitSynSimplePats args
                  yield! mkNodeOption SynExpr_Lambda_Arrow arrowRange
                  yield! nodes ]
                |> finalContinuation)
        | SynExpr.MatchLambda (_, keywordRange, matchClauses, _, range) ->
            mkSynExprNode SynExpr_MatchLambda synExpr range
            :: mkNode SynExpr_MatchLambda_Function keywordRange
               :: (List.collect visitSynMatchClause matchClauses)
            |> finalContinuation
        | SynExpr.Match (_, expr, clauses, range, trivia) ->
            visit expr (fun nodes ->
                [ yield mkSynExprNode SynExpr_Match synExpr range
                  yield mkNode SynExpr_Match_Match trivia.MatchKeyword
                  yield! nodes
                  yield mkNode SynExpr_Match_With trivia.WithKeyword
                  yield! (List.collect visitSynMatchClause clauses) ]
                |> finalContinuation)
        | SynExpr.Do (expr, StartRange 2 (doKeyword, range)) ->
            visit expr (fun nodes ->
                [ yield mkSynExprNode SynExpr_Do synExpr range
                  yield mkNode SynExpr_Do_Do doKeyword
                  yield! nodes ]
                |> finalContinuation)
        | SynExpr.Assert (expr, StartRange 6 (assertKeyword, range)) ->
            visit expr (fun nodes ->
                [ yield mkSynExprNode SynExpr_Assert synExpr range
                  yield mkNode SynExpr_Assert_Assert assertKeyword
                  yield! nodes ]
                |> finalContinuation)
        | SourceParser.InfixApp (_, sli, e1, e2, range) ->
            let continuations: ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                [ visit e1; visit e2 ]

            let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                [ yield mkSynExprNode SynExpr_App synExpr range
                  yield! visitSynLongIdent sli
                  yield! List.collect id nodes ]
                |> finalContinuation

            Continuation.sequence continuations finalContinuation
        | SynExpr.App (_, _, funcExpr, argExpr, range) ->
            let continuations: ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                [ visit funcExpr; visit argExpr ]

            let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                mkSynExprNode SynExpr_App synExpr range
                :: (List.collect id nodes)
                |> finalContinuation

            Continuation.sequence continuations finalContinuation
        | SynExpr.TypeApp (expr, lessRange, typeNames, _, greaterRange, _, range) ->
            visit expr (fun nodes ->
                [ yield mkSynExprNode SynExpr_TypeApp synExpr range
                  yield mkNode SynExpr_TypeApp_Less lessRange
                  yield! nodes
                  yield! (List.collect visitSynType typeNames)
                  yield! mkNodeOption SynExpr_TypeApp_Greater greaterRange ]
                |> finalContinuation)
        | SynExpr.LetOrUse (_, _, bindings, body, _, trivia) ->
            visit body (fun nodes ->
                [ yield! (List.collect visitSynBinding bindings)
                  yield! mkNodeOption SynExpr_LetOrUse_In trivia.InKeyword
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
                [ yield mkSynExprNode SynExpr_TryWith synExpr range
                  yield mkNode SynExpr_TryWith_Try tryKeyword
                  yield! nodes
                  yield mkNode SynExpr_TryWith_With withKeyword
                  yield! withCases |> List.collect visitSynMatchClause ]
                |> finalContinuation)
        | SynExpr.TryFinally (tryExpr, finallyExpr, range, _, _, trivia) ->
            let continuations: ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                [ visit tryExpr; visit finallyExpr ]

            let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                [ yield mkSynExprNode SynExpr_TryFinally synExpr range
                  yield mkNode SynExpr_TryFinally_Try trivia.TryKeyword
                  yield mkNode SynExpr_TryFinally_Finally trivia.FinallyKeyword
                  yield! List.collect id nodes ]
                |> finalContinuation

            Continuation.sequence continuations finalContinuation
        | SynExpr.Lazy (ex, StartRange 4 (lazyKeyword, range)) ->
            visit ex (fun nodes ->
                [ yield mkSynExprNode SynExpr_Lazy synExpr range
                  yield mkNode SynExpr_Lazy_Lazy lazyKeyword
                  yield! nodes ]
                |> finalContinuation)
        | SynExpr.Sequential (_, _, expr1, expr2, _) ->
            visit expr2 (fun nodes1 -> visit expr1 (fun nodes2 -> nodes1 @ nodes2 |> finalContinuation))
        | SynExpr.SequentialOrImplicitYield (_, expr1, expr2, ifNotStmt, range) ->
            let continuations: ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                [ visit expr1
                  visit expr2
                  visit ifNotStmt ]

            let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                mkSynExprNode SynExpr_SequentialOrImplicitYield synExpr range
                :: (List.collect id nodes)
                |> finalContinuation

            Continuation.sequence continuations finalContinuation
        // don't collect nested elif expression as nodes.
        // the ranges are often incorrect and using the else if or elif token is more reliable.
        | SourceParser.ElIf ((_leadingElseKw, ifKw, isElif, ifExpr, thenKw, thenExpr) :: es, (elseKw, elseExpr), range) ->
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
                    mkNodeOption SynExpr_IfThenElse_Else elseKw

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

                [ yield mkSynExprNode SynExpr_IfThenElse synExpr range
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

        | SynExpr.IfThenElse (ifExpr, thenExpr, elseExpr, _, _, range, trivia) ->
            let continuations: ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                [ yield visit ifExpr
                  yield visit thenExpr
                  yield! (Option.toList elseExpr |> List.map visit) ]

            let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                [ yield mkSynExprNode SynExpr_IfThenElse synExpr range
                  yield
                      mkNode
                          (if trivia.IsElif then
                               SynExpr_IfThenElse_Elif
                           else
                               SynExpr_IfThenElse_If)
                          trivia.IfKeyword
                  yield mkNode SynExpr_IfThenElse_Then trivia.ThenKeyword
                  yield! mkNodeOption SynExpr_IfThenElse_Else trivia.ElseKeyword
                  yield! (List.collect id nodes) ]
                |> finalContinuation

            Continuation.sequence continuations finalContinuation
        | SynExpr.Ident id ->
            mkSynExprNode SynExpr_Ident synExpr id.idRange
            |> List.singleton
            |> finalContinuation
        | SynExpr.LongIdent (_, longDotId, _, range) ->
            mkSynExprNode SynExpr_LongIdent synExpr range
            :: (visitSynLongIdent longDotId)
            |> finalContinuation
        | SynExpr.LongIdentSet (_, expr, range) ->
            visit expr (fun nodes ->
                mkSynExprNode SynExpr_LongIdentSet synExpr range
                :: nodes
                |> finalContinuation)
        | SynExpr.DotGet (expr, _, longDotId, range) ->
            visit expr (fun nodes ->
                [ yield mkSynExprNode SynExpr_DotGet synExpr range
                  yield! nodes
                  // Idents are collected as children here to deal with unit test ``Fluent api with comments should remain on same lines``
                  yield! (visitSynLongIdent longDotId) ]
                |> finalContinuation)
        | SynExpr.DotSet (expr, _, e2, range) ->
            let continuations: ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                [ visit expr; visit e2 ]

            let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                mkSynExprNode SynExpr_DotSet synExpr range
                :: (List.collect id nodes)
                |> finalContinuation

            Continuation.sequence continuations finalContinuation
        | SynExpr.Set (e1, e2, range) ->
            let continuations: ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                [ visit e1; visit e2 ]

            let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                mkSynExprNode SynExpr_Set synExpr range
                :: (List.collect id nodes)
                |> finalContinuation

            Continuation.sequence continuations finalContinuation
        | SynExpr.DotIndexedGet (objectExpr, indexArgs, _, range) ->
            visit objectExpr (fun nodes ->
                [ yield mkSynExprNode SynExpr_DotIndexedGet synExpr range
                  yield! nodes
                  yield! visitSynExpr indexArgs ]
                |> finalContinuation)
        | SynExpr.DotIndexedSet (objectExpr, indexArgs, valueExpr, _, _, range) ->
            let continuations: ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                [ visit objectExpr; visit valueExpr ]

            let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                [ yield mkSynExprNode SynExpr_DotIndexedSet synExpr range
                  yield! (List.collect id nodes)
                  yield! visitSynExpr indexArgs ]
                |> finalContinuation

            Continuation.sequence continuations finalContinuation
        | SynExpr.NamedIndexedPropertySet (_, e1, e2, range) ->
            let continuations: ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                [ visit e1; visit e2 ]

            let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                mkSynExprNode SynExpr_NamedIndexedPropertySet synExpr range
                :: (List.collect id nodes)
                |> finalContinuation

            Continuation.sequence continuations finalContinuation
        | SynExpr.DotNamedIndexedPropertySet (expr, _, e1, e2, range) ->
            let continuations: ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                [ visit expr; visit e1; visit e2 ]

            let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                mkSynExprNode SynExpr_DotNamedIndexedPropertySet synExpr range
                :: (List.collect id nodes)
                |> finalContinuation

            Continuation.sequence continuations finalContinuation
        | SynExpr.TypeTest (expr, typeName, range) ->
            visit expr (fun nodes ->
                [ yield mkSynExprNode SynExpr_TypeTest synExpr range
                  yield! nodes
                  yield! visitSynType typeName ]
                |> finalContinuation)
        | SynExpr.Upcast (expr, typeName, range) ->
            visit expr (fun nodes ->
                [ yield mkSynExprNode SynExpr_Upcast synExpr range
                  yield! nodes
                  yield! visitSynType typeName ]
                |> finalContinuation)
        | SynExpr.Downcast (expr, typeName, range) ->
            visit expr (fun nodes ->
                [ yield mkSynExprNode SynExpr_Downcast synExpr range
                  yield! nodes
                  yield! visitSynType typeName ]
                |> finalContinuation)
        | SynExpr.InferredUpcast (expr, StartRange 6 (upcastKeyword, range)) ->
            visit expr (fun nodes ->
                [ yield mkSynExprNode SynExpr_InferredUpcast synExpr range
                  yield mkNode SynExpr_InferredUpcast_Upcast upcastKeyword
                  yield! nodes ]
                |> finalContinuation)
        | SynExpr.InferredDowncast (expr, StartRange 8 (downcastKeyword, range)) ->
            visit expr (fun nodes ->
                [ yield mkSynExprNode SynExpr_InferredDowncast synExpr range
                  yield mkNode SynExpr_InferredDowncast_Downcast downcastKeyword
                  yield! nodes ]
                |> finalContinuation)
        | SynExpr.Null range ->
            mkSynExprNode SynExpr_Null synExpr range
            |> List.singleton
            |> finalContinuation
        | SynExpr.AddressOf (isByRef, expr, _, range) ->
            let ampersandRange, ampersandType =
                if isByRef then
                    RangeHelpers.mkStartRange 1 range, SynExpr_AddressOf_SingleAmpersand
                else
                    RangeHelpers.mkStartRange 2 range, SynExpr_AddressOf_DoubleAmpersand

            visit expr (fun nodes ->
                [ yield mkSynExprNode SynExpr_AddressOf synExpr range
                  yield mkNode ampersandType ampersandRange
                  yield! nodes ]
                |> finalContinuation)
        | SynExpr.TraitCall (_typars, sign, expr, range) ->
            visit expr (fun nodes ->
                [ yield mkSynExprNode SynExpr_TraitCall synExpr range
                  yield! visitSynMemberSig sign
                  yield! nodes ]
                |> finalContinuation)
        | SynExpr.JoinIn (expr, _, expr2, range) ->
            let continuations: ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                [ visit expr; visit expr2 ]

            let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                mkSynExprNode SynExpr_JoinIn synExpr range
                :: (List.collect id nodes)
                |> finalContinuation

            Continuation.sequence continuations finalContinuation
        | SynExpr.ImplicitZero range ->
            mkSynExprNode SynExpr_ImplicitZero synExpr range
            |> List.singleton
            |> finalContinuation
        | SynExpr.YieldOrReturn ((isYield, _), expr, range) ->
            let keywordType, keywordRange =
                if isYield then
                    SynExpr_YieldOrReturn_Yield, RangeHelpers.mkStartRange 5 range
                else
                    SynExpr_YieldOrReturn_Return, RangeHelpers.mkStartRange 6 range

            visit expr (fun nodes ->
                [ yield mkSynExprNode SynExpr_YieldOrReturn synExpr range
                  yield mkNode keywordType keywordRange
                  yield! nodes ]
                |> finalContinuation)
        | SynExpr.YieldOrReturnFrom ((isYield, _), expr, range) ->
            let keywordType, keywordRange =
                if isYield then
                    SynExpr_YieldOrReturnFrom_YieldBang, RangeHelpers.mkStartRange 6 range
                else
                    SynExpr_YieldOrReturnFrom_ReturnBang, RangeHelpers.mkStartRange 7 range

            visit expr (fun nodes ->
                [ yield mkSynExprNode SynExpr_YieldOrReturnFrom synExpr range
                  yield mkNode keywordType keywordRange
                  yield! nodes ]
                |> finalContinuation)
        | SynExpr.LetOrUseBang (_, _, _, pat, rhsExpr, andBangs, body, range, trivia) ->
            let continuations: ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                [ yield visit rhsExpr
                  yield visit body
                  yield (fun continuation -> continuation (List.collect visitSynExprAndBang andBangs)) ]

            let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                [ yield mkSynExprNode SynExpr_LetOrUseBang synExpr range
                  yield! visitSynPat pat
                  yield! mkNodeOption SynExpr_LetOrUseBang_Equals trivia.EqualsRange
                  yield! (List.collect id nodes) ]
                |> finalContinuation

            Continuation.sequence continuations finalContinuation
        | SynExpr.MatchBang (_, expr, clauses, range, trivia) ->
            visit expr (fun nodes ->
                [ yield mkSynExprNode SynExpr_MatchBang synExpr range
                  yield mkNode SynExpr_MatchBang_Match trivia.MatchBangKeyword
                  yield! nodes
                  yield mkNode SynExpr_MatchBang_With trivia.WithKeyword
                  yield! clauses |> List.collect visitSynMatchClause ]
                |> finalContinuation)
        | SynExpr.DoBang (expr, StartRange 3 (doBangKeyword, range)) ->
            visit expr (fun nodes ->
                [ yield mkSynExprNode SynExpr_DoBang synExpr range
                  yield mkNode SynExpr_DoBang_DoBang doBangKeyword
                  yield! nodes ]
                |> finalContinuation)
        | SynExpr.LibraryOnlyILAssembly (_, _, _, _, range) ->
            mkSynExprNode SynExpr_LibraryOnlyILAssembly synExpr range
            |> List.singleton
            |> finalContinuation
        | SynExpr.LibraryOnlyStaticOptimization (_, _, _, range) ->
            mkSynExprNode SynExpr_LibraryOnlyStaticOptimization synExpr range
            |> List.singleton
            |> finalContinuation
        | SynExpr.LibraryOnlyUnionCaseFieldGet (expr, _, _, range) ->
            visit expr (fun nodes ->
                mkSynExprNode SynExpr_LibraryOnlyUnionCaseFieldGet synExpr range
                :: nodes
                |> finalContinuation)
        | SynExpr.LibraryOnlyUnionCaseFieldSet (e1, _, _, e2, range) ->
            let continuations: ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                [ visit e1; visit e2 ]

            let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                mkSynExprNode SynExpr_LibraryOnlyUnionCaseFieldSet synExpr range
                :: (List.collect id nodes)
                |> finalContinuation

            Continuation.sequence continuations finalContinuation
        | SynExpr.ArbitraryAfterError (_, range) ->
            mkSynExprNode SynExpr_ArbitraryAfterError synExpr range
            |> List.singleton
            |> finalContinuation
        | SynExpr.FromParseError (expr, range) ->
            visit expr (fun nodes ->
                mkSynExprNode SynExpr_FromParseError synExpr range
                :: nodes
                |> finalContinuation)
        | SynExpr.DiscardAfterMissingQualificationAfterDot (expr, range) ->
            visit expr (fun nodes ->
                mkSynExprNode SynExpr_DiscardAfterMissingQualificationAfterDot synExpr range
                :: nodes
                |> finalContinuation)
        | SynExpr.Fixed (expr, StartRange 5 (fixedKeyword, range)) ->
            visit expr (fun nodes ->
                [ yield mkSynExprNode SynExpr_Fixed synExpr range
                  yield mkNode SynExpr_Fixed_Fixed fixedKeyword
                  yield! nodes ]
                |> finalContinuation)
        | SynExpr.InterpolatedString (parts, _, range) ->
            mkSynExprNode SynExpr_InterpolatedString synExpr range
            :: (List.collect visitSynInterpolatedStringPart parts)
            |> finalContinuation
        | SynExpr.IndexRange (e1, _, e2, _, _, range) ->
            [ yield mkSynExprNode SynExpr_IndexRange synExpr range
              yield! (e1 |> Option.toList |> List.collect visitSynExpr)
              yield! (e2 |> Option.toList |> List.collect visitSynExpr) ]
            |> finalContinuation
        | SynExpr.IndexFromEnd (e, range) ->
            [ yield mkSynExprNode SynExpr_IndexFromEnd synExpr range
              yield! visitSynExpr e ]
        | SynExpr.DebugPoint _ -> []
        | SynExpr.Dynamic (funcExpr, _, argExpr, range) ->
            let continuations: ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                [ visit funcExpr; visit argExpr ]

            let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                mkSynExprNode SynExpr_Dynamic synExpr range
                :: (List.collect id nodes)
                |> finalContinuation

            Continuation.sequence continuations finalContinuation

    visit synExpr id

and visitSynInterpolatedStringPart (synInterpolatedStringPart: SynInterpolatedStringPart) =
    match synInterpolatedStringPart with
    | SynInterpolatedStringPart.String (_, range) ->
        mkNode SynInterpolatedStringPart_String range
        |> List.singleton
    | SynInterpolatedStringPart.FillExpr (expr, ident) ->
        visitSynExpr expr
        @ (Option.toList ident |> List.map visitIdent)

and visitRecordField (SynExprRecordField ((fieldName, _), equalsRange, synExprOption, _blockSeparator) as rf) =
    [ yield mkNode SynExprRecordField_ rf.FullRange
      yield! visitSynLongIdent fieldName
      yield! mkNodeOption SynExprRecordField_Equals equalsRange
      yield!
          (match synExprOption with
           | Some e -> visitSynExpr e
           | None -> []) ]

and visitAnonRecordField (ident: Ident, equalsRange: range option, expr: SynExpr) =
    [ yield visitIdent ident
      yield! mkNodeOption SynExpr_AnonRecd_Field_Equals equalsRange
      yield! visitSynExpr expr ]

and visitAnonRecordTypeField (_: Ident, t: SynType) = visitSynType t

and visitSynMemberSig (ms: SynMemberSig) : TriviaNodeAssigner list =
    match ms with
    | SynMemberSig.Member (valSig, mf, range) ->
        [ yield mkNode SynMemberSig_Member range
          yield! visitSynMemberFlags mf
          yield! visitSynValSig valSig ]
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
    | SourceParser.Clause (barRange, pat, eo, arrowRange, e, range) ->
        [ yield mkNode SynMatchClause_ range
          yield! visitSynPat pat
          yield! mkNodeOption SynMatchClause_Bar barRange
          if eo.IsSome then
              yield! visitSynExpr eo.Value
          yield! mkNodeOption SynMatchClause_Arrow arrowRange
          yield! visitSynExpr e ]

and visitSynExprAndBang (SynExprAndBang (_, _, _, pat, body, range, trivia)) : TriviaNodeAssigner list =
    [ yield mkNode SynExprAndBang_ range
      yield! visitSynPat pat
      yield mkNode SynExprAndBang_Equals trivia.EqualsRange
      yield! visitSynExpr body ]

and visitArgsOption (expr: SynExpr, _: Ident option) = visitSynExpr expr

and visitSynInterfaceImpl (ii: SynInterfaceImpl) : TriviaNodeAssigner list =
    match ii with
    | SynInterfaceImpl (typ, withKeyword, bindings, members, range) ->
        [ yield mkNode SynInterfaceImpl_ range
          yield! mkNodeOption SynInterfaceImpl_With withKeyword
          yield! visitSynType typ
          yield! (bindings |> List.collect visitSynBinding)
          yield! (members |> List.collect visitSynMemberDefn) ]

and visitSynTypeDefn (td: SynTypeDefn) =
    match td with
    | SynTypeDefn (SynComponentInfo (xmlDoc = preXmlDoc; attributes = attrs) as sci,
                   stdr,
                   members,
                   _implicitConstructor,
                   range,
                   trivia) ->
        let typeKeyword =
            mkNodeForRangeAfterXmlAndAttributes SynTypeDefn_Type preXmlDoc attrs trivia.TypeKeyword

        [ yield mkNode SynTypeDefn_ range
          yield! typeKeyword
          yield! mkNodeOption SynTypeDefn_Equals trivia.EqualsRange
          yield! visitSynComponentInfo sci
          yield! visitSynTypeDefnRepr stdr
          yield! mkNodeOption SynTypeDefn_With trivia.WithKeyword
          yield! (members |> List.collect visitSynMemberDefn) ]

and visitSynTypeDefnSig (typeDefSig: SynTypeDefnSig) : TriviaNodeAssigner list =
    match typeDefSig with
    | SynTypeDefnSig (sci, equalsRange, synTypeDefnSigReprs, withRange, memberSig, _) ->
        [ yield mkNode SynTypeDefnSig_ typeDefSig.Range
          yield! visitSynComponentInfo sci
          yield! mkNodeOption SynTypeDefnSig_Equals equalsRange
          yield! visitSynTypeDefnSigRepr synTypeDefnSigReprs
          yield! mkNodeOption SynTypeDefnSig_With withRange
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
    | SynMemberDefn.Interface (typ, withKeyword, members, range) ->
        [ yield mkNode SynMemberDefn_Interface range
          yield! mkNodeOption SynMemberDefn_Interface_With withKeyword
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
    | SynMemberDefn.AutoProperty (attrs, _, _, typeOpt, _, _, _, _, equalsRange, synExpr, withKeyword, _, range) ->
        [ yield mkNode SynMemberDefn_AutoProperty range
          yield mkNode SynMemberDefn_AutoProperty_Equals equalsRange
          yield! mkNodeOption SynMemberDefn_AutoProperty_With withKeyword
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
    | SynBinding (_, kind, _, _, attrs, preXml, valData, headPat, returnInfo, expr, _range, _, trivia) ->
        let t =
            match kind with
            | SynBindingKind.StandaloneExpression -> SynBindingKind_StandaloneExpression
            | SynBindingKind.Normal -> SynBindingKind_Normal
            | SynBindingKind.Do -> SynBindingKind_Do

        let headPatNodes =
            match kind with
            | SynBindingKind.Do -> []
            | _ -> visitSynPat headPat

        let letNode =
            mkNodeForRangeAfterXmlAndAttributes SynBinding_Let preXml attrs trivia.LetKeyword

        [ yield mkNode t binding.RangeOfBindingWithRhs
          yield! visitSynAttributeLists attrs
          yield! letNode
          yield! visitSynValData valData
          yield! headPatNodes
          yield!
              (match returnInfo with
               | Some ri -> visitSynBindingReturnInfo ri
               | None -> [])
          yield! mkNodeOption SynBinding_Equals trivia.EqualsRange
          yield! visitSynExpr expr ]

and visitSynValData (svd: SynValData) : TriviaNodeAssigner list =
    match svd with
    | SynValData (memberFlags, svi, _) ->
        let flagNodes =
            match memberFlags with
            | Some mf -> visitSynMemberFlags mf
            | None -> []

        [ yield! flagNodes
          yield! visitSynValInfo svi ]

and visitSynMemberFlags (memberFlags: SynMemberFlags) : TriviaNodeAssigner list =
    [ yield! mkNodeOption SynValData_Static memberFlags.Trivia.StaticRange
      yield! mkNodeOption SynValData_Member memberFlags.Trivia.MemberRange ]

and visitSynValSig (svs: SynValSig) : TriviaNodeAssigner list =
    match svs with
    | SynValSig (attrs, ident, explicitValDecls, synType, arity, _, _, _, _, expr, range, trivia) ->
        [ yield mkNode SynValSig_ range
          yield! mkNodeOption SynValSig_Val trivia.ValKeyword
          yield visitSynIdent ident
          yield! (visitSynAttributeLists attrs)
          yield! visitSynValTyparDecls explicitValDecls
          yield! visitSynType synType
          yield! visitSynValInfo arity
          if expr.IsSome then
              yield! visitSynExpr expr.Value
          yield! mkNodeOption SynValSig_With trivia.ValKeyword ]

and visitSynValTyparDecls (valTypeDecl: SynValTyparDecls) : TriviaNodeAssigner list =
    match valTypeDecl with
    | SynValTyparDecls (Some typardecl, _) -> visitSynTyparDecls typardecl
    | _ -> []

and visitSynTyparDecl (std: SynTyparDecl) : TriviaNodeAssigner list =
    match std with
    | SynTyparDecl (attrs, synTypar) ->
        [ yield! (visitSynAttributeLists attrs)
          yield visitSynTypar synTypar ]

and visitSynTypar (SynTypar (ident, _typarStaticReq, _isCompGen)) = mkNode Ident_ ident.idRange

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
        | SynPat.Named (si, _, _, range) ->
            [ mkNode SynPat_Named range
              visitSynIdent si ]
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
        | SynPat.Or (synPat, synPat2, _range, trivia) ->
            let continuations: ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                [ visit synPat; visit synPat2 ]

            let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                [ yield! List.collect id nodes
                  yield mkNode SynPat_Or_Bar trivia.BarRange ]
                |> finalContinuation

            Continuation.sequence continuations finalContinuation
        | SynPat.Ands (pats, range) ->
            let continuations: ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                pats |> List.map visit

            let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                mkNode SynPat_Ands range
                :: (List.collect id nodes)
                |> finalContinuation

            Continuation.sequence continuations finalContinuation
        | SynPat.LongIdent (_, propertyKeyword, _, svtd, ctorArgs, _, range) ->
            let propertyNodes =
                match propertyKeyword with
                | Some (PropertyKeyword.And r) -> [ mkNode SynPat_LongIdent_And r ]
                | Some (PropertyKeyword.With r) -> [ mkNode SynPat_LongIdent_With r ]
                | None -> []

            [ yield mkNode SynPat_LongIdent range
              yield! propertyNodes
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
                pats |> List.map (fun (_, _, pat) -> visit pat)

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
        :: (List.collect
                (fun (_, equalsRange, pat) ->
                    mkNode SynArgPats_NamePatPairs_Equals equalsRange
                    :: visitSynPat pat)
                pats)

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
    | SynTypeDefnKind.Union
    | SynTypeDefnKind.IL -> []
    | SynTypeDefnKind.Augmentation withRange -> [ mkNode SynTypeDefnKind_Augmentation_With withRange ]

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
    | SynExceptionDefn (sedr, withKeyword, members, range) ->
        [ yield mkNode SynExceptionDefn_ range
          yield! visitSynExceptionDefnRepr sedr
          yield! mkNodeOption SynExceptionDefn_With withKeyword
          yield! (members |> List.collect visitSynMemberDefn) ]

and visitSynExceptionDefnRepr (sedr: SynExceptionDefnRepr) : TriviaNodeAssigner list =
    match sedr with
    | SynExceptionDefnRepr (attrs, unionCase, _, _, _, range) ->
        [ yield mkNode SynExceptionDefnRepr_ range
          yield! (visitSynAttributeLists attrs)
          yield! visitSynUnionCase unionCase ]

and visitSynAttribute (attr: SynAttribute) : TriviaNodeAssigner list =
    mkNode SynAttribute_ attr.Range
    :: (visitSynExpr attr.ArgExpr)

and visitSynAttributeLists (attrs: SynAttributeList list) : TriviaNodeAssigner list =
    List.collect visitSynAttributeList attrs

and visitSynAttributeList (attrs: SynAttributeList) : TriviaNodeAssigner list =
    mkNode SynAttributeList_ attrs.Range
    :: (List.collect visitSynAttribute attrs.Attributes)

and visitSynUnionCase (uc: SynUnionCase) : TriviaNodeAssigner list =
    match uc with
    | SourceParser.UnionCase (attrs, _, barRange, _, synIdent, uct, range) ->
        [ yield mkNode SynUnionCase_ range
          yield! mkNodeOption SynUnionCase_Bar barRange
          yield visitSynIdent synIdent
          yield! visitSynUnionCaseType uct
          yield! (visitSynAttributeLists attrs) ]

and visitSynUnionCaseType (uct: SynUnionCaseKind) =
    match uct with
    | SynUnionCaseKind.Fields fields -> List.collect visitSynField fields
    | SynUnionCaseKind.FullType (stype, valInfo) -> visitSynType stype @ visitSynValInfo valInfo

and visitSynEnumCase (sec: SynEnumCase) : TriviaNodeAssigner list =
    match sec with
    | SourceParser.EnumCase (attrs, barRange, _, synIdent, equalsRange, value, valueRange, range) ->
        [ yield! mkNodeOption SynEnumCase_Bar barRange
          yield mkNode SynEnumCase_ range
          yield! (visitSynAttributeLists attrs)
          yield visitSynIdent synIdent
          yield mkNode SynEnumCase_Equals equalsRange
          yield! visitSynConst valueRange value ]

and visitSynField (sfield: SynField) : TriviaNodeAssigner list =
    match sfield with
    | SynField (attrs, _, ident, typ, _, preXmlDoc, _, range) ->
        let innerNode =
            match ident with
            | None -> []
            | Some ident ->
                Range.unionRanges ident.idRange typ.Range
                |> Some
                |> mkNodeForRangeAfterXmlAndAttributes SynField_IdentifierAndType preXmlDoc attrs

        [ yield mkNode SynField_ range
          yield! (visitSynAttributeLists attrs)
          yield! innerNode
          yield! visitSynType typ ]

and visitSynType (st: SynType) =
    let rec visit
        (st: SynType)
        (finalContinuation: TriviaNodeAssigner list -> TriviaNodeAssigner list)
        : TriviaNodeAssigner list =
        match st with
        | SynType.LongIdent li -> visitSynLongIdent li |> finalContinuation
        | SynType.App (typeName, lessRange, typeArgs, _, greaterRange, _, range) ->
            let continuations: ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                [ yield! (List.map visit typeArgs)
                  yield visit typeName ]

            let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                [ yield mkNode SynType_App range
                  yield! mkNodeOption SynType_App_Less lessRange
                  yield! List.collect id nodes
                  yield! mkNodeOption SynType_App_Greater greaterRange ]
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
        | SynType.Fun (argType, returnType, _range) ->
            let continuations: ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                [ visit argType; visit returnType ]

            let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                List.collect id nodes |> finalContinuation

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
    | SynModuleOrNamespaceSig (longIdent, _, kind, decls, _, attrs, _, range, trivia) ->
        let longIdentNodes =
            match kind, decls with
            | SynModuleOrNamespaceKind.AnonModule, _ :: _ -> []
            | _ -> visitLongIdentIncludingFullRange longIdent

        let moduleOrNamespaceSig =
            match kind with
            | SynModuleOrNamespaceKind.DeclaredNamespace ->
                [ yield mkNode SynModuleOrNamespaceSig_DeclaredNamespace range
                  yield! mkNodeOption SynModuleOrNamespace_Namespace trivia.NamespaceKeyword ]
            | SynModuleOrNamespaceKind.GlobalNamespace ->
                [ mkNode SynModuleOrNamespaceSig_GlobalNamespace range
                  yield! mkNodeOption SynModuleOrNamespace_Namespace trivia.NamespaceKeyword ]
            | SynModuleOrNamespaceKind.NamedModule ->
                [ mkNode SynModuleOrNamespaceSig_NamedModule range
                  yield! mkNodeOption SynModuleOrNamespace_Module trivia.ModuleKeyword ]
            | _ -> []

        [ yield! moduleOrNamespaceSig
          yield! longIdentNodes
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
        | SynModuleSigDecl.NestedModule (SynComponentInfo (xmlDoc = preXmlDoc; attributes = attrs) as sci,
                                         _,
                                         decls,
                                         range,
                                         trivia) ->
            let continuations: ((TriviaNodeAssigner list -> TriviaNodeAssigner list) -> TriviaNodeAssigner list) list =
                List.map visit decls

            let moduleKeyword =
                mkNodeForRangeAfterXmlAndAttributes
                    SynModuleSigDecl_NestedModule_Module
                    preXmlDoc
                    attrs
                    trivia.ModuleKeyword

            let finalContinuation (nodes: TriviaNodeAssigner list list) : TriviaNodeAssigner list =
                [ yield mkNode SynModuleSigDecl_NestedModule range
                  yield! moduleKeyword
                  yield! visitSynComponentInfo sci
                  yield! mkNodeOption SynModuleSigDecl_NestedModule_Equals trivia.EqualsRange
                  yield! (List.collect id nodes) ]
                |> finalContinuation

            Continuation.sequence continuations finalContinuation
        | SynModuleSigDecl.Val (SynValSig.SynValSig _ as node, range) ->
            [ yield mkNode SynModuleSigDecl_Val range
              yield! visitSynValSig node ]
            |> finalContinuation
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
        | SynModuleSigDecl.Exception (synExceptionSig, range) ->
            mkNode SynModuleSigDecl_Exception range
            :: (visitSynExceptionSig synExceptionSig)
            |> finalContinuation

    visit ast id

and visitSynExceptionSig (exceptionDef: SynExceptionSig) : TriviaNodeAssigner list =
    match exceptionDef with
    | SynExceptionSig (sedr, withKeyword, members, range) ->
        [ yield mkNode SynExceptionSig_ range
          yield! visitSynExceptionDefnRepr sedr
          yield! mkNodeOption SynExceptionSig_With withKeyword
          yield! (members |> List.collect visitSynMemberSig) ]

and visitSynLongIdent (lid: SynLongIdent) : TriviaNodeAssigner list =
    [ yield mkNode SynLongIdent_ lid.FullRange
      yield! List.map visitSynIdent lid.IdentsWithTrivia ]

and visitLongIdentIncludingFullRange (li: LongIdent) : TriviaNodeAssigner list =
    mkNode LongIdent_ (longIdentFullRange li)
    :: List.map visitIdent li

and visitIdent (ident: Ident) : TriviaNodeAssigner = mkNode Ident_ ident.idRange

and visitSynIdent (si: SynIdent) = mkNode SynIdent_ si.FullRange

let astToNode (range: range) (hds: ParsedHashDirective list) (mdls: SynModuleOrNamespace list) : TriviaNodeAssigner =
    { Range = range
      Type = ParsedInput_
      Children =
        Helpers.sortChildren
            [| yield! List.map Ast.visitSynModuleOrNamespace mdls
               yield! List.map Ast.visitParsedHashDirective hds |]
      FSharpASTNode = None }
//
// [ yield! List.collect Ast.visitSynModuleOrNamespace mdls
//   yield! List.map Ast.visitParsedHashDirective hds ]

let sigAstToNode (ast: SynModuleOrNamespaceSig list) : TriviaNodeAssigner list =
    List.collect visitSynModuleOrNamespaceSig ast
