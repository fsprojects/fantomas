module Fantomas.AstTransformer

open FSharp.Compiler.Text
open FSharp.Compiler.SyntaxTree
open Fantomas.TriviaTypes
open Fantomas

//let rec (|Sequentials|_|) =
//    function
//    | SynExpr.Sequential (_, isTrueSeq, e, Sequentials es, range) -> Some((isTrueSeq, e, None, range) :: es)
//    | SynExpr.Sequential (_, isTrueSeq, e1, e2, range) -> Some [ isTrueSeq, e1, Some e2, range ]
//    | _ -> None

type Id = { Ident: string; Range: Range option }

type FsAstNode = obj

type Node =
    { Type: FsAstType
      Range: Range option
      Properties: Map<string, obj>
      // Childs: Node list
      FsAstNode: FsAstNode }

module Helpers =
    let r (r: Range) : Range option = Some r

    let p = Map.ofList
    let inline (==>) a b = (a, box b)

    let noRange = None

    let i (id: Ident) : Id =
        { Ident = id.idText
          Range = r id.idRange }

    let li (id: LongIdent) = id |> List.map i

    let lid (id: LongIdentWithDots) = li id.Lid

module private Ast =
    open Helpers

    let rec visitSynModuleOrNamespace (modOrNs: SynModuleOrNamespace) : Node list =
        match modOrNs with
        | SynModuleOrNamespace (longIdent, isRecursive, synModuleOrNamespaceKind, decls, _, attrs, access, range) ->
            let collectIdents (idents: LongIdent) =
                idents
                |> List.map
                    (fun ident ->
                        { Type = Ident_
                          Range = r ident.idRange
                          Properties = Map.empty
                          FsAstNode = ident })

            let typeName =
                match synModuleOrNamespaceKind with
                | SynModuleOrNamespaceKind.AnonModule -> SynModuleOrNamespace_AnonModule
                | SynModuleOrNamespaceKind.NamedModule -> SynModuleOrNamespace_NamedModule
                | SynModuleOrNamespaceKind.DeclaredNamespace -> SynModuleOrNamespace_DeclaredNamespace
                | SynModuleOrNamespaceKind.GlobalNamespace -> SynModuleOrNamespace_GlobalNamespace

            { Type = typeName
              Range = r range
              Properties =
                  p [ yield "isRecursive" ==> isRecursive
                      yield
                          "synModuleOrNamespaceKind"
                          ==> synModuleOrNamespaceKind
                      yield "longIdent" ==> li longIdent
                      if access.IsSome then
                          yield "access" ==> (access.Value |> visitSynAccess) ]
              FsAstNode = modOrNs }
            :: [ yield!
                     if synModuleOrNamespaceKind = SynModuleOrNamespaceKind.DeclaredNamespace then
                         collectIdents longIdent
                     else
                         []
                 yield! (visitSynAttributeLists range attrs)
                 yield! (decls |> List.collect visitSynModuleDecl) ]

    and visitSynModuleDecl (ast: SynModuleDecl) : Node list =
        let rec visit (ast: SynModuleDecl) (finalContinuation: Node list -> Node list) : Node list =
            match ast with
            | SynModuleDecl.ModuleAbbrev (ident, longIdent, range) ->
                [ { Type = SynModuleDecl_ModuleAbbrev
                    Range = r range
                    Properties =
                        p [ "ident" ==> i ident
                            "longIdent" ==> li longIdent ]
                    FsAstNode = ast } ]
                |> finalContinuation
            | SynModuleDecl.NestedModule (sci, isRecursive, decls, _, range) ->
                let continuations : ((Node list -> Node list) -> Node list) list = decls |> List.map visit

                let finalContinuation (nodes: Node list list) : Node list =
                    [ { Type = SynModuleDecl_NestedModule
                        Range = r range
                        Properties = p [ "isRecursive" ==> isRecursive ]
                        FsAstNode = ast }
                      yield! visitSynComponentInfo sci
                      yield! (List.collect id nodes) ]
                    |> finalContinuation

                Continuation.sequence continuations finalContinuation
            | SynModuleDecl.Let (_, bindings, range) ->
                { Type = SynModuleDecl_Let
                  Range = r range
                  Properties = p []
                  FsAstNode = ast }
                :: (bindings |> List.collect visitSynBinding)
                |> finalContinuation
            | SynModuleDecl.DoExpr (_, expr, range) ->
                { Type = SynModuleDecl_DoExpr
                  Range = r range
                  Properties = p []
                  FsAstNode = ast }
                :: visitSynExpr expr
                |> finalContinuation
            | SynModuleDecl.Types (typeDefs, range) ->
                { Type = SynModuleDecl_Types
                  Range = r range
                  Properties = p []
                  FsAstNode = ast }
                :: (typeDefs |> List.collect visitSynTypeDefn)
                |> finalContinuation
            | SynModuleDecl.Exception (exceptionDef, range) ->
                { Type = SynModuleDecl_Exception
                  Range = r range
                  Properties = p []
                  FsAstNode = ast }
                :: (visitSynExceptionDefn exceptionDef)
                |> finalContinuation
            | SynModuleDecl.Open (target, parentRange) ->
                // we use the parent ranges here to match up with the trivia parsed
                match target with
                | SynOpenDeclTarget.ModuleOrNamespace (longIdent, _range) ->
                    { Type = SynModuleDecl_Open
                      Range = r parentRange
                      Properties = p [ "longIdent" ==> li longIdent ]
                      FsAstNode = ast }
                    |> List.singleton
                    |> finalContinuation
                | SynOpenDeclTarget.Type (synType, _range) ->
                    { Type = SynModuleDecl_OpenType
                      Range = r parentRange
                      Properties = p []
                      FsAstNode = ast }
                    :: (visitSynType synType)
                    |> finalContinuation
            | SynModuleDecl.Attributes (attrs, range) ->
                { Type = SynModuleDecl_Attributes
                  Range = r range
                  Properties = p []
                  FsAstNode = ast }
                :: (visitSynAttributeLists range attrs)
                |> finalContinuation
            | SynModuleDecl.HashDirective (hash, range) ->
                [ { Type = SynModuleDecl_HashDirective
                    Range = r range
                    Properties = p []
                    FsAstNode = ast }
                  visitParsedHashDirective hash ]
                |> finalContinuation
            | SynModuleDecl.NamespaceFragment (moduleOrNamespace) ->
                { Type = SynModuleDecl_NamespaceFragment
                  Range = noRange
                  Properties = p []
                  FsAstNode = ast }
                :: (visitSynModuleOrNamespace moduleOrNamespace)
                |> finalContinuation

        visit ast id

    and visitSynExpr (synExpr: SynExpr) : Node list =
        let rec visit (synExpr: SynExpr) (finalContinuation: Node list -> Node list) : Node list =
            match synExpr with
            | SynExpr.Paren (expr, leftParenRange, rightParenRange, range) ->
                visit
                    expr
                    (fun nodes ->
                        [ { Type = SynExpr_Paren
                            Range = r range
                            Properties =
                                p [ yield "leftParenRange" ==> r leftParenRange
                                    if rightParenRange.IsSome then
                                        yield "rightParenRange" ==> r rightParenRange.Value ]
                            FsAstNode = synExpr }
                          yield! nodes ]
                        |> finalContinuation)
            | SynExpr.Quote (operator, isRaw, quotedSynExpr, isFromQueryExpression, range) ->
                let continuations : ((Node list -> Node list) -> Node list) list =
                    [ visit operator; visit quotedSynExpr ]

                let finalContinuation (nodes: Node list list) : Node list =
                    [ { Type = SynExpr_Quote
                        Range = r range
                        Properties =
                            p [ "isRaw" ==> isRaw
                                "isFromQueryExpression" ==> isFromQueryExpression ]
                        FsAstNode = synExpr }
                      yield! (List.collect id nodes) ]
                    |> finalContinuation

                Continuation.sequence continuations finalContinuation
            | SynExpr.Const (constant, range) ->
                [ { Type = SynExpr_Const
                    Range = r range
                    Properties = p []
                    FsAstNode = synExpr }
                  visitSynConst range constant ]
                |> finalContinuation
            | SynExpr.Typed (expr, typeName, range) ->
                visit
                    expr
                    (fun nodes ->
                        [ { Type = SynExpr_Typed
                            Range = r range
                            Properties = p []
                            FsAstNode = synExpr }
                          yield! nodes
                          yield! visitSynType typeName ]
                        |> finalContinuation)
            | SynExpr.Tuple (isStruct, exprs, commaRanges, range) ->
                let continuations : ((Node list -> Node list) -> Node list) list = exprs |> List.map visit

                let finalContinuation (nodes: Node list list) : Node list =
                    [ { Type = SynExpr_Tuple
                        Range = r range
                        Properties =
                            p [ "isStruct" ==> isStruct
                                "commaRanges" ==> (commaRanges |> List.map r) ]
                        FsAstNode = synExpr }
                      yield! (List.collect id nodes) ]
                    |> finalContinuation

                Continuation.sequence continuations finalContinuation
            | SynExpr.ArrayOrList (isList, exprs, range) ->
                let continuations : ((Node list -> Node list) -> Node list) list = exprs |> List.map visit

                let finalContinuation (nodes: Node list list) : Node list =
                    [ { Type = SynExpr_ArrayOrList
                        Range = r range
                        Properties = p [ "isList" ==> isList ]
                        FsAstNode = synExpr }
                      yield! (List.collect id nodes) ]
                    |> finalContinuation

                Continuation.sequence continuations finalContinuation
            | SynExpr.Record (_, _, recordFields, range) ->
                { Type = SynExpr_Record
                  Range = r range
                  Properties = p []
                  FsAstNode = synExpr }
                :: (List.collect visitRecordField recordFields)
                |> finalContinuation
            | SynExpr.AnonRecd (_, _, recordFields, range) ->
                { Type = SynExpr_AnonRecd
                  Range = r range
                  Properties = p []
                  FsAstNode = synExpr }
                :: (List.collect visitAnonRecordField recordFields)
                |> finalContinuation
            | SynExpr.New (isProtected, typeName, expr, range) ->
                visit
                    expr
                    (fun nodes ->
                        [ { Type = SynExpr_New
                            Range = r range
                            Properties = p [ "isProtected" ==> isProtected ]
                            FsAstNode = synExpr }
                          yield! nodes
                          yield! visitSynType typeName ]
                        |> finalContinuation)
            | SynExpr.ObjExpr (objType, argOptions, bindings, extraImpls, newExprRange, range) ->
                { Type = SynExpr_ObjExpr
                  Range = r range
                  Properties = p [ "newExprRange" ==> r newExprRange ]
                  FsAstNode = synExpr }
                :: [ yield! visitSynType objType
                     if argOptions.IsSome then
                         yield! visitArgsOption argOptions.Value
                     yield! extraImpls |> List.collect visitSynInterfaceImpl
                     yield! bindings |> List.collect visitSynBinding ]
                |> finalContinuation
            | SynExpr.While (_, whileExpr, doExpr, range) ->
                let continuations : ((Node list -> Node list) -> Node list) list = [ visit whileExpr; visit doExpr ]

                let finalContinuation (nodes: Node list list) : Node list =
                    [ { Type = SynExpr_While
                        Range = r range
                        Properties = p []
                        FsAstNode = synExpr }
                      yield! (List.collect id nodes) ]
                    |> finalContinuation

                Continuation.sequence continuations finalContinuation
            | SynExpr.For (_, ident, identBody, _, toBody, doBody, range) ->
                let continuations : ((Node list -> Node list) -> Node list) list =
                    [ visit identBody
                      visit toBody
                      visit doBody ]

                let finalContinuation (nodes: Node list list) : Node list =
                    [ { Type = SynExpr_For
                        Range = r range
                        Properties = p [ "ident" ==> i ident ]
                        FsAstNode = synExpr }
                      yield! (List.collect id nodes) ]
                    |> finalContinuation

                Continuation.sequence continuations finalContinuation
            | SynExpr.ForEach (_, (SeqExprOnly seqExprOnly), isFromSource, pat, enumExpr, bodyExpr, range) ->
                let continuations : ((Node list -> Node list) -> Node list) list = [ visit enumExpr; visit bodyExpr ]

                let finalContinuation (nodes: Node list list) : Node list =
                    [ { Type = SynExpr_ForEach
                        Range = r range
                        Properties =
                            p [ "isFromSource" ==> isFromSource
                                "seqExprOnly" ==> seqExprOnly ]
                        FsAstNode = synExpr }
                      yield! visitSynPat pat
                      yield! (List.collect id nodes) ]
                    |> finalContinuation

                Continuation.sequence continuations finalContinuation
            | SynExpr.ArrayOrListOfSeqExpr (isArray, expr, range) ->
                visit
                    expr
                    (fun nodes ->
                        [ { Type = SynExpr_ArrayOrListOfSeqExpr
                            Range = r range
                            Properties = p [ "isArray" ==> isArray ]
                            FsAstNode = synExpr }
                          yield! nodes ]
                        |> finalContinuation)
            | SynExpr.CompExpr (isArrayOrList, isNotNakedRefCell, expr, range) ->
                visit
                    expr
                    (fun nodes ->
                        { Type = SynExpr_CompExpr
                          Range = r range
                          Properties =
                              p [ "isArrayOrList" ==> isArrayOrList
                                  "isNotNakedRefCell" ==> isNotNakedRefCell ]
                          FsAstNode = synExpr }
                        :: nodes
                        |> finalContinuation)
            | SynExpr.Lambda (fromMethod, inLambdaSeq, args, body, _parsedData, range) ->
                visit
                    body
                    (fun nodes ->
                        [ { Type = SynExpr_Lambda
                            Range = r range
                            Properties =
                                p [ "fromMethod" ==> fromMethod
                                    "inLambdaSeq" ==> inLambdaSeq ]
                            FsAstNode = synExpr }
                          yield! visitSynSimplePats args
                          yield! nodes ]
                        |> finalContinuation)
            | SynExpr.MatchLambda (isExnMatch, _, matchClauses, _, range) ->
                { Type = SynExpr_MatchLambda
                  Range = r range
                  Properties = p [ "isExnMatch" ==> isExnMatch ]
                  FsAstNode = synExpr }
                :: (List.collect visitSynMatchClause matchClauses)
                |> finalContinuation
            | SynExpr.Match (_, expr, clauses, range) ->
                visit
                    expr
                    (fun nodes ->
                        [ { Type = SynExpr_Match
                            Range = r range
                            Properties = p []
                            FsAstNode = synExpr }
                          yield! nodes
                          yield! (List.collect visitSynMatchClause clauses) ]
                        |> finalContinuation)
            | SynExpr.Do (expr, range) ->
                visit
                    expr
                    (fun nodes ->
                        { Type = SynExpr_Do
                          Range = r range
                          Properties = p []
                          FsAstNode = synExpr }
                        :: nodes
                        |> finalContinuation)
            | SynExpr.Assert (expr, range) ->
                visit
                    expr
                    (fun nodes ->
                        { Type = SynExpr_Assert
                          Range = r range
                          Properties = p []
                          FsAstNode = synExpr }
                        :: nodes
                        |> finalContinuation)
            | SynExpr.App (atomicFlag, isInfix, funcExpr, argExpr, range) ->
                let continuations : ((Node list -> Node list) -> Node list) list = [ visit funcExpr; visit argExpr ]

                let finalContinuation (nodes: Node list list) : Node list =
                    { Type = SynExpr_App
                      Range = r range
                      Properties =
                          p [ "atomicFlag"
                              ==> (match atomicFlag with
                                   | ExprAtomicFlag.Atomic -> "Atomic"
                                   | _ -> "Not Atomic")
                              "isInfix" ==> isInfix ]
                      FsAstNode = synExpr }
                    :: (List.collect id nodes)
                    |> finalContinuation

                Continuation.sequence continuations finalContinuation
            | SynExpr.TypeApp (expr, lESSrange, typeNames, commaRanges, gREATERrange, typeArgsRange, range) ->
                visit
                    expr
                    (fun nodes ->
                        [ { Type = SynExpr_TypeApp
                            Range = r range
                            Properties =
                                p [ yield "lESSrange" ==> r lESSrange
                                    yield "commaRanges" ==> (commaRanges |> List.map r)
                                    if gREATERrange.IsSome then
                                        yield "gREATERrange" ==> r gREATERrange.Value
                                    yield "typeArgsRange" ==> r typeArgsRange ]
                            FsAstNode = synExpr }
                          yield! nodes
                          yield! (List.collect visitSynType typeNames) ]
                        |> finalContinuation)
            | SynExpr.LetOrUse (isRecursive, isUse, bindings, body, range) ->
                visit
                    body
                    (fun nodes ->
                        [ { Type = SynExpr_LetOrUse
                            Range = r range
                            Properties =
                                p [ "isRecursive" ==> isRecursive
                                    "isUse" ==> isUse ]
                            FsAstNode = synExpr }
                          yield! bindings |> List.collect visitSynBinding
                          yield! nodes ]
                        |> finalContinuation)
            | SynExpr.TryWith (tryExpr, tryRange, withCases, withRange, range, _, _) ->
                visit
                    tryExpr
                    (fun nodes ->
                        [ { Type = SynExpr_TryWith
                            Range = r range
                            Properties =
                                p [ "tryRange" ==> r tryRange
                                    "withRange" ==> r withRange ]
                            FsAstNode = synExpr }
                          yield! nodes
                          yield! withCases |> List.collect visitSynMatchClause ]
                        |> finalContinuation)
            | SynExpr.TryFinally (tryExpr, finallyExpr, range, _, _) ->
                let continuations : ((Node list -> Node list) -> Node list) list = [ visit tryExpr; visit finallyExpr ]

                let finalContinuation (nodes: Node list list) : Node list =
                    { Type = SynExpr_TryFinally
                      Range = r range
                      Properties = p []
                      FsAstNode = synExpr }
                    :: (List.collect id nodes)
                    |> finalContinuation

                Continuation.sequence continuations finalContinuation
            | SynExpr.Lazy (ex, range) ->
                visit
                    ex
                    (fun nodes ->
                        { Type = SynExpr_Lazy
                          Range = r range
                          Properties = p []
                          FsAstNode = synExpr }
                        :: nodes
                        |> finalContinuation)
            //        | Sequentials xs ->
            //            let rec cons xs =
            //                match xs with
            //                | [] -> failwith "should not happen" // expr2Opt is always Some in last item
            //                | ((isTrueSeq, expr1, expr2Opt, range) :: rest) ->
            //                    { Type = SynExpr_Sequential
            //                      Range = r range
            //                      Properties = p [ "isTrueSeq" ==> isTrueSeq ]
            //                      FsAstNode = synExpr
            //                      Childs =
            //                          [ yield visitSynExpr expr1
            //                            yield
            //                                expr2Opt
            //                                |> Option.map visitSynExpr
            //                                |> Option.defaultWith (fun () -> cons rest) ] }
            //
            //            cons xs
            | SynExpr.Sequential (_, isTrueSeq, expr1, expr2, range) ->
                let continuations : ((Node list -> Node list) -> Node list) list = [ visit expr1; visit expr2 ]

                let finalContinuation (nodes: Node list list) : Node list =
                    { Type = SynExpr_Sequential
                      Range = r range
                      Properties = p [ "isTrueSeq" ==> isTrueSeq ]
                      FsAstNode = synExpr }
                    :: (List.collect id nodes)
                    |> finalContinuation

                Continuation.sequence continuations finalContinuation
            | SynExpr.SequentialOrImplicitYield (seqPoint, expr1, expr2, ifNotStmt, range) ->
                let continuations : ((Node list -> Node list) -> Node list) list =
                    [ visit expr1
                      visit expr2
                      visit ifNotStmt ]

                let finalContinuation (nodes: Node list list) : Node list =
                    { Type = SynExpr_SequentialOrImplicitYield
                      Range = r range
                      FsAstNode = synExpr
                      Properties = p [ "seqPoint" ==> seqPoint ] }
                    :: (List.collect id nodes)
                    |> finalContinuation

                Continuation.sequence continuations finalContinuation
            | SynExpr.IfThenElse (ifExpr, thenExpr, elseExpr, _, isFromErrorRecovery, ifToThenRange, range) ->
                let continuations : ((Node list -> Node list) -> Node list) list =
                    [ visit ifExpr
                      visit thenExpr
                      yield! (Option.toList elseExpr |> List.map visit) ]

                let finalContinuation (nodes: Node list list) : Node list =
                    { Type = SynExpr_IfThenElse
                      Range = r range
                      Properties =
                          p [ "isFromErrorRecovery" ==> isFromErrorRecovery
                              "ifToThenRange" ==> r ifToThenRange ]
                      FsAstNode = synExpr }
                    :: (List.collect id nodes)
                    |> finalContinuation

                Continuation.sequence continuations finalContinuation
            | SynExpr.Ident (id) ->
                { Type = SynExpr_Ident
                  Range = (i id).Range
                  Properties = p [ "ident" ==> i id ]
                  FsAstNode = synExpr }
                |> List.singleton
                |> finalContinuation
            | SynExpr.LongIdent (isOptional, longDotId, _, range) ->
                { Type = SynExpr_LongIdent
                  Range = r range
                  Properties =
                      p [ "isOptional" ==> isOptional
                          "longDotId" ==> lid longDotId ]
                  FsAstNode = synExpr }
                :: (visitLongIdentWithDots longDotId)
                |> finalContinuation
            | SynExpr.LongIdentSet (longDotId, expr, range) ->
                visit
                    expr
                    (fun nodes ->
                        { Type = SynExpr_LongIdentSet
                          Range = r range
                          Properties = p [ "longDotId" ==> lid longDotId ]
                          FsAstNode = synExpr }
                        :: nodes
                        |> finalContinuation)
            | SynExpr.DotGet (expr, rangeOfDot, longDotId, range) ->
                visit
                    expr
                    (fun nodes ->
                        [ { Type = SynExpr_DotGet
                            Range = r range
                            Properties =
                                p [ "rangeOfDot" ==> r rangeOfDot
                                    "longDotId" ==> lid longDotId ]
                            FsAstNode = synExpr }
                          yield! nodes
                          // Idents are collected as childs here to deal with unit test ``Fluent api with comments should remain on same lines``
                          yield! (visitLongIdentWithDots longDotId) ]
                        |> finalContinuation)
            | SynExpr.DotSet (expr, longDotId, e2, range) ->
                let continuations : ((Node list -> Node list) -> Node list) list = [ visit expr; visit e2 ]

                let finalContinuation (nodes: Node list list) : Node list =
                    { Type = SynExpr_DotSet
                      Range = r range
                      Properties = p [ "longDotId" ==> lid longDotId ]
                      FsAstNode = synExpr }
                    :: (List.collect id nodes)
                    |> finalContinuation

                Continuation.sequence continuations finalContinuation
            | SynExpr.Set (e1, e2, range) ->
                let continuations : ((Node list -> Node list) -> Node list) list = [ visit e1; visit e2 ]

                let finalContinuation (nodes: Node list list) : Node list =
                    { Type = SynExpr_Set
                      Range = r range
                      Properties = p []
                      FsAstNode = synExpr }
                    :: (List.collect id nodes)
                    |> finalContinuation

                Continuation.sequence continuations finalContinuation
            | SynExpr.DotIndexedGet (objectExpr, indexExprs, dotRange, range) ->
                visit
                    objectExpr
                    (fun nodes ->
                        [ { Type = SynExpr_DotIndexedGet
                            Range = r range
                            Properties = p [ "dotRange" ==> r dotRange ]
                            FsAstNode = synExpr }
                          yield! nodes
                          yield! indexExprs |> List.collect visitSynIndexerArg ]
                        |> finalContinuation)
            | SynExpr.DotIndexedSet (objectExpr, indexExprs, valueExpr, leftOfSetRange, dotRange, range) ->
                let continuations : ((Node list -> Node list) -> Node list) list = [ visit objectExpr; visit valueExpr ]

                let finalContinuation (nodes: Node list list) : Node list =
                    [ { Type = SynExpr_DotIndexedSet
                        Range = r range
                        Properties =
                            p [ "leftOfSetRange" ==> r leftOfSetRange
                                "dotRange" ==> r dotRange ]
                        FsAstNode = synExpr }
                      yield! (List.collect id nodes)
                      yield! indexExprs |> List.collect visitSynIndexerArg ]
                    |> finalContinuation

                Continuation.sequence continuations finalContinuation
            | SynExpr.NamedIndexedPropertySet (longDotId, e1, e2, range) ->
                let continuations : ((Node list -> Node list) -> Node list) list = [ visit e1; visit e2 ]

                let finalContinuation (nodes: Node list list) : Node list =
                    { Type = SynExpr_NamedIndexedPropertySet
                      Range = r range
                      Properties = p [ "longDotId" ==> lid longDotId ]
                      FsAstNode = synExpr }
                    :: (List.collect id nodes)
                    |> finalContinuation

                Continuation.sequence continuations finalContinuation
            | SynExpr.DotNamedIndexedPropertySet (expr, longDotId, e1, e2, range) ->
                let continuations : ((Node list -> Node list) -> Node list) list = [ visit expr; visit e1; visit e2 ]

                let finalContinuation (nodes: Node list list) : Node list =
                    { Type = SynExpr_DotNamedIndexedPropertySet
                      Range = r range
                      Properties = p [ "longDotId" ==> lid longDotId ]
                      FsAstNode = synExpr }
                    :: (List.collect id nodes)
                    |> finalContinuation

                Continuation.sequence continuations finalContinuation
            | SynExpr.TypeTest (expr, typeName, range) ->
                visit
                    expr
                    (fun nodes ->
                        [ { Type = SynExpr_TypeTest
                            Range = r range
                            Properties = p []
                            FsAstNode = synExpr }
                          yield! nodes
                          yield! visitSynType typeName ]
                        |> finalContinuation)
            | SynExpr.Upcast (expr, typeName, range) ->
                visit
                    expr
                    (fun nodes ->
                        [ { Type = SynExpr_Upcast
                            Range = r range
                            Properties = p []
                            FsAstNode = synExpr }
                          yield! nodes
                          yield! visitSynType typeName ]
                        |> finalContinuation)
            | SynExpr.Downcast (expr, typeName, range) ->
                visit
                    expr
                    (fun nodes ->
                        [ { Type = SynExpr_Downcast
                            Range = r range
                            Properties = p []
                            FsAstNode = synExpr }
                          yield! nodes
                          yield! visitSynType typeName ]
                        |> finalContinuation)
            | SynExpr.InferredUpcast (expr, range) ->
                visit
                    expr
                    (fun nodes ->
                        { Type = SynExpr_InferredUpcast
                          Range = r range
                          Properties = p []
                          FsAstNode = synExpr }
                        :: nodes
                        |> finalContinuation)
            | SynExpr.InferredDowncast (expr, range) ->
                visit
                    expr
                    (fun nodes ->
                        { Type = SynExpr_InferredDowncast
                          Range = r range
                          Properties = p []
                          FsAstNode = synExpr }
                        :: nodes
                        |> finalContinuation)
            | SynExpr.Null (range) ->
                { Type = SynExpr_Null
                  Range = r range
                  Properties = p []
                  FsAstNode = synExpr }
                |> List.singleton
                |> finalContinuation
            | SynExpr.AddressOf (isByref, expr, refRange, range) ->
                visit
                    expr
                    (fun nodes ->
                        { Type = SynExpr_AddressOf
                          Range = r range
                          Properties =
                              p [ "isByref" ==> isByref
                                  "refRange" ==> r refRange ]
                          FsAstNode = synExpr }
                        :: nodes
                        |> finalContinuation)
            | SynExpr.TraitCall (typars, sign, expr, range) ->
                visit
                    expr
                    (fun nodes ->
                        [ { Type = SynExpr_TraitCall
                            Range = r range
                            Properties = p []
                            FsAstNode = synExpr }
                          yield! typars |> List.collect visitSynTypar
                          yield! visitSynMemberSig sign
                          yield! nodes ]
                        |> finalContinuation)
            | SynExpr.JoinIn (expr, inrange, expr2, range) ->
                let continuations : ((Node list -> Node list) -> Node list) list = [ visit expr; visit expr2 ]

                let finalContinuation (nodes: Node list list) : Node list =
                    { Type = SynExpr_JoinIn
                      Range = r range
                      Properties = p [ "inRange" ==> r inrange ]
                      FsAstNode = synExpr }
                    :: (List.collect id nodes)
                    |> finalContinuation

                Continuation.sequence continuations finalContinuation
            | SynExpr.ImplicitZero (range) ->
                { Type = SynExpr_ImplicitZero
                  Range = r range
                  Properties = p []
                  FsAstNode = synExpr }
                |> List.singleton
                |> finalContinuation
            | SynExpr.YieldOrReturn (_, expr, range) ->
                visit
                    expr
                    (fun nodes ->
                        { Type = SynExpr_YieldOrReturn
                          Range = r range
                          Properties = p []
                          FsAstNode = synExpr }
                        :: nodes
                        |> finalContinuation)
            | SynExpr.YieldOrReturnFrom (_, expr, range) ->
                visit
                    expr
                    (fun nodes ->
                        { Type = SynExpr_YieldOrReturnFrom
                          Range = r range
                          Properties = p []
                          FsAstNode = synExpr }
                        :: nodes
                        |> finalContinuation)
            | SynExpr.LetOrUseBang (_, isUse, isFromSource, pat, rhsExpr, andBangs, body, range) ->
                let continuations : ((Node list -> Node list) -> Node list) list =
                    [ visit rhsExpr
                      visit body
                      yield! (List.map (fun (_, _, _, _, body, _) -> visit body) andBangs) ]

                let finalContinuation (nodes: Node list list) : Node list =
                    [ { Type = SynExpr_LetOrUseBang
                        Range = r range
                        Properties =
                            p [ "isUse" ==> isUse
                                "isFromSource" ==> isFromSource ]
                        FsAstNode = synExpr }
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
                        [ { Type = SynExpr_MatchBang
                            Range = r range
                            Properties = p []
                            FsAstNode = synExpr }
                          yield! nodes
                          yield! clauses |> List.collect visitSynMatchClause ]
                        |> finalContinuation)
            | SynExpr.DoBang (expr, range) ->
                visit
                    expr
                    (fun nodes ->
                        { Type = SynExpr_DoBang
                          Range = r range
                          Properties = p []
                          FsAstNode = synExpr }
                        :: nodes
                        |> finalContinuation)
            | SynExpr.LibraryOnlyILAssembly (_, _, _, _, range) ->
                { Type = SynExpr_LibraryOnlyILAssembly
                  Range = r range
                  Properties = p []
                  FsAstNode = synExpr }
                |> List.singleton
                |> finalContinuation
            | SynExpr.LibraryOnlyStaticOptimization (_, _, _, range) ->
                { Type = SynExpr_LibraryOnlyStaticOptimization
                  Range = r range
                  Properties = p []
                  FsAstNode = synExpr }
                |> List.singleton
                |> finalContinuation
            | SynExpr.LibraryOnlyUnionCaseFieldGet (expr, longId, _, range) ->
                visit
                    expr
                    (fun nodes ->
                        { Type = SynExpr_LibraryOnlyUnionCaseFieldGet
                          Range = r range
                          Properties = p [ "longId" ==> li longId ]
                          FsAstNode = synExpr }
                        :: nodes
                        |> finalContinuation)
            | SynExpr.LibraryOnlyUnionCaseFieldSet (e1, longId, _, e2, range) ->
                let continuations : ((Node list -> Node list) -> Node list) list = [ visit e1; visit e2 ]

                let finalContinuation (nodes: Node list list) : Node list =
                    { Type = SynExpr_LibraryOnlyUnionCaseFieldSet
                      Range = r range
                      Properties = p [ "longId" ==> li longId ]
                      FsAstNode = synExpr }
                    :: (List.collect id nodes)
                    |> finalContinuation

                Continuation.sequence continuations finalContinuation
            | SynExpr.ArbitraryAfterError (debugStr, range) ->
                { Type = SynExpr_ArbitraryAfterError
                  Range = r range
                  Properties = p [ "debugStr" ==> debugStr ]
                  FsAstNode = synExpr }
                |> List.singleton
                |> finalContinuation
            | SynExpr.FromParseError (expr, range) ->
                visit
                    expr
                    (fun nodes ->
                        { Type = SynExpr_FromParseError
                          Range = r range
                          Properties = p []
                          FsAstNode = synExpr }
                        :: nodes
                        |> finalContinuation)
            | SynExpr.DiscardAfterMissingQualificationAfterDot (expr, range) ->
                visit
                    expr
                    (fun nodes ->
                        { Type = SynExpr_DiscardAfterMissingQualificationAfterDot
                          Range = r range
                          Properties = p []
                          FsAstNode = synExpr }
                        :: nodes
                        |> finalContinuation)
            | SynExpr.Fixed (expr, range) ->
                visit
                    expr
                    (fun nodes ->
                        { Type = SynExpr_Fixed
                          Range = r range
                          Properties = p []
                          FsAstNode = synExpr }
                        :: nodes
                        |> finalContinuation)
            | SynExpr.InterpolatedString (parts, range) ->
                { Type = SynExpr_InterpolatedString
                  Range = r range
                  Properties = p []
                  FsAstNode = synExpr }
                :: (List.collect visitSynInterpolatedStringPart parts)
                |> finalContinuation

        visit synExpr id

    and visitSynInterpolatedStringPart (synInterpolatedStringPart: SynInterpolatedStringPart) =
        match synInterpolatedStringPart with
        | SynInterpolatedStringPart.String (value, range) ->
            { Type = SynInterpolatedStringPart_String
              Range = r range
              Properties = p [ "value", box value ]
              FsAstNode = synInterpolatedStringPart }
            |> List.singleton
        | SynInterpolatedStringPart.FillExpr (expr, ident) ->
            { Type = SynInterpolatedStringPart_FillExpr
              Range = None
              Properties = p []
              FsAstNode = synInterpolatedStringPart }
            :: [ yield! visitSynExpr expr
                 yield! (Option.toList ident |> List.map visitIdent) ]

    and visitRecordField ((longId, _) as rfn: RecordFieldName, expr: SynExpr option, _: BlockSeparator option) =
        { Type = RecordField_
          Range = r longId.Range
          Properties = p [ "ident" ==> lid longId ]
          FsAstNode = rfn }
        :: (match expr with
            | Some e -> visitSynExpr e
            | None -> [])

    and visitAnonRecordField (ident: Ident, expr: SynExpr) =
        { Type = AnonRecordField_
          Range = noRange
          Properties = p [ "ident" ==> i ident ]
          FsAstNode = expr }
        :: (visitSynExpr expr)

    and visitAnonRecordTypeField (ident: Ident, t: SynType) =
        { Type = AnonRecordTypeField_
          Range = noRange
          Properties = p [ "ident" ==> i ident ]
          FsAstNode = t }
        :: (visitSynType t)

    and visitSynMemberSig (ms: SynMemberSig) : Node list =
        match ms with
        | SynMemberSig.Member (valSig, _, range) ->
            { Type = SynMemberSig_Member
              Range = r range
              Properties = p []
              FsAstNode = ms }
            :: (visitSynValSig valSig)
        | SynMemberSig.Interface (typeName, range) ->
            { Type = SynMemberSig_Interface
              Range = r range
              Properties = p []
              FsAstNode = ms }
            :: (visitSynType typeName)
        | SynMemberSig.Inherit (typeName, range) ->
            { Type = SynMemberSig_Inherit
              Range = r range
              Properties = p []
              FsAstNode = ms }
            :: (visitSynType typeName)
        | SynMemberSig.ValField (f, range) ->
            { Type = SynMemberSig_ValField
              Range = r range
              Properties = p []
              FsAstNode = ms }
            :: (visitSynField f)
        | SynMemberSig.NestedType (typedef, range) ->
            { Type = SynMemberSig_NestedType
              Range = r range
              Properties = p []
              FsAstNode = ms }
            :: (visitSynTypeDefnSig typedef)

    and visitSynIndexerArg (ia: SynIndexerArg) : Node list =
        match ia with
        | SynIndexerArg.One (e, _fromEnd, _) ->
            { Type = SynIndexerArg_One
              Range = noRange
              Properties = p []
              FsAstNode = ia }
            :: (visitSynExpr e)
        | SynIndexerArg.Two (e1, _fromEnd1, e2, _fromEnd2, _, _) ->
            { Type = SynIndexerArg_Two
              Range = noRange
              Properties = p []
              FsAstNode = ia }
            :: [ yield! visitSynExpr e1
                 yield! visitSynExpr e2 ]

    and visitSynMatchClause (mc: SynMatchClause) : Node list =
        match mc with
        | SynMatchClause.Clause (pat, e1, e2, _range, _) ->
            { Type = SynMatchClause_Clause
              Range = r mc.Range // _range is the same range as pat, see https://github.com/dotnet/fsharp/issues/10877
              Properties = p []
              FsAstNode = mc }
            :: [ yield! visitSynPat pat
                 if e1.IsSome then
                     yield! visitSynExpr e1.Value
                 yield! visitSynExpr e2 ]

    and visitArgsOption (expr: SynExpr, ident: Ident option) =
        { Type = ArgOptions_
          Range = noRange
          Properties =
              p [ if ident.IsSome then
                      yield "ident" ==> i ident.Value ]
          FsAstNode = expr }
        :: (visitSynExpr expr)

    and visitSynInterfaceImpl (ii: SynInterfaceImpl) : Node list =
        match ii with
        | InterfaceImpl (typ, bindings, range) ->
            { Type = InterfaceImpl_
              Range = r range
              Properties = p []
              FsAstNode = ii }
            :: [ yield! visitSynType typ
                 yield! (bindings |> List.collect visitSynBinding) ]

    and visitSynTypeDefn (td: SynTypeDefn) =
        match td with
        | TypeDefn (sci, stdr, members, range) ->
            { Type = TypeDefn_
              Range = r range
              Properties = p []
              FsAstNode = td }
            :: [ yield! visitSynComponentInfo sci
                 yield! visitSynTypeDefnRepr stdr
                 yield! (members |> List.collect visitSynMemberDefn) ]

    and visitSynTypeDefnSig (typeDefSig: SynTypeDefnSig) : Node list =
        match typeDefSig with
        | TypeDefnSig (sci, synTypeDefnSigReprs, memberSig, range) ->
            { Type = TypeDefnSig_
              Range = r range
              Properties = p []
              FsAstNode = typeDefSig }
            :: [ yield! visitSynComponentInfo sci
                 yield! visitSynTypeDefnSigRepr synTypeDefnSigReprs
                 yield! (memberSig |> List.collect visitSynMemberSig) ]

    and visitSynTypeDefnSigRepr (stdr: SynTypeDefnSigRepr) : Node list =
        match stdr with
        | SynTypeDefnSigRepr.ObjectModel (kind, members, range) ->
            { Type = SynTypeDefnSigRepr_ObjectModel
              Range = r range
              Properties = p []
              FsAstNode = stdr }
            :: [ yield! visitSynTypeDefnKind kind
                 yield! (members |> List.collect visitSynMemberSig) ]
        | SynTypeDefnSigRepr.Simple (simpleRepr, range) ->
            { Type = SynTypeDefnSigRepr_ObjectModel
              Range = r range
              Properties = p []
              FsAstNode = stdr }
            :: (visitSynTypeDefnSimpleRepr simpleRepr)
        | SynTypeDefnSigRepr.Exception (exceptionRepr) ->
            { Type = SynTypeDefnSigRepr_Exception
              Range = noRange
              Properties = p []
              FsAstNode = stdr }
            :: (visitSynExceptionDefnRepr exceptionRepr)

    and visitSynMemberDefn (mbrDef: SynMemberDefn) : Node list =
        match mbrDef with
        | SynMemberDefn.Open (target, parentRange) ->
            // we use the parent ranges here to match up with the trivia parsed
            match target with
            | SynOpenDeclTarget.ModuleOrNamespace (longIdent, _range) ->
                { Type = SynMemberDefn_Open
                  Range = r parentRange
                  Properties = p [ "longIdent" ==> li longIdent ]
                  FsAstNode = target }
                |> List.singleton
            | SynOpenDeclTarget.Type (synType, _range) ->
                { Type = SynMemberDefn_OpenType
                  Range = r parentRange
                  Properties = p []
                  FsAstNode = target }
                :: (visitSynType synType)
        | SynMemberDefn.Member (memberDefn, range) ->
            { Type = SynMemberDefn_Member
              Range = r range
              Properties = p []
              FsAstNode = mbrDef }
            :: (visitSynBinding memberDefn)
        | SynMemberDefn.ImplicitCtor (access, attrs, ctorArgs, selfIdentifier, _xmlDoc, range) ->
            { Type = SynMemberDefn_ImplicitCtor
              Range = r range
              Properties =
                  p [ if selfIdentifier.IsSome then
                          yield "selfIdent" ==> i selfIdentifier.Value
                      if access.IsSome then
                          yield "access" ==> (access.Value |> visitSynAccess) ]
              FsAstNode = mbrDef }
            :: [ yield! (visitSynAttributeLists range attrs)
                 yield! visitSynSimplePats ctorArgs ]
        | SynMemberDefn.ImplicitInherit (inheritType, inheritArgs, inheritAlias, range) ->
            { Type = SynMemberDefn_ImplicitInherit
              Range = r range
              Properties =
                  p [ if inheritAlias.IsSome then
                          yield "inheritAlias" ==> i inheritAlias.Value ]
              FsAstNode = mbrDef }
            :: [ yield! visitSynType inheritType
                 yield! visitSynExpr inheritArgs ]
        | SynMemberDefn.LetBindings (bindings, isStatic, isRecursive, range) ->
            { Type = SynMemberDefn_LetBindings
              Range = r range
              Properties =
                  p [ "isStatic" ==> isStatic
                      "isRecursive" ==> isRecursive ]
              FsAstNode = mbrDef }
            :: (List.collect visitSynBinding bindings)
        | SynMemberDefn.AbstractSlot (valSig, _, range) ->
            { Type = SynMemberDefn_AbstractSlot
              Range = r range
              Properties = p []
              FsAstNode = mbrDef }
            :: (visitSynValSig valSig)
        | SynMemberDefn.Interface (typ, members, range) ->
            { Type = SynMemberDefn_Interface
              Range = r range
              Properties = p []
              FsAstNode = mbrDef }
            :: [ yield! visitSynType typ
                 if members.IsSome then
                     yield! members.Value |> List.collect visitSynMemberDefn ]
        | SynMemberDefn.Inherit (typ, ident, range) ->
            { Type = SynMemberDefn_Inherit
              Range = r range
              Properties =
                  p [ if ident.IsSome then
                          yield "ident" ==> i ident.Value ]
              FsAstNode = mbrDef }
            :: (visitSynType typ)
        | SynMemberDefn.ValField (fld, range) ->
            { Type = SynMemberDefn_ValField
              Range = r range
              Properties = p []
              FsAstNode = mbrDef }
            :: (visitSynField fld)
        | SynMemberDefn.NestedType (typeDefn, access, range) ->
            { Type = SynMemberDefn_NestedType
              Range = r range
              Properties =
                  p [ if access.IsSome then
                          yield "access" ==> (access.Value |> visitSynAccess) ]
              FsAstNode = mbrDef }
            :: (visitSynTypeDefn typeDefn)
        | SynMemberDefn.AutoProperty (attrs,
                                      isStatic,
                                      ident,
                                      typeOpt,
                                      propKind,
                                      _,
                                      _,
                                      access,
                                      synExpr,
                                      getSetRange,
                                      range) ->
            { Type = SynMemberDefn_AutoProperty
              Range = r range
              Properties =
                  p [ yield "isStatic" ==> isStatic
                      yield "ident" ==> i ident
                      yield "propKind" ==> visitMemberKind propKind
                      if access.IsSome then
                          yield "access" ==> (access.Value |> visitSynAccess)
                      if getSetRange.IsSome then
                          yield "getSetRange" ==> (getSetRange.Value |> r) ]
              FsAstNode = mbrDef }
            :: [ yield! (visitSynAttributeLists range attrs)
                 if typeOpt.IsSome then
                     yield! visitSynType typeOpt.Value
                 yield! visitSynExpr synExpr ]

    and visitSynSimplePat (sp: SynSimplePat) : Node list =
        match sp with
        | SynSimplePat.Id (ident, _, isCompilerGenerated, isThisVar, isOptArg, range) ->
            { Type = SynSimplePat_Id
              Range = r range
              Properties =
                  p [ "isCompilerGenerated" ==> isCompilerGenerated
                      "isThisVar" ==> isThisVar
                      "isOptArg" ==> isOptArg
                      "ident" ==> i ident ]
              FsAstNode = sp }
            |> List.singleton
        | SynSimplePat.Typed (simplePat, typ, range) ->
            { Type = SynSimplePat_Typed
              Range = r range
              Properties = p []
              FsAstNode = sp }
            :: [ yield! visitSynSimplePat simplePat
                 yield! visitSynType typ ]
        | SynSimplePat.Attrib (simplePat, attrs, range) ->
            { Type = SynSimplePat_Attrib
              Range = r range
              Properties = p []
              FsAstNode = sp }
            :: [ yield! visitSynSimplePat simplePat
                 yield! (visitSynAttributeLists range attrs) ]

    and visitSynSimplePats (sp: SynSimplePats) : Node list =
        match sp with
        | SynSimplePats.SimplePats (pats, range) ->
            { Type = SynSimplePats_SimplePats
              Range = r range
              Properties = p []
              FsAstNode = sp }
            :: [ yield! pats |> List.collect visitSynSimplePat ]
        | SynSimplePats.Typed (pats, typ, range) ->
            { Type = SynSimplePat_Typed
              Range = r range
              Properties = p []
              FsAstNode = sp }
            :: [ yield! visitSynSimplePats pats
                 yield! visitSynType typ ]

    and visitSynBinding (binding: SynBinding) : Node list =
        match binding with
        | Binding (access, kind, mustInline, isMutable, attrs, _, valData, headPat, returnInfo, expr, range, _) ->
            let t =
                match kind with
                | SynBindingKind.StandaloneExpression -> StandaloneExpression_
                | SynBindingKind.NormalBinding -> NormalBinding_
                | SynBindingKind.DoBinding -> DoBinding_

            { Type = t
              Range = r binding.RangeOfBindingAndRhs
              Properties =
                  p [ yield "mustInline" ==> mustInline
                      yield "isMutable" ==> isMutable
                      yield "kind" ==> visitSynBindingKind kind
                      if access.IsSome then
                          yield "access" ==> (access.Value |> visitSynAccess) ]
              FsAstNode = binding }
            :: [ yield! (visitSynAttributeLists range attrs)
                 yield! visitSynValData valData
                 yield! visitSynPat headPat
                 if returnInfo.IsSome then
                     yield! visitSynBindingReturnInfo returnInfo.Value
                 yield! visitSynExpr expr ]

    and visitSynValData (svd: SynValData) : Node list =
        match svd with
        | SynValData (_, svi, ident) ->
            { Type = SynValData_
              Range = noRange
              Properties =
                  p [ if ident.IsSome then
                          yield "ident" ==> (ident.Value |> i) ]
              FsAstNode = svd }
            :: (visitSynValInfo svi)

    and visitSynValSig (svs: SynValSig) : Node list =
        match svs with
        | ValSpfn (attrs, ident, explicitValDecls, synType, arity, isInline, isMutable, _, access, expr, range) ->
            { Type = ValSpfn_
              Range = r range
              Properties =
                  p [ yield "ident" ==> i ident
                      yield "isMutable" ==> isMutable
                      yield "isInline" ==> isInline
                      if access.IsSome then
                          yield "access" ==> (access.Value |> visitSynAccess) ]
              FsAstNode = svs }
            :: [ yield! (visitSynAttributeLists range attrs)
                 yield! visitSynValTyparDecls explicitValDecls
                 yield! visitSynType synType
                 yield! visitSynValInfo arity
                 if expr.IsSome then
                     yield! visitSynExpr expr.Value ]

    and visitSynValTyparDecls (valTypeDecl: SynValTyparDecls) : Node list =
        match valTypeDecl with
        | SynValTyparDecls (typardecls, _, _) ->
            { Type = SynValTyparDecls_
              Range = noRange
              Properties = p []
              FsAstNode = valTypeDecl }
            :: (List.collect visitSynTyparDecl typardecls)

    and visitSynTyparDecl (std: SynTyparDecl) : Node list =
        match std with
        | TyparDecl (attrs, typar) ->
            { Type = TyparDecl_
              Range = noRange
              Properties = p []
              FsAstNode = std }
            :: [ yield! (visitSynAttributeLists typar.Range attrs)
                 yield! visitSynTypar typar ]

    and visitSynTypar (typar: SynTypar) : Node list =
        match typar with
        | Typar (ident, staticReq, isComGen) ->
            { Type = Typar_
              Range = noRange
              Properties =
                  p [ "ident" ==> i ident
                      "isComGen" ==> isComGen
                      "staticReq" ==> visitTyparStaticReq staticReq ]
              FsAstNode = typar }
            |> List.singleton

    and visitTyparStaticReq (tsr: TyparStaticReq) =
        match tsr with
        | NoStaticReq -> "NoStaticReq"
        | HeadTypeStaticReq -> "HeadTypeStaticReq"

    and visitSynBindingReturnInfo (returnInfo: SynBindingReturnInfo) : Node list =
        match returnInfo with
        | SynBindingReturnInfo (typeName, range, attrs) ->
            { Type = SynBindingReturnInfo_
              Range = r range
              Properties = p []
              FsAstNode = returnInfo }
            :: [ yield! visitSynType typeName
                 yield! (visitSynAttributeLists range attrs) ]

    and visitSynPat (sp: SynPat) : Node list =
        let rec visit (sp: SynPat) (finalContinuation: Node list -> Node list) : Node list =
            match sp with
            | SynPat.Const (sc, range) ->
                [ { Type = SynPat_Const
                    Range = r range
                    Properties = p []
                    FsAstNode = sp }
                  visitSynConst range sc ]
                |> finalContinuation
            | SynPat.Wild (range) ->
                { Type = SynPat_Wild
                  Range = r range
                  Properties = p []
                  FsAstNode = sp }
                |> List.singleton
                |> finalContinuation
            | SynPat.Named (synPat, ident, isSelfIdentifier, access, range) ->
                visit
                    synPat
                    (fun nodes ->
                        { Type = SynPat_Named
                          Range = r range
                          Properties =
                              p [ yield "ident" ==> i ident
                                  yield "isSelfIdentifier" ==> isSelfIdentifier
                                  if access.IsSome then
                                      yield "access" ==> (access.Value |> visitSynAccess) ]
                          FsAstNode = sp }
                        :: nodes
                        |> finalContinuation)
            | SynPat.Typed (synPat, synType, range) ->
                visit
                    synPat
                    (fun nodes ->
                        [ { Type = SynPat_Typed
                            Range = r range
                            Properties = p []
                            FsAstNode = sp }
                          yield! nodes
                          yield! (visitSynType synType) ]
                        |> finalContinuation)
            | SynPat.Attrib (synPat, attrs, range) ->
                visit
                    synPat
                    (fun nodes ->
                        [ { Type = SynPat_Attrib
                            Range = r range
                            Properties = p []
                            FsAstNode = sp }
                          yield! nodes
                          yield! (visitSynAttributeLists range attrs) ]
                        |> finalContinuation)
            | SynPat.Or (synPat, synPat2, range) ->
                let continuations : ((Node list -> Node list) -> Node list) list = [ visit synPat; visit synPat2 ]

                let finalContinuation (nodes: Node list list) : Node list =
                    [ { Type = SynPat_Or
                        Range = r range
                        Properties = p []
                        FsAstNode = sp }
                      yield! (List.collect id nodes) ]
                    |> finalContinuation

                Continuation.sequence continuations finalContinuation
            | SynPat.Ands (pats, range) ->
                let continuations : ((Node list -> Node list) -> Node list) list = pats |> List.map visit

                let finalContinuation (nodes: Node list list) : Node list =
                    { Type = SynPat_Ands
                      Range = r range
                      Properties = p []
                      FsAstNode = sp }
                    :: (List.collect id nodes)
                    |> finalContinuation

                Continuation.sequence continuations finalContinuation
            | SynPat.LongIdent (longDotId, ident, svtd, ctorArgs, access, range) ->
                { Type = SynPat_LongIdent
                  Range = r range
                  Properties =
                      p [ if ident.IsSome then
                              yield "ident" ==> (ident.Value |> i)
                          yield "longDotId" ==> lid longDotId
                          if access.IsSome then
                              yield "access" ==> (access.Value |> visitSynAccess) ]
                  FsAstNode = sp }
                :: [ if svtd.IsSome then
                         yield! visitSynValTyparDecls svtd.Value
                     yield! visitSynConstructorArgs ctorArgs ]
                |> finalContinuation
            | SynPat.Tuple (isStruct, pats, range) ->
                let continuations : ((Node list -> Node list) -> Node list) list = pats |> List.map visit

                let finalContinuation (nodes: Node list list) : Node list =
                    [ { Type = SynPat_Tuple
                        Range = r range
                        Properties = p [ "isStruct" ==> isStruct ]
                        FsAstNode = sp }
                      yield! (List.collect id nodes) ]
                    |> finalContinuation

                Continuation.sequence continuations finalContinuation
            | SynPat.Paren (pat, range) ->
                visit
                    pat
                    (fun nodes ->
                        { Type = SynPat_Paren
                          Range = r range
                          Properties = p []
                          FsAstNode = sp }
                        :: nodes
                        |> finalContinuation)
            | SynPat.ArrayOrList (_, pats, range) ->
                let continuations : ((Node list -> Node list) -> Node list) list = pats |> List.map visit

                let finalContinuation (nodes: Node list list) : Node list =
                    [ { Type = SynPat_ArrayOrList
                        Range = r range
                        Properties = p []
                        FsAstNode = sp }
                      yield! (List.collect id nodes) ]
                    |> finalContinuation

                Continuation.sequence continuations finalContinuation
            | SynPat.Record (pats, range) ->
                let continuations : ((Node list -> Node list) -> Node list) list = pats |> List.map (snd >> visit)

                let finalContinuation (nodes: Node list list) : Node list =
                    [ { Type = SynPat_Record
                        Range = r range
                        Properties = p []
                        FsAstNode = sp }
                      yield! (List.collect id nodes) ]
                    |> finalContinuation

                Continuation.sequence continuations finalContinuation
            | SynPat.Null (range) ->
                { Type = SynPat_Null
                  Range = r range
                  Properties = p []
                  FsAstNode = sp }
                |> List.singleton
                |> finalContinuation
            | SynPat.OptionalVal (ident, range) ->
                { Type = SynPat_OptionalVal
                  Range = r range
                  Properties = p [ "ident" ==> i ident ]
                  FsAstNode = sp }
                |> List.singleton
                |> finalContinuation
            | SynPat.IsInst (typ, range) ->
                { Type = SynPat_IsInst
                  Range = r range
                  Properties = p []
                  FsAstNode = sp }
                :: visitSynType typ
                |> finalContinuation
            | SynPat.QuoteExpr (expr, range) ->
                { Type = SynPat_QuoteExpr
                  Range = r range
                  Properties = p []
                  FsAstNode = sp }
                :: visitSynExpr expr
                |> finalContinuation
            | SynPat.DeprecatedCharRange (c, c2, range) ->
                { Type = SynPat_DeprecatedCharRange
                  Range = r range
                  Properties = p [ "c" ==> c; "c2" ==> c2 ]
                  FsAstNode = sp }
                |> List.singleton
                |> finalContinuation
            | SynPat.InstanceMember (ident, ident2, ident3, access, range) ->
                { Type = SynPat_InstanceMember
                  Range = r range
                  Properties =
                      p [ yield "ident" ==> i ident
                          yield "ident2" ==> i ident2
                          if ident3.IsSome then
                              yield "ident3" ==> (ident3.Value |> i)
                          if access.IsSome then
                              yield "access" ==> (access.Value |> visitSynAccess) ]
                  FsAstNode = sp }
                |> List.singleton
                |> finalContinuation
            | SynPat.FromParseError (pat, range) ->
                visit
                    pat
                    (fun nodes ->
                        { Type = SynPat_FromParseError
                          Range = r range
                          Properties = p []
                          FsAstNode = sp }
                        :: nodes
                        |> finalContinuation)

        visit sp id

    and visitSynConstructorArgs (ctorArgs: SynArgPats) : Node list =
        match ctorArgs with
        | Pats (pats) ->
            { Type = Pats_
              Range = noRange
              Properties = p []
              FsAstNode = ctorArgs }
            :: (List.collect visitSynPat pats)
        | NamePatPairs (pats, range) ->
            { Type = NamePatPairs_
              Range = r range
              Properties = p []
              FsAstNode = ctorArgs }
            :: (List.collect (snd >> visitSynPat) pats)

    and visitSynComponentInfo (sci: SynComponentInfo) : Node list =
        match sci with
        | ComponentInfo (attribs, typeParams, _, longId, _, preferPostfix, access, range) ->
            { Type = ComponentInfo_
              Range = r range
              Properties =
                  p [ yield "longIdent" ==> li longId
                      yield "preferPostfix" ==> preferPostfix
                      if access.IsSome then
                          yield "access" ==> (access.Value |> visitSynAccess) ]
              FsAstNode = sci }
            :: [ yield! (visitSynAttributeLists range attribs)
                 yield! (typeParams |> List.collect (visitSynTyparDecl)) ]

    and visitSynTypeDefnRepr (stdr: SynTypeDefnRepr) : Node list =
        match stdr with
        | SynTypeDefnRepr.ObjectModel (kind, members, range) ->
            { Type = SynTypeDefnRepr_ObjectModel
              Range = r range
              Properties = p []
              FsAstNode = stdr }
            :: [ yield! visitSynTypeDefnKind kind
                 yield! (members |> List.collect visitSynMemberDefn) ]
        | SynTypeDefnRepr.Simple (simpleRepr, range) ->
            { Type = SynTypeDefnRepr_Simple
              Range = r range
              Properties = p []
              FsAstNode = stdr }
            :: (visitSynTypeDefnSimpleRepr simpleRepr)
        | SynTypeDefnRepr.Exception (exceptionRepr) ->
            { Type = SynTypeDefnRepr_Exception
              Range = noRange
              Properties = p []
              FsAstNode = stdr }
            :: (visitSynExceptionDefnRepr exceptionRepr)

    and visitSynTypeDefnKind (kind: SynTypeDefnKind) : Node list =
        match kind with
        | TyconUnspecified ->
            { Type = SynTypeDefnKind_TyconUnspecified
              Range = noRange
              Properties = p []
              FsAstNode = kind }
            |> List.singleton
        | TyconClass ->
            { Type = SynTypeDefnKind_TyconClass
              Range = noRange
              Properties = p []
              FsAstNode = kind }
            |> List.singleton
        | TyconInterface ->
            { Type = SynTypeDefnKind_TyconInterface
              Range = noRange
              Properties = p []
              FsAstNode = kind }
            |> List.singleton
        | TyconStruct ->
            { Type = SynTypeDefnKind_TyconStruct
              Range = noRange
              Properties = p []
              FsAstNode = kind }
            |> List.singleton
        | TyconRecord ->
            { Type = SynTypeDefnKind_TyconRecord
              Range = noRange
              Properties = p []
              FsAstNode = kind }
            |> List.singleton
        | TyconUnion ->
            { Type = SynTypeDefnKind_TyconUnion
              Range = noRange
              Properties = p []
              FsAstNode = kind }
            |> List.singleton
        | TyconAbbrev ->
            { Type = SynTypeDefnKind_TyconAbbrev
              Range = noRange
              Properties = p []
              FsAstNode = kind }
            |> List.singleton
        | TyconHiddenRepr ->
            { Type = SynTypeDefnKind_TyconHiddenRepr
              Range = noRange
              Properties = p []
              FsAstNode = kind }
            |> List.singleton
        | TyconAugmentation ->
            { Type = SynTypeDefnKind_TyconAugmentation
              Range = noRange
              Properties = p []
              FsAstNode = kind }
            |> List.singleton
        | TyconILAssemblyCode ->
            { Type = SynTypeDefnKind_TyconILAssemblyCode
              Range = noRange
              Properties = p []
              FsAstNode = kind }
            |> List.singleton
        | TyconDelegate (typ, valinfo) ->
            { Type = SynTypeDefnKind_TyconDelegate
              Range = noRange
              Properties = p []
              FsAstNode = kind }
            :: [ yield! visitSynType typ
                 yield! visitSynValInfo valinfo ]

    and visitSynTypeDefnSimpleRepr (arg: SynTypeDefnSimpleRepr) =
        match arg with
        | SynTypeDefnSimpleRepr.None (range) ->
            { Type = SynTypeDefnSimpleRepr_None
              Range = r range
              Properties = p []
              FsAstNode = arg }
            |> List.singleton
        | SynTypeDefnSimpleRepr.Union (access, unionCases, range) ->
            { Type = SynTypeDefnSimpleRepr_Union
              Range = r range
              Properties =
                  p [ if access.IsSome then
                          yield "access" ==> (access.Value |> visitSynAccess) ]
              FsAstNode = arg }
            :: (List.collect visitSynUnionCase unionCases)
        | SynTypeDefnSimpleRepr.Enum (enumCases, range) ->
            { Type = SynTypeDefnSimpleRepr_Enum
              Range = r range
              Properties = p []
              FsAstNode = arg }
            :: (List.collect visitSynEnumCase enumCases)
        | SynTypeDefnSimpleRepr.Record (access, recordFields, range) ->
            { Type = SynTypeDefnSimpleRepr_Record
              Range = r range
              Properties =
                  p [ if access.IsSome then
                          yield "access" ==> (access.Value |> visitSynAccess) ]
              FsAstNode = arg }
            :: (List.collect visitSynField recordFields)
        | SynTypeDefnSimpleRepr.General (_, _, _, _, _, _, _, range) ->
            { Type = SynTypeDefnSimpleRepr_General
              Range = r range
              Properties = p []
              FsAstNode = arg }
            |> List.singleton
        | SynTypeDefnSimpleRepr.LibraryOnlyILAssembly (_, range) ->
            { Type = SynTypeDefnSimpleRepr_LibraryOnlyILAssembly
              Range = r range
              Properties = p []
              FsAstNode = arg }
            |> List.singleton
        | SynTypeDefnSimpleRepr.TypeAbbrev (_, typ, range) ->
            { Type = SynTypeDefnSimpleRepr_TypeAbbrev
              Range = r range
              Properties = p []
              FsAstNode = arg }
            :: (visitSynType typ)
        | SynTypeDefnSimpleRepr.Exception (edr) ->
            { Type = SynTypeDefnSimpleRepr_Exception
              Range = noRange
              Properties = p []
              FsAstNode = arg }
            :: (visitSynExceptionDefnRepr edr)

    and visitSynExceptionDefn (exceptionDef: SynExceptionDefn) : Node list =
        match exceptionDef with
        | SynExceptionDefn (sedr, members, range) ->
            { Type = SynExceptionDefn_
              Range = r range
              Properties = p []
              FsAstNode = exceptionDef }
            :: [ yield! visitSynExceptionDefnRepr sedr
                 yield! (members |> List.collect visitSynMemberDefn) ]

    and visitSynExceptionDefnRepr (sedr: SynExceptionDefnRepr) : Node list =
        match sedr with
        | SynExceptionDefnRepr (attrs, unionCase, longId, _, access, range) ->
            { Type = SynExceptionDefnRepr_
              Range = r range
              Properties =
                  p [ if longId.IsSome then
                          yield "longIdent" ==> (longId.Value |> li)
                      if access.IsSome then
                          yield "access" ==> (access.Value |> visitSynAccess) ]
              FsAstNode = sedr }
            :: [ yield! (visitSynAttributeLists range attrs)
                 yield! visitSynUnionCase unionCase ]

    and visitSynAttribute (attr: SynAttribute) : Node list =
        { Type = SynAttribute_
          Range = r attr.Range
          Properties =
              p [ if attr.Target.IsSome then
                      yield "target" ==> i attr.Target.Value
                  yield "typeName" ==> lid attr.TypeName
                  yield
                      "appliesToGetterAndSetter"
                      ==> attr.AppliesToGetterAndSetter
                  yield "typeName" ==> lid attr.TypeName ]
          FsAstNode = attr }
        :: (visitSynExpr attr.ArgExpr)

    and visitSynAttributeLists (parentRange: Range) (attrs: SynAttributeList list) : Node list =
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

    and visitSynAttributeList (parentRange: Range) (attrs: SynAttributeList) : Node list =
        { Type = SynAttributeList_
          Range = r attrs.Range
          Properties = p [ "linesBetweenParent", box (parentRange.StartLine - attrs.Range.EndLine - 1) ]
          FsAstNode = attrs }
        :: (List.collect visitSynAttribute attrs.Attributes)

    and visitSynUnionCase (uc: SynUnionCase) : Node list =
        match uc with
        | UnionCase (attrs, ident, uct, _, access, range) ->
            { Type = UnionCase_
              Range = r range
              Properties =
                  p [ yield "ident" ==> i ident
                      if access.IsSome then
                          yield "access" ==> (access.Value |> visitSynAccess) ]
              FsAstNode = uc }
            :: [ yield! visitSynUnionCaseType uct
                 yield! (visitSynAttributeLists range attrs) ]

    and visitSynUnionCaseType (uct: SynUnionCaseType) =
        match uct with
        | UnionCaseFields (cases) ->
            { Type = UnionCaseFields_
              Range = noRange
              Properties = p []
              FsAstNode = uct }
            :: (List.collect visitSynField cases)
        | UnionCaseFullType (stype, valInfo) ->
            { Type = UnionCaseFullType_
              Range = noRange
              Properties = p []
              FsAstNode = uct }
            :: [ yield! visitSynType stype
                 yield! visitSynValInfo valInfo ]

    and visitSynEnumCase (sec: SynEnumCase) : Node list =
        match sec with
        | EnumCase (attrs, ident, _, _, range) ->
            { Type = EnumCase_
              Range = r range
              Properties = p []
              FsAstNode = sec }
            :: [ yield! (visitSynAttributeLists range attrs)
                 yield visitIdent ident ]

    and visitSynField (sfield: SynField) : Node list =
        match sfield with
        | Field (attrs, isStatic, ident, typ, _, _, access, range) ->
            let parentRange =
                Option.map (fun (i: Ident) -> i.idRange) ident
                |> Option.defaultValue range


            { Type = Field_
              Range = r range
              Properties =
                  p [ if ident.IsSome then
                          yield "ident" ==> (ident.Value |> i)
                      yield "isStatic" ==> isStatic
                      if access.IsSome then
                          yield "access" ==> (access.Value |> visitSynAccess) ]
              FsAstNode = sfield }
            :: [ yield! (visitSynAttributeLists parentRange attrs)
                 yield! visitSynType typ ]

    and visitSynType (st: SynType) =
        match st with
        | SynType.LongIdent (li) ->
            { Type = SynType_LongIdent
              Range = noRange
              Properties = p []
              FsAstNode = st }
            :: (visitLongIdentWithDots li)
        | SynType.App (typeName, lessRange, typeArgs, commaRanges, greaterRange, isPostfix, range) ->
            { Type = SynType_App
              Range = r range
              Properties =
                  p [ if lessRange.IsSome then
                          yield "lessRange" ==> (lessRange.Value |> r)
                      yield "commaRanges" ==> (commaRanges |> List.map r)
                      if greaterRange.IsSome then
                          yield "greaterRange" ==> (greaterRange.Value |> r)
                      yield "isPostfix" ==> isPostfix ]
              FsAstNode = st }
            :: [ yield! typeArgs |> List.collect visitSynType
                 yield! visitSynType typeName ]
        | SynType.LongIdentApp (typeName, longDotId, lessRange, typeArgs, commaRanges, greaterRange, range) ->
            { Type = SynType_LongIdentApp
              Range = r range
              Properties =
                  p [ yield "ident" ==> lid longDotId
                      if lessRange.IsSome then
                          yield "lessRange" ==> (lessRange.Value |> r)
                      yield "commaRanges" ==> (commaRanges |> List.map r)
                      if greaterRange.IsSome then
                          yield "greaterRange" ==> (greaterRange.Value |> r) ]
              FsAstNode = st }
            :: [ yield! typeArgs |> List.collect visitSynType
                 yield! visitSynType typeName ]
        | SynType.Tuple (isStruct, typeNames, range) ->
            { Type = SynType_Tuple
              Range = r range
              Properties = p [ "isStruct" ==> isStruct ]
              FsAstNode = st }
            :: (List.collect (snd >> visitSynType) typeNames)
        | SynType.Array (_, elementType, range) ->
            { Type = SynType_Array
              Range = r range
              Properties = p []
              FsAstNode = st }
            :: (visitSynType elementType)
        | SynType.Fun (argType, returnType, range) ->
            { Type = SynType_Fun
              Range = r range
              Properties = p []
              FsAstNode = st }
            :: [ yield! visitSynType argType
                 yield! visitSynType returnType ]
        | SynType.Var (genericName, range) ->
            { Type = SynType_Var
              Range = r range
              Properties = p []
              FsAstNode = st }
            :: (visitSynTypar genericName)
        | SynType.Anon (range) ->
            { Type = SynType_Anon
              Range = r range
              Properties = p []
              FsAstNode = st }
            |> List.singleton
        | SynType.WithGlobalConstraints (typeName, _, range) ->
            { Type = SynType_WithGlobalConstraints
              Range = r range
              Properties = p []
              FsAstNode = st }
            :: (visitSynType typeName)
        | SynType.HashConstraint (synType, range) ->
            { Type = SynType_HashConstraint
              Range = r range
              Properties = p []
              FsAstNode = st }
            :: (visitSynType synType)
        | SynType.MeasureDivide (dividendType, divisorType, range) ->
            { Type = SynType_MeasureDivide
              Range = r range
              Properties = p []
              FsAstNode = st }
            :: [ yield! visitSynType dividendType
                 yield! visitSynType divisorType ]
        | SynType.MeasurePower (measureType, _, range) ->
            { Type = SynType_MeasurePower
              Range = r range
              Properties = p []
              FsAstNode = st }
            :: (visitSynType measureType)
        | SynType.StaticConstant (constant, range) ->
            { Type = SynType_StaticConstant
              Range = r range
              Properties = p []
              FsAstNode = st }
            :: [ (visitSynConst range constant) ]
        | SynType.StaticConstantExpr (expr, range) ->
            { Type = SynType_StaticConstantExpr
              Range = r range
              Properties = p []
              FsAstNode = st }
            :: (visitSynExpr expr)
        | SynType.StaticConstantNamed (expr, typ, range) ->
            { Type = SynType_StaticConstantNamed
              Range = r range
              Properties = p []
              FsAstNode = st }
            :: [ yield! visitSynType expr
                 yield! visitSynType typ ]
        | SynType.AnonRecd (isStruct, typeNames, range) ->
            { Type = SynType_AnonRecd
              Range = r range
              Properties = p [ "isStruct" ==> isStruct ]
              FsAstNode = st }
            :: (List.collect visitAnonRecordTypeField typeNames)
        | SynType.Paren (innerType, range) ->
            { Type = SynType_Paren
              Range = r range
              Properties = p []
              FsAstNode = st }
            :: (visitSynType innerType)

    and visitSynConst (parentRange: Range) (sc: SynConst) : Node =
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

        { Type = t
          Range = r (sc.Range parentRange)
          Properties = p []
          FsAstNode = sc }

    and visitSynValInfo (svi: SynValInfo) =
        match svi with
        | SynValInfo (args, arg) ->
            { Type = SynValInfo_
              Range = noRange
              Properties = p []
              FsAstNode = svi }
            :: [ yield!
                     args
                     |> List.collect (List.collect visitSynArgInfo)
                 yield! visitSynArgInfo arg ]

    and visitSynArgInfo (sai: SynArgInfo) : Node list =
        match sai with
        | SynArgInfo (attrs, optional, ident) ->
            let parentRange =
                ident
                |> Option.map (fun i -> i.idRange)
                |> Option.defaultValue range.Zero

            { Type = SynArgInfo_
              Range = noRange
              Properties =
                  p [ if ident.IsSome then
                          yield "ident" ==> i ident.Value
                      yield "optional" ==> optional ]
              FsAstNode = sai }
            :: (visitSynAttributeLists parentRange attrs)

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

    and visitParsedHashDirective (hash: ParsedHashDirective) : Node =
        match hash with
        | ParsedHashDirective (ident, longIdent, range) ->
            { Type = ParsedHashDirective_
              Range = r range
              Properties =
                  p [ "ident" ==> ident
                      "longIdent" ==> longIdent ]
              FsAstNode = hash }

    and visitSynModuleOrNamespaceSig (modOrNs: SynModuleOrNamespaceSig) : Node list =
        match modOrNs with
        | SynModuleOrNamespaceSig (longIdent, isRecursive, synModuleOrNamespaceKind, decls, _, attrs, access, range) ->
            let typeName =
                match synModuleOrNamespaceKind with
                | SynModuleOrNamespaceKind.AnonModule -> SynModuleOrNamespaceSig_AnonModule
                | SynModuleOrNamespaceKind.NamedModule -> SynModuleOrNamespaceSig_NamedModule
                | SynModuleOrNamespaceKind.DeclaredNamespace -> SynModuleOrNamespaceSig_DeclaredNamespace
                | SynModuleOrNamespaceKind.GlobalNamespace -> SynModuleOrNamespaceSig_GlobalNamespace


            { Type = typeName
              Range = r range
              Properties =
                  p [ yield "isRecursive" ==> isRecursive
                      yield "isModule" ==> synModuleOrNamespaceKind
                      yield "longIdent" ==> li longIdent
                      if access.IsSome then
                          yield "access" ==> (access.Value |> visitSynAccess) ]
              FsAstNode = modOrNs }
            :: [ yield!
                     if synModuleOrNamespaceKind = SynModuleOrNamespaceKind.DeclaredNamespace then
                         visitLongIdent longIdent
                     else
                         []
                 yield! (visitSynAttributeLists range attrs)
                 yield! (decls |> List.collect visitSynModuleSigDecl) ]

    and visitSynModuleSigDecl (ast: SynModuleSigDecl) : Node list =
        match ast with
        | SynModuleSigDecl.ModuleAbbrev (ident, longIdent, range) ->
            { Type = SynModuleSigDecl_ModuleAbbrev
              Range = r range
              Properties =
                  p [ "ident" ==> i ident
                      "longIdent" ==> li longIdent ]
              FsAstNode = ast }
            |> List.singleton
        | SynModuleSigDecl.NestedModule (sci, isRecursive, decls, range) ->
            { Type = SynModuleSigDecl_NestedModule
              Range = r range
              Properties = p [ "isRecursive" ==> isRecursive ]
              FsAstNode = ast }
            :: [ yield! visitSynComponentInfo sci
                 yield! (decls |> List.collect visitSynModuleSigDecl) ]
        | SynModuleSigDecl.Val (SynValSig.ValSpfn _ as node, _) -> visitSynValSig node
        | SynModuleSigDecl.Types (typeDefs, range) ->
            { Type = SynModuleSigDecl_Types
              Range = r range
              Properties = p []
              FsAstNode = ast }
            :: (List.collect visitSynTypeDefnSig typeDefs)
        | SynModuleSigDecl.Open (target, parentRange) ->
            // we use the parent ranges here to match up with the trivia parsed
            match target with
            | SynOpenDeclTarget.ModuleOrNamespace (longIdent, _range) ->
                { Type = SynModuleSigDecl_Open
                  Range = r parentRange
                  Properties = p [ "longIdent" ==> li longIdent ]
                  FsAstNode = target }
                |> List.singleton
            | SynOpenDeclTarget.Type (synType, _range) ->
                { Type = SynModuleSigDecl_OpenType
                  Range = r parentRange
                  Properties = p []
                  FsAstNode = target }
                :: (visitSynType synType)
        | SynModuleSigDecl.HashDirective (hash, range) ->
            [ { Type = SynModuleSigDecl_HashDirective
                Range = r range
                Properties = p []
                FsAstNode = ast }
              (visitParsedHashDirective hash) ]
        | SynModuleSigDecl.NamespaceFragment (moduleOrNamespace) ->
            { Type = SynModuleSigDecl_NamespaceFragment
              Range = noRange
              Properties = p []
              FsAstNode = ast }
            :: (visitSynModuleOrNamespaceSig moduleOrNamespace)
        | SynModuleSigDecl.Exception (synExceptionSig, range) ->
            { Type = SynModuleSigDecl_Exception
              Range = r range
              Properties = p []
              FsAstNode = ast }
            :: (visitSynExceptionSig synExceptionSig)

    and visitSynExceptionSig (exceptionDef: SynExceptionSig) : Node list =
        match exceptionDef with
        | SynExceptionSig (sedr, members, range) ->
            { Type = SynExceptionSig_
              Range = r range
              Properties = p []
              FsAstNode = exceptionDef }
            :: [ yield! visitSynExceptionDefnRepr sedr
                 yield! (members |> List.collect visitSynMemberSig) ]

    and visitLongIdentWithDots (lid: LongIdentWithDots) : Node list =
        match lid with
        | LongIdentWithDots (ids, _) -> List.map visitIdent ids

    and visitLongIdent (li: LongIdent) : Node list = List.map visitIdent li

    and visitIdent (ident: Ident) : Node =
        { Type = Ident_
          Range = r ident.idRange
          Properties = Map.empty
          FsAstNode = ident }

let astToNode (hds: ParsedHashDirective list) (mdls: SynModuleOrNamespace list) : Node list =
    let children =
        [ yield! List.collect Ast.visitSynModuleOrNamespace mdls
          yield! List.map Ast.visitParsedHashDirective hds ]

    { Type = File_
      Range = None
      Properties = Map.empty
      FsAstNode = mdls }
    :: children

let sigAstToNode (ast: SynModuleOrNamespaceSig list) : Node list =
    let children =
        List.collect Ast.visitSynModuleOrNamespaceSig ast

    { Type = SigFile_
      Range = None
      Properties = Map.empty
      FsAstNode = ast }
    :: children
