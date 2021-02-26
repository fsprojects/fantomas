module Fantomas.AstTransformer

open FSharp.Compiler.Text
open FSharp.Compiler.SyntaxTree
open Fantomas.TriviaTypes

let rec (|Sequentials|_|) =
    function
    | SynExpr.Sequential (_, isTrueSeq, e, Sequentials es, range) -> Some((isTrueSeq, e, None, range) :: es)
    | SynExpr.Sequential (_, isTrueSeq, e1, e2, range) -> Some [ isTrueSeq, e1, Some e2, range ]
    | _ -> None

type Id = { Ident: string; Range: Range option }

type FsAstNode = obj

type Node =
    { Type: FsAstType
      Range: Range option
      Properties: Map<string, obj>
      Childs: Node list
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

    let rec visit (ast: SynModuleOrNamespace) : Node = visitSynModuleOrNamespace ast

    and visitSynModuleOrNamespace (modOrNs: SynModuleOrNamespace) : Node =
        match modOrNs with
        | SynModuleOrNamespace (longIdent, isRecursive, synModuleOrNamespaceKind, decls, _, attrs, access, range) ->
            let collectIdents (idents: LongIdent) =
                idents
                |> List.map
                    (fun ident ->
                        { Type = Ident_
                          Range = r ident.idRange
                          Properties = Map.empty
                          FsAstNode = ident
                          Childs = [] })

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
              FsAstNode = modOrNs
              Childs =
                  [ yield!
                      if synModuleOrNamespaceKind = SynModuleOrNamespaceKind.DeclaredNamespace then
                          collectIdents longIdent
                      else
                          []
                    yield! (visitSynAttributeLists range attrs)
                    yield! (decls |> List.map visitSynModuleDecl) ] }

    and visitSynModuleDecl (ast: SynModuleDecl) : Node =
        match ast with
        | SynModuleDecl.ModuleAbbrev (ident, longIdent, range) ->
            { Type = SynModuleDecl_ModuleAbbrev
              Range = r range
              Properties =
                  p [ "ident" ==> i ident
                      "longIdent" ==> li longIdent ]
              FsAstNode = ast
              Childs = [] }
        | SynModuleDecl.NestedModule (sci, isRecursive, decls, _, range) ->
            { Type = SynModuleDecl_NestedModule
              Range = r range
              Properties = p [ "isRecursive" ==> isRecursive ]
              FsAstNode = ast
              Childs =
                  [ yield visitSynComponentInfo sci
                    yield! (decls |> List.map visitSynModuleDecl) ] }
        | SynModuleDecl.Let (_, bindings, range) ->
            { Type = SynModuleDecl_Let
              Range = r range
              Properties = p []
              FsAstNode = ast
              Childs = bindings |> List.map visitSynBinding }
        | SynModuleDecl.DoExpr (_, expr, range) ->
            { Type = SynModuleDecl_DoExpr
              Range = r range
              Properties = p []
              FsAstNode = ast
              Childs = [ visitSynExpr expr ] }
        | SynModuleDecl.Types (typeDefs, range) ->
            { Type = SynModuleDecl_Types
              Range = r range
              Properties = p []
              FsAstNode = ast
              Childs = typeDefs |> List.map visitSynTypeDefn }
        | SynModuleDecl.Exception (exceptionDef, range) ->
            { Type = SynModuleDecl_Exception
              Range = r range
              Properties = p []
              FsAstNode = ast
              Childs = [ visitSynExceptionDefn exceptionDef ] }
        | SynModuleDecl.Open (target, parentRange) ->
            // we use the parent ranges here to match up with the trivia parsed
            match target with
            | SynOpenDeclTarget.ModuleOrNamespace (longIdent, _range) ->
                { Type = SynModuleDecl_Open
                  Range = r parentRange
                  Properties = p [ "longIdent" ==> li longIdent ]
                  FsAstNode = ast
                  Childs = [] }
            | SynOpenDeclTarget.Type (synType, _range) ->
                { Type = SynModuleDecl_OpenType
                  Range = r parentRange
                  Properties = p []
                  FsAstNode = ast
                  Childs = [ visitSynType synType ] }
        | SynModuleDecl.Attributes (attrs, range) ->
            { Type = SynModuleDecl_Attributes
              Range = r range
              Properties = p []
              FsAstNode = ast
              Childs = visitSynAttributeLists range attrs }
        | SynModuleDecl.HashDirective (hash, range) ->
            { Type = SynModuleDecl_HashDirective
              Range = r range
              Properties = p []
              FsAstNode = ast
              Childs = [ visitParsedHashDirective hash ] }
        | SynModuleDecl.NamespaceFragment (moduleOrNamespace) ->
            { Type = SynModuleDecl_NamespaceFragment
              Range = noRange
              Properties = p []
              FsAstNode = ast
              Childs = [ visitSynModuleOrNamespace moduleOrNamespace ] }

    and visitSynExpr (synExpr: SynExpr) : Node =
        match synExpr with
        | SynExpr.Paren (expr, leftParenRange, rightParenRange, range) ->
            { Type = SynExpr_Paren
              Range = r range
              Properties =
                  p [ yield "leftParenRange" ==> r leftParenRange
                      if rightParenRange.IsSome then
                          yield "rightParenRange" ==> r rightParenRange.Value ]
              FsAstNode = synExpr
              Childs = [ yield visitSynExpr expr ] }
        | SynExpr.Quote (operator, isRaw, quotedSynExpr, isFromQueryExpression, range) ->
            { Type = SynExpr_Quote
              Range = r range
              Properties =
                  p [ "isRaw" ==> isRaw
                      "isFromQueryExpression" ==> isFromQueryExpression ]
              FsAstNode = synExpr
              Childs =
                  [ yield visitSynExpr operator
                    yield visitSynExpr quotedSynExpr ] }
        | SynExpr.Const (constant, range) ->
            { Type = SynExpr_Const
              Range = r range
              Properties = p []
              FsAstNode = synExpr
              Childs = [ visitSynConst range constant ] }
        | SynExpr.Typed (expr, typeName, range) ->
            { Type = SynExpr_Typed
              Range = r range
              Properties = p []
              FsAstNode = synExpr
              Childs =
                  [ yield visitSynExpr expr
                    yield visitSynType typeName ] }
        | SynExpr.Tuple (isStruct, exprs, commaRanges, range) ->
            { Type = SynExpr_Tuple
              Range = r range
              Properties =
                  p [ "isStruct" ==> isStruct
                      "commaRanges" ==> (commaRanges |> List.map r) ]
              FsAstNode = synExpr
              Childs = [ yield! exprs |> List.map visitSynExpr ] }
        | SynExpr.ArrayOrList (isList, exprs, range) ->
            { Type = SynExpr_ArrayOrList
              Range = r range
              Properties = p [ "isList" ==> isList ]
              FsAstNode = synExpr
              Childs = [ yield! exprs |> List.map visitSynExpr ] }
        | SynExpr.Record (_, _, recordFields, range) ->
            { Type = SynExpr_Record
              Range = r range
              Properties = p []
              FsAstNode = synExpr
              Childs = [ yield! recordFields |> List.map visitRecordField ] }
        | SynExpr.AnonRecd (_, _, recordFields, range) ->
            { Type = SynExpr_AnonRecd
              Range = r range
              Properties = p []
              FsAstNode = synExpr
              Childs = [ yield! recordFields |> List.map visitAnonRecordField ] }
        | SynExpr.New (isProtected, typeName, expr, range) ->
            { Type = SynExpr_New
              Range = r range
              Properties = p [ "isProtected" ==> isProtected ]
              FsAstNode = synExpr
              Childs =
                  [ yield visitSynExpr expr
                    yield visitSynType typeName ] }
        | SynExpr.ObjExpr (objType, argOptions, bindings, extraImpls, newExprRange, range) ->
            { Type = SynExpr_ObjExpr
              Range = r range
              Properties = p [ "newExprRange" ==> r newExprRange ]
              FsAstNode = synExpr
              Childs =
                  [ yield visitSynType objType
                    if argOptions.IsSome then
                        yield visitArgsOption argOptions.Value
                    yield! extraImpls |> List.map visitSynInterfaceImpl
                    yield! bindings |> List.map visitSynBinding ] }
        | SynExpr.While (_, whileExpr, doExpr, range) ->
            { Type = SynExpr_While
              Range = r range
              Properties = p []
              FsAstNode = synExpr
              Childs =
                  [ yield visitSynExpr whileExpr
                    yield visitSynExpr doExpr ] }
        | SynExpr.For (_, ident, identBody, _, toBody, doBody, range) ->
            { Type = SynExpr_For
              Range = r range
              Properties = p [ "ident" ==> i ident ]
              FsAstNode = synExpr
              Childs =
                  [ yield visitSynExpr identBody
                    yield visitSynExpr toBody
                    yield visitSynExpr doBody ] }
        | SynExpr.ForEach (_, (SeqExprOnly seqExprOnly), isFromSource, pat, enumExpr, bodyExpr, range) ->
            { Type = SynExpr_ForEach
              Range = r range
              Properties =
                  p [ "isFromSource" ==> isFromSource
                      "seqExprOnly" ==> seqExprOnly ]
              FsAstNode = synExpr
              Childs =
                  [ yield visitSynPat pat
                    yield visitSynExpr enumExpr
                    yield visitSynExpr bodyExpr ] }
        | SynExpr.ArrayOrListOfSeqExpr (isArray, expr, range) ->
            { Type = SynExpr_ArrayOrListOfSeqExpr
              Range = r range
              Properties = p [ "isArray" ==> isArray ]
              FsAstNode = synExpr
              Childs = [ yield visitSynExpr expr ] }
        | SynExpr.CompExpr (isArrayOrList, isNotNakedRefCell, expr, range) ->
            { Type = SynExpr_CompExpr
              Range = r range
              Properties =
                  p [ "isArrayOrList" ==> isArrayOrList
                      "isNotNakedRefCell" ==> isNotNakedRefCell ]
              FsAstNode = synExpr
              Childs = [ yield visitSynExpr expr ] }
        | SynExpr.Lambda (fromMethod, inLambdaSeq, args, body, _parsedData, range) ->
            { Type = SynExpr_Lambda
              Range = r range
              Properties =
                  p [ "fromMethod" ==> fromMethod
                      "inLambdaSeq" ==> inLambdaSeq ]
              FsAstNode = synExpr
              Childs =
                  [ yield visitSynSimplePats args
                    yield visitSynExpr body ] }
        | SynExpr.MatchLambda (isExnMatch, _, matchClauses, _, range) ->
            { Type = SynExpr_MatchLambda
              Range = r range
              Properties = p [ "isExnMatch" ==> isExnMatch ]
              FsAstNode = synExpr
              Childs = [ yield! matchClauses |> List.map visitSynMatchClause ] }
        | SynExpr.Match (_, expr, clauses, range) ->
            { Type = SynExpr_Match
              Range = r range
              Properties = p []
              FsAstNode = synExpr
              Childs =
                  [ yield visitSynExpr expr
                    yield! clauses |> List.map visitSynMatchClause ] }
        | SynExpr.Do (expr, range) ->
            { Type = SynExpr_Do
              Range = r range
              Properties = p []
              FsAstNode = synExpr
              Childs = [ yield visitSynExpr expr ] }
        | SynExpr.Assert (expr, range) ->
            { Type = SynExpr_Assert
              Range = r range
              Properties = p []
              FsAstNode = synExpr
              Childs = [ yield visitSynExpr expr ] }
        | SynExpr.App (atomicFlag, isInfix, funcExpr, argExpr, range) ->
            { Type = SynExpr_App
              Range = r range
              Properties =
                  p [ "atomicFlag"
                      ==> (match atomicFlag with
                           | ExprAtomicFlag.Atomic -> "Atomic"
                           | _ -> "Not Atomic")
                      "isInfix" ==> isInfix ]
              FsAstNode = synExpr
              Childs =
                  [ yield visitSynExpr funcExpr
                    yield visitSynExpr argExpr ] }
        | SynExpr.TypeApp (expr, lESSrange, typeNames, commaRanges, gREATERrange, typeArgsRange, range) ->
            { Type = SynExpr_TypeApp
              Range = r range
              Properties =
                  p [ yield "lESSrange" ==> r lESSrange
                      yield "commaRanges" ==> (commaRanges |> List.map r)
                      if gREATERrange.IsSome then
                          yield "gREATERrange" ==> r gREATERrange.Value
                      yield "typeArgsRange" ==> r typeArgsRange ]
              FsAstNode = synExpr
              Childs =
                  [ yield visitSynExpr expr
                    yield! typeNames |> List.map visitSynType ] }
        | SynExpr.LetOrUse (isRecursive, isUse, bindings, body, range) ->
            { Type = SynExpr_LetOrUse
              Range = r range
              Properties =
                  p [ "isRecursive" ==> isRecursive
                      "isUse" ==> isUse ]
              FsAstNode = synExpr
              Childs =
                  [ yield! bindings |> List.map visitSynBinding
                    yield visitSynExpr body ] }
        | SynExpr.TryWith (tryExpr, tryRange, withCases, withRange, range, _, _) ->
            { Type = SynExpr_TryWith
              Range = r range
              Properties =
                  p [ "tryRange" ==> r tryRange
                      "withRange" ==> r withRange ]
              FsAstNode = synExpr
              Childs =
                  [ yield visitSynExpr tryExpr
                    yield! withCases |> List.map visitSynMatchClause ] }
        | SynExpr.TryFinally (tryExpr, finallyExpr, range, _, _) ->
            { Type = SynExpr_TryFinally
              Range = r range
              Properties = p []
              FsAstNode = synExpr
              Childs =
                  [ yield visitSynExpr tryExpr
                    yield visitSynExpr finallyExpr ] }
        | SynExpr.Lazy (ex, range) ->
            { Type = SynExpr_Lazy
              Range = r range
              Properties = p []
              FsAstNode = synExpr
              Childs = [ yield visitSynExpr ex ] }
        | Sequentials xs ->
            let rec cons xs =
                match xs with
                | [] -> failwith "should not happen" // expr2Opt is always Some in last item
                | ((isTrueSeq, expr1, expr2Opt, range) :: rest) ->
                    { Type = SynExpr_Sequential
                      Range = r range
                      Properties = p [ "isTrueSeq" ==> isTrueSeq ]
                      FsAstNode = synExpr
                      Childs =
                          [ yield visitSynExpr expr1
                            yield
                                expr2Opt
                                |> Option.map visitSynExpr
                                |> Option.defaultWith (fun () -> cons rest) ] }

            cons xs
        | SynExpr.Sequential (_, isTrueSeq, expr1, expr2, range) ->
            { Type = SynExpr_Sequential
              Range = r range
              Properties = p [ "isTrueSeq" ==> isTrueSeq ]
              FsAstNode = synExpr
              Childs =
                  [ yield visitSynExpr expr1
                    yield visitSynExpr expr2 ] }
        | SynExpr.SequentialOrImplicitYield (seqPoint, expr1, expr2, ifNotStmt, range) ->
            { Type = SynExpr_SequentialOrImplicitYield
              Range = r range
              FsAstNode = synExpr
              Properties = p [ "seqPoint" ==> seqPoint ]
              Childs =
                  [ yield visitSynExpr expr1
                    yield visitSynExpr expr2
                    yield visitSynExpr ifNotStmt ] }
        | SynExpr.IfThenElse (ifExpr, thenExpr, elseExpr, _, isFromErrorRecovery, ifToThenRange, range) ->
            { Type = SynExpr_IfThenElse
              Range = r range
              Properties =
                  p [ "isFromErrorRecovery" ==> isFromErrorRecovery
                      "ifToThenRange" ==> r ifToThenRange ]
              FsAstNode = synExpr
              Childs =
                  [ yield visitSynExpr ifExpr
                    yield visitSynExpr thenExpr
                    if elseExpr.IsSome then
                        yield visitSynExpr elseExpr.Value ] }
        | SynExpr.Ident (id) ->
            { Type = SynExpr_Ident
              Range = (i id).Range
              Properties = p [ "ident" ==> i id ]
              FsAstNode = synExpr
              Childs = [] }
        | SynExpr.LongIdent (isOptional, longDotId, _, range) ->
            let ids = visitLongIdentWithDots longDotId

            { Type = SynExpr_LongIdent
              Range = r range
              Properties =
                  p [ "isOptional" ==> isOptional
                      "longDotId" ==> lid longDotId ]
              FsAstNode = synExpr
              Childs = ids }
        | SynExpr.LongIdentSet (longDotId, expr, range) ->
            { Type = SynExpr_LongIdentSet
              Range = r range
              Properties = p [ "longDotId" ==> lid longDotId ]
              FsAstNode = synExpr
              Childs = [ yield visitSynExpr expr ] }
        | SynExpr.DotGet (expr, rangeOfDot, longDotId, range) ->
            // Idents are collected as childs here to deal with unit test ``Fluent api with comments should remain on same lines``
            let ids = visitLongIdentWithDots longDotId

            { Type = SynExpr_DotGet
              Range = r range
              Properties =
                  p [ "rangeOfDot" ==> r rangeOfDot
                      "longDotId" ==> lid longDotId ]
              FsAstNode = synExpr
              Childs = [ yield visitSynExpr expr; yield! ids ] }
        | SynExpr.DotSet (expr, longDotId, e2, range) ->
            { Type = SynExpr_DotSet
              Range = r range
              Properties = p [ "longDotId" ==> lid longDotId ]
              FsAstNode = synExpr
              Childs =
                  [ yield visitSynExpr expr
                    yield visitSynExpr e2 ] }
        | SynExpr.Set (e1, e2, range) ->
            { Type = SynExpr_Set
              Range = r range
              Properties = p []
              FsAstNode = synExpr
              Childs =
                  [ yield visitSynExpr e1
                    yield visitSynExpr e2 ] }
        | SynExpr.DotIndexedGet (objectExpr, indexExprs, dotRange, range) ->
            { Type = SynExpr_DotIndexedGet
              Range = r range
              Properties = p [ "dotRange" ==> r dotRange ]
              FsAstNode = synExpr
              Childs =
                  [ yield visitSynExpr objectExpr
                    yield! indexExprs |> List.map visitSynIndexerArg ] }
        | SynExpr.DotIndexedSet (objectExpr, indexExprs, valueExpr, leftOfSetRange, dotRange, range) ->
            { Type = SynExpr_DotIndexedSet
              Range = r range
              Properties =
                  p [ "leftOfSetRange" ==> r leftOfSetRange
                      "dotRange" ==> r dotRange ]
              FsAstNode = synExpr
              Childs =
                  [ yield visitSynExpr objectExpr
                    yield! indexExprs |> List.map visitSynIndexerArg
                    yield visitSynExpr valueExpr ] }
        | SynExpr.NamedIndexedPropertySet (longDotId, e1, e2, range) ->
            { Type = SynExpr_NamedIndexedPropertySet
              Range = r range
              Properties = p [ "longDotId" ==> lid longDotId ]
              FsAstNode = synExpr
              Childs =
                  [ yield visitSynExpr e1
                    yield visitSynExpr e2 ] }
        | SynExpr.DotNamedIndexedPropertySet (expr, longDotId, e1, e2, range) ->
            { Type = SynExpr_DotNamedIndexedPropertySet
              Range = r range
              Properties = p [ "longDotId" ==> lid longDotId ]
              FsAstNode = synExpr
              Childs =
                  [ yield visitSynExpr expr
                    yield visitSynExpr e1
                    yield visitSynExpr e2 ] }
        | SynExpr.TypeTest (expr, typeName, range) ->
            { Type = SynExpr_TypeTest
              Range = r range
              Properties = p []
              FsAstNode = synExpr
              Childs =
                  [ yield visitSynExpr expr
                    yield visitSynType typeName ] }
        | SynExpr.Upcast (expr, typeName, range) ->
            { Type = SynExpr_Upcast
              Range = r range
              Properties = p []
              FsAstNode = synExpr
              Childs =
                  [ yield visitSynExpr expr
                    yield visitSynType typeName ] }
        | SynExpr.Downcast (expr, typeName, range) ->
            { Type = SynExpr_Downcast
              Range = r range
              Properties = p []
              FsAstNode = synExpr
              Childs =
                  [ yield visitSynExpr expr
                    yield visitSynType typeName ] }
        | SynExpr.InferredUpcast (expr, range) ->
            { Type = SynExpr_InferredUpcast
              Range = r range
              Properties = p []
              FsAstNode = synExpr
              Childs = [ yield visitSynExpr expr ] }
        | SynExpr.InferredDowncast (expr, range) ->
            { Type = SynExpr_InferredDowncast
              Range = r range
              Properties = p []
              FsAstNode = synExpr
              Childs = [ yield visitSynExpr expr ] }
        | SynExpr.Null (range) ->
            { Type = SynExpr_Null
              Range = r range
              Properties = p []
              FsAstNode = synExpr
              Childs = [] }
        | SynExpr.AddressOf (isByref, expr, refRange, range) ->
            { Type = SynExpr_AddressOf
              Range = r range
              Properties =
                  p [ "isByref" ==> isByref
                      "refRange" ==> r refRange ]
              FsAstNode = synExpr
              Childs = [ yield visitSynExpr expr ] }
        | SynExpr.TraitCall (typars, sign, expr, range) ->
            { Type = SynExpr_TraitCall
              Range = r range
              Properties = p []
              FsAstNode = synExpr
              Childs =
                  [ yield! typars |> List.map visitSynTypar
                    yield visitSynMemberSig sign
                    yield visitSynExpr expr ] }
        | SynExpr.JoinIn (expr, inrange, expr2, range) ->
            { Type = SynExpr_JoinIn
              Range = r range
              Properties = p [ "inRange" ==> r inrange ]
              FsAstNode = synExpr
              Childs =
                  [ yield visitSynExpr expr
                    yield visitSynExpr expr2 ] }
        | SynExpr.ImplicitZero (range) ->
            { Type = SynExpr_ImplicitZero
              Range = r range
              Properties = p []
              FsAstNode = synExpr
              Childs = [] }
        | SynExpr.YieldOrReturn (_, expr, range) ->
            { Type = SynExpr_YieldOrReturn
              Range = r range
              Properties = p []
              FsAstNode = synExpr
              Childs = [ yield visitSynExpr expr ] }
        | SynExpr.YieldOrReturnFrom (_, expr, range) ->
            { Type = SynExpr_YieldOrReturnFrom
              Range = r range
              Properties = p []
              FsAstNode = synExpr
              Childs = [ yield visitSynExpr expr ] }
        | SynExpr.LetOrUseBang (_, isUse, isFromSource, pat, rhsExpr, andBangs, body, range) ->
            { Type = SynExpr_LetOrUseBang
              Range = r range
              Properties =
                  p [ "isUse" ==> isUse
                      "isFromSource" ==> isFromSource ]
              FsAstNode = synExpr
              Childs =
                  [ yield visitSynPat pat
                    yield visitSynExpr rhsExpr
                    yield!
                        andBangs
                        |> List.collect (fun (_, _, _, pat, body, _) -> visitSynPat pat :: [ visitSynExpr body ])
                    yield visitSynExpr body ] }
        | SynExpr.MatchBang (_, expr, clauses, range) ->
            { Type = SynExpr_MatchBang
              Range = r range
              Properties = p []
              FsAstNode = synExpr
              Childs =
                  [ yield visitSynExpr expr
                    yield! clauses |> List.map visitSynMatchClause ] }
        | SynExpr.DoBang (expr, range) ->
            { Type = SynExpr_DoBang
              Range = r range
              Properties = p []
              FsAstNode = synExpr
              Childs = [ yield visitSynExpr expr ] }
        | SynExpr.LibraryOnlyILAssembly (_, _, _, _, range) ->
            { Type = SynExpr_LibraryOnlyILAssembly
              Range = r range
              Properties = p []
              FsAstNode = synExpr
              Childs = [] }
        | SynExpr.LibraryOnlyStaticOptimization (_, _, _, range) ->
            { Type = SynExpr_LibraryOnlyStaticOptimization
              Range = r range
              Properties = p []
              FsAstNode = synExpr
              Childs = [] }
        | SynExpr.LibraryOnlyUnionCaseFieldGet (expr, longId, _, range) ->
            { Type = SynExpr_LibraryOnlyUnionCaseFieldGet
              Range = r range
              Properties = p [ "longId" ==> li longId ]
              FsAstNode = synExpr
              Childs = [ yield visitSynExpr expr ] }
        | SynExpr.LibraryOnlyUnionCaseFieldSet (e1, longId, _, e2, range) ->
            { Type = SynExpr_LibraryOnlyUnionCaseFieldSet
              Range = r range
              Properties = p [ "longId" ==> li longId ]
              FsAstNode = synExpr
              Childs =
                  [ yield visitSynExpr e1
                    yield visitSynExpr e2 ] }
        | SynExpr.ArbitraryAfterError (debugStr, range) ->
            { Type = SynExpr_ArbitraryAfterError
              Range = r range
              Properties = p [ "debugStr" ==> debugStr ]
              FsAstNode = synExpr
              Childs = [] }
        | SynExpr.FromParseError (expr, range) ->
            { Type = SynExpr_FromParseError
              Range = r range
              Properties = p []
              FsAstNode = synExpr
              Childs = [ yield visitSynExpr expr ] }
        | SynExpr.DiscardAfterMissingQualificationAfterDot (expr, range) ->
            { Type = SynExpr_DiscardAfterMissingQualificationAfterDot
              Range = r range
              Properties = p []
              FsAstNode = synExpr
              Childs = [ yield visitSynExpr expr ] }
        | SynExpr.Fixed (expr, range) ->
            { Type = SynExpr_Fixed
              Range = r range
              Properties = p []
              FsAstNode = synExpr
              Childs = [ yield visitSynExpr expr ] }
        | SynExpr.InterpolatedString (parts, range) ->
            { Type = SynExpr_InterpolatedString
              Range = r range
              Properties = p []
              FsAstNode = synExpr
              Childs = List.map visitSynInterpolatedStringPart parts }

    and visitSynInterpolatedStringPart (synInterpolatedStringPart: SynInterpolatedStringPart) =
        match synInterpolatedStringPart with
        | SynInterpolatedStringPart.String (value, range) ->
            { Type = SynInterpolatedStringPart_String
              Range = r range
              Properties = p [ "value", box value ]
              FsAstNode = synInterpolatedStringPart
              Childs = [] }
        | SynInterpolatedStringPart.FillExpr (expr, ident) ->
            { Type = SynInterpolatedStringPart_FillExpr
              Range = None
              Properties = p []
              FsAstNode = synInterpolatedStringPart
              Childs =
                  [ visitSynExpr expr
                    yield! (Option.toList ident |> List.map visitIdent) ] }

    and visitRecordField ((longId, _) as rfn: RecordFieldName, expr: SynExpr option, _: BlockSeparator option) =
        { Type = RecordField_
          Range = r longId.Range
          Properties = p [ "ident" ==> lid longId ]
          FsAstNode = rfn
          Childs =
              [ if expr.IsSome then
                    yield visitSynExpr expr.Value ] }

    and visitAnonRecordField (ident: Ident, expr: SynExpr) =
        { Type = AnonRecordField_
          Range = noRange
          Properties = p [ "ident" ==> i ident ]
          FsAstNode = expr
          Childs = [ yield visitSynExpr expr ] }

    and visitAnonRecordTypeField (ident: Ident, t: SynType) =
        { Type = AnonRecordTypeField_
          Range = noRange
          Properties = p [ "ident" ==> i ident ]
          FsAstNode = t
          Childs = [ yield visitSynType t ] }

    and visitSynMemberSig (ms: SynMemberSig) : Node =
        match ms with
        | SynMemberSig.Member (valSig, _, range) ->
            { Type = SynMemberSig_Member
              Range = r range
              Properties = p []
              FsAstNode = ms
              Childs = [ yield visitSynValSig valSig ] }
        | SynMemberSig.Interface (typeName, range) ->
            { Type = SynMemberSig_Interface
              Range = r range
              Properties = p []
              FsAstNode = ms
              Childs = [ yield visitSynType typeName ] }
        | SynMemberSig.Inherit (typeName, range) ->
            { Type = SynMemberSig_Inherit
              Range = r range
              Properties = p []
              FsAstNode = ms
              Childs = [ yield visitSynType typeName ] }
        | SynMemberSig.ValField (f, range) ->
            { Type = SynMemberSig_ValField
              Range = r range
              Properties = p []
              FsAstNode = ms
              Childs = [ yield visitSynField f ] }
        | SynMemberSig.NestedType (typedef, range) ->
            { Type = SynMemberSig_NestedType
              Range = r range
              Properties = p []
              FsAstNode = ms
              Childs = [ yield visitSynTypeDefnSig typedef ] }

    and visitSynIndexerArg (ia: SynIndexerArg) : Node =
        match ia with
        | SynIndexerArg.One (e, _fromEnd, _) ->
            { Type = SynIndexerArg_One
              Range = noRange
              Properties = p []
              FsAstNode = ia
              Childs = [ yield visitSynExpr e ] }
        | SynIndexerArg.Two (e1, _fromEnd1, e2, _fromEnd2, _, _) ->
            { Type = SynIndexerArg_Two
              Range = noRange
              Properties = p []
              FsAstNode = ia
              Childs =
                  [ yield visitSynExpr e1
                    yield visitSynExpr e2 ] }

    and visitSynMatchClause (mc: SynMatchClause) : Node =
        match mc with
        | SynMatchClause.Clause (pat, e1, e2, _range, _) ->
            { Type = SynMatchClause_Clause
              Range = r mc.Range // _range is the same range as pat, see https://github.com/dotnet/fsharp/issues/10877
              Properties = p []
              FsAstNode = mc
              Childs =
                  [ yield visitSynPat pat
                    if e1.IsSome then
                        yield visitSynExpr e1.Value
                    yield visitSynExpr e2 ] }

    and visitArgsOption (expr: SynExpr, ident: Ident option) =
        { Type = ArgOptions_
          Range = noRange
          Properties =
              p [ if ident.IsSome then
                      yield "ident" ==> i ident.Value ]
          FsAstNode = expr
          Childs = [ yield visitSynExpr expr ] }

    and visitSynInterfaceImpl (ii: SynInterfaceImpl) : Node =
        match ii with
        | InterfaceImpl (typ, bindings, range) ->
            { Type = InterfaceImpl_
              Range = r range
              Properties = p []
              FsAstNode = ii
              Childs =
                  [ yield visitSynType typ
                    yield! (bindings |> List.map visitSynBinding) ] }

    and visitSynTypeDefn (td: SynTypeDefn) =
        match td with
        | TypeDefn (sci, stdr, members, range) ->
            { Type = TypeDefn_
              Range = r range
              Properties = p []
              FsAstNode = td
              Childs =
                  [ yield visitSynComponentInfo sci
                    yield visitSynTypeDefnRepr stdr
                    yield! (members |> List.map visitSynMemberDefn) ] }

    and visitSynTypeDefnSig (typeDefSig: SynTypeDefnSig) : Node =
        match typeDefSig with
        | TypeDefnSig (sci, synTypeDefnSigReprs, memberSig, range) ->
            { Type = TypeDefnSig_
              Range = r range
              Properties = p []
              FsAstNode = typeDefSig
              Childs =
                  [ yield visitSynComponentInfo sci
                    yield visitSynTypeDefnSigRepr synTypeDefnSigReprs
                    yield! (memberSig |> List.map visitSynMemberSig) ] }

    and visitSynTypeDefnSigRepr (stdr: SynTypeDefnSigRepr) : Node =
        match stdr with
        | SynTypeDefnSigRepr.ObjectModel (kind, members, range) ->
            { Type = SynTypeDefnSigRepr_ObjectModel
              Range = r range
              Properties = p []
              FsAstNode = stdr
              Childs =
                  [ yield visitSynTypeDefnKind kind
                    yield! (members |> List.map visitSynMemberSig) ] }
        | SynTypeDefnSigRepr.Simple (simpleRepr, range) ->
            { Type = SynTypeDefnSigRepr_ObjectModel
              Range = r range
              Properties = p []
              FsAstNode = stdr
              Childs = [ yield visitSynTypeDefnSimpleRepr simpleRepr ] }
        | SynTypeDefnSigRepr.Exception (exceptionRepr) ->
            { Type = SynTypeDefnSigRepr_Exception
              Range = noRange
              Properties = p []
              FsAstNode = stdr
              Childs = [ yield visitSynExceptionDefnRepr exceptionRepr ] }

    and visitSynMemberDefn (mbrDef: SynMemberDefn) : Node =
        match mbrDef with
        | SynMemberDefn.Open (target, parentRange) ->
            // we use the parent ranges here to match up with the trivia parsed
            match target with
            | SynOpenDeclTarget.ModuleOrNamespace (longIdent, _range) ->
                { Type = SynMemberDefn_Open
                  Range = r parentRange
                  Properties = p [ "longIdent" ==> li longIdent ]
                  FsAstNode = target
                  Childs = [] }
            | SynOpenDeclTarget.Type (synType, _range) ->
                { Type = SynMemberDefn_OpenType
                  Range = r parentRange
                  Properties = p []
                  FsAstNode = target
                  Childs = [ visitSynType synType ] }
        | SynMemberDefn.Member (memberDefn, range) ->
            { Type = SynMemberDefn_Member
              Range = r range
              Properties = p []
              FsAstNode = mbrDef
              Childs = [ yield visitSynBinding memberDefn ] }
        | SynMemberDefn.ImplicitCtor (access, attrs, ctorArgs, selfIdentifier, _xmlDoc, range) ->
            { Type = SynMemberDefn_ImplicitCtor
              Range = r range
              Properties =
                  p [ if selfIdentifier.IsSome then
                          yield "selfIdent" ==> i selfIdentifier.Value
                      if access.IsSome then
                          yield "access" ==> (access.Value |> visitSynAccess) ]
              FsAstNode = mbrDef
              Childs =
                  [ yield! (visitSynAttributeLists range attrs)
                    yield visitSynSimplePats ctorArgs ] }
        | SynMemberDefn.ImplicitInherit (inheritType, inheritArgs, inheritAlias, range) ->
            { Type = SynMemberDefn_ImplicitInherit
              Range = r range
              Properties =
                  p [ if inheritAlias.IsSome then
                          yield "inheritAlias" ==> i inheritAlias.Value ]
              FsAstNode = mbrDef
              Childs =
                  [ yield visitSynType inheritType
                    yield visitSynExpr inheritArgs ] }
        | SynMemberDefn.LetBindings (bindings, isStatic, isRecursive, range) ->
            { Type = SynMemberDefn_LetBindings
              Range = r range
              Properties =
                  p [ "isStatic" ==> isStatic
                      "isRecursive" ==> isRecursive ]
              FsAstNode = mbrDef
              Childs = [ yield! bindings |> List.map visitSynBinding ] }
        | SynMemberDefn.AbstractSlot (valSig, _, range) ->
            { Type = SynMemberDefn_AbstractSlot
              Range = r range
              Properties = p []
              FsAstNode = mbrDef
              Childs = [ yield visitSynValSig valSig ] }
        | SynMemberDefn.Interface (typ, members, range) ->
            { Type = SynMemberDefn_Interface
              Range = r range
              Properties = p []
              FsAstNode = mbrDef
              Childs =
                  [ yield visitSynType typ
                    if members.IsSome then
                        yield! members.Value |> List.map visitSynMemberDefn ] }
        | SynMemberDefn.Inherit (typ, ident, range) ->
            { Type = SynMemberDefn_Inherit
              Range = r range
              Properties =
                  p [ if ident.IsSome then
                          yield "ident" ==> i ident.Value ]
              FsAstNode = mbrDef
              Childs = [ yield visitSynType typ ] }
        | SynMemberDefn.ValField (fld, range) ->
            { Type = SynMemberDefn_ValField
              Range = r range
              Properties = p []
              FsAstNode = mbrDef
              Childs = [ yield visitSynField fld ] }
        | SynMemberDefn.NestedType (typeDefn, access, range) ->
            { Type = SynMemberDefn_NestedType
              Range = r range
              Properties =
                  p [ if access.IsSome then
                          yield "access" ==> (access.Value |> visitSynAccess) ]
              FsAstNode = mbrDef
              Childs = [ yield visitSynTypeDefn typeDefn ] }
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
              FsAstNode = mbrDef
              Childs =
                  [ yield! (visitSynAttributeLists range attrs)
                    if typeOpt.IsSome then
                        yield visitSynType typeOpt.Value
                    yield visitSynExpr synExpr ] }

    and visitSynSimplePat (sp: SynSimplePat) : Node =
        match sp with
        | SynSimplePat.Id (ident, _, isCompilerGenerated, isThisVar, isOptArg, range) ->
            { Type = SynSimplePat_Id
              Range = r range
              Properties =
                  p [ "isCompilerGenerated" ==> isCompilerGenerated
                      "isThisVar" ==> isThisVar
                      "isOptArg" ==> isOptArg
                      "ident" ==> i ident ]
              FsAstNode = sp
              Childs = [] }
        | SynSimplePat.Typed (simplePat, typ, range) ->
            { Type = SynSimplePat_Typed
              Range = r range
              Properties = p []
              FsAstNode = sp
              Childs =
                  [ yield visitSynSimplePat simplePat
                    yield visitSynType typ ] }
        | SynSimplePat.Attrib (simplePat, attrs, range) ->
            { Type = SynSimplePat_Attrib
              Range = r range
              Properties = p []
              FsAstNode = sp
              Childs =
                  [ yield visitSynSimplePat simplePat
                    yield! (visitSynAttributeLists range attrs) ] }

    and visitSynSimplePats (sp: SynSimplePats) : Node =
        match sp with
        | SynSimplePats.SimplePats (pats, range) ->
            { Type = SynSimplePats_SimplePats
              Range = r range
              Properties = p []
              FsAstNode = sp
              Childs = [ yield! pats |> List.map visitSynSimplePat ] }
        | SynSimplePats.Typed (pats, typ, range) ->
            { Type = SynSimplePat_Typed
              Range = r range
              Properties = p []
              FsAstNode = sp
              Childs =
                  [ yield visitSynSimplePats pats
                    yield visitSynType typ ] }

    and visitSynBinding (binding: SynBinding) : Node =
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
              FsAstNode = binding
              Childs =
                  [ yield! (visitSynAttributeLists range attrs)
                    yield visitSynValData valData
                    yield visitSynPat headPat
                    if returnInfo.IsSome then
                        yield visitSynBindingReturnInfo returnInfo.Value
                    yield visitSynExpr expr ] }

    and visitSynValData (svd: SynValData) : Node =
        match svd with
        | SynValData (_, svi, ident) ->
            { Type = SynValData_
              Range = noRange
              Properties =
                  p [ if ident.IsSome then
                          yield "ident" ==> (ident.Value |> i) ]
              FsAstNode = svd
              Childs = [ yield visitSynValInfo svi ] }

    and visitSynValSig (svs: SynValSig) : Node =
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
              FsAstNode = svs
              Childs =
                  [ yield! (visitSynAttributeLists range attrs)
                    yield visitSynValTyparDecls explicitValDecls
                    yield visitSynType synType
                    yield visitSynValInfo arity
                    if expr.IsSome then
                        yield visitSynExpr expr.Value ] }

    and visitSynValTyparDecls (valTypeDecl: SynValTyparDecls) : Node =
        match valTypeDecl with
        | SynValTyparDecls (typardecls, _, _) ->
            { Type = SynValTyparDecls_
              Range = noRange
              Properties = p []
              FsAstNode = valTypeDecl
              Childs = [ yield! typardecls |> List.map visitSynTyparDecl ] }

    and visitSynTyparDecl (std: SynTyparDecl) : Node =
        match std with
        | TyparDecl (attrs, typar) ->
            { Type = TyparDecl_
              Range = noRange
              Properties = p []
              FsAstNode = std
              Childs =
                  [ yield! (visitSynAttributeLists typar.Range attrs)
                    yield visitSynTypar typar ] }

    and visitSynTypar (typar: SynTypar) : Node =
        match typar with
        | Typar (ident, staticReq, isComGen) ->
            { Type = Typar_
              Range = noRange
              Properties =
                  p [ "ident" ==> i ident
                      "isComGen" ==> isComGen
                      "staticReq" ==> visitTyparStaticReq staticReq ]
              FsAstNode = typar
              Childs = [] }

    and visitTyparStaticReq (tsr: TyparStaticReq) =
        match tsr with
        | NoStaticReq -> "NoStaticReq"
        | HeadTypeStaticReq -> "HeadTypeStaticReq"

    and visitSynBindingReturnInfo (returnInfo: SynBindingReturnInfo) : Node =
        match returnInfo with
        | SynBindingReturnInfo (typeName, range, attrs) ->
            { Type = SynBindingReturnInfo_
              Range = r range
              Properties = p []
              FsAstNode = returnInfo
              Childs =
                  [ yield visitSynType typeName
                    yield! (visitSynAttributeLists range attrs) ] }

    and visitSynPat (sp: SynPat) : Node =
        match sp with
        | SynPat.Const (sc, range) ->
            { Type = SynPat_Const
              Range = r range
              Properties = p []
              FsAstNode = sp
              Childs = [ visitSynConst range sc ] }
        | SynPat.Wild (range) ->
            { Type = SynPat_Wild
              Range = r range
              Properties = p []
              FsAstNode = sp
              Childs = [] }
        | SynPat.Named (synPat, ident, isSelfIdentifier, access, range) ->
            { Type = SynPat_Named
              Range = r range
              Properties =
                  p [ yield "ident" ==> i ident
                      yield "isSelfIdentifier" ==> isSelfIdentifier
                      if access.IsSome then
                          yield "access" ==> (access.Value |> visitSynAccess) ]
              FsAstNode = sp
              Childs = [ yield visitSynPat synPat ] }
        | SynPat.Typed (synPat, synType, range) ->
            { Type = SynPat_Typed
              Range = r range
              Properties = p []
              FsAstNode = sp
              Childs =
                  [ yield visitSynPat synPat
                    yield visitSynType synType ] }
        | SynPat.Attrib (synPat, attrs, range) ->
            { Type = SynPat_Attrib
              Range = r range
              Properties = p []
              FsAstNode = sp
              Childs =
                  [ yield visitSynPat synPat
                    yield! (visitSynAttributeLists range attrs) ] }
        | SynPat.Or (synPat, synPat2, range) ->
            { Type = SynPat_Or
              Range = r range
              Properties = p []
              FsAstNode = sp
              Childs =
                  [ yield visitSynPat synPat
                    yield visitSynPat synPat2 ] }
        | SynPat.Ands (pats, range) ->
            { Type = SynPat_Ands
              Range = r range
              Properties = p []
              FsAstNode = sp
              Childs = [ yield! pats |> List.map visitSynPat ] }
        | SynPat.LongIdent (longDotId, ident, svtd, ctorArgs, access, range) ->
            { Type = SynPat_LongIdent
              Range = r range
              Properties =
                  p [ if ident.IsSome then
                          yield "ident" ==> (ident.Value |> i)
                      yield "longDotId" ==> lid longDotId
                      if access.IsSome then
                          yield "access" ==> (access.Value |> visitSynAccess) ]
              FsAstNode = sp
              Childs =
                  [ if svtd.IsSome then
                        yield visitSynValTyparDecls svtd.Value
                    yield visitSynConstructorArgs ctorArgs ] }
        | SynPat.Tuple (isStruct, pats, range) ->
            { Type = SynPat_Tuple
              Range = r range
              Properties = p [ "isStruct" ==> isStruct ]
              FsAstNode = sp
              Childs = [ yield! pats |> List.map visitSynPat ] }
        | SynPat.Paren (pat, range) ->
            { Type = SynPat_Paren
              Range = r range
              Properties = p []
              FsAstNode = sp
              Childs = [ visitSynPat pat ] }
        | SynPat.ArrayOrList (_, pats, range) ->
            { Type = SynPat_ArrayOrList
              Range = r range
              Properties = p []
              FsAstNode = sp
              Childs = [ yield! pats |> List.map visitSynPat ] }
        | SynPat.Record (pats, range) ->
            { Type = SynPat_Record
              Range = r range
              Properties = p []
              FsAstNode = sp
              Childs = [ yield! pats |> List.map (snd >> visitSynPat) ] }
        | SynPat.Null (range) ->
            { Type = SynPat_Null
              Range = r range
              Properties = p []
              FsAstNode = sp
              Childs = [] }
        | SynPat.OptionalVal (ident, range) ->
            { Type = SynPat_OptionalVal
              Range = r range
              Properties = p [ "ident" ==> i ident ]
              FsAstNode = sp
              Childs = [] }
        | SynPat.IsInst (typ, range) ->
            { Type = SynPat_IsInst
              Range = r range
              Properties = p []
              FsAstNode = sp
              Childs = [ visitSynType typ ] }
        | SynPat.QuoteExpr (expr, range) ->
            { Type = SynPat_QuoteExpr
              Range = r range
              Properties = p []
              FsAstNode = sp
              Childs = [ visitSynExpr expr ] }
        | SynPat.DeprecatedCharRange (c, c2, range) ->
            { Type = SynPat_DeprecatedCharRange
              Range = r range
              Properties = p [ "c" ==> c; "c2" ==> c2 ]
              FsAstNode = sp
              Childs = [] }
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
              FsAstNode = sp
              Childs = [] }
        | SynPat.FromParseError (pat, range) ->
            { Type = SynPat_FromParseError
              Range = r range
              Properties = p []
              FsAstNode = sp
              Childs = [ visitSynPat pat ] }

    and visitSynConstructorArgs (ctorArgs: SynArgPats) : Node =
        match ctorArgs with
        | Pats (pats) ->
            { Type = Pats_
              Range = noRange
              Properties = p []
              FsAstNode = ctorArgs
              Childs = [ yield! pats |> List.map visitSynPat ] }
        | NamePatPairs (pats, range) ->
            { Type = NamePatPairs_
              Range = r range
              Properties = p []
              FsAstNode = ctorArgs
              Childs = [ yield! pats |> List.map (snd >> visitSynPat) ] }

    and visitSynComponentInfo (sci: SynComponentInfo) : Node =
        match sci with
        | ComponentInfo (attribs, typeParams, _, longId, _, preferPostfix, access, range) ->
            { Type = ComponentInfo_
              Range = r range
              Properties =
                  p [ yield "longIdent" ==> li longId
                      yield "preferPostfix" ==> preferPostfix
                      if access.IsSome then
                          yield "access" ==> (access.Value |> visitSynAccess) ]
              FsAstNode = sci
              Childs =
                  [ yield! (visitSynAttributeLists range attribs)
                    yield! (typeParams |> List.map (visitSynTyparDecl)) ] }

    and visitSynTypeDefnRepr (stdr: SynTypeDefnRepr) : Node =
        match stdr with
        | SynTypeDefnRepr.ObjectModel (kind, members, range) ->
            { Type = SynTypeDefnRepr_ObjectModel
              Range = r range
              Properties = p []
              FsAstNode = stdr
              Childs =
                  [ yield visitSynTypeDefnKind kind
                    yield! (members |> List.map visitSynMemberDefn) ] }
        | SynTypeDefnRepr.Simple (simpleRepr, range) ->
            { Type = SynTypeDefnRepr_Simple
              Range = r range
              Properties = p []
              FsAstNode = stdr
              Childs = [ yield visitSynTypeDefnSimpleRepr simpleRepr ] }
        | SynTypeDefnRepr.Exception (exceptionRepr) ->
            { Type = SynTypeDefnRepr_Exception
              Range = noRange
              Properties = p []
              FsAstNode = stdr
              Childs = [ yield visitSynExceptionDefnRepr exceptionRepr ] }

    and visitSynTypeDefnKind (kind: SynTypeDefnKind) =
        match kind with
        | TyconUnspecified ->
            { Type = SynTypeDefnKind_TyconUnspecified
              Range = noRange
              Properties = p []
              FsAstNode = kind
              Childs = [] }
        | TyconClass ->
            { Type = SynTypeDefnKind_TyconClass
              Range = noRange
              Properties = p []
              FsAstNode = kind
              Childs = [] }
        | TyconInterface ->
            { Type = SynTypeDefnKind_TyconInterface
              Range = noRange
              Properties = p []
              FsAstNode = kind
              Childs = [] }
        | TyconStruct ->
            { Type = SynTypeDefnKind_TyconStruct
              Range = noRange
              Properties = p []
              FsAstNode = kind
              Childs = [] }
        | TyconRecord ->
            { Type = SynTypeDefnKind_TyconRecord
              Range = noRange
              Properties = p []
              FsAstNode = kind
              Childs = [] }
        | TyconUnion ->
            { Type = SynTypeDefnKind_TyconUnion
              Range = noRange
              Properties = p []
              FsAstNode = kind
              Childs = [] }
        | TyconAbbrev ->
            { Type = SynTypeDefnKind_TyconAbbrev
              Range = noRange
              Properties = p []
              FsAstNode = kind
              Childs = [] }
        | TyconHiddenRepr ->
            { Type = SynTypeDefnKind_TyconHiddenRepr
              Range = noRange
              Properties = p []
              FsAstNode = kind
              Childs = [] }
        | TyconAugmentation ->
            { Type = SynTypeDefnKind_TyconAugmentation
              Range = noRange
              Properties = p []
              FsAstNode = kind
              Childs = [] }
        | TyconILAssemblyCode ->
            { Type = SynTypeDefnKind_TyconILAssemblyCode
              Range = noRange
              Properties = p []
              FsAstNode = kind
              Childs = [] }
        | TyconDelegate (typ, valinfo) ->
            { Type = SynTypeDefnKind_TyconDelegate
              Range = noRange
              Properties = p []
              FsAstNode = kind
              Childs =
                  [ yield visitSynType typ
                    yield visitSynValInfo valinfo ] }

    and visitSynTypeDefnSimpleRepr (arg: SynTypeDefnSimpleRepr) =
        match arg with
        | SynTypeDefnSimpleRepr.None (range) ->
            { Type = SynTypeDefnSimpleRepr_None
              Range = r range
              Properties = p []
              FsAstNode = arg
              Childs = [] }
        | SynTypeDefnSimpleRepr.Union (access, unionCases, range) ->
            { Type = SynTypeDefnSimpleRepr_Union
              Range = r range
              Properties =
                  p [ if access.IsSome then
                          yield "access" ==> (access.Value |> visitSynAccess) ]
              FsAstNode = arg
              Childs = [ yield! unionCases |> List.map visitSynUnionCase ] }
        | SynTypeDefnSimpleRepr.Enum (enumCases, range) ->
            { Type = SynTypeDefnSimpleRepr_Enum
              Range = r range
              Properties = p []
              FsAstNode = arg
              Childs = [ yield! enumCases |> List.map visitSynEnumCase ] }
        | SynTypeDefnSimpleRepr.Record (access, recordFields, range) ->
            { Type = SynTypeDefnSimpleRepr_Record
              Range = r range
              Properties =
                  p [ if access.IsSome then
                          yield "access" ==> (access.Value |> visitSynAccess) ]
              FsAstNode = arg
              Childs = [ yield! recordFields |> List.map visitSynField ] }
        | SynTypeDefnSimpleRepr.General (_, _, _, _, _, _, _, range) ->
            { Type = SynTypeDefnSimpleRepr_General
              Range = r range
              Properties = p []
              FsAstNode = arg
              Childs = [] }
        | SynTypeDefnSimpleRepr.LibraryOnlyILAssembly (_, range) ->
            { Type = SynTypeDefnSimpleRepr_LibraryOnlyILAssembly
              Range = r range
              Properties = p []
              FsAstNode = arg
              Childs = [] }
        | SynTypeDefnSimpleRepr.TypeAbbrev (_, typ, range) ->
            { Type = SynTypeDefnSimpleRepr_TypeAbbrev
              Range = r range
              Properties = p []
              FsAstNode = arg
              Childs = [ visitSynType typ ] }
        | SynTypeDefnSimpleRepr.Exception (edr) ->
            { Type = SynTypeDefnSimpleRepr_Exception
              Range = noRange
              Properties = p []
              FsAstNode = arg
              Childs = [ visitSynExceptionDefnRepr edr ] }

    and visitSynExceptionDefn (exceptionDef: SynExceptionDefn) : Node =
        match exceptionDef with
        | SynExceptionDefn (sedr, members, range) ->
            { Type = SynExceptionDefn_
              Range = r range
              Properties = p []
              FsAstNode = exceptionDef
              Childs =
                  [ yield visitSynExceptionDefnRepr sedr
                    yield! (members |> List.map visitSynMemberDefn) ] }

    and visitSynExceptionDefnRepr (sedr: SynExceptionDefnRepr) : Node =
        match sedr with
        | SynExceptionDefnRepr (attrs, unionCase, longId, _, access, range) ->
            { Type = SynExceptionDefnRepr_
              Range = r range
              Properties =
                  p [ if longId.IsSome then
                          yield "longIdent" ==> (longId.Value |> li)
                      if access.IsSome then
                          yield "access" ==> (access.Value |> visitSynAccess) ]
              FsAstNode = sedr
              Childs =
                  [ yield! (visitSynAttributeLists range attrs)
                    yield visitSynUnionCase unionCase ] }

    and visitSynAttribute (attr: SynAttribute) : Node =
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
          FsAstNode = attr
          Childs = [ visitSynExpr attr.ArgExpr ] }

    and visitSynAttributeLists (parentRange: Range) (attrs: SynAttributeList list) : Node list =
        match attrs with
        | [ h ] ->
            visitSynAttributeList parentRange h
            |> List.singleton
        | _ :: tail ->
            let aRanges =
                tail
                |> List.map (fun a -> a.Range)
                |> fun r -> r @ [ parentRange ]

            List.zip attrs aRanges
            |> List.map (fun (a, r) -> visitSynAttributeList r a)
        | [] -> []

    and visitSynAttributeList (parentRange: Range) (attrs: SynAttributeList) : Node =
        { Type = SynAttributeList_
          Range = r attrs.Range
          Properties = p [ "linesBetweenParent", box (parentRange.StartLine - attrs.Range.EndLine - 1) ]
          FsAstNode = attrs
          Childs = attrs.Attributes |> List.map visitSynAttribute }

    and visitSynUnionCase (uc: SynUnionCase) : Node =
        match uc with
        | UnionCase (attrs, ident, uct, _, access, range) ->
            { Type = UnionCase_
              Range = r range
              Properties =
                  p [ yield "ident" ==> i ident
                      if access.IsSome then
                          yield "access" ==> (access.Value |> visitSynAccess) ]
              FsAstNode = uc
              Childs =
                  [ yield visitSynUnionCaseType uct
                    yield! (visitSynAttributeLists range attrs) ] }

    and visitSynUnionCaseType (uct: SynUnionCaseType) =
        match uct with
        | UnionCaseFields (cases) ->
            { Type = UnionCaseFields_
              Range = noRange
              Properties = p []
              FsAstNode = uct
              Childs = [ yield! cases |> List.map visitSynField ] }
        | UnionCaseFullType (stype, valInfo) ->
            { Type = UnionCaseFullType_
              Range = noRange
              Properties = p []
              FsAstNode = uct
              Childs =
                  [ yield visitSynType stype
                    yield visitSynValInfo valInfo ] }

    and visitSynEnumCase (sec: SynEnumCase) : Node =
        match sec with
        | EnumCase (attrs, ident, _, _, range) ->
            { Type = EnumCase_
              Range = r range
              Properties = p []
              FsAstNode = sec
              Childs =
                  [ yield! (visitSynAttributeLists range attrs)
                    yield visitIdent ident ] }

    and visitSynField (sfield: SynField) : Node =
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
              FsAstNode = sfield
              Childs =
                  [ yield! (visitSynAttributeLists parentRange attrs)
                    yield visitSynType typ ] }

    and visitSynType (st: SynType) =
        match st with
        | SynType.LongIdent (li) ->
            { Type = SynType_LongIdent
              Range = noRange
              Properties = p []
              FsAstNode = st
              Childs = visitLongIdentWithDots li }
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
              FsAstNode = st
              Childs =
                  [ yield! typeArgs |> List.map visitSynType
                    yield visitSynType typeName ] }
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
              FsAstNode = st
              Childs =
                  [ yield! typeArgs |> List.map visitSynType
                    yield visitSynType typeName ] }
        | SynType.Tuple (isStruct, typeNames, range) ->
            { Type = SynType_Tuple
              Range = r range
              Properties = p [ "isStruct" ==> isStruct ]
              FsAstNode = st
              Childs = [ yield! typeNames |> List.map (snd >> visitSynType) ] }
        | SynType.Array (_, elementType, range) ->
            { Type = SynType_Array
              Range = r range
              Properties = p []
              FsAstNode = st
              Childs = [ yield visitSynType elementType ] }
        | SynType.Fun (argType, returnType, range) ->
            { Type = SynType_Fun
              Range = r range
              Properties = p []
              FsAstNode = st
              Childs =
                  [ yield visitSynType argType
                    yield visitSynType returnType ] }
        | SynType.Var (genericName, range) ->
            { Type = SynType_Var
              Range = r range
              Properties = p []
              FsAstNode = st
              Childs = [ yield visitSynTypar genericName ] }
        | SynType.Anon (range) ->
            { Type = SynType_Anon
              Range = r range
              Properties = p []
              FsAstNode = st
              Childs = [] }
        | SynType.WithGlobalConstraints (typeName, _, range) ->
            { Type = SynType_WithGlobalConstraints
              Range = r range
              Properties = p []
              FsAstNode = st
              Childs = [ yield visitSynType typeName ] }
        | SynType.HashConstraint (synType, range) ->
            { Type = SynType_HashConstraint
              Range = r range
              Properties = p []
              FsAstNode = st
              Childs = [ yield visitSynType synType ] }
        | SynType.MeasureDivide (dividendType, divisorType, range) ->
            { Type = SynType_MeasureDivide
              Range = r range
              Properties = p []
              FsAstNode = st
              Childs =
                  [ yield visitSynType dividendType
                    yield visitSynType divisorType ] }
        | SynType.MeasurePower (measureType, _, range) ->
            { Type = SynType_MeasurePower
              Range = r range
              Properties = p []
              FsAstNode = st
              Childs = [ yield visitSynType measureType ] }
        | SynType.StaticConstant (constant, range) ->
            { Type = SynType_StaticConstant
              Range = r range
              Properties = p []
              FsAstNode = st
              Childs = [ visitSynConst range constant ] }
        | SynType.StaticConstantExpr (expr, range) ->
            { Type = SynType_StaticConstantExpr
              Range = r range
              Properties = p []
              FsAstNode = st
              Childs = [ yield visitSynExpr expr ] }
        | SynType.StaticConstantNamed (expr, typ, range) ->
            { Type = SynType_StaticConstantNamed
              Range = r range
              Properties = p []
              FsAstNode = st
              Childs =
                  [ yield visitSynType expr
                    yield visitSynType typ ] }
        | SynType.AnonRecd (isStruct, typeNames, range) ->
            { Type = SynType_AnonRecd
              Range = r range
              Properties = p [ "isStruct" ==> isStruct ]
              FsAstNode = st
              Childs = List.map visitAnonRecordTypeField typeNames }
        | SynType.Paren (innerType, range) ->
            { Type = SynType_Paren
              Range = r range
              Properties = p []
              FsAstNode = st
              Childs = [ yield visitSynType innerType ] }

    and visitSynConst (parentRange: Range) (sc: SynConst) =
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
          FsAstNode = sc
          Childs = [] }

    and visitSynValInfo (svi: SynValInfo) =
        match svi with
        | SynValInfo (args, arg) ->
            { Type = SynValInfo_
              Range = noRange
              Properties = p []
              FsAstNode = svi
              Childs =
                  [ yield! args |> List.collect (List.map visitSynArgInfo)
                    yield visitSynArgInfo arg ] }

    and visitSynArgInfo (sai: SynArgInfo) =
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
              FsAstNode = sai
              Childs = [ yield! (visitSynAttributeLists parentRange attrs) ] }

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
              FsAstNode = hash
              Childs = [] }

    and visitSynModuleOrNamespaceSig (modOrNs: SynModuleOrNamespaceSig) : Node =
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
              FsAstNode = modOrNs
              Childs =
                  [ yield!
                      if synModuleOrNamespaceKind = SynModuleOrNamespaceKind.DeclaredNamespace then
                          visitLongIdent longIdent
                      else
                          []
                    yield! (visitSynAttributeLists range attrs)
                    yield! (decls |> List.map visitSynModuleSigDecl) ] }

    and visitSynModuleSigDecl (ast: SynModuleSigDecl) : Node =
        match ast with
        | SynModuleSigDecl.ModuleAbbrev (ident, longIdent, range) ->
            { Type = SynModuleSigDecl_ModuleAbbrev
              Range = r range
              Properties =
                  p [ "ident" ==> i ident
                      "longIdent" ==> li longIdent ]
              FsAstNode = ast
              Childs = [] }
        | SynModuleSigDecl.NestedModule (sci, isRecursive, decls, range) ->
            { Type = SynModuleSigDecl_NestedModule
              Range = r range
              Properties = p [ "isRecursive" ==> isRecursive ]
              FsAstNode = ast
              Childs =
                  [ yield visitSynComponentInfo sci
                    yield! (decls |> List.map visitSynModuleSigDecl) ] }
        | SynModuleSigDecl.Val (SynValSig.ValSpfn _ as node, _) -> visitSynValSig node
        | SynModuleSigDecl.Types (typeDefs, range) ->
            { Type = SynModuleSigDecl_Types
              Range = r range
              Properties = p []
              FsAstNode = ast
              Childs = typeDefs |> List.map visitSynTypeDefnSig }
        | SynModuleSigDecl.Open (target, parentRange) ->
            // we use the parent ranges here to match up with the trivia parsed
            match target with
            | SynOpenDeclTarget.ModuleOrNamespace (longIdent, _range) ->
                { Type = SynModuleSigDecl_Open
                  Range = r parentRange
                  Properties = p [ "longIdent" ==> li longIdent ]
                  FsAstNode = target
                  Childs = [] }
            | SynOpenDeclTarget.Type (synType, _range) ->
                { Type = SynModuleSigDecl_OpenType
                  Range = r parentRange
                  Properties = p []
                  FsAstNode = target
                  Childs = [ visitSynType synType ] }
        | SynModuleSigDecl.HashDirective (hash, range) ->
            { Type = SynModuleSigDecl_HashDirective
              Range = r range
              Properties = p []
              FsAstNode = ast
              Childs = [ visitParsedHashDirective hash ] }
        | SynModuleSigDecl.NamespaceFragment (moduleOrNamespace) ->
            { Type = SynModuleSigDecl_NamespaceFragment
              Range = noRange
              Properties = p []
              FsAstNode = ast
              Childs = [ visitSynModuleOrNamespaceSig moduleOrNamespace ] }
        | SynModuleSigDecl.Exception (synExceptionSig, range) ->
            { Type = SynModuleSigDecl_Exception
              Range = r range
              Properties = p []
              FsAstNode = ast
              Childs = [ visitSynExceptionSig synExceptionSig ] }

    and visitSynExceptionSig (exceptionDef: SynExceptionSig) : Node =
        match exceptionDef with
        | SynExceptionSig (sedr, members, range) ->
            { Type = SynExceptionSig_
              Range = r range
              Properties = p []
              FsAstNode = exceptionDef
              Childs =
                  [ yield visitSynExceptionDefnRepr sedr
                    yield! (members |> List.map visitSynMemberSig) ] }

    and visitLongIdentWithDots (lid: LongIdentWithDots) : Node list =
        match lid with
        | LongIdentWithDots (ids, _) -> List.map visitIdent ids

    and visitLongIdent (li: LongIdent) : Node list = List.map visitIdent li

    and visitIdent (ident: Ident) : Node =
        { Type = Ident_
          Range = r ident.idRange
          Properties = Map.empty
          FsAstNode = ident
          Childs = [] }

let astToNode (hds: ParsedHashDirective list) (mdls: SynModuleOrNamespace list) : Node =
    let children =
        [ yield! List.map Ast.visit mdls
          yield! List.map Ast.visitParsedHashDirective hds ]

    { Type = File_
      Range = None
      Properties = Map.empty
      FsAstNode = mdls
      Childs = children }

let sigAstToNode (ast: SynModuleOrNamespaceSig list) : Node =
    let children =
        List.map Ast.visitSynModuleOrNamespaceSig ast

    { Type = SigFile_
      Range = None
      Properties = Map.empty
      FsAstNode = ast
      Childs = children }
