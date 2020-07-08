module Fantomas.AstTransformer

open FSharp.Compiler.Range
open FSharp.Compiler.SyntaxTree

let rec (|Sequentials|_|) = function
    | SynExpr.Sequential(_, isTrueSeq, e, Sequentials es, range) ->
        Some((isTrueSeq, e, None, range)::es)
    | SynExpr.Sequential(_, isTrueSeq, e1, e2, range) ->
        Some [isTrueSeq, e1, Some e2, range]
    | _ -> None

type Id =
    { Ident: string
      Range: range option }

type FsAstNode = obj

type Node =
    { Type: string
      Range: range option
      Properties: Map<string, obj>
      Childs: Node list
      FsAstNode: FsAstNode }

module Helpers =
    let r(r: FSharp.Compiler.Range.range): range option =
        Some r

    let p = Map.ofList
    let inline (==>) a b = (a,box b)

    let noRange =
        None

    let i (id: Ident) : Id =
        { Ident = id.idText
          Range = r id.idRange}

    let li (id: LongIdent) =
        id |> List.map i

    let lid (id: LongIdentWithDots) = li id.Lid

module private Ast =
    open Helpers

    let rec visit(ast: SynModuleOrNamespace ): Node =
        visitSynModuleOrNamespace ast

    and visitSynModuleOrNamespace(modOrNs: SynModuleOrNamespace): Node =
        match modOrNs with
        | SynModuleOrNamespace(longIdent,isRecursive,isModule,decls,_,attrs,access,range) ->
            let collectIdents (idents: LongIdent) =
                idents
                |> List.map (fun ident ->
                    { Type = "Ident"
                      Range = r ident.idRange
                      Properties = Map.empty
                      FsAstNode = ident
                      Childs = [] })
            {Type = sprintf "SynModuleOrNamespace.%A" isModule
             Range = r range
             Properties =
                 p [yield "isRecursive" ==> isRecursive
                    yield "isModule" ==> isModule
                    yield "longIdent" ==> li longIdent
                    if access.IsSome then yield "access" ==> (access.Value |> visitSynAccess)]
             FsAstNode = modOrNs
             Childs =
                 [yield! (if isModule = SynModuleOrNamespaceKind.DeclaredNamespace then collectIdents longIdent else [])
                  yield! (visitSynAttributeLists range attrs)
                  yield! (decls |> List.map visitSynModuleDecl)]}

    and visitSynModuleDecl(ast: SynModuleDecl) : Node =
        match ast with
        | SynModuleDecl.ModuleAbbrev(ident,longIdent,range) ->
            {Type = "SynModuleDecl.ModuleAbbrev"
             Range = r range
             Properties =
                 p ["ident" ==> i ident
                    "longIdent" ==> li longIdent]
             FsAstNode = ast
             Childs = []}
        | SynModuleDecl.NestedModule(sci,isRecursive,decls,_,range) ->
            {Type = "SynModuleDecl.NestedModule"
             Range = r range
             Properties = p ["isRecursive" ==> isRecursive]
             FsAstNode = ast
             Childs =
                 [yield visitSynComponentInfo sci
                  yield! (decls |> List.map visitSynModuleDecl)]}
        | SynModuleDecl.Let(_,bindings,range) ->
            {Type = "SynModuleDecl.Let"
             Range = r range
             Properties = p []
             FsAstNode = ast
             Childs = bindings |> List.map visitSynBinding}
        | SynModuleDecl.DoExpr(_,expr,range) ->
            {Type = "SynModuleDecl.DoExpr"
             Range = r range
             Properties = p []
             FsAstNode = ast
             Childs = [visitSynExpr expr]}
        | SynModuleDecl.Types(typeDefs,range) ->
            {Type = "SynModuleDecl.Types"
             Range = r range
             Properties = p []
             FsAstNode = ast
             Childs = typeDefs |> List.map visitSynTypeDefn}
        | SynModuleDecl.Exception(exceptionDef,range) ->
            {Type = "SynModuleDecl.Exception"
             Range = r range
             Properties = p []
             FsAstNode = ast
             Childs = [visitSynExceptionDefn exceptionDef]}
        | SynModuleDecl.Open(longDotId,range) ->
            {Type = "SynModuleDecl.Open"
             Range = r range
             Properties = p ["longIdent" ==> lid longDotId]
             FsAstNode = ast
             Childs = []}
        | SynModuleDecl.Attributes(attrs,range) ->
            {Type = "SynModuleDecl.Attributes"
             Range = r range
             Properties = p []
             FsAstNode = ast
             Childs = visitSynAttributeLists range attrs }
        | SynModuleDecl.HashDirective(hash,range) ->
            {Type = "SynModuleDecl.HashDirective"
             Range = r range
             Properties = p []
             FsAstNode = ast
             Childs = [visitParsedHashDirective hash]}
        | SynModuleDecl.NamespaceFragment(moduleOrNamespace) ->
            {Type = "SynModuleDecl.NamespaceFragment"
             Range = noRange
             Properties = p []
             FsAstNode = ast
             Childs = [visitSynModuleOrNamespace moduleOrNamespace]}

    and visitSynExpr(synExpr: SynExpr): Node =
        match synExpr with
        | SynExpr.Paren(expr,leftParenRange,rightParenRange,range) ->
            {Type = "SynExpr.Paren"
             Range = r range
             Properties =
                 p [yield "leftParenRange" ==> r leftParenRange
                    if rightParenRange.IsSome then yield "rightParenRange" ==> r rightParenRange.Value]
             FsAstNode = synExpr
             Childs = [yield visitSynExpr expr]}
        | SynExpr.Quote(operator,isRaw,quotedSynExpr,isFromQueryExpression,range) ->
            {Type = "SynExpr.Quote"
             Range = r range
             Properties =
                 p ["isRaw" ==> isRaw
                    "isFromQueryExpression" ==> isFromQueryExpression]
             FsAstNode = synExpr
             Childs =
                 [yield visitSynExpr operator
                  yield visitSynExpr quotedSynExpr]}
        | SynExpr.Const(constant,range) ->
            {Type = "SynExpr.Const"
             Range = r range
             Properties = p ["constant" ==> visitSynConst constant]
             FsAstNode = synExpr
             Childs = []}
        | SynExpr.Typed(expr,typeName,range) ->
            {Type = "SynExpr.Typed"
             Range = r range
             Properties = p []
             FsAstNode = synExpr
             Childs =
                 [yield visitSynExpr expr
                  yield visitSynType typeName]}
        | SynExpr.Tuple(isStruct,exprs,commaRanges,range) ->
            {Type = "SynExpr.Tuple"
             Range = r range
             Properties = p ["isStruct" ==> isStruct; "commaRanges" ==> (commaRanges |> List.map r)]
             FsAstNode = synExpr
             Childs = [yield! exprs |> List.map visitSynExpr]}
        | SynExpr.ArrayOrList(isList,exprs,range) ->
            {Type = "SynExpr.StructTuple"
             Range = r range
             Properties = p ["isList" ==> isList]
             FsAstNode = synExpr
             Childs = [yield! exprs |> List.map visitSynExpr]}
        | SynExpr.Record(_,_,recordFields,range) ->
            {Type = "SynExpr.Record"
             Range = r range
             Properties = p []
             FsAstNode = synExpr
             Childs = [yield! recordFields |> List.map visitRecordField]}
        | SynExpr.AnonRecd(_,_,recordFields,range) ->
            {Type = "SynExpr.AnonRecd"
             Range = r range
             Properties = p []
             FsAstNode = synExpr
             Childs = [yield! recordFields |> List.map visitAnonRecordField]}
        | SynExpr.New(isProtected,typeName,expr,range) ->
            {Type = "SynExpr.New"
             Range = r range
             Properties = p ["isProtected" ==> isProtected]
             FsAstNode = synExpr
             Childs =
                 [yield visitSynExpr expr
                  yield visitSynType typeName]}
        | SynExpr.ObjExpr(objType,argOptions,bindings,extraImpls,newExprRange,range) ->
            {Type = "SynExpr.ObjExpr"
             Range = r range
             Properties = p ["newExprRange" ==> r newExprRange]
             FsAstNode = synExpr
             Childs =
                 [yield visitSynType objType
                  if argOptions.IsSome then yield visitArgsOption argOptions.Value
                  yield! extraImpls |> List.map visitSynInterfaceImpl
                  yield! bindings |> List.map visitSynBinding]}
        | SynExpr.While(_,whileExpr,doExpr,range) ->
            {Type = "SynExpr.While"
             Range = r range
             Properties = p []
             FsAstNode = synExpr
             Childs =
                 [yield visitSynExpr whileExpr
                  yield visitSynExpr doExpr]}
        | SynExpr.For(_,ident,identBody,_,toBody,doBody,range) ->
            {Type = "SynExpr.For"
             Range = r range
             Properties = p ["ident" ==> i ident]
             FsAstNode = synExpr
             Childs =
                 [yield visitSynExpr identBody
                  yield visitSynExpr toBody
                  yield visitSynExpr doBody]}
        | SynExpr.ForEach(_,(SeqExprOnly seqExprOnly),isFromSource,pat,enumExpr,bodyExpr,range) ->
            {Type = "SynExpr.ForEach"
             Range = r range
             Properties =
                 p ["isFromSource" ==> isFromSource
                    "seqExprOnly" ==> seqExprOnly]
             FsAstNode = synExpr
             Childs =
                 [yield visitSynPat pat
                  yield visitSynExpr enumExpr
                  yield visitSynExpr bodyExpr]}
        | SynExpr.ArrayOrListOfSeqExpr(isArray,expr,range) ->
            {Type = "SynExpr.ArrayOrListOfSeqExpr"
             Range = r range
             Properties = p ["isArray" ==> isArray]
             FsAstNode = synExpr
             Childs = [yield visitSynExpr expr]}
        | SynExpr.CompExpr(isArrayOrList,isNotNakedRefCell,expr,range) ->
            {Type = "SynExpr.CompExpr"
             Range = r range
             Properties =
                 p ["isArrayOrList" ==> isArrayOrList
                    "isNotNakedRefCell" ==> isNotNakedRefCell]
             FsAstNode = synExpr
             Childs = [yield visitSynExpr expr]}
        | SynExpr.Lambda(fromMethod,inLambdaSeq,args,body,range) ->
            {Type = "SynExpr.Lambda"
             Range = r range
             Properties =
                 p ["fromMethod" ==> fromMethod
                    "inLambdaSeq" ==> inLambdaSeq]
             FsAstNode = synExpr
             Childs =
                 [yield visitSynSimplePats args
                  yield visitSynExpr body]}
        | SynExpr.MatchLambda(isExnMatch,_,matchClauses,_,range) ->
            {Type = "SynExpr.MatchLambda"
             Range = r range
             Properties = p ["isExnMatch" ==> isExnMatch]
             FsAstNode = synExpr
             Childs = [yield! matchClauses |> List.map visitSynMatchClause]}
        | SynExpr.Match(_,expr,clauses,range) ->
            {Type = "SynExpr.Match"
             Range = r range
             Properties = p []
             FsAstNode = synExpr
             Childs =
                 [yield visitSynExpr expr
                  yield! clauses |> List.map visitSynMatchClause]}
        | SynExpr.Do(expr,range) ->
            {Type = "SynExpr.Do"
             Range = r range
             Properties = p []
             FsAstNode = synExpr
             Childs = [yield visitSynExpr expr]}
        | SynExpr.Assert(expr,range) ->
            {Type = "SynExpr.Assert"
             Range = r range
             Properties = p []
             FsAstNode = synExpr
             Childs = [yield visitSynExpr expr]}
        | SynExpr.App(atomicFlag,isInfix,funcExpr,argExpr,range) ->
            {Type = "SynExpr.App"
             Range = r range
             Properties =
                 p ["atomicFlag" ==> (match atomicFlag with
                                      | ExprAtomicFlag.Atomic -> "Atomic"
                                      | _ -> "Not Atomic")
                    "isInfix" ==> isInfix]
             FsAstNode = synExpr
             Childs =
                 [yield visitSynExpr funcExpr
                  yield visitSynExpr argExpr]}
        | SynExpr.TypeApp(expr,lESSrange,typeNames,commaRanges,gREATERrange,typeArgsRange,range) ->
            {Type = "SynExpr.TypeApp"
             Range = r range
             Properties =
                 p [yield "lESSrange" ==> r lESSrange
                    yield "commaRanges" ==> (commaRanges |> List.map r)
                    if gREATERrange.IsSome then yield "gREATERrange" ==> r gREATERrange.Value
                    yield "typeArgsRange" ==> r typeArgsRange]
             FsAstNode = synExpr
             Childs =
                 [yield visitSynExpr expr
                  yield! typeNames |> List.map visitSynType]}
        | SynExpr.LetOrUse(isRecursive,isUse,bindings,body,range) ->
            {Type = "SynExpr.LetOrUse"
             Range = r range
             Properties =
                 p ["isRecursive" ==> isRecursive
                    "isUse" ==> isUse]
             FsAstNode = synExpr
             Childs =
                 [yield! bindings |> List.map visitSynBinding
                  yield visitSynExpr body]}
        | SynExpr.TryWith(tryExpr,tryRange,withCases,withRange,range,_,_) ->
            {Type = "SynExpr.TryWith"
             Range = r range
             Properties =
                 p ["tryRange" ==> r tryRange
                    "withRange" ==> r withRange]
             FsAstNode = synExpr
             Childs =
                 [yield visitSynExpr tryExpr
                  yield! withCases |> List.map visitSynMatchClause]}
        | SynExpr.TryFinally(tryExpr,finallyExpr,range,_,_) ->
            {Type = "SynExpr.TryFinally"
             Range = r range
             Properties = p []
             FsAstNode = synExpr
             Childs =
                 [yield visitSynExpr tryExpr
                  yield visitSynExpr finallyExpr]}
        | SynExpr.Lazy(ex,range) ->
            {Type = "SynExpr.Lazy"
             Range = r range
             Properties = p []
             FsAstNode = synExpr
             Childs = [yield visitSynExpr ex]}
        | Sequentials xs -> // use tail-rec active pattern to avoid stack overflow
            let rec cons xs =
                match xs with
                | [] -> failwith "should not happen" // expr2Opt is always Some in last item
                | ((isTrueSeq,expr1,expr2Opt,range)::rest) ->
                    {Type = "SynExpr.Sequential"
                     Range = r range
                     Properties = p ["isTrueSeq" ==> isTrueSeq]
                     FsAstNode = synExpr
                     Childs =
                         [yield visitSynExpr expr1
                          yield expr2Opt |> Option.map visitSynExpr |> Option.defaultWith (fun () -> cons rest)]}
            cons xs
        | SynExpr.Sequential(_,isTrueSeq,expr1,expr2,range) ->
            {Type = "SynExpr.Sequential"
             Range = r range
             Properties = p ["isTrueSeq" ==> isTrueSeq]
             FsAstNode = synExpr
             Childs =
                 [yield visitSynExpr expr1
                  yield visitSynExpr expr2]}
        | SynExpr.SequentialOrImplicitYield(seqPoint,expr1,expr2,ifNotStmt,range) ->
            {Type = "SynExpr.SequentialOrImplicitYield"
             Range = r range
             FsAstNode = synExpr
             Properties = p ["seqPoint" ==> seqPoint] // https://fsharp.github.io/FSharp.Compiler.Service/reference/fsharp-compiler-ast-sequencepointinfoforseq.html
             Childs = [ yield visitSynExpr expr1
                        yield visitSynExpr expr2
                        yield visitSynExpr ifNotStmt ]}
        | SynExpr.IfThenElse(ifExpr,thenExpr,elseExpr,_,isFromErrorRecovery,ifToThenRange,range) ->
            {Type = "SynExpr.IfThenElse"
             Range = r range
             Properties =
                 p ["isFromErrorRecovery" ==> isFromErrorRecovery
                    "ifToThenRange" ==> r ifToThenRange]
             FsAstNode = synExpr
             Childs =
                 [yield visitSynExpr ifExpr
                  yield visitSynExpr thenExpr
                  if elseExpr.IsSome then yield visitSynExpr elseExpr.Value]}
        | SynExpr.Ident(id) ->
            {Type = "SynExpr.Ident"
             Range = (i id).Range
             Properties = p ["ident" ==> i id]
             FsAstNode = synExpr
             Childs = []}
        | SynExpr.LongIdent(isOptional,longDotId,_,range) ->
            let ids = visitLongIdentWithDots longDotId
            {Type = "SynExpr.LongIdent"
             Range = r range
             Properties =
                 p ["isOptional" ==> isOptional
                    "longDotId" ==> lid longDotId]
             FsAstNode = synExpr
             Childs = ids}
        | SynExpr.LongIdentSet(longDotId,expr,range) ->
            {Type = "SynExpr.LongIdentSet"
             Range = r range
             Properties = p ["longDotId" ==> lid longDotId]
             FsAstNode = synExpr
             Childs = [yield visitSynExpr expr]}
        | SynExpr.DotGet(expr,rangeOfDot,longDotId,range) ->
            // Idents are collected as childs here to deal with unit test ``Fluent api with comments should remain on same lines``
            let ids = visitLongIdentWithDots longDotId
            {Type = "SynExpr.DotGet"
             Range = r range
             Properties =
                 p ["rangeOfDot" ==> r rangeOfDot
                    "longDotId" ==> lid longDotId]
             FsAstNode = synExpr
             Childs = [
                 yield visitSynExpr expr
                 yield! ids
             ]}
        | SynExpr.DotSet(expr,longDotId,e2,range) ->
            {Type = "SynExpr.DotSet"
             Range = r range
             Properties = p ["longDotId" ==> lid longDotId]
             FsAstNode = synExpr
             Childs =
                 [yield visitSynExpr expr
                  yield visitSynExpr e2]}
        | SynExpr.Set(e1,e2,range) ->
            {Type = "SynExpr.Set"
             Range = r range
             Properties = p []
             FsAstNode = synExpr
             Childs =
                 [yield visitSynExpr e1
                  yield visitSynExpr e2]}
        | SynExpr.DotIndexedGet(objectExpr,indexExprs,dotRange,range) ->
            {Type = "SynExpr.DotIndexedGet"
             Range = r range
             Properties = p ["dotRange" ==> r dotRange]
             FsAstNode = synExpr
             Childs =
                 [yield visitSynExpr objectExpr
                  yield! indexExprs |> List.map visitSynIndexerArg]}
        | SynExpr.DotIndexedSet(objectExpr,indexExprs,valueExpr,leftOfSetRange,dotRange,range) ->
            {Type = "SynExpr.DotIndexedSet"
             Range = r range
             Properties =
                 p ["leftOfSetRange" ==> r leftOfSetRange
                    "dotRange" ==> r dotRange]
             FsAstNode = synExpr
             Childs =
                 [yield visitSynExpr objectExpr
                  yield! indexExprs |> List.map visitSynIndexerArg
                  yield visitSynExpr valueExpr]}
        | SynExpr.NamedIndexedPropertySet(longDotId,e1,e2,range) ->
            {Type = "SynExpr.NamedIndexedPropertySet"
             Range = r range
             Properties = p ["longDotId" ==> lid longDotId]
             FsAstNode = synExpr
             Childs =
                 [yield visitSynExpr e1
                  yield visitSynExpr e2]}
        | SynExpr.DotNamedIndexedPropertySet(expr,longDotId,e1,e2,range) ->
            {Type = "SynExpr.DotNamedIndexedPropertySet"
             Range = r range
             Properties = p ["longDotId" ==> lid longDotId]
             FsAstNode = synExpr
             Childs =
                 [yield visitSynExpr expr
                  yield visitSynExpr e1
                  yield visitSynExpr e2]}
        | SynExpr.TypeTest(expr,typeName,range) ->
            {Type = "SynExpr.TypeTest"
             Range = r range
             Properties = p []
             FsAstNode = synExpr
             Childs =
                 [yield visitSynExpr expr
                  yield visitSynType typeName]}
        | SynExpr.Upcast(expr,typeName,range) ->
            {Type = "SynExpr.Upcast"
             Range = r range
             Properties = p []
             FsAstNode = synExpr
             Childs =
                 [yield visitSynExpr expr
                  yield visitSynType typeName]}
        | SynExpr.Downcast(expr,typeName,range) ->
            {Type = "SynExpr.Downcast"
             Range = r range
             Properties = p []
             FsAstNode = synExpr
             Childs =
                 [yield visitSynExpr expr
                  yield visitSynType typeName]}
        | SynExpr.InferredUpcast(expr,range) ->
            {Type = "SynExpr.InferredUpcast"
             Range = r range
             Properties = p []
             FsAstNode = synExpr
             Childs = [yield visitSynExpr expr]}
        | SynExpr.InferredDowncast(expr,range) ->
            {Type = "SynExpr.InferredDowncast"
             Range = r range
             Properties = p []
             FsAstNode = synExpr
             Childs = [yield visitSynExpr expr]}
        | SynExpr.Null(range) ->
            {Type = "SynExpr.Null"
             Range = r range
             Properties = p []
             FsAstNode = synExpr
             Childs = []}
        | SynExpr.AddressOf(isByref,expr,refRange,range) ->
            {Type = "SynExpr.AddressOf"
             Range = r range
             Properties =
                 p ["isByref" ==> isByref
                    "refRange" ==> r refRange]
             FsAstNode = synExpr
             Childs = [yield visitSynExpr expr]}
        | SynExpr.TraitCall(typars,sign,expr,range) ->
            {Type = "SynExpr.AddressOf"
             Range = r range
             Properties = p []
             FsAstNode = synExpr
             Childs =
                 [yield! typars |> List.map visitSynTypar
                  yield visitSynMemberSig sign
                  yield visitSynExpr expr]}
        | SynExpr.JoinIn(expr,inrange,expr2,range) ->
            {Type = "SynExpr.JoinIn"
             Range = r range
             Properties = p ["inRange" ==> r inrange]
             FsAstNode = synExpr
             Childs =
                 [yield visitSynExpr expr
                  yield visitSynExpr expr2]}
        | SynExpr.ImplicitZero(range) ->
            {Type = "SynExpr.ImplicitZero"
             Range = r range
             Properties = p []
             FsAstNode = synExpr
             Childs = []}
        | SynExpr.YieldOrReturn(_,expr,range) ->
            {Type = "SynExpr.YieldOrReturn"
             Range = r range
             Properties = p []
             FsAstNode = synExpr
             Childs = [yield visitSynExpr expr]}
        | SynExpr.YieldOrReturnFrom(_,expr,range) ->
            {Type = "SynExpr.YieldOrReturnFrom"
             Range = r range
             Properties = p []
             FsAstNode = synExpr
             Childs = [yield visitSynExpr expr]}
        | SynExpr.LetOrUseBang(_,isUse,isFromSource,pat,rhsExpr,andBangs,body,range) ->
            {Type = "SynExpr.LetOrUseBang"
             Range = r range
             Properties =
                 p ["isUse" ==> isUse
                    "isFromSource" ==> isFromSource]
             FsAstNode = synExpr
             Childs =
                 [yield visitSynPat pat
                  yield visitSynExpr rhsExpr
                  yield! andBangs |> List.collect (fun (_,_,_,pat,body,_) -> visitSynPat pat :: [visitSynExpr body])
                  yield visitSynExpr body]}
        | SynExpr.MatchBang(_,expr,clauses,range) ->
            {Type = "SynExpr.MatchBang"
             Range = r range
             Properties = p []
             FsAstNode = synExpr
             Childs =
                 [yield visitSynExpr expr
                  yield! clauses |> List.map visitSynMatchClause]}
        | SynExpr.DoBang(expr,range) ->
            {Type = "SynExpr.DoBang"
             Range = r range
             Properties = p []
             FsAstNode = synExpr
             Childs = [yield visitSynExpr expr]}
        | SynExpr.LibraryOnlyILAssembly(_,_,_,_,range) ->
            {Type = "SynExpr.LibraryOnlyILAssembly"
             Range = r range
             Properties = p []
             FsAstNode = synExpr
             Childs = []}
        | SynExpr.LibraryOnlyStaticOptimization(_,_,_,range) ->
            {Type = "SynExpr.LibraryOnlyStaticOptimization"
             Range = r range
             Properties = p []
             FsAstNode = synExpr
             Childs = []}
        | SynExpr.LibraryOnlyUnionCaseFieldGet(expr,longId,_,range) ->
            {Type = "SynExpr.LibraryOnlyUnionCaseFieldGet"
             Range = r range
             Properties = p ["longId" ==> li longId]
             FsAstNode = synExpr
             Childs = [yield visitSynExpr expr]}
        | SynExpr.LibraryOnlyUnionCaseFieldSet(e1,longId,_,e2,range) ->
            {Type = "SynExpr.LibraryOnlyUnionCaseFieldSet"
             Range = r range
             Properties = p ["longId" ==> li longId]
             FsAstNode = synExpr
             Childs =
                 [yield visitSynExpr e1
                  yield visitSynExpr e2]}
        | SynExpr.ArbitraryAfterError(debugStr,range) ->
            {Type = "SynExpr.ArbitraryAfterError"
             Range = r range
             Properties = p ["debugStr" ==> debugStr]
             FsAstNode = synExpr
             Childs = []}
        | SynExpr.FromParseError(expr,range) ->
            {Type = "SynExpr.FromParseError"
             Range = r range
             Properties = p []
             FsAstNode = synExpr
             Childs = [yield visitSynExpr expr]}
        | SynExpr.DiscardAfterMissingQualificationAfterDot(expr,range) ->
            {Type = "SynExpr.DiscardAfterMissingQualificationAfterDot"
             Range = r range
             Properties = p []
             FsAstNode = synExpr
             Childs = [yield visitSynExpr expr]}
        | SynExpr.Fixed(expr,range) ->
            {Type = "SynExpr.Fixed"
             Range = r range
             Properties = p []
             FsAstNode = synExpr
             Childs = [yield visitSynExpr expr]}

    and visitRecordField((longId,_) as rfn: RecordFieldName,expr: SynExpr option, _: BlockSeparator option) =
        {Type = "RecordField"
         Range = r longId.Range
         Properties = p ["ident" ==> lid longId]
         FsAstNode = rfn
         Childs =
             [if expr.IsSome then yield visitSynExpr expr.Value]}
    and visitAnonRecordField(ident: Ident,expr: SynExpr) =
        {Type = "AnonRecordField"
         Range = noRange
         Properties = p ["ident" ==> i ident]
         FsAstNode = expr
         Childs =
             [yield visitSynExpr expr]}
    and visitAnonRecordTypeField(ident: Ident,t: SynType) =
        {Type = "AnonRecordTypeField"
         Range = noRange
         Properties = p ["ident" ==> i ident]
         FsAstNode = t
         Childs =
             [yield visitSynType t]}

    and visitSynMemberSig(ms: SynMemberSig): Node =
        match ms with
        | SynMemberSig.Member(valSig,_,range) ->
            {Type = "SynMemberSig.Member"
             Range = r range
             Properties = p []
             FsAstNode = ms
             Childs = [yield visitSynValSig valSig]}
        | SynMemberSig.Interface(typeName,range) ->
            {Type = "SynMemberSig.Interface"
             Range = r range
             Properties = p []
             FsAstNode = ms
             Childs = [yield visitSynType typeName]}
        | SynMemberSig.Inherit(typeName,range) ->
            {Type = "SynMemberSig.Inherit"
             Range = r range
             Properties = p []
             FsAstNode = ms
             Childs = [yield visitSynType typeName]}
        | SynMemberSig.ValField(f,range) ->
            {Type = "SynMemberSig.ValField"
             Range = r range
             Properties = p []
             FsAstNode = ms
             Childs = [yield visitSynField f]}
        | SynMemberSig.NestedType(typedef,range) ->
            {Type = "SynMemberSig.NestedType"
             Range = r range
             Properties = p []
             FsAstNode = ms
             Childs = [yield visitSynTypeDefnSig typedef]}

    and visitSynIndexerArg(ia: SynIndexerArg): Node =
        match ia with
        | SynIndexerArg.One(e,_fromEnd,_) ->
            {Type = "SynIndexerArg.One"
             Range = noRange
             Properties = p []
             FsAstNode = ia
             Childs = [yield visitSynExpr e]}
        | SynIndexerArg.Two(e1,_fromEnd1,e2,_fromEnd2,_,_) ->
            {Type = "SynIndexerArg.Two"
             Range = noRange
             Properties = p []
             FsAstNode = ia
             Childs =
                 [yield visitSynExpr e1
                  yield visitSynExpr e2]}

    and visitSynMatchClause(mc: SynMatchClause): Node =
        match mc with
        | SynMatchClause.Clause(pat,e1,e2,range,_) ->
            {Type = "SynMatchClause.Clause"
             Range = r range
             Properties = p []
             FsAstNode = mc
             Childs =
                 [yield visitSynPat pat
                  if e1.IsSome then yield visitSynExpr e1.Value
                  yield visitSynExpr e2]}

    and visitArgsOption(expr: SynExpr,ident: Ident option) =
        {Type = "ArgOptions"
         Range = noRange
         Properties = p [if ident.IsSome then yield "ident" ==> i ident.Value]
         FsAstNode = expr
         Childs = [yield visitSynExpr expr]}

    and visitSynInterfaceImpl(ii: SynInterfaceImpl): Node =
        match ii with
        | InterfaceImpl(typ,bindings,range) ->
            {Type = "InterfaceImpl"
             Range = r range
             Properties = p []
             FsAstNode = ii
             Childs =
                 [yield visitSynType typ
                  yield! (bindings |> List.map visitSynBinding)]}

    and visitSynTypeDefn(td: SynTypeDefn) =
        match td with
        | TypeDefn(sci,stdr,members,range) ->
            {Type = "TypeDefn"
             Range = r range
             Properties = p []
             FsAstNode = td
             Childs =
                 [yield visitSynComponentInfo sci
                  yield visitSynTypeDefnRepr stdr
                  yield! (members |> List.map visitSynMemberDefn)]}

    and visitSynTypeDefnSig(typeDefSig: SynTypeDefnSig): Node =
        match typeDefSig with
        | TypeDefnSig(sci, synTypeDefnSigReprs,memberSig,range) ->
            {Type = "TypeDefnSig"
             Range = r range
             Properties = p []
             FsAstNode = typeDefSig
             Childs =
                 [yield visitSynComponentInfo sci
                  yield visitSynTypeDefnSigRepr synTypeDefnSigReprs
                  yield! (memberSig |> List.map visitSynMemberSig)]}

    and visitSynTypeDefnSigRepr(stdr: SynTypeDefnSigRepr): Node =
        match stdr with
        | SynTypeDefnSigRepr.ObjectModel(kind,members,range) ->
            {Type = "SynTypeDefnSigRepr.ObjectModel"
             Range = r range
             Properties = p []
             FsAstNode = stdr
             Childs =
                 [yield visitSynTypeDefnKind kind
                  yield! (members |> List.map visitSynMemberSig)]}
        | SynTypeDefnSigRepr.Simple(simpleRepr,range) ->
            {Type = "SynTypeDefnSigRepr.ObjectModel"
             Range = r range
             Properties = p []
             FsAstNode = stdr
             Childs = [yield visitSynTypeDefnSimpleRepr simpleRepr]}
        | SynTypeDefnSigRepr.Exception(exceptionRepr) ->
            {Type = "SynTypeDefnSigRepr.Exception"
             Range = noRange
             Properties = p []
             FsAstNode = stdr
             Childs = [yield visitSynExceptionDefnRepr exceptionRepr]}

    and visitSynMemberDefn(mbrDef: SynMemberDefn): Node =
        match mbrDef with
        | SynMemberDefn.Open(longIdent,range) ->
            {Type = "SynMemberDefn.Open"
             Range = r range
             Properties = p ["longIdent" ==> li longIdent]
             FsAstNode = mbrDef
             Childs = []}
        | SynMemberDefn.Member(memberDefn,range) ->
            {Type = "SynMemberDefn.Member"
             Range = r range
             Properties = p []
             FsAstNode = mbrDef
             Childs = [yield visitSynBinding memberDefn]}
        | SynMemberDefn.ImplicitCtor(access,attrs,ctorArgs,selfIdentifier,range) ->
            {Type = "SynMemberDefn.ImplicitCtor"
             Range = r range
             Properties =
                 p [if selfIdentifier.IsSome then yield "selfIdent" ==> i selfIdentifier.Value
                    if access.IsSome then yield "access" ==> (access.Value |> visitSynAccess)]
             FsAstNode = mbrDef
             Childs =
                 [yield! (visitSynAttributeLists range attrs)
                  yield visitSynSimplePats ctorArgs]}
        | SynMemberDefn.ImplicitInherit(inheritType,inheritArgs,inheritAlias,range) ->
            {Type = "SynMemberDefn.ImplicitInherit"
             Range = r range
             Properties = p [if inheritAlias.IsSome then yield "inheritAlias" ==> i inheritAlias.Value]
             FsAstNode = mbrDef
             Childs =
                 [yield visitSynType inheritType
                  yield visitSynExpr inheritArgs]}
        | SynMemberDefn.LetBindings(bindings,isStatic,isRecursive,range) ->
            {Type = "SynMemberDefn.LetBindings"
             Range = r range
             Properties =
                 p ["isStatic" ==> isStatic
                    "isRecursive" ==> isRecursive]
             FsAstNode = mbrDef
             Childs = [yield! bindings |> List.map visitSynBinding]}
        | SynMemberDefn.AbstractSlot(valSig,_,range) ->
            {Type = "SynMemberDefn.AbstractSlot"
             Range = r range
             Properties = p []
             FsAstNode = mbrDef
             Childs = [yield visitSynValSig valSig]}
        | SynMemberDefn.Interface(typ,members,range) ->
            {Type = "SynMemberDefn.Interface"
             Range = r range
             Properties = p []
             FsAstNode = mbrDef
             Childs =
                 [yield visitSynType typ
                  if members.IsSome then yield! members.Value |> List.map visitSynMemberDefn]}
        | SynMemberDefn.Inherit(typ,ident,range) ->
            {Type = "SynMemberDefn.Inherit"
             Range = r range
             Properties = p [if ident.IsSome then yield "ident" ==> i ident.Value]
             FsAstNode = mbrDef
             Childs = [yield visitSynType typ]}
        | SynMemberDefn.ValField(fld,range) ->
            {Type = "SynMemberDefn.ValField"
             Range = r range
             Properties = p []
             FsAstNode = mbrDef
             Childs = [yield visitSynField fld]}
        | SynMemberDefn.NestedType(typeDefn,access,range) ->
            {Type = "SynMemberDefn.NestedType"
             Range = r range
             Properties = p [if access.IsSome then yield "access" ==> (access.Value |> visitSynAccess)]
             FsAstNode = mbrDef
             Childs = [yield visitSynTypeDefn typeDefn]}
        | SynMemberDefn.AutoProperty(attrs,isStatic,ident,typeOpt,propKind,_,_,access,synExpr,getSetRange,range) ->
            {Type = "SynMemberDefn.AutoProperty"
             Range = r range
             Properties =
                 p [yield "isStatic" ==> isStatic
                    yield "ident" ==> i ident
                    yield "propKind" ==> visitMemberKind propKind
                    if access.IsSome then yield "access" ==> (access.Value |> visitSynAccess)
                    if getSetRange.IsSome then yield "getSetRange" ==> (getSetRange.Value |> r)]
             FsAstNode = mbrDef
             Childs =
                 [yield! (visitSynAttributeLists range attrs)
                  if typeOpt.IsSome then yield visitSynType typeOpt.Value
                  yield visitSynExpr synExpr]}

    and visitSynSimplePat(sp: SynSimplePat): Node =
        match sp with
        | SynSimplePat.Id(ident,_,isCompilerGenerated,isThisVar,isOptArg,range) ->
            {Type = "SynSimplePat.Id"
             Range = r range
             Properties =
                 p ["isCompilerGenerated" ==> isCompilerGenerated
                    "isThisVar" ==> isThisVar
                    "isOptArg" ==> isOptArg
                    "ident" ==> i ident]
             FsAstNode = sp
             Childs = []}
        | SynSimplePat.Typed(simplePat,typ,range) ->
            {Type = "SynSimplePat.Typed"
             Range = r range
             Properties = p []
             FsAstNode = sp
             Childs =
                 [yield visitSynSimplePat simplePat
                  yield visitSynType typ]}
        | SynSimplePat.Attrib(simplePat,attrs,range) ->
            {Type = "SynSimplePat.Attrib"
             Range = r range
             Properties = p []
             FsAstNode = sp
             Childs =
                 [yield visitSynSimplePat simplePat
                  yield! (visitSynAttributeLists range attrs)]}

    and visitSynSimplePats(sp: SynSimplePats): Node =
        match sp with
        | SynSimplePats.SimplePats(pats,range) ->
            {Type = "SynSimplePats.SimplePats"
             Range = r range
             Properties = p []
             FsAstNode = sp
             Childs = [yield! pats |> List.map visitSynSimplePat]}
        | SynSimplePats.Typed(pats,typ,range) ->
            {Type = "SynSimplePats.Typed"
             Range = r range
             Properties = p []
             FsAstNode = sp
             Childs =
                 [yield visitSynSimplePats pats
                  yield visitSynType typ]}

    and visitSynBinding(binding: SynBinding): Node =
        match binding with
        | Binding(access,kind,mustInline,isMutable,attrs,_,valData,headPat,returnInfo,expr,range,_) ->
            {Type = "Binding"
             Range = r range
             Properties =
                 p [yield "mustInline" ==> mustInline
                    yield "isMutable" ==> isMutable
                    yield "kind" ==> visitSynBindingKind kind
                    if access.IsSome then yield "access" ==> (access.Value |> visitSynAccess)]
             FsAstNode = binding
             Childs =
                 [yield! (visitSynAttributeLists range attrs)
                  yield visitSynValData valData
                  yield visitSynPat headPat
                  if returnInfo.IsSome then yield visitSynBindingReturnInfo returnInfo.Value
                  yield visitSynExpr expr]}

    and visitSynValData(svd: SynValData): Node =
        match svd with
        | SynValData(_,svi,ident) ->
            {Type = "Binding"
             Range = noRange
             Properties = p [ if ident.IsSome then yield "ident" ==> (ident.Value |> i)]
             FsAstNode = svd
             Childs = [yield visitSynValInfo svi]}

    and visitSynValSig(svs: SynValSig): Node =
        match svs with
        | ValSpfn(attrs,ident,explicitValDecls,synType,arity,isInline,isMutable,_,access,expr,range) ->
            {Type = "ValSpfn"
             Range = r range
             Properties =
                 p [yield "ident" ==> i ident
                    yield "isMutable" ==> isMutable
                    yield "isInline" ==> isInline
                    if access.IsSome then yield "access" ==> (access.Value |> visitSynAccess)]
             FsAstNode = svs
             Childs =
                 [yield! (visitSynAttributeLists range attrs)
                  yield visitSynValTyparDecls explicitValDecls
                  yield visitSynType synType
                  yield visitSynValInfo arity
                  if expr.IsSome then yield visitSynExpr expr.Value]}

    and visitSynValTyparDecls(valTypeDecl: SynValTyparDecls): Node =
        match valTypeDecl with
        | SynValTyparDecls(typardecls,_,_) ->
            {Type = "SynValTyparDecls"
             Range = noRange
             Properties = p []
             FsAstNode = valTypeDecl
             Childs = [yield! typardecls |> List.map visitSynTyparDecl]}

    and visitSynTyparDecl(std: SynTyparDecl): Node =
        match std with
        | TyparDecl(attrs,typar) ->
            {Type = "TyparDecl"
             Range = noRange
             Properties = p []
             FsAstNode = std
             Childs =
                 [yield! (visitSynAttributeLists typar.Range attrs)
                  yield visitSynTypar typar]}

    and visitSynTypar(typar: SynTypar): Node =
        match typar with
        | Typar(ident,staticReq,isComGen) ->
            {Type = "ValSpfn"
             Range = noRange
             Properties =
                 p ["ident" ==> i ident
                    "isComGen" ==> isComGen
                    "staticReq" ==> visitTyparStaticReq staticReq]
             FsAstNode = typar
             Childs = []}

    and visitTyparStaticReq(tsr: TyparStaticReq) =
        match tsr with
        | NoStaticReq -> "NoStaticReq"
        | HeadTypeStaticReq -> "HeadTypeStaticReq"

    and visitSynBindingReturnInfo(returnInfo: SynBindingReturnInfo): Node =
        match returnInfo with
        | SynBindingReturnInfo(typeName,range,attrs) ->
            {Type = "ComponentInfo"
             Range = r range
             Properties = p []
             FsAstNode = returnInfo
             Childs =
                 [yield visitSynType typeName
                  yield! (visitSynAttributeLists range attrs)]}

    and visitSynPat(sp: SynPat): Node =
        match sp with
        | SynPat.Const(sc,range) ->
            {Type = "SynPat.Const"
             Range = r range
             Properties = p ["const" ==> visitSynConst sc]
             FsAstNode = sp
             Childs = []}
        | SynPat.Wild(range) ->
            {Type = "SynPat.Wild"
             Range = r range
             Properties = p []
             FsAstNode = sp
             Childs = []}
        | SynPat.Named(synPat,ident,isSelfIdentifier,access,range) ->
            {Type = "SynPat.Named"
             Range = r range
             Properties =
                 p [yield "ident" ==> i ident
                    yield "isSelfIdentifier" ==> isSelfIdentifier
                    if access.IsSome then yield "access" ==> (access.Value |> visitSynAccess)]
             FsAstNode = sp
             Childs = [yield visitSynPat synPat]}
        | SynPat.Typed(synPat,synType,range) ->
            {Type = "SynPat.Typed"
             Range = r range
             Properties = p []
             FsAstNode = sp
             Childs =
                 [yield visitSynPat synPat
                  yield visitSynType synType]}
        | SynPat.Attrib(synPat,attrs,range) ->
            {Type = "SynPat.Attrib"
             Range = r range
             Properties = p []
             FsAstNode = sp
             Childs =
                 [yield visitSynPat synPat
                  yield! (visitSynAttributeLists range attrs)]}
        | SynPat.Or(synPat,synPat2,range) ->
            {Type = "SynPat.Or"
             Range = r range
             Properties = p []
             FsAstNode = sp
             Childs =
                 [yield visitSynPat synPat
                  yield visitSynPat synPat2]}
        | SynPat.Ands(pats,range) ->
            {Type = "SynPat.Ands"
             Range = r range
             Properties = p []
             FsAstNode = sp
             Childs = [yield! pats |> List.map visitSynPat]}
        | SynPat.LongIdent(longDotId,ident,svtd,ctorArgs,access,range) ->
            {Type = "SynPat.LongIdent"
             Range = r range
             Properties =
                 p [if ident.IsSome then yield "ident" ==> (ident.Value |> i)
                    yield "longDotId" ==> lid longDotId
                    if access.IsSome then yield "access" ==> (access.Value |> visitSynAccess)]
             FsAstNode = sp
             Childs =
                 [if svtd.IsSome then yield visitSynValTyparDecls svtd.Value
                  yield visitSynConstructorArgs ctorArgs]}
        | SynPat.Tuple(isStruct,pats,range) ->
            {Type = "SynPat.Tuple"
             Range = r range
             Properties = p ["isStruct" ==> isStruct]
             FsAstNode = sp
             Childs = [yield! pats |> List.map visitSynPat]}
        | SynPat.Paren(pat,range) ->
            {Type = "SynPat.Paren"
             Range = r range
             Properties = p []
             FsAstNode = sp
             Childs = [visitSynPat pat]}
        | SynPat.ArrayOrList(_,pats,range) ->
            {Type = "SynPat.ArrayOrList"
             Range = r range
             Properties = p []
             FsAstNode = sp
             Childs = [yield! pats |> List.map visitSynPat]}
        | SynPat.Record(pats,range) ->
            {Type = "SynPat.Record"
             Range = r range
             Properties = p []
             FsAstNode = sp
             Childs = [yield! pats |> List.map(snd >> visitSynPat)]}
        | SynPat.Null(range) ->
            {Type = "SynPat.Null"
             Range = r range
             Properties = p []
             FsAstNode = sp
             Childs = []}
        | SynPat.OptionalVal(ident,range) ->
            {Type = "SynPat.OptionalVal"
             Range = r range
             Properties = p ["ident" ==> i ident]
             FsAstNode = sp
             Childs = []}
        | SynPat.IsInst(typ,range) ->
            {Type = "SynPat.IsInst"
             Range = r range
             Properties = p []
             FsAstNode = sp
             Childs = [visitSynType typ]}
        | SynPat.QuoteExpr(expr,range) ->
            {Type = "SynPat.QuoteExpr"
             Range = r range
             Properties = p []
             FsAstNode = sp
             Childs = [visitSynExpr expr]}
        | SynPat.DeprecatedCharRange(c,c2,range) ->
            {Type = "SynPat.DeprecatedCharRange"
             Range = r range
             Properties =
                 p ["c" ==> c
                    "c2" ==> c2]
             FsAstNode = sp
             Childs = []}
        | SynPat.InstanceMember(ident,ident2,ident3,access,range) ->
            {Type = "SynPat.InstanceMember"
             Range = r range
             Properties =
                 p [yield "ident" ==> i ident
                    yield "ident2" ==> i ident2
                    if ident3.IsSome then yield "ident3" ==> (ident3.Value |> i)
                    if access.IsSome then yield "access" ==> (access.Value |> visitSynAccess)]
             FsAstNode = sp
             Childs = []}
        | SynPat.FromParseError(pat,range) ->
            {Type = "SynPat.FromParseError"
             Range = r range
             Properties = p []
             FsAstNode = sp
             Childs = [visitSynPat pat]}

    and visitSynConstructorArgs(ctorArgs: SynArgPats): Node =
        match ctorArgs with
        | Pats(pats) ->
            {Type = "Pats"
             Range = noRange
             Properties = p []
             FsAstNode = ctorArgs
             Childs = [yield! pats |> List.map visitSynPat]}
        | NamePatPairs(pats,range) ->
            {Type = "NamePatPairs"
             Range = r range
             Properties = p []
             FsAstNode = ctorArgs
             Childs = [yield! pats |> List.map(snd >> visitSynPat)]}

    and visitSynComponentInfo(sci: SynComponentInfo): Node =
        match sci with
        | ComponentInfo(attribs,typeParams,_,longId,_,preferPostfix,access,range) ->
            {Type = "ComponentInfo"
             Range = r range
             Properties =
                 p [yield "longIdent" ==> li longId
                    yield "preferPostfix" ==> preferPostfix
                    if access.IsSome then yield "access" ==> (access.Value |> visitSynAccess)]
             FsAstNode = sci
             Childs =
                 [yield! (visitSynAttributeLists range attribs)
                  yield! (typeParams |> List.map(visitSynTyparDecl))]}

    and visitSynTypeDefnRepr(stdr: SynTypeDefnRepr): Node =
        match stdr with
        | SynTypeDefnRepr.ObjectModel(kind,members,range) ->
            {Type = "SynTypeDefnRepr.ObjectModel"
             Range = r range
             Properties = p []
             FsAstNode = stdr
             Childs =
                 [yield visitSynTypeDefnKind kind
                  yield! (members |> List.map visitSynMemberDefn)]}
        | SynTypeDefnRepr.Simple(simpleRepr,range) ->
            {Type = "SynTypeDefnRepr.ObjectModel"
             Range = r range
             Properties = p []
             FsAstNode = stdr
             Childs = [yield visitSynTypeDefnSimpleRepr simpleRepr]}
        | SynTypeDefnRepr.Exception(exceptionRepr) ->
            {Type = "SynTypeDefnRepr.Exception"
             Range = noRange
             Properties = p []
             FsAstNode = stdr
             Childs = [yield visitSynExceptionDefnRepr exceptionRepr]}

    and visitSynTypeDefnKind(kind: SynTypeDefnKind) =
        match kind with
        | TyconUnspecified ->
            {Type = "SynTypeDefnKind.TyconUnspecified"
             Range = noRange
             Properties = p []
             FsAstNode = kind
             Childs = []}
        | TyconClass ->
            {Type = "SynTypeDefnKind.TyconClass"
             Range = noRange
             Properties = p []
             FsAstNode = kind
             Childs = []}
        | TyconInterface ->
            {Type = "SynTypeDefnKind.TyconInterface"
             Range = noRange
             Properties = p []
             FsAstNode = kind
             Childs = []}
        | TyconStruct ->
            {Type = "SynTypeDefnKind.TyconStruct"
             Range = noRange
             Properties = p []
             FsAstNode = kind
             Childs = []}
        | TyconRecord ->
            {Type = "SynTypeDefnKind.TyconRecord"
             Range = noRange
             Properties = p []
             FsAstNode = kind
             Childs = []}
        | TyconUnion ->
            {Type = "SynTypeDefnKind.TyconUnion"
             Range = noRange
             Properties = p []
             FsAstNode = kind
             Childs = []}
        | TyconAbbrev ->
            {Type = "SynTypeDefnKind.TyconAbbrev"
             Range = noRange
             Properties = p []
             FsAstNode = kind
             Childs = []}
        | TyconHiddenRepr ->
            {Type = "SynTypeDefnKind.TyconHiddenRepr"
             Range = noRange
             Properties = p []
             FsAstNode = kind
             Childs = []}
        | TyconAugmentation ->
            {Type = "SynTypeDefnKind.TyconAugmentation"
             Range = noRange
             Properties = p []
             FsAstNode = kind
             Childs = []}
        | TyconILAssemblyCode ->
            {Type = "SynTypeDefnKind.TyconILAssemblyCode"
             Range = noRange
             Properties = p []
             FsAstNode = kind
             Childs = []}
        | TyconDelegate(typ,valinfo) ->
            {Type = "SynTypeDefnKind.TyconDelegate"
             Range = noRange
             Properties = p []
             FsAstNode = kind
             Childs =
                 [yield visitSynType typ
                  yield visitSynValInfo valinfo]}

    and visitSynTypeDefnSimpleRepr(arg: SynTypeDefnSimpleRepr) =
        match arg with
        | SynTypeDefnSimpleRepr.None(range) ->
            {Type = "SynTypeDefnSimpleRepr.None"
             Range = r range
             Properties = p []
             FsAstNode = arg
             Childs = []}
        | SynTypeDefnSimpleRepr.Union(access,unionCases,range) ->
            {Type = "SynTypeDefnSimpleRepr.Union"
             Range = r range
             Properties = p [if access.IsSome then yield "access" ==> (access.Value |> visitSynAccess)]
             FsAstNode = arg
             Childs = [yield! unionCases |> List.map visitSynUnionCase]}
        | SynTypeDefnSimpleRepr.Enum(enumCases,range) ->
            {Type = "SynTypeDefnSimpleRepr.Enum"
             Range = r range
             Properties = p []
             FsAstNode = arg
             Childs = [yield! enumCases |> List.map visitSynEnumCase]}
        | SynTypeDefnSimpleRepr.Record(access,recordFields,range) ->
            {Type = "SynTypeDefnSimpleRepr.Record"
             Range = r range
             Properties = p [if access.IsSome then yield "access" ==> (access.Value |> visitSynAccess)]
             FsAstNode = arg
             Childs = [yield! recordFields |> List.map visitSynField]}
        | SynTypeDefnSimpleRepr.General(_,_,_,_,_,_,_,range) ->
            {Type = "SynTypeDefnSimpleRepr.General"
             Range = r range
             Properties = p []
             FsAstNode = arg
             Childs = []}
        | SynTypeDefnSimpleRepr.LibraryOnlyILAssembly(_,range) ->
            {Type = "SynTypeDefnSimpleRepr.LibraryOnlyILAssembly"
             Range = r range
             Properties = p []
             FsAstNode = arg
             Childs = []}
        | SynTypeDefnSimpleRepr.TypeAbbrev(_,typ,range) ->
            {Type = "SynTypeDefnSimpleRepr.TypeAbbrev"
             Range = r range
             Properties = p []
             FsAstNode = arg
             Childs = [visitSynType typ]}
        | SynTypeDefnSimpleRepr.Exception(edr) ->
            {Type = "SynTypeDefnSimpleRepr.Exception"
             Range = noRange
             Properties = p []
             FsAstNode = arg
             Childs = [visitSynExceptionDefnRepr edr]}

    and visitSynExceptionDefn(exceptionDef: SynExceptionDefn): Node =
        match exceptionDef with
        | SynExceptionDefn(sedr,members,range) ->
            {Type = "SynExceptionDefn"
             Range = r range
             Properties = p []
             FsAstNode = exceptionDef
             Childs =
                 [yield visitSynExceptionDefnRepr sedr
                  yield! (members |> List.map visitSynMemberDefn)]}

    and visitSynExceptionDefnRepr(sedr: SynExceptionDefnRepr): Node =
        match sedr with
        | SynExceptionDefnRepr(attrs,unionCase,longId,_,access,range) ->
            {Type = "SynExceptionDefnRepr"
             Range = r range
             Properties =
                 p [if longId.IsSome then yield "longIdent" ==> (longId.Value |> li)
                    if access.IsSome then yield "access" ==> (access.Value |> visitSynAccess)]
             FsAstNode = sedr
             Childs =
                 [yield! (visitSynAttributeLists range attrs)
                  yield visitSynUnionCase unionCase]}

    and visitSynAttribute(attr: SynAttribute): Node =
        {Type = "SynAttribute"
         Range = r attr.Range
         Properties =
             p [if attr.Target.IsSome then yield "target" ==> i attr.Target.Value
                yield "typeName" ==> lid attr.TypeName
                yield "appliesToGetterAndSetter" ==> attr.AppliesToGetterAndSetter
                yield "typeName" ==> lid attr.TypeName]
         FsAstNode = attr
         Childs = [visitSynExpr attr.ArgExpr]}

    and visitSynAttributeLists (parentRange:range) (attrs: SynAttributeList list) : Node list =
        match attrs with
        | [h] -> visitSynAttributeList parentRange h |> List.singleton
        | _::tail ->
            let aRanges =
                tail
                |> List.map (fun a -> a.Range)
                |> fun r -> r @ [parentRange]
            List.zip attrs aRanges
            |> List.map (fun (a,r) -> visitSynAttributeList r a)
        | [] -> []

    and visitSynAttributeList (parentRange:range) (attrs: SynAttributeList): Node =
        {Type = "SynAttributeList"
         Range = r attrs.Range
         Properties = p ["linesBetweenParent", box (parentRange.StartLine - attrs.Range.EndLine - 1)]
         FsAstNode = attrs
         Childs = attrs.Attributes |> List.map visitSynAttribute
        }

    and visitSynUnionCase(uc: SynUnionCase): Node =
        match uc with
        | UnionCase(attrs,ident,uct,_,access,range) ->
            {Type = "UnionCase"
             Range = r range
             Properties =
                 p [yield "ident" ==> i ident
                    if access.IsSome then yield "access" ==> (access.Value |> visitSynAccess)]
             FsAstNode = uc
             Childs =
                 [yield visitSynUnionCaseType uct
                  yield! (visitSynAttributeLists range attrs)]}

    and visitSynUnionCaseType(uct: SynUnionCaseType) =
        match uct with
        | UnionCaseFields(cases) ->
            {Type = "UnionCaseFields"
             Range = noRange
             Properties = p []
             FsAstNode = uct
             Childs = [yield! cases |> List.map visitSynField]}
        | UnionCaseFullType(stype,valInfo) ->
            {Type = "UnionCaseFullType"
             Range = noRange
             Properties = p []
             FsAstNode = uct
             Childs =
                 [yield visitSynType stype
                  yield visitSynValInfo valInfo]}

    and visitSynEnumCase(sec: SynEnumCase): Node =
        match sec with
        | EnumCase(attrs,ident,_,_,range) ->
            {Type = "EnumCase"
             Range = r range
             Properties = p []
             FsAstNode = sec
             Childs = [yield! (visitSynAttributeLists range attrs)
                       yield visitIdent ident]}

    and visitSynField(sfield: SynField): Node =
        match sfield with
        | Field(attrs,isStatic,ident,typ,_,_,access,range) ->
            let parentRange =
                Option.map (fun (i:Ident) -> i.idRange) ident |> Option.defaultValue range
            
            {Type = "Field"
             Range = r range
             Properties =
                 p [if ident.IsSome then yield "ident" ==> (ident.Value |> i)
                    yield "isStatic" ==> isStatic
                    if access.IsSome then yield "access" ==> (access.Value |> visitSynAccess)]
             FsAstNode = sfield
             Childs =
                 [yield! (visitSynAttributeLists parentRange attrs)
                  yield visitSynType typ]}

    and visitSynType(st: SynType) =
        match st with
        | SynType.LongIdent(li) ->
            {Type = "SynType.LongIdent"
             Range = noRange
             Properties = p ["ident" ==> lid li]
             FsAstNode = st
             Childs = []}
        | SynType.App(typeName,lESSrange,typeArgs,commaRanges,gREATERrange,isPostfix,range) ->
            {Type = "SynType.App"
             Range = r range
             Properties =
                 p [if lESSrange.IsSome then yield "lESSrange" ==> (lESSrange.Value |> r)
                    yield "commaRanges" ==> (commaRanges |> List.map r)
                    if gREATERrange.IsSome then yield "gREATERrange" ==> (gREATERrange.Value |> r)
                    yield "isPostfix" ==> isPostfix]
             FsAstNode = st
             Childs =
                 [yield! typeArgs |> List.map visitSynType
                  yield visitSynType typeName]}
        | SynType.LongIdentApp(typeName,longDotId,lESSRange,typeArgs,commaRanges,gREATERrange,range) ->
            {Type = "SynType.LongIdentApp"
             Range = r range
             Properties =
                 p [yield "ident" ==> lid longDotId
                    if lESSRange.IsSome then yield "lESSRange" ==> (lESSRange.Value |> r)
                    yield "commaRanges" ==> (commaRanges |> List.map r)
                    if gREATERrange.IsSome then yield "gREATERrange" ==> (gREATERrange.Value |> r)]
             FsAstNode = st
             Childs =
                 [yield! typeArgs |> List.map visitSynType
                  yield visitSynType typeName]}
        | SynType.Tuple(isStruct,typeNames,range) ->
            {Type = "SynType.Tuple"
             Range = r range
             Properties = p ["isStruct" ==> isStruct]
             FsAstNode = st
             Childs = [yield! typeNames |> List.map(snd >> visitSynType)]}
        | SynType.Array(_,elementType,range) ->
            {Type = "SynType.Array"
             Range = r range
             Properties = p []
             FsAstNode = st
             Childs = [yield visitSynType elementType]}
        | SynType.Fun(argType,returnType,range) ->
            {Type = "SynType.Fun"
             Range = r range
             Properties = p []
             FsAstNode = st
             Childs =
                 [yield visitSynType argType
                  yield visitSynType returnType]}
        | SynType.Var(genericName,range) ->
            {Type = "SynType.Var"
             Range = r range
             Properties = p []
             FsAstNode = st
             Childs = [yield visitSynTypar genericName]}
        | SynType.Anon(range) ->
            {Type = "SynType.Anon"
             Range = r range
             Properties = p []
             FsAstNode = st
             Childs = []}
        | SynType.WithGlobalConstraints(typeName,_,range) ->
            {Type = "SynType.WithGlobalConstraints"
             Range = r range
             Properties = p []
             FsAstNode = st
             Childs = [yield visitSynType typeName]}
        | SynType.HashConstraint(synType,range) ->
            {Type = "SynType.HashConstraint"
             Range = r range
             Properties = p []
             FsAstNode = st
             Childs = [yield visitSynType synType]}
        | SynType.MeasureDivide(dividendType,divisorType,range) ->
            {Type = "SynType.MeasureDivide"
             Range = r range
             Properties = p []
             FsAstNode = st
             Childs =
                 [yield visitSynType dividendType
                  yield visitSynType divisorType]}
        | SynType.MeasurePower(measureType,_,range) ->
            {Type = "SynType.MeasurePower"
             Range = r range
             Properties = p []
             FsAstNode = st
             Childs = [yield visitSynType measureType]}
        | SynType.StaticConstant(constant,range) ->
            {Type = "SynType.StaticConstant"
             Range = r range
             Properties = p ["constant" ==> visitSynConst constant]
             FsAstNode = st
             Childs = []}
        | SynType.StaticConstantExpr(expr,range) ->
            {Type = "SynType.StaticConstantExpr"
             Range = r range
             Properties = p []
             FsAstNode = st
             Childs = [yield visitSynExpr expr]}
        | SynType.StaticConstantNamed(expr,typ,range) ->
            {Type = "SynType.StaticConstantNamed"
             Range = r range
             Properties = p []
             FsAstNode = st
             Childs =
                 [yield visitSynType expr
                  yield visitSynType typ]}
        | SynType.AnonRecd(isStruct,typeNames,range) ->
            {Type = "SynType.AnonRecd"
             Range = r range
             Properties = p ["isStruct" ==> isStruct]
             FsAstNode = st
             Childs = List.map visitAnonRecordTypeField typeNames}
        | SynType.Paren(innerType,range) ->
            {Type = "SynType.Paren"
             Range = r range
             Properties = p []
             FsAstNode = st
             Childs = [yield visitSynType innerType]}

    and visitSynConst(sc: SynConst) = sprintf "%A" sc

    and visitSynValInfo(svi: SynValInfo) =
        match svi with
        | SynValInfo(args,arg) ->
            {Type = "SynValInfo"
             Range = noRange
             Properties = p []
             FsAstNode = svi
             Childs =
                 [yield! args |> List.collect(List.map visitSynArgInfo)
                  yield visitSynArgInfo arg]}

    and visitSynArgInfo(sai: SynArgInfo) =
        match sai with
        | SynArgInfo(attrs,optional,ident) ->
            let parentRange =
                ident
                |> Option.map (fun i -> i.idRange)
                |> Option.defaultValue range.Zero

            {Type = "SynArgInfo"
             Range = noRange
             Properties =
                 p [if ident.IsSome then yield "ident" ==> i ident.Value
                    yield "optional" ==> optional]
             FsAstNode = sai
             Childs = [yield! (visitSynAttributeLists parentRange attrs)]}

    and visitSynAccess(a: SynAccess) =
        match a with
        | SynAccess.Private -> "Private"
        | SynAccess.Internal -> "Internal"
        | SynAccess.Public -> "Public"

    and visitSynBindingKind(kind: SynBindingKind) =
        match kind with
        | SynBindingKind.DoBinding -> "Do Binding"
        | SynBindingKind.StandaloneExpression -> "Standalone Expression"
        | SynBindingKind.NormalBinding -> "Normal Binding"

    and visitMemberKind(mk: MemberKind) =
        match mk with
        | MemberKind.ClassConstructor -> "ClassConstructor"
        | MemberKind.Constructor -> "Constructor"
        | MemberKind.Member -> "Member"
        | MemberKind.PropertyGet -> "PropertyGet"
        | MemberKind.PropertySet -> "PropertySet"
        | MemberKind.PropertyGetSet -> "PropertyGetSet"

    and visitParsedHashDirective(hash: ParsedHashDirective): Node =
        match hash with
        | ParsedHashDirective(ident,longIdent,range) ->
            {Type = "ParsedHashDirective"
             Range = r range
             Properties =
                 p ["ident" ==> ident
                    "longIdent" ==> longIdent]
             FsAstNode = hash
             Childs = []}

    and visitSynModuleOrNamespaceSig(modOrNs: SynModuleOrNamespaceSig): Node =
        match modOrNs with
        | SynModuleOrNamespaceSig(longIdent,isRecursive,isModule,decls,_,attrs,access,range) ->
            {Type = sprintf "SynModuleOrNamespaceSig.%A" isModule
             Range = r range
             Properties =
                 p [yield "isRecursive" ==> isRecursive
                    yield "isModule" ==> isModule
                    yield "longIdent" ==> li longIdent
                    if access.IsSome then yield "access" ==> (access.Value |> visitSynAccess)]
             FsAstNode = modOrNs
             Childs =
                 [yield! (if isModule = SynModuleOrNamespaceKind.DeclaredNamespace then visitLongIdent longIdent else [])
                  yield! (visitSynAttributeLists range attrs)
                  yield! (decls |> List.map visitSynModuleSigDecl)]}

    and visitSynModuleSigDecl(ast: SynModuleSigDecl) : Node =
        match ast with
        | SynModuleSigDecl.ModuleAbbrev(ident,longIdent,range) ->
            {Type = "SynModuleSigDecl.ModuleAbbrev"
             Range = r range
             Properties =
                 p ["ident" ==> i ident
                    "longIdent" ==> li longIdent]
             FsAstNode = ast
             Childs = []}
        | SynModuleSigDecl.NestedModule(sci,isRecursive,decls,range) ->
            {Type = "SynModuleSigDecl.NestedModule"
             Range = r range
             Properties = p ["isRecursive" ==> isRecursive]
             FsAstNode = ast
             Childs =
                 [yield visitSynComponentInfo sci
                  yield! (decls |> List.map visitSynModuleSigDecl)]}
        | SynModuleSigDecl.Val(SynValSig.ValSpfn _ as node, _) ->
            visitSynValSig node
        | SynModuleSigDecl.Types(typeDefs,range) ->
            {Type = "SynModuleSigDecl.Types"
             Range = r range
             Properties = p []
             FsAstNode = ast
             Childs = typeDefs |> List.map visitSynTypeDefnSig}
        | SynModuleSigDecl.Open(longId,range) ->
            {Type = "SynModuleSigDecl.Open"
             Range = r range
             Properties = p ["longIdent" ==> li longId]
             FsAstNode = ast
             Childs = []}
        | SynModuleSigDecl.HashDirective(hash,range) ->
            {Type = "SynModuleSigDecl.HashDirective"
             Range = r range
             Properties = p []
             FsAstNode = ast
             Childs = [visitParsedHashDirective hash]}
        | SynModuleSigDecl.NamespaceFragment(moduleOrNamespace) ->
            {Type = "SynModuleDecl.NamespaceFragment"
             Range = noRange
             Properties = p []
             FsAstNode = ast
             Childs = [visitSynModuleOrNamespaceSig moduleOrNamespace]}
        | SynModuleSigDecl.Exception(synExceptionSig, range) ->
            {Type = "SynModuleSigDecl.Exception"
             Range = r range
             Properties = p []
             FsAstNode = ast
             Childs = [visitSynExceptionSig synExceptionSig]}

    and visitSynExceptionSig(exceptionDef: SynExceptionSig): Node =
        match exceptionDef with
        | SynExceptionSig(sedr,members,range) ->
            {Type = "SynExceptionSig"
             Range = r range
             Properties = p []
             FsAstNode = exceptionDef
             Childs =
                 [yield visitSynExceptionDefnRepr sedr
                  yield! (members |> List.map visitSynMemberSig)]}

    and visitLongIdentWithDots (lid: LongIdentWithDots): Node list =
        match lid with
        | LongIdentWithDots(ids,_) ->
            List.map visitIdent ids

    and visitLongIdent (li: LongIdent) : Node list =
        List.map visitIdent li

    and visitIdent (ident: Ident) : Node =
        { Type = "Ident"
          Range = r ident.idRange
          Properties = Map.empty
          FsAstNode = ident
          Childs = [] }

let astToNode (hds: ParsedHashDirective list) (mdls: SynModuleOrNamespace list): Node =
    let children =
        [ yield! List.map Ast.visit mdls
          yield! List.map Ast.visitParsedHashDirective hds ]
    {Type = "File"
     Range = None
     Properties = Map.empty
     FsAstNode = mdls
     Childs = children}

let sigAstToNode (ast: SynModuleOrNamespaceSig list) : Node =
    let children = List.map Ast.visitSynModuleOrNamespaceSig ast
    {Type = "SigFile"
     Range = None
     Properties = Map.empty
     FsAstNode = ast
     Childs = children}
