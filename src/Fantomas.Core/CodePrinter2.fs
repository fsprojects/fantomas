module internal rec Fantomas.Core.CodePrinter2

open System
open Fantomas.Core.Context
open Fantomas.Core.SyntaxOak

let rec (|UppercaseType|LowercaseType|) (t: Type) : Choice<unit, unit> =
    let upperOrLower (v: string) =
        let isUpper = Seq.tryHead v |> Option.map Char.IsUpper |> Option.defaultValue false
        if isUpper then UppercaseType else LowercaseType

    match t with
    | Type.LongIdent node ->
        let lastIdent =
            List.tryFindBack
                (function
                | IdentifierOrDot.Ident _ -> true
                | _ -> false)
                node.Content

        match lastIdent with
        | Some (IdentifierOrDot.Ident ident) -> upperOrLower ident.Text
        | _ -> LowercaseType
    | Type.Var node -> upperOrLower node.Text
    | Type.AppPostfix node -> (|UppercaseType|LowercaseType|) node.First
    | Type.AppPrefix node -> (|UppercaseType|LowercaseType|) node.Identifier
    | _ -> failwithf $"Cannot determine if synType %A{t} is uppercase or lowercase"

let genTrivia (trivia: TriviaNode) (ctx: Context) =
    let currentLastLine = ctx.WriterModel.Lines |> List.tryHead

    // Some items like #if or Newline should be printed on a newline
    // It is hard to always get this right in CodePrinter, so we detect it based on the current code.
    let addNewline =
        currentLastLine
        |> Option.map (fun line -> line.Trim().Length > 0)
        |> Option.defaultValue false

    let addSpace =
        currentLastLine
        |> Option.bind (fun line -> Seq.tryLast line |> Option.map (fun lastChar -> lastChar <> ' '))
        |> Option.defaultValue false

    let gen =
        match trivia.Content with
        | LineCommentAfterSourceCode s ->
            let comment = sprintf "%s%s" (if addSpace then " " else String.empty) s
            writerEvent (WriteBeforeNewline comment)
        | CommentOnSingleLine comment -> (ifElse addNewline sepNlnForTrivia sepNone) +> !-comment +> sepNlnForTrivia
        | Newline -> (ifElse addNewline (sepNlnForTrivia +> sepNlnForTrivia) sepNlnForTrivia)

    gen ctx

let enterNode<'n when 'n :> Node> (n: 'n) = col sepNone n.ContentBefore genTrivia
let leaveNode<'n when 'n :> Node> (n: 'n) = col sepNone n.ContentAfter genTrivia
let genNode<'n when 'n :> Node> (n: 'n) (f: Context -> Context) = enterNode n +> f +> leaveNode n

let genSingleTextNode (node: SingleTextNode) = !-node.Text |> genNode node
let genSingleTextNodeWithLeadingDot (node: SingleTextNode) = !- $".{node.Text}" |> genNode node

let genMultipleTextsNode (node: MultipleTextsNode) =
    col sepSpace node.Content genSingleTextNode

let genIdentListNodeAux addLeadingDot (iln: IdentListNode) =
    col sepNone iln.Content (fun identOrDot ->
        match identOrDot with
        | IdentifierOrDot.Ident ident ->
            if addLeadingDot then
                genSingleTextNodeWithLeadingDot ident
            else
                genSingleTextNode ident
        | IdentifierOrDot.KnownDot _
        | IdentifierOrDot.UnknownDot _ -> sepDot)
    |> genNode iln

let genIdentListNode iln = genIdentListNodeAux false iln
let genIdentListNodeWithDot iln = genIdentListNodeAux true iln

let genAccessOpt (nodeOpt: SingleTextNode option) =
    match nodeOpt with
    | None -> sepNone
    | Some node -> genSingleTextNode node +> sepSpace

let addSpaceBeforeParenInPattern (node: IdentListNode) (ctx: Context) =
    node.Content
    |> List.tryFindBack (function
        | IdentifierOrDot.Ident node -> not (String.IsNullOrWhiteSpace node.Text)
        | _ -> false)
    |> fun identOrDot ->
        match identOrDot with
        | Some (IdentifierOrDot.Ident node) ->
            let parameterValue =
                if Char.IsUpper node.Text.[0] then
                    ctx.Config.SpaceBeforeUppercaseInvocation
                else
                    ctx.Config.SpaceBeforeLowercaseInvocation

            onlyIf parameterValue sepSpace ctx
        | _ -> sepSpace ctx

let genParsedHashDirective (phd: ParsedHashDirectiveNode) =
    !- "#" +> !-phd.Ident +> sepSpace +> col sepSpace phd.Args genSingleTextNode
    |> genNode phd

let genUnit (n: UnitNode) =
    genSingleTextNode n.OpeningParen +> genSingleTextNode n.ClosingParen

// genNode will should be called in the caller function.
let genConstant (c: Constant) =
    match c with
    | Constant.FromText n -> genSingleTextNode n
    | Constant.Unit n -> genUnit n

let genAttributesCore (ats: AttributeNode list) =
    let genAttributeExpr (attr: AttributeNode) =
        match attr.Expr with
        | None -> opt sepColon attr.Target genSingleTextNode +> genIdentListNode attr.TypeName
        | Some e ->
            let argSpacing = if e.HasParentheses then sepNone else sepSpace

            opt sepColon attr.Target genSingleTextNode
            +> genIdentListNode attr.TypeName
            +> argSpacing
            +> genExpr e
        |> genNode attr

    let shortExpression = atCurrentColumn (col sepSemi ats genAttributeExpr)
    let longExpression = atCurrentColumn (col (sepSemi +> sepNln) ats genAttributeExpr)
    ifElse ats.IsEmpty sepNone (expressionFitsOnRestOfLine shortExpression longExpression)

let genOnelinerAttributes (n: MultipleAttributeListNode) =
    let ats =
        List.collect (fun (al: AttributeListNode) -> al.Attributes) n.AttributeLists

    let openingToken =
        List.tryHead n.AttributeLists
        |> Option.map (fun (a: AttributeListNode) -> a.Opening)

    let closingToken =
        List.tryLast n.AttributeLists
        |> Option.map (fun (a: AttributeListNode) -> a.Closing)

    let genAttrs =
        optSingle genSingleTextNode openingToken
        +> genAttributesCore ats
        +> optSingle genSingleTextNode closingToken
        |> genNode n

    ifElse ats.IsEmpty sepNone (genAttrs +> sepSpace)

let genAttributes (node: MultipleAttributeListNode) =
    colPost sepNlnUnlessLastEventIsNewline sepNln node.AttributeLists (fun a ->
        genSingleTextNode a.Opening
        +> (genAttributesCore a.Attributes)
        +> genSingleTextNode a.Closing
        +> sepNlnWhenWriteBeforeNewlineNotEmpty)

let genInheritConstructor (ic: InheritConstructor) =
    match ic with
    | InheritConstructor.TypeOnly node -> genSingleTextNode node.InheritKeyword +> sepSpace +> genType node.Type
    | InheritConstructor.Unit node ->
        genSingleTextNode node.InheritKeyword
        +> sepSpace
        +> genType node.Type
        +> sepSpaceBeforeClassConstructor
        +> genSingleTextNode node.OpeningParen
        +> genSingleTextNode node.ClosingParen
    | InheritConstructor.Paren node ->
        genSingleTextNode node.InheritKeyword
        +> sepSpace
        +> genType node.Type
        +> sepSpaceBeforeClassConstructor
        +> expressionFitsOnRestOfLine (genExpr node.Expr) (genMultilineFunctionApplicationArguments node.Expr)
    | InheritConstructor.Other node ->
        genSingleTextNode node.InheritKeyword
        +> sepSpace
        +> genType node.Type
        +> sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidth (genExpr node.Expr)

let genExpr (e: Expr) =
    match e with
    | Expr.Lazy node ->
        let genInfixExpr (ctx: Context) =
            isShortExpression
                ctx.Config.MaxInfixOperatorExpression
                // if this fits on the rest of line right after the lazy keyword, it should be wrapped in parenthesis.
                (sepOpenT +> genExpr node.Expr +> sepCloseT)
                // if it is multiline there is no need for parenthesis, because of the indentation
                (indent +> sepNln +> genExpr node.Expr +> unindent)
                ctx

        let genNonInfixExpr =
            autoIndentAndNlnIfExpressionExceedsPageWidth (genExpr node.Expr)

        genSingleTextNode node.LazyWord
        +> sepSpaceUnlessWriteBeforeNewlineNotEmpty
        +> ifElse node.ExprIsInfix genInfixExpr genNonInfixExpr
    | Expr.Single node ->
        genSingleTextNode node.Leading
        +> sepSpace
        +> ifElse
            node.SupportsStroustrup
            (autoIndentAndNlnIfExpressionExceedsPageWidthUnlessStroustrup genExpr node.Expr)
            (autoIndentAndNlnIfExpressionExceedsPageWidth (genExpr node.Expr))
    | Expr.Constant node -> genConstant node
    | Expr.Null node -> genSingleTextNode node
    | Expr.Quote node -> genQuoteExpr node
    | Expr.Typed node ->
        let short =
            genExpr node.Expr
            +> sepSpace
            +> !-node.Operator
            +> sepSpace
            +> genType node.Type

        let long =
            genExpr node.Expr +> sepNln +> !-node.Operator +> sepSpace +> genType node.Type

        match node.Expr with
        | Expr.Lambda _ -> long
        | _ -> expressionFitsOnRestOfLine short long
    | Expr.NewParen node ->
        let sepSpaceBeforeArgs (ctx: Context) =
            match node.Type with
            | UppercaseType -> onlyIf ctx.Config.SpaceBeforeUppercaseInvocation sepSpace ctx
            | LowercaseType -> onlyIf ctx.Config.SpaceBeforeLowercaseInvocation sepSpace ctx

        let short =
            genSingleTextNode node.NewKeyword
            +> sepSpace
            +> genType node.Type
            +> sepSpaceBeforeArgs
            +> genExpr node.Arguments

        let long =
            genSingleTextNode node.NewKeyword
            +> sepSpace
            +> genType node.Type
            +> sepSpaceBeforeArgs
            +> genMultilineFunctionApplicationArguments node.Arguments

        expressionFitsOnRestOfLine short long
    | Expr.New _ -> failwith "Not Implemented"
    | Expr.Tuple _ -> failwith "Not Implemented"
    | Expr.StructTuple _ -> failwith "Not Implemented"
    | Expr.ArrayOrList node ->
        if node.Elements.IsEmpty then
            genSingleTextNode node.Opening +> genSingleTextNode node.Closing
        else
            let smallExpression =
                genSingleTextNode node.Opening
                +> addSpaceIfSpaceAroundDelimiter
                +> col sepSemi node.Elements genExpr
                +> addSpaceIfSpaceAroundDelimiter
                +> genSingleTextNode node.Closing

            let multilineExpression =
                let genMultiLineArrayOrListAlignBrackets =
                    genSingleTextNode node.Opening
                    +> indent
                    +> sepNlnUnlessLastEventIsNewline
                    +> col sepNln node.Elements genExpr
                    +> unindent
                    +> sepNlnUnlessLastEventIsNewline
                    +> genSingleTextNode node.Closing

                let genMultiLineArrayOrList =
                    genSingleTextNode node.Opening
                    +> addSpaceIfSpaceAroundDelimiter
                    +> atCurrentColumnIndent (
                        sepNlnWhenWriteBeforeNewlineNotEmpty
                        +> col sepNln node.Elements genExpr
                        +> (enterNode node.Closing
                            +> (fun ctx ->
                                let isFixed = lastWriteEventIsNewline ctx
                                (onlyIfNot isFixed sepSpace +> leaveNode node.Closing) ctx))
                    )

                ifAlignBrackets genMultiLineArrayOrListAlignBrackets genMultiLineArrayOrList

            fun ctx ->
                let alwaysMultiline = false
                // List.exists isIfThenElseWithYieldReturn xs
                // || List.forall isSynExprLambdaOrIfThenElse xs
                if alwaysMultiline then
                    multilineExpression ctx
                else
                    let size = getListOrArrayExprSize ctx ctx.Config.MaxArrayOrListWidth node.Elements
                    isSmallExpression size smallExpression multilineExpression ctx
    | Expr.Record node ->
        let genRecordFieldName (node: RecordFieldNode) =
            genIdentListNode node.FieldName
            +> sepSpace
            +> genSingleTextNode node.Equals
            +> sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidth (genExpr node.Expr)
            |> genNode node

        let fieldsExpr = col sepNln node.Fields genRecordFieldName
        let hasFields = List.isNotEmpty node.Fields

        let smallRecordExpr =
            genSingleTextNode node.OpeningBrace
            +> addSpaceIfSpaceAroundDelimiter
            +> match node.Extra with
               | RecordNodeExtra.Inherit ie -> genInheritConstructor ie +> onlyIf hasFields sepSemi
               | RecordNodeExtra.With we -> genExpr we +> !- " with "
               | RecordNodeExtra.None -> sepNone
            +> col sepSemi node.Fields genRecordFieldName
            +> addSpaceIfSpaceAroundDelimiter
            +> genSingleTextNode node.ClosingBrace

        let multilineRecordExpr =
            let genMultilineRecordInstanceAlignBrackets =
                match node.Extra with
                | RecordNodeExtra.Inherit ie ->
                    genSingleTextNode node.OpeningBrace
                    +> indentSepNlnUnindent (
                        !- "inherit "
                        +> autoIndentAndNlnIfExpressionExceedsPageWidth (genInheritConstructor ie)
                        +> onlyIf hasFields sepNln
                        +> fieldsExpr
                    )
                    +> sepNln
                    +> genSingleTextNode node.ClosingBrace
                | RecordNodeExtra.With we ->
                    genSingleTextNode node.OpeningBrace
                    +> atCurrentColumnIndent (genExpr we)
                    +> !- " with"
                    +> indent
                    +> whenShortIndent indent
                    +> sepNln
                    +> fieldsExpr
                    +> unindent
                    +> whenShortIndent unindent
                    +> sepNln
                    +> genSingleTextNode node.ClosingBrace
                | RecordNodeExtra.None ->
                    genSingleTextNode node.OpeningBrace
                    +> indentSepNlnUnindent fieldsExpr
                    +> ifElseCtx lastWriteEventIsNewline sepNone sepNln
                    +> genSingleTextNode node.ClosingBrace

            let genMultilineRecordInstance =
                match node.Extra with
                | RecordNodeExtra.Inherit ie ->
                    genSingleTextNode node.OpeningBrace
                    +> addSpaceIfSpaceAroundDelimiter
                    +> atCurrentColumn (
                        !- "inherit "
                        +> autoIndentAndNlnIfExpressionExceedsPageWidth (genInheritConstructor ie)
                        +> onlyIf hasFields sepNln
                        +> fieldsExpr
                        +> addSpaceIfSpaceAroundDelimiter
                        +> genSingleTextNode node.ClosingBrace
                    )
                | RecordNodeExtra.With we ->
                    genSingleTextNode node.OpeningBrace
                    +> addSpaceIfSpaceAroundDelimiter
                    +> atCurrentColumnIndent (genExpr we)
                    +> !- " with"
                    +> indent
                    +> whenShortIndent indent
                    +> sepNln
                    +> fieldsExpr
                    +> unindent
                    +> whenShortIndent unindent
                    +> addSpaceIfSpaceAroundDelimiter
                    +> genSingleTextNode node.ClosingBrace
                | RecordNodeExtra.None ->
                    // TODO: revist when everything is ported
                    fun (ctx: Context) ->
                        // position after `{ ` or `{`
                        let targetColumn = ctx.Column + (if ctx.Config.SpaceAroundDelimiter then 2 else 1)

                        atCurrentColumn
                            (genSingleTextNode node.OpeningBrace
                             +> addSpaceIfSpaceAroundDelimiter
                             +> sepNlnWhenWriteBeforeNewlineNotEmpty // comment after curly brace
                             +> col sepNln node.Fields (fun e ->
                                 // Add spaces to ensure the record field (incl trivia) starts at the right column.
                                 addFixedSpaces targetColumn
                                 // Lock the start of the record field, however keep potential indentations in relation to the opening curly brace
                                 +> atCurrentColumn (genRecordFieldName e))
                             +> sepNlnWhenWriteBeforeNewlineNotEmpty
                             +> addSpaceIfSpaceAroundDelimiter
                             +> genSingleTextNode node.ClosingBrace)
                            ctx

            ifAlignBrackets genMultilineRecordInstanceAlignBrackets genMultilineRecordInstance

        fun ctx ->
            let size = getRecordSize ctx node.Fields
            isSmallExpression size smallRecordExpr multilineRecordExpr ctx
    | Expr.AnonRecord _ -> failwith "Not Implemented"
    | Expr.ObjExpr _ -> failwith "Not Implemented"
    | Expr.While _ -> failwith "Not Implemented"
    | Expr.For _ -> failwith "Not Implemented"
    | Expr.ForEach _ -> failwith "Not Implemented"
    | Expr.NamedComputation _ -> failwith "Not Implemented"
    | Expr.Computation _ -> failwith "Not Implemented"
    | Expr.CompExprBody _ -> failwith "Not Implemented"
    | Expr.JoinIn _ -> failwith "Not Implemented"
    | Expr.ParenLambda _ -> failwith "Not Implemented"
    | Expr.Lambda _ -> failwith "Not Implemented"
    | Expr.MatchLambda _ -> failwith "Not Implemented"
    | Expr.Match _ -> failwith "Not Implemented"
    | Expr.TraitCall _ -> failwith "Not Implemented"
    | Expr.ParenILEmbedded _ -> failwith "Not Implemented"
    | Expr.ParenFunctionNameWithStar _ -> failwith "Not Implemented"
    | Expr.Paren node ->
        genSingleTextNode node.OpeningParen
        +> genExpr node.Expr
        +> genSingleTextNode node.ClosingParen
    | Expr.Dynamic _ -> failwith "Not Implemented"
    | Expr.PrefixApp _ -> failwith "Not Implemented"
    | Expr.NewlineInfixAppAlwaysMultiline _ -> failwith "Not Implemented"
    | Expr.NewlineInfixApps _ -> failwith "Not Implemented"
    | Expr.SameInfixApps _ -> failwith "Not Implemented"
    | Expr.InfixApp _ -> failwith "nope"
    | Expr.TernaryApp _ -> failwith "Not Implemented"
    | Expr.IndexWithoutDot _ -> failwith "Not Implemented"
    | Expr.AppDotGetTypeApp _ -> failwith "Not Implemented"
    | Expr.DotGetAppDotGetAppParenLambda _ -> failwith "Not Implemented"
    | Expr.DotGetAppParen _ -> failwith "Not Implemented"
    | Expr.DotGetAppWithParenLambda _ -> failwith "Not Implemented"
    | Expr.DotGetApp _ -> failwith "Not Implemented"
    | Expr.AppLongIdentAndSingleParenArg _ -> failwith "Not Implemented"
    | Expr.AppSingleParenArg _ -> failwith "Not Implemented"
    | Expr.DotGetAppWithLambda _ -> failwith "Not Implemented"
    | Expr.AppWithLambda _ -> failwith "Not Implemented"
    | Expr.NestedIndexWithoutDot _ -> failwith "Not Implemented"
    | Expr.EndsWithDualListApp node ->
        fun ctx ->
            // check if everything else beside the last array/list fits on one line
            let singleLineTestExpr =
                genExpr node.FunctionExpr
                +> sepSpace
                +> col sepSpace node.SequentialExpr genExpr
                +> sepSpace
                +> genExpr node.FirstArrayOrList

            let short =
                genExpr node.FunctionExpr
                +> sepSpace
                +> col sepSpace node.SequentialExpr genExpr
                +> onlyIfNot node.SequentialExpr.IsEmpty sepSpace
                +> genExpr node.FirstArrayOrList
                +> sepSpace
                +> genExpr node.LastArrayOrList

            let long =
                // check if everything besides both lists fits on one line
                let singleLineTestExpr =
                    genExpr node.FunctionExpr
                    +> sepSpace
                    +> col sepSpace node.SequentialExpr genExpr

                if futureNlnCheck singleLineTestExpr ctx then
                    genExpr node.FunctionExpr
                    +> indent
                    +> sepNln
                    +> col sepNln node.SequentialExpr genExpr
                    +> sepSpace
                    +> genExpr node.FirstArrayOrList
                    +> sepSpace
                    +> genExpr node.LastArrayOrList
                    +> unindent
                else
                    genExpr node.FunctionExpr
                    +> sepSpace
                    +> col sepSpace node.SequentialExpr genExpr
                    +> genExpr node.FirstArrayOrList
                    +> sepSpace
                    +> genExpr node.LastArrayOrList

            if futureNlnCheck singleLineTestExpr ctx then
                long ctx
            else
                short ctx
    | Expr.EndsWithSingleListApp node ->
        fun ctx ->
            // check if everything else beside the last array/list fits on one line
            let singleLineTestExpr =
                genExpr node.FunctionExpr
                +> sepSpace
                +> col sepSpace node.SequentialExpr genExpr

            let short =
                genExpr node.FunctionExpr
                +> sepSpace
                +> col sepSpace node.SequentialExpr genExpr
                +> onlyIfNot node.SequentialExpr.IsEmpty sepSpace
                +> genExpr node.ArrayOrList

            let long =
                genExpr node.FunctionExpr
                +> indent
                +> sepNln
                +> col sepNln node.SequentialExpr genExpr
                +> onlyIfNot node.SequentialExpr.IsEmpty sepNln
                +> genExpr node.ArrayOrList
                +> unindent

            if futureNlnCheck singleLineTestExpr ctx then
                long ctx
            else
                short ctx
    | Expr.App _ -> failwith "Not Implemented"
    | Expr.TypeApp _ -> failwith "Not Implemented"
    | Expr.LetOrUses _ -> failwith "Not Implemented"
    | Expr.TryWithSingleClause _ -> failwith "Not Implemented"
    | Expr.TryWith _ -> failwith "Not Implemented"
    | Expr.TryFinally _ -> failwith "Not Implemented"
    | Expr.Sequentials _ -> failwith "Not Implemented"
    | Expr.IfThen _ -> failwith "Not Implemented"
    | Expr.IfThenElse _ -> failwith "Not Implemented"
    | Expr.IfThenElif _ -> failwith "Not Implemented"
    | Expr.Ident node -> !-node.Text
    | Expr.OptVar _ -> failwith "Not Implemented"
    | Expr.LongIdentSet _ -> failwith "Not Implemented"
    | Expr.DotIndexedGet _ -> failwith "Not Implemented"
    | Expr.DotIndexedSet _ -> failwith "Not Implemented"
    | Expr.NamedIndexedPropertySet _ -> failwith "Not Implemented"
    | Expr.DotNamedIndexedPropertySet _ -> failwith "Not Implemented"
    | Expr.DotGet _ -> failwith "Not Implemented"
    | Expr.DotSet _ -> failwith "Not Implemented"
    | Expr.Set _ -> failwith "Not Implemented"
    | Expr.LibraryOnlyStaticOptimization _ -> failwith "Not Implemented"
    | Expr.InterpolatedStringExpr _ -> failwith "Not Implemented"
    | Expr.IndexRangeWildcard _ -> failwith "Not Implemented"
    | Expr.IndexRange _ -> failwith "Not Implemented"
    | Expr.IndexFromEnd _ -> failwith "Not Implemented"
    | Expr.Typar _ -> failwith "Not Implemented"
    |> genNode (Expr.Node e)

let genQuoteExpr (node: ExprQuoteNode) =
    genSingleTextNode node.OpenToken
    +> sepSpace
    +> expressionFitsOnRestOfLine (genExpr node.Expr) (indent +> sepNln +> genExpr node.Expr +> unindent +> sepNln)
    +> sepSpace
    +> genSingleTextNode node.CloseToken

let genMultilineFunctionApplicationArguments (argExpr: Expr) = !- "todo!"
// let argsInsideParenthesis lpr rpr pr f =
//     sepOpenTFor lpr +> indentSepNlnUnindent f +> sepNln +> sepCloseTFor rpr
//     |> genTriviaFor SynExpr_Paren pr
//
// let genExpr e =
//     match e with
//     | InfixApp (equal, operatorSli, e1, e2, range) when (equal = "=") ->
//         genNamedArgumentExpr operatorSli e1 e2 range
//     | _ -> genExpr e
//
// match argExpr with
// | Paren (lpr, Lambda (pats, arrowRange, body, range), rpr, _pr) ->
//     fun ctx ->
//         if ctx.Config.MultiLineLambdaClosingNewline then
//             let genPats =
//                 let shortPats = col sepSpace pats genPat
//                 let longPats = atCurrentColumn (sepNln +> col sepNln pats genPat)
//                 expressionFitsOnRestOfLine shortPats longPats
//
//             (sepOpenTFor lpr
//              +> (!- "fun " +> genPats +> genLambdaArrowWithTrivia genExpr body arrowRange
//                  |> genTriviaFor SynExpr_Lambda range)
//              +> sepNln
//              +> sepCloseTFor rpr)
//                 ctx
//         else
//             genExpr argExpr ctx
// | Paren (lpr, Tuple (args, tupleRange), rpr, pr) ->
//     genTupleMultiline args
//     |> genTriviaFor SynExpr_Tuple tupleRange
//     |> argsInsideParenthesis lpr rpr pr
// | Paren (lpr, singleExpr, rpr, pr) -> genExpr singleExpr |> argsInsideParenthesis lpr rpr pr
// | _ -> genExpr argExpr

let genPatLeftMiddleRight (node: PatLeftMiddleRight) =
    genPat node.LeftHandSide
    +> sepSpace
    +> (match node.Middle with
        | Choice1Of2 node -> genSingleTextNode node
        | Choice2Of2 text -> !-text)
    +> sepSpace
    +> genPat node.RightHandSide

let genTyparDecls (td: TyparDecls) = !- "todo"

let genPat (p: Pattern) =
    match p with
    | Pattern.OptionalVal n -> genSingleTextNode n
    | Pattern.Attrib node -> genOnelinerAttributes node.Attributes +> genPat node.Pattern
    | Pattern.Or node -> genPatLeftMiddleRight node
    | Pattern.Ands node -> col (!- " & ") node.Patterns genPat
    | Pattern.Null node
    | Pattern.Wild node -> genSingleTextNode node
    | Pattern.Typed node ->
        genPat node.Pattern
        +> sepColon
        +> autoIndentAndNlnIfExpressionExceedsPageWidth (atCurrentColumnIndent (genType node.Type))
    | Pattern.Named node -> genSingleTextNode node.Name
    | Pattern.As node
    | Pattern.ListCons node -> genPatLeftMiddleRight node
    | Pattern.NamePatPairs node ->
        let genPatWithIdent (node: NamePatPair) =
            genSingleTextNode node.Ident
            +> sepSpace
            +> genSingleTextNode node.Equals
            +> sepSpace
            +> genPat node.Pattern

        let pats =
            expressionFitsOnRestOfLine
                (atCurrentColumn (col sepSemi node.Pairs genPatWithIdent))
                (atCurrentColumn (col sepNln node.Pairs genPatWithIdent))

        genIdentListNode node.Identifier
        +> optSingle genTyparDecls node.TyparDecls
        +> addSpaceBeforeParenInPattern node.Identifier
        +> genSingleTextNode node.OpeningParen
        +> autoIndentAndNlnIfExpressionExceedsPageWidth (sepNlnWhenWriteBeforeNewlineNotEmpty +> pats)
        +> genSingleTextNode node.ClosingParen

    | Pattern.LongIdent node ->
        let genName =
            genAccessOpt node.Accessibility
            +> genIdentListNode node.Identifier
            +> optSingle genTyparDecls node.TyparDecls

        let genParameters =
            match node.Parameters with
            | [] -> sepNone
            | [ Pattern.Paren _ as parameter ] -> addSpaceBeforeParenInPattern node.Identifier +> genPat parameter
            | ps -> sepSpace +> atCurrentColumn (col sepSpace ps genPat)

        genName +> genParameters
    | Pattern.Unit n -> genUnit n
    | Pattern.Paren node ->
        genSingleTextNode node.OpeningParen
        +> genPat node.Pattern
        +> genSingleTextNode node.ClosingParen
    | Pattern.Tuple node ->
        expressionFitsOnRestOfLine
            (col sepComma node.Patterns genPat)
            (atCurrentColumn (col (sepComma +> sepNln) node.Patterns genPat))
    | Pattern.StructTuple node ->
        !- "struct "
        +> sepOpenT
        +> atCurrentColumn (colAutoNlnSkip0 sepComma node.Patterns genPat)
        +> sepCloseT
    | Pattern.ArrayOrList node ->
        let genPats =
            let short = colAutoNlnSkip0 sepSemi node.Patterns genPat
            let long = col sepNln node.Patterns genPat
            expressionFitsOnRestOfLine short long

        ifElse
            node.Patterns.IsEmpty
            (genSingleTextNode node.OpenToken +> genSingleTextNode node.CloseToken)
            (genSingleTextNode node.OpenToken
             +> addSpaceIfSpaceAroundDelimiter
             +> atCurrentColumn genPats
             +> addSpaceIfSpaceAroundDelimiter
             +> genSingleTextNode node.CloseToken)
    | Pattern.Record node ->
        let smallRecordExpr =
            genSingleTextNode node.OpeningNode
            +> addSpaceIfSpaceAroundDelimiter
            +> col sepSemi node.Fields genPatRecordFieldName
            +> addSpaceIfSpaceAroundDelimiter
            +> genSingleTextNode node.ClosingNode

        let multilineRecordExpr =
            genSingleTextNode node.OpeningNode
            +> addSpaceIfSpaceAroundDelimiter
            +> atCurrentColumn (col sepNln node.Fields genPatRecordFieldName)
            +> addSpaceIfSpaceAroundDelimiter
            +> genSingleTextNode node.ClosingNode

        let multilineRecordExprAlignBrackets =
            genSingleTextNode node.OpeningNode
            +> indent
            +> sepNln
            +> atCurrentColumn (col sepNln node.Fields genPatRecordFieldName)
            +> unindent
            +> sepNln
            +> genSingleTextNode node.ClosingNode
            |> atCurrentColumnIndent

        let multilineExpressionIfAlignBrackets =
            ifAlignBrackets multilineRecordExprAlignBrackets multilineRecordExpr

        fun ctx ->
            let size = getRecordSize ctx node.Fields
            isSmallExpression size smallRecordExpr multilineExpressionIfAlignBrackets ctx
    | Pattern.Const c -> genConstant c
    | Pattern.IsInst node -> genSingleTextNode node.Token +> sepSpace +> genType node.Type
    | Pattern.QuoteExpr node -> genQuoteExpr node
    |> genNode (Pattern.Node p)

let genPatRecordFieldName (node: PatRecordField) =
    match node.Prefix with
    | None ->
        genSingleTextNode node.FieldName
        +> sepSpace
        +> genSingleTextNode node.Equals
        +> sepSpace
        +> genPat node.Pattern
    | Some prefix ->
        genIdentListNode prefix
        +> sepDot
        +> genSingleTextNode node.FieldName
        +> sepSpace
        +> genSingleTextNode node.Equals
        +> sepSpace
        +> genPat node.Pattern

let genBinding (b: BindingNode) =
    let genParameters =
        match b.Parameters with
        | [] -> sepNone
        | ps -> sepSpace +> col sepSpace ps genPat +> sepSpace

    let genReturnType =
        match b.ReturnType with
        | None -> sepNone
        | Some (colon, t) -> genSingleTextNode colon +> sepSpace +> genType t

    genMultipleTextsNode b.LeadingKeyword
    +> sepSpace
    +> (match b.FunctionName with
        | Choice1Of2 n -> genSingleTextNode n
        | Choice2Of2 pat -> genPat pat)
    +> genParameters
    +> genReturnType
    +> sepSpace
    +> genSingleTextNode b.Equals
    +> sepSpace
    +> genExpr b.Expr
    |> genNode b

let genOpenList (openList: OpenListNode) =
    col sepNln openList.Opens (function
        | Open.ModuleOrNamespace node -> !- "open " +> genIdentListNode node.Name |> genNode node
        | Open.Target node -> !- "open type " +> genType node.Target)

let genTypeConstraint (tc: TypeConstraint) =
    match tc with
    | TypeConstraint.Single node -> genSingleTextNode node.Typar +> sepColon +> genSingleTextNode node.Kind
    | TypeConstraint.DefaultsToType node ->
        genSingleTextNode node.Default
        +> sepSpace
        +> genSingleTextNode node.Typar
        +> sepColon
        +> genType node.Type
    | TypeConstraint.SubtypeOfType node -> genSingleTextNode node.Typar +> !- " :> " +> genType node.Type
    | TypeConstraint.SupportsMember _ -> failwith "todo!"
    | TypeConstraint.EnumOrDelegate node ->
        genSingleTextNode node.Typar
        +> sepColon
        +> !- $"{node.Verb}<"
        +> col sepComma node.Types genType
        +> !- ">"
    | TypeConstraint.WhereSelfConstrained t -> genType t

let genTypeConstraints (tcs: TypeConstraint list) =
    !- "when" +> sepSpace +> col wordAnd tcs genTypeConstraint

let genType (t: Type) =
    match t with
    | Type.Funs node ->
        let short =
            col sepNone node.Parameters (fun (t, arrow) ->
                genType t
                +> sepSpace
                +> genSingleTextNode arrow
                +> sepSpace
                +> sepNlnWhenWriteBeforeNewlineNotEmpty)
            +> genType node.ReturnType

        let long =
            match node.Parameters with
            | [] -> genType node.ReturnType
            | (ht, ha) :: rest ->
                genType ht
                +> indentSepNlnUnindent (
                    genSingleTextNode ha
                    +> sepSpace
                    +> col sepNone rest (fun (t, arrow) -> genType t +> sepNln +> genSingleTextNode arrow +> sepSpace)
                    +> genType node.ReturnType
                )

        expressionFitsOnRestOfLine short long
    | Type.Tuple node -> genSynTupleTypeSegments node.Path
    | Type.HashConstraint node -> genSingleTextNode node.Hash +> genType node.Type
    | Type.MeasurePower node -> genType node.BaseMeasure +> !- "^" +> !-node.Exponent
    | Type.StaticConstant c -> genConstant c
    | Type.StaticConstantExpr node -> genSingleTextNode node.Const +> sepSpace +> genExpr node.Expr
    | Type.StaticConstantNamed node ->
        genType node.Identifier
        +> !- "="
        +> addSpaceIfSynTypeStaticConstantHasAtSignBeforeString node.Value
        +> genType node.Value
    | Type.Array node -> genType node.Type +> !- "[" +> rep (node.Rank - 1) (!- ",") +> !- "]"
    | Type.Anon node -> genSingleTextNode node
    | Type.Var node -> genSingleTextNode node
    | Type.AppPostfix node -> genType node.First +> sepSpace +> genType node.Last
    | Type.AppPrefix node ->
        let addExtraSpace =
            match node.Arguments with
            | [] -> sepNone
            | [ Type.Var node ] when node.Text.StartsWith "^" -> sepSpace
            | t :: _ -> addSpaceIfSynTypeStaticConstantHasAtSignBeforeString t

        genType node.Identifier
        +> optSingle genIdentListNodeWithDot node.PostIdentifier
        +> genSingleTextNode node.LessThen
        +> addExtraSpace
        +> col sepComma node.Arguments genType
        +> addExtraSpace
        +> genSingleTextNode node.GreaterThan
    | Type.StructTuple node ->
        genSingleTextNode node.Keyword
        +> sepSpace
        +> sepOpenT
        +> genSynTupleTypeSegments node.Path
        +> genSingleTextNode node.ClosingParen
    | Type.WithSubTypeConstraint tc -> genTypeConstraint tc
    | Type.WithGlobalConstraints node -> genType node.Type +> sepSpace +> genTypeConstraints node.TypeConstraints
    | Type.LongIdent idn -> genIdentListNode idn
    | Type.AnonRecord node -> genAnonRecordType node
    | Type.Paren node ->
        genSingleTextNode node.OpeningParen
        +> genType node.Type
        +> genSingleTextNode node.ClosingParen
    | Type.SignatureParameter node ->
        genOnelinerAttributes node.Attributes
        +> optSingle (fun id -> genSingleTextNode id +> sepColon) node.Identifier
        +> autoIndentAndNlnIfExpressionExceedsPageWidth (genType node.Type)
    | Type.Or node ->
        genType node.LeftHandSide
        +> sepSpace
        +> genSingleTextNode node.Or
        +> sepSpace
        +> genType node.RightHandSide
    |> genNode (Type.Node t)

let genSynTupleTypeSegments (path: Choice<Type, SingleTextNode> list) =
    let genTs addNewline =
        col sepSpace path (fun t ->
            match t with
            | Choice1Of2 t -> genType t
            | Choice2Of2 node -> genSingleTextNode node +> onlyIf addNewline sepNln)

    expressionFitsOnRestOfLine (genTs false) (genTs true)

let genAnonRecordType (node: TypeAnonRecordNode) =
    let genStruct =
        match node.Struct with
        | None -> sepNone
        | Some n -> genSingleTextNode n +> sepSpace

    let genOpening =
        match node.Opening with
        | None -> sepOpenAnonRecdFixed
        | Some n -> genSingleTextNode n

    let genAnonRecordFieldType (i, t) =
        genSingleTextNode i
        +> sepColon
        +> autoIndentAndNlnIfExpressionExceedsPageWidth (genType t)

    let smallExpression =
        genStruct
        +> genOpening
        +> col sepSemi node.Fields genAnonRecordFieldType
        +> sepCloseAnonRecd

    let longExpression =
        let genFields = col sepNln node.Fields genAnonRecordFieldType

        let genMultilineAnonRecordTypeAlignBrackets =
            let genRecord =
                sepOpenAnonRecdFixed
                +> indentSepNlnUnindent (atCurrentColumnIndent genFields)
                +> sepNln
                +> sepCloseAnonRecdFixed

            genStruct +> genRecord

        let genMultilineAnonRecordType =
            let genRecord = sepOpenAnonRecd +> atCurrentColumn genFields +> sepCloseAnonRecd
            genStruct +> genRecord

        ifAlignBrackets genMultilineAnonRecordTypeAlignBrackets genMultilineAnonRecordType

    fun (ctx: Context) ->
        let size = getRecordSize ctx node.Fields
        isSmallExpression size smallExpression longExpression ctx

let addSpaceIfSynTypeStaticConstantHasAtSignBeforeString (t: Type) =
    match t with
    | Type.StaticConstant sc ->
        match sc with
        | Constant.FromText node -> onlyIf (node.Text.StartsWith("@")) sepSpace
        | _ -> sepNone
    | _ -> sepNone

let sepNlnTypeAndMembers (node: ITypeDefn) (ctx: Context) : Context =
    match node.Members with
    | [] -> sepNone ctx
    | firstMember :: _ ->
        match node.TypeName.WithKeyword with
        | Some node when (not (Seq.isEmpty (node :> Node).ContentBefore)) -> enterNode node ctx
        | _ ->
            if ctx.Config.NewlineBetweenTypeDefinitionAndMembers then
                sepNlnUnlessContentBefore (MemberDefn.Node firstMember) ctx
            else
                ctx

let genTypeWithImplicitConstructor (typeName: TypeNameNode) (implicitConstructor: ImplicitConstructorNode option) =
    genSingleTextNode typeName.LeadingKeyword
    +> sepSpace
    +> genIdentListNode typeName.Identifier
    +> leadingExpressionIsMultiline
        (optSingle (fun imCtor -> sepSpaceBeforeClassConstructor +> genImplicitConstructor imCtor) implicitConstructor)
        (fun isMulti ctx ->
            if isMulti && ctx.Config.AlternativeLongMemberDefinitions then
                (optSingle genSingleTextNode typeName.EqualsToken) ctx
            else
                (sepSpace +> optSingle genSingleTextNode typeName.EqualsToken) ctx)

let genImplicitConstructor (node: ImplicitConstructorNode) =
    let genSimplePat (node: SimplePatNode) =
        genOnelinerAttributes node.Attributes
        +> onlyIf node.IsOptional (!- "?")
        +> genSingleTextNode node.Identifier
        +> optSingle (fun t -> sepColon +> autoIndentAndNlnIfExpressionExceedsPageWidth (genType t)) node.Type

    let shortPats = col sepComma node.Parameters genSimplePat

    let longPats =
        indentSepNlnUnindent (col (sepComma +> sepNln) node.Parameters genSimplePat)
        +> sepNln

    let short =
        optSingle (fun xml -> genSingleTextNode xml +> sepNln) node.XmlDoc
        +> genOnelinerAttributes node.Attributes
        +> genAccessOpt node.Accessibility
        +> genSingleTextNode node.OpeningParen
        +> shortPats
        +> genSingleTextNode node.ClosingParen

    let long =
        let genPats =
            genSingleTextNode node.OpeningParen
            +> expressionFitsOnRestOfLine shortPats longPats
            +> genSingleTextNode node.ClosingParen

        indentSepNlnUnindent (
            optSingle (fun xml -> genSingleTextNode xml +> sepNln) node.XmlDoc
            +> onlyIfNot node.Attributes.IsEmpty (genOnelinerAttributes node.Attributes +> sepNln)
            +> expressionFitsOnRestOfLine
                (genAccessOpt node.Accessibility +> genPats)
                (genAccessOpt node.Accessibility
                 +> optSingle (fun _ -> sepNln) node.Accessibility
                 +> genPats)
            +> (fun ctx -> onlyIf ctx.Config.AlternativeLongMemberDefinitions sepNln ctx)
        )

    expressionFitsOnRestOfLine short long
    +> optSingle (fun self -> sepSpace +> !- "as " +> genSingleTextNode self) node.Self

let genTypeDefn (td: TypeDefn) =
    let typeDefnNode = TypeDefn.TypeDefnNode td
    let typeName = typeDefnNode.TypeName
    let members = typeDefnNode.Members

    let header =
        genSingleTextNode typeName.LeadingKeyword
        +> sepSpace
        +> genIdentListNode typeName.Identifier
        +> sepSpace
        +> optSingle genSingleTextNode typeName.EqualsToken

    match td with
    | TypeDefn.Enum node ->
        let genEnumCase (node: EnumCaseNode) =
            optSingle genSingleTextNode node.XmlDoc
            +> (match node.Bar with
                | None -> sepBar
                | Some bar -> genSingleTextNode bar +> sepSpace)
            +> genOnelinerAttributes node.Attributes
            +> genSingleTextNode node.Identifier
            +> sepSpace
            +> genSingleTextNode node.Equals
            +> autoIndentAndNlnWhenWriteBeforeNewlineNotEmpty (sepSpace +> genConstant node.Constant)

        header
        +> indentSepNlnUnindent (
            col sepNln node.EnumCases genEnumCase
            +> onlyIf (List.isNotEmpty members) sepNln
            +> sepNlnTypeAndMembers typeDefnNode
            +> genMemberDefnList members
        )
    | TypeDefn.Union node ->
        let unionCases (ctx: Context) =
            match node.UnionCases with
            | [] -> ctx
            | [ singleCase ] when List.isEmpty members ->
                let hasVerticalBar =
                    ctx.Config.BarBeforeDiscriminatedUnionDeclaration
                    || List.isNotEmpty singleCase.Attributes.AttributeLists
                    || List.isEmpty singleCase.Fields

                let genCase hasVerticalBar =
                    opt sepSpace node.Accessibility (fun vis ->
                        genSingleTextNode vis +> onlyIfNot singleCase.XmlDoc.IsNone sepNln)
                    +> genUnionCase hasVerticalBar singleCase

                expressionFitsOnRestOfLine
                    (sepSpace +> genCase hasVerticalBar)
                    (indentSepNlnUnindent (genCase true))
                    ctx
            | xs ->
                indentSepNlnUnindent
                    (opt sepNln node.Accessibility genSingleTextNode
                     +> col sepNln xs (genUnionCase true))
                    ctx

        header
        +> unionCases
        +> onlyIf (List.isNotEmpty members) sepNln
        +> sepNlnTypeAndMembers typeDefnNode
        +> genMemberDefnList members
        +> unindent

    | TypeDefn.Record node ->
        let smallExpression =
            sepSpace
            +> genAccessOpt node.Accessibility
            +> sepSpace
            +> genSingleTextNode node.OpeningBrace
            +> addSpaceIfSpaceAroundDelimiter
            +> col sepSemi node.Fields genField
            +> addSpaceIfSpaceAroundDelimiter
            +> genSingleTextNode node.ClosingBrace

        let multilineExpression (ctx: Context) =
            if
                ctx.Config.MultilineBlockBracketsOnSameColumn
                || (List.exists (fun (fieldNode: FieldNode) -> fieldNode.XmlDoc.IsSome) node.Fields)
            then
                (ifElseCtx
                    (fun ctx -> ctx.Config.ExperimentalStroustrupStyle && List.isEmpty members)
                    (genAccessOpt node.Accessibility)
                    (opt (indent +> sepNln) node.Accessibility genSingleTextNode)
                 +> genSingleTextNode node.OpeningBrace
                 +> indentSepNlnUnindent (atCurrentColumn (col sepNln node.Fields genField))
                 +> sepNln
                 +> genSingleTextNode node.ClosingBrace
                 +> optSingle (fun _ -> unindent) node.Accessibility
                 +> onlyIf (List.isNotEmpty members) sepNln
                 +> sepNlnTypeAndMembers typeDefnNode
                 +> genMemberDefnList members)
                    ctx
            else
                (sepNlnUnlessLastEventIsNewline
                 +> opt (indent +> sepNln) node.Accessibility genSingleTextNode
                 +> genSingleTextNode node.OpeningBrace
                 +> addSpaceIfSpaceAroundDelimiter
                 +> atCurrentColumn (sepNlnWhenWriteBeforeNewlineNotEmpty +> col sepNln node.Fields genField)
                 +> addSpaceIfSpaceAroundDelimiter
                 +> genSingleTextNode node.ClosingBrace
                 +> optSingle (fun _ -> unindent) node.Accessibility
                 +> onlyIf (List.isNotEmpty members) sepNln
                 +> sepNlnTypeAndMembers typeDefnNode
                 +> genMemberDefnList members)
                    ctx

        let bodyExpr size ctx =
            if (List.isEmpty members) then
                (isSmallExpression size smallExpression multilineExpression) ctx
            else
                multilineExpression ctx

        let genTypeDefinition (ctx: Context) =
            let size = getRecordSize ctx node.Fields
            let short = bodyExpr size

            if ctx.Config.ExperimentalStroustrupStyle && members.IsEmpty then
                (sepSpace +> short) ctx
            else
                isSmallExpression size short (indentSepNlnUnindent short) ctx

        header +> genTypeDefinition

    | TypeDefn.None _ -> header
    | TypeDefn.Abbrev node -> header +> sepSpace +> genType node.Type
    | TypeDefn.Explicit node ->
        let bodyNode = node.Body

        genTypeWithImplicitConstructor typeName node.ImplicitConstructor
        +> indentSepNlnUnindent (
            genSingleTextNode bodyNode.Kind
            +> onlyIfNot bodyNode.Members.IsEmpty (indentSepNlnUnindent (genMemberDefnList bodyNode.Members))
            +> sepNln
            +> genSingleTextNode bodyNode.End
            |> genNode bodyNode
        )
        +> onlyIfNot members.IsEmpty (sepNln +> indentSepNlnUnindent (genMemberDefnList members))
    | TypeDefn.Augmentation _ ->
        header
        +> sepSpace
        +> optSingle genSingleTextNode typeName.WithKeyword
        +> indentSepNlnUnindent (sepNlnTypeAndMembers typeDefnNode +> genMemberDefnList members)
    | TypeDefn.Delegate node ->
        header
        +> sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidth (
            genSingleTextNode node.DelegateNode
            +> sepSpace
            +> !- "of"
            +> sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidth (genTypeList node.TypeList)
        )

    | TypeDefn.Regular node ->
        genTypeWithImplicitConstructor typeName node.ImplicitConstructor
        +> indentSepNlnUnindent (genMemberDefnList members)

    |> genNode (TypeDefn.Node td)

let genTypeList (node: TypeListNode) =
    let shortExpr =
        col sepSpace node.Parameters (fun (t, arrow) -> genType t +> sepSpace +> genSingleTextNode arrow)
        +> sepSpace
        +> genType node.ReturnType

    let longExpr =
        let rec visit parameters level (continuation: Context -> Context) =
            match parameters with
            | [] -> continuation
            | [ lastType, lastArrow ] ->
                continuation
                +> genType lastType
                +> sepSpace
                +> genSingleTextNode lastArrow
                +> indent
                +> sepNln
                +> genType node.ReturnType
                +> rep (level + 1) unindent
            | (t, arrow) :: tail ->
                let isTuple =
                    match t with
                    | Type.Tuple _ -> true
                    | _ -> false

                visit
                    tail
                    (level + if isTuple then 1 else 0)
                    (continuation
                     +> genType t
                     +> sepSpace
                     +> genSingleTextNode arrow
                     +> onlyIf isTuple indent
                     +> sepNln)

        visit node.Parameters 0 sepNone

    expressionFitsOnRestOfLine shortExpr longExpr |> genNode node

let genField (node: FieldNode) =
    optSingle genSingleTextNode node.XmlDoc
    +> genAttributes node.Attributes
    +> optSingle (fun lk -> genMultipleTextsNode lk +> sepSpace) node.LeadingKeyword
    +> onlyIf node.IsMutable (!- "mutable ")
    +> genAccessOpt node.Accessibility
    +> opt sepColon node.Name genSingleTextNode
    +> autoIndentAndNlnIfExpressionExceedsPageWidth (genType node.Type)
    |> genNode node

let genUnionCase (hasVerticalBar: bool) (node: UnionCaseNode) =
    let shortExpr = col sepStar node.Fields genField

    let longExpr =
        indentSepNlnUnindent (atCurrentColumn (col (sepStar +> sepNln) node.Fields genField))

    let genBar =
        match node.Bar with
        | Some bar -> ifElse hasVerticalBar (genSingleTextNode bar +> sepSpace) (genNode bar sepNone)
        | None -> onlyIf hasVerticalBar sepBar

    optSingle genSingleTextNode node.XmlDoc
    +> genBar
    +> atCurrentColumn (
        // If the bar has a comment after, add a newline and print the identifier on the same column on the next line.
        sepNlnWhenWriteBeforeNewlineNotEmpty
        +> genOnelinerAttributes node.Attributes
        +> genSingleTextNode node.Identifier
        +> onlyIf (List.isNotEmpty node.Fields) wordOf
    )
    +> onlyIf (List.isNotEmpty node.Fields) (expressionFitsOnRestOfLine shortExpr longExpr)
    |> genNode node

let genMemberDefnList mds =
    match mds with
    | [] -> sepNone
    | _ -> colWithNlnWhenMappedNodeIsMultiline MemberDefn.Node genMemberDefn mds

let genMemberDefn (md: MemberDefn) =
    match md with
    | MemberDefn.ImplicitInherit ic -> genInheritConstructor ic
    | MemberDefn.Inherit node -> genSingleTextNode node.Inherit +> sepSpace +> genType node.BaseType
    | MemberDefn.ValField node -> genField node
    | MemberDefn.Member node -> genBinding node
    | MemberDefn.ExternBinding _ -> failwithf "todo %A" md
    | MemberDefn.DoExpr node -> genExpr (Expr.Single node)
    | MemberDefn.LetBinding _ -> failwithf "todo %A" md
    | MemberDefn.ExplicitCtor _ -> failwithf "todo %A" md
    | MemberDefn.Interface _ -> failwithf "todo %A" md
    | MemberDefn.AutoProperty _ -> failwithf "todo %A" md
    | MemberDefn.AbstractSlot _ -> failwithf "todo %A" md
    | MemberDefn.PropertyGetSet _ -> failwithf "todo %A" md
    |> genNode (MemberDefn.Node md)

let genExceptionBody px ats ao uc =
    optSingle genSingleTextNode px
    +> genAttributes ats
    +> !- "exception "
    +> genAccessOpt ao
    +> genUnionCase false uc

let genException (node: ExceptionDefnNode) =
    genExceptionBody node.XmlDoc node.Attributes node.Accessibility node.UnionCase
    +> onlyIf
        (not node.Members.IsEmpty)
        (sepSpace
         +> optSingle genSingleTextNode node.WithKeyword
         +> indentSepNlnUnindent (genMemberDefnList node.Members))

let genModuleDecl (md: ModuleDecl) =
    match md with
    | ModuleDecl.OpenList ol -> genOpenList ol
    | ModuleDecl.HashDirectiveList node -> col sepNln node.HashDirectives genParsedHashDirective
    | ModuleDecl.Attributes node -> genAttributes node.Attributes +> genExpr node.Expr
    | ModuleDecl.DeclExpr e -> genExpr e
    | ModuleDecl.Exception node -> genException node
    | ModuleDecl.ExternBinding _ -> failwith "Not Implemented"
    | ModuleDecl.TopLevelBinding b -> genBinding b
    | ModuleDecl.ModuleAbbrev node ->
        genSingleTextNode node.Module
        +> sepSpace
        +> genSingleTextNode node.Name
        +> sepEqFixed
        +> sepSpace
        +> genIdentListNode node.Alias
    | ModuleDecl.NestedModule node -> optSingle genSingleTextNode node.XmlDoc +> genAttributes node.Attributes
    | ModuleDecl.TypeDefn td -> genTypeDefn td

let sepNlnUnlessContentBefore (node: Node) =
    if Seq.isEmpty node.ContentBefore then sepNln else sepNone

let colWithNlnWhenMappedNodeIsMultiline<'n>
    (mapNode: 'n -> Node)
    (f: 'n -> Context -> Context)
    (nodes: 'n list)
    : Context -> Context =
    nodes
    |> List.map (fun n -> ColMultilineItem(f n, (mapNode >> sepNlnUnlessContentBefore) n))
    |> colWithNlnWhenItemIsMultiline

let colWithNlnWhenNodeIsMultiline<'n when 'n :> Node> (f: 'n -> Context -> Context) (nodes: 'n list) =
    colWithNlnWhenMappedNodeIsMultiline<'n> (fun n -> n :> Node) f nodes

let genModule (m: ModuleOrNamespaceNode) =
    onlyIf
        m.IsNamed
        (optSingle
            (fun (n: SingleTextNode) -> genSingleTextNode n +> sepSpace +> genIdentListNode m.Name)
            m.LeadingKeyword
         +> onlyIf (not m.Declarations.IsEmpty) (sepNln +> sepNln))
    +> colWithNlnWhenMappedNodeIsMultiline ModuleDecl.Node genModuleDecl m.Declarations
    |> genNode m

let genFile (oak: Oak) =
    col sepNln oak.ParsedHashDirectives genParsedHashDirective
    +> (if oak.ParsedHashDirectives.IsEmpty then sepNone else sepNln)
    +> col sepNln oak.ModulesOrNamespaces genModule
    +> (fun ctx -> onlyIf ctx.Config.InsertFinalNewline sepNln ctx)
    |> genNode oak
