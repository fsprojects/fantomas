module internal rec Fantomas.Core.CodePrinter2

open System
open Fantomas.Core.Context
open Fantomas.Core.SyntaxOak

let noBreakInfixOps = set [| "="; ">"; "<"; "%" |]
let newLineInfixOps = set [ "|>"; "||>"; "|||>"; ">>"; ">>=" ]

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
        | Some(IdentifierOrDot.Ident ident) -> upperOrLower ident.Text
        | _ -> LowercaseType
    | Type.Var node -> upperOrLower (node.Text.Substring(1))
    | Type.AppPostfix node -> (|UppercaseType|LowercaseType|) node.First
    | Type.AppPrefix node -> (|UppercaseType|LowercaseType|) node.Identifier
    | _ -> failwithf $"Cannot determine if synType %A{t} is uppercase or lowercase"

let rec (|UppercaseExpr|LowercaseExpr|) (expr: Expr) =
    let upperOrLower (v: string) =
        let isUpper = Seq.tryHead v |> Option.map Char.IsUpper |> Option.defaultValue false
        if isUpper then UppercaseExpr else LowercaseExpr

    let lastFragmentInList (identList: IdentListNode) =
        match List.tryLast identList.Content with
        | None
        | Some(IdentifierOrDot.KnownDot _ | IdentifierOrDot.UnknownDot) -> LowercaseExpr
        | Some(IdentifierOrDot.Ident node) -> upperOrLower node.Text

    match expr with
    | Expr.Ident ident -> upperOrLower ident.Text
    | Expr.OptVar node -> lastFragmentInList node.Identifier
    | Expr.DotGet node -> lastFragmentInList node.Identifier
    | Expr.DotIndexedGet node -> (|UppercaseExpr|LowercaseExpr|) node.ObjectExpr
    | Expr.TypeApp node -> (|UppercaseExpr|LowercaseExpr|) node.Identifier
    | Expr.Dynamic node -> (|UppercaseExpr|LowercaseExpr|) node.FuncExpr
    | Expr.DotGetAppParen node -> (|UppercaseExpr|LowercaseExpr|) node.Function
    | _ -> failwithf "cannot determine if Expr %A is uppercase or lowercase" expr

let (|ParenExpr|_|) (e: Expr) =
    match e with
    | Expr.Paren _
    | Expr.ParenLambda _
    | Expr.ParenFunctionNameWithStar _
    | Expr.Constant(Constant.Unit _) -> Some e
    | _ -> None

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
        | BlockComment(comment, before, after) ->
            ifElse (before && addNewline) sepNlnForTrivia sepNone
            +> sepSpace
            +> !-comment
            +> sepSpace
            +> ifElse after sepNlnForTrivia sepNone
        | CommentOnSingleLine s
        | Directive s -> (ifElse addNewline sepNlnForTrivia sepNone) +> !-s +> sepNlnForTrivia
        | Newline -> (ifElse addNewline (sepNlnForTrivia +> sepNlnForTrivia) sepNlnForTrivia)

    gen ctx

let enterNode<'n when 'n :> Node> (n: 'n) = col sepNone n.ContentBefore genTrivia
let leaveNode<'n when 'n :> Node> (n: 'n) = col sepNone n.ContentAfter genTrivia
let genNode<'n when 'n :> Node> (n: 'n) (f: Context -> Context) = enterNode n +> f +> leaveNode n

let genSingleTextNode (node: SingleTextNode) = !-node.Text |> genNode node

// Alternative for genSingleTextNode to avoid a double space when the node has line comment after it.
let genSingleTextNodeWithSpaceSuffix (addSpace: Context -> Context) (node: SingleTextNode) =
    (!-node.Text +> addSpace) |> genNode node

let genSingleTextNodeSuffixDelimiter (node: SingleTextNode) =
    genSingleTextNodeWithSpaceSuffix addSpaceIfSpaceAroundDelimiter node

let genSingleTextNodeWithLeadingDot (node: SingleTextNode) = !- $".{node.Text}" |> genNode node

let genMultipleTextsNode (node: MultipleTextsNode) =
    col sepSpace node.Content genSingleTextNode |> genNode node

let genIdentListNodeAux addLeadingDot (iln: IdentListNode) =
    coli sepNone iln.Content (fun idx identOrDot ->
        match identOrDot with
        | IdentifierOrDot.Ident ident ->
            if addLeadingDot && idx = 0 then
                genSingleTextNodeWithLeadingDot ident
            else
                genSingleTextNode ident
        | IdentifierOrDot.KnownDot _
        | IdentifierOrDot.UnknownDot _ -> sepDot)
    |> genNode iln

let genIdentListNode iln = genIdentListNodeAux false iln
let genIdentListNodeWithDot iln = genIdentListNodeAux true iln

let genIdentListNodeWithDotMultiline (iln: IdentListNode) =
    coli sepNone iln.Content (fun idx iod ->
        match iod with
        | IdentifierOrDot.Ident ident ->
            if idx = 0 then
                genSingleTextNodeWithLeadingDot ident
            else
                genSingleTextNode ident
        | IdentifierOrDot.KnownDot _
        | IdentifierOrDot.UnknownDot _ -> sepNln +> sepDot)
    |> genNode iln

let genAccessOpt (nodeOpt: SingleTextNode option) =
    match nodeOpt with
    | None -> sepNone
    | Some node -> genSingleTextNode node +> sepSpace

let genXml (node: XmlDocNode option) =
    match node with
    | None -> sepNone
    | Some node -> col sepNln node.Lines (!-) +> sepNln |> genNode node

let addSpaceBeforeParenInPattern (node: IdentListNode) (ctx: Context) =
    node.Content
    |> List.tryFindBack (function
        | IdentifierOrDot.Ident node -> not (String.IsNullOrWhiteSpace node.Text)
        | _ -> false)
    |> fun identOrDot ->
        match identOrDot with
        | Some(IdentifierOrDot.Ident node) ->
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
    |> genNode n

// genNode will should be called in the caller function.
let genConstant (c: Constant) =
    match c with
    | Constant.FromText n -> genSingleTextNode n
    | Constant.Unit n -> genUnit n
    | Constant.Measure n ->
        (genConstant n.Constant |> genNode (Constant.Node n.Constant))
        +> !- "<"
        +> genMeasure n.Measure
        +> !- ">"
        |> genNode n

let genMeasure (measure: Measure) =
    match measure with
    | Measure.Single n -> genSingleTextNode n
    | Measure.Operator n ->
        genMeasure n.LeftHandSide
        +> sepSpace
        +> genSingleTextNode n.Operator
        +> sepSpace
        +> genMeasure n.RightHandSide
        |> genNode n
    | Measure.Power n -> genMeasure n.Measure +> !- "^" +> genSingleTextNode n.Exponent |> genNode n
    | Measure.Seq n -> col sepSpace n.Measures genMeasure
    | Measure.Multiple n -> genIdentListNode n
    | Measure.Paren n ->
        genSingleTextNode n.OpeningParen
        +> genMeasure n.Measure
        +> genSingleTextNode n.ClosingParen
        |> genNode n

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
        +> sepNlnWhenWriteBeforeNewlineNotEmpty
        |> genNode a)
    |> genNode node

// The inherit keyword should already be printed by the caller
let genInheritConstructor (ic: InheritConstructor) =
    match ic with
    | InheritConstructor.TypeOnly node -> genType node.Type
    | InheritConstructor.Unit node ->
        genType node.Type
        +> sepSpaceBeforeClassConstructor
        +> genSingleTextNode node.OpeningParen
        +> genSingleTextNode node.ClosingParen
    | InheritConstructor.Paren node ->
        genType node.Type
        +> sepSpaceBeforeClassConstructor
        +> expressionFitsOnRestOfLine (genExpr node.Expr) (genMultilineFunctionApplicationArguments node.Expr)
    | InheritConstructor.Other node ->
        genType node.Type
        +> sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidth (genExpr node.Expr)

let isSynExprLambdaOrIfThenElse (e: Expr) =
    match e with
    | Expr.Lambda _
    | Expr.IfThen _
    | Expr.IfThenElif _
    | Expr.IfThenElse _ -> true
    | _ -> false

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
        |> genNode node
    | Expr.Single node ->
        genSingleTextNode node.Leading
        +> onlyIf node.AddSpace sepSpaceUnlessWriteBeforeNewlineNotEmpty
        +> ifElse
            node.SupportsStroustrup
            (autoIndentAndNlnIfExpressionExceedsPageWidthUnlessStroustrup genExpr node.Expr)
            (autoIndentAndNlnIfExpressionExceedsPageWidth (genExpr node.Expr))
        |> genNode node
    | Expr.Constant node -> genConstant node
    | Expr.Null node -> genSingleTextNode node
    | Expr.Quote node -> genQuoteExpr node
    | Expr.Typed node ->
        let short =
            let genOperator ctx =
                if node.Operator.Length > 1 || ctx.Config.SpaceBeforeColon then
                    (sepSpace +> !-node.Operator) ctx
                else
                    !- node.Operator ctx

            genExpr node.Expr +> genOperator +> sepSpace +> genType node.Type

        let long =
            genExpr node.Expr +> sepNln +> !-node.Operator +> sepSpace +> genType node.Type

        match node.Expr with
        | Expr.Lambda _ -> long
        | _ -> expressionFitsOnRestOfLine short long
        |> genNode node
    | Expr.New node ->
        match node.Arguments with
        | ParenExpr _ ->
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
        | _ ->
            genSingleTextNode node.NewKeyword
            +> sepSpace
            +> genType node.Type
            +> sepSpace
            +> genExpr node.Arguments
        |> genNode node
    | Expr.Tuple node -> genTuple node
    | Expr.StructTuple node ->
        genSingleTextNode node.Struct
        +> sepSpace
        +> sepOpenT
        +> genTuple node.Tuple
        +> genSingleTextNode node.ClosingParen
        |> genNode node
    | Expr.ArrayOrList node ->
        if node.Elements.IsEmpty then
            genSingleTextNode node.Opening +> genSingleTextNode node.Closing |> genNode node
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
                                (onlyIfNot isFixed sepSpace +> !-node.Closing.Text +> leaveNode node.Closing) ctx))
                    )

                ifAlignBrackets genMultiLineArrayOrListAlignBrackets genMultiLineArrayOrList

            fun ctx ->
                let alwaysMultiline =
                    let isIfThenElseWithYieldReturn e =
                        let (|YieldLikeExpr|_|) e =
                            match e with
                            | Expr.Single singleNode ->
                                if singleNode.Leading.Text.StartsWith("yield") then
                                    Some e
                                else
                                    None
                            | _ -> None

                        match e with
                        | Expr.IfThen ifThenNode ->
                            match ifThenNode.ThenExpr with
                            | YieldLikeExpr _ -> true
                            | _ -> false
                        | Expr.IfThenElse ifThenElseNode ->
                            match ifThenElseNode.IfExpr, ifThenElseNode.ElseExpr with
                            | YieldLikeExpr _, _
                            | _, YieldLikeExpr _ -> true
                            | _ -> false
                        | _ -> false

                    List.exists isIfThenElseWithYieldReturn node.Elements
                    || List.forall isSynExprLambdaOrIfThenElse node.Elements

                if alwaysMultiline then
                    multilineExpression ctx
                else
                    let size = getListOrArrayExprSize ctx ctx.Config.MaxArrayOrListWidth node.Elements
                    isSmallExpression size smallExpression multilineExpression ctx
            |> genNode node
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
               | RecordNodeExtra.Inherit ie ->
                   (genSingleTextNode ie.InheritKeyword +> sepSpace +> genInheritConstructor ie
                    |> genNode (InheritConstructor.Node ie))
                   +> onlyIf hasFields sepSemi
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
                        (genSingleTextNode ie.InheritKeyword
                         +> sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidth (genInheritConstructor ie)
                         |> genNode (InheritConstructor.Node ie))
                        +> onlyIf hasFields sepNln
                        +> fieldsExpr
                    )
                    +> sepNln
                    +> genSingleTextNode node.ClosingBrace
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
                        (genSingleTextNode ie.InheritKeyword
                         +> sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidth (genInheritConstructor ie)
                         |> genNode (InheritConstructor.Node ie))
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
                            (genSingleTextNodeSuffixDelimiter node.OpeningBrace
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
            genNode node (isSmallExpression size smallRecordExpr multilineRecordExpr) ctx
    | Expr.AnonRecord node ->
        let genAnonRecordFieldName (node: AnonRecordFieldNode) =
            genSingleTextNode node.Ident
            +> sepSpace
            +> genSingleTextNode node.Equals
            +> sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidth (genExpr node.Expr)
            |> genNode node

        let smallExpression =
            onlyIf node.IsStruct !- "struct "
            +> genSingleTextNode node.OpeningBrace
            +> addSpaceIfSpaceAroundDelimiter
            +> optSingle (fun e -> genExpr e +> !- " with ") node.CopyInfo
            +> col sepSemi node.Fields genAnonRecordFieldName
            +> addSpaceIfSpaceAroundDelimiter
            +> genSingleTextNode node.ClosingBrace

        let longExpression =
            let fieldsExpr = col sepNln node.Fields genAnonRecordFieldName

            let genMultilineAnonRecord =
                let recordExpr =
                    match node.CopyInfo with
                    | Some e ->
                        genSingleTextNodeSuffixDelimiter node.OpeningBrace
                        +> sepNlnWhenWriteBeforeNewlineNotEmpty // comment after curly brace
                        +> atCurrentColumn (genExpr e +> (!- " with" +> indentSepNlnUnindent fieldsExpr))
                        +> addSpaceIfSpaceAroundDelimiter
                        +> genSingleTextNode node.ClosingBrace
                    | None ->
                        fun ctx ->
                            // position after `{| ` or `{|`
                            let targetColumn = ctx.Column + (if ctx.Config.SpaceAroundDelimiter then 3 else 2)

                            atCurrentColumn
                                (genSingleTextNodeSuffixDelimiter node.OpeningBrace
                                 +> sepNlnWhenWriteBeforeNewlineNotEmpty // comment after curly brace
                                 +> col sepNln node.Fields (fun fieldNode ->
                                     let expr =
                                         if ctx.Config.IndentSize < 3 then
                                             sepSpaceOrDoubleIndentAndNlnIfExpressionExceedsPageWidth (
                                                 genExpr fieldNode.Expr
                                             )
                                         else
                                             sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidth (genExpr fieldNode.Expr)

                                     // Add enough spaces to start at the right column but indent from the opening curly brace.
                                     // Use a double indent when using a small indent size to avoid offset warnings.
                                     addFixedSpaces targetColumn
                                     +> atCurrentColumn (enterNode fieldNode +> genSingleTextNode fieldNode.Ident)
                                     +> sepSpace
                                     +> genSingleTextNode fieldNode.Equals
                                     +> expr
                                     +> leaveNode fieldNode)
                                 +> addSpaceIfSpaceAroundDelimiter
                                 +> genSingleTextNode node.ClosingBrace)
                                ctx

                onlyIf node.IsStruct !- "struct " +> recordExpr

            let genMultilineAnonRecordAlignBrackets =
                let copyExpr fieldsExpr e =
                    atCurrentColumnIndent (genExpr e)
                    +> (!- " with"
                        +> indent
                        +> whenShortIndent indent
                        +> sepNln
                        +> fieldsExpr
                        +> whenShortIndent unindent
                        +> unindent)

                let genAnonRecord =
                    match node.CopyInfo with
                    | Some ci ->
                        genSingleTextNodeSuffixDelimiter node.OpeningBrace
                        +> sepNlnWhenWriteBeforeNewlineNotEmpty // comment after curly brace
                        +> copyExpr fieldsExpr ci
                        +> sepNln
                        +> genSingleTextNode node.ClosingBrace
                    | None ->
                        genSingleTextNode node.OpeningBrace
                        +> indentSepNlnUnindent fieldsExpr
                        +> sepNln
                        +> genSingleTextNode node.ClosingBrace

                ifElse node.IsStruct !- "struct " sepNone +> genAnonRecord

            ifAlignBrackets genMultilineAnonRecordAlignBrackets genMultilineAnonRecord

        fun (ctx: Context) ->
            let size = getRecordSize ctx node.Fields
            genNode node (isSmallExpression size smallExpression longExpression) ctx
    | Expr.ObjExpr node ->
        let param = optSingle genExpr node.Expr

        if node.Bindings.IsEmpty && node.Members.IsEmpty && node.Interfaces.IsEmpty then
            // See https://devblogs.microsoft.com/dotnet/announcing-f-5/#default-interface-member-consumption
            genSingleTextNode node.OpeningBrace
            +> addSpaceIfSpaceAroundDelimiter
            +> genSingleTextNode node.New
            +> sepSpace
            +> genType node.Type
            +> param
            +> addSpaceIfSpaceAroundDelimiter
            +> genSingleTextNode node.ClosingBrace
        else
            let genInterfaceImpl (node: InterfaceImplNode) =
                if node.Bindings.IsEmpty && node.Members.IsEmpty then
                    genSingleTextNode node.Interface +> sepSpace +> genType node.Type
                else
                    genSingleTextNode node.Interface
                    +> sepSpace
                    +> genType node.Type
                    +> sepSpace
                    +> optSingle genSingleTextNode node.With
                    +> indentSepNlnUnindent (genBindings false node.Bindings +> genMemberDefnList node.Members)

            let genBody =
                indentSepNlnUnindent (genBindings false node.Bindings +> genMemberDefnList node.Members)
                +> colPre sepNln sepNln node.Interfaces genInterfaceImpl

            let genObjExpr =
                genSingleTextNode node.OpeningBrace
                +> addSpaceIfSpaceAroundDelimiter
                +> atCurrentColumn (
                    genSingleTextNode node.New
                    +> sepSpace
                    +> genType node.Type
                    +> param
                    +> sepSpace
                    +> optSingle genSingleTextNode node.With
                    +> genBody
                )
                +> addSpaceIfSpaceAroundDelimiter
                +> genSingleTextNode node.ClosingBrace

            let genObjExprAlignBrackets =
                let genObjExpr =
                    atCurrentColumn (
                        genSingleTextNode node.New
                        +> sepSpace
                        +> genType node.Type
                        +> param
                        +> sepSpace
                        +> optSingle genSingleTextNode node.With
                        +> genBody
                    )

                atCurrentColumnIndent (
                    genSingleTextNode node.OpeningBrace
                    +> addSpaceIfSpaceAroundDelimiter
                    +> genObjExpr
                    +> sepNln
                    +> genSingleTextNode node.ClosingBrace
                )

            ifAlignBrackets genObjExprAlignBrackets genObjExpr
        |> genNode node
    | Expr.While node ->
        atCurrentColumn (
            genSingleTextNode node.While
            +> sepSpace
            +> genExpr node.WhileExpr
            +> !- " do"
            +> indentSepNlnUnindent (genExpr node.DoExpr)
        )
        |> genNode node
    | Expr.For node ->
        atCurrentColumn (
            genSingleTextNode node.For
            +> sepSpace
            +> genSingleTextNode node.Ident
            +> sepSpace
            +> genSingleTextNode node.Equals
            +> sepSpace
            +> genExpr node.IdentBody
            +> ifElse node.Direction (!- " to ") (!- " downto ")
            +> genExpr node.ToBody
            +> !- " do"
            +> indent
            +> sepNln
            +> genExpr node.DoBody
            +> unindent
        )
        |> genNode node
    | Expr.ForEach node ->
        atCurrentColumn (
            genSingleTextNode node.For
            +> sepSpace
            +> genPat node.Pattern
            +> !- " in "
            +> autoIndentAndNlnIfExpressionExceedsPageWidth (genExpr node.EnumExpr)
            +> ifElse
                node.IsArrow
                (sepArrow +> autoIndentAndNlnIfExpressionExceedsPageWidth (genExpr node.BodyExpr))
                (!- " do" +> indent +> sepNln +> genExpr node.BodyExpr +> unindent)
        )
        |> genNode node
    | Expr.NamedComputation node ->
        let short =
            genExpr node.Name
            +> sepSpace
            +> genSingleTextNode node.OpeningBrace
            +> addSpaceIfSpaceAroundDelimiter
            +> genExpr node.Body
            +> addSpaceIfSpaceAroundDelimiter
            +> genSingleTextNode node.ClosingBrace

        let long =
            genExpr node.Name
            +> sepSpace
            +> genSingleTextNode node.OpeningBrace
            +> indentSepNlnUnindent (genExpr node.Body)
            +> sepNln
            +> genSingleTextNode node.ClosingBrace

        expressionFitsOnRestOfLine short long |> genNode node
    | Expr.Computation node ->
        genSingleTextNode node.OpeningBrace
        +> addSpaceIfSpaceAroundDelimiter
        +> genExpr node.Body
        +> addSpaceIfSpaceAroundDelimiter
        +> genSingleTextNode node.ClosingBrace
        |> genNode node
    | Expr.CompExprBody node ->
        node.Statements
        |> List.map (function
            | ComputationExpressionStatement.LetOrUseStatement node ->
                let expr =
                    genBinding node.Binding
                    +> optSingle (fun inNode -> sepSpace +> genSingleTextNode inNode +> sepSpace) node.In
                    |> genNode node

                ColMultilineItem(expr, sepNlnUnlessContentBefore node)
            | ComputationExpressionStatement.LetOrUseBangStatement node ->
                let expr =
                    genSingleTextNode node.LeadingKeyword
                    +> sepSpace
                    +> genPat node.Pattern
                    +> sepSpace
                    +> genSingleTextNode node.Equals
                    +> sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidthUnlessStroustrup genExpr node.Expression
                    |> genNode node

                ColMultilineItem(expr, sepNlnUnlessContentBefore node)
            | ComputationExpressionStatement.AndBangStatement node ->
                let expr =
                    genSingleTextNode node.LeadingKeyword
                    +> sepSpace
                    +> genPat node.Pattern
                    +> sepSpace
                    +> genSingleTextNode node.Equals
                    +> sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidthUnlessStroustrup genExpr node.Expression
                    |> genNode node

                ColMultilineItem(expr, sepNlnUnlessContentBefore node)
            | ComputationExpressionStatement.OtherStatement e ->
                ColMultilineItem(genExpr e, sepNlnUnlessContentBefore (Expr.Node e)))
        |> colWithNlnWhenItemIsMultilineUsingConfig
        |> genNode node
    | Expr.JoinIn node ->
        genExpr node.LeftHandSide
        +> sepSpace
        +> genSingleTextNode node.In
        +> sepSpace
        +> genExpr node.RightHandSide
        |> genNode node
    | Expr.ParenLambda node ->
        genSingleTextNode node.OpeningParen
        +> leadingExpressionIsMultiline
            ((genLambda node.Lambda |> genNode node.Lambda)
             +> sepNlnWhenWriteBeforeNewlineNotEmpty)
            (fun isMultiline ctx -> onlyIf (isMultiline && ctx.Config.MultiLineLambdaClosingNewline) sepNln ctx)
        +> genSingleTextNode node.ClosingParen
        |> genNode node
    | Expr.Lambda node -> genLambda node
    | Expr.MatchLambda node ->
        genSingleTextNode node.Function +> sepNln +> genClauses node.Clauses
        |> genNode node
    | Expr.Match node ->
        atCurrentColumn (
            genControlExpressionStartCore (Choice1Of2 node.Match) node.MatchExpr node.With
            +> sepNln
            +> genClauses node.Clauses
        )
        |> genNode node
    | Expr.TraitCall node ->
        genType node.Type
        +> sepColon
        +> sepOpenT
        +> genMemberDefn node.MemberDefn
        +> sepCloseT
        +> sepSpace
        +> genExpr node.Expr
        |> genNode node
    | Expr.ParenILEmbedded node -> genSingleTextNode node
    | Expr.ParenFunctionNameWithStar node ->
        genSingleTextNode node.OpeningParen
        +> sepSpace
        +> genSingleTextNode node.FunctionName
        +> sepSpace
        +> genSingleTextNode node.ClosingParen
        |> genNode node
    | Expr.Paren node ->
        match node.Expr with
        | Expr.CompExprBody _ ->
            genSingleTextNode node.OpeningParen
            +> atCurrentColumn (genExpr node.Expr)
            +> genSingleTextNode node.ClosingParen
        | _ ->
            genSingleTextNode node.OpeningParen
            +> genExpr node.Expr
            +> genSingleTextNode node.ClosingParen
        |> genNode node
    | Expr.Dynamic node -> genExpr node.FuncExpr +> !- "?" +> genExpr node.ArgExpr |> genNode node
    | Expr.PrefixApp node ->
        match node.Expr with
        | Expr.Constant _
        | Expr.InterpolatedStringExpr _ -> genSingleTextNode node.Operator +> sepSpace +> genExpr node.Expr
        | Expr.AppSingleParenArg appNode ->
            genSingleTextNode node.Operator
            +> sepSpace
            +> genExpr appNode.FunctionExpr
            +> genExpr appNode.ArgExpr
        | _ -> genSingleTextNode node.Operator +> genExpr node.Expr
        |> genNode node
    | Expr.SameInfixApps node ->
        let headIsSynExprLambdaOrIfThenElse = isSynExprLambdaOrIfThenElse node.LeadingExpr

        let shortExpr =
            onlyIf headIsSynExprLambdaOrIfThenElse sepOpenT
            +> genExpr node.LeadingExpr
            +> onlyIf headIsSynExprLambdaOrIfThenElse sepCloseT
            +> sepSpace
            +> col sepSpace node.SubsequentExpressions (fun (operator, rhs) ->
                genSingleTextNode operator
                +> sepSpace
                +> onlyIf headIsSynExprLambdaOrIfThenElse sepOpenT
                +> genExpr rhs
                +> onlyIf headIsSynExprLambdaOrIfThenElse sepCloseT)

        let multilineExpr =
            match node.SubsequentExpressions with
            | [] -> genExpr node.LeadingExpr
            | (operator, e2) :: es ->
                let m =
                    FSharp.Compiler.Text.Range.unionRanges (Expr.Node node.LeadingExpr).Range (Expr.Node e2).Range

                genMultilineInfixExpr (ExprInfixAppNode(node.LeadingExpr, operator, e2, m))
                +> sepNln
                +> col sepNln es (fun (operator, e) ->
                    genSingleTextNode operator +> sepSpace +> genExprInMultilineInfixExpr e)

        fun ctx ->
            genNode
                node
                (atCurrentColumn (isShortExpression ctx.Config.MaxInfixOperatorExpression shortExpr multilineExpr))
                ctx

    | Expr.InfixApp node ->
        let genOnelinerInfixExpr (node: ExprInfixAppNode) =
            let genExpr e =
                match e with
                | Expr.Record _
                | Expr.AnonRecord _ -> atCurrentColumnIndent (genExpr e)
                | _ -> genExpr e

            genExpr node.LeftHandSide
            +> sepSpace
            +> genSingleTextNode node.Operator
            +> sepNlnWhenWriteBeforeNewlineNotEmpty
            +> sepSpace
            +> genExpr node.RightHandSide

        if
            isSynExprLambdaOrIfThenElse node.LeftHandSide
            && newLineInfixOps.Contains node.Operator.Text
        then
            genNode node (genMultilineInfixExpr node)
        else
            fun ctx ->
                genNode
                    node
                    (isShortExpression
                        ctx.Config.MaxInfixOperatorExpression
                        (genOnelinerInfixExpr node)
                        (ifElse
                            (noBreakInfixOps.Contains(node.Operator.Text))
                            (genOnelinerInfixExpr node)
                            (genMultilineInfixExpr node)))
                    ctx

    | Expr.IndexWithoutDot node ->
        let genIndexExpr = genExpr node.Index

        genExpr node.Identifier
        +> sepOpenLFixed
        +> expressionFitsOnRestOfLine genIndexExpr (atCurrentColumnIndent genIndexExpr)
        +> sepCloseLFixed
        |> genNode node

    // Result<int, string>.Ok 42
    | Expr.AppDotGetTypeApp node ->
        genExpr node.TypeApp.Identifier
        +> genGenericTypeParameters node.TypeApp
        +> genIdentListNodeWithDot node.Property
        +> sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidth (col sepSpace node.Arguments genExpr)
        |> genNode node

    // Foo(fun x -> x).Bar().Meh
    | Expr.DotGetAppDotGetAppParenLambda node ->
        let short =
            genExpr node.Identifier
            +> genExpr node.IdentifierArg
            +> genIdentListNodeWithDot node.AppLids
            +> col sepComma node.Args genExpr
            +> genIdentListNodeWithDot node.Property

        let long =
            let functionName =
                match node.Identifier with
                | Expr.OptVar identifierNode when List.moreThanOne identifierNode.Identifier.Content ->
                    genFunctionNameWithMultilineLids sepNone identifierNode.Identifier identifierNode
                | Expr.TypeApp typedAppNode ->
                    match typedAppNode.Identifier with
                    | Expr.OptVar identifierNode when List.moreThanOne identifierNode.Identifier.Content ->
                        genFunctionNameWithMultilineLids
                            (genGenericTypeParameters typedAppNode)
                            identifierNode.Identifier
                            typedAppNode
                    | _ -> genExpr node.Identifier
                | _ -> genExpr node.Identifier

            functionName
            +> indent
            +> genExpr node.IdentifierArg
            +> sepNln
            +> genIdentListNodeWithDot node.AppLids
            +> col sepComma node.Args genExpr
            +> sepNln
            +> genIdentListNodeWithDot node.Property
            +> unindent

        fun ctx -> genNode node (isShortExpression ctx.Config.MaxDotGetExpressionWidth short long) ctx

    // Foo().Bar
    | Expr.DotGetAppParen node ->
        let short =
            genExpr node.Function
            +> genExpr node.ParenArg
            +> genIdentListNodeWithDot node.Property

        let long =
            let functionName argFn =
                match node.Function with
                | Expr.OptVar identifierNode when List.moreThanOne identifierNode.Identifier.Content ->
                    genFunctionNameWithMultilineLids argFn identifierNode.Identifier identifierNode
                | Expr.TypeApp typedAppNode ->
                    match typedAppNode.Identifier with
                    | Expr.OptVar identifierNode when List.moreThanOne identifierNode.Identifier.Content ->
                        genFunctionNameWithMultilineLids
                            (genGenericTypeParameters typedAppNode +> argFn)
                            identifierNode.Identifier
                            typedAppNode
                    | _ -> genExpr node.Function
                | Expr.DotGetAppDotGetAppParenLambda _ ->
                    leadingExpressionIsMultiline (genExpr node.Function) (fun isMultiline ->
                        if isMultiline then indent +> argFn +> unindent else argFn)
                | _ -> genExpr node.Function +> argFn

            let arguments = genMultilineFunctionApplicationArguments node.ParenArg

            functionName arguments
            +> indentSepNlnUnindent (genIdentListNodeWithDotMultiline node.Property)

        fun ctx -> genNode node (isShortExpression ctx.Config.MaxDotGetExpressionWidth short long) ctx

    // Foo(fun x -> x).Bar()
    | Expr.DotGetAppWithParenLambda node ->
        let genLongFunctionName f =
            match node.Function with
            | Expr.OptVar identifierNode when List.moreThanOne identifierNode.Identifier.Content ->
                genFunctionNameWithMultilineLids f identifierNode.Identifier identifierNode
            | Expr.TypeApp typedAppNode ->
                match typedAppNode.Identifier with
                | Expr.OptVar identifierNode when List.moreThanOne identifierNode.Identifier.Content ->
                    genFunctionNameWithMultilineLids
                        (genGenericTypeParameters typedAppNode +> f)
                        identifierNode.Identifier
                        typedAppNode
                | _ -> genExpr node.Function
            | _ -> genExpr node.Function +> f

        let lastEsIndex = node.Arguments.Length - 1

        let genApp (idx: int) (argNode: DotGetAppPartNode) : Context -> Context =
            let short =
                genIdentListNodeWithDot argNode.Identifier
                +> optSingle (fun (lt, ts, gt) -> genGenericTypeParametersAux lt ts gt) argNode.TypeParameterInfo
                +> genSpaceBeforeLids idx lastEsIndex argNode.Identifier argNode.Expr
                +> genExpr argNode.Expr

            let long =
                genIdentListNodeWithDotMultiline argNode.Identifier
                +> optSingle (fun (lt, ts, gt) -> genGenericTypeParametersAux lt ts gt) argNode.TypeParameterInfo
                +> genSpaceBeforeLids idx lastEsIndex argNode.Identifier argNode.Expr
                +> genMultilineFunctionApplicationArguments argNode.Expr

            expressionFitsOnRestOfLine short long

        let short =
            genExpr node.Function
            +> genExpr (Expr.ParenLambda node.ParenLambda)
            +> coli sepNone node.Arguments (fun idx (argNode: DotGetAppPartNode) ->
                genIdentListNodeWithDot argNode.Identifier
                +> optSingle (fun (lt, ts, gt) -> genGenericTypeParametersAux lt ts gt) argNode.TypeParameterInfo
                +> genSpaceBeforeLids idx lastEsIndex argNode.Identifier argNode.Expr
                +> genExpr argNode.Expr)

        let long =
            genLongFunctionName (genExpr (Expr.ParenLambda node.ParenLambda))
            +> indent
            +> sepNln
            +> coli sepNln node.Arguments genApp
            +> unindent

        fun ctx -> genNode node (isShortExpression ctx.Config.MaxDotGetExpressionWidth short long) ctx

    // Foo().Bar().Meh()
    | Expr.DotGetApp node ->
        let genLongFunctionName =
            let genApp funcExpr argExpr =
                match funcExpr, argExpr with
                // | AppOrTypeApp(LongIdentExprWithMoreThanOneIdent lids, t, [ Paren _ as px ]) ->
                | Expr.OptVar optVarNode, [ ParenExpr px ] when List.moreThanOne optVarNode.Identifier.Content ->
                    genFunctionNameWithMultilineLids
                        (expressionFitsOnRestOfLine (genExpr px) (genMultilineFunctionApplicationArguments px))
                        optVarNode.Identifier
                        optVarNode

                | Expr.TypeApp typeAppNode, [ ParenExpr px ] ->
                    match typeAppNode.Identifier with
                    | Expr.OptVar optVarNode when List.moreThanOne optVarNode.Identifier.Content ->
                        genFunctionNameWithMultilineLids
                            (genGenericTypeParameters typeAppNode
                             +> expressionFitsOnRestOfLine (genExpr px) (genMultilineFunctionApplicationArguments px))
                            optVarNode.Identifier
                            optVarNode
                    | _ -> genExpr node.FunctionExpr

                // | AppOrTypeApp(LongIdentExprWithMoreThanOneIdent lids, t, [ e2 ]) ->
                | Expr.OptVar optVarNode, [ e2 ] when List.moreThanOne optVarNode.Identifier.Content ->
                    genFunctionNameWithMultilineLids (genExpr e2) optVarNode.Identifier optVarNode

                | Expr.TypeApp typeAppNode, [ e2 ] ->
                    match typeAppNode.Identifier with
                    | Expr.OptVar optVarNode when List.moreThanOne optVarNode.Identifier.Content ->
                        genFunctionNameWithMultilineLids
                            (genGenericTypeParameters typeAppNode +> genExpr e2)
                            optVarNode.Identifier
                            optVarNode
                    | _ -> genExpr node.FunctionExpr

                // | AppOrTypeApp(SimpleExpr e, t, [ ConstExpr(SynConst.Unit, r) ]) ->
                | Expr.Ident _, [ Expr.Constant(Constant.Unit _) as unitExpr ] -> genExpr funcExpr +> genExpr unitExpr

                | Expr.TypeApp typeAppNode, [ Expr.Constant(Constant.Unit _) as unitExpr ] ->
                    match typeAppNode.Identifier with
                    | Expr.Ident _ ->
                        genExpr typeAppNode.Identifier
                        +> genGenericTypeParameters typeAppNode
                        +> genExpr unitExpr
                    | _ -> genExpr node.FunctionExpr

                // | AppOrTypeApp(SimpleExpr e, t, [ Paren _ as px ]) ->
                | Expr.Ident _, [ ParenExpr parenExpr ] ->
                    let short = genExpr funcExpr +> genExpr parenExpr
                    let long = genExpr funcExpr +> genMultilineFunctionApplicationArguments parenExpr
                    expressionFitsOnRestOfLine short long

                | Expr.TypeApp typeAppNode, [ ParenExpr parenExpr ] ->
                    match typeAppNode.Identifier with
                    | Expr.Ident _ ->
                        let short =
                            genExpr typeAppNode.Identifier
                            +> genGenericTypeParameters typeAppNode
                            +> genExpr parenExpr

                        let long =
                            genExpr typeAppNode.Identifier
                            +> genGenericTypeParameters typeAppNode
                            +> genMultilineFunctionApplicationArguments parenExpr

                        expressionFitsOnRestOfLine short long
                    | _ -> genExpr node.FunctionExpr

                | _ -> genExpr node.FunctionExpr

            match node.FunctionExpr with
            | Expr.App appNode -> genApp appNode.FunctionExpr appNode.Arguments
            | Expr.AppLongIdentAndSingleParenArg appNode ->
                let m = (appNode.FunctionName :> Node).Range
                genApp (Expr.OptVar(ExprOptVarNode(false, appNode.FunctionName, m))) [ appNode.ArgExpr ]
            | Expr.AppSingleParenArg appNode -> genApp appNode.FunctionExpr [ appNode.ArgExpr ]
            | _ -> genExpr node.FunctionExpr

        let lastEsIndex = node.Arguments.Length - 1

        let genApp (idx: int) (argNode: DotGetAppPartNode) : Context -> Context =
            let short =
                genIdentListNodeWithDot argNode.Identifier
                +> optSingle (fun (lt, ts, gt) -> genGenericTypeParametersAux lt ts gt) argNode.TypeParameterInfo
                +> genSpaceBeforeLids idx lastEsIndex argNode.Identifier argNode.Expr
                +> genExpr argNode.Expr

            let long =
                genIdentListNodeWithDotMultiline argNode.Identifier
                +> optSingle (fun (lt, ts, gt) -> genGenericTypeParametersAux lt ts gt) argNode.TypeParameterInfo
                +> genSpaceBeforeLids idx lastEsIndex argNode.Identifier argNode.Expr
                +> genMultilineFunctionApplicationArguments argNode.Expr

            expressionFitsOnRestOfLine short long |> genNode argNode

        let short =
            match node.FunctionExpr with
            | Expr.AppLongIdentAndSingleParenArg appNode ->
                genIdentListNode appNode.FunctionName +> genExpr appNode.ArgExpr
            | Expr.AppSingleParenArg appNode -> genExpr appNode.FunctionExpr +> genExpr appNode.ArgExpr
            | Expr.App appNode ->
                if appNode.Arguments.Length = 1 then
                    match appNode.Arguments.[0] with
                    | ParenExpr parenExpr -> genExpr appNode.FunctionExpr +> genExpr parenExpr
                    | Expr.ArrayOrList _ -> genExpr appNode.FunctionExpr +> genExpr appNode.Arguments.[0]
                    | _ -> genExpr node.FunctionExpr
                else
                    genExpr node.FunctionExpr
            | _ -> genExpr node.FunctionExpr
            +> coli sepNone node.Arguments genApp

        let long =
            genLongFunctionName
            +> indent
            +> sepNln
            +> coli sepNln node.Arguments genApp
            +> unindent

        fun ctx -> genNode node (isShortExpression ctx.Config.MaxDotGetExpressionWidth short long) ctx

    // path.Replace("../../../", "....")
    | Expr.AppLongIdentAndSingleParenArg node ->
        let addSpace =
            sepSpaceBeforeParenInFuncInvocation
                (Expr.OptVar(ExprOptVarNode(false, node.FunctionName, (node.FunctionName :> Node).Range)))
                node.ArgExpr

        let shortLids = genIdentListNode node.FunctionName
        let short = shortLids +> addSpace +> genExpr node.ArgExpr

        let long =
            let args =
                addSpace
                +> expressionFitsOnRestOfLine
                    (genExpr node.ArgExpr)
                    (genMultilineFunctionApplicationArguments node.ArgExpr)

            ifElseCtx
                (futureNlnCheck shortLids)
                (genFunctionNameWithMultilineLids args node.FunctionName node)
                (shortLids +> args)

        expressionFitsOnRestOfLine short long |> genNode node
    // fn (a, b, c)
    | Expr.AppSingleParenArg node ->
        let short =
            genExpr node.FunctionExpr
            +> sepSpaceBeforeParenInFuncInvocation node.FunctionExpr node.ArgExpr
            +> genExpr node.ArgExpr

        let long =
            genExpr node.FunctionExpr
            +> sepSpaceBeforeParenInFuncInvocation node.FunctionExpr node.ArgExpr
            +> genMultilineFunctionApplicationArguments node.ArgExpr

        expressionFitsOnRestOfLine short long |> genNode node

    // functionName arg1 arg2 (fun x y z -> ...)
    | Expr.DotGetAppWithLambda node ->
        leadingExpressionIsMultiline (genAppWithLambda sepNone node.AppWithLambda) (fun isMultiline ->
            if isMultiline then
                (indent +> sepNln +> genIdentListNodeWithDotMultiline node.Property +> unindent)
            else
                genIdentListNodeWithDot node.Property)
        |> genNode node
    | Expr.AppWithLambda node ->
        let sepSpaceAfterFunctionName =
            match node.Arguments with
            | [] ->
                // We create a temporary fake paren node only for the sepSpaceBeforeParenInFuncInvocation call.
                let parenExpr =
                    ExprParenNode(
                        node.OpeningParen,
                        Expr.Null(SingleTextNode("", FSharp.Compiler.Text.Range.Zero)),
                        node.ClosingParen,
                        FSharp.Compiler.Text.Range.Zero
                    )
                    |> Expr.Paren

                sepSpaceBeforeParenInFuncInvocation node.FunctionName parenExpr
            | _ -> sepSpace

        genAppWithLambda sepSpaceAfterFunctionName node
    | Expr.NestedIndexWithoutDot node ->
        genExpr node.Identifier
        +> sepOpenLFixed
        +> genExpr node.Index
        +> sepCloseLFixed
        +> genExpr node.Argument
        |> genNode node
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
        |> genNode node
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
        |> genNode node
    | Expr.App node ->
        let shortExpression =
            let sep ctx =
                match node.Arguments with
                | [] -> ctx
                | [ singleArg ] -> sepSpaceBeforeParenInFuncInvocation node.FunctionExpr singleArg ctx
                | _ -> sepSpace ctx

            atCurrentColumn (genExpr node.FunctionExpr +> sep +> col sepSpace node.Arguments genExpr)

        let longExpression =
            atCurrentColumn (
                genExpr node.FunctionExpr
                +> indentSepNlnUnindent (col sepNln node.Arguments genExpr)
            )

        expressionFitsOnRestOfLine shortExpression longExpression |> genNode node
    | Expr.TypeApp node -> genExpr node.Identifier +> genGenericTypeParameters node |> genNode node
    | Expr.TryWithSingleClause node ->
        let genClause =
            let clauseNode = node.Clause

            leadingExpressionResult
                (enterNode clauseNode
                 +> optSingle (fun bar -> genNode bar sepNone) clauseNode.Bar)
                (fun ((linesBefore, _), (linesAfter, _)) ->
                    onlyIfCtx (fun ctx -> linesAfter > linesBefore || hasWriteBeforeNewlineContent ctx) sepBar)
            +> genPatInClause clauseNode.Pattern
            +> optSingle
                (fun e -> !- " when" +> sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidth (genExpr e))
                clauseNode.WhenExpr
            +> sepSpace
            +> genSingleTextNode clauseNode.Arrow
            +> autoIndentAndNlnExpressUnlessStroustrup genExpr clauseNode.BodyExpr
            +> leaveNode clauseNode

        atCurrentColumn (
            genSingleTextNode node.Try
            +> indent
            +> sepNln
            +> genExpr node.TryExpr
            +> unindent
            +> sepNln
            +> genSingleTextNode node.With
            +> sepSpace
            +> genClause
        )
        |> genNode node
    | Expr.TryWith node ->
        atCurrentColumn (
            genSingleTextNode node.Try
            +> indent
            +> sepNln
            +> genExpr node.TryExpr
            +> unindent
            +> sepNln
            +> genSingleTextNode node.With
            +> sepNln
            +> col sepNln node.Clauses (genClause false)
        )
        |> genNode node
    | Expr.TryFinally node ->
        atCurrentColumn (
            genSingleTextNode node.Try
            +> indentSepNlnUnindent (genExpr node.TryExpr)
            +> sepNln
            +> genSingleTextNode node.Finally
            +> indentSepNlnUnindent (genExpr node.FinallyExpr)
        )
        |> genNode node
    | Expr.IfThen node ->
        leadingExpressionResult
            (genControlExpressionStartCore (Choice2Of2 node.If) node.IfExpr node.Then)
            (fun ((lineCountBefore, columnBefore), (lineCountAfter, columnAfter)) ctx ->
                // Check if the `if expr then` is already multiline or cross the max_line_length.
                let isMultiline =
                    lineCountAfter > lineCountBefore || columnAfter > ctx.Config.MaxLineLength

                if isMultiline then
                    indentSepNlnUnindent (genExpr node.ThenExpr) ctx
                else
                    // Check if the entire expression is will still fit on one line, respecting MaxIfThenShortWidth
                    let remainingMaxLength =
                        ctx.Config.MaxIfThenShortWidth - (columnAfter - columnBefore)

                    isShortExpression
                        remainingMaxLength
                        (sepSpace +> genExpr node.ThenExpr)
                        (indentSepNlnUnindent (genExpr node.ThenExpr))
                        ctx)
        |> atCurrentColumnIndent
        |> genNode node
    | Expr.IfThenElse node ->
        leadingExpressionResult
            (genControlExpressionStartCore (Choice2Of2 node.If) node.IfExpr node.Then)
            (fun ((lineCountBefore, columnBefore), (lineCountAfter, columnAfter)) ctx ->
                let long =
                    indentSepNlnUnindent (genExpr node.ThenExpr)
                    +> sepNln
                    +> genSingleTextNode node.Else
                    +> genKeepIdent node.Else node.ElseExpr
                    +> sepNln
                    +> genExpr node.ElseExpr
                    +> unindent

                // Check if the `if expr then` is already multiline or cross the max_line_length.
                let isMultiline =
                    lineCountAfter > lineCountBefore || columnAfter > ctx.Config.MaxLineLength

                // If the `thenExpr` is also an SynExpr.IfThenElse, it will not be valid code if put on one line.
                // ex: if cond then if a then b else c else e2
                let thenExprIsIfThenElse =
                    match node.ThenExpr with
                    | Expr.IfThen _
                    | Expr.IfThenElse _
                    | Expr.IfThenElif _ -> true
                    | _ -> false

                if isMultiline || thenExprIsIfThenElse then
                    long ctx
                else
                    // Check if the entire expression is will still fit on one line, respecting MaxIfThenShortWidth
                    let remainingMaxLength =
                        ctx.Config.MaxIfThenElseShortWidth - (columnAfter - columnBefore)

                    isShortExpression
                        remainingMaxLength
                        (sepSpace
                         +> genExpr node.ThenExpr
                         +> sepSpace
                         +> genSingleTextNode node.Else
                         +> sepSpace
                         +> genExpr node.ElseExpr)
                        long
                        ctx)
        |> atCurrentColumnIndent
        |> genNode node
    | Expr.IfThenElif node ->
        // multiple branches but no else expr
        // use the same threshold check as for if-then
        // Everything should fit on one line
        let areAllShort ctx =
            let anyThenExprIsIfThenElse =
                node.Branches
                |> List.exists (fun node ->
                    match node.ThenExpr with
                    | Expr.IfThen _
                    | Expr.IfThenElif _
                    | Expr.IfThenElse _ -> true
                    | _ -> false)

            let checkIfLine (node: ExprIfThenNode) =
                genControlExpressionStartCore (Choice2Of2 node.If) node.IfExpr node.Then
                +> sepSpace
                +> genExpr node.ThenExpr

            let linesToCheck =
                match node.Else with
                | None -> List.map checkIfLine node.Branches
                | Some(elseNode, elseExpr) ->
                    // This may appear a bit odd that we are adding the `else elseExpr` before the `if expr then expr` lines but purely for this check this doesn't matter.
                    // Each lines needs to fit on one line in order for us to format the short way
                    (genSingleTextNode elseNode +> sepSpace +> genExpr elseExpr)
                    :: (List.map checkIfLine node.Branches)

            let lineCheck () =
                linesToCheck
                |> List.forall (fun lineCheck ->
                    let maxWidth =
                        if node.Else.IsSome then
                            ctx.Config.MaxIfThenElseShortWidth
                        else
                            ctx.Config.MaxIfThenShortWidth

                    not (exceedsWidth maxWidth lineCheck ctx))

            not anyThenExprIsIfThenElse && lineCheck ()

        let shortExpr =
            col sepNln node.Branches (fun (node: ExprIfThenNode) ->
                genControlExpressionStartCore (Choice2Of2 node.If) node.IfExpr node.Then
                +> sepSpace
                +> genExpr node.ThenExpr
                |> genNode node)
            +> optSingle
                (fun (elseNode, elseExpr) -> sepNln +> genSingleTextNode elseNode +> sepSpace +> genExpr elseExpr)
                node.Else

        let longExpr =
            col sepNln node.Branches (fun (node: ExprIfThenNode) ->
                genControlExpressionStartCore (Choice2Of2 node.If) node.IfExpr node.Then
                +> indentSepNlnUnindent (genExpr node.ThenExpr)
                |> genNode node)
            +> optSingle
                (fun (elseNode, elseExpr) ->
                    sepNln
                    +> genSingleTextNode elseNode
                    +> genKeepIdent elseNode elseExpr
                    +> sepNln
                    +> genExpr elseExpr
                    +> unindent)
                node.Else

        ifElseCtx areAllShort shortExpr longExpr
        |> atCurrentColumnIndent
        |> genNode node
    | Expr.Ident node -> genSingleTextNode node
    | Expr.OptVar node ->
        onlyIf node.IsOptional (!- "?") +> genIdentListNode node.Identifier
        |> genNode node
    | Expr.LongIdentSet node ->
        genIdentListNode node.Identifier
        +> sepArrowRev
        +> autoIndentAndNlnIfExpressionExceedsPageWidthUnlessStroustrup genExpr node.Expr
        |> genNode node
    | Expr.DotIndexedGet node ->
        let genDotIndexedGet =
            let isParen =
                match node.ObjectExpr with
                | Expr.Paren _ -> true
                | _ -> false

            ifElse isParen (genExpr node.ObjectExpr) (addParenIfAutoNln node.ObjectExpr genExpr)
            +> !- "."
            +> sepOpenLFixed
            +> genExpr node.IndexExpr
            +> sepCloseLFixed

        match node.ObjectExpr with
        | Expr.App appNode ->
            match appNode.Arguments with
            | [ Expr.Constant(Constant.Unit _) ] ->
                genExpr appNode.FunctionExpr
                +> genExpr node.ObjectExpr
                +> !- "."
                +> sepOpenLFixed
                +> genExpr node.IndexExpr
                +> sepCloseLFixed
            | _ -> genDotIndexedGet
        | Expr.AppSingleParenArg appNode ->
            let short = genExpr appNode.FunctionExpr +> genExpr appNode.ArgExpr

            let long =
                genExpr appNode.FunctionExpr
                +> genMultilineFunctionApplicationArguments appNode.ArgExpr

            let idx = !- "." +> sepOpenLFixed +> genExpr node.IndexExpr +> sepCloseLFixed
            expressionFitsOnRestOfLine (short +> idx) (long +> idx)
        | _ -> genDotIndexedGet
        |> genNode node
    | Expr.DotIndexedSet node ->
        let genDotIndexedSet =
            addParenIfAutoNln node.ObjectExpr genExpr
            +> !- ".["
            +> genExpr node.Index
            +> !- "] <- "
            +> autoIndentAndNlnIfExpressionExceedsPageWidthUnlessStroustrup genExpr node.Value

        match node.ObjectExpr with
        | Expr.App appNode ->
            match appNode.Arguments with
            | [ Expr.Constant(Constant.Unit _) as ux ] ->
                let appExpr = genExpr appNode.FunctionExpr +> genExpr ux

                let idx =
                    !- "." +> sepOpenLFixed +> genExpr node.Index +> sepCloseLFixed +> sepArrowRev

                expressionFitsOnRestOfLine
                    (appExpr +> idx +> genExpr node.Value)
                    (appExpr
                     +> idx
                     +> autoIndentAndNlnIfExpressionExceedsPageWidthUnlessStroustrup genExpr node.Value)
            | _ -> genDotIndexedSet
        | Expr.AppSingleParenArg appNode ->
            let short = genExpr appNode.FunctionExpr +> genExpr appNode.ArgExpr

            let long =
                genExpr appNode.FunctionExpr
                +> genMultilineFunctionApplicationArguments appNode.ArgExpr

            let idx =
                !- "." +> sepOpenLFixed +> genExpr node.Index +> sepCloseLFixed +> sepArrowRev

            expressionFitsOnRestOfLine
                (short +> idx +> genExpr node.Value)
                (long
                 +> idx
                 +> autoIndentAndNlnIfExpressionExceedsPageWidthUnlessStroustrup genExpr node.Value)
        | _ -> genDotIndexedSet
        |> genNode node
    | Expr.NamedIndexedPropertySet node ->
        match node.Index with
        | Expr.ArrayOrList arrayNode when arrayNode.Elements.Length = 1 ->
            genIdentListNode node.Identifier
            +> genSingleTextNode arrayNode.Opening
            +> col sepNone arrayNode.Elements genExpr
            +> genSingleTextNode arrayNode.Closing
            +> sepArrowRev
            +> autoIndentAndNlnIfExpressionExceedsPageWidth (genExpr node.Value)
        | _ ->
            let sep =
                match node.Index with
                | Expr.Constant _
                | Expr.Ident _ -> sepSpace
                | _ -> sepNone

            genIdentListNode node.Identifier
            +> sep
            +> genExpr node.Index
            +> sepArrowRev
            +> autoIndentAndNlnIfExpressionExceedsPageWidth (genExpr node.Value)
        |> genNode node
    | Expr.DotNamedIndexedPropertySet node ->
        genExpr node.Identifier
        +> sepDot
        +> genIdentListNode node.Name
        +> genExpr node.Property
        +> sepArrowRev
        +> autoIndentAndNlnIfExpressionExceedsPageWidth (genExpr node.Set)
        |> genNode node
    | Expr.DotGet node ->
        let shortExpr = genExpr node.Expr +> genIdentListNodeWithDot node.Identifier

        let longExpr =
            genExpr node.Expr
            +> indentSepNlnUnindent (genIdentListNodeWithDotMultiline node.Identifier)

        fun ctx -> genNode node (isShortExpression ctx.Config.MaxDotGetExpressionWidth shortExpr longExpr) ctx
    | Expr.DotSet node ->
        match node.Identifier with
        | Expr.AppLongIdentAndSingleParenArg appNode ->
            genIdentListNode appNode.FunctionName
            +> genExpr appNode.ArgExpr
            +> sepDot
            +> genIdentListNode node.Property
            +> sepArrowRev
            +> autoIndentAndNlnIfExpressionExceedsPageWidthUnlessStroustrup genExpr node.Set
        | _ ->
            addParenIfAutoNln node.Identifier genExpr
            +> sepDot
            +> genIdentListNode node.Property
            +> sepArrowRev
            +> autoIndentAndNlnIfExpressionExceedsPageWidthUnlessStroustrup genExpr node.Set
        |> genNode node
    | Expr.Set node ->
        addParenIfAutoNln node.Identifier genExpr
        +> sepArrowRev
        +> autoIndentAndNlnIfExpressionExceedsPageWidthUnlessStroustrup genExpr node.Set
        |> genNode node
    | Expr.LibraryOnlyStaticOptimization node ->
        genExpr node.OptimizedExpr
        +> onlyIfNot node.Constraints.IsEmpty (!- " when ")
        +> col sepSpace node.Constraints (function
            | StaticOptimizationConstraint.WhenTyparTyconEqualsTycon n ->
                genSingleTextNode n.TypeParameter +> sepColon +> sepSpace +> genType n.Type
            | StaticOptimizationConstraint.WhenTyparIsStruct t -> genSingleTextNode t)
        +> sepEq
        +> sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidth (genExpr node.Expr)
        |> genNode node
    | Expr.InterpolatedStringExpr node ->
        let genInterpolatedFillExpr expr =
            fun ctx ->
                let currentConfig = ctx.Config

                let interpolatedConfig =
                    { currentConfig with
                        // override the max line length for the interpolated expression.
                        // this is to avoid scenarios where the long / multiline format of the expression will be used
                        // where the construct is this short
                        // see unit test ``construct url with Fable``
                        MaxLineLength = ctx.WriterModel.Column + ctx.Config.MaxLineLength }

                genExpr expr { ctx with Config = interpolatedConfig }
                // Restore the existing configuration after printing the interpolated expression
                |> fun ctx -> { ctx with Config = currentConfig }
            |> atCurrentColumnIndent

        onlyIfCtx (fun ctx -> ctx.Config.StrictMode) (!- "$\"")
        +> col sepNone node.Parts (fun part ->
            match part with
            | Choice1Of2 stringNode -> genSingleTextNode stringNode
            | Choice2Of2 fillNode ->
                fun ctx ->
                    let genFill =
                        genInterpolatedFillExpr fillNode.Expr
                        +> optSingle (fun format -> sepColonFixed +> genSingleTextNode format) fillNode.Ident

                    if ctx.Config.StrictMode then
                        (!- "{" +> genFill +> !- "}") ctx
                    else
                        genFill ctx)
        +> onlyIfCtx (fun ctx -> ctx.Config.StrictMode) (!- "\"")
        |> genNode node
    | Expr.IndexRangeWildcard node -> genSingleTextNode node
    | Expr.TripleNumberIndexRange node ->
        genSingleTextNode node.Start
        +> genSingleTextNode node.StartDots
        +> genSingleTextNode node.Center
        +> genSingleTextNode node.EndDots
        +> genSingleTextNode node.End
        |> genNode node
    | Expr.IndexRange node ->
        optSingle genExpr node.From
        +> genSingleTextNode node.Dots
        +> optSingle genExpr node.To
        |> genNode node
    | Expr.IndexFromEnd node -> !- "^" +> genExpr node.Expr |> genNode node
    | Expr.Typar node -> genSingleTextNode node

let genQuoteExpr (node: ExprQuoteNode) =
    genSingleTextNode node.OpenToken
    +> sepSpace
    +> expressionFitsOnRestOfLine (genExpr node.Expr) (indent +> sepNln +> genExpr node.Expr +> unindent +> sepNln)
    +> sepSpace
    +> genSingleTextNode node.CloseToken
    |> genNode node

let genMultilineFunctionApplicationArguments (argExpr: Expr) =
    let argsInsideParenthesis (parenNode: ExprParenNode) f =
        genSingleTextNode parenNode.OpeningParen
        +> indentSepNlnUnindent f
        +> sepNln
        +> genSingleTextNode parenNode.ClosingParen
        |> genNode parenNode

    let genExpr e =
        match e with
        | Expr.InfixApp infixNode when (infixNode.Operator.Text = "=") -> genNamedArgumentExpr infixNode
        | _ -> genExpr e

    match argExpr with
    | Expr.Paren parenNode ->
        match parenNode.Expr with
        | Expr.Tuple tupleNode ->
            genTupleMultiline tupleNode
            |> genNode tupleNode
            |> argsInsideParenthesis parenNode
        | _ -> genExpr parenNode.Expr |> argsInsideParenthesis parenNode
    | _ -> genExpr argExpr

let genTuple (node: ExprTupleNode) =
    let shortExpression =
        col sepNone node.Items (function
            | Choice1Of2 e ->
                match e with
                | Expr.IfThen _
                | Expr.IfThenElif _
                | Expr.IfThenElse _
                | Expr.Lambda _ -> sepOpenT +> genExpr e +> sepCloseT
                | e -> genExpr e
            | Choice2Of2 comma -> genSingleTextNode comma +> addSpaceIfSpaceAfterComma)

    let longExpression = genTupleMultiline node

    atCurrentColumn (expressionFitsOnRestOfLine shortExpression longExpression)
    |> genNode node

let genTupleMultiline (node: ExprTupleNode) =
    let containsLambdaOrMatchExpr =
        // If the any items (expect the last) is a match/lambda
        node.Items
        |> List.chunkBySize 2
        |> List.exists (fun pair ->
            match pair with
            | [ Choice1Of2 e; Choice2Of2 _ ] ->
                match e with
                | Expr.Match _
                | Expr.Lambda _ -> true
                | Expr.InfixApp node ->
                    match node.RightHandSide with
                    | Expr.Lambda _ -> true
                    | _ -> false
                | _ -> false
            | _ -> false)

    let lastIndex = List.length node.Items - 1

    let genItem idx =
        function
        | Choice1Of2 e ->
            match e with
            | Expr.IfThen _
            | Expr.IfThenElif _
            | Expr.IfThenElse _ when (idx < lastIndex) -> autoParenthesisIfExpressionExceedsPageWidth (genExpr e)
            | Expr.InfixApp node when (node.Operator.Text = "=") -> genNamedArgumentExpr node
            | _ -> genExpr e
        | Choice2Of2 comma ->
            if containsLambdaOrMatchExpr then
                sepNln +> genSingleTextNode comma +> sepSpace
            else
                genSingleTextNode comma +> sepNln

    coli sepNone node.Items genItem

let genNamedArgumentExpr (node: ExprInfixAppNode) =
    let short =
        genExpr node.LeftHandSide
        +> sepSpace
        +> genSingleTextNode node.Operator
        +> sepSpace
        +> genExpr node.RightHandSide

    let long =
        genExpr node.LeftHandSide
        +> sepSpace
        +> genSingleTextNode node.Operator
        +> autoIndentAndNlnExpressUnlessStroustrup (fun e -> sepSpace +> genExpr e) node.RightHandSide

    expressionFitsOnRestOfLine short long |> genNode node

let genLambda (node: ExprLambdaNode) =
    let genPats =
        let shortPats = col sepSpace node.Parameters genPat
        let longPats = indentSepNlnUnindent (col sepNln node.Parameters genPat)
        expressionFitsOnRestOfLine shortPats longPats

    genSingleTextNode node.Fun
    +> sepSpace
    +> genPats
    +> sepSpace
    +> genSingleTextNode node.Arrow
    +> (fun ctx ->
        if hasWriteBeforeNewlineContent ctx then
            indentSepNlnUnindent (genExpr node.Expr) ctx
        else
            sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidthUnlessStroustrup genExpr node.Expr ctx)
    |> genNode node

let genClauses (clauses: MatchClauseNode list) =
    let lastIndex = clauses.Length - 1

    coli sepNln clauses (fun idx clause ->
        let isLastItem = lastIndex = idx

        genClause isLastItem clause)

let genClause (isLastItem: bool) (node: MatchClauseNode) =
    let genBar =
        match node.Bar with
        | Some barNode -> genSingleTextNode barNode +> sepSpace
        | None -> sepBar

    let genPatAndBody =
        let genPatAndBody =
            genPatInClause node.Pattern
            +> sepSpace
            +> optSingle (fun whenExpr -> !- "when " +> genExpr whenExpr +> sepSpace) node.WhenExpr
            +> genSingleTextNode node.Arrow
            +> (fun ctx ->
                if isLastItem && ctx.Config.ExperimentalKeepIndentInBranch then
                    expressionFitsOnRestOfLine
                        (genExpr node.BodyExpr)
                        (sepNln
                         +> sepNlnUnlessContentBefore (Expr.Node node.BodyExpr)
                         +> genExpr node.BodyExpr)
                        ctx
                else
                    sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidthUnlessStroustrup genExpr node.BodyExpr ctx)

        fun ctx ->
            if ctx.Config.ExperimentalStroustrupStyle && node.BodyExpr.IsStroustrupStyleExpr then
                atCurrentColumn genPatAndBody ctx
            else
                genPatAndBody ctx

    genBar +> genPatAndBody |> genNode node

let genControlExpressionStartCore
    (startKeyword: Choice<SingleTextNode, MultipleTextsNode>)
    (innerExpr: Expr)
    (endKeyword: SingleTextNode)
    =
    let enterStart =
        match startKeyword with
        | Choice1Of2 n -> enterNode (n :> Node)
        | Choice2Of2 n -> enterNode (n :> Node) +> optSingle enterNode (List.tryHead n.Content)

    let genStart =
        match startKeyword with
        | Choice1Of2 node -> !-node.Text
        | Choice2Of2 mtn ->
            coli sepSpace mtn.Content (fun idx node ->
                onlyIf (idx <> 0) (enterNode node)
                +> !-node.Text
                +> leaveNode node
                +> sepNlnWhenWriteBeforeNewlineNotEmpty)

    let leaveStart =
        match startKeyword with
        | Choice1Of2 n -> leaveNode (n :> Node)
        | Choice2Of2 n -> leaveNode (n :> Node)

    let shortIfExpr =
        genStart
        +> leaveStart
        +> sepNlnWhenWriteBeforeNewlineNotEmptyOr sepSpace
        +> genExpr innerExpr
        +> sepSpace
        +> enterNode endKeyword
        +> !-endKeyword.Text

    let longIfExpr =
        genStart
        +> leaveStart
        +> indentSepNlnUnindent (genExpr innerExpr)
        +> sepNln
        +> enterNode endKeyword
        +> !-endKeyword.Text

    // A code comment before the start keyword should not make the expression long.
    enterStart
    +> expressionFitsOnRestOfLine shortIfExpr longIfExpr
    +> leaveNode endKeyword

// Caller of this function is responsible for genNode!
let genMultilineInfixExpr (node: ExprInfixAppNode) =
    let genLhs (ctx: Context) =
        match node.LeftHandSide with
        | Expr.IfThen _
        | Expr.IfThenElse _
        | Expr.IfThenElif _ when (ctx.Config.IndentSize - 1 <= node.Operator.Text.Length) ->
            autoParenthesisIfExpressionExceedsPageWidth (genExpr node.LeftHandSide) ctx
        | Expr.Match _ when (ctx.Config.IndentSize <= node.Operator.Text.Length) ->
            let ctxAfterMatch = genExpr node.LeftHandSide ctx

            let lastClauseIsSingleLine =
                Queue.rev ctxAfterMatch.WriterEvents
                |> Seq.skipWhile (fun e ->
                    match e with
                    | RestoreIndent _
                    | RestoreAtColumn _ -> true
                    | _ -> false)
                // In case the last clause was multiline an UnIndent event should follow
                |> Seq.tryHead
                |> fun e ->
                    match e with
                    | Some(UnIndentBy _) -> false
                    | _ -> true

            if lastClauseIsSingleLine then
                ctxAfterMatch
            else
                autoParenthesisIfExpressionExceedsPageWidth (genExpr node.LeftHandSide) ctx
        | lhsExpr -> genExpr lhsExpr ctx

    atCurrentColumn (
        genLhs
        +> sepNln
        +> genSingleTextNode node.Operator
        +> sepNlnWhenWriteBeforeNewlineNotEmpty
        +> sepSpace
        +> genExprInMultilineInfixExpr node.RightHandSide
    )

let genExprInMultilineInfixExpr (e: Expr) =
    match e with
    // | LetOrUses (xs, e) ->
    //     atCurrentColumn (
    //         col sepNln xs (fun (lb, inKeyword) ->
    //             genSynBinding lb
    //             +> (match inKeyword with
    //                 | Some inKw -> genTriviaFor SynExpr_LetOrUse_In inKw !- " in"
    //                 | None -> !- " in"))
    //         +> sepNln
    //         +> expressionFitsOnRestOfLine
    //             (genExpr e)
    //             (let t, r = synExprToFsAstType e in
    //
    //              sepNlnConsideringTriviaContentBeforeFor t r +> genExpr e)
    //     )
    // | Paren (lpr, (Match _ as mex), rpr, pr) ->
    //     fun ctx ->
    //         if ctx.Config.MultiLineLambdaClosingNewline then
    //             (sepOpenTFor lpr
    //              +> indentSepNlnUnindent (genExpr mex)
    //              +> sepNln
    //              +> sepCloseTFor rpr
    //              |> genTriviaFor SynExpr_Paren pr)
    //                 ctx
    //         else
    //             (sepOpenTFor lpr +> atCurrentColumnIndent (genExpr mex) +> sepCloseTFor rpr
    //              |> genTriviaFor SynExpr_Paren pr)
    //                 ctx
    // | Paren (_, InfixApp (_, _, DotGet _, _, _), _, _)
    // | Paren (_, DotGetApp _, _, _) -> atCurrentColumnIndent (genExpr e)
    | Expr.MatchLambda matchLambdaNode ->
        genSingleTextNode matchLambdaNode.Function
        +> indentSepNlnUnindent (genClauses matchLambdaNode.Clauses)
        |> genNode matchLambdaNode
    // | Record _ -> atCurrentColumnIndent (genExpr e)
    | _ -> genExpr e

let genKeepIdent (startNode: Node) (e: Expr) ctx =
    let exprNode = Expr.Node e

    if
        ctx.Config.ExperimentalKeepIndentInBranch
        && startNode.Range.StartColumn = exprNode.Range.StartColumn
    then
        sepNlnUnlessContentBefore exprNode ctx
    else
        indent ctx

let genGenericTypeParametersAux lessThan typeParameters greaterThan =
    genSingleTextNode lessThan
    +> coli sepComma typeParameters (fun idx t ->
        let leadingSpace =
            match t with
            | Type.Var n when idx = 0 && n.Text.StartsWith("^") -> sepSpace
            | _ -> sepNone

        leadingSpace +> genType t)
    +> indentIfNeeded sepNone
    +> genSingleTextNode greaterThan

let genGenericTypeParameters (typeApp: ExprTypeAppNode) =
    genGenericTypeParametersAux typeApp.LessThan typeApp.TypeParameters typeApp.GreaterThan

let genFunctionNameWithMultilineLids (trailing: Context -> Context) (longIdent: IdentListNode) (parentNode: Node) =
    match longIdent.Content with
    | IdentifierOrDot.Ident identNode :: t ->
        genSingleTextNode identNode
        +> indentSepNlnUnindent (
            colEx
                (function
                | IdentifierOrDot.Ident _ -> sepNone
                | IdentifierOrDot.KnownDot _
                | IdentifierOrDot.UnknownDot _ -> sepNln)
                t
                (function
                | IdentifierOrDot.Ident identNode -> genSingleTextNode identNode
                | IdentifierOrDot.KnownDot _
                | IdentifierOrDot.UnknownDot _ -> sepDot)
            +> trailing
        )
    | _ -> sepNone
    |> genNode parentNode

let genAppWithLambda sep (node: ExprAppWithLambdaNode) =
    let short =
        genExpr node.FunctionName
        +> sep
        +> col sepSpace node.Arguments genExpr
        +> onlyIf (List.isNotEmpty node.Arguments) sepSpace
        +> (genSingleTextNode node.OpeningParen
            +> (match node.Lambda with
                | Choice1Of2 lambdaNode -> genLambda lambdaNode |> genNode lambdaNode
                | Choice2Of2 matchLambdaNode ->
                    genSingleTextNode matchLambdaNode.Function
                    +> indentSepNlnUnindent (genClauses matchLambdaNode.Clauses)
                    |> genNode matchLambdaNode)
            +> sepNlnWhenWriteBeforeNewlineNotEmpty
            +> genSingleTextNode node.ClosingParen)

    let long (ctx: Context) : Context =
        if ctx.Config.MultiLineLambdaClosingNewline then
            let genArguments =
                match node.Arguments with
                | [] ->
                    match node.Lambda with
                    | Choice1Of2 lambdaNode ->
                        genSingleTextNode node.OpeningParen
                        +> (genLambda lambdaNode |> genNode lambdaNode)
                        +> sepNln
                        +> genSingleTextNode node.ClosingParen
                    | Choice2Of2 matchLambdaNode ->
                        genSingleTextNode node.OpeningParen
                        +> indentSepNlnUnindent (
                            genSingleTextNode matchLambdaNode.Function
                            +> sepNln
                            +> genClauses matchLambdaNode.Clauses
                            |> genNode matchLambdaNode
                        )
                        +> sepNln
                        +> genSingleTextNode node.ClosingParen
                | es ->
                    col sepNln es genExpr
                    +> sepNln
                    +> (match node.Lambda with
                        | Choice1Of2 lambdaNode ->
                            leadingExpressionIsMultiline
                                (genSingleTextNode node.OpeningParen
                                 +> (genLambda lambdaNode |> genNode lambdaNode))
                                (fun isMultiline -> onlyIf isMultiline sepNln +> genSingleTextNode node.ClosingParen)
                        | Choice2Of2 matchLambdaNode ->
                            (genSingleTextNode node.OpeningParen
                             +> (genSingleTextNode matchLambdaNode.Function
                                 +> sepNln
                                 +> genClauses matchLambdaNode.Clauses
                                 |> genNode matchLambdaNode)
                             +> sepNln
                             +> genSingleTextNode node.ClosingParen))
                    +> unindent

            (genExpr node.FunctionName
             +> ifElse (List.isEmpty node.Arguments) sep (indent +> sepNln)
             +> genArguments)
                ctx
        else
            match node.Lambda with
            | Choice1Of2 lambdaNode ->
                let singleLineTestExpr =
                    genExpr node.FunctionName
                    +> sep
                    +> col sepSpace node.Arguments genExpr
                    +> sep
                    +> genSingleTextNode node.OpeningParen
                    +> enterNode lambdaNode
                    +> genSingleTextNode lambdaNode.Fun
                    +> col sepSpace lambdaNode.Parameters genPat
                    +> sepSpace
                    +> genSingleTextNode lambdaNode.Arrow

                let singleLine =
                    genExpr node.FunctionName
                    +> sep
                    +> col sepSpace node.Arguments genExpr
                    +> sep
                    +> genSingleTextNode node.OpeningParen
                    +> (genLambda lambdaNode |> genNode lambdaNode)
                    +> sepNlnWhenWriteBeforeNewlineNotEmpty
                    +> genSingleTextNode node.ClosingParen

                let multiLine =
                    genExpr node.FunctionName
                    +> indentSepNlnUnindent (
                        col sepNln node.Arguments genExpr
                        +> onlyIfNot (List.isEmpty node.Arguments) sepNln
                        +> genSingleTextNode node.OpeningParen
                        +> (genLambda lambdaNode |> genNode lambdaNode)
                        +> genSingleTextNode node.ClosingParen
                    )

                if futureNlnCheck singleLineTestExpr ctx then
                    multiLine ctx
                else
                    singleLine ctx

            | Choice2Of2 matchLambdaNode ->
                let singleLineTestExpr =
                    genExpr node.FunctionName
                    +> sep
                    +> col sepSpace node.Arguments genExpr
                    +> genSingleTextNode node.OpeningParen
                    +> enterNode matchLambdaNode
                    +> genSingleTextNode matchLambdaNode.Function

                let singleLine =
                    genExpr node.FunctionName
                    +> sep
                    +> col sepSpace node.Arguments genExpr
                    +> sepSpace
                    +> genSingleTextNode node.OpeningParen
                    +> (genSingleTextNode matchLambdaNode.Function
                        +> indentSepNlnUnindent (genClauses matchLambdaNode.Clauses)
                        |> genNode matchLambdaNode)
                    +> sepNlnWhenWriteBeforeNewlineNotEmpty
                    +> genSingleTextNode node.ClosingParen

                let multiLine =
                    genExpr node.FunctionName
                    +> indentSepNlnUnindent (
                        col sepNln node.Arguments genExpr
                        +> sepNln
                        +> genSingleTextNode node.OpeningParen
                        +> atCurrentColumn (
                            genSingleTextNode matchLambdaNode.Function
                            +> sepNln
                            +> genClauses matchLambdaNode.Clauses
                            |> genNode matchLambdaNode
                        )
                        +> genSingleTextNode node.ClosingParen
                    )

                if futureNlnCheck singleLineTestExpr ctx then
                    multiLine ctx
                else
                    singleLine ctx

    expressionFitsOnRestOfLine short long |> genNode node

let sepSpaceBeforeParenInFuncInvocation (functionExpr: Expr) (argExpr: Expr) ctx =
    match functionExpr, argExpr with
    | Expr.Constant _, _ -> sepSpace ctx
    | ParenExpr _, _ -> sepSpace ctx
    | UppercaseExpr, ParenExpr _ -> onlyIf ctx.Config.SpaceBeforeUppercaseInvocation sepSpace ctx
    | LowercaseExpr, ParenExpr _ -> onlyIf ctx.Config.SpaceBeforeLowercaseInvocation sepSpace ctx
    | Expr.Ident _, Expr.Ident _ -> sepSpace ctx
    | _ -> sepSpace ctx

let genSpaceBeforeLids
    (currentIndex: int)
    (lastEsIndex: int)
    (lids: IdentListNode)
    (arg: Expr)
    (ctx: Context)
    : Context =
    let config =
        match lids.Content with
        | IdentifierOrDot.Ident h :: _ ->
            let s = h.Text

            if Char.IsUpper(s.[0]) then
                ctx.Config.SpaceBeforeUppercaseInvocation
            else
                ctx.Config.SpaceBeforeLowercaseInvocation
        | _ -> ctx.Config.SpaceBeforeUppercaseInvocation

    let hasParenthesis =
        match arg with
        | ParenExpr _ -> true
        | _ -> false

    if (lastEsIndex = currentIndex) && (not hasParenthesis || config) then
        sepSpace ctx
    else
        ctx
// end expressions

let genPatLeftMiddleRight (node: PatLeftMiddleRight) =
    genPat node.LeftHandSide
    +> sepSpace
    +> (match node.Middle with
        | Choice1Of2 node -> genSingleTextNode node
        | Choice2Of2 text -> !-text)
    +> sepSpace
    +> genPat node.RightHandSide

let genTyparDecl (isFirstTypeParam: bool) (td: TyparDeclNode) =
    genOnelinerAttributes td.Attributes
    +> onlyIf (isFirstTypeParam && td.TypeParameter.Text.StartsWith("^")) sepSpace
    +> genSingleTextNode td.TypeParameter
    |> genNode td

let genTyparDecls (td: TyparDecls) =
    match td with
    | TyparDecls.PostfixList node ->
        genSingleTextNode node.LessThan
        +> coli sepComma node.Decls (fun i -> genTyparDecl (i = 0))
        +> onlyIf (List.isNotEmpty node.Constraints) (sepSpace +> genTypeConstraints node.Constraints)
        +> genSingleTextNode node.GreaterThan
        |> genNode node
    | TyparDecls.PrefixList node ->
        genSingleTextNode node.OpeningParen
        +> coli sepComma node.Decls (fun i -> genTyparDecl (i = 0))
        +> genSingleTextNode node.ClosingParen
        |> genNode node
    | TyparDecls.SinglePrefix node -> genTyparDecl true node

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
    | Pattern.NamedParenStarIdent node ->
        genAccessOpt node.Accessibility
        +> genSingleTextNode node.OpeningParen
        +> sepSpace
        +> genSingleTextNode node.Name
        +> sepSpace
        +> genSingleTextNode node.ClosingParen
    | Pattern.Named node -> genAccessOpt node.Accessibility +> genSingleTextNode node.Name
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

let genPatInClause (pat: Pattern) =
    let rec genPatMultiline p =
        match p with
        | Pattern.Or p ->
            let genBar =
                match p.Middle with
                | Choice1Of2 barNode -> genSingleTextNode barNode +> sepSpace
                | Choice2Of2 _ -> sepBar

            genPatMultiline p.LeftHandSide +> sepNln +> genBar +> genPat p.RightHandSide

        | Pattern.As p ->
            let genAs =
                match p.Middle with
                | Choice1Of2 asNode -> genSingleTextNode asNode
                | Choice2Of2 _ -> !- "as"

            genPatMultiline p.LeftHandSide
            +> sepSpace
            +> genAs
            +> sepSpace
            +> genPat p.RightHandSide

        | p -> genPat p

    genPatMultiline pat

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

let genReturnTypeBinding (node: BindingReturnInfoNode option) =
    match node with
    | None -> sepNone
    | Some node -> genSingleTextNode node.Colon +> sepSpace +> genType node.Type

let clean_up_space_settings (leadingKeyword: MultipleTextsNode) ctx =
    let (|Keywords|) (mt: MultipleTextsNode) =
        List.map (fun (st: SingleTextNode) -> st.Text) mt.Content

    match leadingKeyword with
    | Keywords [ "member" ]
    | Keywords [ "override" ]
    | Keywords [ "static"; "member" ]
    | Keywords [ "abstract"; "member" ]
    | Keywords [ "default" ] -> ctx.Config.SpaceBeforeMember, ctx.Config.AlternativeLongMemberDefinitions
    | _ -> ctx.Config.SpaceBeforeParameter, ctx.Config.AlignFunctionSignatureToIndentation

let clean_up_is_rec_function (leadingKeyword: MultipleTextsNode) =
    match leadingKeyword.Content with
    | [ singleText ] -> singleText.Text = "and"
    | _ -> false

let clean_up_addSpaceBeforeParensInFunDef (spaceBeforeSetting: bool) (functionOrMethod: IdentListNode) (arg: Pattern) =
    match functionOrMethod.Content, arg with
    | [ IdentifierOrDot.Ident newIdent ], _ when newIdent.Text = "new" -> false
    | _, Pattern.Paren _
    | _, Pattern.Unit _ -> spaceBeforeSetting
    | _, Pattern.Named _
    | _, Pattern.Wild _ -> true
    | content, _ ->
        match List.tryLast content with
        | None -> false
        | Some(IdentifierOrDot.KnownDot _)
        | Some IdentifierOrDot.UnknownDot -> true
        | Some(IdentifierOrDot.Ident ident) -> not (Char.IsUpper ident.Text.[0])

let clean_up_genParenTupleWithIndentAndNewlines
    (parenNode: PatParenNode)
    (tupleNode: PatTupleNode)
    : Context -> Context =
    genSingleTextNode parenNode.OpeningParen
    +> indentSepNlnUnindent ((col (sepComma +> sepNln) tupleNode.Patterns genPat) |> genNode tupleNode)
    +> sepNln
    +> genSingleTextNode parenNode.ClosingParen
    |> genNode parenNode

let genBinding (b: BindingNode) (ctx: Context) : Context =
    let binding =
        match b.ReturnType, b.FunctionName with
        | Some returnTypeNode, Choice1Of2 functionName when List.isNotEmpty b.Parameters ->
            let spaceBefore, alternativeSyntax = clean_up_space_settings b.LeadingKeyword ctx
            let isRecursiveLetOrUseFunction = clean_up_is_rec_function b.LeadingKeyword

            let genAttrIsFirstChild =
                onlyIf (not isRecursiveLetOrUseFunction) (genAttributes b.Attributes)

            let genPref =
                if not isRecursiveLetOrUseFunction then
                    genMultipleTextsNode b.LeadingKeyword +> sepSpace
                else
                    genMultipleTextsNode b.LeadingKeyword
                    +> sepSpace
                    +> genOnelinerAttributes b.Attributes

            let afterLetKeyword =
                ifElse b.IsMutable (!- "mutable ") sepNone
                +> ifElse b.IsInline (!- "inline ") sepNone
                +> genAccessOpt b.Accessibility

            let genFunctionName =
                genIdentListNode functionName +> optSingle genTyparDecls b.GenericTypeParameters

            let genReturnType isFixed =
                onlyIfNot isFixed sepSpace
                +> (genSingleTextNode returnTypeNode.Colon
                    +> sepSpace
                    +> atCurrentColumnIndent (genType returnTypeNode.Type)
                    |> genNode returnTypeNode)

            let genSignature =
                let spaceBeforeParameters =
                    match b.Parameters with
                    | [] -> sepNone
                    | [ p ] ->
                        ifElse (clean_up_addSpaceBeforeParensInFunDef spaceBefore functionName p) sepSpace sepNone
                    | _ -> sepSpace

                let short =
                    afterLetKeyword
                    +> sepSpace
                    +> genFunctionName
                    +> spaceBeforeParameters
                    +> col sepSpace b.Parameters genPat
                    +> genReturnType false
                    +> sepSpace
                    +> genSingleTextNode b.Equals

                let long (ctx: Context) =
                    let genParameters, hasSingleTupledArg =
                        match b.Parameters with
                        | [ Pattern.Paren parenNode ] ->
                            match parenNode.Pattern with
                            | Pattern.Tuple tupleNode ->
                                clean_up_genParenTupleWithIndentAndNewlines parenNode tupleNode, true
                            | _ -> col sepNln b.Parameters genPat, false
                        | _ -> col sepNln b.Parameters genPat, false

                    (afterLetKeyword
                     +> sepSpace
                     +> genFunctionName
                     +> indent
                     +> sepNln
                     +> genParameters
                     +> onlyIf (not hasSingleTupledArg || alternativeSyntax) sepNln
                     +> leadingExpressionIsMultiline
                         (genReturnType (not hasSingleTupledArg || alternativeSyntax))
                         (fun isMultiline ->
                             ifElse (alternativeSyntax || isMultiline) (sepNln +> genSingleTextNode b.Equals) sepEq)
                     +> unindent)
                        ctx

                expressionFitsOnRestOfLine short long

            let body = genExpr b.Expr

            let genExpr isMultiline =
                if isMultiline then
                    indentSepNlnUnindent body
                else
                    let short = sepSpace +> body

                    let long =
                        autoIndentAndNlnExpressUnlessStroustrup (fun e -> sepSpace +> genExpr e) b.Expr

                    isShortExpression ctx.Config.MaxFunctionBindingWidth short long

            (genXml b.XmlDoc
             +> genAttrIsFirstChild
             +> genPref
             +> leadingExpressionIsMultiline genSignature genExpr)
        | None, Choice1Of2 functionName when List.isNotEmpty b.Parameters ->
            let spaceBefore, alternativeSyntax = clean_up_space_settings b.LeadingKeyword ctx
            let isRecursiveLetOrUseFunction = clean_up_is_rec_function b.LeadingKeyword

            let genAttrIsFirstChild =
                onlyIf (not isRecursiveLetOrUseFunction) (genAttributes b.Attributes)

            let genPref =
                if not isRecursiveLetOrUseFunction then
                    genMultipleTextsNode b.LeadingKeyword +> sepSpace
                else
                    genMultipleTextsNode b.LeadingKeyword
                    +> sepSpace
                    +> genOnelinerAttributes b.Attributes

            let afterLetKeyword =
                ifElse b.IsMutable (!- "mutable ") sepNone
                +> ifElse b.IsInline (!- "inline ") sepNone
                +> genAccessOpt b.Accessibility

            let genFunctionName =
                genIdentListNode functionName +> optSingle genTyparDecls b.GenericTypeParameters

            let genSignature =
                let spaceBeforeParameters =
                    match b.Parameters with
                    | [] -> sepNone
                    | [ p ] ->
                        ifElse (clean_up_addSpaceBeforeParensInFunDef spaceBefore functionName p) sepSpace sepNone
                    | _ -> sepSpace

                let short =
                    afterLetKeyword
                    +> genFunctionName
                    +> spaceBeforeParameters
                    +> col sepSpace b.Parameters genPat
                    +> sepSpace
                    +> genSingleTextNode b.Equals

                let long (ctx: Context) =
                    let genParameters, hasSingleTupledArg =
                        match b.Parameters with
                        | [ Pattern.Paren parenNode ] ->
                            match parenNode.Pattern with
                            | Pattern.Tuple tupleNode ->
                                clean_up_genParenTupleWithIndentAndNewlines parenNode tupleNode, true
                            | _ -> col sepNln b.Parameters genPat, false
                        | _ -> col sepNln b.Parameters genPat, false

                    (afterLetKeyword
                     +> sepSpace
                     +> genFunctionName
                     +> indent
                     +> sepNln
                     +> genParameters
                     +> ifElse (hasSingleTupledArg && not alternativeSyntax) sepSpace sepNln
                     +> genSingleTextNode b.Equals
                     +> unindent)
                        ctx

                expressionFitsOnRestOfLine short long

            let body (ctx: Context) = genExpr b.Expr ctx

            let genExpr isMultiline =
                if isMultiline then
                    indentSepNlnUnindent body
                else
                    let short = sepSpace +> body

                    let long =
                        autoIndentAndNlnExpressUnlessStroustrup (fun e -> sepSpace +> genExpr e) b.Expr

                    isShortExpression ctx.Config.MaxFunctionBindingWidth short long

            (genXml b.XmlDoc
             +> genAttrIsFirstChild
             +> genPref
             +> leadingExpressionIsMultiline genSignature genExpr)
        | None, Choice2Of2(Pattern.Tuple _ as pat) ->
            let isRecursiveLetOrUseFunction = clean_up_is_rec_function b.LeadingKeyword

            let genAttrAndPref =
                if not isRecursiveLetOrUseFunction then
                    (genAttributes b.Attributes +> genMultipleTextsNode b.LeadingKeyword)
                else
                    (genMultipleTextsNode b.LeadingKeyword
                     +> sepSpace
                     +> genOnelinerAttributes b.Attributes)

            let afterLetKeyword =
                genAccessOpt b.Accessibility
                +> ifElse b.IsMutable (!- "mutable ") sepNone
                +> ifElse b.IsInline (!- "inline ") sepNone

            let genDestructedTuples =
                expressionFitsOnRestOfLine (genPat pat) (sepOpenT +> genPat pat +> sepCloseT)

            genXml b.XmlDoc
            +> genAttrAndPref
            +> sepSpace
            +> (fun ctx ->
                let prefix =
                    afterLetKeyword
                    +> sepSpace
                    +> genDestructedTuples
                    +> sepSpace
                    +> genSingleTextNode b.Equals

                let long = prefix +> indentSepNlnUnindent (genExpr b.Expr)
                let short = prefix +> sepSpace +> genExpr b.Expr
                isShortExpression ctx.Config.MaxValueBindingWidth short long ctx)
        | _ ->
            // old code of genSynBindingValue

            let isRecursiveLetOrUseFunction = clean_up_is_rec_function b.LeadingKeyword

            let genAttrIsFirstChild =
                onlyIf (not isRecursiveLetOrUseFunction) (genAttributes b.Attributes)

            let genPref =
                if not isRecursiveLetOrUseFunction then
                    genMultipleTextsNode b.LeadingKeyword +> sepSpace
                else
                    (genMultipleTextsNode b.LeadingKeyword
                     +> sepSpace
                     +> genOnelinerAttributes b.Attributes)

            let afterLetKeyword =
                genAccessOpt b.Accessibility
                +> ifElse b.IsMutable (!- "mutable ") sepNone
                +> ifElse b.IsInline (!- "inline ") sepNone

            let genValueName =
                match b.FunctionName with
                | Choice1Of2 lid -> genIdentListNode lid
                | Choice2Of2 pat -> genPat pat
                +> optSingle genTyparDecls b.GenericTypeParameters

            let genEqualsInBinding (ctx: Context) =
                (genSingleTextNode b.Equals +> sepSpaceUnlessWriteBeforeNewlineNotEmpty) ctx

            let genReturnType =
                match b.ReturnType with
                | Some rt ->
                    let hasGenerics = b.GenericTypeParameters.IsSome

                    onlyIfCtx (fun ctx -> hasGenerics || ctx.Config.SpaceBeforeColon) sepSpace
                    +> genSingleTextNode rt.Colon
                    +> sepSpace
                    +> genType rt.Type
                    +> sepSpaceUnlessWriteBeforeNewlineNotEmpty
                    +> autoIndentAndNlnWhenWriteBeforeNewlineNotEmpty genEqualsInBinding
                | _ -> sepSpace +> genEqualsInBinding

            genXml b.XmlDoc
            +> genAttrIsFirstChild
            +> genPref
            +> (fun ctx ->
                let prefix = afterLetKeyword +> sepSpace +> genValueName +> genReturnType
                let short = prefix +> genExpr b.Expr
                let long = prefix +> autoIndentAndNlnExpressUnlessStroustrup genExpr b.Expr
                isShortExpression ctx.Config.MaxValueBindingWidth short long ctx)

    genNode b binding ctx

let genBindings withUseConfig (bs: BindingNode list) : Context -> Context =
    colWithNlnWhenNodeIsMultiline withUseConfig genBinding bs

let genOpenList (openList: OpenListNode) =
    col sepNln openList.Opens (function
        | Open.ModuleOrNamespace node -> !- "open " +> genIdentListNode node.Name |> genNode node
        | Open.Target node -> !- "open type " +> genType node.Target)
    |> genNode openList

let genTypeConstraint (tc: TypeConstraint) =
    match tc with
    | TypeConstraint.Single node ->
        genSingleTextNode node.Typar +> sepColon +> genSingleTextNode node.Kind
        |> genNode node
    | TypeConstraint.DefaultsToType node ->
        genSingleTextNode node.Default
        +> sepSpace
        +> genSingleTextNode node.Typar
        +> sepColon
        +> genType node.Type
        |> genNode node
    | TypeConstraint.SubtypeOfType node ->
        genSingleTextNode node.Typar +> !- " :> " +> genType node.Type |> genNode node
    | TypeConstraint.SupportsMember node ->
        genType node.Type
        +> sepColon
        +> sepOpenT
        +> genMemberDefn node.MemberSig
        +> sepCloseT
        |> genNode node
    | TypeConstraint.EnumOrDelegate node ->
        genSingleTextNode node.Typar
        +> sepColon
        +> !- $"{node.Verb}<"
        +> col sepComma node.Types genType
        +> !- ">"
        |> genNode node
    | TypeConstraint.WhereSelfConstrained t -> genType t

let genTypeConstraints (tcs: TypeConstraint list) =
    let short = colPre (sepSpace +> !- "when ") wordAnd tcs genTypeConstraint

    let long =
        colPre (!- "when ") (sepNln +> wordAndFixed +> sepSpace) tcs genTypeConstraint

    autoIndentAndNlnIfExpressionExceedsPageWidth (expressionFitsOnRestOfLine short long)

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

        expressionFitsOnRestOfLine short long |> genNode node
    | Type.Tuple node -> genSynTupleTypeSegments node.Path |> genNode node
    | Type.HashConstraint node -> genSingleTextNode node.Hash +> genType node.Type |> genNode node
    | Type.MeasurePower node -> genType node.BaseMeasure +> !- "^" +> !-node.Exponent |> genNode node
    | Type.StaticConstant c -> genConstant c
    | Type.StaticConstantExpr node -> genSingleTextNode node.Const +> sepSpace +> genExpr node.Expr |> genNode node
    | Type.StaticConstantNamed node ->
        genType node.Identifier
        +> !- "="
        +> addSpaceIfSynTypeStaticConstantHasAtSignBeforeString node.Value
        +> genType node.Value
        |> genNode node
    | Type.Array node ->
        genType node.Type +> !- "[" +> rep (node.Rank - 1) (!- ",") +> !- "]"
        |> genNode node
    | Type.Anon node -> genSingleTextNode node
    | Type.Var node -> genSingleTextNode node
    | Type.AppPostfix node -> genType node.First +> sepSpace +> genType node.Last |> genNode node
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
        |> genNode node
    | Type.StructTuple node ->
        genSingleTextNode node.Keyword
        +> sepSpace
        +> sepOpenT
        +> genSynTupleTypeSegments node.Path
        +> genSingleTextNode node.ClosingParen
        |> genNode node
    | Type.WithSubTypeConstraint tc -> genTypeConstraint tc
    | Type.WithGlobalConstraints node ->
        leadingExpressionIsMultiline (genType node.Type) (fun isMultiline ->
            if isMultiline then
                indentSepNlnUnindent (genTypeConstraints node.TypeConstraints)
            else
                sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidth (genTypeConstraints node.TypeConstraints))
        |> genNode node
    | Type.LongIdent idn -> genIdentListNode idn
    | Type.AnonRecord node ->
        let genStruct =
            match node.Struct with
            | None -> sepNone
            | Some n -> genSingleTextNode n +> sepSpace

        let genOpening =
            match node.Opening with
            | None -> sepOpenAnonRecdFixed +> addSpaceIfSpaceAroundDelimiter
            | Some n -> genSingleTextNode n +> addSpaceIfSpaceAroundDelimiter

        let genAnonRecordFieldType (i, t) =
            genSingleTextNode i
            +> sepColon
            +> autoIndentAndNlnIfExpressionExceedsPageWidth (genType t)

        let smallExpression =
            genStruct
            +> genOpening
            +> col sepSemi node.Fields genAnonRecordFieldType
            +> addSpaceIfSpaceAroundDelimiter
            +> genSingleTextNode node.Closing

        let longExpression =
            let genFields = col sepNln node.Fields genAnonRecordFieldType

            let genMultilineAnonRecordTypeAlignBrackets =
                let genRecord =
                    sepOpenAnonRecdFixed
                    +> indentSepNlnUnindent (atCurrentColumnIndent genFields)
                    +> sepNln
                    +> genSingleTextNode node.Closing

                genStruct +> genRecord

            let genMultilineAnonRecordType =
                let genRecord =
                    genOpening
                    +> atCurrentColumn genFields
                    +> addSpaceIfSpaceAroundDelimiter
                    +> genSingleTextNode node.Closing

                genStruct +> genRecord

            ifAlignBrackets genMultilineAnonRecordTypeAlignBrackets genMultilineAnonRecordType

        fun (ctx: Context) ->
            let size = getRecordSize ctx node.Fields
            genNode node (isSmallExpression size smallExpression longExpression) ctx

    | Type.Paren node ->
        genSingleTextNode node.OpeningParen
        +> genType node.Type
        +> genSingleTextNode node.ClosingParen
        |> genNode node
    | Type.SignatureParameter node ->
        genOnelinerAttributes node.Attributes
        +> optSingle (fun id -> genSingleTextNode id +> sepColon) node.Identifier
        +> autoIndentAndNlnIfExpressionExceedsPageWidth (genType node.Type)
        |> genNode node
    | Type.Or node ->
        genType node.LeftHandSide
        +> sepSpace
        +> genSingleTextNode node.Or
        +> sepSpace
        +> genType node.RightHandSide
        |> genNode node

let genSynTupleTypeSegments (path: Choice<Type, SingleTextNode> list) =
    let genTs addNewline =
        col sepSpace path (fun t ->
            match t with
            | Choice1Of2 t -> genType t
            | Choice2Of2 node -> genSingleTextNode node +> onlyIf addNewline sepNln)

    expressionFitsOnRestOfLine (genTs false) (genTs true)

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
    genXml typeName.XmlDoc
    +> genAttributes typeName.Attributes
    +> genSingleTextNode typeName.LeadingKeyword
    +> sepSpace
    +> genAccessOpt typeName.Accessibility
    +> genTypeAndParam (genIdentListNode typeName.Identifier) typeName.TypeParameters
    +> leadingExpressionIsMultiline
        (optSingle (fun imCtor -> sepSpaceBeforeClassConstructor +> genImplicitConstructor imCtor) implicitConstructor)
        (fun isMulti ctx ->
            if isMulti && ctx.Config.AlternativeLongMemberDefinitions then
                (optSingle genSingleTextNode typeName.EqualsToken) ctx
            else
                (sepSpace +> optSingle genSingleTextNode typeName.EqualsToken) ctx)
    |> genNode typeName

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
        genXml node.XmlDoc
        +> onlyIfNot node.Attributes.IsEmpty sepSpace
        +> genOnelinerAttributes node.Attributes
        +> onlyIf node.Accessibility.IsSome sepSpace
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
            genXml node.XmlDoc
            +> onlyIfNot node.Attributes.IsEmpty (genOnelinerAttributes node.Attributes +> sepNln)
            +> expressionFitsOnRestOfLine
                (genAccessOpt node.Accessibility +> genPats)
                (genAccessOpt node.Accessibility
                 +> optSingle (fun _ -> sepNln) node.Accessibility
                 +> genPats)
            +> (fun ctx -> onlyIf ctx.Config.AlternativeLongMemberDefinitions sepNln ctx)
        )

    expressionFitsOnRestOfLine short long
    +> optSingle (fun self -> sepSpace +> !- "as " +> genSingleTextNode self +> sepSpace) node.Self

let genTypeDefn (td: TypeDefn) =
    let typeDefnNode = TypeDefn.TypeDefnNode td
    let typeName = typeDefnNode.TypeName
    let members = typeDefnNode.Members

    let header =
        let hasAndKeyword = typeName.LeadingKeyword.Text = "and"

        genXml typeName.XmlDoc
        +> onlyIfNot hasAndKeyword (genAttributes typeName.Attributes)
        +> genSingleTextNode typeName.LeadingKeyword
        +> onlyIf hasAndKeyword (sepSpace +> genOnelinerAttributes typeName.Attributes)
        +> sepSpace
        +> genAccessOpt typeName.Accessibility
        +> genTypeAndParam (genIdentListNode typeName.Identifier) typeName.TypeParameters
        +> sepSpace
        +> genTypeConstraints typeName.Constraints
        +> onlyIfNot typeName.Constraints.IsEmpty sepSpace
        +> optSingle genSingleTextNode typeName.EqualsToken
        |> genNode typeName

    match td with
    | TypeDefn.Enum node ->
        let genEnumCase (node: EnumCaseNode) =
            genXml node.XmlDoc
            +> (match node.Bar with
                | None -> sepBar
                | Some bar -> genSingleTextNode bar +> sepSpace)
            +> genOnelinerAttributes node.Attributes
            +> genSingleTextNode node.Identifier
            +> sepSpace
            +> genSingleTextNode node.Equals
            +> autoIndentAndNlnWhenWriteBeforeNewlineNotEmpty (sepSpace +> genConstant node.Constant)
            |> genNode node

        header
        +> indentSepNlnUnindent (
            col sepNln node.EnumCases genEnumCase
            +> onlyIf (List.isNotEmpty members) sepNln
            +> sepNlnTypeAndMembers typeDefnNode
            +> genMemberDefnList members
        )
        |> genNode node
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
        +> onlyIf
            (List.isNotEmpty members)
            (indentSepNlnUnindent (sepNlnTypeAndMembers typeDefnNode +> genMemberDefnList members))
        |> genNode node
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
                 +> genSingleTextNodeSuffixDelimiter node.OpeningBrace
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

        header +> genTypeDefinition |> genNode node
    | TypeDefn.None _ -> header
    | TypeDefn.Abbrev node ->
        header
        +> sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidth (genType node.Type)
        +> onlyIf
            (List.isNotEmpty members)
            (optSingle
                (fun withNode ->
                    indentSepNlnUnindent (
                        genSingleTextNode withNode +> indentSepNlnUnindent (genMemberDefnList members)
                    ))
                typeName.WithKeyword)
        |> genNode node
    | TypeDefn.Explicit node ->
        let bodyNode = node.Body

        let additionMembers =
            match members with
            | [] -> sepNone
            | h :: _ ->
                sepNlnUnlessContentBefore (MemberDefn.Node h)
                +> indentSepNlnUnindent (genMemberDefnList members)

        genTypeWithImplicitConstructor typeName node.ImplicitConstructor
        +> indentSepNlnUnindent (
            genSingleTextNode bodyNode.Kind
            +> onlyIfNot bodyNode.Members.IsEmpty (indentSepNlnUnindent (genMemberDefnList bodyNode.Members))
            +> sepNln
            +> genSingleTextNode bodyNode.End
            |> genNode bodyNode
        )
        +> additionMembers
        |> genNode node
    | TypeDefn.Augmentation node ->
        header
        +> sepSpace
        +> optSingle genSingleTextNode typeName.WithKeyword
        +> indentSepNlnUnindent (sepNlnTypeAndMembers typeDefnNode +> genMemberDefnList members)
        |> genNode node
    | TypeDefn.Delegate node ->
        header
        +> sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidth (
            genSingleTextNode node.DelegateNode
            +> sepSpace
            +> !- "of"
            +> sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidth (genTypeList node.TypeList)
        )
        |> genNode node
    | TypeDefn.Regular node ->
        genTypeWithImplicitConstructor typeName node.ImplicitConstructor
        +> indentSepNlnUnindent (genMemberDefnList members)
        |> genNode (TypeDefn.Node td)

let genTypeList (node: TypeFunsNode) =
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

let genTypeInSignature (t: Type) =
    match t with
    | Type.WithGlobalConstraints node ->
        match node.Type with
        | Type.Funs funsNode ->
            let genConstraints =
                let short =
                    ifElse (List.isNotEmpty node.TypeConstraints) (!- "when ") sepSpace
                    +> col wordAnd node.TypeConstraints genTypeConstraint

                let long =
                    ifElse (List.isNotEmpty node.TypeConstraints) (!- "when ") sepSpace
                    +> col (sepNln +> wordAndFixed +> sepSpace) node.TypeConstraints genTypeConstraint

                expressionFitsOnRestOfLine short long

            autoIndentAndNlnIfExpressionExceedsPageWidth (
                leadingExpressionIsMultiline (genTypeList funsNode) (fun isMultiline ->
                    if isMultiline then
                        indentSepNlnUnindent genConstraints
                    else
                        sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidth genConstraints)
            )
        | _ -> autoIndentAndNlnIfExpressionExceedsPageWidth (genType t)
    | Type.Funs funsNode -> autoIndentAndNlnIfExpressionExceedsPageWidth (genTypeList funsNode)
    | _ -> autoIndentAndNlnIfExpressionExceedsPageWidth (genType t)

let genField (node: FieldNode) =
    genXml node.XmlDoc
    +> genAttributes node.Attributes
    +> optSingle (fun lk -> genMultipleTextsNode lk +> sepSpace) node.LeadingKeyword
    +> onlyIf node.IsMutable (!- "mutable ")
    +> genAccessOpt node.Accessibility
    +> (match node.Name with
        | None -> genType node.Type
        | Some name ->
            genSingleTextNode name
            +> sepColon
            +> autoIndentAndNlnIfExpressionExceedsPageWidth (genType node.Type))
    |> genNode node

let genUnionCase (hasVerticalBar: bool) (node: UnionCaseNode) =
    let shortExpr = col sepStar node.Fields genField

    let longExpr =
        indentSepNlnUnindent (atCurrentColumn (col (sepStar +> sepNln) node.Fields genField))

    let genBar =
        match node.Bar with
        | Some bar -> ifElse hasVerticalBar (genSingleTextNodeWithSpaceSuffix sepSpace bar) (genNode bar sepNone)
        | None -> onlyIf hasVerticalBar sepBar

    genXml node.XmlDoc
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

let genTypeAndParam (genTypeName: Context -> Context) (tds: TyparDecls option) =
    match tds with
    | None -> genTypeName
    | Some(TyparDecls.PostfixList _) -> genTypeName +> optSingle genTyparDecls tds
    | Some(TyparDecls.PrefixList _) -> optSingle (fun tds -> genTyparDecls tds +> sepSpace) tds +> genTypeName
    | Some(TyparDecls.SinglePrefix singlePrefixNode) -> genTyparDecl true singlePrefixNode +> sepSpace +> genTypeName

let genVal (node: ValNode) (optGetSet: MultipleTextsNode option) =
    let genOptExpr =
        match node.Equals, node.Expr with
        | Some eq, Some e -> sepSpace +> genSingleTextNode eq +> sepSpace +> genExpr e
        | _ -> sepNone

    genXml node.XmlDoc
    +> genAttributes node.Attributes
    +> optSingle (fun lk -> genMultipleTextsNode lk +> sepSpace) node.LeadingKeyword
    +> onlyIf node.IsInline (!- "inline ")
    +> onlyIf node.IsMutable (!- "mutable ")
    +> genAccessOpt node.Accessibility
    +> genTypeAndParam (genSingleTextNode node.Identifier) node.TypeParams
    +> ifElse (Option.isSome node.TypeParams) sepColonWithSpacesFixed sepColon
    +> genTypeInSignature node.Type
    +> optSingle (fun gs -> sepSpace +> genMultipleTextsNode gs) optGetSet
    +> genOptExpr
    |> genNode node

let genMemberDefnList mds =
    match mds with
    | [] -> sepNone
    | _ -> colWithNlnWhenMappedNodeIsMultiline false MemberDefn.Node genMemberDefn mds

let genMemberDefn (md: MemberDefn) =
    match md with
    | MemberDefn.ImplicitInherit ic ->
        genSingleTextNode ic.InheritKeyword
        +> sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidth (genInheritConstructor ic)
        |> genNode (InheritConstructor.Node ic)
    | MemberDefn.Inherit node -> genSingleTextNode node.Inherit +> sepSpace +> genType node.BaseType
    | MemberDefn.ValField node -> genField node
    | MemberDefn.Member node -> genBinding node
    | MemberDefn.ExternBinding _ -> failwithf "todo %A" md
    | MemberDefn.DoExpr node -> genExpr (Expr.Single node)
    | MemberDefn.ExplicitCtor node ->
        genXml node.XmlDoc
        +> genAccessOpt node.Accessibility
        +> genSingleTextNode node.New
        +> sepSpaceBeforeClassConstructor
        +> genPat node.Pattern
        +> optSingle (fun alias -> sepSpace +> !- "as" +> sepSpace +> genSingleTextNode alias) node.Alias
        +> sepSpace
        +> genSingleTextNode node.Equals
        +> sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidth (
            genExpr node.Expr
            +> optSingle
                (fun thenExpr ->
                    sepNln
                    +> !- "then"
                    +> sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidth (genExpr thenExpr))
                node.ThenExpr
        )
        |> genNode (MemberDefn.Node md)
    | MemberDefn.LetBinding node -> genBindings true node.Bindings |> genNode (MemberDefn.Node md)
    | MemberDefn.Interface node ->
        genSingleTextNode node.Interface
        +> sepSpace
        +> genType node.Type
        +> optSingle
            (fun withNode ->
                sepSpace
                +> genSingleTextNode withNode
                +> indentSepNlnUnindent (genMemberDefnList node.Members))
            node.With
        |> genNode (MemberDefn.Node md)
    | MemberDefn.AutoProperty node ->
        genXml node.XmlDoc
        +> genAttributes node.Attributes
        +> genMultipleTextsNode node.LeadingKeyword
        +> sepSpace
        +> genAccessOpt node.Accessibility
        +> genSingleTextNode node.Identifier
        +> optPre sepColon sepNone node.Type genType
        +> sepSpace
        +> genSingleTextNode node.Equals
        +> sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidth (
            genExpr node.Expr
            +> optSingle (fun gs -> sepSpace +> genMultipleTextsNode gs) node.WithGetSet
        )
        |> genNode (MemberDefn.Node md)
    | MemberDefn.AbstractSlot node ->
        genXml node.XmlDoc
        +> genAttributes node.Attributes
        +> genMultipleTextsNode node.LeadingKeyword
        +> sepSpace
        +> genSingleTextNode node.Identifier
        +> optSingle genTyparDecls node.TypeParams
        +> ifElse node.TypeParams.IsSome sepColonWithSpacesFixed sepColon
        +> genTypeInSignature node.Type
        +> optSingle (fun gs -> sepSpace +> genMultipleTextsNode gs) node.WithGetSet
        |> genNode (MemberDefn.Node md)
    | MemberDefn.PropertyGetSet node ->
        let genProperty (node: PropertyGetSetBindingNode) =
            genAccessOpt node.Accessibility
            +> genSingleTextNode node.LeadingKeyword
            +> sepSpace
            +> col sepSpace node.Parameters genPat
            +> genReturnTypeBinding node.ReturnType
            +> sepSpace
            +> genSingleTextNode node.Equals
            +> sepSpaceOrIndentAndNlnIfExpressionExceedsPageWidth (genExpr node.Expr)

        genXml node.XmlDoc
        +> genAttributes node.Attributes
        +> genMultipleTextsNode node.LeadingKeyword
        +> sepSpace
        +> onlyIf node.IsInline (!- "inline ")
        +> genAccessOpt node.Accessibility
        +> genIdentListNode node.MemberName
        +> indent
        +> sepNln
        +> genSingleTextNode node.WithKeyword
        +> sepSpace
        +> genProperty node.FirstBinding
        +> optSingle (fun a -> sepNln +> genSingleTextNode a +> sepSpace) node.AndKeyword
        +> optSingle genProperty node.LastBinding
        +> unindent
        |> genNode (MemberDefn.Node md)
    | MemberDefn.SigMember node -> genVal node.Val node.WithGetSet |> genNode node

let genException (node: ExceptionDefnNode) =
    genXml node.XmlDoc
    +> genAttributes node.Attributes
    +> !- "exception "
    +> genAccessOpt node.Accessibility
    +> genUnionCase false node.UnionCase
    +> onlyIf
        (not node.Members.IsEmpty)
        (sepSpace
         +> optSingle genSingleTextNode node.WithKeyword
         +> indentSepNlnUnindent (genMemberDefnList node.Members))
    |> genNode node

let genModuleDecl (md: ModuleDecl) =
    match md with
    | ModuleDecl.OpenList ol -> genOpenList ol
    | ModuleDecl.HashDirectiveList node -> col sepNln node.HashDirectives genParsedHashDirective |> genNode node
    | ModuleDecl.Attributes node -> genAttributes node.Attributes +> genExpr node.Expr |> genNode node
    | ModuleDecl.DeclExpr e -> genExpr e
    | ModuleDecl.Exception node -> genException node
    | ModuleDecl.ExternBinding _ -> failwith "Not Implemented"
    | ModuleDecl.TopLevelBinding b -> genBinding b
    | ModuleDecl.ModuleAbbrev node ->
        genSingleTextNode node.Module
        +> sepSpace
        +> genSingleTextNode node.Name
        +> sepSpace
        +> sepEqFixed
        +> sepSpace
        +> genIdentListNode node.Alias
        |> genNode (ModuleDecl.Node md)
    | ModuleDecl.NestedModule node ->
        genXml node.XmlDoc
        +> genAttributes node.Attributes
        +> genSingleTextNode node.Module
        +> sepSpace
        +> genAccessOpt node.Accessibility
        +> onlyIf node.IsRecursive (sepSpace +> !- "rec" +> sepSpace)
        +> genIdentListNode node.Identifier
        +> sepSpace
        +> genSingleTextNode node.Equals
        +> indentSepNlnUnindent (
            colWithNlnWhenMappedNodeIsMultiline false ModuleDecl.Node genModuleDecl node.Declarations
        )
        |> genNode (ModuleDecl.Node md)
    | ModuleDecl.TypeDefn td -> genTypeDefn td
    | ModuleDecl.Val node -> genVal node None

let sepNlnUnlessContentBefore (node: Node) =
    if Seq.isEmpty node.ContentBefore then sepNln else sepNone

let colWithNlnWhenMappedNodeIsMultiline<'n>
    (withUseConfig: bool)
    (mapNode: 'n -> Node)
    (f: 'n -> Context -> Context)
    (nodes: 'n list)
    : Context -> Context =
    nodes
    |> List.map (fun n -> ColMultilineItem(f n, (mapNode >> sepNlnUnlessContentBefore) n))
    |> (if withUseConfig then
            colWithNlnWhenItemIsMultiline
        else
            colWithNlnWhenItemIsMultilineUsingConfig)

let colWithNlnWhenNodeIsMultiline<'n when 'n :> Node>
    (withUseConfig: bool)
    (f: 'n -> Context -> Context)
    (nodes: 'n list)
    : Context -> Context =
    colWithNlnWhenMappedNodeIsMultiline<'n> withUseConfig (fun n -> n :> Node) f nodes

let genModule (m: ModuleOrNamespaceNode) =
    let newline =
        match m.Declarations with
        | [] -> onlyIfNot (Seq.isEmpty (m :> Node).ContentAfter) sepNln
        | h :: _ -> sepNln +> sepNlnUnlessContentBefore (ModuleDecl.Node h)

    onlyIf
        m.IsNamed
        (optSingle
            (fun (n: MultipleTextsNode) ->
                genXml m.XmlDoc
                +> genAttributes m.Attributes
                +> genMultipleTextsNode n
                +> sepSpace
                +> genAccessOpt m.Accessibility
                +> onlyIf m.IsRecursive (sepSpace +> !- "rec" +> sepSpace)
                +> genIdentListNode m.Name)
            m.LeadingKeyword
         +> newline)
    +> colWithNlnWhenMappedNodeIsMultiline false ModuleDecl.Node genModuleDecl m.Declarations
    |> genNode m

let addFinalNewline ctx =
    let lastEvent = ctx.WriterEvents.TryHead

    match lastEvent with
    | Some WriteLineBecauseOfTrivia ->
        if ctx.Config.InsertFinalNewline then
            ctx
        else
            // Due to trivia the last event is a newline, if insert_final_newline is false, we need to remove it.
            { ctx with
                WriterEvents = ctx.WriterEvents.Tail
                WriterModel = { ctx.WriterModel with Lines = List.tail ctx.WriterModel.Lines } }
    | _ -> onlyIf ctx.Config.InsertFinalNewline sepNln ctx

let genFile (oak: Oak) =
    (col sepNln oak.ParsedHashDirectives genParsedHashDirective
     +> (if oak.ParsedHashDirectives.IsEmpty then sepNone else sepNln)
     +> col sepNln oak.ModulesOrNamespaces genModule
     |> genNode oak)
    +> addFinalNewline
