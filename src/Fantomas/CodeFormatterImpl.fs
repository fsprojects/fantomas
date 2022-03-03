[<RequireQualifiedAccess>]
module internal Fantomas.CodeFormatterImpl

open System
open System.Diagnostics
open System.Text.RegularExpressions
//open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Diagnostics
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Compiler.Text.Position
open FSharp.Compiler.Text.Range
//open FSharp.Compiler.Tokenization
open Fantomas
open Fantomas.FormatConfig
open Fantomas.SourceOrigin
open Fantomas.SourceParser
open Fantomas.TriviaTypes
open Fantomas.CodePrinter

// Share an F# checker instance across formatting calls
//let sharedChecker = lazy (FSharpChecker.Create())

//let createParsingOptionsFromFile fileName =
//    { FSharpParsingOptions.Default with
//        SourceFiles = [| fileName |]
//        IsExe = true
//        LangVersionText = "preview" }

//let private getSourceString (source: SourceOrigin) =
//    match source with
//    | SourceString s -> String.normalizeNewLine s
//    | SourceText sourceText ->
//        let totalLines = sourceText.GetLineCount()
//
//        [ 0 .. (totalLines - 1) ]
//        |> List.map sourceText.GetLineString
//        |> String.concat "\n"


let getSourceText (source: string) : ISourceText = source.TrimEnd() |> SourceText.ofString

let parse (isSignature: bool) (source: ISourceText) : Async<(ParsedInput * TokenWithRangeList * DefineCombination) []> =
    let tokens, defineCombinations = TokenParser.getDefineCombination source

    defineCombinations
    |> List.map (fun defineCombination ->
        async {
            let untypedTree, diagnostics =
                Fantomas.FCS.Parse.parseFile isSignature source defineCombination

            let errors =
                diagnostics
                |> List.filter (fun (_, s) -> s = FSharpDiagnosticSeverity.Error)

            if not errors.IsEmpty then
                raise (FormatException $"Parsing failed with errors: %A{diagnostics}\nAnd options: %A{[]}")

            return (untypedTree, tokens, defineCombination) // , defineHashTokens)
        })
    |> Async.Parallel

/// Check whether an AST consists of parsing errors
//let isValidAST ast =
//    let rec validateImplFileInput (SourceParser.ParsedImplFileInput (_, moduleOrNamespaceList)) =
//        List.forall validateModuleOrNamespace moduleOrNamespaceList
//
//    and validateModuleOrNamespace (SynModuleOrNamespace (decls = decls)) = List.forall validateModuleDecl decls
//
//    and validateModuleDecl (decl: SynModuleDecl) =
//        match decl with
//        | SynModuleDecl.Exception(exnDefn = SynExceptionDefn (members = synMembers)) ->
//            List.forall validateMemberDefn synMembers
//        | SynModuleDecl.Let (_isRecursive, bindings, _range) -> List.forall validateBinding bindings
//        | SynModuleDecl.ModuleAbbrev (_lhs, _rhs, _range) -> true
//        | SynModuleDecl.NamespaceFragment fragment -> validateModuleOrNamespace fragment
//        | SynModuleDecl.NestedModule (decls = decls) -> List.forall validateModuleDecl decls
//        | SynModuleDecl.Types (typeDefs, _range) -> List.forall validateTypeDefn typeDefs
//        | SynModuleDecl.DoExpr (_, expr, _) -> validateExpr expr
//        | SynModuleDecl.Attributes _
//        | SynModuleDecl.HashDirective _
//        | SynModuleDecl.Open _ -> true
//
//    and validateTypeDefn (SynTypeDefn (typeRepr = representation; members = members)) =
//        validateTypeDefnRepr representation
//        && List.forall validateMemberDefn members
//
//    and validateTypeDefnRepr (typeDefnRepr: SynTypeDefnRepr) =
//        match typeDefnRepr with
//        | SynTypeDefnRepr.ObjectModel (_kind, members, _range) -> List.forall validateMemberDefn members
//        | SynTypeDefnRepr.Simple (repr, _range) ->
//            match repr with
//            | SynTypeDefnSimpleRepr.Union (_, cases, _) -> not (List.isEmpty cases)
//            | SynTypeDefnSimpleRepr.Enum (cases, _) -> not (List.isEmpty cases)
//            | SynTypeDefnSimpleRepr.Record (_, fields, _) -> not (List.isEmpty fields)
//            | SynTypeDefnSimpleRepr.General (_, types, _, _, _, _, _, _) -> not (List.isEmpty types)
//            | SynTypeDefnSimpleRepr.LibraryOnlyILAssembly _
//            | SynTypeDefnSimpleRepr.TypeAbbrev _
//            | SynTypeDefnSimpleRepr.Exception _
//            | SynTypeDefnSimpleRepr.None _ -> true
//        | SynTypeDefnRepr.Exception _ -> true
//
//    and validateMemberDefn (memberDefn: SynMemberDefn) =
//        match memberDefn with
//        | SynMemberDefn.AbstractSlot (_synValSig, _memberFlags, _range) -> true
//        | SynMemberDefn.AutoProperty (synExpr = expr) -> validateExpr expr
//        | SynMemberDefn.Interface (members = members) ->
//            defaultArg (Option.map (List.forall validateMemberDefn) members) true
//        | SynMemberDefn.Member (binding, _range) -> validateBinding binding
//        | SynMemberDefn.NestedType (typeDef, _access, _range) -> validateTypeDefn typeDef
//        | SynMemberDefn.ValField (_field, _range) -> true
//        | SynMemberDefn.LetBindings (bindings, _isStatic, _isRec, _range) -> List.forall validateBinding bindings
//        | SynMemberDefn.Open _
//        | SynMemberDefn.Inherit _
//        | SynMemberDefn.ImplicitCtor _ -> true
//        | SynMemberDefn.ImplicitInherit (_, expr, _, _) -> validateExpr expr
//
//    and validateBinding (SynBinding (headPat = headPat; expr = expr)) =
//        validateExpr expr && validatePattern headPat
//
//    and validateClause (Clause (pat, exprOpt, _, expr)) =
//        validatePattern pat
//        && validateExpr expr
//        && defaultArg (Option.map validateExpr exprOpt) true
//
//    and validateExpr =
//        function
//        | SynExpr.Quote (synExpr1, _, synExpr2, _, _range) -> List.forall validateExpr [ synExpr1; synExpr2 ]
//
//        | SynExpr.Const (_synConst, _range) -> true
//
//        | SynExpr.Paren (synExpr, _, _, _parenRange) -> validateExpr synExpr
//        | SynExpr.Typed (synExpr, _synType, _range) -> validateExpr synExpr
//
//        | SynExpr.Tuple (_, synExprList, _, _range)
//        | SynExpr.ArrayOrList (_, synExprList, _range) -> List.forall validateExpr synExprList
//
//        | SynExpr.Record (_inheritOpt, _copyOpt, fields, _range) ->
//            List.forall (fun (SynExprRecordField (expr = e)) -> defaultArg (Option.map validateExpr e) true) fields
//        | SynExpr.AnonRecd (_inheritOpt, _copyOpt, fields, _range) ->
//            List.forall (fun (_, _, e) -> validateExpr e) fields
//
//        | SynExpr.New (_, _synType, synExpr, _range) -> validateExpr synExpr
//
//        | SynExpr.ObjExpr (bindings = binds) -> List.forall validateBinding binds
//
//        | SynExpr.While (_sequencePointInfoForWhileLoop, synExpr1, synExpr2, _range) ->
//            List.forall validateExpr [ synExpr1; synExpr2 ]
//        | SynExpr.ForEach (_sequencePointInfoForForLoop, _seqExprOnly, _isFromSource, synPat, synExpr1, synExpr2, _range) ->
//            List.forall validateExpr [ synExpr1; synExpr2 ]
//            && validatePattern synPat
//
//        | SynExpr.For (identBody = synExpr1; toBody = synExpr2; doBody = synExpr3) ->
//            List.forall validateExpr [ synExpr1; synExpr2; synExpr3 ]
//
//        | SynExpr.ArrayOrListComputed (_, synExpr, _range) -> validateExpr synExpr
//        | SynExpr.ComputationExpr (_, synExpr, _range) -> validateExpr synExpr
//        | SynExpr.Lambda (body = synExpr) -> validateExpr synExpr
//
//        | SynExpr.MatchLambda (_isExnMatch, _argm, synMatchClauseList, _spBind, _wholem) ->
//            List.forall validateClause synMatchClauseList
//        | SynExpr.Match (expr = synExpr; clauses = synMatchClauseList)
//        | SynExpr.MatchBang (expr = synExpr; clauses = synMatchClauseList) ->
//            validateExpr synExpr
//            && List.forall validateClause synMatchClauseList
//
//        | SynExpr.Lazy (synExpr, _range) -> validateExpr synExpr
//        | SynExpr.Do (synExpr, _range) -> validateExpr synExpr
//        | SynExpr.Assert (synExpr, _range) -> validateExpr synExpr
//
//        | SynExpr.App (_exprAtomicFlag, _isInfix, synExpr1, synExpr2, _range) ->
//            List.forall validateExpr [ synExpr1; synExpr2 ]
//
//        | SynExpr.TypeApp (synExpr, _, _synTypeList, _commas, _, _, _range) -> validateExpr synExpr
//
//        | SynExpr.LetOrUse (bindings = synBindingList; body = synExpr) ->
//            List.forall validateBinding synBindingList
//            && validateExpr synExpr
//
//        | SynExpr.TryWith (tryExpr = synExpr; withCases = synMatchClauseList) ->
//            validateExpr synExpr
//            && List.forall validateClause synMatchClauseList
//
//        | SynExpr.TryFinally (tryExpr = synExpr1; finallyExpr = synExpr2) ->
//            List.forall validateExpr [ synExpr1; synExpr2 ]
//
//        | SynExpr.Sequential (_sequencePointInfoForSeq, _, synExpr1, synExpr2, _range)
//        | SynExpr.SequentialOrImplicitYield (_sequencePointInfoForSeq, synExpr1, synExpr2, _, _range) ->
//            List.forall validateExpr [ synExpr1; synExpr2 ]
//
//        | SynExpr.IfThenElse (ifExpr = synExpr1; thenExpr = synExpr2; elseExpr = synExprOpt) ->
//            match synExprOpt with
//            | Some synExpr3 -> List.forall validateExpr [ synExpr1; synExpr2; synExpr3 ]
//            | None -> List.forall validateExpr [ synExpr1; synExpr2 ]
//
//        | SynExpr.Ident _ident -> true
//        | SynExpr.LongIdent (_, _longIdent, _altNameRefCell, _range) -> true
//
//        | SynExpr.LongIdentSet (_longIdent, synExpr, _range) -> validateExpr synExpr
//        | SynExpr.DotGet (synExpr, _dotm, _longIdent, _range) -> validateExpr synExpr
//
//        | SynExpr.DotSet (synExpr1, _, synExpr2, _)
//        | SynExpr.Set (synExpr1, synExpr2, _) -> List.forall validateExpr [ synExpr1; synExpr2 ]
//
//        | SynExpr.DotIndexedGet (synExpr, indexArgs, _range, _range2) -> validateExpr synExpr && validateExpr indexArgs
//
//        | SynExpr.DotIndexedSet (synExpr1, indexArgs, synExpr2, _, _range, _range2) ->
//            [ synExpr1; indexArgs; synExpr2 ]
//            |> List.forall validateExpr
//
//        | SynExpr.JoinIn (synExpr1, _range, synExpr2, _range2) -> List.forall validateExpr [ synExpr1; synExpr2 ]
//        | SynExpr.NamedIndexedPropertySet (_longIdent, synExpr1, synExpr2, _range) ->
//            List.forall validateExpr [ synExpr1; synExpr2 ]
//
//        | SynExpr.DotNamedIndexedPropertySet (synExpr1, _longIdent, synExpr2, synExpr3, _range) ->
//            List.forall validateExpr [ synExpr1; synExpr2; synExpr3 ]
//
//        | SynExpr.TypeTest (synExpr, _synType, _range)
//        | SynExpr.Upcast (synExpr, _synType, _range)
//        | SynExpr.Downcast (synExpr, _synType, _range) -> validateExpr synExpr
//        | SynExpr.InferredUpcast (synExpr, _range)
//        | SynExpr.InferredDowncast (synExpr, _range) -> validateExpr synExpr
//        | SynExpr.AddressOf (_, synExpr, _range, _range2) -> validateExpr synExpr
//        | SynExpr.TraitCall (_synTyparList, _synMemberSig, synExpr, _range) -> validateExpr synExpr
//
//        | SynExpr.Null _range
//        | SynExpr.ImplicitZero _range -> true
//
//        | SynExpr.YieldOrReturn (_, synExpr, _range)
//        | SynExpr.YieldOrReturnFrom (_, synExpr, _range)
//        | SynExpr.DoBang (synExpr, _range) -> validateExpr synExpr
//
//        | SynExpr.LetOrUseBang (pat = synPat; rhs = synExpr1; andBangs = ands; body = synExpr2) ->
//            List.forall validateExpr [ synExpr1; synExpr2 ]
//            && validatePattern synPat
//            && List.forall
//                (fun (pat, e) -> validateExpr e && validatePattern pat)
//                (ands
//                 |> List.map (fun (SynExprAndBang (pat = pat; body = e)) -> pat, e))
//
//        | SynExpr.LibraryOnlyILAssembly _
//        | SynExpr.LibraryOnlyStaticOptimization _
//        | SynExpr.LibraryOnlyUnionCaseFieldGet _
//        | SynExpr.LibraryOnlyUnionCaseFieldSet _ -> true
//
//        | SynExpr.ArbitraryAfterError (_debugStr, _range) -> false
//        | SynExpr.FromParseError (_synExpr, _range)
//        | SynExpr.DiscardAfterMissingQualificationAfterDot (_synExpr, _range) -> false
//        | SynExpr.Fixed _ -> true
//        | SynExpr.InterpolatedString (parts, _, _) ->
//            parts
//            |> List.forall (function
//                | SynInterpolatedStringPart.String _ -> true
//                | SynInterpolatedStringPart.FillExpr (e, _) -> validateExpr e)
//        | SynExpr.IndexRange (e1, _, e2, _, _, _) ->
//            defaultArg (Option.map validateExpr e1) true
//            && defaultArg (Option.map validateExpr e2) true
//        | SynExpr.IndexFromEnd (e, _) -> validateExpr e
//
//    and validatePattern =
//        function
//        | SynPat.Const (_const, _range) -> true
//        | SynPat.Wild _
//        | SynPat.Null _ -> true
//        | SynPat.Named (_ident, _isThis, _accessOpt, _range) -> true
//        | SynPat.As (pat1, pat2, _) -> validatePattern pat1 && validatePattern pat2
//        | SynPat.Typed (pat, _typ, _range) -> validatePattern pat
//        | SynPat.Attrib (pat, _attrib, _range) -> validatePattern pat
//        | SynPat.Or (lhsPat = pat1; rhsPat = pat2) -> validatePattern pat1 && validatePattern pat2
//        | SynPat.Ands (pats, _range) -> List.forall validatePattern pats
//        | SynPat.LongIdent (argPats = constructorArgs) -> validateConstructorArgs constructorArgs
//        | SynPat.Tuple (false, pats, _range) -> List.forall validatePattern pats
//        | SynPat.Paren (pat, _range) -> validatePattern pat
//        | SynPat.ArrayOrList (_isArray, pats, _range) -> List.forall validatePattern pats
//        | SynPat.Record (identPats, _range) -> List.forall (fun (_, _, pat) -> validatePattern pat) identPats
//        | SynPat.OptionalVal (_ident, _range) -> true
//        | SynPat.IsInst (_typ, _range) -> true
//        | SynPat.QuoteExpr (expr, _range) -> validateExpr expr
//        | SynPat.DeprecatedCharRange _
//        | SynPat.InstanceMember _
//        | SynPat.Tuple (true, _, _) -> true
//        | SynPat.FromParseError _ -> false
//
//    and validateConstructorArgs =
//        function
//        | SynArgPats.Pats pats -> List.forall validatePattern pats
//        | SynArgPats.NamePatPairs (identPats, _range) -> List.forall (fun (_, _, pat) -> validatePattern pat) identPats
//
//    match ast with
//    | ParsedInput.SigFile _input ->
//        // There is not much to explore in signature files
//        true
//    | ParsedInput.ImplFile input -> validateImplFileInput input

/// Check whether an input string is invalid in F# by looking for erroneous nodes in ASTs
let isValidFSharpCode (isSignature: bool) (source: ISourceText) : Async<bool> =
    async {
        try
            let! ast = parse isSignature source

            //            let isValid =
//                ast
//                |> Array.forall (fun (a, _, _) -> isValidAST a)
//
//            return isValid
            return true
        with
        | _ -> return false
    }

let formatWith
    (sourceAndTokens: (ISourceText * TokenWithRangeList) option)
    (defineCombination: DefineCombination)
    ast
    config
    =
    let formattedSourceCode =
        let context = Context.Context.Create config sourceAndTokens defineCombination ast

        context
        |> genParsedInput ASTContext.Default ast
        |> Context.dump

    formattedSourceCode

let format (config: FormatConfig) (isSignature: bool) (source: ISourceText) : Async<string> =
    async {
        let! asts = parse isSignature source

        let! results =
            asts
            |> Array.map (fun (ast', tokens, defineCombination) ->
                async {
                    let formattedCode = formatWith (Some(source, tokens)) defineCombination ast' config
                    return (defineCombination, formattedCode)
                })
            |> Async.Parallel
            |> Async.map Array.toList

        let merged =
            match results with
            | [] -> failwith "not possible"
            | [ (_, x) ] -> x
            | all ->
                let allInFragments =
                    all
                    |> String.splitInFragments config.EndOfLine.NewLineString

                let allHaveSameFragmentCount =
                    let allWithCount = List.map (fun (_, f: string list) -> f.Length) allInFragments

                    (Set allWithCount).Count = 1

                if not allHaveSameFragmentCount then
                    let chunkReport =
                        allInFragments
                        |> List.map (fun (defines, fragments) ->
                            sprintf "[%s] has %i fragments" (String.concat ", " defines) fragments.Length)
                        |> String.concat config.EndOfLine.NewLineString

                    failwithf
                        """Fantomas is trying to format the input multiple times due to the detect of multiple defines.
There is a problem with merging all the code back together.
%s
Please raise an issue at https://fsprojects.github.io/fantomas-tools/#/fantomas/preview."""
                        chunkReport

                List.map snd allInFragments
                |> List.reduce String.merge
                |> String.concat config.EndOfLine.NewLineString

        return merged
    }

/// Format a source string using given config
let formatDocument config isSignature source = format config isSignature source

/// Format an abstract syntax tree using given config
let formatAST ast defines sourceAndTokens config =
    formatWith sourceAndTokens defines ast config

/// Make a range from (startLine, startCol) to (endLine, endCol) to select some text
let makeRange fileName startLine startCol endLine endCol =
    mkRange fileName (mkPos startLine startCol) (mkPos endLine endCol)

/// Get first non-whitespace line
let rec getStartLineIndex (lines: _ []) i =
    if i = lines.Length - 1
       || not <| String.IsNullOrWhiteSpace(lines.[i]) then
        i
    else
        getStartLineIndex lines (i + 1)

let rec getEndLineIndex (lines: _ []) i =
    if i = 0
       || not <| String.IsNullOrWhiteSpace(lines.[i]) then
        i
    else
        getEndLineIndex lines (i - 1)

//let isSignificantToken (tok: FSharpTokenInfo) =
//    tok.CharClass <> FSharpTokenCharKind.WhiteSpace
//    && tok.CharClass <> FSharpTokenCharKind.LineComment
//    && tok.CharClass <> FSharpTokenCharKind.Comment
//    && tok.TokenName <> "STRING_TEXT"

/// Find out the start token
//let rec getStartCol (r: Range) (tokenizer: FSharpLineTokenizer) lexState =
//    match tokenizer.ScanToken(!lexState) with
//    | Some tok, state ->
//        if tok.RightColumn >= r.StartColumn
//           && isSignificantToken tok then
//            tok.LeftColumn
//        else
//            lexState := state
//            getStartCol r tokenizer lexState
//    | None, _ -> r.StartColumn

/// Find out the end token
//let rec getEndCol (r: Range) (tokenizer: FSharpLineTokenizer) lexState =
//    match tokenizer.ScanToken(!lexState) with
//    | Some tok, state ->
//        Debug.WriteLine("End token: {0}", sprintf "%A" tok |> box)
//
//        if tok.RightColumn >= r.EndColumn
//           && isSignificantToken tok then
//            tok.RightColumn
//        else
//            lexState := state
//            getEndCol r tokenizer lexState
//    | None, _ -> r.EndColumn

//type PatchKind =
//    | TypeMember
//    | RecType
//    | RecLet
//    | Nothing
//
//let startWithMember (sel: string) =
//    [| "member"
//       "abstract"
//       "default"
//       "override"
//       "static"
//       "interface"
//       "new"
//       "val"
//       "inherit" |]
//    |> Array.exists (sel.TrimStart().StartsWith)
//
///// Find the first type declaration or let binding at beginnings of lines
//let private getPatch startCol (lines: string []) =
//    let rec loop i =
//        if i < 0 then
//            Nothing
//        elif Regex.Match(lines.[i], "^[\s]*type").Success then
//            RecType
//        else
//            // Need to compare column to ensure that the let binding is at the same level
//            let m = Regex.Match(lines.[i], "^[\s]*let")
//            let col = m.Index + m.Length
//            // Value 4 accounts for length of "and "
//            if m.Success && col <= startCol + 4 then
//                RecLet
//            else
//                loop (i - 1)
//
//    loop (lines.Length - 1)
//
///// Convert from range to string positions
//let private stringPos (r: Range) (sourceCode: string) =
//    // Assume that content has been normalized (no "\r\n" anymore)
//    let positions =
//        sourceCode.Split('\n')
//        // Skip '\r' as a new line character on Windows
//        |> Seq.map (fun s -> String.length s + 1)
//        |> Seq.scan (+) 0
//        |> Seq.toArray
//
//    let start = positions.[r.StartLine - 1] + r.StartColumn
//    // We can't assume the range is valid, so check string boundary here
//    let finish =
//        let pos = positions.[r.EndLine - 1] + r.EndColumn
//
//        if pos >= sourceCode.Length then
//            sourceCode.Length - 1
//        else
//            pos
//
//    (start, finish)
//
//let private formatRange
//    (checker: FSharpChecker)
//    (parsingOptions: FSharpParsingOptions)
//    returnFormattedContentOnly
//    (range: Range)
//    (lines: _ [])
//    config
//    ({ Source = sourceCode
//       FileName = fileName } as formatContext)
//    =
//    let startLine = range.StartLine
//    let startCol = range.StartColumn
//    let endLine = range.EndLine
//
//    let start, finish = stringPos range sourceCode
//
//    let pre =
//        if start = 0 then
//            String.Empty
//        else
//            sourceCode.[0 .. start - 1].TrimEnd('\r')
//
//    // Prepend selection by an appropriate amount of whitespace
//    let selection, patch =
//        let sel = sourceCode.[start..finish].TrimEnd('\r')
//
//        if startWithMember sel then
//            (String.Join(String.Empty, "type T = ", Environment.NewLine, System.String(' ', startCol), sel), TypeMember)
//        elif String.startsWithOrdinal "and" (sel.TrimStart()) then
//            let p = getPatch startCol lines.[.. startLine - 1]
//
//            let pattern = Regex("and")
//
//            let replacement =
//                match p with
//                | RecType -> "type"
//                | RecLet -> "let rec"
//                | _ -> "and"
//            // Replace "and" by "type" or "let rec"
//            if startLine = endLine then
//                (pattern.Replace(sel, replacement, 1), p)
//            else
//                (System.String(' ', startCol)
//                 + pattern.Replace(sel, replacement, 1),
//                 p)
//        elif startLine = endLine then
//            (sel, Nothing)
//        else
//            (System.String(' ', startCol) + sel, Nothing)
//
//    let post =
//        if finish < sourceCode.Length then
//            let post = sourceCode.[finish + 1 ..]
//
//            if String.startsWithOrdinal "\n" post then
//                Environment.NewLine + post.[1..]
//            else
//                post
//        else
//            String.Empty
//
//    Debug.WriteLine("pre:\n'{0}'", box pre)
//    Debug.WriteLine("selection:\n'{0}'", box selection)
//    Debug.WriteLine("post:\n'{0}'", box post)
//
//    let formatSelection sourceCode config =
//        async {
//            // From this point onwards, we focus on the current selection
//            let formatContext = { formatContext with Source = sourceCode }
//
//            let! formattedSourceCode = format checker parsingOptions config formatContext
//            // If the input is not inline, the output should not be inline as well
//            if
//                sourceCode.EndsWith("\n")
//                && not
//                   <| formattedSourceCode.EndsWith(Environment.NewLine)
//            then
//                return formattedSourceCode + Environment.NewLine
//            elif
//                not <| sourceCode.EndsWith("\n")
//                && formattedSourceCode.EndsWith(Environment.NewLine)
//            then
//                return formattedSourceCode.TrimEnd('\r', '\n')
//            else
//                return formattedSourceCode
//        }
//
//    let reconstructSourceCode startCol formatteds pre post =
//        Debug.WriteLine("Formatted parts: '{0}' at column {1}", sprintf "%A" formatteds, startCol)
//        // Realign results on the correct column
//        Context.Context.Create config [] fileName [] String.Empty None
//        // Mono version of indent text writer behaves differently from .NET one,
//        // So we add an empty string first to regularize it
//        |> if returnFormattedContentOnly then
//               Context.str String.Empty
//           else
//               Context.str pre
//        |> Context.atIndentLevel true startCol (Context.col Context.sepNln formatteds Context.str)
//        |> if returnFormattedContentOnly then
//               Context.str String.Empty
//           else
//               Context.str post
//        |> Context.dump
//
//    async {
//        match patch with
//        | TypeMember ->
//            // Get formatted selection with "type T = \n" patch
//            let! result = formatSelection selection config
//            // Remove the patch
//            let contents = String.normalizeThenSplitNewLine result
//
//            if Array.isEmpty contents then
//                if returnFormattedContentOnly then
//                    return result
//                else
//                    return String.Join(String.Empty, pre, result, post)
//            else
//                // Due to patching, the text has at least two lines
//                let first = contents.[1]
//                let column = first.Length - first.TrimStart().Length
//
//                let formatteds = contents.[1..] |> Seq.map (fun s -> s.[column..])
//
//                return reconstructSourceCode startCol formatteds pre post
//        | RecType
//        | RecLet ->
//            // Get formatted selection with "type" or "let rec" replacement for "and"
//            let! result = formatSelection selection config
//            // Substitute by old contents
//            let pattern =
//                if patch = RecType then
//                    Regex("type")
//                else
//                    Regex("let rec")
//
//            let formatteds =
//                String.normalizeThenSplitNewLine (pattern.Replace(result, "and", 1))
//
//            return reconstructSourceCode startCol formatteds pre post
//        | Nothing ->
//            let! result = formatSelection selection config
//            let formatteds = String.normalizeThenSplitNewLine result
//            return reconstructSourceCode startCol formatteds pre post
//    }
//
///// Format a part of source string using given config, and return the (formatted) selected part only.
///// Beware that the range argument is inclusive. If the range has a trailing newline, it will appear in the formatted result.
//let formatSelection
//    (checker: FSharpChecker)
//    (parsingOptions: FSharpParsingOptions)
//    (range: Range)
//    config
//    ({ Source = sourceCode
//       FileName = fileName } as formatContext)
//    =
//    let lines = String.normalizeThenSplitNewLine sourceCode
//
//    // Move to the section with real contents
//    let contentRange =
//        if range.StartLine = range.EndLine then
//            range
//        else
//            let startLine = getStartLineIndex lines (range.StartLine - 1) + 1
//
//            let endLine = getEndLineIndex lines (range.EndLine - 1) + 1
//
//            Debug.Assert(startLine >= range.StartLine, "Should shrink selections only.")
//            Debug.Assert(endLine <= range.EndLine, "Should shrink selections only.")
//
//            let startCol =
//                if startLine = range.StartLine then
//                    max range.StartColumn 0
//                else
//                    0
//
//            let endCol =
//                if endLine = range.EndLine then
//                    min range.EndColumn (lines.[endLine - 1].Length - 1)
//                else
//                    lines.[endLine - 1].Length - 1
//            // Notice that Line indices start at 1 while Column indices start at 0.
//            makeRange fileName startLine startCol endLine endCol
//
//    let startCol =
//        let line = lines.[contentRange.StartLine - 1].[contentRange.StartColumn ..]
//
//        contentRange.StartColumn + line.Length
//        - line.TrimStart().Length
//
//    let endCol =
//        let line = lines.[contentRange.EndLine - 1].[.. contentRange.EndColumn]
//
//        contentRange.EndColumn - line.Length
//        + line.TrimEnd().Length
//
//    let modifiedRange = makeRange fileName range.StartLine startCol range.EndLine endCol
//
//    Debug.WriteLine(
//        "Original range: {0} --> content range: {1} --> modified range: {2}",
//        sprintf "%O" range,
//        sprintf "%O" contentRange,
//        sprintf "%O" modifiedRange
//    )
//
//    async {
//        let! formatted = formatRange checker parsingOptions true modifiedRange lines config formatContext
//
//        let start, finish = stringPos range sourceCode
//        let newStart, newFinish = stringPos modifiedRange sourceCode
//
//        let pre = sourceCode.[start .. newStart - 1].TrimEnd('\r')
//
//        let post =
//            if newFinish + 1 >= sourceCode.Length
//               || newFinish >= finish then
//                String.Empty
//            else
//                sourceCode.[newFinish + 1 .. finish]
//                    .Replace("\r", "\n")
//
//        Debug.WriteLine(
//            "Original index: {0} --> modified index: {1}",
//            sprintf "%O" (start, finish),
//            sprintf "%O" (newStart, newFinish)
//        )
//
//        Debug.WriteLine("Join '{0}', '{1}' and '{2}'", pre, formatted, post)
//        return String.Join(String.Empty, pre, formatted, post)
//    }

type internal BlockType =
    | List
    | Array
    | SequenceOrRecord
    | Tuple

/// Make a position at (line, col) to denote cursor position
let makePos line col = mkPos line col
