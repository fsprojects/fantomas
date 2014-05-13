module Fantomas.CodeFormatter

open System
open System.Diagnostics
open System.Collections.Generic
open System.Text.RegularExpressions

open Microsoft.FSharp.Compiler.Ast
open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.SourceCodeServices

open Fantomas.TokenMatcher
open Fantomas.FormatConfig
open Fantomas.SourceParser
open Fantomas.CodePrinter

let internal parseWith fileName content = 
    // Create an interactive checker instance (ignore notifications)
    let checker = InteractiveChecker.Create()
    // Get compiler options for a single script file
    let checkOptions = 
        checker.GetProjectOptionsFromScript(fileName, content, DateTime.Now, filterDefines content) 
        |> Async.RunSynchronously
    // Run the first phase (untyped parsing) of the compiler
    let untypedRes = checker.ParseFileInProject(fileName, content, checkOptions) |> Async.RunSynchronously
    match untypedRes.ParseTree with
    | Some tree -> tree
    | None -> raise <| FormatException "Parsing failed. Please select a complete code fragment to format."

/// Parse a source code string
let parse isFsiFile s = 
    let fileName = if isFsiFile then "/tmp.fsi" else "/tmp.fs"
    parseWith fileName s

/// Check whether an AST consists of parsing errors 
let isValidAST ast = 
    let (|IndexerArg|) = function
        | SynIndexerArg.Two(e1, e2) -> [e1; e2]
        | SynIndexerArg.One e -> [e]

    let (|IndexerArgList|) xs =
        List.collect (|IndexerArg|) xs

    let rec validateImplFileInput (ParsedImplFileInput(_, moduleOrNamespaceList)) = 
        List.forall validateModuleOrNamespace moduleOrNamespaceList

    and validateModuleOrNamespace(SynModuleOrNamespace(_lid, _isModule, decls, _xmldoc, _attributes, _access, _range)) =
        List.forall validateModuleDecl decls

    and validateModuleDecl(decl: SynModuleDecl) =
        match decl with
        | SynModuleDecl.Exception(ExceptionDefn(_repr, synMembers, _defnRange), _range) -> 
            List.forall validateMemberDefn synMembers
        | SynModuleDecl.Let(_isRecursive, bindings, _range) ->
            List.forall validateBinding bindings
        | SynModuleDecl.ModuleAbbrev(_lhs, _rhs, _range) ->
            true
        | SynModuleDecl.NamespaceFragment(fragment) ->
            validateModuleOrNamespace fragment
        | SynModuleDecl.NestedModule(_componentInfo, modules, _isContinuing, _range) ->
            List.forall validateModuleDecl modules
        | SynModuleDecl.Types(typeDefs, _range) ->
            List.forall validateTypeDefn typeDefs
        | SynModuleDecl.DoExpr (_, expr, _) ->
            validateExpr expr
        | SynModuleDecl.Attributes _
        | SynModuleDecl.HashDirective _
        | SynModuleDecl.Open _ -> 
            true

    and validateTypeDefn(TypeDefn(_componentInfo, representation, members, _range)) = 
        validateTypeDefnRepr representation && List.forall validateMemberDefn members        

    and validateTypeDefnRepr(typeDefnRepr: SynTypeDefnRepr) = 
        match typeDefnRepr with
        | SynTypeDefnRepr.ObjectModel(_kind, members, _range) ->
            List.forall validateMemberDefn members
        | SynTypeDefnRepr.Simple(_repr, _range) -> 
            true

    and validateMemberDefn (memberDefn: SynMemberDefn) =
        match memberDefn with
        | SynMemberDefn.AbstractSlot(_synValSig, _memberFlags, _range) ->
            true
        | SynMemberDefn.AutoProperty(_attributes, _isStatic, _id, _type, _memberKind, _memberFlags, _xmlDoc, _access, expr, _r1, _r2) ->
            validateExpr expr
        | SynMemberDefn.Interface(_interfaceType, members, _range) ->
            defaultArg (Option.map (List.forall validateMemberDefn) members) true
        | SynMemberDefn.Member(binding, _range) ->
            validateBinding binding
        | SynMemberDefn.NestedType(typeDef, _access, _range) -> 
            validateTypeDefn typeDef
        | SynMemberDefn.ValField(_field, _range) ->
            true
        | SynMemberDefn.LetBindings(bindings, _isStatic, _isRec, _range) ->
            List.forall validateBinding bindings
        | SynMemberDefn.Open _
        | SynMemberDefn.Inherit _
        | SynMemberDefn.ImplicitCtor _ -> 
            true
        | SynMemberDefn.ImplicitInherit(_, expr, _, _) ->
            validateExpr expr

    and validateBinding (Binding(_access, _bindingKind, _isInline, _isMutable, _attrs, _xmldoc, _valData, headPat, _retTy, expr, _bindingRange, _seqPoint)) =
        validateExpr expr && validatePattern headPat

    and validateClause (Clause(pat, expr, exprOpt)) =
        validatePattern pat && validateExpr expr && defaultArg (Option.map validateExpr exprOpt) true

    and validateExpr expr =
        match expr with
        | SynExpr.Quote(synExpr1, _, synExpr2, _, _range) ->
            List.forall validateExpr [synExpr1; synExpr2]

        | SynExpr.Const(_synConst, _range) -> 
            true

        | SynExpr.Paren(synExpr, _, _, _parenRange) ->
            validateExpr synExpr
        | SynExpr.Typed(synExpr, _synType, _range) -> 
            validateExpr synExpr

        | SynExpr.Tuple(synExprList, _, _range)
        | SynExpr.ArrayOrList(_, synExprList, _range) ->
            List.forall validateExpr synExprList
        | SynExpr.Record(_inheritOpt, _copyOpt, fields, _range) -> 
            List.forall (fun (_, e, _) -> defaultArg (Option.map validateExpr e) true) fields

        | SynExpr.New(_, _synType, synExpr, _range) -> 
            validateExpr synExpr

        | SynExpr.ObjExpr(_ty, _baseCallOpt, binds, _ifaces, _range1, _range2) -> 
            List.forall validateBinding binds

        | SynExpr.While(_sequencePointInfoForWhileLoop, synExpr1, synExpr2, _range) ->
            List.forall validateExpr [synExpr1; synExpr2]
        | SynExpr.ForEach(_sequencePointInfoForForLoop, _seqExprOnly, _isFromSource, synPat, synExpr1, synExpr2, _range) -> 
            List.forall validateExpr [synExpr1; synExpr2] && validatePattern synPat

        | SynExpr.For(_sequencePointInfoForForLoop, _ident, synExpr1, _, synExpr2, synExpr3, _range) -> 
            List.forall validateExpr [synExpr1; synExpr2; synExpr3]

        | SynExpr.ArrayOrListOfSeqExpr(_, synExpr, _range) ->
            validateExpr synExpr
        | SynExpr.CompExpr(_, _, synExpr, _range) ->
            validateExpr synExpr
        | SynExpr.Lambda(_, _, _synSimplePats, synExpr, _range) ->
                validateExpr synExpr

        | SynExpr.MatchLambda(_isExnMatch, _argm, synMatchClauseList, _spBind, _wholem) -> 
            List.forall validateClause synMatchClauseList
        | SynExpr.Match(_sequencePointInfoForBinding, synExpr, synMatchClauseList, _, _range) ->
            validateExpr synExpr && List.forall validateClause synMatchClauseList

        | SynExpr.Lazy(synExpr, _range) ->
            validateExpr synExpr
        | SynExpr.Do(synExpr, _range) ->
            validateExpr synExpr
        | SynExpr.Assert(synExpr, _range) -> 
            validateExpr synExpr

        | SynExpr.App(_exprAtomicFlag, _isInfix, synExpr1, synExpr2, _range) ->
            List.forall validateExpr [synExpr1; synExpr2]

        | SynExpr.TypeApp(synExpr, _, _synTypeList, _commas, _, _, _range) -> 
            validateExpr synExpr

        | SynExpr.LetOrUse(_, _, synBindingList, synExpr, _range) -> 
            List.forall validateBinding synBindingList && validateExpr synExpr

        | SynExpr.TryWith(synExpr, _range, synMatchClauseList, _range2, _range3, _sequencePointInfoForTry, _sequencePointInfoForWith) -> 
            validateExpr synExpr && List.forall validateClause synMatchClauseList

        | SynExpr.TryFinally(synExpr1, synExpr2, _range, _sequencePointInfoForTry, _sequencePointInfoForFinally) -> 
            List.forall validateExpr [synExpr1; synExpr2]

        | SynExpr.Sequential(_sequencePointInfoForSeq, _, synExpr1, synExpr2, _range) -> 
            List.forall validateExpr [synExpr1; synExpr2]

        | SynExpr.IfThenElse(synExpr1, synExpr2, synExprOpt, _sequencePointInfoForBinding, _isRecovery, _range, _range2) -> 
            match synExprOpt with
            | Some synExpr3 ->
                List.forall validateExpr [synExpr1; synExpr2; synExpr3]
            | None ->
                List.forall validateExpr [synExpr1; synExpr2]

        | SynExpr.Ident(_ident) ->
            true
        | SynExpr.LongIdent(_, _longIdent, _altNameRefCell, _range) -> 
            true

        | SynExpr.LongIdentSet(_longIdent, synExpr, _range) ->
            validateExpr synExpr
        | SynExpr.DotGet(synExpr, _dotm, _longIdent, _range) -> 
            validateExpr synExpr

        | SynExpr.DotSet(synExpr1, _longIdent, synExpr2, _range) ->
            List.forall validateExpr [synExpr1; synExpr2]

        | SynExpr.DotIndexedGet(synExpr, IndexerArgList synExprList, _range, _range2) -> 
            validateExpr synExpr && List.forall validateExpr synExprList 

        | SynExpr.DotIndexedSet(synExpr1, IndexerArgList synExprList, synExpr2, _, _range, _range2) -> 
            [ yield synExpr1
              yield! synExprList
              yield synExpr2 ]
            |> List.forall validateExpr

        | SynExpr.JoinIn(synExpr1, _range, synExpr2, _range2) ->
            List.forall validateExpr [synExpr1; synExpr2]
        | SynExpr.NamedIndexedPropertySet(_longIdent, synExpr1, synExpr2, _range) ->
            List.forall validateExpr [synExpr1; synExpr2]

        | SynExpr.DotNamedIndexedPropertySet(synExpr1, _longIdent, synExpr2, synExpr3, _range) ->  
            List.forall validateExpr [synExpr1; synExpr2; synExpr3]

        | SynExpr.TypeTest(synExpr, _synType, _range)
        | SynExpr.Upcast(synExpr, _synType, _range)
        | SynExpr.Downcast(synExpr, _synType, _range) ->
            validateExpr synExpr
        | SynExpr.InferredUpcast(synExpr, _range)
        | SynExpr.InferredDowncast(synExpr, _range) ->
            validateExpr synExpr
        | SynExpr.AddressOf(_, synExpr, _range, _range2) ->
            validateExpr synExpr
        | SynExpr.TraitCall(_synTyparList, _synMemberSig, synExpr, _range) ->
            validateExpr synExpr

        | SynExpr.Null(_range)
        | SynExpr.ImplicitZero(_range) -> 
            true

        | SynExpr.YieldOrReturn(_, synExpr, _range)
        | SynExpr.YieldOrReturnFrom(_, synExpr, _range) 
        | SynExpr.DoBang(synExpr, _range) -> 
            validateExpr synExpr

        | SynExpr.LetOrUseBang(_sequencePointInfoForBinding, _, _, synPat, synExpr1, synExpr2, _range) -> 
            List.forall validateExpr [synExpr1; synExpr2] && validatePattern synPat

        | SynExpr.LibraryOnlyILAssembly _
        | SynExpr.LibraryOnlyStaticOptimization _ 
        | SynExpr.LibraryOnlyUnionCaseFieldGet _
        | SynExpr.LibraryOnlyUnionCaseFieldSet _ ->
            true

        | SynExpr.ArbitraryAfterError(_debugStr, _range) -> 
            false
        | SynExpr.FromParseError(_synExpr, _range)
        | SynExpr.DiscardAfterMissingQualificationAfterDot(_synExpr, _range) -> 
            false

    and validatePattern = function
        | SynPat.Const(_const, _range) -> true
        | SynPat.Wild _ 
        | SynPat.Null _-> true
        | SynPat.Named(pat, _ident, _isThis,  _accessOpt, _range) ->
            validatePattern pat
        | SynPat.Typed(pat, _typ, _range) ->
            validatePattern pat
        | SynPat.Attrib(pat, _attrib, _range) ->
            validatePattern pat
        | SynPat.Or(pat1, pat2, _range) ->
            validatePattern pat1 && validatePattern pat2
        | SynPat.Ands(pats, _range) ->
            List.forall validatePattern pats
        | SynPat.LongIdent(_, _, _, constructorArgs, _, _) -> 
            validateConstructorArgs constructorArgs
        | SynPat.Tuple(pats, _range) ->
            List.forall validatePattern pats
        | SynPat.Paren(pat, _range) ->
            validatePattern pat
        | SynPat.ArrayOrList(_isArray, pats, _range) ->
            List.forall validatePattern pats
        | SynPat.Record(identPats, _range) ->
            List.forall (fun (_, pat) -> validatePattern pat) identPats
        | SynPat.OptionalVal(_ident, _range) -> true
        | SynPat.IsInst(_typ, _range) -> true
        | SynPat.QuoteExpr(expr, _range) ->
            validateExpr expr
        | SynPat.DeprecatedCharRange _
        | SynPat.InstanceMember _ -> true
        | SynPat.FromParseError _ -> false

    and validateConstructorArgs = function
        | SynConstructorArgs.Pats pats ->
            List.forall validatePattern pats
        | SynConstructorArgs.NamePatPairs(identPats, _range) ->
            List.forall (snd >> validatePattern) identPats

    match ast with
    | ParsedInput.SigFile _input ->
        // There is not much to explore in signature files
        true
    | ParsedInput.ImplFile input -> 
        validateImplFileInput input

/// Check whether an input string is invalid in F# by looking for erroneous nodes in ASTs
let isValidFSharpCode isFsiFile s =
    try
        isValidAST (parse isFsiFile s)
    with _ -> false

let internal split (s : string) =
    s.Replace("\r\n", "\n").Replace("\r", "\n").Split('\n')

let internal formatWith ast (s : string) config =
    // Use '\n' as the new line delimiter consistently
    // It would be easier for F# parser
    let s = s.Replace("\r\n", "\n").Replace("\r", "\n")
    let s' =    
        Context.create config s 
        |> genParsedInput ASTContext.Default ast
        |> dump
        |> if config.StrictMode then id else integrateComments s

    // Sometimes F# parser gives a partial AST for incorrect input
    if String.IsNullOrWhiteSpace s <> String.IsNullOrWhiteSpace s' then
        raise <| FormatException "Incomplete code fragment which is most likely due to parsing errors or the use of F# constructs newer than supported."
    else s'

let internal format isFsiFile (s : string) config =
    formatWith (parse isFsiFile s) s config

/// Format a source string using given config
let formatSourceString isFsiFile s config =    
    let s' = format isFsiFile s config
        
    // When formatting the whole document, an EOL is required
    if s'.EndsWith(Environment.NewLine) then s' else s' + Environment.NewLine

/// Format an abstract syntax tree using given config
let formatAST ast s config =    
    let s' = formatWith ast s config
        
    // When formatting the whole document, an EOL is required
    if s'.EndsWith(Environment.NewLine) then s' else s' + Environment.NewLine

/// Make a range from (startLine, startCol) to (endLine, endCol) to select some text
let makeRange startLine startCol endLine endCol =
    mkRange "/tmp.fsx" (mkPos startLine startCol) (mkPos endLine endCol)

/// Get first non-whitespace line
let rec internal getStartLineIndex (lines : _ []) i =
    if i = lines.Length-1 || not <| String.IsNullOrWhiteSpace(lines.[i]) then i
    else getStartLineIndex lines (i + 1)

let rec internal getEndLineIndex (lines : _ []) i =
    if i = 0 || not <| String.IsNullOrWhiteSpace(lines.[i]) then i
    else getEndLineIndex lines (i - 1)

let internal isDelimitToken (tok : TokenInformation) =
    tok.CharClass <> TokenCharKind.WhiteSpace && 
    tok.CharClass <> TokenCharKind.LineComment &&
    tok.CharClass <> TokenCharKind.Comment &&
    tok.TokenName <> "STRING_TEXT"

/// Find out the start token
let rec internal getStartCol (r : range) (tokenizer : LineTokenizer) nstate = 
    match tokenizer.ScanToken(!nstate) with
    | Some(tok), state ->
        if tok.RightColumn >= r.StartColumn && isDelimitToken tok then tok.LeftColumn
        else
            nstate := state 
            getStartCol r tokenizer nstate
    | None, _ -> r.StartColumn 

/// Find out the end token
let rec internal getEndCol (r : range) (tokenizer : LineTokenizer) nstate = 
    match tokenizer.ScanToken(!nstate) with
    | Some(tok), state ->
        Debug.WriteLine("End token: {0}", sprintf "%A" tok |> box)
        if tok.RightColumn >= r.EndColumn && isDelimitToken tok then tok.RightColumn
        else
            nstate := state 
            getEndCol r tokenizer nstate
    | None, _ -> r.EndColumn 

type internal Patch =
    | TypeMember
    | RecType
    | RecLet
    | Nothing

let internal startWithMember (sel : string) =  
    [|"member"; "abstract"; "default"; "override"; 
      "static"; "interface"; "new"; "val"; "inherit"|] 
    |> Array.exists (sel.TrimStart().StartsWith)

/// Find the first type declaration or let binding at beginnings of lines
let internal getPatch startCol (lines : string []) =
    let rec loop i = 
        if i < 0 then Nothing 
        elif Regex.Match(lines.[i], "^[\s]*type").Success then RecType
        else
            // Need to compare column to ensure that the let binding is at the same level
            let m = Regex.Match(lines.[i], "^[\s]*let")
            let col = m.Index + m.Length
            // Value 4 accounts for length of "and "
            if m.Success && col <= startCol + 4 then RecLet else loop (i - 1)
    loop (lines.Length - 1)

let internal formatRange replaceDocument isFsiFile (range : range) (lines : _ []) (s : string) config =
    let startLine = range.StartLine
    let startCol = range.StartColumn
    let endLine = range.EndLine
    let s = s.Replace("\r\n", "\n").Replace("\r", "\n")

    // Convert from range to string positions
    let stringPos (r : range) =
        // Assume that content has been normalized (no "\r\n" anymore)
        let positions = 
            s.Split('\n')
            |> Seq.map (fun s -> String.length s + 1)
            |> Seq.scan (+) 0
            |> Seq.toArray

        let start = positions.[r.StartLine-1] + r.StartColumn
        // We can't assume the range is valid, so check string boundary here
        let finish = 
            let pos = positions.[r.EndLine-1] + r.EndColumn
            if pos >= s.Length then s.Length - 1 else pos 
        (start, finish)

    
    let (start, finish) = stringPos range
    let pre = if start = 0 || not replaceDocument then "" else s.[0..start-1]

    // Prepend selection by an appropriate amount of whitespace
    let (selection, patch) = 
        let sel = s.[start..finish]
        if startWithMember sel then
           (String.Join("", "type T = ", Environment.NewLine, new String(' ', startCol), sel), TypeMember)
        elif sel.TrimStart().StartsWith("and") then
            let p = getPatch startCol lines.[..startLine-1]
            let pattern = Regex("and")
            let replacement = 
                match p with
                | RecType -> "type"
                | RecLet -> "let rec"
                | _ -> "and"
            // Replace "and" by "type" or "let rec"
            if startLine = endLine then (pattern.Replace(sel, replacement, 1), p)
            else (new String(' ', startCol) + pattern.Replace(sel, replacement, 1), p)
        elif startLine = endLine then (sel, Nothing)
        else (new String(' ', startCol) + sel, Nothing)

    let post =
        if finish < s.Length && replaceDocument then 
            s.[finish+1..].Replace("\n", Environment.NewLine)
        else ""

    Debug.WriteLine("pre:\n{0}", box pre)
    Debug.WriteLine("selection:\n{0}", box selection)
    Debug.WriteLine("post:\n{0}", box post)

    let formatSelection isFsiFile s config =
        let s' = format isFsiFile s config
        // If the input is not inline, the output should not be inline as well
        if s.EndsWith("\n") && not <| s'.EndsWith(Environment.NewLine) then 
            s' + Environment.NewLine 
        else s'

    let reconstructSourceCode startCol formatteds pre post =
        // Realign results on the correct column
        Context.create config "" 
        |> str pre
        |> atIndentLevel startCol (col sepNln formatteds str)
        |> str post
        |> dump

    match patch with
    | TypeMember ->
        // Get formatted selection with "type T = \n" patch
        let result = formatSelection isFsiFile selection config
        // Remove the patch
        let contents = split result
        if Array.isEmpty contents then
            String.Join("", pre, result, post)
        else
            // Due to patching, the text has at least two lines
            let first = contents.[1]
            let column = first.Length - first.TrimStart().Length
            let formatteds = contents.[1..] |> Seq.map (fun s -> s.[column..])
            reconstructSourceCode startCol formatteds pre post
    | RecType 
    | RecLet ->        
        // Get formatted selection with "type" or "let rec" replacement for "and"
        let result = formatSelection isFsiFile selection config
        // Substitute by old contents
        let pattern = if patch = RecType then Regex("type") else Regex("let rec")
        let formatteds = split (pattern.Replace(result, "and", 1))
        reconstructSourceCode startCol formatteds pre post
    | Nothing ->
        let result = formatSelection isFsiFile selection config
        let formatteds = split result
        reconstructSourceCode startCol formatteds pre post

/// Format a part of source string using given config, and return the (formatted) selected part only.
let formatSelectionOnly isFsiFile (r : range) (s : string) config =
    let lines = split s
    formatRange false isFsiFile r lines s config

 /// Format a selected part of source string using given config; expanded selected ranges to parsable ranges. 
let formatSelectionExpanded isFsiFile (r : range) (s : string) config =
    let lines = split s
    let sourceTokenizer = SourceTokenizer([], "/tmp.fsx")

    // Move to the section with real contents
    let contentRange =
        if r.StartLine = r.EndLine then r
        else
            let startLine = getStartLineIndex lines (r.StartLine - 1) + 1
            let endLine = getEndLineIndex lines (r.EndLine - 1) + 1
            // Notice that Line indices start at 1 while Column indices start at 0.
            makeRange startLine 0 endLine (lines.[endLine-1].Length - 1)

    let startTokenizer = sourceTokenizer.CreateLineTokenizer(lines.[contentRange.StartLine-1])

    let startCol = getStartCol contentRange startTokenizer (ref 0L)

    let endTokenizer =
        if contentRange.StartLine = contentRange.EndLine then startTokenizer 
        else sourceTokenizer.CreateLineTokenizer(lines.[contentRange.EndLine-1])

    let endCol = getEndCol contentRange endTokenizer (ref 0L)

    let expandedRange = makeRange contentRange.StartLine startCol contentRange.EndLine endCol
    (formatRange true isFsiFile expandedRange lines s config, expandedRange)    
   
/// Format a selected part of source string using given config; keep other parts unchanged. 
let formatSelectionFromString isFsiFile (r : range) (s : string) config =
    fst (formatSelectionExpanded isFsiFile r s config)

type internal BlockType =
   | List
   | Array
   | SequenceOrRecord
   | Tuple

/// Make a position from (line, col) to to denote cursor position
let makePos line col = mkPos line col

/// Format around cursor delimited by '[' and ']', '{' and '}' or '(' and ')' using given config; keep other parts unchanged. 
let formatAroundCursor isFsiFile (p : pos) (s : string) config = 
    let lines = split s
    let sourceTokenizer = SourceTokenizer([], "/tmp.fsx")
    let openDelimiters = dict ["[", List; "[|", Array; "{", SequenceOrRecord; "(", Tuple]
    let closeDelimiters = dict ["]", List; "|]", Array; "}", SequenceOrRecord; ")", Tuple]

    /// Find the delimiter at the end
    let rec tryFindEndDelimiter (dic : Dictionary<_, _>) i (lines : _ []) =
        if i >= lines.Length then
            None
        else
            let line = lines.[i]
            let lineTokenizer = sourceTokenizer.CreateLineTokenizer(line)
            let finLine = ref false
            let result = ref None
            let lexState = ref 0L
            while not !finLine do
                let tok, newLexState = lineTokenizer.ScanToken(!lexState)
                lexState := newLexState
                match tok with 
                | None -> 
                    finLine := true
                | Some t when t.CharClass = TokenCharKind.Delimiter -> 
                    if i + 1 > p.Line || (i + 1 = p.Line && t.RightColumn >= p.Column) then
                        let text = line.[t.LeftColumn..t.RightColumn]
                        match text with
                        | "[" | "[|" | "{" | "(" ->
                            Debug.WriteLine("Found opening token '{0}'", text)
                            let delimiter = openDelimiters.[text]
                            match dic.TryGetValue(delimiter) with
                            | true, c -> 
                                dic.[delimiter] <- c + 1
                            | _ -> 
                                dic.Add(delimiter, 1)
                        | "]" | "|]" | "}" | ")" ->
                            Debug.WriteLine("Found closing token '{0}'", text)
                            let delimiter = closeDelimiters.[text]
                            match dic.TryGetValue(delimiter) with
                            | true, 1 -> 
                                dic.Remove(delimiter) |> ignore
                            | true, c -> 
                                dic.[delimiter] <- c - 1
                            | _ -> 
                                // The delimiter has count 0; record as a result
                                Debug.WriteLine("Record closing token '{0}'", text)
                                result := Some (i + 1, t.RightColumn, delimiter)
                        | _ -> ()
                | _ -> ()

            if Option.isNone !result then
                tryFindEndDelimiter dic (i + 1) lines
            else
                !result

    /// Find the delimiter at the beginning              
    let rec tryFindStartDelimiter blockType (dic : Dictionary<_, _>) acc i (lines : _ []) =
        if i >= p.Line then
            acc
        else
            let line = lines.[i]
            let lineTokenizer = sourceTokenizer.CreateLineTokenizer(line)
            let finLine = ref false
            let result = ref acc
            let lexState = ref 0L
            while not !finLine do
                let tok, newLexState = lineTokenizer.ScanToken(!lexState)
                lexState := newLexState
                match tok with 
                | None -> 
                    finLine := true
                | Some t when t.CharClass = TokenCharKind.Delimiter -> 
                    if i + 1 < p.Line || (i + 1 = p.Line && t.LeftColumn <= p.Column) then
                        let text = line.[t.LeftColumn..t.RightColumn]
                        match text, blockType with
                        | "]", List
                        | "|]", Array 
                        | "}", SequenceOrRecord 
                        | ")", Tuple ->
                            Debug.WriteLine("Found closing delimiter '{0}'", text)
                            let delimiter = closeDelimiters.[text]
                            match dic.TryGetValue(delimiter) with
                            | true, 1 -> 
                                dic.Remove(delimiter) |> ignore
                            | true, c -> 
                                dic.[delimiter] <- c - 1
                            | _ -> 
                                Debug.WriteLine("It's a dangling closing delimiter")
                                result := None
                        | "[", List 
                        | "[|", Array 
                        | "{", SequenceOrRecord 
                        | "(", Tuple ->
                            Debug.WriteLine("Found opening delimiter '{0}'", text)
                            let delimiter = openDelimiters.[text]
                            match dic.TryGetValue(delimiter) with
                            | true, c -> 
                                dic.[delimiter] <- c + 1
                            | _ -> 
                                Debug.WriteLine("Record opening delimiter '{0}'", text)
                                dic.Add(delimiter, 1)
                                result := Some (i + 1, t.LeftColumn)
                        | _ -> ()
                | _ -> ()

            // We find the last opening delimiter
            tryFindStartDelimiter blockType dic !result (i + 1) lines
            
    match tryFindEndDelimiter (Dictionary()) (p.Line - 1) lines with
    | None -> 
        raise <| FormatException("""Found no pair of delimiters (e.g. "[ ]", "[| |]", "{ }" or "( )") around the cursor.""")
    | Some (endLine, endCol, blockType) ->
        match tryFindStartDelimiter blockType (Dictionary()) None 0 lines with
        | None ->
            raise <| FormatException("""Found no pair of delimiters (e.g. "[ ]", "[| |]", "{ }" or "( )") around the cursor.""")
        | Some (startLine, startCol) ->
            formatSelectionFromString isFsiFile (makeRange startLine startCol endLine endCol) s config
