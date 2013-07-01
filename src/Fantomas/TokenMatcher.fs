module internal Fantomas.TokenMatcher

open System
open System.Collections.Generic
open System.Diagnostics

open Microsoft.FSharp.Compiler.Range
open Microsoft.FSharp.Compiler.PrettyNaming
open Microsoft.FSharp.Compiler.SourceCodeServices

#if INTERACTIVE
type Debug = Console
#endif

type Token = 
   | EOL
   | Tok of TokenInformation * int

let tokenize defines (content : string) =
    seq { 
        let sourceTokenizer = SourceTokenizer(defines, "/tmp.fsx")
        let lines = content.Replace("\r\n","\n").Split('\r', '\n')
        let lexState = ref 0L
        for (i, line) in lines |> Seq.zip [1..lines.Length] do 
            let lineTokenizer = sourceTokenizer.CreateLineTokenizer line
            let finLine = ref false
            while not !finLine do
                let tok, newLexState = lineTokenizer.ScanToken(!lexState)
                lexState := newLexState
                match tok with 
                | None ->
                    if i <> lines.Length then
                        // New line except at the very last token 
                        yield (EOL, System.Environment.NewLine) 
                    finLine := true
                | Some t -> 
                    yield (Tok(t, i), line.[t.LeftColumn..t.RightColumn]) }

/// Create the view as if there is no attached line number
let (|Token|_|) = function
    | EOL -> None
    | Tok(ti, _) -> Some ti

// This part of the module takes care of annotating the AST with additional information
// about comments

/// Whitespace token without EOL
let (|Space|_|) t = 
    match t with
    | (Token origTok, origTokText) when origTok.TokenName = "WHITESPACE" -> 
        Some origTokText
    | _ -> None

let (|NewLine|_|) t = 
    match t with
    | (EOL, tokText) -> Some tokText
    | _ -> None

let (|WhiteSpaces|_|) origTokens = 
    match origTokens with 
    | Space t1 :: moreOrigTokens -> 
        let rec loop ts acc = 
            match ts with 
            | NewLine t2 :: ts2
            | Space t2 :: ts2 -> loop ts2 (t2 :: acc)
            | _ -> List.rev acc, ts
        Some (loop moreOrigTokens [t1])
    | _ -> None

let (|RawAttribute|_|) origTokens = 
    match origTokens with 
    | (Token origTok, "[<") :: moreOrigTokens 
        when origTok.CharClass = TokenCharKind.Delimiter -> 
        let rec loop ts acc = 
            match ts with 
            | (Token ti2, ">]") :: ts2 
                when ti2.CharClass = TokenCharKind.Delimiter -> Some (List.rev(">]" :: acc), ts2)
            | (_, t2) :: ts2 -> loop ts2 (t2 :: acc)
            | [] -> None
        loop moreOrigTokens ["[<"]
    | _ -> None

let (|Comment|_|) = function
    | (Token ti, t) 
        when ti.CharClass = TokenCharKind.Comment || ti.CharClass = TokenCharKind.LineComment -> 
        Some t
    | _ -> None

let (|CommentChunk|_|) origTokens = 
    match origTokens with 
    | Comment t1 :: moreOrigTokens -> 
        let rec loop ts acc = 
            match ts with
            | NewLine t2 :: ts2
            | Comment t2 :: ts2
            | Space t2 :: ts2 -> loop ts2 (t2 :: acc)
            | _ -> List.rev acc, ts
        Some (loop moreOrigTokens [t1])
    | _ -> None

/// Get all comment chunks before a token 
let (|CommentChunks|_|) origTokens = 
    match origTokens with 
    | CommentChunk(ts1, moreOrigTokens) -> 
        let rec loop ts acc = 
            match ts with 
            | WhiteSpaces(_, CommentChunk(ts2, ts')) ->
                // Just keep a newline between two comment chunks
                loop ts' (ts2 :: [Environment.NewLine] :: acc)
            | CommentChunk(ts2, ts') -> 
                loop ts' (ts2 :: acc)
            | _ -> (List.rev acc |> List.map (String.concat "")), ts
        Some (loop moreOrigTokens [ts1])
    | _ -> None      

/// Given a list of tokens, attach comments to appropriate positions
let collectComments tokens =
    let rec loop origTokens (dic : Dictionary<_, _>) =
        match origTokens with
        | (Token origTok, _) :: moreOrigTokens
            when origTok.CharClass <> TokenCharKind.Comment && origTok.CharClass <> TokenCharKind.LineComment ->
            loop moreOrigTokens dic
        | NewLine _ :: moreOrigTokens -> loop moreOrigTokens dic
        | CommentChunks(ts, WhiteSpaces(_, (Tok(origTok, lineNo), _) :: moreOrigTokens))
        | CommentChunks(ts, (Tok(origTok, lineNo), _) :: moreOrigTokens) ->
            dic.Add(mkPos lineNo origTok.LeftColumn, ts)
            loop moreOrigTokens dic
        | _ -> dic
    loop tokens (Dictionary())

let (|SkipUntilIdent|_|) origTokens =
    let rec loop = function
        | (Token ti, t) :: moreOrigTokens when ti.TokenName = "IDENT" ->
            Some(t, moreOrigTokens)
        | NewLine _ :: _ -> None
        | (Token ti, _) :: _ when ti.ColorClass = TokenColorKind.PreprocessorKeyword -> None
        | _ :: moreOrigTokens -> loop moreOrigTokens
        | [] -> None
    loop origTokens

let (|SkipUntilEOL|_|) origTokens =
    let rec loop = function
        | NewLine t :: moreOrigTokens -> Some(t, moreOrigTokens)
        | (Token ti, _) :: _ when ti.ColorClass = TokenColorKind.PreprocessorKeyword -> None
        | _ :: moreOrigTokens -> loop moreOrigTokens
        | [] -> None
    loop origTokens

/// Skip all whitespaces or comments in an active block
let (|SkipWhiteSpaceOrComment|_|) origTokens =
    let rec loop = function
        | Space _ :: moreOrigTokens
        | NewLine _ :: moreOrigTokens -> loop moreOrigTokens
        | (Token ti, _) :: moreOrigTokens 
            when ti.CharClass = TokenCharKind.Comment || ti.CharClass = TokenCharKind.LineComment ->
            loop moreOrigTokens
        | (Token ti, _) :: _ when ti.ColorClass = TokenColorKind.PreprocessorKeyword -> None
        | t :: moreOrigTokens -> Some(t, moreOrigTokens)
        | [] -> None
    loop origTokens

/// Filter all directives
let collectDirectives tokens =
    let rec loop origTokens (dic : Dictionary<_, _>)  = 
        match origTokens with 
        | (Token _, "#if") :: 
          SkipUntilIdent(t, SkipUntilEOL(_, SkipWhiteSpaceOrComment((Tok(origTok, lineNo), _), moreOrigTokens))) -> 
            dic.Add(mkPos lineNo origTok.LeftColumn, t) |> ignore
            loop moreOrigTokens dic
        | _ :: moreOrigTokens -> loop moreOrigTokens dic
        | [] -> dic
    loop tokens (Dictionary()) 

/// Filter all constants to be used in lexing
let filterConstants content =
    let rec loop origTokens (hs : HashSet<_>)  = 
        match origTokens with 
        | (Token _, "#if") :: 
          SkipUntilIdent(t, SkipUntilEOL(_, moreOrigTokens)) -> 
            hs.Add(t) |> ignore
            loop moreOrigTokens hs
        | _ :: moreOrigTokens -> loop moreOrigTokens hs
        | [] -> hs
    let hs = loop (tokenize [] content |> Seq.toList) (HashSet())
    Seq.toList hs

/// Filter all defined constants to be used in parsing
let filterDefines content =
    filterConstants content
    |> Seq.map (sprintf "--define:%s")
    |> Seq.toArray

/// Filter all comments and directives; assuming all constants are defined
let filterCommentsAndDirectives content =
    let constants = filterConstants content
    let tokens = tokenize constants content |> Seq.toList
    (collectComments tokens, collectDirectives tokens)

// This part processes the token stream post- pretty printing

type LineCommentStickiness = | StickyLeft | StickyRight | NotApplicable

type MarkedToken = 
    | Marked of Token * string * LineCommentStickiness
    member x.Text = 
        let (Marked(_,t,_)) = x
        t

let (|SpaceToken|_|) t = 
    match t with
    | Marked(Token origTok, origTokText, _) when origTok.TokenName = "WHITESPACE" -> 
        Some origTokText
    | _ -> None

let (|NewLineToken|_|) t = 
    match t with
    | Marked(EOL, tokText, _) -> Some tokText
    | _ -> None

let (|WhiteSpaceTokens|_|) origTokens = 
   match origTokens with 
   | SpaceToken t1 :: moreOrigTokens -> 
       let rec loop ts acc = 
           match ts with 
           | NewLineToken t2 :: ts2
           | SpaceToken t2 :: ts2 -> loop ts2 (t2 :: acc)
           | _ -> List.rev acc, ts
       Some (loop moreOrigTokens [t1])
   | _ -> None

let (|Attribute|_|) origTokens = 
   match origTokens with 
   | Marked(Token origTok, "[<", _) :: moreOrigTokens 
       when origTok.CharClass = TokenCharKind.Delimiter -> 
       let rec loop ts acc = 
           match ts with 
           | Marked(Token ti2, ">]", _) :: ts2 
                when ti2.CharClass = TokenCharKind.Delimiter -> Some (List.rev(">]" :: acc), ts2)
           | Marked(_, t2, _) :: ts2 -> loop ts2 (t2 :: acc)
           | [] -> None
       loop moreOrigTokens ["[<"]
   | _ -> None

let rec (|Attributes|_|) = function
    | Attribute(xs, Attributes(xss, toks)) 
    | Attribute(xs, WhiteSpaceTokens(_, Attributes(xss, toks))) -> Some(xs::xss, toks)
    | Attribute(xs, toks)  -> Some([xs], toks)
    | _ -> None

let (|PreprocessorKeywordToken|_|) requiredText t = 
    match t with
    | Marked(Token origTok, origTokText, _) 
        when origTok.ColorClass = TokenColorKind.PreprocessorKeyword && origTokText = requiredText -> 
        Some origTokText
    | _ -> None

let (|InactiveCodeToken|_|) t = 
    match t with
    | Marked(Token origTok, origTokText, _) 
        when origTok.ColorClass = TokenColorKind.InactiveCode -> Some origTokText
    | _ -> None

let (|LineCommentToken|_|) wantStickyLeft t = 
    match t with
    | Marked(Token origTok, origTokText, lcs) 
        when (not wantStickyLeft || (lcs = StickyLeft)) && 
             origTok.CharClass = TokenCharKind.LineComment -> Some origTokText
    | _ -> None

let (|BlockCommentToken|_|) t = 
    match t with
    | Marked(Token origTok, origTokText, _) when origTok.CharClass = TokenCharKind.Comment -> 
        Some origTokText
    | _ -> None

let (|BlockCommentOrNewLineToken|_|) t = 
    match t with
    | BlockCommentToken tokText -> Some tokText
    | NewLineToken tokText -> Some tokText
    | _ -> None

let (|LineCommentChunk|_|) wantStickyLeft origTokens = 
   match origTokens with 
   | LineCommentToken wantStickyLeft t1 :: moreOrigTokens -> 
       let rec loop ts acc = 
           match ts with 
           | LineCommentToken false t2 :: ts2 -> loop ts2 (t2 :: acc)
           | _ -> List.rev acc, ts
       Some (loop moreOrigTokens [t1])
   | _ -> None

// TODO: does not cope with directives that have comments, e.g. 
//      #if (* hello *) FOOBAR
// or
//      #endif // FOOBAR
// or ones with extra whitespace at the end of line

let (|PreprocessorDirectiveChunk|_|) origTokens = 
   match origTokens with 
   | PreprocessorKeywordToken "#if" t1 :: 
     Marked(Token ti2, t2, _) ::
     Marked(Token ti3, t3, _) ::
     Marked(EOL, t4, _) ::
     moreOrigTokens 
         when ti2.TokenName = "WHITESPACE" && ti3.TokenName = "IDENT" -> 
        Some ([t1; t2; t3; t4], moreOrigTokens)

   | PreprocessorKeywordToken "#else" t1 :: 
     Marked(EOL, _, _) ::
     moreOrigTokens -> Some ([t1], moreOrigTokens)

   | PreprocessorKeywordToken "#endif" t1 :: 
     Marked(EOL, _, _) ::
     moreOrigTokens -> Some ([t1], moreOrigTokens)

   | _ -> None

let (|InactiveCodeChunk|_|) origTokens = 
   match origTokens with 
   | InactiveCodeToken t1 :: moreOrigTokens -> 
       let rec loop ts acc = 
           match ts with 
           | InactiveCodeToken t2 :: ts2 -> loop ts2 (t2 :: acc) 
           | NewLineToken t2 :: ts2 -> loop ts2 (t2 :: acc) 
           | _ -> List.rev acc, ts
       Some (loop moreOrigTokens [t1])
   | _ -> None

let (|BlockCommentChunk|_|) origTokens = 
   match origTokens with 
   | BlockCommentToken t1 :: moreOrigTokens -> 
       let rec loop ts acc = 
           match ts with 
           | BlockCommentOrNewLineToken t2 :: ts2 -> loop ts2 (t2 :: acc)
           | _ -> List.rev acc, ts
       Some (loop moreOrigTokens [t1])
   | _ -> None  

/// Add a flag into the token stream indicating if the first token in 
/// the tokens of a line comment is sticky-to-the-left
///       text // comment
/// or sticky-to-the-right
///       // comment
///
let markStickiness (tokens: seq<Token * string>) = 
    seq { let inWhiteSpaceAtStartOfLine = ref true
          let inLineComment = ref true
          for (tio, tt) in tokens do 
             match tio with 
             | Token ti when ti.CharClass = TokenCharKind.LineComment ->
                  if !inLineComment then 
                      // Subsequent tokens in a line comment
                      yield Marked(tio, tt, NotApplicable)
                  else
                      // First token in a line comment. 
                      inLineComment := true
                      yield Marked(tio, tt, if !inWhiteSpaceAtStartOfLine then StickyRight else StickyLeft)
             
             // Comments can't be attached to delimiters
             | Token ti 
                  when !inWhiteSpaceAtStartOfLine 
                       && (ti.CharClass = TokenCharKind.WhiteSpace || ti.CharClass = TokenCharKind.Delimiter) ->
                  // Whitespace at start of line
                  yield Marked(tio, tt, NotApplicable)
             | Tok _ ->
                  // Some other token on a line
                  inWhiteSpaceAtStartOfLine := false
                  yield Marked(tio, tt, NotApplicable)
             | EOL -> 
                  // End of line marker
                 inLineComment := false
                 inWhiteSpaceAtStartOfLine := true
                 yield Marked(tio, tt, NotApplicable) }

let (|NewTokenAfterWhitespaceOrNewLine|_|) toks = 
    let rec loop toks acc = 
        match toks with
        | (EOL, tt) :: more -> loop more (tt::acc)
        | (Token tok, tt) :: more 
           when tok.CharClass = TokenCharKind.WhiteSpace && tok.ColorClass <> TokenColorKind.InactiveCode 
                && tok.ColorClass <> TokenColorKind.PreprocessorKeyword -> 
            loop more (tt::acc)
        | newTok :: more -> 
            Some(List.rev acc, newTok, more)
        | [] -> None
    loop toks []
 
/// Assume that originalText and newText are derived from the same AST. 
/// Pick all comments and directives from originalText to insert into newText               
let integrateComments (originalText : string) (newText : string) =
    let origTokens = tokenize (filterConstants originalText) originalText |> markStickiness |> Seq.toList
    let newTokens = tokenize [] newText |> Seq.toList

    let buffer = System.Text.StringBuilder()
    let column = ref 0
    let indent = ref 0

    let addText (text : string) = 
        buffer.Append text |> ignore
        if text = System.Environment.NewLine then column := 0
        else column := !column + text.Length

    let maintainIndent f =  
        let c = !column
        f()
        addText System.Environment.NewLine
        addText (String.replicate c " ")

    let saveIndent c =
        indent := c

    let restoreIndent f =
        let c = !indent
        Debug.WriteLine("set indent back to {0}", c)
        addText System.Environment.NewLine
        addText (String.replicate c " ")
        f()

    // Assume that starting whitespaces after EOL give indentation of a chunk
    let rec getIndent newTokens =
        match newTokens with
        | (Token _, _) :: moreNewTokens -> getIndent moreNewTokens
        | (EOL, _) :: moreNewTokens ->
            match moreNewTokens with
            | (Token origTok, origTokText) :: _ when origTok.CharClass = TokenCharKind.WhiteSpace -> 
                String.length origTokText
            | _ -> 0
        | _ -> 0
        
    let countStartingSpaces (lines: string []) = 
        if lines.Length = 0 then 0
        else
            Seq.min [ for line in lines -> line.Length - line.TrimStart(' ').Length ]

    let tokensMatch t1 t2 = 
        match t1, t2 with 
        | (Marked(Token origTok, origTokText, _), (Token newTok, newTokText)) -> 
            origTok.CharClass = newTok.CharClass && origTokText = newTokText
        // Use this pattern to avoid discrepancy between two versions of the same identifier
        | (Marked(Token origTok, origTokText, _), (Token newTok, newTokText)) -> 
            origTok.CharClass = TokenCharKind.Identifier && newTok.CharClass = TokenCharKind.Identifier 
            && DecompileOpName(origTokText.Trim('`')) = DecompileOpName(newTokText.Trim('`'))
        | _ -> false

    let rec loop origTokens newTokens = 
        match origTokens, newTokens with 
        | (Marked(Token origTok, _, _) :: moreOrigTokens),  _ 
            when origTok.CharClass = TokenCharKind.WhiteSpace && origTok.ColorClass <> TokenColorKind.InactiveCode 
                && origTok.ColorClass <> TokenColorKind.PreprocessorKeyword ->
            Debug.WriteLine "dropping whitespace from orig tokens" 
            loop moreOrigTokens newTokens 

        | (Marked(EOL, _, _) :: moreOrigTokens),  _ ->
            Debug.WriteLine "dropping newline from orig tokens" 
            loop moreOrigTokens newTokens
        
        // Inject #if... #else or #endif directive
        // These directives could occur inside an inactive code chunk
        // Assume that only #endif directive follows by an EOL 
        | (PreprocessorDirectiveChunk (tokensText, moreOrigTokens)), newTokens ->            
            let text = String.concat "" tokensText
            Debug.WriteLine("injecting preprocessor directive '{0}'", text)
            addText System.Environment.NewLine
            for x in tokensText do addText x
            let moreNewTokens =
                if text.StartsWith("#endif") then
                    match newTokens with
                    | WhiteSpaces(ws, moreNewTokens) ->
                        // There are some whitespaces, use them up
                        for s in ws do addText s
                        moreNewTokens
                    | _ :: _ ->
                        // This fixes the case where newTokens advance too fast
                        // and emit whitespaces even before #endif 
                        restoreIndent id
                        newTokens
                    | [] -> []
                else if text.StartsWith("#if") then
                    // Save current indentation for #else branch
                    let indent = getIndent newTokens 
                    saveIndent indent
                    newTokens
                else newTokens
            loop moreOrigTokens moreNewTokens

        // Inject inactive code
        // These chunks come out from any #else branch in our scenarios
        | (InactiveCodeChunk (tokensText, moreOrigTokens)),  _ ->
            Debug.WriteLine("injecting inactive code '{0}'", String.concat "" tokensText)
            let text = String.concat "" tokensText
            let lines = text.Replace("\r\n", "\n").Split([|'\r'; '\n'|], StringSplitOptions.RemoveEmptyEntries)
            // What is current indentation of this chunk
            let numSpaces = countStartingSpaces lines
            Debug.WriteLine("the number of starting spaces is {0}", numSpaces)
            // Write the chunk in the same indentation with #if branch
            for line in lines do
                if line.[numSpaces..].StartsWith("#") then
                    // Naive recognition of inactive preprocessors
                    addText Environment.NewLine
                    addText line.[numSpaces..]
                else
                    restoreIndent (fun () -> addText line.[numSpaces..])
            loop moreOrigTokens newTokens

        | (LineCommentChunk true (commentTokensText, moreOrigTokens)),  _ ->
            let tokText = String.concat "" commentTokensText
            Debug.WriteLine("injecting sticky-to-the-left line comment '{0}'", tokText)
              
            match newTokens with 
            // If there is a new line coming, use it up
            | ((EOL, newTokText) :: moreNewTokens) ->
                addText " "
                for x in commentTokensText do addText x
                Debug.WriteLine "emitting newline for end of sticky-to-left comment" 
                addText newTokText 
                loop moreOrigTokens moreNewTokens 
            // Otherwise, if there is a token coming, maintain the indentation
            | _ -> 
                addText " "
                maintainIndent (fun () -> for x in commentTokensText do addText x)
                loop moreOrigTokens newTokens 

        // Inject line commment that is sticky-to-the-left, e.g. 
        //   let f x = 
        //       x + x  // HERE
        // Because it is sticky-to-the-left, we do it _before_ emitting end-of-line from the newText
        | (LineCommentChunk true (commentTokensText, moreOrigTokens)),  _ ->
            Debug.WriteLine("injecting stick-to-the-left line comment '{0}'", String.concat "" commentTokensText)
            addText " "
            for x in commentTokensText do addText x
            loop moreOrigTokens newTokens 

        // Emit end-of-line from new tokens
        | _,  ((EOL, newTokText) :: moreNewTokens) ->
            Debug.WriteLine "emitting newline in new tokens" 
            addText newTokText 
            loop origTokens moreNewTokens 

        | _,  ((Token newTok, newTokText) :: moreNewTokens) 
            when newTok.CharClass = TokenCharKind.WhiteSpace && newTok.ColorClass <> TokenColorKind.InactiveCode ->
            Debug.WriteLine("emitting whitespace '{0}' in new tokens", newTokText)
            addText newTokText 
            loop origTokens moreNewTokens 

        | _,  ((_, newTokText) :: moreNewTokens) 
            when newTokText = ";" || newTokText = ";;" ->
            Debug.WriteLine("emitting non-matching '{0}' in new tokens", newTokText)
            addText newTokText 
            loop origTokens moreNewTokens 

        // Inject line commment, after all whitespace and newlines emitted, so
        // the line comment will appear just before the subsequent text, e.g. 
        //   let f x = 
        //       // HERE
        //       x + x
        | (LineCommentChunk false (commentTokensText, moreOrigTokens)),  _ ->
            Debug.WriteLine("injecting line comment '{0}'", String.concat "" commentTokensText)
            maintainIndent (fun () -> for x in commentTokensText do addText x)
            loop moreOrigTokens newTokens 

        // Inject block commment 
        | (BlockCommentChunk (commentTokensText, moreOrigTokens)),  _ ->
            Debug.WriteLine("injecting block comment '{0}'", String.concat "" commentTokensText)
            let comments = String.concat "" commentTokensText
            if comments.IndexOf('\n') = -1 then
                // This is an inline block comment
                addText comments
                addText " "
            else
                let len = List.length commentTokensText
                maintainIndent (fun () -> 
                    commentTokensText |> List.iteri (fun i x ->
                        // Drop the last newline 
                        if i = len - 1 && x = Environment.NewLine then ()
                        else addText x))
            loop moreOrigTokens newTokens 

        // Consume attributes in the new text
        | _, RawAttribute(newTokensText, moreNewTokens) ->
            Debug.WriteLine("no matching of attribute tokens")
            for x in newTokensText do addText x
            loop origTokens moreNewTokens
              
        // Skip attributes in the old text
        | (Attribute (tokensText, moreOrigTokens)), _ ->
            Debug.WriteLine("skip matching of attribute tokens '{0}'", tokensText)            
            loop moreOrigTokens newTokens   

        // Matching tokens
        | (origTok :: moreOrigTokens), (newTok :: moreNewTokens) when tokensMatch origTok newTok ->
            Debug.WriteLine("matching token '{0}'", origTok.Text)
            addText (snd newTok)
            loop moreOrigTokens moreNewTokens 

        // Matching tokens, after one new token, compensating for insertions of "|", ";" and others
        | (origTok :: moreOrigTokens), (newTok1 :: NewTokenAfterWhitespaceOrNewLine(whiteTokens, newTok2, moreNewTokens)) 
            when tokensMatch origTok newTok2 ->
            Debug.WriteLine("fresh non-matching new token '%s'", snd newTok1)
            addText (snd newTok1)
            Debug.WriteLine("matching token '{0}' (after one fresh new token)", snd newTok2)
            for x in whiteTokens do addText x
            addText (snd newTok2)
            loop moreOrigTokens moreNewTokens 

        // Not a comment, drop the original token text until something matches
        | (origTok :: moreOrigTokens), _ ->
            Debug.WriteLine("dropping '{0}' from original text", origTok.Text)
            loop moreOrigTokens newTokens 

        // Dangling text at the end 
        | [], ((_, newTokText) :: moreNewTokens) ->
            Debug.WriteLine("dangling new token '{0}'", newTokText)
            addText newTokText 
            loop [] moreNewTokens 

        // Dangling input text - extra comments or whitespace
        | (Marked(origTok, origTokText, _) :: moreOrigTokens), [] ->
            Debug.WriteLine("dropping dangling old token '{0}'", origTokText)
            loop moreOrigTokens [] 

        | [], [] -> 
            ()

    loop origTokens newTokens 
    buffer.ToString()
            


