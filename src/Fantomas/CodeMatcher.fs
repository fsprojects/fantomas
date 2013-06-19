module Fantomas.CodeMatcher

open System.Diagnostics
open Microsoft.FSharp.Compiler.SourceCodeServices

type Token = 
   | EOL
   | Token of TokenInformation

let tokenize fileName (content:string) =
    seq { let sourceTokenizer = SourceTokenizer([ ], fileName)
          let lines = content.Replace("\r\n","\n").Split('\r', '\n')
          let lexState = ref 0L
          for line in lines do 
              let lineTokenizer = sourceTokenizer.CreateLineTokenizer line
              let finLine = ref false
              while not finLine.Value do
                  let tok, newLexState = lineTokenizer.ScanToken(lexState.Value)
                  lexState := newLexState
                  match tok with 
                  | None -> 
                      yield (EOL, System.Environment.NewLine) // new line
                      finLine := true
                  | Some t -> 
                      yield (Token t, line.[t.LeftColumn..t.RightColumn]) }

type LineCommentStickiness = | StickyLeft | StickyRight | NotApplicable

type MarkedToken = 
    | Marked of Token * string * LineCommentStickiness
    member x.Text = (let (Marked(_,t,_)) = x in t)

let (|PreprocessorKeywordToken|_|) requiredText t = 
    match t with
    | Marked(Token (origTok:TokenInformation), origTokText, _) 
        when  origTok.ColorClass = TokenColorKind.PreprocessorKeyword && origTokText = requiredText  -> Some origTokText
    | _ -> None

let (|InactiveCodeToken|_|) t = 
    match t with
    | Marked(Token (origTok:TokenInformation), origTokText, _) 
        when  origTok.ColorClass = TokenColorKind.InactiveCode   -> Some origTokText
    | _ -> None

let (|LineCommentToken|_|) wantStickyLeft t = 
    match t with
    | Marked(Token (origTok:TokenInformation), origTokText, lcs) 
        when (not wantStickyLeft || (lcs = StickyLeft)) && 
             origTok.CharClass = TokenCharKind.LineComment  -> Some origTokText
    | _ -> None

let (|BlockCommentToken|_|) t = 
    match t with
    | Marked(Token (origTok:TokenInformation), origTokText, _) when  origTok.CharClass = TokenCharKind.Comment  -> Some origTokText
    | _ -> None

let (|NewLineToken|_|) t = 
    match t with
    | Marked(EOL, tokText, _) -> Some tokText
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
     Marked(Token ti2,t2,_) ::
     Marked(Token ti3,t3,_) ::
     Marked(EOL,t4,_) ::
     moreOrigTokens 
         when ti2.TokenName = "WHITESPACE" && ti3.TokenName = "IDENT"  -> 
        Some ([t1;t2;t3;t4], moreOrigTokens)

   | PreprocessorKeywordToken "#else" t1 :: 
     Marked(EOL,t2,_) ::
     moreOrigTokens -> Some ([t1;t2], moreOrigTokens)

   | PreprocessorKeywordToken "#endif" t1 :: 
     Marked(EOL,t2,_) ::
     moreOrigTokens -> Some ([t1;t2], moreOrigTokens)

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
          for (tio,tt) in tokens do 
             match tio with 
             | Token ti when ti.CharClass = TokenCharKind.LineComment ->
                  if inLineComment.Value then 
                      // Subsequent tokens in a line comment
                      yield Marked(tio,tt,NotApplicable)
                  else
                      // First token in a line comment. 
                      inLineComment := true
                      yield Marked(tio, tt, if inWhiteSpaceAtStartOfLine.Value then StickyRight else StickyLeft)

             | Token ti when inWhiteSpaceAtStartOfLine.Value && ti.CharClass = TokenCharKind.WhiteSpace  ->
                  // Whitespace at start of line
                  yield Marked(tio,tt,NotApplicable)
             | Token ti ->
                  // Some other token on a line
                  inWhiteSpaceAtStartOfLine := false
                  yield Marked(tio,tt,NotApplicable)
             | EOL -> 
                  // End of line marker
                 inLineComment := false
                 inWhiteSpaceAtStartOfLine := true
                 yield Marked(tio,tt,NotApplicable) }

let (|NewTokenAfterWhitespaceOrNewLine|_|) toks = 
    let rec loop toks acc = 
        match toks with
        | (EOL, tt) :: more -> loop more (tt::acc)
        | (Token tok, tt) :: more  when tok.CharClass = TokenCharKind.WhiteSpace && tok.ColorClass <> TokenColorKind.InactiveCode  && tok.ColorClass <> TokenColorKind.PreprocessorKeyword -> 
            loop more (tt::acc)
        | newTok :: more -> 
            Some(List.rev acc, newTok, more)
        | [] -> None
    loop toks []
                
let integrateComments (originalText : string) (newText : string) =
    let origTokens = tokenize "UNIQUE.fsx" originalText |> markStickiness |> Seq.toList
    let newTokens = tokenize "UNIQUE.fsx" newText  |> Seq.toList

    let buffer = System.Text.StringBuilder()
    let column = ref 0
    let addText (text:string) = 
        buffer.Append text |> ignore
        if text = System.Environment.NewLine then column := 0 else column := column.Value + text.Length

    let maintainIndent f =  
        let c = column.Value
        f()
        addText System.Environment.NewLine
        addText (String.replicate c " ")

    let tokensMatch t1 t2 = 
        match t1, t2 with 
        | (Marked(Token origTok, origTokText, _), (Token newTok, newTokText)) -> 
            origTok.CharClass = newTok.CharClass &&  origTokText = newTokText 
        | _ -> false

    let rec loop origTokens newTokens = 
        match origTokens, newTokens with 
        | (Marked(Token origTok, origTokText, _) :: moreOrigTokens),  _ 
              when  origTok.CharClass = TokenCharKind.WhiteSpace && origTok.ColorClass <> TokenColorKind.InactiveCode  && origTok.ColorClass <> TokenColorKind.PreprocessorKeyword ->
              Debug.WriteLine "dropping whitespace from orig tokens" 
              loop moreOrigTokens newTokens 


        | (Marked(EOL, origTokText, _) :: moreOrigTokens),  _ ->
              Debug.WriteLine "dropping newline from orig tokens" 
              loop moreOrigTokens newTokens 

        | (LineCommentChunk true (commentTokensText, moreOrigTokens)),  _ ->
              let tokText = String.concat "" commentTokensText
              Debug.WriteLine("injecting sticky-to-the-left line comment '{0}'", tokText)
              
              match newTokens with 
              // If there is a new line coming, use it up
              | ((EOL, newTokText) :: moreNewTokens) ->
                  addText " "
                  addText tokText
                  Debug.WriteLine "emitting newline for end of sticky-to-left comment" 
                  addText newTokText 
                  loop moreOrigTokens moreNewTokens 
              // Otherwise, if there is a token coming, maintain the indentation
              | _ -> 
                  addText " "
                  maintainIndent (fun () -> addText tokText)
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
              when  newTok.CharClass = TokenCharKind.WhiteSpace && newTok.ColorClass <> TokenColorKind.InactiveCode  ->
              Debug.WriteLine("emitting whitespace '{0}' in new tokens", newTokText)
              addText newTokText 
              loop origTokens moreNewTokens 

        | _,  ((_, newTokText) :: moreNewTokens) 
              when  newTokText = ";" || newTokText = "|" || newTokText = ">]"->
              Debug.WriteLine("emitting non-matching '{0}' in new tokens", newTokText)
              addText newTokText 
              loop origTokens moreNewTokens 

        // inject line commment, after all whitespace and newlines emitted, so
        // the line comment will appear just before the subsequent text, e.g. 
        //   let f x = 
        //       // HERE
        //       x + x
        | (LineCommentChunk false (commentTokensText, moreOrigTokens)),  _ ->
              Debug.WriteLine("injecting line comment '{0}'", String.concat "" commentTokensText)
              maintainIndent (fun () -> for x in commentTokensText do addText x)
              loop moreOrigTokens newTokens 

        // inject block commment 
        | (BlockCommentChunk (commentTokensText, moreOrigTokens)),  _ ->
              Debug.WriteLine("injecting block comment '{0}'", String.concat "" commentTokensText)
              maintainIndent (fun () -> for x in commentTokensText do addText x)
              loop moreOrigTokens newTokens 

        // inject inactive code
        | (InactiveCodeChunk (tokensText, moreOrigTokens)),  _ ->
              Debug.WriteLine("injecting inactive code '{0}'", String.concat "" tokensText)
              for x in tokensText do addText x
              loop moreOrigTokens newTokens 

        // inject #if... #else or #endif directive 
        | (PreprocessorDirectiveChunk (tokensText, moreOrigTokens)),  _ ->
              let text = (String.concat "" tokensText)
              Debug.WriteLine("injecting preprocessor directive '{0}'", text)
              if text.StartsWith "#if" then 
                  addText System.Environment.NewLine
              addText text
              if text.StartsWith "#endif" then 
                  addText System.Environment.NewLine
              loop moreOrigTokens newTokens 

        // Matching tokens
        | (origTok :: moreOrigTokens), (newTok :: moreNewTokens) when tokensMatch origTok newTok ->
              Debug.WriteLine("matching token '{0}'", origTok.Text)
              addText (snd newTok)
              loop moreOrigTokens moreNewTokens 

(*
        // Matching tokens, after one new token, compensating for insertions of "|", ";" and others
        | (origTok :: moreOrigTokens), (newTok1 :: NewTokenAfterWhitespaceOrNewLine(whiteTokens, newTok2, moreNewTokens)) when tokensMatch origTok newTok2 ->
              Debug.WriteLine "fresh non-matching new token '%s'" (snd newTok1)
              addText (snd newTok1)
              Debug.WriteLine("matching token '{0}' (after one fresh new token)", snd newTok2)
              for x in whiteTokens do addText x
              addText (snd newTok2)
              loop moreOrigTokens moreNewTokens 
*)

        // not a comment, drop the original token text until something matches
        | (origTok :: moreOrigTokens), _ ->
            Debug.WriteLine("dropping '{0}' from original text", origTok.Text)
            loop moreOrigTokens newTokens 

        // Dangling text at the end 
        | [], ((newTok, newTokText) :: moreNewTokens) ->
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
            


