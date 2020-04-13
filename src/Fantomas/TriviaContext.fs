module internal Fantomas.TriviaContext

open Fantomas
open Fantomas.Context
open Fantomas.TriviaTypes
open FSharp.Compiler.Range
open FSharp.Compiler.SyntaxTree

let node (range: range) (s: string) =
    enterNode range +> (!-s) +> leaveNode range

let tok (range: range) (s: string) =
    enterNodeToken range +> (!-s) +> leaveNodeToken range

let tokN (range: range) (tokenName: string) f =
    enterNodeTokenByName range tokenName +> f +> leaveNodeTokenByName range tokenName

let firstNewlineOrComment (es: SynExpr list) (ctx: Context) =
    es
    |> List.tryHead
    |> Option.bind (fun e -> TriviaHelpers.findByRange ctx.Trivia e.Range)
    |> fun cb ->
        match cb with
        | Some ({ ContentBefore = (TriviaContent.Newline|TriviaContent.Comment _) as head ::rest } as tn) ->
            let updatedTriviaNodes =
                ctx.Trivia
                |> List.map (fun t ->
                    if t = tn then
                        { tn with ContentBefore = rest }
                    else t
                )

            let ctx' = { ctx with Trivia = updatedTriviaNodes }
            printTriviaContent head ctx'
        | _ -> sepNone ctx

let triviaAfterArrow (range: range) (ctx: Context) =
    let hasCommentAfterArrow =
        findTriviaTokenFromName range ctx.Trivia "RARROW"
        |> Option.bind (fun t ->
            t.ContentAfter
            |> List.tryFind (function | Comment(LineCommentAfterSourceCode(_)) -> true | _ -> false)
        )
        |> Option.isSome
    ((tokN range "RARROW" sepArrow) +> ifElse hasCommentAfterArrow sepNln sepNone) ctx

let ``else if / elif`` (rangeOfIfThenElse: range) (ctx: Context) =
    let keywords =
        ctx.Trivia
        |> TriviaHelpers.``keyword tokens inside range`` ["ELSE";"IF";"ELIF"] rangeOfIfThenElse
        |> List.map (fun (tok, t) -> (tok.TokenName, t))

    let resultExpr =
        match keywords with
        | ("ELSE", elseTrivia)::("IF", ifTrivia)::_ ->
            let commentAfterElseKeyword = TriviaHelpers.``has line comment after`` elseTrivia
            let commentAfterIfKeyword = TriviaHelpers.``has line comment after`` ifTrivia
            let triviaBeforeIfKeyword =
                ctx.Trivia
                |> List.filter (fun t ->
                    match t.Type with
                    | MainNode("SynExpr.IfThenElse") ->
                        RangeHelpers.``range contains`` rangeOfIfThenElse t.Range
                        && (RangeHelpers.``range after`` elseTrivia.Range t.Range)
                    | _ -> false
                    )
                |> List.tryHead

            tokN rangeOfIfThenElse "ELSE" (!- "else") +>
            ifElse commentAfterElseKeyword sepNln sepSpace +>
            opt sepNone triviaBeforeIfKeyword printContentBefore +>
            tokN rangeOfIfThenElse "IF" (!- "if ") +>
            ifElse commentAfterIfKeyword (indent +> sepNln) sepNone

        | ("ELIF",elifTok)::_
        | [("ELIF",elifTok)] ->
            let commentAfterElIfKeyword = TriviaHelpers.``has line comment after`` elifTok
            tokN rangeOfIfThenElse "ELIF" (!- "elif ")
            +> ifElse commentAfterElIfKeyword (indent +> sepNln) sepNone

        | [] ->
            // formatting from AST
            !- "else if "

        | _ ->
            failwith "Unexpected scenario when formatting else if / elif, please open an issue via https://jindraivanek.gitlab.io/fantomas-ui"

    resultExpr ctx