module internal Fantomas.TriviaContext

open Fantomas
open Fantomas.Context
open Fantomas.TriviaTypes
open FSharp.Compiler.Text

let tokN (range: Range) (tokenName: FsTokenType) f =
    enterNodeTokenByName range tokenName
    +> f
    +> leaveNodeTokenByName range tokenName

let sepOpenTFor r = tokN r LPAREN sepOpenT

let sepCloseTFor rpr pr =
    tokN (Option.defaultValue pr rpr) RPAREN sepCloseT

let genArrowWithTrivia (bodyExpr: Context -> Context) (range: Range) =
    (tokN range RARROW sepArrow)
    +> (fun ctx ->
        if String.isNotNullOrEmpty ctx.WriterModel.WriteBeforeNewline then
            (indent +> sepNln +> bodyExpr +> unindent) ctx
        else
            (autoIndentAndNlnIfExpressionExceedsPageWidth bodyExpr) ctx)

let ``else if / elif`` (rangeOfIfThenElse: Range) (ctx: Context) =
    let keywords =
        [ yield! (Map.tryFindOrEmptyList ELSE ctx.TriviaTokenNodes)
          yield! (Map.tryFindOrEmptyList IF ctx.TriviaTokenNodes)
          yield! (Map.tryFindOrEmptyList ELIF ctx.TriviaTokenNodes) ]
        |> List.sortBy (fun tn -> tn.Range.StartLine, tn.Range.StartColumn)
        |> TriviaHelpers.``keyword token inside range`` rangeOfIfThenElse
        |> List.map (fun (tok, t) -> (TokenParser.getFsToken tok.TokenInfo.TokenName, t))

    let resultExpr =
        match keywords with
        | (ELSE, elseTrivia) :: (IF, ifTrivia) :: _ ->
            printContentBefore elseTrivia
            +> (!- "else")
            +> printContentAfter elseTrivia
            +> sepNlnWhenWriteBeforeNewlineNotEmpty sepSpace
            +> printContentBefore ifTrivia
            +> (!- "if ")
            +> printContentAfter ifTrivia
        | (ELIF, elifTok) :: _
        | [ (ELIF, elifTok) ] ->
            printContentBefore elifTok
            +> (!- "elif ")
            +> printContentAfter elifTok
        | [] ->
            // formatting from AST
            !- "else if "
        | _ ->
            failwithf
                "Unexpected scenario when formatting else if / elif (near %A), please open an issue via https://fsprojects.github.io/fantomas-tools/#/fantomas/preview"
                rangeOfIfThenElse

    resultExpr ctx

let getIndentBetweenTicksFromSynPat patRange fallback ctx =
    TriviaHelpers.getNodesForTypes [ SynPat_LongIdent; SynPat_Named ] ctx.TriviaMainNodes
    |> List.choose
        (fun t ->
            match t.Range = patRange with
            | true ->
                match t.ContentItself with
                | Some (IdentBetweenTicks iiw) -> Some iiw
                | Some (IdentOperatorAsWord iow) -> Some iow
                | _ -> None
            | _ -> None)
    |> List.tryHead
    |> fun iiw ->
        match iiw with
        | Some iiw -> !- iiw ctx
        | None -> !- fallback ctx
