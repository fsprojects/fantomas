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
