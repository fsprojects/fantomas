module internal Fantomas.TriviaContext

open Fantomas
open Fantomas.Context
open Fantomas.TriviaTypes
open FSharp.Compiler.Text

let tokN (range: Range) (tokenName: FsTokenType) f =
    enterNodeTokenByName range tokenName
    +> f
    +> leaveNodeTokenByName range tokenName

let getIndentBetweenTicksFromSynPat patRange fallback ctx =
    TriviaHelpers.getNodesForTypes [ SynPat_LongIdent; SynPat_Named ] ctx.TriviaMainNodes
    |> List.choose (fun t ->
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
