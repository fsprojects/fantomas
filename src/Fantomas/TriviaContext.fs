module internal Fantomas.TriviaContext

open FSharp.Compiler.Ast
open Fantomas
open Fantomas.Context
open Fantomas.TriviaTypes

let firstNewline (es: SynExpr list) (ctx: Context) =
    es
    |> List.tryHead
    |> Option.bind (fun e -> TriviaHelpers.findByRange ctx.Trivia e.Range)
    |> fun cb ->
        match cb with
        | Some ({ ContentBefore = TriviaContent.Newline::rest } as tn) ->
            let updatedTriviaNodes =
                ctx.Trivia
                |> List.map (fun t ->
                    if t = tn then
                        { tn with ContentBefore = rest }
                    else t
                )

            let ctx' = { ctx with Trivia = updatedTriviaNodes }
            printTriviaContent Newline ctx'
        | _ -> sepNone ctx

