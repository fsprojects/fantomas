module Fantomas.AstExtensions

open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Compiler.Text.Range

// TODO: verify if this still is necessary?
type SynTypeDefnSig with
    /// Combines the range of type name and the body.
    member this.FullRange: Range =
        match this with
        | SynTypeDefnSig (comp, _, _, r) -> mkRange r.FileName comp.Range.Start r.End

let longIdentFullRange (li: LongIdent) : Range =
    match li with
    | [] -> range.Zero
    | h :: _ -> unionRanges h.idRange (List.last li).idRange

type SynBinding with
    /// Construct an artificial range after the attributes and before the head pattern.
    /// This is to detect newline or comment trivia in that exact location.
    member this.AfterAttributesBeforeHeadPattern : Range option =
        match this with
        | SynBinding(attributes = []) -> None
        | SynBinding (attributes = attrs; headPat = pat) ->
            let lastAttribute = List.last attrs
            let fileName = pat.Range.FileName

            if lastAttribute.Range.EndLine + 1 < pat.Range.StartLine
               && pat.Range.Start.Column > 0 then
                let pos = Position.mkPos pat.Range.StartLine 0
                mkRange fileName pos pos |> Some
            else
                None
