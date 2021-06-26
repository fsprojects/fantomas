module Fantomas.AstExtensions

open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Compiler.Text.Range

let longIdentFullRange (li: LongIdent) : Range =
    match li with
    | [] -> range.Zero
    | h :: _ -> unionRanges h.idRange (List.last li).idRange

let private hasLinesBetweenAttributesAndFirstNode (attributes: SynAttributes) (firstNodeRange: Range) : Range option =
    let fileName = firstNodeRange.FileName

    List.tryLast attributes
    |> Option.bind
        (fun lastAttribute ->
            if lastAttribute.Range.EndLine + 1 < firstNodeRange.StartLine
               && firstNodeRange.Start.Column > 0 then
                let pos =
                    Position.mkPos firstNodeRange.StartLine 0

                mkRange fileName pos pos |> Some
            else
                None)

type SynBinding with
    /// Construct an artificial range after the attributes and before the head pattern.
    /// This is to detect newline or comment trivia in that exact location.
    member this.AfterAttributesBeforeHeadPattern: Range option =
        match this with
        | SynBinding(attributes = []) -> None
        | SynBinding (attributes = attrs; headPat = pat) -> hasLinesBetweenAttributesAndFirstNode attrs pat.Range

type SynTypeDefn with
    member this.AfterAttributesBeforeComponentInfo: Range option =
        match this with
        | SynTypeDefn(typeInfo = SynComponentInfo(attributes = [])) -> None
        | SynTypeDefn(typeInfo = SynComponentInfo (attributes = attrs; range = compRange)) ->
            hasLinesBetweenAttributesAndFirstNode attrs compRange

type SynField with
    member this.AfterAttributesBeforeIdentifier: Range option =
        match this with
        | SynField(attributes = []) -> None
        | SynField (attributes = _ :: _; idOpt = None) -> None
        | SynField (attributes = attrs; idOpt = Some ident) -> hasLinesBetweenAttributesAndFirstNode attrs ident.idRange
