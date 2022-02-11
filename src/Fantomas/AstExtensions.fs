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
    |> Option.bind (fun lastAttribute ->
        if lastAttribute.Range.EndLine + 1 < firstNodeRange.StartLine
           && firstNodeRange.Start.Column > 0 then
            let pos = Position.mkPos firstNodeRange.StartLine 0

            mkRange fileName pos pos |> Some
        else
            None)

type SynModuleDecl with
    member this.AfterAttributesBeforeNestedModuleName: Range option =
        match this with
        | SynModuleDecl.NestedModule(moduleInfo = SynComponentInfo(attributes = [])) -> None
        | SynModuleDecl.NestedModule(moduleInfo = SynComponentInfo (attributes = attrs; longId = lid :: _)) ->
            hasLinesBetweenAttributesAndFirstNode attrs lid.idRange
        | _ -> None

type SynModuleSigDecl with
    member this.AfterAttributesBeforeNestedModuleName: Range option =
        match this with
        | SynModuleSigDecl.NestedModule(moduleInfo = SynComponentInfo(attributes = [])) -> None
        | SynModuleSigDecl.NestedModule(moduleInfo = SynComponentInfo (attributes = attrs; longId = lid :: _)) ->
            hasLinesBetweenAttributesAndFirstNode attrs lid.idRange
        | _ -> None
