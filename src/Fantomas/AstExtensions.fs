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

// TODO: Remove when https://github.com/dotnet/fsharp/pull/12441 is part of FCS
type SynExceptionDefnRepr with
    member this.FullRange: range =
        match this with
        | SynExceptionDefnRepr (attributes = attrs; range = m) ->
            match attrs with
            | h :: _ -> mkRange m.FileName h.Range.Start m.End
            | _ -> m

type SynExceptionSig with
    member this.FullRange: range =
        match this with
        | SynExceptionSig (exnRepr = exnRepr; members = members; range = m) ->
            match List.tryLast members with
            | Some lastMember -> mkRange m.FileName exnRepr.FullRange.Start lastMember.Range.End
            | None -> exnRepr.FullRange

type SynModuleSigDecl with
    member this.FullRange: Range =
        match this with
        | SynModuleSigDecl.Exception (SynExceptionSig _ as ses, _) -> ses.FullRange
        | _ -> this.Range
