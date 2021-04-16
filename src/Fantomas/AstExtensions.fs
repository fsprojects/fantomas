module Fantomas.AstExtensions

open FSharp.Compiler.SyntaxTree
open FSharp.Compiler.Text
open FSharp.Compiler.Text.Range

type SynTypeDefnSig with
    /// Combines the range of type name and the body.
    member this.FullRange : Range =
        match this with
        | SynTypeDefnSig.TypeDefnSig (comp, _, _, r) -> mkRange r.FileName comp.Range.Start r.End

let longIdentFullRange (li: LongIdent) : Range =
    match li with
    | [] -> range.Zero
    | h :: _ -> unionRanges h.idRange (List.last li).idRange
