module Fantomas.Analyzers.XmlDocAnalyzer

open FSharp.Analyzers.SDK
open FSharp.Analyzers.SDK.ASTCollecting
open FSharp.Compiler.CodeAnalysis
open FSharp.Compiler.Symbols
open FSharp.Compiler.Syntax
open FSharp.Compiler.Text
open FSharp.Compiler.Xml
open Fantomas.Analyzers.Common

[<Literal>]
let Code: string = "FANTOMAS-XMLDOC-001"

[<Literal>]
let Name: string = "XmlDocAnalyzer"

[<Literal>]
let ShortDescription: string =
    "Detects a documentation comment that is duplicated in an implementation file and its signature file, where the signature is the copy readers and tooling see."

[<Literal>]
let HelpUri: string =
    "https://github.com/fsprojects/fantomas/blob/main/analyzers/AGENTS.md#fantomas-xmldoc-001"

// The name a declaration introduces, and where that name sits.
//
// The doc comment's own range is what gets reported, but the name is what answers whether the
// signature file declares the same thing, so both travel together.
[<NoComparison; NoEquality>]
type DocumentedDeclaration =
    {
        DocRange: range
        Name: string
        NameRange: range
    }

// The last identifier of a pattern is the name a `let` introduces. A pattern that introduces
// several names, or none, is not a documented declaration this rule can speak for.
let bindingName (pat: SynPat) : Ident option =
    match pat with
    | SynPat.Named(ident = SynIdent(ident, _)) -> Some ident
    | SynPat.LongIdent(longDotId = SynLongIdent(id = ids)) -> List.tryLast ids
    | _ -> None

/// Whether the signature file declares the same thing the implementation documents.
///
/// `SignatureLocation` is not the yes or no it reads as: for a symbol the signature does not carry
/// it falls back to the declaration itself, so it is `Some` for every symbol and asking `IsSome`
/// answers nothing. What separates the two is which file it points at. A symbol the signature
/// declares points into the `.fsi`; one it does not points back at the `.fs` it was declared in.
///
/// A symbol that cannot be resolved is treated as not declared in the signature. Reporting it would
/// put the burden of proof on the wrong side: the rule is about a doc comment that exists twice, and
/// nothing here says this one does.
let declaredInSignature
    (checkResults: FSharpCheckFileResults)
    (sourceText: ISourceText)
    (declaration: DocumentedDeclaration)
    : bool =
    let line: int = declaration.NameRange.EndLine

    if line < 1 || line > sourceText.GetLineCount() then
        false
    else
        let lineText: string = sourceText.GetLineString(line - 1)

        checkResults.GetSymbolUseAtLocation(line, declaration.NameRange.EndColumn, lineText, [ declaration.Name ])
        |> Option.map (fun (symbolUse: FSharpSymbolUse) ->
            match symbolUse.Symbol.SignatureLocation, symbolUse.Symbol.DeclarationLocation with
            | Some signature, Some declaration -> signature.FileName <> declaration.FileName
            | _ -> false)
        |> Option.defaultValue false

// Documentation comments that the signature file already carries.
//
// The untyped tree hangs a `PreXmlDoc` off every declaration that can carry one, so finding them
// needs no trivia and no second parse. Deciding which of them are duplicated does need the symbol,
// because the name in the implementation is all the tree has to go on.
let analyze
    (checkResults: FSharpCheckFileResults)
    (sourceText: ISourceText)
    (fileName: string)
    (sourceFiles: string list)
    (parsedInput: ParsedInput)
    : Message list =
    if not (hasSignatureFile fileName sourceFiles) then
        []
    else

        let documented: ResizeArray<DocumentedDeclaration> =
            ResizeArray<DocumentedDeclaration>()

        let collect (doc: PreXmlDoc) (name: Ident option) : unit =
            match name with
            | None -> ()
            | Some ident ->
                if not doc.IsEmpty then
                    documented.Add
                        {
                            DocRange = doc.Range
                            Name = ident.idText
                            NameRange = ident.idRange
                        }

        let walker: SyntaxCollectorBase =
            { new SyntaxCollectorBase() with
                override _.WalkBinding(_path: SyntaxVisitorPath, binding: SynBinding) : unit =
                    match binding with
                    | SynBinding(xmlDoc = doc; headPat = pat) -> collect doc (bindingName pat)

                override _.WalkComponentInfo(_path: SyntaxVisitorPath, info: SynComponentInfo) : unit =
                    match info with
                    | SynComponentInfo(xmlDoc = doc; longId = ids) -> collect doc (List.tryLast ids)

                override _.WalkUnionCase(_path: SyntaxVisitorPath, unionCase: SynUnionCase) : unit =
                    match unionCase with
                    | SynUnionCase(xmlDoc = doc; ident = SynIdent(ident, _)) -> collect doc (Some ident)

                override _.WalkEnumCase(_path: SyntaxVisitorPath, enumCase: SynEnumCase) : unit =
                    match enumCase with
                    | SynEnumCase(xmlDoc = doc; ident = SynIdent(ident, _)) -> collect doc (Some ident)

                override _.WalkField(_path: SyntaxVisitorPath, field: SynField) : unit =
                    match field with
                    | SynField(xmlDoc = doc; idOpt = idOpt) -> collect doc idOpt
            }

        walkAst walker parsedInput

        documented
        |> Seq.filter (declaredInSignature checkResults sourceText)
        |> Seq.map (fun (declaration: DocumentedDeclaration) ->
            {
                Type = Name
                Message =
                    "Move this documentation comment to the signature file. Keeping a copy in both is a second one to keep in step, and the signature is the one readers and tooling see."
                Code = Code
                Severity = Severity.Warning
                Range = declaration.DocRange
                Fixes = []
            })
        |> Seq.toList

let cliAnalyzer (ctx: CliContext) : Async<Message list> =
    async {
        return
            analyze
                ctx.CheckFileResults
                ctx.SourceText
                ctx.FileName
                ctx.ProjectOptions.SourceFiles
                ctx.ParseFileResults.ParseTree
    }

// Without check results there is no symbol to ask, and the rule cannot tell a duplicated doc
// comment from one that only exists here. It says nothing rather than guessing.
let editorAnalyzer (ctx: EditorContext) : Async<Message list> =
    async {
        match ctx.CheckFileResults with
        | None -> return []
        | Some checkResults ->
            return
                analyze
                    checkResults
                    ctx.SourceText
                    ctx.FileName
                    ctx.ProjectOptions.SourceFiles
                    ctx.ParseFileResults.ParseTree
    }
