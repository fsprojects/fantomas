(*---
fsharp_multiline_bracket_style = cramped
---*)
let draftToken =
    match lastToken with
    | Some { Kind = GenericTypeParameter | StaticallyResolvedTypeParameter as kind } when isIdentifier token ->
            DraftToken.Create kind { token with LeftColumn = token.LeftColumn - 1
                                                FullMatchedLength = token.FullMatchedLength + 1 }
    | Some ( { Kind = SymbolKind.ActivePattern } as ap) when token.Tag = FSharpTokenTag.RPAREN ->
            DraftToken.Create SymbolKind.Ident ap.Token
    | _ ->
        let kind =
            if isOperator token then Operator
            elif isIdentifier token then Ident
            elif isKeyword token then Keyword
            elif isPunctuation token then Dot
            else Other
        DraftToken.Create kind token
