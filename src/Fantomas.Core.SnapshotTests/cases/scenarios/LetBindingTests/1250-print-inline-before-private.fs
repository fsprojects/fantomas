    let inline private isIdentifier t = t.CharClass = FSharpTokenCharKind.Identifier
    let inline private isOperator t = t.CharClass = FSharpTokenCharKind.Operator
    let inline private isKeyword t = t.ColorClass = FSharpTokenColorKind.Keyword
    let inline private isPunctuation t = t.ColorClass = FSharpTokenColorKind.Punctuation
