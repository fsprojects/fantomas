let processSnippetLine
    (checkResults: FSharpCheckFileResults)
    (semanticRanges: SemanticClassificationItem array)
    (lines: string array)
    (line: int, lineTokens: SnippetLine)
    =
    let lineStr = lines.[line]
    ()
