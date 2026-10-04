(*---
indent_size = 2
fsharp_multiline_bracket_style = cramped
---*)
let handlerFormattedRangeDoc (lines: NamedText, formatted: string, range: FormatSelectionRange) =
  let range =
    { inherit Foo()
      Start =
        { Line = range.StartLine - 1
          Character = range.StartColumn }
      End =
        { Line = range.EndLine - 1
          Character = range.EndColumn } }
  [| { Range = range; NewText = formatted } |]
