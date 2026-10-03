let handlerFormattedRangeDoc (lines: NamedText, formatted: string, range: FormatSelectionRange) =
  let range = {
    Start = {
      Line = range.StartLine - 1
      Character = range.StartColumn
    }
    End = {
      Line = range.EndLine - 1
      Character = range.EndColumn
    }
  }

  [| { Range = range; NewText = formatted } |]
