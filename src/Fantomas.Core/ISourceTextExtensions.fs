module Fantomas.Core.ISourceTextExtensions

open System.Text
open Fantomas.FCS.Text

type ISourceText with

    member x.GetContentAt(range: range) : string =
        let startLine = range.StartLine - 1
        let line = x.GetLineString startLine

        if range.StartLine = range.EndLine then
            let length = range.EndColumn - range.StartColumn
            line.Substring(range.StartColumn, length)
        else
            let firstLineContent = line.Substring(range.StartColumn)
            let sb = StringBuilder().AppendLine(firstLineContent)

            (sb, [ range.StartLine .. range.EndLine - 2 ])
            ||> List.fold (fun sb lineNumber -> sb.AppendLine(x.GetLineString lineNumber))
            |> fun sb ->
                let lastLine = x.GetLineString(range.EndLine - 1)

                sb.Append(lastLine.Substring(0, range.EndColumn)).ToString()
