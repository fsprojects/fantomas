module Fantomas.SourceTextExtensions

open System.Text
open FSharp.Compiler.Text

// TODO: just move to TokenParser.fs

type ISourceText with
    member this.GetContentAt(range: range) : string =
        let startLine = range.StartLine - 1
        let line = this.GetLineString startLine

        if range.StartLine = range.EndLine then
            let length = range.EndColumn - range.StartColumn
            line.Substring(range.StartColumn, length)
        else
            let firstLineContent = line.Substring(range.StartColumn)
            let sb = StringBuilder().AppendLine(firstLineContent)

            (sb, [ range.StartLine .. range.EndLine - 2 ])
            ||> List.fold (fun sb lineNumber -> sb.AppendLine(this.GetLineString lineNumber))
            |> fun sb ->
                let lastLine = this.GetLineString(range.EndLine - 1)

                sb
                    .Append(lastLine.Substring(0, range.EndColumn))
                    .ToString()
