module Fantomas.Core.SourceOrigin

open FSharp.Compiler.Text

type SourceOrigin =
    | SourceString of string
    | SourceText of ISourceText
