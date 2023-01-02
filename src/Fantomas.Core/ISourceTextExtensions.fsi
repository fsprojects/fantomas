module Fantomas.Core.ISourceTextExtensions

open FSharp.Compiler.Text

type ISourceText with

    member GetContentAt: range: range -> string
