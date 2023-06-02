module Fantomas.Core.ISourceTextExtensions

open Fantomas.FCS.Text

type ISourceText with

    member GetContentAt: range: range -> string
