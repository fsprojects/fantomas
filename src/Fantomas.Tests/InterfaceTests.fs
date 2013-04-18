module Fantomas.Tests.InterfaceTests

open NUnit.Framework
open FsUnit

open Fantomas.CodeFormatter
open Fantomas.Tests.TestHelper


// the current behavior results in a compile error since the interface IDocument is only a marker interface and has no methods
[<Test>]
let ``should not add with to inface definitions``() =
    formatSourceString false """type Text(text : string) = 
    interface IDocument
        
    interface Infrastucture with
        member this.Serialize sb = sb.AppendFormat("\"{0}\"", escape v)
        member this.ToXml() = v :> obj
    """ config
    |> should equal """type Text(text : string) = 
    interface IDocument
    interface Infrastucture with
        member this.Serialize sb = sb.AppendFormat("\"{0}\"", escape v)
        member this.ToXml() = v :> obj
"""
