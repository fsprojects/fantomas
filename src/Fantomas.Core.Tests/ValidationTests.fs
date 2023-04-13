module Fantomas.Core.Tests.ValidationTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelpers

[<Test>]
let ``naked ranges are valid outside for..in.do`` () =
    isValidFSharpCode
        false
        """
let factors number = 2L..number / 2L
                     |> Seq.filter (fun x -> number % x = 0L)"""
    |> should equal true

[<Test>]
let ``misplaced comments should give parser errors`` () =
    isValidFSharpCode
        false
        """
module ServiceSupportMethods =
    let toDisposable (xs : seq<'t // Sleep to give time for printf to succeed
                                  when 't :> IDisposable>) =
        { new IDisposable with
              member x.Dispose() = xs |> Seq.iter (fun x -> x.Dispose()) }"""
    |> should equal false

[<Test>]
let ``should fail on uncompilable extern functions`` () =
    isValidFSharpCode
        false
        """
[<System.Runtime.InteropServices.DllImport("user32.dll")>]
let GetWindowLong hwnd : System.IntPtr, index : int : int = failwith )"""
    |> should equal false
