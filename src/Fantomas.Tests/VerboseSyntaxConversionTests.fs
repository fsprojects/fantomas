module Fantomas.Tests.VerboseSyntaxConversionTests

open NUnit.Framework
open FsUnit

open Fantomas.CodeFormatter
open Fantomas.Tests.TestHelper

[<Test>]
let ``verbose syntax``() =
    formatSourceString false """
    #light "off"

    let div2 = 2;;

    let f x = 
        let r = x % div2 in
          if r = 1 then 
            begin "Odd"  end 
          else 
            begin "Even" end
    """ config
    |> prepend newline
    |> should equal """
let div2 = 2

let f x = 
    let r = x % div2
    if r = 1
    then ("Odd")
    else ("Even")"""
