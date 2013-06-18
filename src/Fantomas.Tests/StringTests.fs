module Fantomas.Tests.StringTests

open NUnit.Framework
open FsUnit

open Fantomas.CodeFormatter
open Fantomas.Tests.TestHelper

[<Test>]
let ``triple-quoted strings``() =
    formatSourceString false "let xmlFragment2 = \"\"\"<book author=\"Milton, John\" title=\"Paradise Lost\">\"\"\"" config
    |> should equal "let xmlFragment2 = \"\"\"<book author=\"Milton, John\" title=\"Paradise Lost\">\"\"\""

[<Test>]
let ``string literals``() =
    formatSourceString false """
let xmlFragment1 = @"<book author=""Milton, John"" title=""Paradise Lost"">"
let str1 = "abc"
    """ config 
    |> prepend newline
    |> append newline
    |> should equal """
let xmlFragment1 = @"<book author=""Milton, John"" title=""Paradise Lost"">"
let str1 = "abc"
"""

[<Test>]
let ``multiline strings``() =
    formatSourceString false """
let alu =
        "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG\
        GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA\
        CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT\
        ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA\
        GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG\
        AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC\
  AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA"B
    """ config
    |> prepend newline
    |> should equal """
let alu = 
    "GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGG\
        GAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGA\
        CCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAAT\
        ACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCA\
        GCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGG\
        AGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCC\
  AGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA"B"""