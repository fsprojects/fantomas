module Fantomas.Core.Tests.MultipleDefineCombinationsTests

open NUnit.Framework
open Fantomas.Core
open Fantomas.Core.Tests.TestHelpers

let private mergeAndCompare (aDefines, aCode) (bDefines, bCode) expected =
    let result =
        MultipleDefineCombinations.mergeMultipleFormatResults
            { config with
                EndOfLine = EndOfLineStyle.LF }
            [ DefineCombination(aDefines),
              { Code = String.normalizeNewLine aCode
                Cursor = None }
              DefineCombination(bDefines),
              { Code = String.normalizeNewLine bCode
                Cursor = None } ]

    let normalizedExpected = String.normalizeNewLine expected
    normalizedExpected == result.Code

[<Test>]
let ``merging of source code that starts with a hash`` () =
    let a =
        """#if NOT_DEFINED
    printfn \"meh\"
#else

#endif
"""

    let b =
        """#if NOT_DEFINED

#else
    printfn \"foo\"
#endif
"""

    """#if NOT_DEFINED
    printfn \"meh\"
#else
    printfn \"foo\"
#endif
"""
    |> mergeAndCompare ([], a) ([ "NOT_DEFINED" ], b)

[<Test>]
let ``merging of defines content work when source code starts with a newline`` () =
    let a =
        """
[<Literal>]
let private assemblyConfig() =
    #if TRACE

    #else
    let x = "x"
    #endif
    x
"""

    let b =
        """
[<Literal>]
let private assemblyConfig() =
    #if TRACE
    let x = ""
    #else

    #endif
    x
"""

    """
[<Literal>]
let private assemblyConfig() =
#if TRACE
    let x = ""
#else
    let x = "x"
#endif
    x
"""
    |> mergeAndCompare ([], a) ([ "TRACE" ], b)

[<Test>]
let ``only split on control structure keyword`` () =
    let a =
        """
#if INTERACTIVE
#else
#load "../FSharpx.TypeProviders/SetupTesting.fsx"

SetupTesting.generateSetupScript __SOURCE_DIRECTORY__

#load "__setup__.fsx"
#endif
"""

    let b =
        """
#if INTERACTIVE
#else



#endif
    """

    """
#if INTERACTIVE
#else
#load "../FSharpx.TypeProviders/SetupTesting.fsx"

SetupTesting.generateSetupScript __SOURCE_DIRECTORY__

#load "__setup__.fsx"
#endif
"""
    |> mergeAndCompare ([], a) ([ "INTERACTIVE" ], b)

// This test illustrates the goal of MultipleDefineCombinations
// All three results will be merged in one go.
[<Test>]
let ``triple merge`` () =
    let result =
        MultipleDefineCombinations.mergeMultipleFormatResults
            { config with
                EndOfLine = EndOfLineStyle.LF }
            [ DefineCombination([]),
              { Code =
                  String.normalizeNewLine
                      """
let v =
  #if A

  #else
    #if B

    #else
      'C'
    #endif 
  #endif
"""
                Cursor = None }
              DefineCombination([ "A" ]),
              { Code =
                  String.normalizeNewLine
                      """
let v =
  #if A
    'A'
  #else
    #if B

    #else

    #endif 
  #endif
"""
                Cursor = None }
              DefineCombination([ "B" ]),
              { Code =
                  String.normalizeNewLine
                      """
let v =
  #if A

  #else
    #if B
      'B'
    #else

    #endif 
  #endif
"""
                Cursor = None } ]

    let expected =
        String.normalizeNewLine
            """
let v =
#if A
    'A'
#else
#if B
      'B'
#else
      'C'
#endif 
#endif
"""

    expected == result.Code
