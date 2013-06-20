module Fantomas.Tests.CommentTests

open NUnit.Framework
open FsUnit

open Fantomas.CodeFormatter
open Fantomas.Tests.TestHelper

[<Test>]
let ``should keep // comments after nowarn directives``() =
    formatSourceString false """#nowarn "51" // address-of operator can occur in the code""" config
    |> should equal """#nowarn "51" // address-of operator can occur in the code"""

[<Test>]
let ``should keep // comments before module definition``() =
    formatSourceString false """
// The original idea for this typeprovider is from Ivan Towlson
// some text
module FSharpx.TypeProviders.VectorTypeProvider

let x = 1""" config
    |> should equal """// The original idea for this typeprovider is from Ivan Towlson
// some text
module FSharpx.TypeProviders.VectorTypeProvider

let x = 1"""

[<Test>]
let ``comments on local let bindings``() =
    formatSourceString false """
let print_30_permut() = 

    /// declare and initialize
    let permutation : int array = Array.init n (fun i -> Console.Write(i+1); i)
    permutation
    """ config
    |> prepend newline
    |> should equal """
let print_30_permut() = 
    /// declare and initialize
    let permutation : int array = 
        Array.init n (fun i -> 
                Console.Write(i + 1)
                i)
    permutation"""

[<Test>]
let ``xml documentation``() =
    formatSourceString false """
/// <summary>
/// Kill Weight Mud
/// </summary>
///<param name="sidpp">description</param>
///<param name="tvd">xdescription</param>
///<param name="omw">ydescription</param>
let kwm sidpp tvd omw =
    (sidpp / 0.052 / tvd) + omw

/// Kill Weight Mud
let kwm sidpp tvd omw = 1.0""" config
    |> prepend newline
    |> should equal """
/// <summary>
/// Kill Weight Mud
/// </summary>
///<param name="sidpp">description</param>
///<param name="tvd">xdescription</param>
///<param name="omw">ydescription</param>
let kwm sidpp tvd omw = (sidpp / 0.052 / tvd) + omw

/// Kill Weight Mud
let kwm sidpp tvd omw = 1.0"""

[<Test>]
let ``should preserve comment-only source code``() =
    formatSourceString false """(*
  line1
  line2
*)
"""  config
    |> should equal """(*
  line1
  line2
*)"""

[<Test>]
let ``should keep sticky-to-the-right comments``() =
    formatSourceString false """
let f() = 
    // COMMENT
    let x = 1
    x + x
"""  config
    |> prepend newline
    |> should equal """
let f() = 
    // COMMENT
    let x = 1
    x + x"""

[<Test>]
let ``should keep sticky-to-the-left comments``() =
    formatSourceString false """
let f() = 
  let x = 1 // COMMENT
  x + x
"""   config
    |> prepend newline
    |> should equal """
let f() = 
    let x = 1 // COMMENT
    x + x""" 

[<Test>]
let ``should keep well-aligned comments``() =
    formatSourceString false """
/// XML COMMENT
// Other comment
let f() = 
    // COMMENT A
    let y = 1
    // COMMENT B
    x + x + x

"""  config
    |> prepend newline
    |> should equal """
/// XML COMMENT
// Other comment
let f () = 
    // COMMENT A
    let y = 1
    // COMMENT B
    x + x + x"""

[<Test>]
let ``should align mis-aligned comments``() =
    formatSourceString false  """
   /// XML COMMENT A
     // Other comment
let f() = 
      // COMMENT A
    let y = 1
      /// XML COMMENT B
    let z = 1
  // COMMENT B
    x + x + x

"""   config
    |> prepend newline
    |> should equal """
/// XML COMMENT A
// Other comment
let f () = 
    // COMMENT A
    let y = 1
    /// XML COMMENT B
    let z = 1
    // COMMENT B
    x + x + x"""