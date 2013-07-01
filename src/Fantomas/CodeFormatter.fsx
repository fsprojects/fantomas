#r "../../lib/FSharp.Compiler.dll"

#load "TokenMatcher.fs"
#load "FormatConfig.fs"
#load "SourceParser.fs"
#load "SourceTransformer.fs"
#load "CodePrinter.fs"
#load "CodeFormatter.fs"

open Fantomas.TokenMatcher
open Fantomas.FormatConfig
open Fantomas.SourceParser
open Fantomas.CodePrinter
open Fantomas.CodeFormatter

let config = FormatConfig.Default

let test s = formatSourceString false s config |> printfn "%A";;

filterCommentsAndDirectives """
let rec [<Literal>] private assemblyConfig() =    
    #if TRACE
    /// Comments
    ""
    #else
      ""
    #endif
"""

// FAILS - doesn't handle nested directives and line breaks are a bit off
test """
let [<Literal>] private assemblyConfig =
    #if DEBUG
    #if TRACE
    "DEBUG;TRACE"
    #else
    "DEBUG"
    #endif
    #else
    #if TRACE
    "TRACE"
    #else
    ""
    #endif
    #endif
"""

// FAILS - sticky-right comment becomes sticky-left
test """
1 +
// Comment
1""" 

test """
1 + // Comment
1""" 


// FAILS - sticky-right comment becomes sticky-left
test """
1 
// Comment
+ 1""" 

// FAILS - sticky-right comment becomes sticky-left
test """
let f() = 
    1 
    // Comment
    + 1
""" 

test """
1 + (* Comment *) 1""" 

test """
let f() = 
    // CommentB
    x + x
""" 

// INADEQUATE: inline block comment should not emit new lines
test """
/// XML comment
type (* comment *) X = 
   | A  // Hello
   | B // Goodbye
""" 

test """
/// XML comment
type (* comment *) 
     X = 
   | A  // Hello
   | B // Goodbye
""" 

// INADEQUATE: block comment not sticky-left
test """
[<NoEquality>]
type IlxClosureInfo = 
    { cloILFormalRetTy: ILType;
      /// An immutable array of free variable descriptions for the closure
      cloILFreeVars: IlxClosureFreeVar[]; 
      cloFreeVars: Val list; (* nb. the freevars we actually close over *)
      ilCloLambdas: IlxClosureLambdas;
    }
"""

// INADEQUATE: block comment not indented properly. Do we care?
test """
let StorageForVal m v eenv = 
    let v = 
        try eenv.valsInScope.[v]
        with :? KeyNotFoundException ->
          (* REVIEW: The  binary will probably still be written under these error conditions.
           *         That is useful when debugging the compiler, but not in Retail mode.
           *         Fail with an internal error if Retail? *)
          (* // Diagnostics for bug://4046
           * let vals = eenv.valsInScope.Contents |> Zmap.toList
           * vals |> List.iter (printf "v,s = %A\n")          
           *)
          assert false
          errorR(Error(FSComp.SR.ilUndefinedValue(showL(vspecAtBindL v)),m)); 
          notlazy (Arg 668(* random value for post-hoc diagnostic analysis on generated tree *) )
    v.Force()
"""