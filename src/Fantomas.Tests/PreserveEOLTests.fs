module Fantomas.Tests.PreserveBlankLinesTests

open NUnit.Framework
open FsUnit

open Fantomas.CodeFormatter
open Fantomas.Tests.TestHelper

let config =  { config with PreserveEndOfLine = true; PageWidth = 20 }

[<Test>]
let ``preserve blank lines`` () =
    formatSourceString false """
let x=    1
    
let y=2""" config
    |> should equal """
let x = 1

let y = 2
"""

[<Test>]
let ``preserve multiple blank lines`` () =
    formatSourceString false """
type Num=int
    


printfn   0""" config
    |> should equal """
type Num = int



printfn 0
"""

[<Test>]
let ``disable auto blank line if preserve enabled`` () =
    formatSourceString false """
let x=
    let z =1
    z
let y =2""" config
    |> should equal """
let x =
    let z = 1
    z
let y = 2
"""

[<Test>]
let ``preserve blank lines in nested case`` () =
    formatSourceString false """
let x=
    let z =1
  
    z

let y =2""" config
    |> should equal """
let x =
    let z = 1

    z

let y = 2
"""

[<Test>]
let ``disabled removing of line breaks`` () =
    formatSourceString false """
let x=  
    1
          
let y=2""" config
    |> should equal """
let x =
    1

let y = 2
"""

[<Test>]
let ``comments on local let bindings``() =
    formatSourceString false """
let f() =  
 
    /// c1  
    /// c2   
    let x = "/// c3  "  
    1""" config
    |> should equal """
let f() =

    /// c1
    /// c2
    let x = "/// c3  "
    1
"""


[<Test>]
let ``should keep comments before attributes``() =
    formatSourceString false  """
[<NoEquality; NoComparison>]
type IlxGenOptions =  
    { fragName: string
      generateFilterBlocks: bool
      workAroundReflectionEmitBugs: bool
      emitConstantArraysUsingStaticDataBlobs: bool
      // If this is set, then the last module becomes the "main" module and its toplevel bindings are executed at startup 
      mainMethodInfo: Tast.Attribs option
      localOptimizationsAreOn: bool
      generateDebugSymbols: bool
      testFlagEmitFeeFeeAs100001: bool
      ilxBackend: IlxGenBackend
      /// Indicates the code is being generated in FSI.EXE and is executed immediately after code generation
      /// This includes all interactively compiled code, including #load, definitions, and expressions
      isInteractive: bool 
      // Indicates the code generated is an interactive 'it' expression. We generate a setter to allow clearing of the underlying
      // storage, even though 'it' is not logically mutable
      isInteractiveItExpr: bool
      // Indicates System.SerializableAttribute is available in the target framework
      netFxHasSerializableAttribute : bool
      /// Whenever possible, use callvirt instead of call
      alwaysCallVirt: bool}
"""   config 
    |> should equal """
[<NoEquality; NoComparison>]
type IlxGenOptions =
    { fragName : string
      generateFilterBlocks : bool
      workAroundReflectionEmitBugs : bool
      emitConstantArraysUsingStaticDataBlobs : bool
      // If this is set, then the last module becomes the "main" module and its toplevel bindings are executed at startup
      mainMethodInfo : Tast.Attribs option
      localOptimizationsAreOn : bool
      generateDebugSymbols : bool
      testFlagEmitFeeFeeAs100001 : bool
      ilxBackend : IlxGenBackend
      /// Indicates the code is being generated in FSI.EXE and is executed immediately after code generation
      /// This includes all interactively compiled code, including #load, definitions, and expressions
      isInteractive : bool
      // Indicates the code generated is an interactive 'it' expression. We generate a setter to allow clearing of the underlying
      // storage, even though 'it' is not logically mutable
      isInteractiveItExpr : bool
      // Indicates System.SerializableAttribute is available in the target framework
      netFxHasSerializableAttribute : bool
      /// Whenever possible, use callvirt instead of call
      alwaysCallVirt : bool }
"""

[<Test>]
let ``should keep well-aligned comments``() =
    formatSourceString false """
/// XML COMMENT
// Other comment
let f() = 
    // COMMENT A
    let y = 1
    (* COMMENT B *)
    (* COMMENT C *)
    x + x + x
"""  config
    |> should equal """
/// XML COMMENT
// Other comment
let f() =
    // COMMENT A
    let y = 1
    (* COMMENT B *)
    (* COMMENT C *)
    x + x + x
"""

[<Test>]
let ``active patterns``() =
    formatSourceString false """
let (|Even|Odd| ) input = if input % 2=0 then Even else Odd

let (|Integer|_|) (str:string) =
    let mutable intvalue = 0
    if System.Int32.TryParse(str, &intvalue) then Some(intvalue)
    else None

let (|ParseRegex|_|) regex str =
    let m = Regex(regex).Match(str)
    if m.Success
    then Some (List.tail [ for x in m.Groups -> x.Value ])
    else None""" config
    |> should equal """
let (|Even|Odd|) input = if input % 2 = 0 then Even else Odd

let (|Integer|_|) (str : string) =
    let mutable intvalue = 0
    if System.Int32.TryParse (str, &intvalue) then Some (intvalue)
    else None

let (|ParseRegex|_|) regex str =
    let m = Regex(regex) .Match(str)
    if m.Success
    then Some (List.tail [ for x in m.Groups -> x.Value ])
    else None
"""

[<Test>]
let ``should keep the attribute on top of the function``() =
    formatSourceString false """[<ExtensionA>]
type Funcs = 
    [<ExtensionB>]
    static member ToFunc (f: Action<_,_,_>) =
        Func<_,_,_,_>(fun a b c -> f.Invoke(a,b,c))
    """ config
    |> should equal """[<ExtensionA>]
type Funcs =
    [<ExtensionB>]
    static member ToFunc(f : Action<_, _, _>) =
        Func<_, _, _, _> (fun a b c -> f.Invoke (a, b, c))
"""

[<Test>]
let ``attributes on expressions``() =
    formatSourceString false """
    [<XYZ>]  
    let   x="1"
    let [<Literal>]y="2" 
    [<Dependency("FSharp.Compiler", LoadHint.Always)>]
    do ()""" config
    |> should equal """
    [<XYZ>]
    let x = "1"
    let [<Literal>] y = "2"
    [<Dependency("FSharp.Compiler", LoadHint.Always)>]
    do ()
"""

[<Test>]
let ``array values``() =
    formatSourceString false """
let x = [ 1
          2]
let y = [| 3;4
           5;6|]
let z = [7; 8]
    """ config
    |> should equal """
let x = [ 1;
          2 ]
let y = [| 3; 4;
           5; 6 |]
let z = [ 7; 8 ]
"""

[<Test>]
let ``array values auto break``() =
    formatSourceString false """
let arr = [|(1, 1,1); (1,2, 2); (1, 3, 3); (2, 1, 2); (2,2,4); (2, 3, 6); (3, 1, 3);
             (3, 2, 6); (3, 3, 9)|]
    """ config
    |> should equal """
let arr = [| (1, 1, 1); (1, 2, 2); (1, 3, 3); (2, 1, 2); (2, 2, 4); (2, 3, 6); (3, 1, 3);
             (3, 2, 6); (3, 3, 9) |]
"""
[<Test>]
let ``handle # identifier``() =
    formatSourceString false """namespace global

#if DEBUG
#endif""" config
    |> should equal """namespace global

#if DEBUG
#endif
"""

[<Test>]
let ``don't remove semicolon in list``() =
    formatSourceString false """
let x = [yield! es; yield (s, e')]
    """ { config with SemicolonAtEndOfLine = false }
    |> should equal """
let x = [ yield! es; yield (s, e') ]
"""

[<Test>]
let ``don't remove semicolon in list with auto semicolon ON``() =
    formatSourceString false """
let x = [yield! es; yield (s, e')]
    """ { config with SemicolonAtEndOfLine = true }
    |> should equal """
let x = [ yield! es; yield (s, e') ]
"""

[<Test>]
let ``indentation issue 1``() =
    formatSourceString false """
let f1 =
    let f2 =
        if pred then 1
        else 2

    let f3 =
        3
    4
    """ config
    |> should equal """
let f1 =
    let f2 =
        if pred then 1
        else 2

    let f3 =
        3
    4
"""

[<Test; Description("core issue, no space before closing }")>]
let ``semicolon issue 1``() =
    formatSourceString false """
seq {yield! 1;yield! 2}
    """ config
    |> should equal """
seq { yield! 1; yield! 2}
"""

[<Test>]
let ``indentation issue 5 (# handling)``() =
    formatSourceString false """
    #if F
    #else
    let f1 = 
        let x = ref 0
        0
    #endif
    """ config
    |> should equal """
    #if F
    #else
    let f1 =
        let x = ref 0
        0
    #endif
"""

[<Test>]
let ``indentation issue 6``() =
    formatSourceString false """
(*

A

    C

D
*)

let x=1"""  config
    |> should equal """
(*

A

    C

D
*)

let x = 1
"""

[<Test>]
let ``records formatting without pEOL``() =
    formatSourceString false """
let rainbow = 
    { b1 = "1"
      b2 = "2" 
      b3 = "3" 
    }
    """ { config with PreserveEndOfLine = false }
    |> should equal """let rainbow =
    { b1 = "1"
      b2 = "2"
      b3 = "3" }
"""

[<Test>]
let ``records formatting with pEOL``() =
    formatSourceString false """
let rainbow = 
    { b1 = "1"
      b2 = "2" 
      b3 = "3" 
    }
    """ config
    |> should equal """
let rainbow =
    { b1 = "1"
      b2 = "2"
      b3 = "3"
    }
"""


[<Test>]
let ``keep single pipe with pEOL``() =
    formatSourceString false """
    try
        ()
    with
    | :? _ ->
        ()
    let y = 4
    """ config
    |> should equal """
    try
        ()
    with
    | :? _ ->
        ()
    let y = 4
"""

[<Test>]
let ``remove single pipe without pEOL``() =
    formatSourceString false """
    try
        ()
    with 
    | :? _ -> 
        ()
    let y = 4
"""     { config with PreserveEndOfLine = false }
    |> prepend newline
    |> should equal """
try 
    ()
with :? _ -> ()

let y = 4
"""

[<Test>]
let ``keep multiple pipes with pEOL``() =
    formatSourceString false """
    match ts with
    | 0
    | 1 -> 2
    | _ -> 3
    """ config
    |> should equal """
    match ts with
    | 0
    | 1 -> 2
    | _ -> 3
"""

[<Test>]
let ``newline handling in directives``() =
    formatSourceString false """
#if NOT_DEFINED
let x = 1
let y = 2
#endif
"""  config
    |> should equal """
#if NOT_DEFINED

let x = 1
let y = 2
#endif
"""
