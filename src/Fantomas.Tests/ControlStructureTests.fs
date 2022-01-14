module Fantomas.Tests.ControlStructureTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper

[<Test>]
let ``if/then/else block`` () =
    formatSourceString
        false
        """
let rec tryFindMatch pred list =
    match list with
    | head :: tail -> if pred(head)
                        then Some(head)
                        else tryFindMatch pred tail
    | [] -> None

let test x y =
  if x = y then "equals"
  elif x < y then "is less than"
  else if x > y then "is greater than"
  else "Don't know"

if age < 10
then printfn "You are only %d years old and already learning F#? Wow!" age"""
        { config with MaxIfThenElseShortWidth = 60 }
    |> prepend newline
    |> should
        equal
        """
let rec tryFindMatch pred list =
    match list with
    | head :: tail -> if pred (head) then Some(head) else tryFindMatch pred tail
    | [] -> None

let test x y =
    if x = y then "equals"
    elif x < y then "is less than"
    else if x > y then "is greater than"
    else "Don't know"

if age < 10 then
    printfn "You are only %d years old and already learning F#? Wow!" age
"""

[<Test>]
let ``for loops`` () =
    formatSourceString
        false
        """
    let function1() =
        for i = 1 to 10 do
            printf "%d " i
        printfn ""
    let function2() =
      for i = 10 downto 1 do
        printf "%d " i
      printfn ""
    """
        config
    |> prepend newline
    |> should
        equal
        """
let function1 () =
    for i = 1 to 10 do
        printf "%d " i

    printfn ""

let function2 () =
    for i = 10 downto 1 do
        printf "%d " i

    printfn ""
"""

[<Test>]
let ``while loop`` () =
    formatSourceString
        false
        """
open System
let lookForValue value maxValue =
  let mutable continueLooping = true
  let randomNumberGenerator = new Random()
  while continueLooping do
    let rand = randomNumberGenerator.Next(maxValue)
    printf "%d " rand
    if rand = value then
       printfn "\nFound a %d!" value
       continueLooping <- false
lookForValue 10 20"""
        config
    |> prepend newline
    |> should
        equal
        """
open System

let lookForValue value maxValue =
    let mutable continueLooping = true
    let randomNumberGenerator = new Random()

    while continueLooping do
        let rand = randomNumberGenerator.Next(maxValue)
        printf "%d " rand

        if rand = value then
            printfn "\nFound a %d!" value
            continueLooping <- false

lookForValue 10 20
"""

[<Test>]
let ``try/with block`` () =
    formatSourceString
        false
        """
let divide1 x y =
   try
      Some (x / y)
   with
      | :? System.DivideByZeroException -> printfn "Division by zero!"; None

let result1 = divide1 100 0
    """
        config
    |> prepend newline
    |> should
        equal
        """
let divide1 x y =
    try
        Some(x / y)
    with
    | :? System.DivideByZeroException ->
        printfn "Division by zero!"
        None

let result1 = divide1 100 0
"""

[<Test>]
let ``try/with and finally`` () =
    formatSourceString
        false
        """
    let function1 x y =
       try
         try
            if x = y then raise (InnerError("inner"))
            else raise (OuterError("outer"))
         with
          | Failure _ -> ()
          | InnerError(str) -> printfn "Error1 %s" str
       finally
          printfn "Always print this."
    """
        config
    |> prepend newline
    |> should
        equal
        """
let function1 x y =
    try
        try
            if x = y then
                raise (InnerError("inner"))
            else
                raise (OuterError("outer"))
        with
        | Failure _ -> ()
        | InnerError (str) -> printfn "Error1 %s" str
    finally
        printfn "Always print this."
"""

[<Test>]
let ``range expressions`` () =
    formatSourceString
        false
        """
    let function2() =
      for i in 1 .. 2 .. 10 do
         printf "%d " i

      printfn ""
    function2()"""
        config
    |> prepend newline
    |> should
        equal
        """
let function2 () =
    for i in 1..2..10 do
        printf "%d " i

    printfn ""

function2 ()
"""

[<Test>]
let ``use binding`` () =
    formatSourceString
        false
        """
    let writetofile filename obj =
     use file1 = File.CreateText(filename)
     file1.WriteLine("{0}", obj.ToString())
    """
        config
    |> prepend newline
    |> should
        equal
        """
let writetofile filename obj =
    use file1 = File.CreateText(filename)
    file1.WriteLine("{0}", obj.ToString())
"""

[<Test>]
let ``access modifiers`` () =
    formatSourceString
        false
        """
    let private myPrivateObj = new MyPrivateType()
    let internal myInternalObj = new MyInternalType()"""
        config
    |> prepend newline
    |> should
        equal
        """
let private myPrivateObj = new MyPrivateType()
let internal myInternalObj = new MyInternalType()
"""

[<Test>]
let ``keyworded expressions`` () =
    formatSourceString
        false
        """
    assert (3 > 2)
    let result = lazy (x + 10)
    do printfn "Hello world"
    """
        config
    |> prepend newline
    |> should
        equal
        """
assert (3 > 2)
let result = lazy (x + 10)
do printfn "Hello world"
"""

[<Test>]
let ``should break lines on multiline if conditions`` () =
    formatSourceString
        false
        """
let x =
    if try
        true
       with
       | Failure _ -> false
    then ()
    else ()
    """
        config
    |> prepend newline
    |> should
        equal
        """
let x =
    if
        try
            true
        with
        | Failure _ -> false
    then
        ()
    else
        ()
"""

[<Test>]
let ``try finally in if expression`` () =
    formatSourceString
        false
        """
let y =
    if  try true
        finally false
    then
        ()
    else
        ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
let y =
    if
        try
            true
        finally
            false
    then
        ()
    else
        ()
"""

[<Test>]
let ``should not escape some specific keywords`` () =
    formatSourceString
        false
        """
base.Initializer()
global.Test()
    """
        config
    |> prepend newline
    |> should
        equal
        """
base.Initializer()
global.Test()
"""

[<Test>]
let ``should handle delimiters before comments`` () =
    formatSourceString
        false
        """
let handle =
    if n<weakThreshhold then
        assert onStrongDiscard.IsNone; // it disappeared
        Weak(WeakReference(v))
    else
        Strong(v)
    """
        config
    |> prepend newline
    |> should
        equal
        """
let handle =
    if n < weakThreshhold then
        assert onStrongDiscard.IsNone // it disappeared
        Weak(WeakReference(v))
    else
        Strong(v)
"""

[<Test>]
let ``should handle infix operators in pattern matching`` () =
    formatSourceString
        false
        """
let url =
  match x with
  | A -> "a"
  | B -> "b"
  + "/c"
    """
        config
    |> prepend newline
    |> should
        equal
        """
let url =
    match x with
    | A -> "a"
    | B -> "b"
    + "/c"
"""

[<Test>]
let ``if/elif without else`` () =
    formatSourceString
        false
        """
if true then ()
elif true then ()
    """
        config
    |> prepend newline
    |> should
        equal
        """
if true then ()
elif true then ()
"""

[<Test>]
let ``multiline if in tuple`` () =
    formatSourceString
        false
        """
(if true then 1 else 2
 ,3)
    """
        config
    |> prepend newline
    |> should
        equal
        """
((if true then 1 else 2), 3)
"""

// https://docs.microsoft.com/en-us/dotnet/fsharp/style-guide/formatting#formatting-if-expressions
[<Test>]
let ``else branch should be on newline in case if branch is long`` () =
    formatSourceString
        false
        """
if cond then
    match foo with
    | Some f -> ()
    | None -> printfn "%s" "meh"
else ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
if cond then
    match foo with
    | Some f -> ()
    | None -> printfn "%s" "meh"
else
    ()
"""

[<Test>]
let ``if branch should be on newline in case else branch is long`` () =
    formatSourceString
        false
        """
if not cond then
    ()
else
    match foo with
    | Some f -> ()
    | None -> printfn "%s" "meh"
"""
        config
    |> prepend newline
    |> should
        equal
        """
if not cond then
    ()
else
    match foo with
    | Some f -> ()
    | None -> printfn "%s" "meh"
"""

[<Test>]
let ``elif branch should on newline if else branch is long`` () =
    formatSourceString
        false
        """
if not cond then
    ()
elif false then ()
else
    match foo with
    | Some f -> ()
    | None -> printfn "%s" "meh"
"""
        config
    |> prepend newline
    |> should
        equal
        """
if not cond then
    ()
elif false then
    ()
else
    match foo with
    | Some f -> ()
    | None -> printfn "%s" "meh"
"""

[<Test>]
let ``multiline elif branch should result in newline for if and else`` () =
    formatSourceString
        false
        """
if foo then ()
elif bar then
    match foo with
    | Some f -> ()
    | None -> printfn "%s" "meh"
else ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
if foo then
    ()
elif bar then
    match foo with
    | Some f -> ()
    | None -> printfn "%s" "meh"
else
    ()
"""

[<Test>]
let ``else keyword should be on separate line, #483`` () =
    formatSourceString
        false
        """  if i.OpCode = OpCodes.Switch then
    AccumulateSwitchTargets i targets
    c
  else
    let branch = i.Operand :?> Cil.Instruction
    c + (Option.nullable branch.Previous)
"""
        config
    |> prepend newline
    |> should
        equal
        """
if i.OpCode = OpCodes.Switch then
    AccumulateSwitchTargets i targets
    c
else
    let branch = i.Operand :?> Cil.Instruction
    c + (Option.nullable branch.Previous)
"""

[<Test>]
let ``relaxation in for loops`` () =
    formatSourceString
        false
        """
for _ in 1..10 do ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
for _ in 1..10 do
    ()
"""

[<Test>]
let ``if elif if with trivia doesn't glitch elif conditional`` () =
    formatSourceString
        false
        """
let a ex =
    if null = ex then
        fooo ()
        None
        // this was None
    elif ex.GetType() = typeof<obj> then
        Some ex
    else
        None
"""
        config
    |> prepend newline
    |> should
        equal
        """
let a ex =
    if null = ex then
        fooo ()
        None
    // this was None
    elif ex.GetType() = typeof<obj> then
        Some ex
    else
        None
"""

[<Test>]
let ``print trivia for SynExpr.Assert, 1071`` () =
    formatSourceString
        false
        """
let genPropertyWithGetSet astContext (b1, b2) rangeOfMember =
    match b1, b2 with
    | PropertyBinding (ats, px, ao, isInline, mf1, PatLongIdent (ao1, s1, ps1, _), e1),
      PropertyBinding (_, _, _, _, _, PatLongIdent (ao2, _, ps2, _), e2) ->
        let prefix =
            genPreXmlDoc px
            +> genAttributes astContext ats
            +> genMemberFlags astContext mf1
            +> ifElse isInline (!- "inline ") sepNone
            +> opt sepSpace ao genAccess

        assert (ps1 |> Seq.map fst |> Seq.forall Option.isNone)
        assert (ps2 |> Seq.map fst |> Seq.forall Option.isNone)
        let ps1 = List.map snd ps1
        let ps2 = List.map snd ps2

        prefix
        +> !-s1
        +> indent
        +> sepNln
        +> optSingle (fun rom -> enterNodeTokenByName rom WITH) rangeOfMember
        +> genProperty astContext "with " ao1 "get " ps1 e1
        +> sepNln
        +> genProperty astContext "and " ao2 "set " ps2 e2
        +> unindent
    | _ -> sepNone
"""
        config
    |> prepend newline
    |> should
        equal
        """
let genPropertyWithGetSet astContext (b1, b2) rangeOfMember =
    match b1, b2 with
    | PropertyBinding (ats, px, ao, isInline, mf1, PatLongIdent (ao1, s1, ps1, _), e1),
      PropertyBinding (_, _, _, _, _, PatLongIdent (ao2, _, ps2, _), e2) ->
        let prefix =
            genPreXmlDoc px
            +> genAttributes astContext ats
            +> genMemberFlags astContext mf1
            +> ifElse isInline (!- "inline ") sepNone
            +> opt sepSpace ao genAccess

        assert (ps1 |> Seq.map fst |> Seq.forall Option.isNone)
        assert (ps2 |> Seq.map fst |> Seq.forall Option.isNone)
        let ps1 = List.map snd ps1
        let ps2 = List.map snd ps2

        prefix
        +> !-s1
        +> indent
        +> sepNln
        +> optSingle (fun rom -> enterNodeTokenByName rom WITH) rangeOfMember
        +> genProperty astContext "with " ao1 "get " ps1 e1
        +> sepNln
        +> genProperty astContext "and " ao2 "set " ps2 e2
        +> unindent
    | _ -> sepNone
"""

[<Test>]
let ``preserve new line before while loop, 1072`` () =
    formatSourceString
        false
        """
let internal coli f' (c: seq<'T>) f (ctx: Context) =
    let mutable tryPick = true
    let mutable st = ctx
    let mutable i = 0
    let e = c.GetEnumerator()

    while (e.MoveNext()) do
        if tryPick then tryPick <- false else st <- f' st
        st <- f i (e.Current) st
        i <- i + 1

    st
"""
        { config with MaxIfThenElseShortWidth = 50 }
    |> prepend newline
    |> should
        equal
        """
let internal coli f' (c: seq<'T>) f (ctx: Context) =
    let mutable tryPick = true
    let mutable st = ctx
    let mutable i = 0
    let e = c.GetEnumerator()

    while (e.MoveNext()) do
        if tryPick then tryPick <- false else st <- f' st
        st <- f i (e.Current) st
        i <- i + 1

    st
"""

[<Test>]
let ``keep new line before for loop, 1317`` () =
    formatSourceString
        false
        """
  /// Fold over the array passing the index and element at that index to a folding function
  let foldi (folder: 'State -> int -> 'T -> 'State) (state: 'State) (array: 'T []) =
    checkNonNull "array" array

    if array.Length = 0 then
      state
    else
      let folder =
        OptimizedClosures.FSharpFunc<_, _, _, _>.Adapt folder

      let mutable state: 'State = state
      let len = array.Length

      for i = 0 to len - 1 do
        state <- folder.Invoke(state, i, array.[i])

      state
"""
        { config with MaxDotGetExpressionWidth = 60 }
    |> prepend newline
    |> should
        equal
        """
/// Fold over the array passing the index and element at that index to a folding function
let foldi (folder: 'State -> int -> 'T -> 'State) (state: 'State) (array: 'T []) =
    checkNonNull "array" array

    if array.Length = 0 then
        state
    else
        let folder = OptimizedClosures.FSharpFunc<_, _, _, _>.Adapt folder

        let mutable state: 'State = state
        let len = array.Length

        for i = 0 to len - 1 do
            state <- folder.Invoke(state, i, array.[i])

        state
"""

[<Test>]
let ``line comment inside short `with` block (of a try-with), 1219`` () =
    formatSourceString
        false
        """
      try
          //comment1
          TrySomething(someParam)
      with
          //comment2
          ex ->
          MakeSureToCleanup(someParam)
"""
        { config with IndentOnTryWith = true }
    |> prepend newline
    |> should
        equal
        """
try
    //comment1
    TrySomething(someParam)
with
    //comment2
    | ex -> MakeSureToCleanup(someParam)
"""

[<Test>]
let ``line comment inside `with` block (of a try-with), 1219`` () =
    formatSourceString
        false
        """module Foo =
        let Bar () =
            async {
                try
                    let! content = tryDownloadFile url
                    return Some content
                with
                    // should we specify HttpRequestException?
                    ex ->
                        Infrastructure.ReportWarning ex
                        return None
            }
"""
        { config with IndentOnTryWith = true }
    |> prepend newline
    |> should
        equal
        """
module Foo =
    let Bar () =
        async {
            try
                let! content = tryDownloadFile url
                return Some content
            with
                // should we specify HttpRequestException?
                | ex ->
                    Infrastructure.ReportWarning ex
                    return None
        }
"""

[<Test>]
let ``line comment inside nested `with` block (of a try-with), 1219`` () =
    formatSourceString
        false
        """
      try
          //comment1
          try
              //comment2
              TrySomething(someParam)
          with
              //comment3
              ex ->
              MakeSureToCleanup(someParam)

      with
          ex ->
          Infrastructure.ReportWarning ex
          return None
"""
        { config with IndentOnTryWith = true }
    |> prepend newline
    |> should
        equal
        """
try
    //comment1
    try
        //comment2
        TrySomething(someParam)
    with
        //comment3
        | ex -> MakeSureToCleanup(someParam)

with
    | ex ->
        Infrastructure.ReportWarning ex
        return None
"""

[<Test>]
let ``try/with with multiple type checks, 1395`` () =
    formatSourceString
        false
        """
things
|> Seq.map (fun a ->
    try
        Some i
    with
    | :? Foo
    | :? Bar as e when true ->
        None
)
"""
        config
    |> prepend newline
    |> should
        equal
        """
things
|> Seq.map (fun a ->
    try
        Some i
    with
    | :? Foo
    | :? Bar as e when true -> None)
"""

[<Test>]
let ``try/with with named or pattern`` () =
    formatSourceString
        false
        """
things
|> Seq.map (fun a ->
    try
        Some i
    with
    | Foo _
    | Bar _ as e when true ->
        None
)
"""
        config
    |> prepend newline
    |> should
        equal
        """
things
|> Seq.map (fun a ->
    try
        Some i
    with
    | Foo _
    | Bar _ as e when true -> None)
"""

[<Test>]
let ``comment above pipe of try/with`` () =
    formatSourceString
        false
        """
try
    let defaultTime = (DateTime.FromFileTimeUtc 0L).ToLocalTime ()
    foo.CreationTime <> defaultTime
with
// hmm
| :? FileNotFoundException -> false
"""
        config
    |> prepend newline
    |> should
        equal
        """
try
    let defaultTime = (DateTime.FromFileTimeUtc 0L).ToLocalTime()
    foo.CreationTime <> defaultTime
with
// hmm
| :? FileNotFoundException -> false
"""

[<Test>]
let ``comment above pipe of try/with, idempotent`` () =
    formatSourceString
        false
        """
try
    let defaultTime =
        (DateTime.FromFileTimeUtc 0L).ToLocalTime()

    foo.CreationTime <> defaultTime
with
// hmm
:? FileNotFoundException -> false
"""
        config
    |> prepend newline
    |> should
        equal
        """
try
    let defaultTime = (DateTime.FromFileTimeUtc 0L).ToLocalTime()

    foo.CreationTime <> defaultTime
with
// hmm
| :? FileNotFoundException -> false
"""

[<Test>]
let ``comment above pipe of try/with named clause, 1686`` () =
    formatSourceString
        false
        """
namespace Foo

module Foo =
    let a =
        try
            failwith ""
        with
        // hi!
        | :? Exception as e ->
            failwith ""
"""
        { config with
            SpaceBeforeColon = true
            SpaceBeforeSemicolon = true
            IndentOnTryWith = true }
    |> prepend newline
    |> should
        equal
        """
namespace Foo

module Foo =
    let a =
        try
            failwith ""
        with
            // hi!
            | :? Exception as e -> failwith ""
"""

[<Test>]
let ``comment above pipe of try/with named clause, idempotent`` () =
    formatSourceString
        false
        """
namespace Foo

module Foo =
    let a =
        try
            failwith ""
        with
            // hi!
            :? Exception as e -> failwith ""
"""
        { config with
            SpaceBeforeColon = true
            SpaceBeforeSemicolon = true
            IndentOnTryWith = true }
    |> prepend newline
    |> should
        equal
        """
namespace Foo

module Foo =
    let a =
        try
            failwith ""
        with
            // hi!
            | :? Exception as e -> failwith ""
"""

[<Test>]
let ``respect IndentOnTryWith setting when there is trivia before SynMatchClause_Clause, 1647`` () =
    formatSourceString
        false
        """
module Foo =
    let blah () =
        match foo with
        | Thing crate ->

        crate.Apply
            { new Evaluator<_, _> with
                member __.Eval inner teq =
                    let foo =
                        // blah
                        let exists =
                            try
                                let defaultTime =
                                    (DateTime.FromFileTimeUtc 0L).ToLocalTime ()

                                foo.CreationTime <> defaultTime
                            with
                            // hmm
                            :? FileNotFoundException -> false

                        exists

                    ()
            }
"""
        { config with
            SpaceBeforeUppercaseInvocation = true
            SpaceBeforeClassConstructor = true
            SpaceBeforeMember = true
            SpaceBeforeColon = true
            SpaceBeforeSemicolon = true
            MultilineBlockBracketsOnSameColumn = true
            NewlineBetweenTypeDefinitionAndMembers = true
            KeepIfThenInSameLine = true
            AlignFunctionSignatureToIndentation = true
            AlternativeLongMemberDefinitions = true
            MultiLineLambdaClosingNewline = true
            KeepIndentInBranch = true }
    |> prepend newline
    |> should
        equal
        """
module Foo =
    let blah () =
        match foo with
        | Thing crate ->

        crate.Apply
            { new Evaluator<_, _> with
                member __.Eval inner teq =
                    let foo =
                        // blah
                        let exists =
                            try
                                let defaultTime = (DateTime.FromFileTimeUtc 0L).ToLocalTime ()

                                foo.CreationTime <> defaultTime
                            with
                            // hmm
                            | :? FileNotFoundException -> false

                        exists

                    ()
            }
"""

[<Test>]
let ``short catch clause in try/with should have pipe, 1571`` () =
    formatSourceString
        false
        """
try
    ()
with
| exc ->
    ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
try
    ()
with
| exc -> ()
"""

[<Test>]
let ``short catch clause in try/with should have pipe, IndentOnTryWith = true`` () =
    formatSourceString
        false
        """
try
    ()
with
| exc ->
    ()
"""
        { config with IndentOnTryWith = true }
    |> prepend newline
    |> should
        equal
        """
try
    ()
with
    | exc -> ()
"""

[<Test>]
let ``try/with in infix expression should be indented, 1746`` () =
    formatSourceString
        false
        """
    let isAbstractNonVirtualMember (m: FSharpMemberOrFunctionOrValue) =
      // is an abstract member
      m.IsDispatchSlot
      // this member doesn't implement anything
      && (try m.ImplementedAbstractSignatures <> null &&  m.ImplementedAbstractSignatures.Count = 0 with _ -> true) // exceptions here trying to acces the member means we're safe
      // this member is not an override
      && not m.IsOverrideOrExplicitInterfaceImplementation
"""
        config
    |> prepend newline
    |> should
        equal
        """
let isAbstractNonVirtualMember (m: FSharpMemberOrFunctionOrValue) =
    // is an abstract member
    m.IsDispatchSlot
    // this member doesn't implement anything
    && (try
            m.ImplementedAbstractSignatures <> null
            && m.ImplementedAbstractSignatures.Count = 0
        with
        | _ -> true) // exceptions here trying to acces the member means we're safe
    // this member is not an override
    && not m.IsOverrideOrExplicitInterfaceImplementation
"""
