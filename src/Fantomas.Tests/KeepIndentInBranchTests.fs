module Fantomas.Tests.KeepIndentInBranchTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper
open Fantomas.FormatConfig

let config =
    { config with
          KeepIndentInBranch = true }

[<Test>]
let ``single expression in if branch, multiple expressions in else branch`` () =
    formatSourceString
        false
        """
let foo () =
    if someCondition then
        0
    else
        let config = Configuration.Read "/myfolder/myfile.xml"
        let result = Process.main config otherArg
        if result.IsOk then
            0
        else
            -1
"""
        config
    |> prepend newline
    |> should
        equal
        """
let foo () =
    if someCondition then
        0
    else

    let config =
        Configuration.Read "/myfolder/myfile.xml"

    let result = Process.main config otherArg
    if result.IsOk then 0 else -1
"""

[<Test>]
let ``sequential in if branch, let or use in else branch`` () =
    formatSourceString
        false
        """
open Library

type RunMode =
    | Dry
    | Wet

let main argv =
    let args = parse argv

    let instructions = Library.foo args
    if args.DryRun = RunMode.Dry then
        printfn "Would execute actions, but --dry-run was supplied: %+A" instructions
        0
    else
        // proceed with main method
        let output = Library.execute instructions
        // do more stuff
        0
"""
        config
    |> prepend newline
    |> should
        equal
        """
open Library

type RunMode =
    | Dry
    | Wet

let main argv =
    let args = parse argv

    let instructions = Library.foo args

    if args.DryRun = RunMode.Dry then
        printfn "Would execute actions, but --dry-run was supplied: %+A" instructions
        0
    else
    // proceed with main method
    let output = Library.execute instructions
    // do more stuff
    0
"""

[<Test>]
let ``sequential in first clause, let or use in last clause`` () =
    formatSourceString
        false
        """
open Library

type RunMode =
    | Dry
    | Wet

let main argv =
    let args = parse argv

    let instructions = Library.foo args
    match args.DryRun with
    | RunMode.Dry ->
        printfn "Would execute actions, but --dry-run was supplied: %+A" instructions
        0
    | RunMode.Wet ->
        // proceed with main method
        let output = Library.execute instructions
        // do more stuff
        0
"""
        config
    |> prepend newline
    |> should
        equal
        """
open Library

type RunMode =
    | Dry
    | Wet

let main argv =
    let args = parse argv

    let instructions = Library.foo args

    match args.DryRun with
    | RunMode.Dry ->
        printfn "Would execute actions, but --dry-run was supplied: %+A" instructions
        0
    | RunMode.Wet ->
    // proceed with main method
    let output = Library.execute instructions
    // do more stuff
    0
"""

[<Test>]
let ``always add newline before un-indented last clause`` () =
    formatSourceString
        false
        """
open Library

type RunMode =
    | Dry
    | Wet

let main argv =
    let args = parse argv

    let instructions = Library.foo args
    match args.DryRun with
    | RunMode.Dry ->
        printfn "Would execute actions, but --dry-run was supplied: %+A" instructions
        0
    | RunMode.Wet ->
        printfn "here it comes"
        let output = Library.execute instructions
        // do more stuff
        0
"""
        config
    |> prepend newline
    |> should
        equal
        """
open Library

type RunMode =
    | Dry
    | Wet

let main argv =
    let args = parse argv

    let instructions = Library.foo args

    match args.DryRun with
    | RunMode.Dry ->
        printfn "Would execute actions, but --dry-run was supplied: %+A" instructions
        0
    | RunMode.Wet ->

    printfn "here it comes"
    let output = Library.execute instructions
    // do more stuff
    0
"""

[<Test>]
let ``sequential in first clause, let or use in last clause, match bang`` () =
    formatSourceString
        false
        """
open Library

type RunMode =
    | Dry
    | Wet

let main argv =
    let args = parse argv

    let instructions = Library.foo args
    match! args.DryRun with
    | RunMode.Dry ->
        printfn "Would execute actions, but --dry-run was supplied: %+A" instructions
        0
    | RunMode.Wet ->
        // proceed with main method
        let output = Library.execute instructions
        // do more stuff
        0
"""
        config
    |> prepend newline
    |> should
        equal
        """
open Library

type RunMode =
    | Dry
    | Wet

let main argv =
    let args = parse argv

    let instructions = Library.foo args

    match! args.DryRun with
    | RunMode.Dry ->
        printfn "Would execute actions, but --dry-run was supplied: %+A" instructions
        0
    | RunMode.Wet ->
    // proceed with main method
    let output = Library.execute instructions
    // do more stuff
    0
"""

[<Test>]
let ``match is return value of SynBinding`` () =
    formatSourceString
        false
        """
module Foo =
    let make () =
        { new IBar with
            member __.DoIt (i : int) =
                doSomeStuff ()
                match i with
                | 0 ->
                    handleIt ()
                    None
                | _ ->

                let baz = foo ()
                match int baz with
                | 0 ->
                    handleAgain ()
                    None
                | _ ->

                    doEvenMore ()
                    Some 3
        }
"""
        config
    |> prepend newline
    |> should
        equal
        """
module Foo =
    let make () =
        { new IBar with
            member __.DoIt(i: int) =
                doSomeStuff ()

                match i with
                | 0 ->
                    handleIt ()
                    None
                | _ ->

                let baz = foo ()

                match int baz with
                | 0 ->
                    handleAgain ()
                    None
                | _ ->

                doEvenMore ()
                Some 3 }
"""

[<Test>]
let ``match is return value of SynBinding with return type`` () =
    formatSourceString
        false
        """
module Foo =
    let make () =
        { new IBar with
            member __.DoIt (i : int) : int option =
                doSomeStuff ()
                match i with
                | 0 ->
                    handleIt ()
                    None
                | _ ->

                let baz = foo ()
                match int baz with
                | 0 ->
                    handleAgain ()
                    None
                | _ ->

                    doEvenMore ()
                    Some 3
        }
"""
        config
    |> prepend newline
    |> should
        equal
        """
module Foo =
    let make () =
        { new IBar with
            member __.DoIt(i: int) : int option =
                doSomeStuff ()

                match i with
                | 0 ->
                    handleIt ()
                    None
                | _ ->

                let baz = foo ()

                match int baz with
                | 0 ->
                    handleAgain ()
                    None
                | _ ->

                doEvenMore ()
                Some 3 }
"""

[<Test>]
let ``match is return value of let binding`` () =
    formatSourceString
        false
        """
let sum a b =
    match a, b with
    | Negative _, _
    | _, Negative _ ->
        None
    | a, b ->
        logMessage "a and b are both positive"
        // some grand explainer about the code
        Some (a + b)
"""
        config
    |> prepend newline
    |> should
        equal
        """
let sum a b =
    match a, b with
    | Negative _, _
    | _, Negative _ -> None
    | a, b ->

    logMessage "a and b are both positive"
    // some grand explainer about the code
    Some(a + b)
"""

[<Test>]
let ``nested match expression should not indent in both cases`` () =
    formatSourceString
        false
        """
let sum a b =
    match a with
    | Negative -> None
    | _ ->
        match b with
        | Negative -> None
        | _ ->
            logMessage "a and b are both positive"
            // some grand explainer about the code
            Some(a + b)
"""
        config
    |> prepend newline
    |> should
        equal
        """
let sum a b =
    match a with
    | Negative -> None
    | _ ->

    match b with
    | Negative -> None
    | _ ->

    logMessage "a and b are both positive"
    // some grand explainer about the code
    Some(a + b)
"""

[<Test>]
let ``nested ifThenElse should not indent in both cases`` () =
    formatSourceString
        false
        """
let sum a b =
    if a < 0 then
        None
    else
        logMessage "a is positive"
        if b < 0 then
            None
        else
            logMessage "a and b are both positive"
            // some grand explainer about the code
            Some(a + b)
"""
        config
    |> prepend newline
    |> should
        equal
        """
let sum a b =
    if a < 0 then
        None
    else

    logMessage "a is positive"

    if b < 0 then
        None
    else

    logMessage "a and b are both positive"
    // some grand explainer about the code
    Some(a + b)
"""

[<Test>]
let ``match followed by if should not indent in both cases`` () =
    formatSourceString
        false
        """
let sum a b =
    if a < 0 then
        None
    else
        logMessage "a is positive"
        match b with
        | Negative -> None
        | _ ->
            logMessage "a and b are both positive"
            // some grand explainer about the code
            Some (a + b)
"""
        config
    |> prepend newline
    |> should
        equal
        """
let sum a b =
    if a < 0 then
        None
    else

    logMessage "a is positive"

    match b with
    | Negative -> None
    | _ ->

    logMessage "a and b are both positive"
    // some grand explainer about the code
    Some(a + b)
"""

[<Test>]
let ``default config should indent`` () =
    formatSourceString
        false
        """
/// Find out the end token
let rec getEndCol (r: Range) (tokenizer: FSharpLineTokenizer) lexState =
    match tokenizer.ScanToken(!lexState) with
    | Some (tok), state ->
        Debug.WriteLine("End token: {0}", sprintf "%A" tok |> box)
        if tok.RightColumn >= r.EndColumn
           && isSignificantToken tok then
            tok.RightColumn
        else
        lexState := state
        getEndCol r tokenizer lexState
    | None, _ -> r.EndColumn
"""
        FormatConfig.Default
    |> prepend newline
    |> should
        equal
        """
/// Find out the end token
let rec getEndCol (r: Range) (tokenizer: FSharpLineTokenizer) lexState =
    match tokenizer.ScanToken(!lexState) with
    | Some (tok), state ->
        Debug.WriteLine("End token: {0}", sprintf "%A" tok |> box)

        if tok.RightColumn >= r.EndColumn
           && isSignificantToken tok then
            tok.RightColumn
        else
            lexState := state
            getEndCol r tokenizer lexState
    | None, _ -> r.EndColumn
"""

[<Test>]
let ``keep indent in SynModuleDecl.Do expression,  1569`` () =
    formatSourceString
        false
        """
if blah then
    printfn "Aborting."
    1
else

printfn "Foo"
let message = baz
if baz then
    printfn "Aborting."
    1
else
0
"""
        config
    |> prepend newline
    |> should
        equal
        """
if blah then
    printfn "Aborting."
    1
else

printfn "Foo"
let message = baz

if baz then
    printfn "Aborting."
    1
else
    0
"""

[<Test>]
let ``inside lambda expression`` () =
    formatSourceString
        false
        """
let foo =
    bar
    |> List.filter (fun i ->
        if false then
            false
        else

        let m = quux
        quux.Success && somethingElse
    )
"""
        config
    |> prepend newline
    |> should
        equal
        """
let foo =
    bar
    |> List.filter
        (fun i ->
            if false then
                false
            else

            let m = quux
            quux.Success && somethingElse)
"""

[<Test>]
let ``inside desugared lambda expression`` () =
    formatSourceString
        false
        """
let foo =
    bar
    |> List.filter (fun { Index = i } ->
        if false then
            false
        else

        let m = quux
        quux.Success && somethingElse
    )
"""
        config
    |> prepend newline
    |> should
        equal
        """
let foo =
    bar
    |> List.filter
        (fun { Index = i } ->
            if false then
                false
            else

            let m = quux
            quux.Success && somethingElse)
"""

[<Test>]
let ``match expression inside lambda expression`` () =
    formatSourceString
        false
        """
lock lockingObj (fun () ->
    if not thing then
        printfn ""

    match error with
    | Some error ->
        if foo then ()
        thing ()
        false
    | None ->

    match list1, list2, list3 with
    | [], [], [] ->
        stuff ()
        true
    | [], [], _ ->
        moreStuff ()
        true
    | _ ->

    doMoreThings ()
    false
)
"""
        config
    |> prepend newline
    |> should
        equal
        """
lock
    lockingObj
    (fun () ->
        if not thing then printfn ""

        match error with
        | Some error ->
            if foo then ()
            thing ()
            false
        | None ->

        match list1, list2, list3 with
        | [], [], [] ->
            stuff ()
            true
        | [], [], _ ->
            moreStuff ()
            true
        | _ ->

        doMoreThings ()
        false)
"""

[<Test>]
let ``try with in else branch`` () =
    formatSourceString
        false
        """
type Foo () =
    interface IDisposable with
        override __.Dispose () =
            if not blah then
                ()
            else

            try
                try
                    cleanUp ()
                with
                | :? IOException ->
                    foo ()
            with exc ->
                foooo ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
type Foo() =
    interface IDisposable with
        override __.Dispose() =
            if not blah then
                ()
            else

            try
                try
                    cleanUp ()
                with :? IOException -> foo ()
            with exc -> foooo ()
"""

[<Test>]
let ``let or uses with sequential that has match `` () =
    formatSourceString
        false
        """
module Foo =

    let main (args : _) =
        let thing1 = ()
        printfn ""

        match instructions with
        | Error e ->
            printfn ""
            2
        | Ok (thing, instructions) ->

        log.LogInformation("")
        match Something.foo args with
        | DryRunMode.Dry ->
            printfn ""
            0
        | DryRunMode.Wet ->

        Thing.execute
            bar
            baz
            (thing, instructions)
        0
"""
        config
    |> prepend newline
    |> should
        equal
        """
module Foo =

    let main (args: _) =
        let thing1 = ()
        printfn ""

        match instructions with
        | Error e ->
            printfn ""
            2
        | Ok (thing, instructions) ->

        log.LogInformation("")

        match Something.foo args with
        | DryRunMode.Dry ->
            printfn ""
            0
        | DryRunMode.Wet ->

        Thing.execute bar baz (thing, instructions)
        0
"""

[<Test>]
let ``let or uses with sequential that has if/then/else `` () =
    formatSourceString
        false
        """
module Foo =

    let main (args : _) =
        let thing1 = ()
        printfn ""

        if hasInstructions () then
            printfn ""
            2
        else

        log.LogInformation("")
        match Something.foo args with
        | DryRunMode.Dry ->
            printfn ""
            0
        | DryRunMode.Wet ->

        Thing.execute
            bar
            baz
            (thing, instructions)
        0
"""
        config
    |> prepend newline
    |> should
        equal
        """
module Foo =

    let main (args: _) =
        let thing1 = ()
        printfn ""

        if hasInstructions () then
            printfn ""
            2
        else

        log.LogInformation("")

        match Something.foo args with
        | DryRunMode.Dry ->
            printfn ""
            0
        | DryRunMode.Wet ->

        Thing.execute bar baz (thing, instructions)
        0
"""

[<Test>]
let ``in combination with NewlinesAroundInnerMultilineExpressions`` () =
    formatSourceString
        false
        """
module Foo =
    let main (args: _) =
        let thing1 = ()
        printfn ""
        if hasInstructions () then
            printfn ""
            2
        else
        log.LogInformation("")
        match Something.foo args with
        | DryRunMode.Dry ->
            printfn ""
            0
        | DryRunMode.Wet ->
        Thing.execute bar baz (thing, instructions)
        0
"""
        { config with
              BlankLinesAroundNestedMultilineExpressions = false }
    |> prepend newline
    |> should
        equal
        """
module Foo =
    let main (args: _) =
        let thing1 = ()
        printfn ""
        if hasInstructions () then
            printfn ""
            2
        else

        log.LogInformation("")
        match Something.foo args with
        | DryRunMode.Dry ->
            printfn ""
            0
        | DryRunMode.Wet ->

        Thing.execute bar baz (thing, instructions)
        0
"""

[<Test>]
let ``sequential, let bindings, keep indent match,  1621`` () =
    formatSourceString
        false
        """
let main (args : Options) =
    log.LogDebug ("Command line options: {Options}", args.ToString())

    let includes =
        if ArgParser.defaultArg args.Flag then
            Flag.Include
        else
            Flag.Exclude

    match dryRunMode with
    | DryRunMode.Dry ->
        log.LogInformation ("No changes made due to --dry-run.")
        0
    | DryRunMode.Wet ->

    match requested with
    | None ->
        log.LogWarning ("No changes required; no action taken.")
        0
    | Some branched ->

    branched
    |> blah
    |> fun i -> log.LogInformation ("Done:\n{It}", i)

    0
"""
        { config with
              MaxLineLength = 100
              SpaceBeforeUppercaseInvocation = true
              SpaceBeforeClassConstructor = true
              SpaceBeforeMember = true
              SpaceBeforeColon = true
              SpaceBeforeSemicolon = true
              IndentOnTryWith = true
              MultilineBlockBracketsOnSameColumn = true
              NewlineBetweenTypeDefinitionAndMembers = true
              AlignFunctionSignatureToIndentation = true
              AlternativeLongMemberDefinitions = true
              MultiLineLambdaClosingNewline = true
              DisableElmishSyntax = true
              KeepIndentInBranch = true }
    |> prepend newline
    |> should
        equal
        """
let main (args : Options) =
    log.LogDebug ("Command line options: {Options}", args.ToString ())

    let includes =
        if ArgParser.defaultArg args.Flag then
            Flag.Include
        else
            Flag.Exclude

    match dryRunMode with
    | DryRunMode.Dry ->
        log.LogInformation ("No changes made due to --dry-run.")
        0
    | DryRunMode.Wet ->

    match requested with
    | None ->
        log.LogWarning ("No changes required; no action taken.")
        0
    | Some branched ->

    branched
    |> blah
    |> fun i -> log.LogInformation ("Done:\n{It}", i)

    0
"""

[<Test>]
let ``sequential, let bindings, keep indent if`` () =
    formatSourceString
        false
        """
let main (args : Options) =
    log.LogDebug ("Command line options: {Options}", args.ToString())

    let includes =
        if ArgParser.defaultArg args.Flag then
            Flag.Include
        else
            Flag.Exclude

    if dryRunMode then
        log.LogInformation ("No changes made due to --dry-run.")
        0
    else

    match requested with
    | None ->
        log.LogWarning ("No changes required; no action taken.")
        0
    | Some branched ->

    branched
    |> blah
    |> fun i -> log.LogInformation ("Done:\n{It}", i)

    0
"""
        { config with
              MaxLineLength = 100
              SpaceBeforeUppercaseInvocation = true
              SpaceBeforeClassConstructor = true
              SpaceBeforeMember = true
              SpaceBeforeColon = true
              SpaceBeforeSemicolon = true
              IndentOnTryWith = true
              MultilineBlockBracketsOnSameColumn = true
              NewlineBetweenTypeDefinitionAndMembers = true
              AlignFunctionSignatureToIndentation = true
              AlternativeLongMemberDefinitions = true
              MultiLineLambdaClosingNewline = true
              DisableElmishSyntax = true
              KeepIndentInBranch = true }
    |> prepend newline
    |> should
        equal
        """
let main (args : Options) =
    log.LogDebug ("Command line options: {Options}", args.ToString ())

    let includes =
        if ArgParser.defaultArg args.Flag then
            Flag.Include
        else
            Flag.Exclude

    if dryRunMode then
        log.LogInformation ("No changes made due to --dry-run.")
        0
    else

    match requested with
    | None ->
        log.LogWarning ("No changes required; no action taken.")
        0
    | Some branched ->

    branched
    |> blah
    |> fun i -> log.LogInformation ("Done:\n{It}", i)

    0
"""

[<Test>]
let ``single pattern match should keep indent, 1638`` () =
    formatSourceString
        false
        """
[<NoEquality ; NoComparison>]
type Foo<'context, 'a> =
    | Apply of ApplyCrate<'context, 'a>

and [<CustomEquality ; NoComparison>] Bar<'context, 'a> =
    internal {
        Hash : int
        Foo : Foo<'a, 'b>
    }
    member this.InnerEquals<'innerContextLongLongLong, 'd, 'e> (a : Foo<'innerContextLongLongLong, 'd>) (b : Foo<'innerContext, 'd>) (cont : bool -> 'e) : 'e =
        if a.Hash <> b.Hash then cont false
        else
            match a.Foo, b.Foo with
            | Foo.Apply a, Foo.Apply b ->
                a.Apply { new ApplyEval<_, _, _> with
                    member __.Eval<'bb> (a : Foo<'innerContextLongLongLong, 'bb -> 'b> * Foo<'innerContextLongLongLong, 'bb>) =
                        let (af, av) = a
                        b.Apply { new ApplyEval<_, _, _> with
                            member __.Eval<'cb> (b : Foo<'innerContextLongLongLong, 'cb -> 'b> * Foo<'innerContextLongLongLong, 'bc>) =
                                let (bf, bv) = b
                                if typeof<'bb> = typeof<'cb> then
                                    let bv = unbox<Foo<'innerContextLongLongLong, 'bb>> bv
                                    this.InnerEquals av bv (fun inner ->
                                        if inner then
                                            let bv = unbox<Foo<'innerContextLongLongLong, 'bb -> 'b>> bf
                                            this.InnerEquals af bf cont
                                        else cont false
                                    )
                                else cont false
                        }
                }
"""
        { config with
              MaxLineLength = 100
              SpaceBeforeUppercaseInvocation = true
              SpaceBeforeClassConstructor = true
              SpaceBeforeMember = true
              SpaceBeforeColon = true
              SpaceBeforeSemicolon = true
              MultilineBlockBracketsOnSameColumn = true
              KeepIfThenInSameLine = true
              KeepIndentInBranch = true
              AlignFunctionSignatureToIndentation = true
              AlternativeLongMemberDefinitions = true
              MultiLineLambdaClosingNewline = true }
    |> prepend newline
    |> should
        equal
        """
[<NoEquality ; NoComparison>]
type Foo<'context, 'a> = Apply of ApplyCrate<'context, 'a>

and [<CustomEquality ; NoComparison>] Bar<'context, 'a> =
    internal
        {
            Hash : int
            Foo : Foo<'a, 'b>
        }
    member this.InnerEquals<'innerContextLongLongLong, 'd, 'e>
        (a : Foo<'innerContextLongLongLong, 'd>)
        (b : Foo<'innerContext, 'd>)
        (cont : bool -> 'e)
        : 'e
        =
        if a.Hash <> b.Hash then
            cont false
        else

        match a.Foo, b.Foo with
        | Foo.Apply a, Foo.Apply b ->

        a.Apply
            { new ApplyEval<_, _, _> with
                member __.Eval<'bb>
                    (a : Foo<'innerContextLongLongLong, 'bb -> 'b> * Foo<'innerContextLongLongLong, 'bb>)
                    =
                    let (af, av) = a

                    b.Apply
                        { new ApplyEval<_, _, _> with
                            member __.Eval<'cb>
                                (b : Foo<'innerContextLongLongLong, 'cb -> 'b> * Foo<'innerContextLongLongLong, 'bc>)
                                =
                                let (bf, bv) = b

                                if typeof<'bb> = typeof<'cb> then
                                    let bv =
                                        unbox<Foo<'innerContextLongLongLong, 'bb>> bv

                                    this.InnerEquals
                                        av
                                        bv
                                        (fun inner ->
                                            if inner then
                                                let bv =
                                                    unbox<Foo<'innerContextLongLongLong, 'bb -> 'b>>
                                                        bf

                                                this.InnerEquals af bf cont
                                            else
                                                cont false
                                        )
                                else
                                    cont false
                        }
            }
"""
