module Fantomas.Tests.KeepIndentInBranchTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper
open Fantomas.FormatConfig

let config = { config with KeepIndentInBranch = true }

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

    let config = Configuration.Read "/myfolder/myfile.xml"
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
    |> List.filter (fun i ->
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
    |> List.filter (fun { Index = i } ->
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
lock lockingObj (fun () ->
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
                with
                | :? IOException -> foo ()
            with
            | exc -> foooo ()
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
        { config with BlankLinesAroundNestedMultilineExpressions = false }
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
                                    let bv = unbox<Foo<'innerContextLongLongLong, 'bb>> bv

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

[<Test>]
let ``ifdef trivia should not influence outcome, 1646`` () =
    formatSourceString
        false
        """
module Foo =
    [<Foo>]
    let blah<'a> config : Type =
#if DEBUG
        failwith ""
#endif
        DoThing.doIt ()
        let result = Runner.Run<'a> config

        if successful |> List.isEmpty then
            result
        else

        let errors =
            unsuccessful
            |> List.filter (fun report ->
                not report.BuildResult.IsBuildSuccess
                || not report.BuildResult.IsGenerateSuccess
            )
            |> List.map (fun report -> report.BuildResult.ErrorMessage)

        failwith ""
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
    [<Foo>]
    let blah<'a> config : Type =
#if DEBUG
        failwith ""
#endif
        DoThing.doIt ()
        let result = Runner.Run<'a> config

        if successful |> List.isEmpty then
            result
        else

        let errors =
            unsuccessful
            |> List.filter (fun report ->
                not report.BuildResult.IsBuildSuccess
                || not report.BuildResult.IsGenerateSuccess
            )
            |> List.map (fun report -> report.BuildResult.ErrorMessage)

        failwith ""
"""

[<Test>]
let ``ifdef trivia should not influence outcome, idempotent`` () =
    formatSourceString
        false
        """
module Foo =
    [<Foo>]
    let blah<'a> config : Type =
#if DEBUG
        failwith ""
#endif
        DoThing.doIt ()
        let result = Runner.Run<'a> config

        if successful |> List.isEmpty then
            result
        else

        let errors =
            unsuccessful
            |> List.filter (fun report ->
                not report.BuildResult.IsBuildSuccess
                || not report.BuildResult.IsGenerateSuccess
            )
            |> List.map (fun report -> report.BuildResult.ErrorMessage)

        failwith ""
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
    [<Foo>]
    let blah<'a> config : Type =
#if DEBUG
        failwith ""
#endif
        DoThing.doIt ()
        let result = Runner.Run<'a> config

        if successful |> List.isEmpty then
            result
        else

        let errors =
            unsuccessful
            |> List.filter (fun report ->
                not report.BuildResult.IsBuildSuccess
                || not report.BuildResult.IsGenerateSuccess
            )
            |> List.map (fun report -> report.BuildResult.ErrorMessage)

        failwith ""
"""

[<Test>]
let ``multiple nested Sequential expressions, 1714`` () =
    formatSourceString
        false
        """
namespace Foo

module Bar =

    [<EntryPoint>]
    let main argv =
        let args = foo
        printfn ""
        printfn ""
        printfn ""
        let m = ""
        if foo then
            printfn "aborting"
            1
        else

        printfn "blah"
        let m = ""
        if foo then
            printfn "aborting"
            1
        else

        let fs = FileSystem ()
        use f = fs.File.Open("")
        0
"""
        config
    |> prepend newline
    |> should
        equal
        """
namespace Foo

module Bar =

    [<EntryPoint>]
    let main argv =
        let args = foo
        printfn ""
        printfn ""
        printfn ""
        let m = ""

        if foo then
            printfn "aborting"
            1
        else

        printfn "blah"
        let m = ""

        if foo then
            printfn "aborting"
            1
        else

        let fs = FileSystem()
        use f = fs.File.Open("")
        0
"""

[<Test>]
let ``multiple nested LetOrUse expressions, 1717`` () =
    formatSourceString
        false
        """
module Foo =
    let main (args : _) =
        let fs = FileSystem()
        let ab = ThingOne.make fs (ThingFour.defaultFoo args.Args.ThingOne)

        let thingFive = ThingFive.thingFive fs ab

        use loggerFactory = Logging.make thingFive LogEventLevel.Verbose LogEventLevel.Information
        let log = loggerFactory.CreateLogger "foo"
        log.LogDebug("Command line options used: {CommandLine}", args)
        log.LogInformation("Thing One: {ThingOne}", ThingOne.getFoo ab)

        let thing, cd =
            args.Thing
            |> Args.render loggerFactory thingFive

        let skipBehaviour =
            if defaultArg args.Skip then
                Options.Exclude
            else
                Options.Include

        let thing, errors = Information.make fs ab thing
        use thing = thing

        let operationToDo =
            Operations.operation loggerFactory fs ab tp thing (DUCase args.Thing) args.Info skipBehaviour

        match ThingEight.defaultFun args.DryRun with
        | DryRunMode.Dry ->
            log.LogInformation("No changes made due to --dry-run.")
            0
        | DryRunMode.Wet ->

        match operationToDo with
        | None ->
            log.LogWarning("No changes required; no action taken.")
            0
        | Some thingsToDo ->

        thingsToDo
        |> Operations.perform loggerFactory (errors |> Map.keys)
        |> Seq.map (fun i -> i.Name)
        |> String.concat "\n"
        |> fun i -> log.LogInformation("Completed operation:\n{Result}", i)
        0
"""
        { config with
            MultiLineLambdaClosingNewline = true
            MultilineBlockBracketsOnSameColumn = true
            AlternativeLongMemberDefinitions = true }
    |> prepend newline
    |> should
        equal
        """
module Foo =
    let main (args: _) =
        let fs = FileSystem()
        let ab = ThingOne.make fs (ThingFour.defaultFoo args.Args.ThingOne)

        let thingFive = ThingFive.thingFive fs ab

        use loggerFactory =
            Logging.make thingFive LogEventLevel.Verbose LogEventLevel.Information

        let log = loggerFactory.CreateLogger "foo"
        log.LogDebug("Command line options used: {CommandLine}", args)
        log.LogInformation("Thing One: {ThingOne}", ThingOne.getFoo ab)

        let thing, cd = args.Thing |> Args.render loggerFactory thingFive

        let skipBehaviour =
            if defaultArg args.Skip then
                Options.Exclude
            else
                Options.Include

        let thing, errors = Information.make fs ab thing
        use thing = thing

        let operationToDo =
            Operations.operation loggerFactory fs ab tp thing (DUCase args.Thing) args.Info skipBehaviour

        match ThingEight.defaultFun args.DryRun with
        | DryRunMode.Dry ->
            log.LogInformation("No changes made due to --dry-run.")
            0
        | DryRunMode.Wet ->

        match operationToDo with
        | None ->
            log.LogWarning("No changes required; no action taken.")
            0
        | Some thingsToDo ->

        thingsToDo
        |> Operations.perform loggerFactory (errors |> Map.keys)
        |> Seq.map (fun i -> i.Name)
        |> String.concat "\n"
        |> fun i -> log.LogInformation("Completed operation:\n{Result}", i)

        0
"""

[<Test>]
let ``multiline if condition is indented, 1729`` () =
    formatSourceString
        false
        """
let x y =
            if not (
                result.HasResultsFor(
                    [ "label"
                      "ipv4"
                      "macAddress"
                      "medium"
                      "manufacturer" ]
                )
            ) then
                None
            else

            let label = string result.["label"]
            let ipv4 = string result.["ipv4"]
            None
"""
        config
    |> prepend newline
    |> should
        equal
        """
let x y =
    if
        not
            (
                result.HasResultsFor(
                    [ "label"
                      "ipv4"
                      "macAddress"
                      "medium"
                      "manufacturer" ]
                )
            )
    then
        None
    else

    let label = string result.["label"]
    let ipv4 = string result.["ipv4"]
    None
"""

[<Test>]
let ``value binding, 1728`` () =
    formatSourceString
        false
        """
let x =
            if not (
                result.HasResultsFor(
                    [ "label"
                      "ipv4"
                      "macAddress"
                      "medium"
                      "manufacturer" ]
                )
            ) then
                None
            else

            let label = string result.["label"]
            let ipv4 = string result.["ipv4"]
            None
"""
        config
    |> prepend newline
    |> should
        equal
        """
let x =
    if
        not
            (
                result.HasResultsFor(
                    [ "label"
                      "ipv4"
                      "macAddress"
                      "medium"
                      "manufacturer" ]
                )
            )
    then
        None
    else

    let label = string result.["label"]
    let ipv4 = string result.["ipv4"]
    None
"""

[<Test>]
let ``combination with MultiLineLambdaClosingNewline, 1715`` () =
    formatSourceString
        false
        """
module Foo =
    let bar () =
        let baz =
            []
            |> List.filter (fun ref ->
                if ref.Type <> "h" then false
                else
                let m = regex.Match ref.To
                m.Success && things |> Set.contains (m.Groups.[1].ToString ())
            )
        0
"""
        { config with
            SpaceBeforeUppercaseInvocation = true
            SpaceBeforeClassConstructor = true
            SpaceBeforeMember = true
            SpaceBeforeColon = true
            SpaceBeforeSemicolon = true
            AlignFunctionSignatureToIndentation = true
            AlternativeLongMemberDefinitions = true
            MultiLineLambdaClosingNewline = true
            KeepIndentInBranch = true }
    |> prepend newline
    |> should
        equal
        """
module Foo =
    let bar () =
        let baz =
            []
            |> List.filter (fun ref ->
                if ref.Type <> "h" then
                    false
                else

                let m = regex.Match ref.To

                m.Success
                && things |> Set.contains (m.Groups.[1].ToString ())
            )

        0
"""

[<Test>]
let ``combination with MultiLineLambdaClosingNewline, multiple params`` () =
    formatSourceString
        false
        """
module Foo =
    let bar () =
        let baz =
            []
            |> List.filterWith X (fun ref ->
                if ref.Type <> "h" then false
                else
                let m = regex.Match ref.To
                m.Success && things |> Set.contains (m.Groups.[1].ToString ())
            )
        0
"""
        { config with
            SpaceBeforeUppercaseInvocation = true
            SpaceBeforeClassConstructor = true
            SpaceBeforeMember = true
            SpaceBeforeColon = true
            SpaceBeforeSemicolon = true
            AlignFunctionSignatureToIndentation = true
            AlternativeLongMemberDefinitions = true
            MultiLineLambdaClosingNewline = true
            KeepIndentInBranch = true }
    |> prepend newline
    |> should
        equal
        """
module Foo =
    let bar () =
        let baz =
            []
            |> List.filterWith
                X
                (fun ref ->
                    if ref.Type <> "h" then
                        false
                    else

                    let m = regex.Match ref.To

                    m.Success
                    && things |> Set.contains (m.Groups.[1].ToString ())
                )

        0
"""

[<Test>]
let ``comment after arrow, 1759`` () =
    formatSourceString
        false
        """
let mapOperationToWebPart (operation: OpenApiOperationDescription) : SynExpr =
    let verb = mkIdentExpr (operation.Method.ToUpper())
    let route =
        match operation with
        | _ -> // no route parameters
            let route = mkAppNonAtomicExpr (mkIdentExpr "route") (mkStringExprConst operation.Path)
            let responseHttpFunc =
                mkLambdaExpr [ ] unitExpr
                |> mkParenExpr
            infixFish route responseHttpFunc

    infixFish verb route
"""
        config
    |> prepend newline
    |> should
        equal
        """
let mapOperationToWebPart (operation: OpenApiOperationDescription) : SynExpr =
    let verb = mkIdentExpr (operation.Method.ToUpper())

    let route =
        match operation with
        | _ -> // no route parameters

        let route =
            mkAppNonAtomicExpr (mkIdentExpr "route") (mkStringExprConst operation.Path)

        let responseHttpFunc = mkLambdaExpr [] unitExpr |> mkParenExpr
        infixFish route responseHttpFunc

    infixFish verb route
"""

[<Test>]
let ``multiline infix application, 1768`` () =
    formatSourceString
        false
        """
let updateModuleInImpl (ast: ParsedInput) (mdl: SynModuleOrNamespace) : ParsedInput =
    match ast with
    | ParsedInput.SigFile _ -> ast
    | ParsedInput.ImplFile _ ->
        ParsedImplFileInput(
            fileName,
            isScript,
            qualifiedNameOfFile,
            scopedPragmas,
            hashDirectives,
            [ mdl ],
            isLastAndCompiled
        )
        |> ParsedInput.ImplFile
"""
        config
    |> prepend newline
    |> should
        equal
        """
let updateModuleInImpl (ast: ParsedInput) (mdl: SynModuleOrNamespace) : ParsedInput =
    match ast with
    | ParsedInput.SigFile _ -> ast
    | ParsedInput.ImplFile _ ->

    ParsedImplFileInput(
        fileName,
        isScript,
        qualifiedNameOfFile,
        scopedPragmas,
        hashDirectives,
        [ mdl ],
        isLastAndCompiled
    )
    |> ParsedInput.ImplFile
"""

[<Test>]
let ``short function application in if branch`` () =
    formatSourceString
        false
        """
let private parseModel (modelSrc: string) : Result<MyReturnType, string list> =
  if not (File.Exists(modelSrc)) then
    Error [ sprintf "Provided modelSrc \"%s\" does not exist" modelSrc ]
  else
    let graph = new QueryableGraph()
    graph.LoadFromFile(modelSrc, TurtleParser())

    let xsdPath =
      Path.Combine(Directory.GetCurrentDirectory(), "Some.xsd")

    let ontologyPath =
      Path.Combine(Directory.GetCurrentDirectory(), "Some.ttl")

    let ontoGraph = new OntologyGraph()
    FileLoader.Load(ontoGraph, ontologyPath)

    let validationErrors =
      GraphValidation.validate xsdPath ontoGraph "http://company.com/meh" graph

    if List.isEmpty validationErrors then
      Ok graph
    else
      Error validationErrors
"""
        config
    |> prepend newline
    |> should
        equal
        """
let private parseModel (modelSrc: string) : Result<MyReturnType, string list> =
    if not (File.Exists(modelSrc)) then
        Error [ sprintf "Provided modelSrc \"%s\" does not exist" modelSrc ]
    else

    let graph = new QueryableGraph()
    graph.LoadFromFile(modelSrc, TurtleParser())

    let xsdPath = Path.Combine(Directory.GetCurrentDirectory(), "Some.xsd")

    let ontologyPath = Path.Combine(Directory.GetCurrentDirectory(), "Some.ttl")

    let ontoGraph = new OntologyGraph()
    FileLoader.Load(ontoGraph, ontologyPath)

    let validationErrors =
        GraphValidation.validate xsdPath ontoGraph "http://company.com/meh" graph

    if List.isEmpty validationErrors then
        Ok graph
    else
        Error validationErrors
"""

[<Test>]
let ``tuple is consider as short branch, 1800`` () =
    formatSourceString
        false
        """
  let nextModel, objectsRemoved =
    List.fold
      (fun acc item ->
        match entityInCurrentModel with
        | None ->
          // look it's a tuple
          nextModel, objectsRemoved
        | Some subjectToRemove ->
          let a = 5
          let b = 6
          someFunctionApp a b |> ignore
          acc)
      state
      []
"""
        { config with
            MultiLineLambdaClosingNewline = true
            KeepIndentInBranch = true }
    |> prepend newline
    |> should
        equal
        """
let nextModel, objectsRemoved =
    List.fold
        (fun acc item ->
            match entityInCurrentModel with
            | None ->
                // look it's a tuple
                nextModel, objectsRemoved
            | Some subjectToRemove ->

            let a = 5
            let b = 6
            someFunctionApp a b |> ignore
            acc
        )
        state
        []
"""

[<Test>]
let ``parenthesis tuple is consider as short branch`` () =
    formatSourceString
        false
        """
  let nextModel, objectsRemoved =
    List.fold
      (fun acc item ->
        match entityInCurrentModel with
        | None ->
          // look it's a tuple but wrapped in parenthesis
          (nextModel, objectsRemoved)
        | Some subjectToRemove ->
          let a = 5
          let b = 6
          someFunctionApp a b |> ignore
          acc)
      state
      []
"""
        { config with
            MultiLineLambdaClosingNewline = true
            KeepIndentInBranch = true }
    |> prepend newline
    |> should
        equal
        """
let nextModel, objectsRemoved =
    List.fold
        (fun acc item ->
            match entityInCurrentModel with
            | None ->
                // look it's a tuple but wrapped in parenthesis
                (nextModel, objectsRemoved)
            | Some subjectToRemove ->

            let a = 5
            let b = 6
            someFunctionApp a b |> ignore
            acc
        )
        state
        []
"""

[<Test>]
let ``long multiline if expression, 1812`` () =
    formatSourceString
        false
        """
module Foo =
    let bar =

        if output.Exists && not (output.EnumerateFiles() |> Seq.isEmpty && onlyContainsFoobar output) then
            Error (FooBarBazError.ErrorCase output)
        else

        let blah =
            let x = y
            None

        {
            Hi = blah
        }
        |> Ok
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
module Foo =
    let bar =

        if
            output.Exists
            && not
                (
                    output.EnumerateFiles () |> Seq.isEmpty
                    && onlyContainsFoobar output
                )
        then
            Error (FooBarBazError.ErrorCase output)
        else

        let blah =
            let x = y
            None

        { Hi = blah } |> Ok
"""

[<Test>]
let ``long multiline match expression`` () =
    formatSourceString
        false
        """
module Foo =
    let bar =

        match output.Exists && not (output.EnumerateFiles() |> Seq.isEmpty && onlyContainsFoobar output) with
        | true ->
            Error (FooBarBazError.ErrorCase output)
        | false ->

        let blah =
            let x = y
            None

        {
            Hi = blah
        }
        |> Ok
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
module Foo =
    let bar =

        match
            output.Exists
            && not
                (
                    output.EnumerateFiles () |> Seq.isEmpty
                    && onlyContainsFoobar output
                )
            with
        | true -> Error (FooBarBazError.ErrorCase output)
        | false ->

        let blah =
            let x = y
            None

        { Hi = blah } |> Ok
"""

[<Test>]
let ``add space after long identifier in if expression, 1816`` () =
    formatSourceString
        false
        """
module Foo =
    let assertConsistent () : unit =
        if veryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryLong then
            ()
        else
            if foo = bar then
                ()
            else
                let leftSet = HashSet (FooBarBaz.keys leftThings)
                leftSet.SymmetricExceptWith (FooBarBaz.keys rightThings)
                |> ignore
"""
        { config with
            MaxLineLength = 100
            MultilineBlockBracketsOnSameColumn = true
            MultiLineLambdaClosingNewline = true
            KeepIndentInBranch = true }
    |> prepend newline
    |> should
        equal
        """
module Foo =
    let assertConsistent () : unit =
        if veryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryLong then
            ()
        else if foo = bar then
            ()
        else

        let leftSet = HashSet(FooBarBaz.keys leftThings)

        leftSet.SymmetricExceptWith(FooBarBaz.keys rightThings)
        |> ignore
"""

[<Test>]
let ``pipe operator application with lambda is considered long, 2003`` () =
    formatSourceString
        false
        """
namespace FicroKanSharp.Test

module TestThing =

    let ``Recursive example`` () =

        match Stream.peel rest with
        | None -> failwith "oh no"
        | Some longName ->
            longName
            |> Map.map (fun _ -> TypedTerm.force<int>)
"""
        config
    |> prepend newline
    |> should
        equal
        """
namespace FicroKanSharp.Test

module TestThing =

    let ``Recursive example`` () =

        match Stream.peel rest with
        | None -> failwith "oh no"
        | Some longName ->

        longName
        |> Map.map (fun _ -> TypedTerm.force<int>)
"""

[<Test>]
let ``else if should be preserved, 1818`` () =
    formatSourceString
        false
        """
module Foo =
    let assertConsistent () : unit =
        if veryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryLong then
            ()
        else
            if foo = bar then
                ()
            else
                let leftSet = HashSet (FooBarBaz.keys leftThings)
                leftSet.SymmetricExceptWith (FooBarBaz.keys rightThings)
                |> ignore
"""
        { config with
            MaxLineLength = 100
            MultilineBlockBracketsOnSameColumn = true
            KeepIfThenInSameLine = true
            MultiLineLambdaClosingNewline = true
            KeepIndentInBranch = true }
    |> prepend newline
    |> should
        equal
        """
module Foo =
    let assertConsistent () : unit =
        if veryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryVeryLong then
            ()
        else if foo = bar then
            ()
        else

        let leftSet = HashSet(FooBarBaz.keys leftThings)

        leftSet.SymmetricExceptWith(FooBarBaz.keys rightThings)
        |> ignore
"""

[<Test>]
let ``comment after match keyword`` () =
    formatSourceString
        false
        """
match  // comment
    Stream.peel rest with
| None -> failwith "oh no"
| Some longName ->

longName
|> Map.map (fun _ -> TypedTerm.force<int>)
"""
        config
    |> prepend newline
    |> should
        equal
        """
match // comment
    Stream.peel rest with
| None -> failwith "oh no"
| Some longName ->

longName
|> Map.map (fun _ -> TypedTerm.force<int>)
"""

[<Test>]
let ``comment after match bang keyword`` () =
    formatSourceString
        false
        """
match!  // comment
    Stream.peel rest with
| None -> failwith "oh no"
| Some longName ->

longName
|> Map.map (fun _ -> TypedTerm.force<int>)
"""
        config
    |> prepend newline
    |> should
        equal
        """
match! // comment
    Stream.peel rest with
| None -> failwith "oh no"
| Some longName ->

longName
|> Map.map (fun _ -> TypedTerm.force<int>)
"""
