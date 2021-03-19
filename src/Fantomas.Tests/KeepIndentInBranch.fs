module Fantomas.Tests.KeepIndentInBranch

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
    formatSourceString false """
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
"""  config
    |> prepend newline
    |> should equal """
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