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
let ``sequential in first clause, let or use in last clause, match lambda`` () =
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

    let exitCode =
        function
        | RunMode.Dry ->
            printfn "Would execute actions, but --dry-run was supplied: %+A" instructions
            0
        | RunMode.Wet ->
            // proceed with main method
            let output = Library.execute instructions
            // do more stuff
            0

    exitCode args
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

    let exitCode =
        function
        | RunMode.Dry ->
            printfn "Would execute actions, but --dry-run was supplied: %+A" instructions
            0
        | RunMode.Wet ->
        // proceed with main method
        let output = Library.execute instructions
        // do more stuff
        0

    exitCode args
"""
