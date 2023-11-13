module Fantomas.Core.Tests.Stroustrup.SynBindingFunctionLongPatternExpressionTests

open NUnit.Framework
open FsUnit
open Fantomas.Core
open Fantomas.Core.Tests.TestHelpers

let config =
    { config with
        MaxLineLength = 80
        MultilineBracketStyle = Stroustrup
        MaxArrayOrListWidth = 40 }

// TODO: conclude on what should happen here
// This one feels very weird to have `= {` because the pattern is already multiline
[<Test>]
let ``synbinding function with record`` () =
    formatSourceString
        """
let private addTaskToScheduler
    (scheduler: IScheduler)
    taskName
    taskCron
    prio
    (task: unit -> unit)
    groupName
    =
    { A = longTypeName
      B = someOtherVariable
      C = ziggyBarX }
"""
        config
    |> prepend newline
    |> should
        equal
        """
let private addTaskToScheduler
    (scheduler: IScheduler)
    taskName
    taskCron
    prio
    (task: unit -> unit)
    groupName
    =
    {
        A = longTypeName
        B = someOtherVariable
        C = ziggyBarX
    }
"""

[<Test>]
let ``synbinding function with update record`` () =
    formatSourceString
        """
let private addTaskToScheduler
    (scheduler: IScheduler)
    taskName
    taskCron
    prio
    (task: unit -> unit)
    groupName
    =
    { astContext with IsInsideMatchClausePattern = true }
"""
        config
    |> prepend newline
    |> should
        equal
        """
let private addTaskToScheduler
    (scheduler: IScheduler)
    taskName
    taskCron
    prio
    (task: unit -> unit)
    groupName
    =
    {
        astContext with
            IsInsideMatchClausePattern = true
    }
"""

[<Test>]
let ``synbinding function with anonymous record`` () =
    formatSourceString
        """
let private addTaskToScheduler
    (scheduler: IScheduler)
    taskName
    taskCron
    prio
    (task: unit -> unit)
    groupName
    =
    {| A = longTypeName
       B = someOtherVariable
       C = ziggyBarX |}
"""
        config
    |> prepend newline
    |> should
        equal
        """
let private addTaskToScheduler
    (scheduler: IScheduler)
    taskName
    taskCron
    prio
    (task: unit -> unit)
    groupName
    =
    {|
        A = longTypeName
        B = someOtherVariable
        C = ziggyBarX
    |}
"""

[<Test>]
let ``synbinding function with computation expression`` () =
    formatSourceString
        """
let private addTaskToScheduler
    (scheduler: IScheduler)
    taskName
    taskCron
    prio
    (task: unit -> unit)
    groupName
    =
    task {
        // some computation here
        ()
    }
"""
        config
    |> prepend newline
    |> should
        equal
        """
let private addTaskToScheduler
    (scheduler: IScheduler)
    taskName
    taskCron
    prio
    (task: unit -> unit)
    groupName
    =
    task {
        // some computation here
        ()
    }
"""

[<Test>]
let ``synbinding function with list`` () =
    formatSourceString
        """
let private addTaskToScheduler
    (scheduler: IScheduler)
    taskName
    taskCron
    prio
    (task: unit -> unit)
    groupName
    =
    [ itemOne
      itemTwo
      itemThree
      itemFour
      itemFive ]
"""
        config
    |> prepend newline
    |> should
        equal
        """
let private addTaskToScheduler
    (scheduler: IScheduler)
    taskName
    taskCron
    prio
    (task: unit -> unit)
    groupName
    =
    [
        itemOne
        itemTwo
        itemThree
        itemFour
        itemFive
    ]
"""

[<Test>]
let ``synbinding function with array`` () =
    formatSourceString
        """
let private addTaskToScheduler
    (scheduler: IScheduler)
    taskName
    taskCron
    prio
    (task: unit -> unit)
    groupName
    =
    [| itemOne
       itemTwo
       itemThree
       itemFour
       itemFive |]
"""
        config
    |> prepend newline
    |> should
        equal
        """
let private addTaskToScheduler
    (scheduler: IScheduler)
    taskName
    taskCron
    prio
    (task: unit -> unit)
    groupName
    =
    [|
        itemOne
        itemTwo
        itemThree
        itemFour
        itemFive
    |]
"""

[<Test>]
let ``type member function with record instance`` () =
    formatSourceString
        """
type Foo() =
    member this.addTaskToScheduler
        (scheduler: IScheduler)
        taskName
        taskCron
        prio
        (task: unit -> unit)
        groupName
        =
        { A = longTypeName
          B = someOtherVariable
          C = ziggyBarX }
"""
        config
    |> prepend newline
    |> should
        equal
        """
type Foo() =
    member this.addTaskToScheduler
        (scheduler: IScheduler)
        taskName
        taskCron
        prio
        (task: unit -> unit)
        groupName
        =
        {
            A = longTypeName
            B = someOtherVariable
            C = ziggyBarX
        }
"""

[<Test>]
let ``type member function with update record`` () =
    formatSourceString
        """
type Foo() =
    member this.addTaskToScheduler
        (scheduler: IScheduler)
        taskName
        taskCron
        prio
        (task: unit -> unit)
        groupName
        = { astContext with IsInsideMatchClausePattern = true }
"""
        config
    |> prepend newline
    |> should
        equal
        """
type Foo() =
    member this.addTaskToScheduler
        (scheduler: IScheduler)
        taskName
        taskCron
        prio
        (task: unit -> unit)
        groupName
        =
        {
            astContext with
                IsInsideMatchClausePattern = true
        }
"""

[<Test>]
let ``type member function with anonymous record instance`` () =
    formatSourceString
        """
type Foo() =
    member this.addTaskToScheduler
        (scheduler: IScheduler)
        taskName
        taskCron
        prio
        (task: unit -> unit)
        groupName
        =
        {| A = longTypeName
           B = someOtherVariable
           C = ziggyBarX |}
"""
        config
    |> prepend newline
    |> should
        equal
        """
type Foo() =
    member this.addTaskToScheduler
        (scheduler: IScheduler)
        taskName
        taskCron
        prio
        (task: unit -> unit)
        groupName
        =
        {|
            A = longTypeName
            B = someOtherVariable
            C = ziggyBarX
        |}
"""

[<Test>]
let ``type member function with anonymous record instance struct`` () =
    formatSourceString
        """
type Foo() =
    member this.addTaskToScheduler
        (scheduler: IScheduler)
        taskName
        taskCron
        prio
        (task: unit -> unit)
        groupName
        =
           struct
                {| A = longTypeName
                   B = someOtherVariable
                   C = ziggyBarX |}
"""
        config
    |> prepend newline
    |> should
        equal
        """
type Foo() =
    member this.addTaskToScheduler
        (scheduler: IScheduler)
        taskName
        taskCron
        prio
        (task: unit -> unit)
        groupName
        =
        struct {|
            A = longTypeName
            B = someOtherVariable
            C = ziggyBarX
        |}
"""

[<Test>]
let ``type member function with computation expression`` () =
    formatSourceString
        """
type Foo() =
    member this.addTaskToScheduler
        (scheduler: IScheduler)
        taskName
        taskCron
        prio
        (task: unit -> unit)
        groupName
        =
        task {
            // some computation here
            ()
        }
"""
        config
    |> prepend newline
    |> should
        equal
        """
type Foo() =
    member this.addTaskToScheduler
        (scheduler: IScheduler)
        taskName
        taskCron
        prio
        (task: unit -> unit)
        groupName
        =
        task {
            // some computation here
            ()
        }
"""

[<Test>]
let ``type member function with list`` () =
    formatSourceString
        """
type Foo() =
    member this.addTaskToScheduler
        (scheduler: IScheduler)
        taskName
        taskCron
        prio
        (task: unit -> unit)
        groupName
        =
        [ itemOne
          itemTwo
          itemThree
          itemFour
          itemFive ]
"""
        config
    |> prepend newline
    |> should
        equal
        """
type Foo() =
    member this.addTaskToScheduler
        (scheduler: IScheduler)
        taskName
        taskCron
        prio
        (task: unit -> unit)
        groupName
        =
        [
            itemOne
            itemTwo
            itemThree
            itemFour
            itemFive
        ]
"""

[<Test>]
let ``type member function with array`` () =
    formatSourceString
        """
type Foo() =
    member this.addTaskToScheduler
        (scheduler: IScheduler)
        taskName
        taskCron
        prio
        (task: unit -> unit)
        groupName
        =
        [| itemOne
           itemTwo
           itemThree
           itemFour
           itemFive |]
"""
        config
    |> prepend newline
    |> should
        equal
        """
type Foo() =
    member this.addTaskToScheduler
        (scheduler: IScheduler)
        taskName
        taskCron
        prio
        (task: unit -> unit)
        groupName
        =
        [|
            itemOne
            itemTwo
            itemThree
            itemFour
            itemFive
        |]
"""
