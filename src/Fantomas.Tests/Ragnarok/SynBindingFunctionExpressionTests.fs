module Fantomas.Tests.Ragnarok.SynBindingFunctionExpressionTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper

let config =
    { config with
          MultilineBlockBracketsOnSameColumn = true
          Ragnarok = true }

[<Test>]
let ``synbinding function with record instance `` () =
    formatSourceString
        false
        """
let x y =
    { A = longTypeName
      B = someOtherVariable
      C = ziggyBarX }
"""
        config
    |> prepend newline
    |> should
        equal
        """
let x y = {
    A = longTypeName
    B = someOtherVariable
    C = ziggyBarX
}
"""

// TODO: conclude on what should happen here
// This one feels very weird to have `= {` because the pattern is already multiline

[<Test>]
let ``multiline pattern in synbinding, record as expression`` () =
    formatSourceString
        false
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
        { config with MaxLineLength = 80 }
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