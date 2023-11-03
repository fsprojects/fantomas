module Fantomas.Core.Tests.TypeAnnotationTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelpers

[<Test>]
let ``multiline type annotation`` () =
    formatSourceString
        false
        """
let f
    (x:
        {|
            x: int
            y: AReallyLongTypeThatIsMuchLongerThan40Characters
        |})
    =
    x
"""
        config
    |> prepend newline
    |> should
        equal
        """
let f
    (x:
        {| x: int
           y: AReallyLongTypeThatIsMuchLongerThan40Characters |})
    =
    x
"""

[<Test>]
let ``multiline tuple type`` () =
    formatSourceString
        false
        """
type Meh
    (
        input: LongTupleItemTypeOneThing * LongTupleItemTypeThingTwo * LongTupleItemTypeThree * LongThingFour * LongThingFiveYow
    ) =
    class
    end
"""
        config
    |> prepend newline
    |> should
        equal
        """
type Meh
    (
        input:
            LongTupleItemTypeOneThing *
            LongTupleItemTypeThingTwo *
            LongTupleItemTypeThree *
            LongThingFour *
            LongThingFiveYow
    ) = class end
"""

[<Test>]
let ``long multiline type application`` () =
    formatSourceString
        false
        """
type X =
    Teq<int, list int, System.DateTime array,
            //
            int>
"""
        config
    |> prepend newline
    |> should
        equal
        """
type X =
    Teq<int, list int, System.DateTime array,
    //
    int>
"""

[<Test>]
let ``multiline app type`` () =
    formatSourceString
        false
        """
type CancellableTaskResultBuilderBase with

    [<NoEagerConstraintApplication>]
    static member inline BindDynamic< ^TaskLike, 'TResult1, 'TResult2, ^Awaiter, 'TOverall, 'Error
        when ^TaskLike: (member GetAwaiter: unit -> ^Awaiter)
        and ^Awaiter :> ICriticalNotifyCompletion
        and ^Awaiter: (member get_IsCompleted: unit -> bool)
        and ^Awaiter: (member GetResult: unit -> Result<'TResult1, 'Error>)>
        (
            sm:
                byref<
                    ResumableStateMachine<
                        CancellableTaskResultStateMachineData<'TOverall, 'Error>
                >
                >,
            task: CancellationToken -> ^TaskLike,
            continuation:
                ('TResult1 -> CancellableTaskResultCode<'TOverall, 'Error, 'TResult2>)
        ) : bool = true
"""
        { config with MaxLineLength = 80 }
    |> prepend newline
    |> should
        equal
        """
type CancellableTaskResultBuilderBase with

    [<NoEagerConstraintApplication>]
    static member inline BindDynamic< ^TaskLike, 'TResult1, 'TResult2, ^Awaiter, 'TOverall, 'Error
        when ^TaskLike: (member GetAwaiter: unit -> ^Awaiter)
        and ^Awaiter :> ICriticalNotifyCompletion
        and ^Awaiter: (member get_IsCompleted: unit -> bool)
        and ^Awaiter: (member GetResult: unit -> Result<'TResult1, 'Error>)>
        (
            sm:
                byref<ResumableStateMachine<CancellableTaskResultStateMachineData<'TOverall, 'Error>>>,
            task: CancellationToken -> ^TaskLike,
            continuation:
                ('TResult1
                    -> CancellableTaskResultCode<'TOverall, 'Error, 'TResult2>)
        ) : bool =
        true
"""
