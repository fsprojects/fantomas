module Fantomas.Core.Tests.TypeAnnotationTests

open NUnit.Framework
open FsUnit
open Fantomas.Core
open Fantomas.Core.Tests.TestHelpers

[<Test>]
let ``multiline type annotation`` () =
    formatSourceString
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
    Teq<
        int,
        list int,
        System.DateTime array,
        //
        int
     >
"""

[<Test>]
let ``multiline app type`` () =
    formatSourceString
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
                byref<
                    ResumableStateMachine<
                        CancellableTaskResultStateMachineData<'TOverall, 'Error>
                     >
                 >,
            task: CancellationToken -> ^TaskLike,
            continuation:
                ('TResult1
                    -> CancellableTaskResultCode<'TOverall, 'Error, 'TResult2>)
        ) : bool =
        true
"""

[<Test>]
let ``Aligned bracket style in anonymous record is respected, #2706`` () =
    formatSourceString
        false
        """
let private asJson (arm: IArmResource) =
    arm.JsonModel
    |> convertTo<{|
        kind: string
        properties: {| statisticsEnabled: bool |}
    |}>
"""
        { config with
            MultilineBracketStyle = Aligned }
    |> prepend newline
    |> should
        equal
        """
let private asJson (arm: IArmResource) =
    arm.JsonModel
    |> convertTo<
        {|
            kind: string
            properties: {| statisticsEnabled: bool |}
        |}
       >
"""

[<Test>]
let ``Aligned bracket style in anonymous record is respected for multiple types, #2706`` () =
    formatSourceString
        false
        """
let private asJson (arm: IArmResource) =
    arm.JsonModel
    |> convertTo<{|
        kind: string
        properties: {| statisticsEnabled: bool |}
    |},{|
        kind: string
        properties: {| statisticsEnabled: bool |}
    |}
    >
"""
        { config with
            MultilineBracketStyle = Aligned }
    |> prepend newline
    |> should
        equal
        """
let private asJson (arm: IArmResource) =
    arm.JsonModel
    |> convertTo<
        {|
            kind: string
            properties: {| statisticsEnabled: bool |}
        |},
        {|
            kind: string
            properties: {| statisticsEnabled: bool |}
        |}
       >
"""

[<Test>]
let ``Cramped bracket style in anonymous record is respected for multiple types, #2706`` () =
    formatSourceString
        false
        """
let private asJson (arm: IArmResource) =
    arm.JsonModel
    |> convertTo<{|
        kind: string
        properties: {| statisticsEnabled: bool |}
    |},{|
        kind: string
        properties: {| statisticsEnabled: bool |}
    |}
    >
"""
        { config with
            MultilineBracketStyle = Cramped }
    |> prepend newline
    |> should
        equal
        """
let private asJson (arm: IArmResource) =
    arm.JsonModel
    |> convertTo<
        {| kind: string
           properties: {| statisticsEnabled: bool |} |},
        {| kind: string
           properties: {| statisticsEnabled: bool |} |}
       >
"""

[<Test>]
let ``Stroustrup bracket style in anonymous record is respected for multiple types, #2706`` () =
    formatSourceString
        false
        """
let private asJson (arm: IArmResource) =
    arm.JsonModel
    |> convertTo<{|
        kind: string
        properties: {| statisticsEnabled: bool |}
    |},{|
        kind: string
        properties: {| statisticsEnabled: bool |}
    |}
    >
"""
        { config with
            MultilineBracketStyle = Stroustrup }
    |> prepend newline
    |> should
        equal
        """
let private asJson (arm: IArmResource) =
    arm.JsonModel
    |> convertTo<
        {|
            kind: string
            properties: {| statisticsEnabled: bool |}
        |},
        {|
            kind: string
            properties: {| statisticsEnabled: bool |}
        |}
       >
"""

[<Test>]
let ``type application including nested multiline function type`` () =
    formatSourceString
        false
        """
let bv = unbox<Foo<'innerContextLongLongLong, 'bb -> 'b>> bf
        """
        { config with
            MaxLineLength = 30
            SpaceBeforeUppercaseInvocation = true
            SpaceBeforeClassConstructor = true
            SpaceBeforeMember = true
            SpaceBeforeColon = true
            SpaceBeforeSemicolon = true
            MultilineBracketStyle = Aligned
            AlignFunctionSignatureToIndentation = true
            AlternativeLongMemberDefinitions = true
            MultiLineLambdaClosingNewline = true
            NewlineBetweenTypeDefinitionAndMembers = false }
    |> prepend newline
    |> should
        equal
        """
let bv =
    unbox<
        Foo<
            'innerContextLongLongLong,
            'bb -> 'b
         >
     >
        bf
"""


[<Test>]
let ``test for AppLongIdentAndSingleParenArg`` () =
    formatSourceString false 
        """
let private asJson (arm: IArmResource) =
    arm.JsonModel
    |> convertTo<{|
        kind: string
        properties: {| statisticsEnabled: bool |}
    |}>
"""     
        config
    |> prepend newline
    |> should 
        equal 
        """
        """
        
[<Test>]
let ``test for AppSingleParenArg`` () =
    formatSourceString false 
        """
"""     
        config
    |> prepend newline
    |> should 
        equal 
        """
        """
        
[<Test>]
let ``test for AppWithLambda`` () =
    formatSourceString false 
        """
"""     
        config
    |> prepend newline
    |> should 
        equal 
        """
        """
        
[<Test>]
let ``test for NestedIndexWithoutDot`` () =
    formatSourceString false 
        """
"""     
        config
    |> prepend newline
    |> should 
        equal 
        """
        """
        
[<Test>]
let ``test for EndsWithDualListApp`` () =
    formatSourceString false 
        """
"""     
        config
    |> prepend newline
    |> should 
        equal 
        """
        """
        
[<Test>]
let ``test for EndsWithSingleListApp`` () =
    formatSourceString false 
        """
"""     
        config
    |> prepend newline
    |> should 
        equal 
        """
        """
        