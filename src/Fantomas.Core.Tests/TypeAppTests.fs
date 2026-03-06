module Fantomas.Core.Tests.TypeAppTests

open NUnit.Framework
open FsUnit
open Fantomas.Core
open Fantomas.Core.Tests.TestHelpers

[<Test>]
let ``type app closing angle bracket not padded by previous expression length, 3179`` () =
    formatSourceString
        """
let x () =
    (someFunctionCall ()) |> anotherOne |> anotherOne |> alsoAnotherOne

    Unchecked.defaultof<_>
"""
        config
    |> prepend newline
    |> should
        equal
        """
let x () =
    (someFunctionCall ()) |> anotherOne |> anotherOne |> alsoAnotherOne

    Unchecked.defaultof<_>
"""

[<Test>]
let ``aligned bracket style in anonymous record is respected, 2706`` () =
    formatSourceString
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
let ``aligned bracket style in anonymous record is respected for multiple types, 2706`` () =
    formatSourceString
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
let ``cramped bracket style in anonymous record is respected for multiple types, 2706`` () =
    formatSourceString
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
let ``stroustrup bracket style in anonymous record is respected for multiple types, 2706`` () =
    formatSourceString
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

let alignedMaxLine30 =
    { config with
        MaxLineLength = 30
        MultilineBracketStyle = Aligned }

[<Test>]
let ``type application including nested multiline function type`` () =
    formatSourceString
        """
let bv = unbox<Foo<'innerContextLongLongLong, 'bb -> 'b>> bf
"""
        alignedMaxLine30
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
let ``multiline type argument with AppLongIdentAndSingleParenArg`` () =
    formatSourceString
        """
path.Replace<
        Foo<
            'innerContextLongLongLong,
            'bb -> 'b
         >
     >("../../../", "....")
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
path.Replace<
    Foo<
        'innerContextLongLongLong,
        'bb -> 'b
     >
 > (
    "../../../",
    "...."
)
"""

[<Test>]
let ``multiline type argument with AppLongIdentAndSingleParenArg 2`` () =
    formatSourceString
        """
path.Replace<
        Foo<
            'innerContextLongLongLong,
            'bb -> 'b
         >
     >("../../../", "....")
"""
        { alignedMaxLine30 with
            SpaceBeforeClassConstructor = true }
    |> prepend newline
    |> should
        equal
        """
path.Replace<
    Foo<
        'innerContextLongLongLong,
        'bb -> 'b
     >
 >(
    "../../../",
    "...."
)
"""

[<Test>]
let ``multiline type argument with AppLongIdentAndSingleParenArg 3`` () =
    formatSourceString
        """
path.Replace<
        Foo<
            'innerContextLongLongLong,
            'bb -> 'b
         >
     >("../../../", "....")
"""
        alignedMaxLine30
    |> prepend newline
    |> should
        equal
        """
path.Replace<
    Foo<
        'innerContextLongLongLong,
        'bb -> 'b
     >
 >(
    "../../../",
    "...."
)
"""

[<Test>]
let ``multiline type argument with AppSingleParenArg`` () =
    formatSourceString
        """
someFunc<
        Foo<
            'innerContextLongLongLong,
            'bb -> 'b
         >
     >(a,b)
"""
        alignedMaxLine30
    |> prepend newline
    |> should
        equal
        """
someFunc<
    Foo<
        'innerContextLongLongLong,
        'bb -> 'b
     >
 > (
    a,
    b
)
"""

[<Test>]
let ``multiline type argument with AppWithLambda`` () =
    formatSourceString
        """
someFunc<
    Bar<
        'innerContextLongLongLong,
        'bb -> 'b
     >
 > (fun x -> x)
"""
        alignedMaxLine30
    |> prepend newline
    |> should
        equal
        """
someFunc<
    Bar<
        'innerContextLongLongLong,
        'bb -> 'b
     >
 >
    (fun x -> x)
"""

[<Test>]
let ``multiline type argument with NestedIndexWithoutDot`` () =
    formatSourceString
        """
something<
        Foo<
            'innerContextLongLongLong,
            'bb -> 'b
         >
     >["thing"][8](a,b)
"""
        alignedMaxLine30
    |> prepend newline
    |> should
        equal
        """
something<
    Foo<
        'innerContextLongLongLong,
        'bb -> 'b
     >
 >["thing"][8](a, b)
"""

[<Test>]
let ``multiline type argument with EndsWithDualListApp`` () =
    formatSourceString
        """
div<
    Bar<
        'innerContextLongLongLong,
        'bb -> 'b
     >
 > [ ClassName "container" ] [ str "meh" ]
"""
        alignedMaxLine30
    |> prepend newline
    |> should
        equal
        """
div<
    Bar<
        'innerContextLongLongLong,
        'bb -> 'b
     >
 >
    [ ClassName "container" ]
    [ str "meh" ]
"""

[<Test>]
let ``multiline type argument with elmish EndsWithDualListApp`` () =
    formatSourceString
        """
div<
    Bar<
        'innerContextLongLongLong,
        'bb -> 'b
     >
 > [ ClassName "container" ] [ str "meh" ]
"""
        { alignedMaxLine30 with
            ExperimentalElmish = true }
    |> prepend newline
    |> should
        equal
        """
div<
    Bar<
        'innerContextLongLongLong,
        'bb -> 'b
     >
 >
    [ ClassName "container" ] [
        str "meh"
    ]
"""

[<Test>]
let ``multiline type argument with EndsWithSingleListApp`` () =
    formatSourceString
        """
input<
    Bar<
        'innerContextLongLongLong,
        'bb -> 'b
     >
 > [ Type "text" ]
"""
        alignedMaxLine30
    |> prepend newline
    |> should
        equal
        """
input<
    Bar<
        'innerContextLongLongLong,
        'bb -> 'b
     >
 >
    [ Type "text" ]
"""

[<Test>]
let ``multiline type argument with index without dot`` () =
    formatSourceString
        """
XYZ.app<int -> int -> int -> string>[tellMeWhy { return wouldSomeoneWriteThisCode }]
"""
        alignedMaxLine30
    |> prepend newline
    |> should
        equal
        """
XYZ.app<
    int
        -> int
        -> int
        -> string
 >[tellMeWhy {
       return
           wouldSomeoneWriteThisCode
   }]
"""

[<Test>]
let ``multiline type argument with Index with dot`` () =
    formatSourceString
        """
XYZ.app<int -> int -> int -> string>.[tellMeWhy { return wouldSomeoneWriteThisCode }]
"""
        alignedMaxLine30
    |> prepend newline
    |> should
        equal
        """
XYZ.app<
    int
        -> int
        -> int
        -> string
 >.[tellMeWhy {
    return
        wouldSomeoneWriteThisCode
}]
"""

[<Test>]
let ``multiple nested generic types`` () =
    formatSourceString
        """
let bv =
    unbox<
        Fooadfadadfdadfadfadfadfadfadfsfdsfadfadadfada<
            Foo<
                innerContextLongLongLong,
                bb
             >
         >
     >
        bf
"""
        { config with MaxLineLength = 10 }
    |> prepend newline
    |> should
        equal
        """
let bv =
    unbox<
        Fooadfadadfdadfadfadfadfadfadfsfdsfadfadadfada<
            Foo<
                innerContextLongLongLong,
                bb
             >
         >
     >
        bf
"""

[<Test>]
let ``nested multiline type app should not produce invalid code, 3243`` () =
    formatSourceString
        """
let f (x: Outer<Map<string, (A * B * C * D)>, Z>) = ()
"""
        { config with MaxLineLength = 30 }
    |> prepend newline
    |> should
        equal
        """
let f
    (x:
        Outer<
            Map<
                string,
                (A * B * C * D)
             >,
            Z
         >)
    =
    ()
"""

[<Test>]
let ``nested multiline type app in function signature should not produce invalid code, 3243`` () =
    formatSourceString
        """
let makePipeline''
    (mode: ExecutionMode<Marker>)
    (resolver: IResolver option Marker)
    (args: Arguments Marker)
    (logging: Marker<ExtraLogging>)
    (processing:
        (PipelineStep<
            Map<string,
                ((Table * IStepMetadata)
                 * Operation seq
                 * FileInfo ResultKind seq
                 * (NodeInfo<string, string> * DayLogs) seq)>,
            PipelineCrate>)
            ->
        (Map<string,
            ((Table * IStepMetadata)
             * Operation seq
             * FileInfo ResultKind seq
             * (NodeInfo<string, string> * DayLogs) seq)> Marker)
            ->
        PipelineStep<unit>)
    =
    ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
let makePipeline''
    (mode: ExecutionMode<Marker>)
    (resolver: IResolver option Marker)
    (args: Arguments Marker)
    (logging: Marker<ExtraLogging>)
    (processing:
        (PipelineStep<
            Map<
                string,
                ((Table * IStepMetadata) *
                Operation seq *
                FileInfo ResultKind seq *
                (NodeInfo<string, string> * DayLogs) seq)
             >,
            PipelineCrate
          >)
            -> (Map<
                string,
                ((Table * IStepMetadata) *
                Operation seq *
                FileInfo ResultKind seq *
                (NodeInfo<string, string> * DayLogs) seq)
                 > Marker)
            -> PipelineStep<unit>)
    =
    ()
"""
