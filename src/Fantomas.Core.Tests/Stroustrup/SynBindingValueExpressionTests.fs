module Fantomas.Core.Tests.Stroustrup.SynBindingValueExpressionTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelpers
open Fantomas.Core

let config =
    { config with
        MultilineBracketStyle = Stroustrup
        MaxArrayOrListWidth = 40
    }

[<Test>]
let ``synbinding value with record instance `` () =
    formatSourceString
        """
let x =
    { A = longTypeName
      B = someOtherVariable
      C = ziggyBarX }
"""
        config
    |> prepend newline
    |> should
        equal
        """
let x = {
    A = longTypeName
    B = someOtherVariable
    C = ziggyBarX
}
"""

[<Test>]
let ``synbinding value with update record`` () =
    formatSourceString
        """
let astCtx =
    { astContext with IsInsideMatchClausePattern = true; OtherThing = "YOLO" }
"""
        { config with
            RecordMultilineFormatter = NumberOfItems
        }
    |> prepend newline
    |> should
        equal
        """
let astCtx = {
    astContext with
        IsInsideMatchClausePattern = true
        OtherThing = "YOLO"
}
"""

[<Test>]
let ``synbinding value with update anonymous record`` () =
    formatSourceString
        """
let astCtx =
    {| astContext with IsInsideMatchClausePattern = true; OtherThing = "YOLO" |}
"""
        { config with
            RecordMultilineFormatter = NumberOfItems
        }
    |> prepend newline
    |> should
        equal
        """
let astCtx = {|
    astContext with
        IsInsideMatchClausePattern = true
        OtherThing = "YOLO"
|}
"""

[<Test>]
let ``synbinding value with anonymous record instance`` () =
    formatSourceString
        """
let x =
   {| A = longTypeName
      B = someOtherVariable
      C = ziggyBarX |}
"""
        config
    |> prepend newline
    |> should
        equal
        """
let x = {|
    A = longTypeName
    B = someOtherVariable
    C = ziggyBarX
|}
"""

[<Test>]
let ``synbinding value with anonymous record instance struct`` () =
    formatSourceString
        """
let x =
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
let x = struct {|
    A = longTypeName
    B = someOtherVariable
    C = ziggyBarX
|}
"""

[<Test>]
let ``synbinding value with list`` () =
    formatSourceString
        """
let t =
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
let t = [
    itemOne
    itemTwo
    itemThree
    itemFour
    itemFive
]
"""

[<Test>]
let ``synbinding value with array`` () =
    formatSourceString
        """
let t =
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
let t = [|
    itemOne
    itemTwo
    itemThree
    itemFour
    itemFive
|]
"""

[<Test>]
let ``synbinding value with object expression`` () =
    formatSourceString
        """
let x =
    { new IFoo with
        member _.Bar() = longTypeName
        member _.Baz() = someOtherVariable }
"""
        config
    |> prepend newline
    |> should
        equal
        """
let x = {
    new IFoo with
        member _.Bar() = longTypeName
        member _.Baz() = someOtherVariable
}
"""

[<Test>]
let ``nested synbinding value with record`` () =
    formatSourceString
        """
let outer =
    let inner =
        {
            X = someGreatXValue
            Y = someRatherSmallYValue
        }
    ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
let outer =
    let inner = {
        X = someGreatXValue
        Y = someRatherSmallYValue
    }

    ()
"""

[<Test>]
let ``type member value with record instance`` () =
    formatSourceString
        """
type Foo() =
    member this.Bar =
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
    member this.Bar = {
        A = longTypeName
        B = someOtherVariable
        C = ziggyBarX
    }
"""

[<Test>]
let ``type member value with update record`` () =
    formatSourceString
        """
type Foo() =
    member this.Bar = { astContext with IsInsideMatchClausePattern = true }
"""
        config
    |> prepend newline
    |> should
        equal
        """
type Foo() =
    member this.Bar = {
        astContext with
            IsInsideMatchClausePattern = true
    }
"""

[<Test>]
let ``type member value with anonymous record instance`` () =
    formatSourceString
        """
type Foo() =
    member this.Bar =
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
    member this.Bar = {|
        A = longTypeName
        B = someOtherVariable
        C = ziggyBarX
    |}
"""

[<Test>]
let ``type member value with anonymous record instance struct`` () =
    formatSourceString
        """
type Foo() =
    member this.Bar =
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
    member this.Bar = struct {|
        A = longTypeName
        B = someOtherVariable
        C = ziggyBarX
    |}
"""

[<Test>]
let ``type member value with list`` () =
    formatSourceString
        """
type Foo() =
    member this.Bar =
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
    member this.Bar = [
        itemOne
        itemTwo
        itemThree
        itemFour
        itemFive
    ]
"""

[<Test>]
let ``type member value with array`` () =
    formatSourceString
        """
type Foo() =
    member this.Bar =
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
    member this.Bar = [|
        itemOne
        itemTwo
        itemThree
        itemFour
        itemFive
    |]
"""

[<Test>]
let ``type member value with object expression`` () =
    formatSourceString
        """
type Foo() =
    member this.Bar =
        { new IFoo with
            member _.Bar() = longTypeName
            member _.Baz() = someOtherVariable }
"""
        config
    |> prepend newline
    |> should
        equal
        """
type Foo() =
    member this.Bar = {
        new IFoo with
            member _.Bar() = longTypeName
            member _.Baz() = someOtherVariable
    }
"""

[<Test>]
let ``let binding for anonymous record with copy expression, 2508`` () =
    formatSourceString
        """
let fooDto =
    {| otherDto with
        TextFilters =
            criteria.Meta.TextFilter
            |> Option.map (fun f -> f.Filters)
            |> Option.map (List.map (sprintf "~%s~"))
            |> Option.toObj
    |}
"""
        config
    |> prepend newline
    |> should
        equal
        """
let fooDto = {|
    otherDto with
        TextFilters =
            criteria.Meta.TextFilter
            |> Option.map (fun f -> f.Filters)
            |> Option.map (List.map (sprintf "~%s~"))
            |> Option.toObj
|}
"""

[<Test>]
let ``let binding with nested anonymous records, 2413`` () =
    formatSourceString
        """
let foo =
    {| Data =
        {| Name = "Isaac"
           Age = 43
           Day = "Monday"
           Colour = "Blue" |} |}
"""
        config
    |> prepend newline
    |> should
        equal
        """
let foo = {|
    Data = {|
        Name = "Isaac"
        Age = 43
        Day = "Monday"
        Colour = "Blue"
    |}
|}
"""

[<Test>]
let ``list expression inside anonymous record, 2413`` () =
    formatSourceString
        """
let foo = {|
    Data =
        {|
            Name = "Isaac"
            Age = 43
            Day = "Monday"
            Colours =
                [
                    "Red"
                    "Blue"
                    "White"
                    "Orange"
                    "Red"
                    "Blue"
                    "White"
                    "Orange"
                    "Red"
                    "Blue"
                    "White"
                    "Orange"
                    "Red"
                    "Blue"
                    "White"
                    "Orange"
                ]
        |}
|}
"""
        config
    |> prepend newline
    |> should
        equal
        """
let foo = {|
    Data = {|
        Name = "Isaac"
        Age = 43
        Day = "Monday"
        Colours = [
            "Red"
            "Blue"
            "White"
            "Orange"
            "Red"
            "Blue"
            "White"
            "Orange"
            "Red"
            "Blue"
            "White"
            "Orange"
            "Red"
            "Blue"
            "White"
            "Orange"
        ]
    |}
|}
"""

[<Test>]
let ``list expression inside regular record, 2413`` () =
    formatSourceString
        """
let foo = {
    Data =
        {
            Name = "Isaac"
            Age = 43
            Day = "Monday"
            Colours =
                [
                    "Red"
                    "Blue"
                    "White"
                    "Orange"
                    "Red"
                    "Blue"
                    "White"
                    "Orange"
                    "Red"
                    "Blue"
                    "White"
                    "Orange"
                    "Red"
                    "Blue"
                    "White"
                    "Orange"
                ]
        }
}
"""
        config
    |> prepend newline
    |> should
        equal
        """
let foo = {
    Data = {
        Name = "Isaac"
        Age = 43
        Day = "Monday"
        Colours = [
            "Red"
            "Blue"
            "White"
            "Orange"
            "Red"
            "Blue"
            "White"
            "Orange"
            "Red"
            "Blue"
            "White"
            "Orange"
            "Red"
            "Blue"
            "White"
            "Orange"
        ]
    }
}
"""

[<Test>]
let ``nested records, 2587`` () =
    formatSourceString
        """
let myRecord = {
    Property1 = {
        Value1 = 20
        Value2 = 30
        Value3 = 40
    }
    Property2 = {
        Value1 = 20
        Value2 = 30
        Value3 = 40
    }
}
"""
        config
    |> prepend newline
    |> should
        equal
        """
let myRecord = {
    Property1 = {
        Value1 = 20
        Value2 = 30
        Value3 = 40
    }
    Property2 = {
        Value1 = 20
        Value2 = 30
        Value3 = 40
    }
}
"""

[<Test>]
let ``app node with single record member`` () =
    formatSourceString
        """
let newState = {
    Foo =
        Some
            {
                F1 = 0
                F2 = ""
            }
}
"""
        { config with
            RecordMultilineFormatter = NumberOfItems
        }
    |> prepend newline
    |> should
        equal
        """
let newState = {
    Foo =
        Some {
            F1 = 0
            F2 = ""
        }
}
"""

[<Test>]
let ``app node with single anonymous record member`` () =
    formatSourceString
        """
let newState = {|
    Foo =
        Some
            {|
                F1 = 0
                F2 = ""
            |}
|}
"""
        { config with
            RecordMultilineFormatter = NumberOfItems
        }
    |> prepend newline
    |> should
        equal
        """
let newState = {|
    Foo =
        Some {|
            F1 = 0
            F2 = ""
        |}
|}
"""

[<Test>]
let ``app node with single record arg`` () =
    formatSourceString
        """
let newState = 
    Some
        {
            F1 = 0
            F2 = ""
        }
"""
        { config with
            RecordMultilineFormatter = NumberOfItems
        }
    |> prepend newline
    |> should
        equal
        """
let newState =
    Some {
        F1 = 0
        F2 = ""
    }
"""

[<Test>]
let ``lowercase app node with single record arg`` () =
    formatSourceString
        """
let newState = 
    someFunc
        {
            F1 = 0
            F2 = ""
        }
"""
        { config with
            RecordMultilineFormatter = NumberOfItems
        }
    |> prepend newline
    |> should
        equal
        """
let newState =
    someFunc {
        F1 = 0
        F2 = ""
    }
"""

[<Test>]
let ``lowercase app node with multiple args ending in a single record arg`` () =
    formatSourceString
        """
let newState = 
    myFn a b c { D = d; E = e }
"""
        { config with
            RecordMultilineFormatter = NumberOfItems
        }
    |> prepend newline
    |> should
        equal
        """
let newState =
    myFn a b c {
        D = d
        E = e
    }
"""

[<Test>]
let ``lowercase app node with multiple args ending in a single anonymous record arg`` () =
    formatSourceString
        """
let newState = 
    myFn a b c {| D = d; E = e |}
"""
        { config with
            RecordMultilineFormatter = NumberOfItems
        }
    |> prepend newline
    |> should
        equal
        """
let newState =
    myFn a b c {|
        D = d
        E = e
    |}
"""

[<Test>]
let ``don't apply stroustrup when the token has trivia after it`` () =
    formatSourceString
        """
let b = // Build an inbound for the specified subnet.
    {
        Name = subnet
        Location = location
        DnsResolverId = Managed(dnsResolvers.resourceId this.Name)
        SubnetId =
            Unmanaged
                { vnetId.ResourceId with
                    Type = Arm.Network.subnets
                    Segments = [ subnet ]
                }
        PrivateIpAllocations = [ DynamicPrivateIp ]
        Dependencies = Set.empty
        Tags = Map.empty
    }
"""
        config
    |> prepend newline
    |> should
        equal
        """
let b = // Build an inbound for the specified subnet.
    {
        Name = subnet
        Location = location
        DnsResolverId = Managed(dnsResolvers.resourceId this.Name)
        SubnetId =
            Unmanaged {
                vnetId.ResourceId with
                    Type = Arm.Network.subnets
                    Segments = [ subnet ]
            }
        PrivateIpAllocations = [ DynamicPrivateIp ]
        Dependencies = Set.empty
        Tags = Map.empty
    }
"""

[<Test>]
let ``hash directive before closing list bracket, 3070`` () =
    formatSourceString
        """
let private knownProviders = [
#if !FABLE_COMPILER
    (SerilogProvider.isAvailable, SerilogProvider.create)
    (MicrosoftExtensionsLoggingProvider.isAvailable, MicrosoftExtensionsLoggingProvider.create)
#endif
                                        ]
"""
        config
    |> prepend newline
    |> should
        equal
        """
let private knownProviders =
    [
#if !FABLE_COMPILER
        (SerilogProvider.isAvailable, SerilogProvider.create)
        (MicrosoftExtensionsLoggingProvider.isAvailable, MicrosoftExtensionsLoggingProvider.create)
#endif
    ]
"""

[<Test>]
let ``empty line before closing list bracket, 3079`` () =
    formatSourceString
        """
let list = [
    someItem

]
"""
        config
    |> prepend newline
    |> should
        equal
        """
let list = [
    someItem

]
"""

[<Test>]
let ``comment before closing list bracket, 3079`` () =
    formatSourceString
        """
let list = [
    someItem
    // comment
]
"""
        config
    |> prepend newline
    |> should
        equal
        """
let list = [
    someItem
    // comment
]
"""

[<Test>]
let ``single line block comment before closing list bracket`` () =
    formatSourceString
        """
let list = [
    someItem
    (* comment *)
]
"""
        config
    |> prepend newline
    |> should
        equal
        """
let list = [
    someItem
    (* comment *)
]
"""

[<Test>]
let ``multiline block comment before closing list bracket`` () =
    formatSourceString
        """
let list = [
    someItem
    (*
        comment
    *)
]
"""
        config
    |> prepend newline
    |> should
        equal
        """
let list = [
    someItem
    (*
        comment
    *)
]
"""

/// Hash directives (#if/#endif) are assigned as ContentBefore of the closing `]` during trivia assignment.
/// This forces the list into aligned bracket layout instead of Stroustrup, because the directive
/// resets indentation to column 0 which would break the offside rule with an inline `[`.
[<Test>]
let ``hash define before closing list bracket`` () =
    formatSourceString
        """
let list = [
    someItem
    #if YOW
    #endif
]
"""
        config
    |> prepend newline
    |> should
        equal
        """
let list =
    [
        someItem
#if YOW
#endif
    ]
"""

[<Test>]
let ``line comment after source after last item in list`` () =
    formatSourceString
        """
let list = [
    someItem // trivia!
]
"""
        config
    |> prepend newline
    |> should
        equal
        """
let list = [
    someItem // trivia!
]
"""

[<Test; Ignore("Inline block comment stays single-line - expected output needs review")>]
let ``multiline block comment after source after last item in list`` () =
    formatSourceString
        """
let list = [
    someItem (*
      trivia!
    *)
]
"""
        config
    |> prepend newline
    |> should
        equal
        """
let list = [
    someItem (*
      trivia!
    *)
]
"""

// The following tests cover an edge case where a line comment sits after #endif
// but before the closing bracket. The core problem:
//
//   1. findNodeBeforeWithMatchingColumn matches "item1" (column 4) for the comment (column 4),
//      assigning it as ContentAfter on item1 — but #else/#endif sit between them in the source.
//   2. The directives are assigned as ContentBefore on ] (they go through the default path).
//   3. This reverses the source order: the comment (line 8) is emitted before #else (line 5).
//
// After formatting, directives move to column 0, so on the second pass the comment is no longer
// at the same column as the preceding item — breaking the column-matching heuristic and causing
// the comment to shift between passes (not idempotent).
//
// A proper fix would need findNodeBeforeWithMatchingColumn to be aware of directive boundaries:
// if a #if/#else/#endif sits between the candidate node and the comment, the match is invalid.
// This is a very specific interaction between the column-matching trivia assignment and the
// multi-define formatting pipeline.

[<Test; Ignore("Trivia ordering broken when comment follows #endif - see comment above")>]
let ``comment before closing list bracket with hash directive, something defined`` () =
    formatSourceStringWithDefines
        [ "something" ]
        """
let list = [
    someItem
    #if something
    item1
    #else
    item2
    #endif
    // comment
                ]
"""
        config
    |> prepend newline
    |> should
        equal
        """
let list = [
    someItem
#if something
    item1
#else
#endif
    // comment
]
"""

[<Test; Ignore("Trivia ordering broken when comment follows #endif - see comment above")>]
let ``comment before closing list bracket with hash directive, nothing defined`` () =
    formatSourceStringWithDefines
        []
        """
let list = [
    someItem
    #if something
    item1
    #else
    item2
    #endif
    // comment
                ]
"""
        config
    |> prepend newline
    |> should
        equal
        """
let list = [
    someItem
#if something
#else
    item2
#endif
    // comment
]
"""

[<Test; Ignore("Trivia ordering broken when comment follows #endif - see comment above")>]
let ``comment before closing list bracket with hash directive`` () =
    formatSourceString
        """
let list = [
    someItem
    #if something
    item1
    #else
    item2
    #endif
    // comment
                ]
"""
        config
    |> prepend newline
    |> should
        equal
        """
let list = [
    someItem
#if something
    item1
#else
    item2
#endif
    // comment
]
"""

[<Test>]
let ``empty array with blank line inside, 3098`` () =
    formatSourceString
        """
let myArray = [|
  
  |]
"""
        config
    |> prepend newline
    |> should
        equal
        """
let myArray = [|

    |]
"""

[<Test>]
let ``empty array with comment inside, 3098`` () =
    formatSourceString
        """
let myArray2 = [|
  // Some comment
  |]
"""
        config
    |> prepend newline
    |> should
        equal
        """
let myArray2 = [|
    // Some comment
    |]
"""
