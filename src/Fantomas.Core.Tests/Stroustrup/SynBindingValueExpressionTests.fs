module Fantomas.Core.Tests.Stroustrup.SynBindingValueExpressionTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelpers
open Fantomas.Core

let config =
    { config with
        MultilineBracketStyle = Stroustrup
        MaxArrayOrListWidth = 40 }

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
            RecordMultilineFormatter = NumberOfItems }
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
            RecordMultilineFormatter = NumberOfItems }
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
            RecordMultilineFormatter = NumberOfItems }
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
            RecordMultilineFormatter = NumberOfItems }
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
            RecordMultilineFormatter = NumberOfItems }
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
            RecordMultilineFormatter = NumberOfItems }
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
            RecordMultilineFormatter = NumberOfItems }
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
            RecordMultilineFormatter = NumberOfItems }
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
let list =
    [
        someItem
#if something
        item1
#else
        item2
#endif
    // comment
    ]
"""
