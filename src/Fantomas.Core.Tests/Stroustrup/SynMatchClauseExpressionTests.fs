module Fantomas.Core.Tests.Stroustrup.SynMatchClauseExpressionTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelpers
open Fantomas.Core

let config =
    { config with
        MultilineBracketStyle = Stroustrup
        MaxArrayOrListWidth = 40 }

[<Test>]
let ``synMatchClause in match expression with record instance `` () =
    formatSourceString
        false
        """
match x with
| _ ->
    { A = longTypeName
      B = someOtherVariable
      C = ziggyBarX }
"""
        config
    |> prepend newline
    |> should
        equal
        """
match x with
| _ -> {
    A = longTypeName
    B = someOtherVariable
    C = ziggyBarX
  }
"""

[<Test>]
let ``synMatchClause in match expression with update record`` () =
    formatSourceString
        false
        """
match x with
| _ ->
    { astContext with IsInsideMatchClausePattern = true }
"""
        config
    |> prepend newline
    |> should
        equal
        """
match x with
| _ -> {
    astContext with
        IsInsideMatchClausePattern = true
  }
"""

[<Test>]
let ``synMatchClause in match expression with anonymous record instance`` () =
    formatSourceString
        false
        """
match x with
| _ ->
   {| A = longTypeName
      B = someOtherVariable
      C = ziggyBarX |}
"""
        config
    |> prepend newline
    |> should
        equal
        """
match x with
| _ -> {|
    A = longTypeName
    B = someOtherVariable
    C = ziggyBarX
  |}
"""

[<Test>]
let ``synMatchClause in match expression with anonymous record instance struct`` () =
    formatSourceString
        false
        """
match x with
| _ ->
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
match x with
| _ -> struct {|
    A = longTypeName
    B = someOtherVariable
    C = ziggyBarX
  |}
"""

[<Test>]
let ``synMatchClause in match expression with list`` () =
    formatSourceString
        false
        """
match x with
| _ ->
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
match x with
| _ -> [
    itemOne
    itemTwo
    itemThree
    itemFour
    itemFive
  ]
"""

[<Test>]
let ``synMatchClause in match expression with array`` () =
    formatSourceString
        false
        """
match x with
| _ ->
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
match x with
| _ -> [|
    itemOne
    itemTwo
    itemThree
    itemFour
    itemFive
  |]
"""

// TODO: Here, I again feel this is fitting not to have stroustrup.
// Similar to long patterns in synbinding functions.

[<Test>]
let ``synMatchClause in match expression with long when expression with record instance `` () =
    formatSourceString
        false
        """
match x with
| _ when (try
            somethingDangerous ()
            true
          with | ex -> false)
                                    ->
                                        { A = longTypeName
                                          B = someOtherVariable
                                          C = ziggyBarX }
"""
        config
    |> prepend newline
    |> should
        equal
        """
match x with
| _ when
    (try
        somethingDangerous ()
        true
     with ex ->
         false)
      ->
      {
          A = longTypeName
          B = someOtherVariable
          C = ziggyBarX
      }
"""

[<Test>]
let ``synMatchClause in try/with expression with record instance `` () =
    formatSourceString
        false
        """
try
    foo()
with ex ->
    { A = longTypeName
      B = someOtherVariable
      C = ziggyBarX }
"""
        config
    |> prepend newline
    |> should
        equal
        """
try
    foo ()
with ex -> {
    A = longTypeName
    B = someOtherVariable
    C = ziggyBarX
}
"""

[<Test>]
let ``synMatchClause in try/with expression with update record`` () =
    formatSourceString
        false
        """
try
    foo()
with ex ->
    { astContext with IsInsideMatchClausePattern = true }
"""
        config
    |> prepend newline
    |> should
        equal
        """
try
    foo ()
with ex -> {
    astContext with
        IsInsideMatchClausePattern = true
}
"""

[<Test>]
let ``synMatchClause in try/with expression with anonymous record instance`` () =
    formatSourceString
        false
        """
try
    foo()
with ex ->
   {| A = longTypeName
      B = someOtherVariable
      C = ziggyBarX |}
"""
        config
    |> prepend newline
    |> should
        equal
        """
try
    foo ()
with ex -> {|
    A = longTypeName
    B = someOtherVariable
    C = ziggyBarX
|}
"""

[<Test>]
let ``synMatchClause in try/with expression with anonymous record instance struct`` () =
    formatSourceString
        false
        """
try
    foo()
with ex ->
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
try
    foo ()
with ex -> struct {|
    A = longTypeName
    B = someOtherVariable
    C = ziggyBarX
|}
"""

[<Test>]
let ``synMatchClause in try/with expression with list`` () =
    formatSourceString
        false
        """
try
    foo ()
with
| ex ->
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
try
    foo ()
with ex -> [
    itemOne
    itemTwo
    itemThree
    itemFour
    itemFive
]
"""

[<Test>]
let ``synMatchClause in try/with expression with array`` () =
    formatSourceString
        false
        """
try
    foo ()
with
| ex ->
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
try
    foo ()
with ex -> [|
    itemOne
    itemTwo
    itemThree
    itemFour
    itemFive
|]
"""

[<Test>]
let ``multiple clauses with lists`` () =
    formatSourceString
        false
        """
match x with
| SynMemberDefn.ImplicitCtor (_, attrs, ctorArgs, _, _xmlDoc, range) ->
    [ yield mkNode SynMemberDefn_ImplicitCtor range
      yield! (visitSynAttributeLists attrs)
      yield! visitSynSimplePats ctorArgs ]
| SynMemberDefn.ImplicitInherit (inheritType, inheritArgs, _, range) ->
    [ yield mkNode SynMemberDefn_ImplicitInherit range
      yield! visitSynType inheritType
      yield! visitSynExpr inheritArgs ]
"""
        config
    |> prepend newline
    |> should
        equal
        """
match x with
| SynMemberDefn.ImplicitCtor(_, attrs, ctorArgs, _, _xmlDoc, range) -> [
    yield mkNode SynMemberDefn_ImplicitCtor range
    yield! (visitSynAttributeLists attrs)
    yield! visitSynSimplePats ctorArgs
  ]
| SynMemberDefn.ImplicitInherit(inheritType, inheritArgs, _, range) -> [
    yield mkNode SynMemberDefn_ImplicitInherit range
    yield! visitSynType inheritType
    yield! visitSynExpr inheritArgs
  ]
"""

[<Test>]
let ``async indentation when inside of a match, 2501`` () =
    formatSourceString
        false
        """
let x =
    match packageRegistrationUrl with
    | Some packageRegistrationUrl ->

        async {

            let response = ""

            return ""
                
        }

    | None -> failwith "Package registration url not found"
"""
        config
    |> prepend newline
    |> should
        equal
        """
let x =
    match packageRegistrationUrl with
    | Some packageRegistrationUrl ->

        async {

            let response = ""

            return ""

        }

    | None -> failwith "Package registration url not found"
"""

[<Test>]
let ``clause indentation when combining clauses with OR and returning a short list, 2586`` () =
    formatSourceString
        false
        """
let y x =
    match x with
        | Case1
        | Case2 -> [ "X" ]
        | Case3 -> [ "Y" ]
"""
        config
    |> prepend newline
    |> should
        equal
        """
let y x =
    match x with
    | Case1
    | Case2 -> [ "X" ]
    | Case3 -> [ "Y" ]
"""

[<Test>]
let ``don't apply stroustrup when the token has trivia after it, 2806`` () =
    formatSourceString
        false
        """
match this.InboundSubnetName with
| None -> ()
| Some subnet -> // Build an inbound for the specified subnet.
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
match this.InboundSubnetName with
| None -> ()
| Some subnet -> // Build an inbound for the specified subnet.
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
