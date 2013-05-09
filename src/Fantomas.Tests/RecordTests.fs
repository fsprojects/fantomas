module Fantomas.Tests.RecordTests

open NUnit.Framework
open FsUnit

open Fantomas.CodeFormatter
open Fantomas.Tests.TestHelper

[<Test>]
let ``record declaration``() =
    formatSourceString false "type AParameters = { a : int }" config
    |> prepend newline
    |> should equal """
type AParameters = 
    { a : int }"""

[<Test>]
let ``record signatures``() =
    formatSourceString true """
/// Represents simple XML elements.
type Element =
    {
        /// The attribute collection.
        Attributes : IDictionary<Name,string>

        /// The children collection.
        Children : seq<INode>

        /// The qualified name.
        Name : Name
    }

    interface INode

    /// Constructs an new empty Element.
    static member Create : name: string * ?uri: string -> Element

    /// Replaces the children.
    static member WithChildren : children: #seq<#INode> -> self: Element -> Element

    /// Replaces the children.
    static member ( - ) : self: Element * children: #seq<#INode> -> Element

    /// Replaces the attributes.
    static member WithAttributes : attrs: #seq<string*string> -> self: Element -> Element

    /// Replaces the attributes.
    static member ( + ) : self: Element * attrs: #seq<string*string> -> Element

    /// Replaces the children with a single text node.
    static member WithText : text: string -> self: Element-> Element

    /// Replaces the children with a single text node.
    static member ( -- ) : self: Element * text: string -> Element""" config
    |> prepend newline
    |> should equal """
/// Represents simple XML elements.
type Element = 
    { /// The attribute collection.
      Attributes : IDictionary<Name, string>;
      /// The children collection.
      Children : seq<INode>;
      /// The qualified name.
      Name : Name }
    interface INode
    /// Constructs an new empty Element.
    static member Create : name:string * ?uri:string -> Element
    /// Replaces the children.
    static member WithChildren : children:#seq<#INode>
         -> self:Element -> Element
    /// Replaces the children.
    static member (-) : self:Element * children:#seq<#INode> -> Element
    /// Replaces the attributes.
    static member WithAttributes : attrs:#seq<string * string>
         -> self:Element -> Element
    /// Replaces the attributes.
    static member (+) : self:Element * attrs:#seq<string * string> -> Element
    /// Replaces the children with a single text node.
    static member WithText : text:string -> self:Element -> Element
    /// Replaces the children with a single text node.
    static member (--) : self:Element * text:string -> Element"""

[<Test>]
let ``records with update``() =
    formatSourceString false """
type Car = {
    Make : string
    Model : string
    mutable Odometer : int
    }

let myRecord3 = { myRecord2 with Y = 100; Z = 2 }""" config
    |> prepend newline
    |> should equal """
type Car = 
    { Make : string;
      Model : string;
      mutable Odometer : int }

let myRecord3 = 
    { myRecord2 with Y = 100;
                     Z = 2 }"""

// the current behavior results in a compile error since the if is not aligned properly
[<Test>]
let ``should not break inside of if statements in records``() =
    formatSourceString false """let XpkgDefaults() =
    {
        ToolPath = "./tools/xpkg/xpkg.exe"
        WorkingDir = "./";
        TimeOut = TimeSpan.FromMinutes 5.
        Package = null
        Version = if not isLocalBuild then buildVersion else "0.1.0.0"
        OutputPath = "./xpkg"
        Project = null
        Summary = null
        Publisher = null
        Website = null
        Details = "Details.md"
        License = "License.md"
        GettingStarted = "GettingStarted.md"
        Icons = []
        Libraries = []
        Samples = [];
    }

    """ config
    |> should equal """let XpkgDefaults() = 
    { ToolPath = "./tools/xpkg/xpkg.exe";
      WorkingDir = "./";
      TimeOut = TimeSpan.FromMinutes 5.0;
      Package = null;
      Version = 
          if not isLocalBuild
          then buildVersion
          else "0.1.0.0";
      OutputPath = "./xpkg";
      Project = null;
      Summary = null;
      Publisher = null;
      Website = null;
      Details = "Details.md";
      License = "License.md";
      GettingStarted = "GettingStarted.md";
      Icons = [];
      Libraries = [];
      Samples = [] }"""