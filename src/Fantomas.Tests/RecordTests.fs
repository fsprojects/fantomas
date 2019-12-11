module Fantomas.Tests.RecordTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper

[<Test>]
let ``record declaration``() =
    formatSourceString false "type AParameters = { a : int }" config
    |> prepend newline
    |> should equal """
type AParameters =
    { a: int }
"""

[<Test>]
let ``record declaration with implementation visibility attribute``() =
    formatSourceString false "type AParameters = private { a : int; b: float }" config
    |> prepend newline
    |> should equal """
type AParameters =
    private { a: int
              b: float }
"""

[<Test>]
let ``record signatures``() =
    formatSourceString true """
module RecordSignature
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
    static member ( -- ) : self: Element * text: string -> Element""" { config with SemicolonAtEndOfLine = true }
    |> prepend newline
    |> should equal """
module RecordSignature
/// Represents simple XML elements.
type Element =
    { /// The attribute collection.
      Attributes: IDictionary<Name, string>;

      /// The children collection.
      Children: seq<INode>;

      /// The qualified name.
      Name: Name }

    interface INode

    /// Constructs an new empty Element.
    static member Create: name:string * ?uri:string -> Element

    /// Replaces the children.
    static member WithChildren: children:#seq<#INode> -> self:Element -> Element

    /// Replaces the children.
    static member (-): self:Element * children:#seq<#INode> -> Element

    /// Replaces the attributes.
    static member WithAttributes: attrs:#seq<string * string> -> self:Element -> Element

    /// Replaces the attributes.
    static member (+): self:Element * attrs:#seq<string * string> -> Element

    /// Replaces the children with a single text node.
    static member WithText: text:string -> self:Element -> Element

    /// Replaces the children with a single text node.
    static member (--): self:Element * text:string -> Element
"""

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
    { Make: string
      Model: string
      mutable Odometer: int }

let myRecord3 =
    { myRecord2 with
          Y = 100
          Z = 2 }
"""

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

    """ { config with SemicolonAtEndOfLine = true }
    |> should equal """let XpkgDefaults() =
    { ToolPath = "./tools/xpkg/xpkg.exe";
      WorkingDir = "./";
      TimeOut = TimeSpan.FromMinutes 5.;
      Package = null;
      Version =
          if not isLocalBuild then buildVersion else "0.1.0.0";
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
      Samples = [] }
"""

[<Test>]
let ``should not add redundant newlines when using a record in a DU``() =
    formatSourceString false """
let rec make item depth = 
    if depth > 0 then 
        Tree({ Left = make (2 * item - 1) (depth - 1)
               Right = make (2 * item) (depth - 1) }, item)
    else Tree(defaultof<_>, item)""" config
  |> prepend newline
  |> should equal """
let rec make item depth =
    if depth > 0 then
        Tree
            ({ Left = make (2 * item - 1) (depth - 1)
               Right = make (2 * item) (depth - 1) }, item)
    else
        Tree(defaultof<_>, item)
"""

[<Test>]
let ``should keep unit of measures in record and DU declaration``() =
    formatSourceString false """
type rate = {Rate:float<GBP*SGD/USD>}
type rate2 = Rate of float<GBP/SGD*USD>
"""  config
  |> prepend newline
  |> should equal """
type rate =
    { Rate: float<GBP * SGD / USD> }

type rate2 = Rate of float<GBP / SGD * USD>
"""

[<Test>]
let ``should keep comments on records``() =
    shouldNotChangeAfterFormat """
let newDocument = //somecomment
    { program = Encoding.Default.GetBytes(document.Program) |> Encoding.UTF8.GetString
      content = Encoding.Default.GetBytes(document.Content) |> Encoding.UTF8.GetString
      created = document.Created.ToLocalTime() }
    |> JsonConvert.SerializeObject
"""

[<Test>]
let ``should preserve inherit parts in records``() =
    formatSourceString false """
type MyExc =
    inherit Exception
    new(msg) = {inherit Exception(msg)}
"""  config
  |> prepend newline
  |> should equal """
type MyExc =
    inherit Exception
    new(msg) = { inherit Exception(msg) }
"""

[<Test>]
let ``should preserve inherit parts in records with field``() =
    formatSourceString false """
type MyExc =
    inherit Exception
    new(msg) = {inherit Exception(msg)
                X = 1}
"""  config
  |> prepend newline
  |> should equal """
type MyExc =
    inherit Exception
    new(msg) = { inherit Exception(msg); X = 1 }
"""

[<Test>]
let ``should preserve inherit parts in records multiline``() =
    formatSourceString false """
type MyExc =
    inherit Exception
    new(msg) = {inherit Exception(msg)
                X = 1
                Y = 2}
"""  config
  |> prepend newline
  |> should equal """
type MyExc =
    inherit Exception
    new(msg) =
        { inherit Exception(msg)
          X = 1
          Y = 2 }
"""

[<Test>]
let ``anon record``() =
    shouldNotChangeAfterFormat """
let r: {| Foo: int; Bar: string |} =
    {| Foo = 123
       Bar = "" |}
"""

[<Test>]
let `` anon record - struct``() =
    shouldNotChangeAfterFormat """
let r: struct {| Foo: int; Bar: string |} =
    struct {| Foo = 123
              Bar = "" |}
"""

[<Test>]
let ``meaningful space should be preserved, 353`` () =
    formatSourceString false """to'.WithCommon(fun o' ->
        { dotnetOptions o' with WorkingDirectory =
                                  Path.getFullName "RegressionTesting/issue29"
                                Verbosity = Some DotNet.Verbosity.Minimal }).WithParameters""" config
    |> should equal """to'.WithCommon(fun o' ->
    { dotnetOptions o' with
          WorkingDirectory = Path.getFullName "RegressionTesting/issue29"
          Verbosity = SomeDotNet.Verbosity.Minimal }).WithParameters
"""

[<Test>]
let ``record with long string inside array`` () =
    formatSourceString false "
type Database =

    static member Default () =
        Database.Lowdb
            .defaults(
                { Version = CurrentVersion
                  Questions =
                    [| { Id = 0
                         AuthorId = 1
                         Title = \"What is the average wing speed of an unladen swallow?\"
                         Description =
                             \"\"\"
Hello, yesterday I saw a flight of swallows and was wondering what their **average wing speed** is?

If you know the answer please share it.
                             \"\"\"
                         Answers =
                            [| { Id = 0
                                 CreatedAt = DateTime.Parse \"2017-09-14T19:57:33.103Z\"
                                 AuthorId = 0
                                 Score = 2
                                 Content =
                                    \"\"\"
> What do you mean, an African or European Swallow?
>
> Monty Python’s: The Holy Grail

Ok I must admit, I use google to search the question and found a post explaining the reference :).

I thought you were asking it seriously, well done.
                                    x\"\"\" }
                               { Id = 1
                                 CreatedAt = DateTime.Parse \"2017-09-14T20:07:27.103Z\"
                                 AuthorId = 2
                                 Score = 1
                                 Content =
                                    \"\"\"
Maxime,

I believe you found [this blog post](http://www.saratoga.com/how-should-i-know/2013/07/what-is-the-average-air-speed-velocity-of-a-laden-swallow/).

And so Robin, the conclusion of the post is:

> In the end, it’s concluded that the airspeed velocity of a (European) unladen swallow is about 24 miles per hour or 11 meters per second.
                                    \"\"\" }
                            |]
                         CreatedAt = DateTime.Parse \"2017-09-14T17:44:28.103Z\" }
                       { Id = 1
                         AuthorId = 0
                         Title = \"Why did you create Fable?\"
                         Description =
                             \"\"\"
Hello Alfonso,

I wanted to know why you created Fable. Did you always plan to use F#? Or were you thinking in others languages?
                             \"\"\"
                         Answers = [| |]
                         CreatedAt = DateTime.Parse \"2017-09-12T09:27:28.103Z\" } |]
                  Users =
                    [| { Id = 0
                         Firstname = \"Maxime\"
                         Surname = \"Mangel\"
                         Avatar = \"maxime_mangel.png\" }
                       { Id = 1
                         Firstname = \"Robin\"
                         Surname = \"Munn\"
                         Avatar = \"robin_munn.png\" }
                       { Id = 2
                         Firstname = \"Alfonso\"
                         Surname = \"Garciacaro\"
                         Avatar = \"alfonso_garciacaro.png\" }
                       { Id = 3
                         Firstname = \"Guest\"
                         Surname = \"\"
                         Avatar = \"guest.png\" }
                          |]
                }
            ).write()
        Logger.debug \"Database restored\"

"      config
    |> fun formatted -> formatted
    |> should equal "type Database =

    static member Default() =
        Database.Lowdb.defaults({ Version = CurrentVersion
                                  Questions =
                                      [| { Id = 0
                                           AuthorId = 1
                                           Title = \"What is the average wing speed of an unladen swallow?\"
                                           Description = \"\"\"
Hello, yesterday I saw a flight of swallows and was wondering what their **average wing speed** is?

If you know the answer please share it.
                             \"\"\"
                                           Answers =
                                               [| { Id = 0
                                                    CreatedAt = DateTime.Parse \"2017-09-14T19:57:33.103Z\"
                                                    AuthorId = 0
                                                    Score = 2
                                                    Content = \"\"\"
> What do you mean, an African or European Swallow?
>
> Monty Python’s: The Holy Grail

Ok I must admit, I use google to search the question and found a post explaining the reference :).

I thought you were asking it seriously, well done.
                                    x\"\"\"          }
                                                  { Id = 1
                                                    CreatedAt = DateTime.Parse \"2017-09-14T20:07:27.103Z\"
                                                    AuthorId = 2
                                                    Score = 1
                                                    Content = \"\"\"
Maxime,

I believe you found [this blog post](http://www.saratoga.com/how-should-i-know/2013/07/what-is-the-average-air-speed-velocity-of-a-laden-swallow/).

And so Robin, the conclusion of the post is:

> In the end, it’s concluded that the airspeed velocity of a (European) unladen swallow is about 24 miles per hour or 11 meters per second.
                                    \"\"\"           } |]
                                           CreatedAt = DateTime.Parse \"2017-09-14T17:44:28.103Z\" }
                                         { Id = 1
                                           AuthorId = 0
                                           Title = \"Why did you create Fable?\"
                                           Description = \"\"\"
Hello Alfonso,

I wanted to know why you created Fable. Did you always plan to use F#? Or were you thinking in others languages?
                             \"\"\"
                                           Answers = [||]
                                           CreatedAt = DateTime.Parse \"2017-09-12T09:27:28.103Z\" } |]
                                  Users =
                                      [| { Id = 0
                                           Firstname = \"Maxime\"
                                           Surname = \"Mangel\"
                                           Avatar = \"maxime_mangel.png\" }
                                         { Id = 1
                                           Firstname = \"Robin\"
                                           Surname = \"Munn\"
                                           Avatar = \"robin_munn.png\" }
                                         { Id = 2
                                           Firstname = \"Alfonso\"
                                           Surname = \"Garciacaro\"
                                           Avatar = \"alfonso_garciacaro.png\" }
                                         { Id = 3
                                           Firstname = \"Guest\"
                                           Surname = \"\"
                                           Avatar = \"guest.png\" } |] }).write()
        Logger.debug \"Database restored\"
"

[<Test>]
let ``issue 457``() =
    formatSourceString false """
let x = Foo("").Goo()

let r =
    { s with
        x = 1
        y = 2 }
"""  config
  |> prepend newline
  |> should equal """
let x = Foo("").Goo()

let r =
    { s with
          x = 1
          y = 2 }
"""

[<Test>]
let ``class member attributes should not introduce newline, 471`` () =
    formatSourceString false """type Test =
    | String of string

    [<SomeAttribute>]
    member x._Print = ""

    member this.TestMember = ""
"""  config
    |> prepend newline
    |> should equal """
type Test =
    | String of string

    [<SomeAttribute>]
    member x._Print = ""

    member this.TestMember = ""
"""

[<Test>]
let ``multiline record should be on new line after DU constructor, 462`` () =
    formatSourceString false """
let expect =
    Result<Schema, SetError>.Ok { opts =
                                      [ Opts.anyOf
                                          ([ (Optional, Opt.flagTrue [ "first"; "f" ])
                                             (Optional, Opt.value [ "second"; "s" ]) ])
                                        Opts.oneOf
                                            (Optional,
                                             [ Opt.flag [ "third"; "f" ]
                                               Opt.valueWith "new value" [ "fourth"; "ssssssssssssssssssssssssssssssssssssssssssssssssssss" ] ]) ]
                                  args = []
                                  commands = [] }
"""  config
    |> prepend newline
    |> should equal """
let expect =
    Result<Schema, SetError>.Ok
        { opts =
              [ Opts.anyOf
                  ([ (Optional, Opt.flagTrue [ "first"; "f" ])
                     (Optional, Opt.value [ "second"; "s" ]) ])
                Opts.oneOf
                    (Optional,
                     [ Opt.flag [ "third"; "f" ]
                       Opt.valueWith "new value" [ "fourth"; "ssssssssssssssssssssssssssssssssssssssssssssssssssss" ] ]) ]
          args = []
          commands = [] }
"""

[<Test>]
let ``short record should remain on the same line after DU constructor`` () =
    formatSourceString false """
let expect = Result<int,string>.Ok   7"""  config
    |> prepend newline
    |> should equal """
let expect = Result<int, string>.Ok 7
"""

[<Test>]
let ``multiline list after DU should be on new line after DU constructor`` () =
    formatSourceString false """
let expect =
    Result<int,string>.Ok [ "fooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo"
                            "baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaar"
                            "meh"
                          ]"""  config
    |> prepend newline
    |> should equal """
let expect =
    Result<int, string>
        .Ok
        [ "fooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooooo"
          "baaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaar"
          "meh" ]
"""

[<Test>]
let ``record with long string, 472`` () =
    formatSourceString false "
namespace web_core

open WebSharper.UI

module Maintoc =
  let Page =
    { MyPage.Create() with body =
                                [ Doc.Verbatim \"\"\"
This is a very long line in a multi-line string, so long in fact that it is longer than that page width to which I am trying to constrain everything, and so it goes bang.
\"\"\" ] }"  config
    |> prepend newline
    |> should equal "
namespace web_core

open WebSharper.UI

module Maintoc =
    let Page =
        { MyPage.Create() with
              body =
                  [ Doc.Verbatim \"\"\"
This is a very long line in a multi-line string, so long in fact that it is longer than that page width to which I am trying to constrain everything, and so it goes bang.
\"\"\" ]   }
"

[<Test>]
let ``record type signature with line comment, 517`` () =
    formatSourceString true """
module RecordSignature
/// Represents simple XML elements.
type Element =
    { /// The attribute collection.
      Attributes: IDictionary<Name, string>;

      /// The children collection.
      Children: seq<INode>;

      /// The qualified name.
      Name: Name }
"""  config
    |> prepend newline
    |> should equal """
module RecordSignature
/// Represents simple XML elements.
type Element =
    { /// The attribute collection.
      Attributes: IDictionary<Name, string>

      /// The children collection.
      Children: seq<INode>

      /// The qualified name.
      Name: Name }
"""