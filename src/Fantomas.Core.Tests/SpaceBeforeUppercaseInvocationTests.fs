module Fantomas.Core.Tests.SpaceBeforeUppercaseInvocationTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelpers

let spaceBeforeConfig =
    { config with
        SpaceBeforeUppercaseInvocation = true
    }

/// Space before () in Uppercase function call

[<Test>]
let ``default config should not add space before unit in uppercase function call`` () =
    formatSourceString "let value = MyFunction()" config
    |> should
        equal
        """let value = MyFunction()
"""

[<Test>]
let ``spaceBeforeUppercaseInvocation should add space before unit in uppercase function call`` () =
    formatSourceString "let value = MyFunction()" spaceBeforeConfig
    |> should
        equal
        """let value = MyFunction ()
"""

[<Test>]
let ``spaceBeforeUppercaseInvocation should add space before unit in chained uppercase function call`` () =
    formatSourceString "let value = person.ToString()" spaceBeforeConfig
    |> should
        equal
        """let value = person.ToString ()
"""

// Exception to the rule

[<Test>]
let ``spaceBeforeUppercaseInvocation should not have impact when member is called after unit`` () =
    formatSourceString "let v2 = OtherFunction().Member" spaceBeforeConfig
    |> prepend newline
    |> should
        equal
        """
let v2 = OtherFunction().Member
"""

[<Test>]
let ``spaceBeforeUppercaseInvocation should not have impact when member is called after construction invocation, 1401``
    ()
    =
    formatSourceString
        """
let x = DateTimeOffset(2017,6,1,10,3,14,TimeSpan(1,30,0)).LocalDateTime
"""
        spaceBeforeConfig
    |> prepend newline
    |> should
        equal
        """
let x = DateTimeOffset(2017, 6, 1, 10, 3, 14, TimeSpan (1, 30, 0)).LocalDateTime
"""

// Space before parentheses (a+b) in Uppercase function call

[<Test>]
let ``default config should not add space before parentheses in uppercase function call`` () =
    formatSourceString "let value = MyFunction(a+b)" config
    |> should
        equal
        """let value = MyFunction(a + b)
"""

[<Test>]
let ``spaceBeforeUppercaseInvocation should add space before parentheses in uppercase function call`` () =
    formatSourceString "let value = MyFunction(a+b)" spaceBeforeConfig
    |> should
        equal
        """let value = MyFunction (a + b)
"""

[<Test>]
let ``space before uppercase function application cannot apply with dot-chaining, 943`` () =
    formatSourceString
        """foo.Bar().[5]
"""
        { config with
            SpaceBeforeUppercaseInvocation = true
        }
    |> prepend newline
    |> should
        equal
        """
foo.Bar().[5]
"""

[<Test>]
let ``space before uppercase DotIndexedSet`` () =
    formatSourceString
        """foo.Bar().[5] <- 5
"""
        { config with
            SpaceBeforeUppercaseInvocation = true
        }
    |> prepend newline
    |> should
        equal
        """
foo.Bar().[5] <- 5
"""

[<Test>]
let ``setting SpaceBeforeUppercaseInvocation is not applied in the middle of a invocation chain, 853`` () =
    formatSourceString
        """
module SomeModule =
    let DoSomething (a:SomeType) =
        let someValue = a.Some.Thing("aaa").[0]
        someValue
"""
        { config with
            SpaceBeforeUppercaseInvocation = true
        }
    |> prepend newline
    |> should
        equal
        """
module SomeModule =
    let DoSomething (a: SomeType) =
        let someValue = a.Some.Thing("aaa").[0]
        someValue
"""

[<Test>]
let ``space before uppercase constructor without new`` () =
    formatSourceString
        """
let tree1 =
    BinaryNode(BinaryNode(BinaryValue 1, BinaryValue 2), BinaryNode(BinaryValue 3, BinaryValue 4))
"""
        { spaceBeforeConfig with
            MaxLineLength = 80
        }
    |> prepend newline
    |> should
        equal
        """
let tree1 =
    BinaryNode (
        BinaryNode (BinaryValue 1, BinaryValue 2),
        BinaryNode (BinaryValue 3, BinaryValue 4)
    )
"""

[<Test>]
let ``space before upper case constructor invocation with new keyword`` () =
    formatSourceString
        """
let person = new Person("Jim", 33)

let otherThing =
    new Foobar(longname1, longname2, longname3, longname4, longname5, longname6, longname7)
"""
        { spaceBeforeConfig with
            MaxLineLength = 90
        }
    |> prepend newline
    |> should
        equal
        """
let person = new Person ("Jim", 33)

let otherThing =
    new Foobar (
        longname1,
        longname2,
        longname3,
        longname4,
        longname5,
        longname6,
        longname7
    )
"""

[<Test>]
let ``space before uppercase member call`` () =
    formatSourceString
        """
let myRegexMatch = Regex.Match(input, regex)

let myRegexMatchLong =
    Regex.Match("my longer input string with some interesting content in it","myRegexPattern")

let untypedRes = checker.ParseFile(file, source, opts)

let untypedResLong =
    checker.ParseFile(fileName, sourceText, parsingOptionsWithDefines, somethingElseWithARatherLongVariableName)
"""
        { spaceBeforeConfig with
            MaxLineLength = 90
        }
    |> prepend newline
    |> should
        equal
        """
let myRegexMatch = Regex.Match (input, regex)

let myRegexMatchLong =
    Regex.Match (
        "my longer input string with some interesting content in it",
        "myRegexPattern"
    )

let untypedRes = checker.ParseFile (file, source, opts)

let untypedResLong =
    checker.ParseFile (
        fileName,
        sourceText,
        parsingOptionsWithDefines,
        somethingElseWithARatherLongVariableName
    )
"""

[<Test>]
let ``function application inside parenthesis followed by .DotIndexedGet, 1226`` () =
    formatSourceString
        """
module Foo =
    let Bar () =
        (doc.DocumentNode.SelectNodes "//table").[0]
"""
        config
    |> prepend newline
    |> should
        equal
        """
module Foo =
    let Bar () =
        (doc.DocumentNode.SelectNodes "//table").[0]
"""

[<Test>]
let ``ignore setting when function call is the argument of prefix application, 1488`` () =
    formatSourceString
        """
!-String.Empty.PadLeft(braceSize + spaceAround)
(!-System.String.Empty.PadRight(delta)) ({ ctx with RecordBraceStart = rest })
!- Meh()
"""
        spaceBeforeConfig
    |> prepend newline
    |> should
        equal
        """
!-String.Empty.PadLeft(braceSize + spaceAround)
(!-System.String.Empty.PadRight(delta)) ({ ctx with RecordBraceStart = rest })
!-Meh()
"""

[<Test>]
let ``no space before uppercase patterns`` () =
    formatSourceString
        """
match x with
| A () -> ()
| b.C () -> ()
| D (e = f) -> ()
| g.H (i = j) -> ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
match x with
| A() -> ()
| b.C() -> ()
| D(e = f) -> ()
| g.H(i = j) -> ()
"""

[<Test>]
let ``space before uppercase patterns`` () =
    formatSourceString
        """
match x with
| A() -> ()
| b.C() -> ()
| D(e = f) -> ()
| g.H(i = j) -> ()
"""
        spaceBeforeConfig
    |> prepend newline
    |> should
        equal
        """
match x with
| A () -> ()
| b.C () -> ()
| D (e = f) -> ()
| g.H (i = j) -> ()
"""

[<Test>]
let ``never add a space before paren lambda in chain, 2685`` () =
    formatSourceString
        """
module A =
    let foo =
        Foai.SomeLongTextYikes().ConfigureBarry(fun alpha beta gamma ->
            context.AddSomething ("a string") |> ignore
        ).MoreContext(fun builder ->
            // also good stuff
            ()
        ).ABC().XYZ

"""
        spaceBeforeConfig
    |> prepend newline
    |> should
        equal
        """
module A =
    let foo =
        Foai
            .SomeLongTextYikes()
            .ConfigureBarry(fun alpha beta gamma -> context.AddSomething ("a string") |> ignore)
            .MoreContext(fun builder ->
                // also good stuff
                ())
            .ABC()
            .XYZ
"""

[<Test>]
let ``typeApp with dotGet and paren expr, 2700`` () =
    formatSourceString
        """
let f = OptimizedClosures.FSharpFunc<_, _, _>.Adapt (mapping)
"""
        config
    |> prepend newline
    |> should
        equal
        """
let f = OptimizedClosures.FSharpFunc<_, _, _>.Adapt(mapping)
"""

[<Test>]
let ``space should not be added when expression is indexed, 2965`` () =
    formatSourceString
        """
fooo.Bar()[key]
"""
        spaceBeforeConfig
    |> prepend newline
    |> should
        equal
        """
fooo.Bar()[key]
"""

[<Test>]
let ``space should not be added when expression is indexed, single ident application`` () =
    formatSourceString
        """
Bar()[key]
"""
        spaceBeforeConfig
    |> prepend newline
    |> should
        equal
        """
Bar()[key]
"""

[<Test>]
let ``space should not be added when expression is indexed, parentheses argument`` () =
    formatSourceString
        """
fooo.Bar(1)[key]
"""
        spaceBeforeConfig
    |> prepend newline
    |> should
        equal
        """
fooo.Bar(1)[key]
"""

[<Test>]
let ``space should not be added when expression is indexed, single ident application with parentheses argument`` () =
    formatSourceString
        """
Bar(1)[key]
"""
        spaceBeforeConfig
    |> prepend newline
    |> should
        equal
        """
Bar(1)[key]
"""

/// The setting only gets a say when the whole thing being called is a plain dotted name.
/// A call, an index, a bracketed receiver, or a type application anywhere in it, and the
/// parenthesis stays tight. Agreed at https://github.com/fsharp/fslang-design/issues/648.
/// The lowercase half of these live in SpaceBeforeLowercaseInvocationTests.

[<Test>]
let ``space before a call reached by a single dot from a plain identifier`` () =
    formatSourceString
        """
a.Foo(x)
"""
        spaceBeforeConfig
    |> prepend newline
    |> should
        equal
        """
a.Foo (x)
"""

[<Test>]
let ``space before a call however many dots the name has`` () =
    formatSourceString
        """
a.B.C.Foo(x)
"""
        spaceBeforeConfig
    |> prepend newline
    |> should
        equal
        """
a.B.C.Foo (x)
"""

[<Test>]
let ``base and this are plain names and take the space`` () =
    formatSourceString
        """
base.Foo(x)
this.Foo(x)
"""
        spaceBeforeConfig
    |> prepend newline
    |> should
        equal
        """
base.Foo (x)
this.Foo (x)
"""

[<Test>]
let ``no space when a call is reached through a dot before it`` () =
    formatSourceString
        """
a.Foo(x).Bar(y)
"""
        spaceBeforeConfig
    |> prepend newline
    |> should
        equal
        """
a.Foo(x).Bar(y)
"""

[<Test>]
let ``no space when the receiver is itself a call`` () =
    formatSourceString
        """
Foo().Bar(y)
Dictionary<string, int>().Add(k, v)
"""
        spaceBeforeConfig
    |> prepend newline
    |> should
        equal
        """
Foo().Bar(y)
Dictionary<string, int>().Add(k, v)
"""

[<Test>]
let ``no space when an index comes before the call, in either syntax`` () =
    formatSourceString
        """
arr[0].Foo(x)
arr.[0].Foo(x)
"""
        spaceBeforeConfig
    |> prepend newline
    |> should
        equal
        """
arr[0].Foo(x)
arr.[0].Foo(x)
"""

[<Test>]
let ``no space when the receiver is a parenthesised expression`` () =
    formatSourceString
        """
(f x).Bar(y)
"""
        spaceBeforeConfig
    |> prepend newline
    |> should
        equal
        """
(f x).Bar(y)
"""

[<Test>]
let ``space before a call on a literal, which is atomic like a name`` () =
    formatSourceString
        """
"yow".Substring(0, 3)
3L.ToString()
"""
        spaceBeforeConfig
    |> prepend newline
    |> should
        equal
        """
"yow".Substring (0, 3)
3L.ToString ()
"""

[<Test>]
let ``no space when the receiver is bracketed rather than atomic`` () =
    formatSourceString
        """
[ 1; 2 ].Contains(1)
{| X = 1 |}.ToString()
"""
        spaceBeforeConfig
    |> prepend newline
    |> should
        equal
        """
[ 1; 2 ].Contains(1)
{| X = 1 |}.ToString()
"""

[<Test>]
let ``no space when a literal receiver is followed by a call`` () =
    formatSourceString
        """
"yow".Trim().Substring(0, 3)
"""
        spaceBeforeConfig
    |> prepend newline
    |> should
        equal
        """
"yow".Trim().Substring(0, 3)
"""

[<Test>]
let ``no space when the receiver carries a type application`` () =
    formatSourceString
        """
X<Y>.Foo(x)
X<Y>.B.Foo(x)
"""
        spaceBeforeConfig
    |> prepend newline
    |> should
        equal
        """
X<Y>.Foo(x)
X<Y>.B.Foo(x)
"""

[<Test>]
let ``no space when the call itself carries a type application`` () =
    formatSourceString
        """
a.Foo<int>(x)
a.B.Foo<int>(x)
"""
        spaceBeforeConfig
    |> prepend newline
    |> should
        equal
        """
a.Foo<int>(x)
a.B.Foo<int>(x)
"""
