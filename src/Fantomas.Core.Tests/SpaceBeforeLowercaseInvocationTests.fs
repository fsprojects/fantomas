module Fantomas.Core.Tests.SpaceBeforeLowercaseInvocationTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelpers

let noSpaceBefore =
    { config with
        SpaceBeforeLowercaseInvocation = false
    }

/// Space before () in lowercase function call

[<Test>]
let ``default config should add space before unit in lowercase function call`` () =
    formatSourceString "let value = myFunction()" config
    |> should
        equal
        """let value = myFunction ()
"""

[<Test>]
let ``spaceBeforeLowercaseInvocation = false, should not add space before unit in lowercase function call`` () =
    formatSourceString "let value = myFunction()" noSpaceBefore
    |> should
        equal
        """let value = myFunction()
"""

// Space before parentheses (a+b) in lowercase function call

[<Test>]
let ``default config should add space before parentheses in lowercase function call`` () =
    formatSourceString "let value = myFunction(a+b)" config
    |> should
        equal
        """let value = myFunction (a + b)
"""

[<Test>]
let ``spaceBeforeLowercaseInvocation = false, should not add space before parentheses in lowercase function call`` () =
    formatSourceString "let value = myFunction(a+b)" noSpaceBefore
    |> should
        equal
        """let value = myFunction(a + b)
"""

[<Test>]
let ``spaceBeforeLowercaseInvocation should not have impact when member is called after unit`` () =
    formatSourceString "let v1 = myFunction().Member" noSpaceBefore
    |> prepend newline
    |> should
        equal
        """
let v1 = myFunction().Member
"""

[<Test>]
let ``space before lower constructor without new`` () =
    formatSourceString
        """
let tree1 =
    binaryNode(binaryNode(binaryValue 1, binaryValue 2), binaryNode(binaryValue 3, binaryValue 4))
"""
        { config with MaxLineLength = 80 }
    |> prepend newline
    |> should
        equal
        """
let tree1 =
    binaryNode (
        binaryNode (binaryValue 1, binaryValue 2),
        binaryNode (binaryValue 3, binaryValue 4)
    )
"""

[<Test>]
let ``space before lower case constructor invocation with new keyword`` () =
    formatSourceString
        """
let person = new person("Jim", 33)

let otherThing =
    new foobar(longname1, longname2, longname3, longname4, longname5, longname6, longname7)
"""
        { config with MaxLineLength = 90 }
    |> prepend newline
    |> should
        equal
        """
let person = new person ("Jim", 33)

let otherThing =
    new foobar (
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
let ``space before lower member call`` () =
    formatSourceString
        """
let myRegexMatch = Regex.matches(input, regex)

let myRegexMatchLong =
    Regex.matches("my longer input string with some interesting content in it","myRegexPattern")

let untypedRes = checker.parseFile(file, source, opts)

let untypedResLong =
    checker.parseFile(fileName, sourceText, parsingOptionsWithDefines, somethingElseWithARatherLongVariableName)
"""
        { config with MaxLineLength = 90 }
    |> prepend newline
    |> should
        equal
        """
let myRegexMatch = Regex.matches (input, regex)

let myRegexMatchLong =
    Regex.matches (
        "my longer input string with some interesting content in it",
        "myRegexPattern"
    )

let untypedRes = checker.parseFile (file, source, opts)

let untypedResLong =
    checker.parseFile (
        fileName,
        sourceText,
        parsingOptionsWithDefines,
        somethingElseWithARatherLongVariableName
    )
"""

[<Test>]
let ``no space before lowercase member calls and constructors`` () =
    formatSourceString
        """
let tree1 =
    binaryNode(binaryNode(binaryValue 1, binaryValue 2), binaryNode(binaryValue 3, binaryValue 4))

let person = new person("Jim", 33)
let otherThing =
    new foobar(longname1, longname2, longname3, longname4, longname5, longname6, longname7)

let myRegexMatch = Regex.matches(input, regex)

let myRegexMatchLong =
    Regex.matches("my longer input string with some interesting content in it","myRegexPattern")

let untypedRes = checker.parseFile(file, source, opts)

let untypedResLong =
    checker.parseFile(fileName, sourceText, parsingOptionsWithDefines, somethingElseWithARatherLongVariableName)
"""
        { noSpaceBefore with
            MaxLineLength = 60
        }
    |> prepend newline
    |> should
        equal
        """
let tree1 =
    binaryNode(
        binaryNode(binaryValue 1, binaryValue 2),
        binaryNode(binaryValue 3, binaryValue 4)
    )

let person = new person("Jim", 33)

let otherThing =
    new foobar(
        longname1,
        longname2,
        longname3,
        longname4,
        longname5,
        longname6,
        longname7
    )

let myRegexMatch = Regex.matches(input, regex)

let myRegexMatchLong =
    Regex.matches(
        "my longer input string with some interesting content in it",
        "myRegexPattern"
    )

let untypedRes = checker.parseFile(file, source, opts)

let untypedResLong =
    checker.parseFile(
        fileName,
        sourceText,
        parsingOptionsWithDefines,
        somethingElseWithARatherLongVariableName
    )
"""

[<Test>]
let ``ignore setting when function call is the argument of prefix application`` () =
    formatSourceString
        """
!-String.Empty.padLeft(braceSize + spaceAround)
(!-System.String.Empty.padRight(delta)) ({ ctx with RecordBraceStart = rest })
!- meh()
"""
        config
    |> prepend newline
    |> should
        equal
        """
!-String.Empty.padLeft(braceSize + spaceAround)
(!-System.String.Empty.padRight(delta)) ({ ctx with RecordBraceStart = rest })
!-meh()
"""

[<Test>]
let ``setting also affects patterns`` () =
    formatSourceString
        """
match x with
| y() -> ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
match x with
| y () -> ()
"""

[<Test>]
let ``space before lowercase patterns`` () =
    formatSourceString
        """
match x with
| a() -> ()
| B.c() -> ()
| d(e = f) -> ()
| G.h(i = j) -> ()
"""
        config
    |> prepend newline
    |> should
        equal
        """
match x with
| a () -> ()
| B.c () -> ()
| d (e = f) -> ()
| G.h (i = j) -> ()
"""

[<Test>]
let ``no space before lowercase patterns`` () =
    formatSourceString
        """
match x with
| a () -> ()
| B.c () -> ()
| d (e = f) -> ()
| G.h (i = j) -> ()
"""
        noSpaceBefore
    |> prepend newline
    |> should
        equal
        """
match x with
| a() -> ()
| B.c() -> ()
| d(e = f) -> ()
| G.h(i = j) -> ()
"""

/// The setting only gets a say when the whole thing being called is a plain dotted name.
/// A call, an index, a receiver that is not a name, or a type application anywhere in it, and the
/// parenthesis stays tight. Agreed at https://github.com/fsharp/fslang-design/issues/648.
/// The uppercase half of these live in SpaceBeforeUppercaseInvocationTests.

[<Test>]
let ``space before a deeply qualified lowercase function`` () =
    formatSourceString
        """
Fantomas.FCS.Text.Range.unionRanges(r1, r2)
"""
        config
    |> prepend newline
    |> should
        equal
        """
Fantomas.FCS.Text.Range.unionRanges (r1, r2)
"""

[<Test>]
let ``the last part of the name decides which of the two settings applies`` () =
    formatSourceString
        """
a.B.foo(x)
a.b.Foo(x)
"""
        config
    |> prepend newline
    |> should
        equal
        """
a.B.foo (x)
a.b.Foo(x)
"""

[<Test>]
let ``space before a call on a type parameter, which is a plain name`` () =
    formatSourceString
        """
let inline f<'T when 'T: (static member StaticProperty: int with set)> () = 'T.set_StaticProperty(3)
"""
        config
    |> prepend newline
    |> should
        equal
        """
let inline f<'T when 'T: (static member StaticProperty: int with set)> () = 'T.set_StaticProperty (3)
"""

[<Test>]
let ``no space anywhere in the reported fluent chain, fslang-design 648`` () =
    formatSourceString
        """
xs.map(fun a -> a + 1)
  .filter(fun a -> a > 1)
"""
        config
    |> prepend newline
    |> should
        equal
        """
xs.map(fun a -> a + 1).filter(fun a -> a > 1)
"""

[<Test>]
let ``no space before a generic call that has no dots`` () =
    formatSourceString
        """
unbox<int>(obj)
List.map<int>(f)
"""
        config
    |> prepend newline
    |> should
        equal
        """
unbox<int>(obj)
List.map<int>(f)
"""

[<Test>]
let ``a generic application without parentheses is left alone`` () =
    formatSourceString
        """
unbox<int> obj
"""
        config
    |> prepend newline
    |> should
        equal
        """
unbox<int> obj
"""
