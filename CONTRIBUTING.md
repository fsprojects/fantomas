# Introduction

Well hello there my dear reader! Thank you for considering to contribute to the Fantomas project!<br />
Fantomas is a bit a complex project and can be overwhelming to just dive in.
In this guide, we will try to get you starting and express our expectations in terms of potential contributions.<br />
We hope to give you a proper context about fixing bugs, expressing new ideas and the technical aspects.

## What are we looking for?

### Bugs fixes

The most welcome additions to project are bug fixes. The philosophy behind this is that everyone should be able to fix their own bug.
If we can achieve this as a community, we can share the workload and benefit all together.
The project can move at faster pace and improve as a whole.

We strongly encourage people to solve their own problems, it is rewarding and in theory it guarantees that the bug you had cannot occur again.
As a regression test will be asked when fixing a bug.

#### bug (soundness)

Fantomas is not perfect and depending on what code you are trying to format, your code can stop working after formatting.<br />
And there we unfortunately have to admit defeat.<br />
So the utmost important thing about this tool is that the before and after always ends up to the same working code.<br />
And if you can helps us out by fixing a bug that is labelled with `bug (soundness)`, you really help the project move forward.

Some people try Fantomas out, notice one bug and lose interest all together. This sounds harsh and that is why we really want to improve that first user experience.
The first go should be able to format all files, and then in the second go users can explore settings and tweak the style.

#### bug (stylistic)

Besides breaking issues, there are also problems where the output is not what people are expecting.
These are labelled as `bug (stylistic)` and could break some formatting rule or not respect a setting.
Again here, scratch your own itch. If something bothers you, the best cure is to try a take a stab at it yourself.

### Adoption

The dream is that every F# developer can use Fantomas at any time.
This is aspiration is an odyssey and might never come to fruition.
Any step in that direction is most welcome.
Try and introduce formatting in your project, at work or in an open-source project.
This tool will only improve by adoption.

#### Sponsoring

Fantomas knew a significant growth after the first sponsorship deal with [G-Research](https://www.gresearch.co.uk/).
It would still be in the dark ages if it weren't for this support. For that we will forever be grateful.
If you want to help increase adoption by providing financial support you can reach out to [conduct@fantomas.com](mailto:conduct@fantomas.com). 

## What are we not looking for?

### Stylistic features

Fantomas tries to adhere to two F# style guides:
- Microsoft: https://docs.microsoft.com/en-us/dotnet/fsharp/style-guide/formatting
- G-Research: https://github.com/G-Research/fsharp-formatting-conventions

Please o please, discuss everything related to code styles on those repositories.
Can't find anything mentioned there? Open an issue and **engage conversation**.

When a person opens a feature request that has not been discussed anywhere it is hard for the maintainer to be the judge.
How can we tell this is not just one person's nitpick? Will this increase adoption for the project?
Is it really worth changing the code and maintaining this?

Please use our GitHub feature issue template to propose features as they somewhat you guide towards a solid report.

Another reason why stylistic request are extremely frustrating is that the requester almost every time has no idea how Fantomas internally works.
Meaning they have no real idea what they are asking on a technical level and what the impact truly is.
As end-users everybody feels entitled to discuss stylistic matters, yet in reality few people understand how things work.<br />
The best helmsman stand on shore and all these conversations are extremely demotivating knowing the tool has bigger issues than style.

Still here? Great! It can definitely happen that you have a suggestion that just doesn't fit in any style guide.
Be very critical and perhaps do an experiment in the code first to see if your suggestion is worth having.
If a feature can help in the projects adoption and growth we are still happy to discuss it.

# Getting started with Fantomas

Fantomas has fairly straightforward setup.

> git clone https://github.com/yourfork/fantomas

After cloning the repository you can restore the local .NET tools

> dotnet tool restore

Next you can execute FAKE build targets.

> dotnet fake run build.fsx --list

The default target will execute the CI build.

# Pull request ground rules

Code changes in pull requests will have a certain set of expectations.
In general, changes should be made as consistent to the current code base as possible.
Don't introduce unnecessary new concepts and try and change as little as code possible to achieve your goal.

Always start with the mindset that you are going to introduce a change that might have an impact on how the tool behaves.
Capture this change first in a unit test. Set your expectations in the assert part of the test before touching anymore.
This project is very well suited for [Test-driven development](https://en.wikipedia.org/wiki/Test-driven_development) and that should be the goal.

Typical unit test template:

```fsharp
[<Test>]
let ``my new test`` () =
    formatSourceString false """
let myInput =     42
"""  config
    |> prepend newline
    |> should equal """
let myInput = 42
"""
```

The vast majority of the tests uses the template listed above. Only deviate from this when necessary.
Try and find a suiting file in `Fantomas.Tests` or introduce a new file.

A new test file should look like:

```fsharp
module Fantomas.Tests.MyNewConceptTests

open NUnit.Framework
open FsUnit
open Fantomas.Tests.TestHelper

// add tests here...
```

Filename: `MyNewConceptTests.fs`.

When developing a new feature, add new tests to cover all code paths.

## Guidelines

- Unit tests should start with a `lowercase` letter.
- Verify if the change you are making also should apply to signature files (*.fsi).
- Check if you need additional tests to cope with a different combination of settings.
- Check if you need additional tests to cope with a different combination of defines (`#if DEBUG`, ...).
- When creating a test that is linked to a GitHub issue, add the number at the back with a comma f.ex.
- Write/update documentation when necessary.

```fsharp
[<Test>]
let ``preserve compile directive between piped functions, 512`` () = ...
```

### Small steps

Please ask yourself before contributing if you received a clear signal to proceed.
This is too avoid frustrations with large pull requests that were never discussed.
It is better to create a draft pull request with some initial small changes and engage conversation.

# Your First Contribution

> “It's a dangerous business, Frodo, going out of your door," he used to say. "You step into the Road, and if you don't keep your feet, there is no knowing where you might be swept off to. ― J.R.R. Tolkien, The Fellowship of the Ring

Fantomas is project that has its root deeply nested in the F# compiler. This can be an overwhelming experience at first and might even hold you back to contribute in the first place.
Fear not, once you get the hang of it, things are less complicated than they seem.

In short, Fantomas is a source code to source code compiler. It will transform the text in the source code to an intermediate format and transform that again to source code.
It uses the [F# Compiler Services](https://fsharp.github.io/FSharp.Compiler.Service/index.html) to do this. The F# compiler will be used to create an [UnTyped Abstract Syntax](https://fsharp.github.io/FSharp.Compiler.Service/reference/fsharp-compiler-syntaxtree.html) (or AST) tree.
The AST tree is then reprinted in `CodePrinter.fs`. Once the whole tree is traversed, the formatted code can be constructed.

There is a catch though. The AST does not have all the original information the was once in the source code. 
Things like code comments for example are not present in the tree, we call these things `trivia`.
Fantomas uses another service of the FCS to detect these kinds of `trivia` namely the tokenizer.
All tokens are processed to find trivia, then trivia are assigned to trivia nodes.
Trivia nodes are linked to an existing AST node or a F# token. They serve as a marker to indicate where in `CodePrinter.fs` the trivia needs to be added to the formatted code.

## YouTube video series

In order to better explain how everything works, you can watch a [YouTube video series](https://www.youtube.com/playlist?list=PLvw_J2kfZCX3Mf6tEbIPZXbzJOD1VGl4K) on how Fantomas internally works.
This is highly recommended for anybody new to the code base.

## Tools

In order to improve the developer experience when working on Fantomas a set of tools were created:

- F# AST Viewer: https://fsprojects.github.io/fantomas-tools/#/ast
- F# Tokens: https://fsprojects.github.io/fantomas-tools/#/tokens
- Fantomas Online: https://fsprojects.github.io/fantomas-tools/#/fantomas/preview
- Trivia viewer: https://fsprojects.github.io/fantomas-tools/#/trivia

These can you insights in how Fantomas is processing the input code.

### Upgrading FSharp.Compiler.Service

When upgrading the [FSharp.Compiler.Service](https://www.nuget.org/packages/FSharp.Compiler.Service/), you need to juggle a bit between [fantomas](https://github.com/fsprojects/fantomas) and [fantomas-tools](https://github.com/fsprojects/fantomas-tools).
The order:
- Update fantomas
- Update fantomas-tools, `group Server`

## Tips and tricks

Easy bugs are labelled with `low-hanging-fruit`. 
Let's explore some common problems:

### Trivia bug example

One common re-occuring bug is that Fantomas is not preserving a code comment.
Now why is that? Well the process to restore these comments is somewhat complex because that information is lacking in the AST.

When faced with this issue, ask yourself the following questions:
- Was the comment detected in the tokenize phase? Can it be found in the trivia tab?
- Was the trivia assigned to a trivia node?
- Is the trivia being printed in `CodePrinter.fs` when the trivia node is processed?

Without any debugging, these questions can already point you in the right direction to solve the problem.
You might be surprised how little code changes were necessary to resolve the bug ;).

### Repeating newline bug

Another common problem is that Fantomas can add an extra unexpected blank line after formatting.
This is another trivia bug. Newlines are not part of the AST and are detected via trivia.
Fantomas could potentially add a newline because of the current flow, and add an additional newline by printing the trivia.

Use a helper function like `sepNlnConsideringTriviaContentBeforeForMainNode` instead of `sepNln` in `CodePrinter.fs` to solve this.

### Ionide

When you want to contribute to this project in VSCode with Ionide, there  is a trick you need to know to debug Unit tests.

After checking out the repository, open a terminal and set the `VSTEST_HOST_DEBUG` environment variable to `1`.

In PowerShell:

> $env:VSTEST_HOST_DEBUG=1

or in Bash:

> VSTEST_HOST_DEBUG=1

Run a single unit test with `dotnet test --filter`.

> cd .\src\Fantomas.Tests\
> dotnet test --filter "record declaration"

The output looks like:

```
Test run for C:\Temp\fantomas\src\Fantomas.Tests\bin\Debug\netcoreapp3.1\Fantomas.Tests.dll(.NETCoreApp,Version=v3.1)
Microsoft (R) Test Execution Command Line Tool Version 16.3.0
Copyright (c) Microsoft Corporation.  All rights reserved.

Starting test execution, please wait...

A total of 1 test files matched the specified pattern.
Host debugging is enabled. Please attach debugger to testhost process to continue.
Process Id: 20312, Name: dotnet
```

And we can now attach to the unit testing process.

![Run .NET Core Attach ](./docs/fantomas-debug-vscode-1.png)

![Choose process id](./docs/fantomas-debug-vscode-2.png)

**Press the play button once the process has been chosen!**
This might be a bit strange but you need to press play in order for the debugger to start working.

![Hit the breakpoint](./docs/fantomas-debug-vscode-3.png)

Check out this [video fragment](https://youtu.be/axHIazqiO9E?t=65) to see this in action.

# Bug reports

We prefer that all bugs are created using our online tool: https://fsprojects.github.io/fantomas-tools/ <br />
Here we can easily report bugs against the code of the latest master branch.
The tool generates a report with all the technical information that is necessary to reproduce the bug in a unit test.

Please try and make the bug report as small as possible. Isolate that part that is causing the bug.

## Related issues

Be careful before claiming that a bug is related to another issue or is a duplicate.
Only maintainers can truly verify this as they know the code base.
What might look related to you, could still very well be separate issues.
**Please always create a new issue and let the maintainers decide.**


# Community

There is a [Gitter](https://gitter.im/fsprojects/fantomas) channel or you can try the `#editor_support` channel in the [F# Foundation Slack](https://fsharp.slack.com).

## Talks

- [JetBrains .NET Days Online 2020](https://www.youtube.com/watch?v=9kK57hMDLvU)
- [fsharpConf 2020](https://youtu.be/ybkYHYKYeNw?t=4482)
- [.NET Summit 2020](https://www.youtube.com/watch?v=DiRYHD-HiF8)

