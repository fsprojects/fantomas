# Introduction

Well hello there, my dear reader! Thank you for considering contributing to the Fantomas project!<br />
Fantomas is a bit of a complex project and it can be overwhelming to just dive in.
In this guide, we will try to get you started and express our expectations in terms of potential contributions.<br />
We hope to give you proper context about fixing bugs, expressing new ideas and the technical aspects.

## What are we looking for?

### Bug fixes

The most welcome additions to the project are bug fixes. The philosophy behind this is that everyone should be able to fix their own bug.
If we can achieve this as a community, we can share the workload and all benefit together.
The project can move at a faster pace and improve as a whole.

We strongly encourage people to embrace the reward of solving their own problems.
We'll ask for a regression test when you fix a bug, to guarantee that you won't encounter the bug again.

#### bug (soundness)

Our goal is for Fantomas to be able to format all files out of the box without breaking correctness.
It's very important to us that a new user's experience is smooth and at the very least results in correct code.
Bugs labelled `bug (soundness)` all indicate places where a new user might bounce off Fantomas because it actually broke their code.
We want to make sure users get a chance to explore the settings and tweak the style.
If you can help us out by fixing a soundness bug, you can really help the project move forward.

#### bug (stylistic)

Besides breaking correctness another kind of bug is that the style of the output might not be what you expect.
Bugs like this are labelled as `bug (stylistic)`.
This includes cases where Fantomas breaks one of its own formatting rules or fails to respect one of its settings.

Again, here: scratch your own itch. If something bothers you, the best cure is to try a take a stab at it yourself.

### Adoption

The dream is that every F# developer can use Fantomas at any time.
This aspiration is an odyssey that might never be complete, but any step in that direction is most welcome.
Try introducing automatic formatting in your project, at work, or in an open-source project.
This tool will only improve by adoption.

#### Sponsoring

Fantomas grew significantly as a result of its first sponsorship deal with [G-Research](https://www.gresearch.co.uk/).
It would still be in the dark ages if it weren't for this support. For that we will forever be grateful.
If you want to help increase adoption by providing financial support, you can reach out to [sponsoring@fantomas.io](mailto:sponsoring@fantomas.io).

## What are we not looking for?

### Stylistic features

Fantomas tries to adhere to two F# style guides:
- Microsoft: https://docs.microsoft.com/en-us/dotnet/fsharp/style-guide/formatting
- G-Research: https://github.com/G-Research/fsharp-formatting-conventions

Please o please, discuss everything related to code style on those repositories; the Fantomas repo is not the place to discuss whether one style is better than another.
Can't find anything mentioned on those repos? Open an issue there and **engage in conversation**.

To engage in conversation, please be aware of the following: by default Fantomas follows the Microsoft style guide. If you have a question about something that is missing, it should be raised in [fsharp/fslang-design](https://github.com/fsharp/fslang-design#style-guide).
In case something is missing in the G-Research guide, it is implied that they follow the guidance of the Microsoft style guide.

When a person opens a feature request that has not been discussed anywhere, it is hard for the maintainers of Fantomas to be the judge of style.
Will this feature increase adoption for the project?
Every feature carries an ongoing maintenance/support burden; are enough people going to want this feature that it's worth changing the code and taking on the maintenance?
So please do discuss style features on the style repos before requesting them here.

Please use our GitHub feature issue template to propose features, as they will help you create a solid bug report/feature request.

Bear in mind that stylistic requests can appear deceptively simple.
The internal structure of Fantomas might mean a particular stylistic change may actually be difficult to implement, or might have a wide impact on the Fantomas code base.
It's easy to discuss stylistic matters (the best helmsman stands on shore!), but please remember that your stylistic request might translate into a lot of work for someone else to do,
and we would always prefer to prioritise correctness issues when allocating our limited time.
The best cure is to have a go at making the change yourself!

Still here? Great! Sometimes you really do have a stylistic suggestion that just doesn't fit in any style guide.
Think carefully, and perhaps do an experiment in the Fantomas codebase first, to see how difficult your suggestion is going to be.
If a feature can help in the project's adoption and growth, we are happy to discuss it.

# Getting started with Fantomas

Fantomas has a fairly straightforward setup.

> git clone https://github.com/yourfork/fantomas

After cloning the repository, you can restore the local .NET tools:

> dotnet tool restore

Next, you should run a FAKE target that sets up some git repo-level configuration.

> dotnet fake build -t EnsureRepoConfig

This target makes changes to the local git repository configuration to ensure 
that formatting of new code is consistent before it is pushed up to a remote repository.

Finally, you can execute FAKE build targets.

> dotnet fake run build.fsx --list

The default target will execute the CI build.

> dotnet fake build

# Pull request ground rules

We expect some things from code changes.
In general, changes should be made as consistent to the current code base as possible.
Don't introduce unnecessary new concepts and try and change as little code as possible to achieve your goal.

Always start with the mindset that you are going to introduce a change that might have an impact on how the tool behaves.
Capture this change first in a unit test. Set your expectations in the assert part of the test before touching anything.
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

The vast majority of the tests use the template listed above. Only deviate from this when necessary.
Try and find a suitable file in `Fantomas.Core.Tests`, or introduce a new file.

A new test file should look like:

```fsharp
module Fantomas.Core.Tests.MyNewConceptTests

open NUnit.Framework
open FsUnit
open Fantomas.Core.Tests.TestHelper

// add tests here...
```

Filename: `MyNewConceptTests.fs`.

When developing a new feature, add new tests to cover all code paths.

If you come across an issue, which can't be reproduced with the latest version of Fantomas but is still open, please submit a regression test.
That way, we can ensure the issue stays fixed after closing it.

## Guidelines

### Target branch

Please always rebase your code on the targeted branch.
To keep your fork up to date, run this command:
> git remote add upstream https://github.com/fsprojects/fantomas.git

Updating your fork:

> git checkout master && git fetch upstream && git rebase upstream/master && git push

### Unit test

- Unit test names should start with a lowercase letter.
- When creating a test that is linked to a GitHub issue, add the number at the back with a comma, as in the following:

```fsharp
[<Test>]
let ``preserve compile directive between piped functions, 512`` () = ...
```

### Verify signature files

Verify if the change you are making should also apply to signature files (`*.fsi`).

### Verify slight variations

- Check if you need additional tests to cope with a different combination of settings.
- Check if you need additional tests to cope with a different combination of defines (`#if DEBUG`, ...).

### Documentation

Write/update documentation when necessary.

### Pull request title

- Give your PR a meaningful title. Make sure it covers the change you are introducing in Fantomas.

    For example:
*"Fix bug 1404"* is a poor title as it does not tell the maintainers what changed in the codebase.<br />
*"Don't double unindent when record has an access modifier"* is better as it informs us what exactly has changed.
- Add a link to the issue you are solving by using [a keyword](https://docs.github.com/en/github/managing-your-work-on-github/linking-a-pull-request-to-an-issue#linking-a-pull-request-to-an-issue-using-a-keyword) in the PR description.<br />
*"Fixes #1404"* does the trick quite well.
- Not mandatory, but when fixing a bug consider using `fix-<issue-number>` as the git branch name.<br />
For example, `git checkout -b fix-1404`.

### Format your changes

- Code should be formatted to our standard style, using either `dotnet fake run build.fsx -t Format` which works on all files, or
  `dotnet fake run build.fsx -t FormatChanged` to just change the files in git.
  - If you forget, there's a git `pre-push` script that will run this for you, make sure to run `dotnet fake build -t EnsureRepoConfig` to set that hook up.

### Changelog

- Add an entry to the `CHANGELOG.md` in the `Unreleased` section based on what kind of change your change is. Follow the guidelines at [KeepAChangelog](https://keepachangelog.com/en/1.0.0/#how) to make your message relevant to future readers.
  - If you're not sure what Changelog section your change belongs to, start with `Changed` and ask for clarification in your Pull Request
  - If there's not an `Unreleased` section in the `CHANGELOG.md`, create one at the top above the most recent version like so:
  
    ```markdown
    ## [Unreleased]

    ### Changed
    * Your new feature goes here

    ## [4.7.4] - 2022-02-10

    ### Added
    * Awesome feature number one
    ```

  - When fixing a `bug (soundness)`, add a line in the following format to `Fixed`:
    `* <Original GitHub issue title> [#issue-number](https://github.com/fsprojects/fantomas/issues/issue-number)`.
    For example, `* Spaces are lost in multi range expression. [#2071](https://github.com/fsprojects/fantomas/issues/2071)`.
    Do the same, if you fixed a `bug (stylistic)` that is not related to any style guide.
  - When fixing a `bug (stylistic)`, add a line in the following format to `Changed`
    `Update style of xyz. [#issue-number](https://github.com/fsprojects/fantomas/issues/issue-number)`
  - For example, `* Update style of lambda argument. [#1871](https://github.com/fsprojects/fantomas/issues/1871)`.

### Run a local build

Finally, make sure to run `dotnet fake build`. Among other things, this will check the format of the code and will tell you, if
 your changes caused any tests to fail.

### Small steps

It is better to create a draft pull request with some initial small changes, and engage conversation, than to spend a lot of effort on a large pull request that was never discussed.
Someone might be able to warn you in advance that your change will have wide implications for the rest of Fantomas, or might be able to point you in the right direction.
However, this can only happen if you discuss your proposed changes early and often.
It's often better to check *before* contributing that you're setting off on the right path.

## Fixing style guide inconsistencies

Fantomas tries to keep up with the style guides, but as these are living documents, it can occur that something is listed in the style that Fantomas is not respecting.
In this case, please create an issue using our [online tool](https://fsprojects.github.io/fantomas-tools/#/). 
Copy the code snippet from the guide and add a link to the section of the guide that is not being respected. 
The maintainers will then add the `bug (stylistic)` to confirm the bug is fixable in Fantomas. In most cases, it may seem obvious that the case can be fixed.
However, in the past there have been changes to the style guide that Fantomas could not implement for technical reasons: Fantomas can only implement rules based on information entirely contained within the untyped syntax tree.

### Target the next minor or major branch

When fixing a stylistic issue, please ask the maintainers what branch should be targeted. The rule of thumb is that the `master` branch is used for fixing `bug (soundness)` and will be used for revision releases.
Strive to ensure that end users can always update to the latest patch revision of their current minor or major without fear.

A user should only need to deal with style changes when they have explicitly [chosen to upgrade](https://github.com/fsprojects/fantomas/blob/master/docs/Documentation.md#updating-to-a-new-fantomas-version) to a new minor or major version.
In case no major or minor branch was created yet, please reach out to the maintainers. 
The maintainers will frequently rebase this branch on top of the master branch and release alpha/beta packages accordingly.

# Your First Contribution

> “It's a dangerous business, Frodo, going out of your door," he used to say. "You step into the Road, and if you don't keep your feet, there is no knowing where you might be swept off to. ― J.R.R. Tolkien, The Fellowship of the Ring

Fantomas is a project that has its roots deeply nested in the F# compiler. This can be an overwhelming experience at first, and it might even make you nervous about contributing in the first place.
Fear not: once you get the hang of it, things are less complicated than they seem.

In short, Fantomas is a source-code-to-source-code compiler. It will transform the text in the source code to an intermediate format and transform that again to source code.
It uses the [F# Compiler Services](https://fsharp.github.io/FSharp.Compiler.Service/index.html) to do this. The F# compiler will be used to create an [UnTyped Abstract Syntax](https://fsharp.github.io/FSharp.Compiler.Service/reference/fsharp-compiler-syntaxtree.html) tree (or "AST").
The AST is then reprinted in `CodePrinter.fs`: once the whole tree is traversed, the formatted code can be constructed.

There is a catch, though. The AST does not have all the original information that was once in the source code.
Things like code comments, for example, are not present in the tree given to us by the F# compiler.
We call these pieces of information "trivia".
Fantomas uses another part of the F# Compiler Services to detect trivia: namely, the tokenizer.
All tokens are processed to find trivia, then trivia are assigned to "trivia nodes" (a concept of Fantomas, not of the F# compiler).
Trivia nodes are linked to an existing AST node or an F# token. They serve as a marker to indicate where in `CodePrinter.fs` the trivia needs to be added to the formatted code.

## YouTube video series

In order to better understand how everything works, you can watch a [YouTube video series](https://www.youtube.com/playlist?list=PLvw_J2kfZCX3Mf6tEbIPZXbzJOD1VGl4K) on the internals of Fantomas.
This is highly recommended for anybody new to the codebase.

## Tools

In order to improve the developer experience when working on Fantomas, several tools were created:

- F# AST Viewer: https://fsprojects.github.io/fantomas-tools/#/ast
- F# Tokens: https://fsprojects.github.io/fantomas-tools/#/tokens
- Fantomas Online: https://fsprojects.github.io/fantomas-tools/#/fantomas/preview
- Trivia viewer: https://fsprojects.github.io/fantomas-tools/#/trivia

These can give you insights into how Fantomas is processing the input code, and the first step in fixing a bug is usually to run one or more of these tools.

### Upgrading FSharp.Compiler.Service

When upgrading the [FSharp.Compiler.Service](https://www.nuget.org/packages/FSharp.Compiler.Service/), you need to juggle a bit between [fantomas](https://github.com/fsprojects/fantomas) and [fantomas-tools](https://github.com/fsprojects/fantomas-tools).
The order:
- Update fantomas
- Update fantomas-tools, `group Server`

## Tips and tricks

Some easier bugs are labelled with `low-hanging-fruit`.

Often, fixing a bug will involve using one of only a handful of techniques.
Let's explore some common kinds of bug, whose solutions often follow the same process.

### Trivia bug example

One common re-occuring bug is that Fantomas has failed to preserve a code comment.
Now why is that? Well, comments are a piece of trivia (the AST contains no information about comments), so Fantomas has to do some work to restore them.

When faced with this issue, ask yourself the following questions:
- Was the comment detected in the tokenize phase? Can it be found in the trivia viewer?
- Was the trivia assigned to a trivia node?
- Is the trivia being printed in `CodePrinter.fs` when the trivia node is processed?

Without any debugging, these questions can already point you in the right direction to solve the problem.
You might be surprised how little code change was necessary to resolve the bug ;) See, for example, [#1130](https://github.com/fsprojects/fantomas/pull/1130), which solved a problem like this in only five lines.

### Repeating newline bug

Another common problem is that Fantomas can add an extra unexpected blank line after formatting.
This is another trivia bug. Newlines are not part of the AST and are instead detected via trivia.
Often this kind of bug is because Fantomas has added a newline due to some formatting rule, but then has added an additional newline when it subsequently printed out the trivia.

Use a helper function like `sepNlnConsideringTriviaContentBeforeForMainNode` instead of `sepNln` in `CodePrinter.fs` to solve this.

### ASTContext

In [CodePrinter.fs](https://github.com/fsprojects/fantomas/blob/master/src/Fantomas/CodePrinter.fs) the ASTContext record is used to indicate context aware information. This usually is an escape hatch and should be avoided at all times.
The key issue is that flags of the ASTContext are usually not cleaned up after they served their purpose.
Leading to very strange situations and unexpected behavior.

## Rider

The core contributors of this project are using JetBrains Rider. Running and debugging unit tests works out of the box and no additional plugins are needed.

## Ionide

When you want to contribute to this project in VSCode with Ionide, there is a trick you need to know to debug Unit tests.

After checking out the repository, open a terminal and set the `VSTEST_HOST_DEBUG` environment variable to `1`.

In PowerShell:

> $env:VSTEST_HOST_DEBUG=1

or in Bash:

> export VSTEST_HOST_DEBUG=1

Run a single unit test with `dotnet test --filter`.

> cd .\src\Fantomas.Core.Tests\
> dotnet test --filter 1700

The output looks like:

```
Test run for C:\Temp\fantomas\src\Fantomas.Core.Tests\bin\Debug\netcoreapp3.1\Fantomas.Tests.dll(.NETCoreApp,Version=v3.1)
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

Please try and make the bug report as small as possible. Isolate the part of your code that is causing the bug.

## Related issues

Be careful before claiming that a bug is related to another issue or is a duplicate.
If you don't know the Fantomas code base, you might find that issues which look very similar actually have very different causes.
**Please always create a new issue and let the maintainers decide.**
The maintainers know the code and will be able to tell.

# Community

There is a [Discord](https://discord.gg/D5QXvQrBVa) server or you can try the `#editor_support` channel in the [F# Foundation Slack](https://fsharp.slack.com).

## Talks

- [JetBrains .NET Days Online 2020](https://www.youtube.com/watch?v=9kK57hMDLvU)
- [fsharpConf 2020](https://youtu.be/ybkYHYKYeNw?t=4482)
- [.NET Summit 2020](https://www.youtube.com/watch?v=DiRYHD-HiF8)

