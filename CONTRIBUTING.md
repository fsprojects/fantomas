# Introduction

Well hello there, my dear reader! Thank you for considering contributing to the Fantomas project!<br />
Fantomas is a bit of a complex project and it can be overwhelming to just dive in.
In this guide, we will try to get you started and express our expectations in terms of potential contributions.<br />
We hope to give you proper context about fixing bugs, expressing new ideas and the technical aspects.

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

In [CodePrinter.fs](https://github.com/fsprojects/fantomas/blob/main/src/Fantomas/CodePrinter.fs) the ASTContext record is used to indicate context aware information. This usually is an escape hatch and should be avoided at all times.
The key issue is that flags of the ASTContext are usually not cleaned up after they served their purpose.
Leading to very strange situations and unexpected behavior.

# Bug reports

We prefer that all bugs are created using our online tool: https://fsprojects.github.io/fantomas-tools/ <br />
Here we can easily report bugs against the code of the latest main branch.
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

