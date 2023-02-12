---
category: End-users
categoryindex: 1
index: 1000
---
# FAQ

## Why the name "Fantomas"?

There are a few reasons to choose the name as such.
First, it starts with an "F" just like many other F# projects.
Second, Fantomas is my favourite character in the literature.
Finally, Fantomas has the same Greek root as "[phantom](https://en.wiktionary.org/wiki/phantom)"; coincidentally F# ASTs and formatting rules are so *mysterious* to be handled correctly.

## What is `fantomas-tool`?

That is the previous name of the dotnet tool. `v4.7.9` was the last stable release under that name.  
Please use [fantomas](https://www.nuget.org/packages/fantomas/) instead and remove all traces of `fantomas-tool` in your `dotnet-tools.json` file.

## Why exit code 99 for a failed format check?

No real reason, it was [suggested](https://github.com/fsprojects/fantomas/pull/655#discussion_r374849907) by the contributor [lpedrosa](https://github.com/lpedrosa).  
It also reminds us of a certain Jay-Z song 😉.

## Can I make a style suggestion?

As mention in [style guide](https://fsprojects.github.io/fantomas/docs/end-users/StyleGuide.html), Fantomas adheres to [Microsoft](https://docs.microsoft.com/en-us/dotnet/fsharp/style-guide/formatting) and [G-Research](https://github.com/G-Research/fsharp-formatting-conventions) style guidelines.  
For any style related suggestion, please head over to [fsharp/fslang-design](https://github.com/fsharp/fslang-design#style-guide).  
[More context](https://fsprojects.github.io/fantomas/docs/end-users/StyleGuide.html#Default-style-guide).

## Is it safe to use the Alpha version of Fantomas?

Preview alpha versions are generally safe to use but there is no guarantee that the style wouldn't change due to ongoing development.  
You should check the [changelog](https://github.com/fsprojects/fantomas/blob/main/CONTRIBUTING.md) to see if there's any relevant change to try out in the Alpha.

<fantomas-nav previous="./UpgradeGuide.html"></fantomas-nav>
