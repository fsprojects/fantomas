---
category: Contributors
categoryindex: 2
index: 1
---
# Contributors

> “It's a dangerous business, Frodo, going out of your door," he used to say. "You step into the Road, and if you don't keep your feet, there is no knowing where you might be swept off to. ― J.R.R. Tolkien, The Fellowship of the Ring

Fantomas is a project that has its roots deeply nested in the F# compiler. This can be an overwhelming experience at first, and it might even make you nervous about contributing in the first place.
Fear not: once you get the hang of it, things are less complicated than they seem.

In short, Fantomas is a source-code-to-source-code compiler. It will transform the text in the source code to an intermediate format and transform that again to source code.
It uses the F# Compiler to do this. The parser from the F# compiler will be used to create an [UnTyped Abstract Syntax](https://fsharp.github.io/FSharp.Compiler.Service/reference/fsharp-compiler-syntaxtree.html) tree (or "AST").
The AST is then reprinted in `CodePrinter.fs`: once the whole tree is traversed, the formatted code can be constructed.

In this section of our documentation, we wish to teach you everything you need to know to contribute to Fantomas.
Every F# developer should be able to understand the project, even the ones new to the language.
The best is yet to come!

PS: Don't hesitate to open [an issue](https://github.com/fsprojects/fantomas/issues/new/choose) if you have any questions. 
Or if something isn't all that clear. Our goal is to make this documentation as complete as possible🎉!

## Visual Studio Issue

There is currently [a known issue](https://github.com/fsprojects/fantomas/issues/2447) when loading the `fantomas` solution in Visual Studio that prevents the solution from being able to build properly. This is due to an issue in the `Ionide.KeepAChangelog` build step (tracking the issue [here](https://github.com/ionide/KeepAChangelog/issues/8)).

To workaround this in the meantime, you'll need to comment out the references to `Ionide.KeepAChangelog.Tasks` in `Directory.Build.props` and all of the `packages.lock.json` files, and then run `dotnet restore`. **Please be careful not to include these changes when submitting a PR!**

<fantomas-nav next="{{fsdocs-next-page-link}}"></fantomas-nav>
