---
category: Contributors
categoryindex: 2
index: 1000
---
# Glossary

## AST

The [Abstract Syntax Tree](https://en.wikipedia.org/wiki/Abstract_syntax_tree) (AST) is a tree data [structure](https://github.com/dotnet/fsharp/blob/main/src/Compiler/SyntaxTree/SyntaxTree.fs) representing a piece of source code.

## FCS

[FSharp.Compiler.Service](https://fsharp.github.io/fsharp-compiler-docs/fcs/#FSharp-Compiler-Service) (FCS) is a collection of APIs and Services derived from the F# compiler source code.

## PR

A [Pull Request](https://docs.github.com/en/pull-requests) (PR) is a unit of proposed changes to a version control repository. See [here](./Pull%20request%20ground%20rules.html) for Fantomas-specific rules.

## Range

A data structure modeling the exact place and size of a node or language construct in the source code.

## Style Guide

The [F# Style Guide](https://learn.microsoft.com/en-us/dotnet/fsharp/style-guide/). The set of formatting rules Fantomas uses.

## Syntax Node

A node in the AST.

## Trivia

Used to label items (blank lines, code comments, conditional directives) that are not fully captured by the F# compiler in the AST.

## Trivia Node

A node in a special tree structure composed to help Fantomas print code comments.

## Untyped Syntax Tree

The AST from the FCS used by Fantomas.

<fantomas-nav previous="./Pull%20request%20ground%20rules.html"></fantomas-nav>
