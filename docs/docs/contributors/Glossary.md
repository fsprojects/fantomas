---
category: Contributors
categoryindex: 2
index: 1000
---
# Glossary

## AST

The [Abstract Syntax Tree](https://en.wikipedia.org/wiki/Abstract_syntax_tree) (AST) is a tree data [structure](https://github.com/dotnet/fsharp/blob/main/src/Compiler/SyntaxTree/SyntaxTree.fsi) representing a piece of source code.

## FCS

[FSharp.Compiler.Service](https://fsharp.github.io/fsharp-compiler-docs/fcs/#FSharp-Compiler-Service) (FCS) is a collection of APIs and Services derived from the F# compiler source code.

## PR

A [Pull Request](https://docs.github.com/en/pull-requests) (PR) is a unit of proposed changes to a version control repository. See [here](./Pull%20request%20ground%20rules.html) for Fantomas-specific rules.

## Range

A data structure modeling the exact place and size of a node or language construct in the source code.  
A _range_ has a start and end position. A position is composed of a line number and a column number.

## Style Guide

The [F# Style Guide](https://learn.microsoft.com/en-us/dotnet/fsharp/style-guide/formatting). The set of formatting rules Fantomas implements.

## Syntax Node

A node in the AST. A node can represent different types of syntax, e.g. a Record or a Lambda.

## Trivia

_Trivia_ has two meanings, depending on the context it is used in.

### Fantomas

Used to label items (blank lines, code comments, conditional directives) that are not fully captured by the F# compiler in the AST.

### FSharp.Compiler.Service

Additional information captured in the [syntax tree](https://github.com/dotnet/fsharp/blob/main/src/Compiler/SyntaxTree/SyntaxTrivia.fsi), which the compiler does not need to compile the source code.

## Trivia Node

A _trivia node_ can contain _trivia_, either as content before or content after.
A _trivia node_ is a generalized type that serves as a common denominator for all AST node types.  
_Trivia nodes_ are used to construct a hierarchical tree-like structure in which every node can have multiple child nodes and each node has one parent node.

## Typed Syntax Tree

The AST from the FCS carrying typed information about the processed source code. It is contructed from the _Untyped Syntax Tree_.  
Fantomas does not use this AST to format the source code.

## Untyped Syntax Tree

The AST from the FCS used by Fantomas.
It represents the source code as it was processed by the F# compiler. The _Untyped Syntax Tree_ doesn't carry any information regarding the validity of the source code or semantics.
In a later compilation stage, the _Untyped Syntax Tree_ is transformed into the _Typed Syntax Tree_.

<fantomas-nav previous="./Pull%20request%20ground%20rules.html"></fantomas-nav>
