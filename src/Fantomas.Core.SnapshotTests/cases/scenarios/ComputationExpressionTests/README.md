# ComputationExpressionTests

These cases were the tests of [`src/Fantomas.Core.Tests/ComputationExpressionTests.fs`](https://github.com/fsprojects/fantomas/blob/8bf81a121bf52e557b12e710f24a922c172a7ed5/src/Fantomas.Core.Tests/ComputationExpressionTests.fs). The comments that file had around its tests say what the tests alone do not, and they are below as they were written, each with the cases it was written above or inside. The [scenarios README](../README.md) says how to read them.

## and-cannot-have-an-in-keyword

In older versions of F# the parser would allow an in keyword after and! However, according to the F# language specification, it is not allowed.

Written above:

- [`and-cannot-have-an-in-keyword.fs`](and-cannot-have-an-in-keyword.fs)
