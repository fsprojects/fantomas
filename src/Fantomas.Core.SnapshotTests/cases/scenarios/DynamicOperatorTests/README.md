# DynamicOperatorTests

These cases were the tests of [`src/Fantomas.Core.Tests/DynamicOperatorTests.fs`](https://github.com/fsprojects/fantomas/blob/8bf81a121bf52e557b12e710f24a922c172a7ed5/src/Fantomas.Core.Tests/DynamicOperatorTests.fs). The comments that file had around its tests say what the tests alone do not, and they are below as they were written, each with the cases it was written above or inside. The [scenarios README](../README.md) says how to read them.

## 3088-case-determination-issue-with-exprappsingleparenargnode-2

Space before paren args of a `?` result is never added, regardless of SpaceBefore(Upper|Lower)caseInvocation. Adding a space changes the AST when followed by another `?`, e.g. `X?a ("arg")?B`. See #3159.

Written inside:

- [`3088-case-determination-issue-with-exprappsingleparenargnode-2.fs`](negative/3088-case-determination-issue-with-exprappsingleparenargnode-2.fs)

## dynamic-operator-with-a-lambda-argument

A lambda or match-lambda argument is NOT fused into the `?` chain: unlike a plain argument (`obj?y?z (a)`, where the argument ends up inside the chain item), it stays a normal application so the argument can use the ordinary multiline lambda layout.

Written above these cases:

- [`dynamic-operator-with-a-lambda-argument.fs`](negative/dynamic-operator-with-a-lambda-argument.fs)
- [`dynamic-operator-with-a-match-lambda-argument.fs`](dynamic-operator-with-a-match-lambda-argument.fs)
- [`dynamic-chain-with-a-lambda-argument.fs`](negative/dynamic-chain-with-a-lambda-argument.fs)
