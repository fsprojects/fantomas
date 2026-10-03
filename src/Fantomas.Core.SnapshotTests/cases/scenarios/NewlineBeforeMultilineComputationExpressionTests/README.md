# NewlineBeforeMultilineComputationExpressionTests

These cases were the tests of [`src/Fantomas.Core.Tests/NewlineBeforeMultilineComputationExpressionTests.fs`](https://github.com/fsprojects/fantomas/blob/8bf81a121bf52e557b12e710f24a922c172a7ed5/src/Fantomas.Core.Tests/NewlineBeforeMultilineComputationExpressionTests.fs). The comments that file had around its tests say what the tests alone do not, and they are below as they were written, each with the cases it was written above or inside. The [scenarios README](../README.md) says how to read them.

## 3155-multiline-name-expression-falls-back-to-indent-and-newline

When NewlineBeforeMultilineComputationExpression = false, if the builder invocation's argument list wraps to multiple lines the closing ')' would end up at the global indent level (violating the offside rule for the let binding).  Fantomas should fall back to the indent+newline form to produce valid code.

Written inside:

- [`3155-multiline-name-expression-falls-back-to-indent-and-newline.fs`](3155-multiline-name-expression-falls-back-to-indent-and-newline.fs)
