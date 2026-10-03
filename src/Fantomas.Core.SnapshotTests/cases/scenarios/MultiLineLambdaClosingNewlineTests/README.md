# MultiLineLambdaClosingNewlineTests

These cases were the tests of [`src/Fantomas.Core.Tests/MultiLineLambdaClosingNewlineTests.fs`](https://github.com/fsprojects/fantomas/blob/8bf81a121bf52e557b12e710f24a922c172a7ed5/src/Fantomas.Core.Tests/MultiLineLambdaClosingNewlineTests.fs). The comments that file had around its tests say what the tests alone do not, and they are below as they were written, each with the cases it was written above or inside. The [scenarios README](../README.md) says how to read them.

## lambda-moves-to-its-own-line-when-everything-up-to-the-arrow

The two tests below repeat the chain cases from ChainFormattingTests with this setting on. A call reached through a dot is laid out like the same call without one, whatever the setting says about the closing parenthesis.

Written above these cases:

- [`lambda-moves-to-its-own-line-when-everything-up-to-the-arrow.fs`](lambda-moves-to-its-own-line-when-everything-up-to-the-arrow.fs)
- [`lambda-parameters-take-a-line-each-when-they-do-not-fit.fs`](lambda-parameters-take-a-line-each-when-they-do-not-fit.fs)
