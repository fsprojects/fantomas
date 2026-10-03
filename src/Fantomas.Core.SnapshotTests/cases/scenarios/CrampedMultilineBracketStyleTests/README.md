# CrampedMultilineBracketStyleTests

These cases were the tests of [`src/Fantomas.Core.Tests/CrampedMultilineBracketStyleTests.fs`](https://github.com/fsprojects/fantomas/blob/8bf81a121bf52e557b12e710f24a922c172a7ed5/src/Fantomas.Core.Tests/CrampedMultilineBracketStyleTests.fs). The comments that file had around its tests say what the tests alone do not, and they are below as they were written, each with the cases it was written above or inside. The [scenarios README](../README.md) says how to read them.

## should-not-break-inside-of-if-statements-in-records

the current behavior results in a compile error since the if is not aligned properly

Written above:

- [`should-not-break-inside-of-if-statements-in-records.fs`](should-not-break-inside-of-if-statements-in-records.fs)

## short-record-and-let-binding

This test ensures that the normal flow of Fantomas is resumed when the next expression is being written.

Written above:

- [`short-record-and-let-binding.fs`](short-record-and-let-binding.fs)
