# InterpolatedStringTests

These cases were the tests of [`src/Fantomas.Core.Tests/InterpolatedStringTests.fs`](https://github.com/fsprojects/fantomas/blob/8bf81a121bf52e557b12e710f24a922c172a7ed5/src/Fantomas.Core.Tests/InterpolatedStringTests.fs). The comments that file had around its tests say what the tests alone do not, and they are below as they were written, each with the cases it was written above or inside. The [scenarios README](../README.md) says how to read them.

## alignment-in-string-interpolation

The parser reports the alignment of `{x,10}` separately from the expression since dotnet/fsharp#19971. Fantomas has always printed a space after the comma, because the alignment used to arrive as part of a tuple expression. These pin that existing output.

Written above:

- [`alignment-in-string-interpolation.fs`](alignment-in-string-interpolation.fs)
