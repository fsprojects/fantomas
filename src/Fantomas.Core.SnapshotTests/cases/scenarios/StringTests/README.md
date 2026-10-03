# StringTests

These cases were the tests of [`src/Fantomas.Core.Tests/StringTests.fs`](https://github.com/fsprojects/fantomas/blob/8bf81a121bf52e557b12e710f24a922c172a7ed5/src/Fantomas.Core.Tests/StringTests.fs). The comments that file had around its tests say what the tests alone do not, and they are below as they were written, each with the cases it was written above or inside. The [scenarios README](../README.md) says how to read them.

## 2945-string-with-unicode-combining-characters-should-not-affect

Combining characters (e.g. U+036E, U+0312, U+036B) have no visual width of their own. Column tracking must use grapheme clusters, not UTF-16 code units.

Written inside:

- [`2945-string-with-unicode-combining-characters-should-not-affect.fs`](negative/2945-string-with-unicode-combining-characters-should-not-affect.fs)
