# SignatureTests

These cases were the tests of [`src/Fantomas.Core.Tests/SignatureTests.fs`](https://github.com/fsprojects/fantomas/blob/8bf81a121bf52e557b12e710f24a922c172a7ed5/src/Fantomas.Core.Tests/SignatureTests.fs). The comments that file had around its tests say what the tests alone do not, and they are below as they were written, each with the cases it was written above or inside. The [scenarios README](../README.md) says how to read them.

## should-keep-the-string-string-list-type-signature-in-records

the current behavior results in a compile error since "(string * string) list" is converted to "string * string list"

Written above:

- [`should-keep-the-string-string-list-type-signature-in-records.fs`](should-keep-the-string-string-list-type-signature-in-records.fs)
