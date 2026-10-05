# OperatorTests

These cases were the tests of [`src/Fantomas.Core.Tests/OperatorTests.fs`](https://github.com/fsprojects/fantomas/blob/8bf81a121bf52e557b12e710f24a922c172a7ed5/src/Fantomas.Core.Tests/OperatorTests.fs). The comments that file had around its tests say what the tests alone do not, and they are below as they were written, each with the cases it was written above or inside. The [scenarios README](../README.md) says how to read them.

## should-break-on-operator-and-keep-indentation

the current behavior results in a compile error since line break is before the parens and not before the .

Written above:

- [`should-break-on-operator-and-keep-indentation.fs`](should-break-on-operator-and-keep-indentation.fs)

## list-on-the-right-of-a-no-break-infix-operator-cramped

A list or an array on the right of a no-break infix operator indents its items from the binding, not from the operator. `Cramped` lines the items up under the opening bracket instead and is unaffected either way. See 3428.

Written above:

- [`list-on-the-right-of-a-no-break-infix-operator-cramped.fs`](list-on-the-right-of-a-no-break-infix-operator-cramped.fs)
