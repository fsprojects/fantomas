# AppTests

These cases were the tests of [`src/Fantomas.Core.Tests/AppTests.fs`](https://github.com/fsprojects/fantomas/blob/8bf81a121bf52e557b12e710f24a922c172a7ed5/src/Fantomas.Core.Tests/AppTests.fs). The comments that file had around its tests say what the tests alone do not, and they are below as they were written, each with the cases it was written above or inside. The [scenarios README](../README.md) says how to read them.

## 503-no-nln-before-lambda

the current behavior results in a compile error since the |> is merged to the last line

Written above:

- [`503-no-nln-before-lambda.fs`](503-no-nln-before-lambda.fs)

## 545-require-to-ident-at-least-1-after-function-name

compile error due to expression starting before the beginning of the function expression

Written above:

- [`545-require-to-ident-at-least-1-after-function-name.fs`](545-require-to-ident-at-least-1-after-function-name.fs)

## 545-require-to-ident-at-least-1-after-function-name-long

compile error due to expression starting before the beginning of the function expression

Written above:

- [`545-require-to-ident-at-least-1-after-function-name-long.fs`](545-require-to-ident-at-least-1-after-function-name-long.fs)
