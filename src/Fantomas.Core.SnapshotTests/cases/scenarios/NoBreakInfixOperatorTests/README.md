# NoBreakInfixOperatorTests

These cases were the tests of [`src/Fantomas.Core.Tests/NoBreakInfixOperatorTests.fs`](https://github.com/fsprojects/fantomas/blob/8bf81a121bf52e557b12e710f24a922c172a7ed5/src/Fantomas.Core.Tests/NoBreakInfixOperatorTests.fs). The comments that file had around its tests say what the tests alone do not, and they are below as they were written, each with the cases it was written above or inside. The [scenarios README](../README.md) says how to read them.

## At the top of the file

`=`, `>`, `<`, `%` and `%%` cannot start a line at the column of the left-hand side: there the
parser reads `=`, `>` and `<` as the `=` of a binding, and reads `%` and `%%` inside a quotation
as a splice. So the operator either ends the line the left-hand side is on, or takes a line of
its own one level in. Working down from the shortest case:

    1. everything fits           lhs op rhs

    2. the rhs does not fit      lhs op
                                     rhs

    3. the lhs spans lines       lhs
                                     op
                                     rhs

The third is what both style guides ask for in a long function signature, where the parameters
are indented one level and the `=` takes a line of its own before the body. Under
`fsharp_multiline_bracket_style = stroustrup` a right-hand side that opens a bracket keeps
hugging the operator, which outranks the third case.

Written above these cases:

- [`keeps-the-whole-expression-on-one-line-when-it-fits.fs`](negative/keeps-the-whole-expression-on-one-line-when-it-fits.fs)
- [`moves-the-right-hand-side-down-one-level-when-it-does-not.fs`](moves-the-right-hand-side-down-one-level-when-it-does-not.fs)
- [`takes-a-line-of-its-own-when-the-left-hand-side-is-multiline.fs`](takes-a-line-of-its-own-when-the-left-hand-side-is-multiline.fs)
- [`keeps-the-whole-expression-on-one-line-when-it-fits-2.fs`](negative/keeps-the-whole-expression-on-one-line-when-it-fits-2.fs)
- [`moves-the-right-hand-side-down-one-level-when-it-does-not-2.fs`](moves-the-right-hand-side-down-one-level-when-it-does-not-2.fs)
- [`takes-a-line-of-its-own-when-the-left-hand-side-is-multiline-2.fs`](takes-a-line-of-its-own-when-the-left-hand-side-is-multiline-2.fs)
- [`keeps-the-whole-expression-on-one-line-when-it-fits-3.fs`](negative/keeps-the-whole-expression-on-one-line-when-it-fits-3.fs)
- [`moves-the-right-hand-side-down-one-level-when-it-does-not-3.fs`](moves-the-right-hand-side-down-one-level-when-it-does-not-3.fs)
- [`takes-a-line-of-its-own-when-the-left-hand-side-is-multiline-3.fs`](takes-a-line-of-its-own-when-the-left-hand-side-is-multiline-3.fs)
- [`keeps-the-whole-expression-on-one-line-when-it-fits-4.fs`](negative/keeps-the-whole-expression-on-one-line-when-it-fits-4.fs)
- [`moves-the-right-hand-side-down-one-level-when-it-does-not-4.fs`](moves-the-right-hand-side-down-one-level-when-it-does-not-4.fs)
- [`takes-a-line-of-its-own-when-the-left-hand-side-is-multiline-4.fs`](takes-a-line-of-its-own-when-the-left-hand-side-is-multiline-4.fs)
- [`keeps-the-whole-expression-on-one-line-when-it-fits-5.fs`](negative/keeps-the-whole-expression-on-one-line-when-it-fits-5.fs)
- [`moves-the-right-hand-side-down-one-level-when-it-does-not-5.fs`](moves-the-right-hand-side-down-one-level-when-it-does-not-5.fs)
- [`takes-a-line-of-its-own-when-the-left-hand-side-is-multiline-5.fs`](takes-a-line-of-its-own-when-the-left-hand-side-is-multiline-5.fs)
- [`a-right-hand-side-that-opens-a-bracket-starts-at-the-same.fs`](a-right-hand-side-that-opens-a-bracket-starts-at-the-same.fs)
- [`a-match-on-the-right-hand-side-does-not-follow-the-left-hand.fs`](a-match-on-the-right-hand-side-does-not-follow-the-left-hand.fs)
- [`the-name-on-the-left-does-not-decide-where-the-right-hand.fs`](the-name-on-the-left-does-not-decide-where-the-right-hand.fs)
- [`both-sides-multiline-puts-the-operator-between-them.fs`](both-sides-multiline-puts-the-operator-between-them.fs)
- [`a-comment-in-front-of-the-right-hand-side-lands-below-the.fs`](negative/a-comment-in-front-of-the-right-hand-side-lands-below-the.fs)
- [`a-bracket-keeps-hugging-the-operator-under-stroustrup.fs`](a-bracket-keeps-hugging-the-operator-under-stroustrup.fs)
- [`the-stroustrup-hug-outranks-the-operator-taking-its-own-line.fs`](the-stroustrup-hug-outranks-the-operator-taking-its-own-line.fs)
- [`an-infix-percent-inside-a-quotation-does-not-become-a-splice.fs`](an-infix-percent-inside-a-quotation-does-not-become-a-splice.fs)
- [`3463-the-operator-takes-its-own-line-one-level-in-from-the-left.fs`](3463-the-operator-takes-its-own-line-one-level-in-from-the-left.fs)
