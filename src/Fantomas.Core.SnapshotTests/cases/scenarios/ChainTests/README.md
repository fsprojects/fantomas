# ChainTests

These cases were the tests of [`src/Fantomas.Core.Tests/ChainTests.fs`](https://github.com/fsprojects/fantomas/blob/8bf81a121bf52e557b12e710f24a922c172a7ed5/src/Fantomas.Core.Tests/ChainTests.fs). The comments that file had around its tests say what the tests alone do not, and they are below as they were written, each with the cases it was written above or inside. The [scenarios README](../README.md) says how to read them.

## Tight receivers

`mkAtomicExpr` in the ASTTransformer marks a chain that has to stay one indivisible unit, because a prefix operator, an index or a `?member` binds directly to it. The tests below cover the prefix-operator, index and `?`-chain call sites. They all run with `SpaceBeforeUppercaseInvocation = true`, because that is the setting that would otherwise introduce the space: compare with `obj.Bar()` on its own, which correctly becomes `obj.Bar ()` under the same config.

Written above these cases:

- [`tight-receiver-control-case-a-plain-terminal-call-does-take.fs`](tight-receiver-control-case-a-plain-terminal-call-does-take.fs)
- [`tight-receiver-leading-expression-of-a-dynamic-chain.fs`](negative/tight-receiver-leading-expression-of-a-dynamic-chain.fs)
- [`tight-receiver-prefix-operator-applied-to-a-unit-call.fs`](negative/tight-receiver-prefix-operator-applied-to-a-unit-call.fs)
- [`tight-receiver-prefix-operator-applied-to-a-paren-call.fs`](negative/tight-receiver-prefix-operator-applied-to-a-paren-call.fs)
- [`tight-receiver-identifier-of-a-new-style-index.fs`](negative/tight-receiver-identifier-of-a-new-style-index.fs)

## Intermediate calls stay welded to their opening paren

An intermediate call may never be separated from its `(`: `a.Foo (x).Bar()` parses as `a.Foo ((x).Bar())`. A conditional directive attached to the argument pushes that argument onto its own lines, and the break has to land AFTER the `(`, not before it.

Written above these cases:

- [`directive-inside-an-intermediate-call-argument-keeps-the.fs`](directive-inside-an-intermediate-call-argument-keeps-the.fs)
- [`match-lambda-as-an-intermediate-call-argument-keeps-the.fs`](match-lambda-as-an-intermediate-call-argument-keeps-the.fs)
- [`match-lambda-as-a-terminal-call-argument-keeps-the-function.fs`](match-lambda-as-a-terminal-call-argument-keeps-the-function.fs)

## match-lambda-as-an-intermediate-call-argument-keeps-the

Identical in shape to the terminal case below: where the call sits in the chain has no say over a lambda argument, for `function` just as for `fun`.

Written inside:

- [`match-lambda-as-an-intermediate-call-argument-keeps-the.fs`](match-lambda-as-an-intermediate-call-argument-keeps-the.fs)

## Casing of the terminal is decided by the LAST segment

`SpaceBeforeUppercaseInvocation` looks at the name the terminal call is made on, which is always the final segment. Intermediate calls earlier in the chain have no say, and stay tight regardless of their own casing.

Written above these cases:

- [`uppercase-terminal-after-an-uppercase-intermediate-call.fs`](negative/uppercase-terminal-after-an-uppercase-intermediate-call.fs)
- [`lowercase-terminal-after-an-uppercase-intermediate-call-does.fs`](negative/lowercase-terminal-after-an-uppercase-intermediate-call-does.fs)

## A match lambda as the terminal call's argument

Only `MultiLineLambdaClosingNewline` or a break the user already made after the `(` moves `function` onto its own line. Where the call sits in the chain has no say.

Written above these cases:

- [`match-lambda-as-a-terminal-call-argument-breaks-when-closing.fs`](match-lambda-as-a-terminal-call-argument-breaks-when-closing.fs)
- [`match-lambda-as-a-terminal-call-argument-with-the-function.fs`](match-lambda-as-a-terminal-call-argument-with-the-function.fs)
- [`a-comment-before-the-argument-of-a-terminal-call-keeps-the.fs`](negative/a-comment-before-the-argument-of-a-terminal-call-keeps-the.fs)
- [`a-comment-before-the-argument-of-a-terminal-call-written-on.fs`](a-comment-before-the-argument-of-a-terminal-call-written-on.fs)
- [`a-comment-before-the-argument-of-an-intermediate-call-keeps.fs`](negative/a-comment-before-the-argument-of-an-intermediate-call-keeps.fs)
- [`a-comment-between-the-method-name-and-the-parenthesis-takes.fs`](negative/a-comment-between-the-method-name-and-the-parenthesis-takes.fs)
- [`a-comment-between-the-method-name-and-the-parenthesis-leaves.fs`](a-comment-between-the-method-name-and-the-parenthesis-leaves.fs)

## a-comment-on-its-own-line-after-the-receiver-indents-the

A comment written after the receiver ends its line before the chain has decided anything. The steps behind it have to open an indented line, or they land level with the receiver and the result no longer parses. Where the comment attaches depends on how it was written, so the three spellings below are the same chain and have to reach the same output.

Written above these cases:

- [`a-comment-on-its-own-line-after-the-receiver-indents-the.fs`](a-comment-on-its-own-line-after-the-receiver-indents-the.fs)
- [`the-column-the-comment-after-a-receiver-was-written-at-makes.fs`](negative/the-column-the-comment-after-a-receiver-was-written-at-makes.fs)
- [`a-trailing-comment-after-the-receiver-indents-the-steps.fs`](negative/a-trailing-comment-after-the-receiver-indents-the-steps.fs)

## A chain inside parentheses and the offside rule

A segment that lands on the chain head's own column reads as a new item rather than as a continuation. Inside a parenthesis the parser agrees and refuses the output, because the parenthesis opened its offside context at that very column. `genChain` compares the head's column with the column a fresh line would land on and adds a level of indentation when they meet, whatever encloses the chain. The collision depends on the indent size, so both 4 and 2 are covered here.

Written above these cases:

- [`chain-inside-parentheses-indents-its-segments-past-the-head.fs`](chain-inside-parentheses-indents-its-segments-past-the-head.fs)
- [`chain-that-stays-on-one-line-inside-parentheses-does-not.fs`](chain-that-stays-on-one-line-inside-parentheses-does-not.fs)
- [`chain-segments-are-moved-off-the-head-s-column-with-no.fs`](chain-segments-are-moved-off-the-head-s-column-with-no.fs)
- [`chain-inside-parentheses-indents-its-segments-past-the-head-2.fs`](chain-inside-parentheses-indents-its-segments-past-the-head-2.fs)
- [`chain-inside-parentheses-clears-a-head-that-a-pipe-pushed-to.fs`](chain-inside-parentheses-clears-a-head-that-a-pipe-pushed-to.fs)
