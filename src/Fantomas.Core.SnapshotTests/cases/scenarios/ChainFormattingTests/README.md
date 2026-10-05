# ChainFormattingTests

These cases were the tests of [`src/Fantomas.Core.Tests/ChainFormattingTests.fs`](https://github.com/fsprojects/fantomas/blob/8bf81a121bf52e557b12e710f24a922c172a7ed5/src/Fantomas.Core.Tests/ChainFormattingTests.fs). The comments that file had around its tests say what the tests alone do not, and they are below as they were written, each with the cases it was written above or inside. The [scenarios README](../README.md) says how to read them.

## At the top of the file

This file encodes the specification in `docs/docs/end-users/Chains.md`. Most tests mirror a before/after example from that document, at a narrow page width so the intended line breaks are visible. Where the spec shows a setting explicitly, the test uses it (default vs. MultiLineLambdaClosingNewline).

These tests are the north star for the chain redesign: they describe the output the design must produce.

## A single call at the end: break the arguments

Written above these cases:

- [`single-call-at-the-end-breaks-the-arguments.fs`](single-call-at-the-end-breaks-the-arguments.fs)

## Navigation before a single call: still break the arguments

Written above these cases:

- [`navigation-before-a-single-call-still-breaks-the-arguments.fs`](navigation-before-a-single-call-still-breaks-the-arguments.fs)

## A single call whose argument is a lambda

Written above these cases:

- [`single-trailing-lambda-call-closing-newline-false.fs`](single-trailing-lambda-call-closing-newline-false.fs)
- [`single-trailing-lambda-call-closing-newline-true.fs`](single-trailing-lambda-call-closing-newline-true.fs)

## Two or more calls: a pipeline

Written above these cases:

- [`two-or-more-calls-form-a-pipeline.fs`](two-or-more-calls-form-a-pipeline.fs)

## Navigation between calls rides at the front of the line

Written above these cases:

- [`navigation-between-calls-rides-at-the-front-of-the-line.fs`](navigation-between-calls-rides-at-the-front-of-the-line.fs)

## A long pipeline with lambdas: MultiLineLambdaClosingNewline = true

Written above these cases:

- [`long-pipeline-with-lambdas-closing-newline-true.fs`](long-pipeline-with-lambdas-closing-newline-true.fs)

## A chain that ends in navigation, not a call

Written above these cases:

- [`chain-that-ends-in-navigation-not-a-call.fs`](chain-that-ends-in-navigation-not-a-call.fs)

## A chain with no calls at all

Written above these cases:

- [`chain-with-no-calls-at-all-is-balanced-across-its-lines.fs`](chain-with-no-calls-at-all-is-balanced-across-its-lines.fs)

## A single trailing action is not enough on its own

Two further conditions guard the "break the arguments" branch: the starting value must be a plain value, and everything up to the method name must fit on one line.

Written above these cases:

- [`a-plain-starting-value-keeps-the-chain-together.fs`](a-plain-starting-value-keeps-the-chain-together.fs)
- [`a-starting-value-that-is-a-call-leads-a-pipeline-even-with.fs`](a-starting-value-that-is-a-call-leads-a-pipeline-even-with.fs)
- [`a-comment-between-the-steps-leaves-nothing-to-keep-together.fs`](negative/a-comment-between-the-steps-leaves-nothing-to-keep-together.fs)

## a-starting-value-that-is-a-call-leads-a-pipeline-even-with

Same steps and the same single trailing call as the test above; only the starting value differs, and that alone decides the layout.

Written inside:

- [`a-starting-value-that-is-a-call-leads-a-pipeline-even-with.fs`](a-starting-value-that-is-a-call-leads-a-pipeline-even-with.fs)

## When a line is still too long

A run of navigation that does not fit wraps before a dot, chosen so the longest resulting line is as short as possible. Greedy filling would leave one line packed to the margin and a stub behind it.

Written above these cases:

- [`a-long-navigation-run-is-balanced-rather-than-filled.fs`](a-long-navigation-run-is-balanced-rather-than-filled.fs)
- [`when-two-splits-tie-the-longer-first-line-wins.fs`](when-two-splits-tie-the-longer-first-line-wins.fs)
- [`the-receiver-keeps-a-step-rather-than-sitting-alone.fs`](the-receiver-keeps-a-step-rather-than-sitting-alone.fs)
- [`navigation-wraps-to-keep-an-intermediate-call-whole.fs`](navigation-wraps-to-keep-an-intermediate-call-whole.fs)
- [`navigation-wraps-to-keep-the-terminal-call-whole.fs`](navigation-wraps-to-keep-the-terminal-call-whole.fs)
- [`arguments-still-break-when-no-wrap-can-hold-the-whole-call.fs`](arguments-still-break-when-no-wrap-can-hold-the-whole-call.fs)
- [`a-call-leaving-the-receiver-s-line-has-its-own-line-to-fit.fs`](a-call-leaving-the-receiver-s-line-has-its-own-line-to-fit.fs)
- [`two-or-more-calls-form-a-pipeline.fs`](two-or-more-calls-form-a-pipeline.fs)
- [`a-step-carrying-a-comment-opens-its-own-line-and-the-rest-is.fs`](a-step-carrying-a-comment-opens-its-own-line-and-the-rest-is.fs)

## navigation-wraps-to-keep-an-intermediate-call-whole

The run leads a call in the middle of a pipeline. Wrapping the navigation is preferred, so `spec` is never pushed onto a line of its own to make room for it.

Written inside:

- [`navigation-wraps-to-keep-an-intermediate-call-whole.fs`](navigation-wraps-to-keep-an-intermediate-call-whole.fs)

## navigation-wraps-to-keep-the-terminal-call-whole

Wrapping one step earlier than strictly needed keeps `keyName` beside its method.

Written inside:

- [`navigation-wraps-to-keep-the-terminal-call-whole.fs`](navigation-wraps-to-keep-the-terminal-call-whole.fs)

## a-call-leaving-the-receiver-s-line-has-its-own-line-to-fit

The navigation fits on the receiver's line, so the call leaves that line and claims one of its own. There is nothing for the navigation to make room for, and it stays put.

Written inside:

- [`a-call-leaving-the-receiver-s-line-has-its-own-line-to-fit.fs`](a-call-leaving-the-receiver-s-line-has-its-own-line-to-fit.fs)

## a-step-carrying-a-comment-opens-its-own-line-and-the-rest-is

A commented dot renders on lines of its own, so it has no width to balance with. The run stops there and resumes afterwards, measured from where the comment left off.

Written inside:

- [`a-step-carrying-a-comment-opens-its-own-line-and-the-rest-is.fs`](a-step-carrying-a-comment-opens-its-own-line-and-the-rest-is.fs)

## A dot-lambda body (_.…)

Written above these cases:

- [`dot-lambda-body-stays-tight-and-short.fs`](negative/dot-lambda-body-stays-tight-and-short.fs)
- [`dot-lambda-body-too-long-follows-leading-dot.fs`](dot-lambda-body-too-long-follows-leading-dot.fs)

## Exotic and combined shapes

Written above these cases:

- [`pipeline-with-one-multiline-call-closing-newline-false.fs`](pipeline-with-one-multiline-call-closing-newline-false.fs)
- [`pipeline-with-one-multiline-call-closing-newline-true.fs`](pipeline-with-one-multiline-call-closing-newline-true.fs)
- [`intermediate-call-whose-tuple-arguments-break.fs`](intermediate-call-whose-tuple-arguments-break.fs)
- [`several-navigation-steps-between-two-calls.fs`](several-navigation-steps-between-two-calls.fs)
- [`generic-type-application-methods.fs`](generic-type-application-methods.fs)
- [`chain-whose-receiver-is-itself-a-call.fs`](chain-whose-receiver-is-itself-a-call.fs)
- [`index-between-two-calls.fs`](index-between-two-calls.fs)

## pipeline-with-one-multiline-call-closing-newline-false

A pipeline where one call is multiline and the others are not.

Written above:

- [`pipeline-with-one-multiline-call-closing-newline-false.fs`](pipeline-with-one-multiline-call-closing-newline-false.fs)

## intermediate-call-whose-tuple-arguments-break

An intermediate call whose tuple arguments break.

Written above:

- [`intermediate-call-whose-tuple-arguments-break.fs`](intermediate-call-whose-tuple-arguments-break.fs)

## several-navigation-steps-between-two-calls

Several navigation steps between two calls.

Written above:

- [`several-navigation-steps-between-two-calls.fs`](several-navigation-steps-between-two-calls.fs)

## generic-type-application-methods

Generic (type-application) methods.

Written above:

- [`generic-type-application-methods.fs`](generic-type-application-methods.fs)

## chain-whose-receiver-is-itself-a-call

A chain whose receiver is itself a call.

Written above:

- [`chain-whose-receiver-is-itself-a-call.fs`](chain-whose-receiver-is-itself-a-call.fs)

## index-between-two-calls

An index between two calls.

Written above:

- [`index-between-two-calls.fs`](index-between-two-calls.fs)

## The receiver has a vote

Keeping a chain together is only offered when the receiver is a plain value. A compound receiver leads a pipeline even when there is a single trailing call.

Written above these cases:

- [`plain-value-receiver-with-a-single-trailing-call-breaks-the.fs`](plain-value-receiver-with-a-single-trailing-call-breaks-the.fs)
- [`call-receiver-with-a-single-trailing-call-leads-a-pipeline.fs`](call-receiver-with-a-single-trailing-call-leads-a-pipeline.fs)
- [`parenthesised-receiver-with-a-single-trailing-call-leads-a.fs`](parenthesised-receiver-with-a-single-trailing-call-leads-a.fs)
- [`generic-receiver-with-a-single-trailing-call-leads-a.fs`](generic-receiver-with-a-single-trailing-call-leads-a.fs)

## Exception: a parenthesised value that is only indexed

Written above these cases:

- [`multiline-parenthesised-receiver-keeps-a-lone-dot-index.fs`](multiline-parenthesised-receiver-keeps-a-lone-dot-index.fs)
- [`multiline-parenthesised-receiver-followed-by-a-member-does.fs`](multiline-parenthesised-receiver-followed-by-a-member-does.fs)

## A generic call is still a call

Written above these cases:

- [`generic-call-is-an-action-so-a-lone-trailing-one-breaks-its.fs`](generic-call-is-an-action-so-a-lone-trailing-one-breaks-its.fs)
- [`bare-generic-member-is-navigation-and-rides-with-the.fs`](bare-generic-member-is-navigation-and-rides-with-the.fs)

## Lambda arguments do not depend on the call's position

`fun` and `function` are laid out the same way wherever their call sits, as the F# style guide asks ("Treat match lambda's in a similar fashion"). The eight tests below are the full matrix: both lambda forms, both positions, both settings.

Written above these cases:

- [`fun-lambda-mid-pipeline-keeps-its-opener-attached.fs`](fun-lambda-mid-pipeline-keeps-its-opener-attached.fs)
- [`fun-lambda-as-the-last-step-keeps-its-opener-attached.fs`](fun-lambda-as-the-last-step-keeps-its-opener-attached.fs)
- [`match-lambda-mid-pipeline-keeps-its-opener-attached.fs`](match-lambda-mid-pipeline-keeps-its-opener-attached.fs)
- [`match-lambda-as-the-last-step-keeps-its-opener-attached.fs`](match-lambda-as-the-last-step-keeps-its-opener-attached.fs)
- [`fun-lambda-mid-pipeline-closing-newline-true.fs`](fun-lambda-mid-pipeline-closing-newline-true.fs)
- [`fun-lambda-as-the-last-step-closing-newline-true.fs`](fun-lambda-as-the-last-step-closing-newline-true.fs)
- [`match-lambda-mid-pipeline-closing-newline-true.fs`](match-lambda-mid-pipeline-closing-newline-true.fs)
- [`match-lambda-as-the-last-step-closing-newline-true.fs`](match-lambda-as-the-last-step-closing-newline-true.fs)

## match-lambda-as-the-last-step-function-written-below-the

The setting is the only thing that moves `function` onto its own line. The same call written across more lines is still the same call, so it formats the same way.

Written inside:

- [`match-lambda-as-the-last-step-function-written-below-the.fs`](match-lambda-as-the-last-step-function-written-below-the.fs)

## A lambda argument that no longer fits

Where the lambda goes is the argument's business, not the chain's, so a call reached through a dot is laid out exactly like the same call without one. The F# style guide asks for everything up to the arrow on one line, and rejects parameters aligned under the opening parenthesis, because that column depends on the length of the name in front of it.

Written above these cases:

- [`lambda-moves-to-its-own-line-when-everything-up-to-the-arrow.fs`](lambda-moves-to-its-own-line-when-everything-up-to-the-arrow.fs)
- [`lambda-parameters-take-a-line-each-when-they-do-not-fit.fs`](lambda-parameters-take-a-line-each-when-they-do-not-fit.fs)
- [`lambda-that-still-does-not-fit-after-moving-down-keeps-its.fs`](lambda-that-still-does-not-fit-after-moving-down-keeps-its.fs)
- [`lambda-that-fits-on-one-line-after-moving-down-keeps-its.fs`](lambda-that-fits-on-one-line-after-moving-down-keeps-its.fs)

## A lambda argument to a call that is not the last step

The call above is the last step of its chain, which is what lets the `(` move down with the lambda. A call with a step behind it cannot do that: the gap makes `a.Foo (x).Bar()`, which passes `(x).Bar()` to `Foo` instead of calling `Bar` on the result. So the `(` stays against the member name and the break happens behind it.

Hanging the parameters under `(fun` is the other way to keep the `(` where it is, and the style guide rules it out: the column would be the length of the member name. The shape the guide asks for instead, parameters indented one level, is not valid F# below a `(` that sits mid-line.

Written above these cases:

- [`3432-lambda-argument-to-an-intermediate-call-keeps-its-opening.fs`](3432-lambda-argument-to-an-intermediate-call-keeps-its-opening.fs)
- [`lambda-argument-to-an-intermediate-call-member-behind-it.fs`](lambda-argument-to-an-intermediate-call-member-behind-it.fs)
- [`lambda-argument-to-an-intermediate-call-closing-newline-true.fs`](lambda-argument-to-an-intermediate-call-closing-newline-true.fs)
- [`multiline-parameter-of-a-lambda-argument-to-an-intermediate.fs`](multiline-parameter-of-a-lambda-argument-to-an-intermediate.fs)
- [`multiline-parameter-of-a-lambda-argument-to-an-intermediate-2.fs`](multiline-parameter-of-a-lambda-argument-to-an-intermediate-2.fs)
- [`type-arguments-on-an-intermediate-call-taking-a-lambda.fs`](type-arguments-on-an-intermediate-call-taking-a-lambda.fs)
- [`type-arguments-on-an-intermediate-call-taking-a-lambda-2.fs`](type-arguments-on-an-intermediate-call-taking-a-lambda-2.fs)
- [`3432-generic-intermediate-call-taking-a-lambda.fs`](3432-generic-intermediate-call-taking-a-lambda.fs)

## lambda-argument-to-an-intermediate-call-member-behind-it

A member rather than a call behind the lambda reparses without a diagnostic: the `.Value` would silently become part of the argument.

Written inside:

- [`lambda-argument-to-an-intermediate-call-member-behind-it.fs`](lambda-argument-to-an-intermediate-call-member-behind-it.fs)

## multiline-parameter-of-a-lambda-argument-to-an-intermediate

A single parameter that is multiline by itself moves the lambda down for the same reason an opener that does not fit does, so it arrives at the same rule.

Written inside:

- [`multiline-parameter-of-a-lambda-argument-to-an-intermediate.fs`](multiline-parameter-of-a-lambda-argument-to-an-intermediate.fs)

## type-arguments-on-an-intermediate-call-taking-a-lambda

Type arguments lift the call out of the chain and lengthen the opener, but neither is what decides this: the rule is the same one the plain member above answers to.

Written inside:

- [`type-arguments-on-an-intermediate-call-taking-a-lambda.fs`](type-arguments-on-an-intermediate-call-taking-a-lambda.fs)

## lambda-argument-to-the-last-call-keeps-moving-down

The counterparts of the four above, with the lambda call as the last step of the chain. There the `(` is free to move down with the argument, because nothing follows it to be swallowed, so these keep the layout they always had. They are here to mark where the rule above stops.

Written above these cases:

- [`lambda-argument-to-the-last-call-keeps-moving-down.fs`](lambda-argument-to-the-last-call-keeps-moving-down.fs)
- [`type-arguments-on-the-last-call-taking-a-lambda.fs`](type-arguments-on-the-last-call-taking-a-lambda.fs)
- [`lambda-argument-to-the-last-call-closing-newline-true.fs`](lambda-argument-to-the-last-call-closing-newline-true.fs)
- [`type-arguments-on-the-last-call-taking-a-lambda-closing.fs`](type-arguments-on-the-last-call-taking-a-lambda-closing.fs)
