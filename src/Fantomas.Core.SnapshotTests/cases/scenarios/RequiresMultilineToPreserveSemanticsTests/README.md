# RequiresMultilineToPreserveSemanticsTests

These cases were the tests of [`src/Fantomas.Core.Tests/RequiresMultilineToPreserveSemanticsTests.fs`](https://github.com/fsprojects/fantomas/blob/8bf81a121bf52e557b12e710f24a922c172a7ed5/src/Fantomas.Core.Tests/RequiresMultilineToPreserveSemanticsTests.fs). The comments that file had around its tests say what the tests alone do not, and they are below as they were written, each with the cases it was written above or inside. The [scenarios README](../README.md) says how to read them.

## Expr.InfixApp (single infix operator)

Written above these cases:

- [`lambda-on-lhs-of-pipe-operator-stays-multiline.fs`](negative/lambda-on-lhs-of-pipe-operator-stays-multiline.fs)
- [`if-then-else-on-lhs-of-pipe-operator-stays-multiline.fs`](negative/if-then-else-on-lhs-of-pipe-operator-stays-multiline.fs)
- [`if-then-else-on-lhs-of-non-pipe-infix-operator-stays.fs`](negative/if-then-else-on-lhs-of-non-pipe-infix-operator-stays.fs)
- [`infix-app-with-lambda-rhs-on-lhs-of-pipe-operator-stays.fs`](negative/infix-app-with-lambda-rhs-on-lhs-of-pipe-operator-stays.fs)
- [`lambda-on-lhs-of-composition-operator-stays-multiline.fs`](negative/lambda-on-lhs-of-composition-operator-stays-multiline.fs)

## Expr.SameInfixApps (chained same-operator expressions)

Written above these cases:

- [`lambda-leading-chained-pipe-operators-stays-multiline.fs`](negative/lambda-leading-chained-pipe-operators-stays-multiline.fs)
- [`if-then-else-leading-chained-pipe-operators-stays-multiline.fs`](negative/if-then-else-leading-chained-pipe-operators-stays-multiline.fs)
- [`nested-open-ended-expression-leading-chained-pipe-operators.fs`](negative/nested-open-ended-expression-leading-chained-pipe-operators.fs)
- [`open-ended-expression-in-middle-of-chained-pipe-stays.fs`](negative/open-ended-expression-in-middle-of-chained-pipe-stays.fs)

## Expr.Tuple (open-ended non-last element)

Written above these cases:

- [`lambda-as-non-last-tuple-element-stays-multiline.fs`](negative/lambda-as-non-last-tuple-element-stays-multiline.fs)
- [`nested-open-ended-as-non-last-tuple-element-stays-multiline.fs`](negative/nested-open-ended-as-non-last-tuple-element-stays-multiline.fs)
- [`if-then-else-as-non-last-tuple-element-stays-multiline.fs`](negative/if-then-else-as-non-last-tuple-element-stays-multiline.fs)
- [`match-as-non-last-tuple-element-stays-multiline.fs`](negative/match-as-non-last-tuple-element-stays-multiline.fs)

## Expr.ArrayOrList (open-ended non-last element)

Written above these cases:

- [`lambda-as-non-last-list-element-stays-multiline.fs`](lambda-as-non-last-list-element-stays-multiline.fs)
- [`if-then-else-as-non-last-list-element-stays-multiline.fs`](if-then-else-as-non-last-list-element-stays-multiline.fs)
- [`nested-open-ended-as-non-last-list-element-stays-multiline.fs`](nested-open-ended-as-non-last-list-element-stays-multiline.fs)
- [`lambda-in-middle-of-list-stays-multiline.fs`](lambda-in-middle-of-list-stays-multiline.fs)

## Record fields (open-ended non-last field value)

Written above these cases:

- [`lambda-in-non-last-record-field-stays-multiline.fs`](lambda-in-non-last-record-field-stays-multiline.fs)
- [`if-then-else-in-non-last-record-field-stays-multiline.fs`](if-then-else-in-non-last-record-field-stays-multiline.fs)
- [`nested-open-ended-in-non-last-record-field-stays-multiline.fs`](nested-open-ended-in-non-last-record-field-stays-multiline.fs)

## Open-ended expression forms

Written above these cases:

- [`let-in-in-non-last-record-field-stays-multiline.fs`](let-in-in-non-last-record-field-stays-multiline.fs)
- [`let-in-in-non-last-anonymous-record-field-stays-multiline.fs`](let-in-in-non-last-anonymous-record-field-stays-multiline.fs)
- [`lazy-wrapping-a-lambda-as-non-last-list-element-stays.fs`](negative/lazy-wrapping-a-lambda-as-non-last-list-element-stays.fs)
- [`lazy-wrapping-a-plain-expression-as-non-last-list-element.fs`](lazy-wrapping-a-plain-expression-as-non-last-list-element.fs)
- [`yield-wrapping-a-lambda-as-non-last-list-element-stays.fs`](negative/yield-wrapping-a-lambda-as-non-last-list-element-stays.fs)
- [`assert-wrapping-a-lambda-as-non-last-list-element-stays.fs`](negative/assert-wrapping-a-lambda-as-non-last-list-element-stays.fs)
- [`property-assignment-of-a-lambda-as-non-last-list-element.fs`](negative/property-assignment-of-a-lambda-as-non-last-list-element.fs)
- [`property-assignment-of-a-plain-expression-as-non-last-list.fs`](property-assignment-of-a-plain-expression-as-non-last-list.fs)
- [`indexed-assignment-of-a-lambda-as-non-last-list-element.fs`](negative/indexed-assignment-of-a-lambda-as-non-last-list-element.fs)
- [`indexed-assignment-of-a-plain-expression-as-non-last-list.fs`](indexed-assignment-of-a-plain-expression-as-non-last-list.fs)
- [`dynamic-assignment-of-a-lambda-as-non-last-list-element.fs`](negative/dynamic-assignment-of-a-lambda-as-non-last-list-element.fs)
- [`named-indexed-property-assignment-of-a-lambda-as-non-last.fs`](negative/named-indexed-property-assignment-of-a-lambda-as-non-last.fs)
- [`dot-named-indexed-property-assignment-of-a-lambda-as-non.fs`](negative/dot-named-indexed-property-assignment-of-a-lambda-as-non.fs)

## Regression tests

Written above these cases:

- [`3278-lambda-in-tuple-in-list-preserves-semantics.fs`](negative/3278-lambda-in-tuple-in-list-preserves-semantics.fs)
- [`3274-lambda-with-custom-operator-preserves-semantics.fs`](negative/3274-lambda-with-custom-operator-preserves-semantics.fs)
- [`constructor-with-3-args-and-no-open-ended-elements-stays-on.fs`](negative/constructor-with-3-args-and-no-open-ended-elements-stays-on.fs)
- [`constructor-with-4-args-and-no-open-ended-elements-stays-on.fs`](negative/constructor-with-4-args-and-no-open-ended-elements-stays-on.fs)
- [`constructor-with-3-args-and-if-then-else-in-first-uses-comma.fs`](negative/constructor-with-3-args-and-if-then-else-in-first-uses-comma.fs)
- [`constructor-with-3-args-and-lambda-in-middle-uses-comma.fs`](negative/constructor-with-3-args-and-lambda-in-middle-uses-comma.fs)
- [`3-element-tuple-with-lambda-in-last-position-stays-on-one.fs`](negative/3-element-tuple-with-lambda-in-last-position-stays-on-one.fs)
- [`3-element-tuple-with-lambda-in-first-position-uses-comma.fs`](negative/3-element-tuple-with-lambda-in-first-position-uses-comma.fs)
- [`3-element-tuple-with-lambda-in-middle-uses-comma-leading.fs`](negative/3-element-tuple-with-lambda-in-middle-uses-comma-leading.fs)
