# SpaceBeforeUppercaseInvocationTests

These cases were the tests of [`src/Fantomas.Core.Tests/SpaceBeforeUppercaseInvocationTests.fs`](https://github.com/fsprojects/fantomas/blob/8bf81a121bf52e557b12e710f24a922c172a7ed5/src/Fantomas.Core.Tests/SpaceBeforeUppercaseInvocationTests.fs). The comments that file had around its tests say what the tests alone do not, and they are below as they were written, each with the cases it was written above or inside. The [scenarios README](../README.md) says how to read them.

## Space before () in Uppercase function call

Written above these cases:

- [`default-config-should-not-add-space-before-unit-in-uppercase.fs`](negative/default-config-should-not-add-space-before-unit-in-uppercase.fs)
- [`spacebeforeuppercaseinvocation-should-add-space-before-unit.fs`](spacebeforeuppercaseinvocation-should-add-space-before-unit.fs)
- [`spacebeforeuppercaseinvocation-should-add-space-before-unit-2.fs`](spacebeforeuppercaseinvocation-should-add-space-before-unit-2.fs)

## Exception to the rule

Written above these cases:

- [`spacebeforeuppercaseinvocation-should-not-have-impact-when.fs`](negative/spacebeforeuppercaseinvocation-should-not-have-impact-when.fs)
- [`1401-spacebeforeuppercaseinvocation-should-not-have-impact-when.fs`](1401-spacebeforeuppercaseinvocation-should-not-have-impact-when.fs)

## Space before parentheses (a+b) in Uppercase function call

Written above these cases:

- [`default-config-should-not-add-space-before-parentheses-in.fs`](default-config-should-not-add-space-before-parentheses-in.fs)
- [`spacebeforeuppercaseinvocation-should-add-space-before.fs`](spacebeforeuppercaseinvocation-should-add-space-before.fs)
- [`943-space-before-uppercase-function-application-cannot-apply.fs`](negative/943-space-before-uppercase-function-application-cannot-apply.fs)
- [`space-before-uppercase-dotindexedset.fs`](negative/space-before-uppercase-dotindexedset.fs)
- [`853-setting-spacebeforeuppercaseinvocation-is-not-applied-in-the.fs`](853-setting-spacebeforeuppercaseinvocation-is-not-applied-in-the.fs)
- [`space-before-uppercase-constructor-without-new.fs`](space-before-uppercase-constructor-without-new.fs)
- [`space-before-upper-case-constructor-invocation-with-new.fs`](space-before-upper-case-constructor-invocation-with-new.fs)
- [`space-before-uppercase-member-call.fs`](space-before-uppercase-member-call.fs)
- [`1226-function-application-inside-parenthesis-followed-by.fs`](negative/1226-function-application-inside-parenthesis-followed-by.fs)
- [`1488-ignore-setting-when-function-call-is-the-argument-of-prefix.fs`](1488-ignore-setting-when-function-call-is-the-argument-of-prefix.fs)
- [`no-space-before-uppercase-patterns.fs`](no-space-before-uppercase-patterns.fs)
- [`space-before-uppercase-patterns.fs`](space-before-uppercase-patterns.fs)
- [`2685-never-add-a-space-before-paren-lambda-in-chain.fs`](2685-never-add-a-space-before-paren-lambda-in-chain.fs)
- [`2700-typeapp-with-dotget-and-paren-expr.fs`](2700-typeapp-with-dotget-and-paren-expr.fs)
- [`2965-space-should-not-be-added-when-expression-is-indexed.fs`](negative/2965-space-should-not-be-added-when-expression-is-indexed.fs)
- [`space-should-not-be-added-when-expression-is-indexed-single.fs`](negative/space-should-not-be-added-when-expression-is-indexed-single.fs)
- [`space-should-not-be-added-when-expression-is-indexed.fs`](negative/space-should-not-be-added-when-expression-is-indexed.fs)
- [`space-should-not-be-added-when-expression-is-indexed-single-2.fs`](negative/space-should-not-be-added-when-expression-is-indexed-single-2.fs)

## space-before-a-call-reached-by-a-single-dot-from-a-plain

The setting only gets a say when the whole thing being called is a plain dotted name. A call, an index, a receiver that is not a name, or a type application anywhere in it, and the parenthesis stays tight. Agreed at https://github.com/fsharp/fslang-design/issues/648. The lowercase half of these live in SpaceBeforeLowercaseInvocationTests.

Written above these cases:

- [`space-before-a-call-reached-by-a-single-dot-from-a-plain.fs`](space-before-a-call-reached-by-a-single-dot-from-a-plain.fs)
- [`space-before-a-call-however-many-dots-the-name-has.fs`](space-before-a-call-however-many-dots-the-name-has.fs)
- [`base-and-this-are-plain-names-and-take-the-space.fs`](base-and-this-are-plain-names-and-take-the-space.fs)
- [`no-space-when-a-call-is-reached-through-a-dot-before-it.fs`](negative/no-space-when-a-call-is-reached-through-a-dot-before-it.fs)
- [`no-space-when-the-receiver-is-itself-a-call.fs`](negative/no-space-when-the-receiver-is-itself-a-call.fs)
- [`no-space-when-an-index-comes-before-the-call-in-either.fs`](negative/no-space-when-an-index-comes-before-the-call-in-either.fs)
- [`no-space-when-the-receiver-is-a-parenthesised-expression.fs`](negative/no-space-when-the-receiver-is-a-parenthesised-expression.fs)
- [`no-space-when-the-receiver-is-a-constant.fs`](negative/no-space-when-the-receiver-is-a-constant.fs)
- [`no-space-when-the-receiver-is-a-list-or-an-anonymous-record.fs`](negative/no-space-when-the-receiver-is-a-list-or-an-anonymous-record.fs)
- [`no-space-when-the-receiver-carries-a-type-application.fs`](negative/no-space-when-the-receiver-carries-a-type-application.fs)
- [`no-space-when-the-call-itself-carries-a-type-application.fs`](negative/no-space-when-the-call-itself-carries-a-type-application.fs)
