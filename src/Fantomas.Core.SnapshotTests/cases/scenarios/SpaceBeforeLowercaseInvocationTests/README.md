# SpaceBeforeLowercaseInvocationTests

These cases were the tests of [`src/Fantomas.Core.Tests/SpaceBeforeLowercaseInvocationTests.fs`](https://github.com/fsprojects/fantomas/blob/8bf81a121bf52e557b12e710f24a922c172a7ed5/src/Fantomas.Core.Tests/SpaceBeforeLowercaseInvocationTests.fs). The comments that file had around its tests say what the tests alone do not, and they are below as they were written, each with the cases it was written above or inside. The [scenarios README](../README.md) says how to read them.

## Space before () in lowercase function call

Written above these cases:

- [`default-config-should-add-space-before-unit-in-lowercase.fs`](default-config-should-add-space-before-unit-in-lowercase.fs)
- [`spacebeforelowercaseinvocation-false-should-not-add-space.fs`](negative/spacebeforelowercaseinvocation-false-should-not-add-space.fs)

## Space before parentheses (a+b) in lowercase function call

Written above these cases:

- [`default-config-should-add-space-before-parentheses-in.fs`](default-config-should-add-space-before-parentheses-in.fs)
- [`spacebeforelowercaseinvocation-false-should-not-add-space-2.fs`](spacebeforelowercaseinvocation-false-should-not-add-space-2.fs)
- [`spacebeforelowercaseinvocation-should-not-have-impact-when.fs`](negative/spacebeforelowercaseinvocation-should-not-have-impact-when.fs)
- [`space-before-lower-constructor-without-new.fs`](space-before-lower-constructor-without-new.fs)
- [`space-before-lower-case-constructor-invocation-with-new.fs`](space-before-lower-case-constructor-invocation-with-new.fs)
- [`space-before-lower-member-call.fs`](space-before-lower-member-call.fs)
- [`no-space-before-lowercase-member-calls-and-constructors.fs`](no-space-before-lowercase-member-calls-and-constructors.fs)
- [`ignore-setting-when-function-call-is-the-argument-of-prefix.fs`](ignore-setting-when-function-call-is-the-argument-of-prefix.fs)
- [`setting-also-affects-patterns.fs`](setting-also-affects-patterns.fs)
- [`space-before-lowercase-patterns.fs`](space-before-lowercase-patterns.fs)
- [`no-space-before-lowercase-patterns.fs`](no-space-before-lowercase-patterns.fs)

## space-before-a-deeply-qualified-lowercase-function

The setting only gets a say when the whole thing being called is a plain dotted name. A call, an index, a receiver that is not a name, or a type application anywhere in it, and the parenthesis stays tight. Agreed at https://github.com/fsharp/fslang-design/issues/648. The uppercase half of these live in SpaceBeforeUppercaseInvocationTests.

Written above these cases:

- [`space-before-a-deeply-qualified-lowercase-function.fs`](space-before-a-deeply-qualified-lowercase-function.fs)
- [`the-last-part-of-the-name-decides-which-of-the-two-settings.fs`](the-last-part-of-the-name-decides-which-of-the-two-settings.fs)
- [`space-before-a-call-on-a-type-parameter-which-is-a-plain.fs`](space-before-a-call-on-a-type-parameter-which-is-a-plain.fs)
- [`no-space-anywhere-in-the-reported-fluent-chain-fslang-design.fs`](no-space-anywhere-in-the-reported-fluent-chain-fslang-design.fs)
- [`no-space-before-a-generic-call-that-has-no-dots.fs`](negative/no-space-before-a-generic-call-that-has-no-dots.fs)
- [`a-generic-application-without-parentheses-is-left-alone.fs`](negative/a-generic-application-without-parentheses-is-left-alone.fs)
