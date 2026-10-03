# RecordSpreadTests

These cases were the tests of [`src/Fantomas.Core.Tests/RecordSpreadTests.fs`](https://github.com/fsprojects/fantomas/blob/8bf81a121bf52e557b12e710f24a922c172a7ed5/src/Fantomas.Core.Tests/RecordSpreadTests.fs). The comments that file had around its tests say what the tests alone do not, and they are below as they were written, each with the cases it was written above or inside. The [scenarios README](../README.md) says how to read them.

## At the top of the file

Record spreads, RFC FS-1151, dotnet/fsharp#18927. The spread reaches the syntax tree in three distinct places:

SynTypeDefnSimpleRepr.Record   holds SynFieldOrSpread,                carrying SynTypeSpread SynExpr.Record                 holds SynExprRecordFieldOrSpread,      carrying SynExprSpread SynExpr.AnonRecd               holds SynExprAnonRecordFieldOrSpread,  carrying SynExprSpread

Each of those is exercised below, in implementation and signature files where the construct can appear in both.

## Record type definition, SynFieldOrSpread.Spread

Written above these cases:

- [`type-definition-with-only-a-spread.fs`](negative/type-definition-with-only-a-spread.fs)
- [`type-definition-with-a-spread-before-a-field.fs`](negative/type-definition-with-a-spread-before-a-field.fs)
- [`type-definition-with-a-spread-after-a-field.fs`](negative/type-definition-with-a-spread-after-a-field.fs)
- [`type-definition-with-a-spread-between-fields.fs`](negative/type-definition-with-a-spread-between-fields.fs)
- [`type-definition-with-multiple-spreads.fs`](negative/type-definition-with-multiple-spreads.fs)
- [`type-definition-with-a-generic-spread-source.fs`](negative/type-definition-with-a-generic-spread-source.fs)
- [`type-definition-with-a-long-identifier-spread-source.fs`](negative/type-definition-with-a-long-identifier-spread-source.fs)
- [`multiline-type-definition-with-a-spread.fs`](negative/multiline-type-definition-with-a-spread.fs)
- [`multiline-type-definition-with-a-trailing-spread.fs`](negative/multiline-type-definition-with-a-trailing-spread.fs)
- [`type-definition-with-a-spread-and-an-attribute.fs`](negative/type-definition-with-a-spread-and-an-attribute.fs)
- [`type-definition-with-a-spread-and-an-xml-doc.fs`](negative/type-definition-with-a-spread-and-an-xml-doc.fs)
- [`type-definition-with-a-spread-and-a-member.fs`](negative/type-definition-with-a-spread-and-a-member.fs)
- [`type-definition-with-a-spread-stroustrup.fs`](negative/type-definition-with-a-spread-stroustrup.fs)
- [`type-definition-with-a-comment-before-the-spread.fs`](negative/type-definition-with-a-comment-before-the-spread.fs)
- [`type-definition-with-a-comment-after-the-spread.fs`](negative/type-definition-with-a-comment-after-the-spread.fs)
- [`type-definition-with-a-comment-between-the-spread-and-a.fs`](negative/type-definition-with-a-comment-between-the-spread-and-a.fs)

## Record type definition in a signature file

Written above these cases:

- [`signature-file-type-definition-with-only-a-spread.fsi`](negative/signature-file-type-definition-with-only-a-spread.fsi)
- [`signature-file-type-definition-with-a-spread-and-fields.fsi`](negative/signature-file-type-definition-with-a-spread-and-fields.fsi)
- [`signature-file-multiline-type-definition-with-a-spread.fsi`](negative/signature-file-multiline-type-definition-with-a-spread.fsi)
- [`signature-file-type-definition-with-a-spread-and-a-member.fsi`](negative/signature-file-type-definition-with-a-spread-and-a-member.fsi)
- [`signature-file-type-definition-with-a-comment-before-the.fsi`](negative/signature-file-type-definition-with-a-comment-before-the.fsi)
- [`signature-file-type-definition-with-a-comment-after-the.fsi`](negative/signature-file-type-definition-with-a-comment-after-the.fsi)

## Record expression, SynExprRecordFieldOrSpread.Spread

Written above these cases:

- [`record-expression-with-only-a-spread.fs`](negative/record-expression-with-only-a-spread.fs)
- [`record-expression-with-a-spread-before-a-field.fs`](negative/record-expression-with-a-spread-before-a-field.fs)
- [`record-expression-with-a-spread-after-a-field.fs`](negative/record-expression-with-a-spread-after-a-field.fs)
- [`record-expression-with-multiple-spreads.fs`](negative/record-expression-with-multiple-spreads.fs)
- [`record-expression-with-a-record-literal-as-spread-source.fs`](negative/record-expression-with-a-record-literal-as-spread-source.fs)
- [`record-expression-with-an-application-as-spread-source.fs`](negative/record-expression-with-an-application-as-spread-source.fs)
- [`record-expression-with-a-parenthesized-property-get-as.fs`](negative/record-expression-with-a-parenthesized-property-get-as.fs)
- [`multiline-record-expression-with-a-spread.fs`](negative/multiline-record-expression-with-a-spread.fs)
- [`multiline-record-expression-with-a-trailing-spread.fs`](negative/multiline-record-expression-with-a-trailing-spread.fs)
- [`record-expression-with-a-multiline-application-as-spread.fs`](negative/record-expression-with-a-multiline-application-as-spread.fs)
- [`record-expression-with-a-conditional-as-spread-source.fs`](negative/record-expression-with-a-conditional-as-spread-source.fs)
- [`record-expression-with-a-spread-source-that-has-to-break.fs`](negative/record-expression-with-a-spread-source-that-has-to-break.fs)
- [`copy-and-update-record-expression-with-a-spread.fs`](negative/copy-and-update-record-expression-with-a-spread.fs)
- [`record-expression-with-a-spread-stroustrup.fs`](negative/record-expression-with-a-spread-stroustrup.fs)

## Anonymous record expression, SynExprAnonRecordFieldOrSpread.Spread

Written above these cases:

- [`anonymous-record-expression-with-only-a-spread.fs`](negative/anonymous-record-expression-with-only-a-spread.fs)
- [`anonymous-record-expression-with-a-spread-before-a-field.fs`](negative/anonymous-record-expression-with-a-spread-before-a-field.fs)
- [`anonymous-record-expression-with-a-spread-after-a-field.fs`](negative/anonymous-record-expression-with-a-spread-after-a-field.fs)
- [`anonymous-record-expression-with-multiple-spreads.fs`](negative/anonymous-record-expression-with-multiple-spreads.fs)
- [`struct-anonymous-record-expression-with-a-spread.fs`](negative/struct-anonymous-record-expression-with-a-spread.fs)
- [`anonymous-record-expression-with-an-anonymous-record-as.fs`](negative/anonymous-record-expression-with-an-anonymous-record-as.fs)
- [`multiline-anonymous-record-expression-with-a-spread.fs`](negative/multiline-anonymous-record-expression-with-a-spread.fs)
- [`anonymous-record-expression-with-a-multiline-application-as.fs`](negative/anonymous-record-expression-with-a-multiline-application-as.fs)
- [`anonymous-record-expression-with-a-spread-stroustrup.fs`](negative/anonymous-record-expression-with-a-spread-stroustrup.fs)

## Spreads nested in other constructs

Written above these cases:

- [`spread-inside-a-computation-expression.fs`](negative/spread-inside-a-computation-expression.fs)
- [`spread-inside-a-lambda.fs`](negative/spread-inside-a-lambda.fs)
- [`spread-inside-a-quotation.fs`](negative/spread-inside-a-quotation.fs)
- [`record-expression-with-a-comment-after-the-spread.fs`](negative/record-expression-with-a-comment-after-the-spread.fs)
- [`anonymous-record-expression-with-a-comment-before-the-spread.fs`](negative/anonymous-record-expression-with-a-comment-before-the-spread.fs)
- [`anonymous-record-expression-with-a-comment-after-the-spread.fs`](negative/anonymous-record-expression-with-a-comment-after-the-spread.fs)
- [`record-expression-with-a-comment-before-the-spread.fs`](negative/record-expression-with-a-comment-before-the-spread.fs)
