(*---
fsharp_multiline_bracket_style = stroustrup
---*)
type LongerRecordName = {
    ...SomeSourceRecordType
    FirstAdditionalField: int
    SecondAdditionalField: string
}
