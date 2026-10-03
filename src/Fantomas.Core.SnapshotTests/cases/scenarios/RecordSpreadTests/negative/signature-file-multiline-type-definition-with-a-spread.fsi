module Foo

type LongerRecordName =
    {
        ...SomeSourceRecordType
        FirstAdditionalField: int
        SecondAdditionalField: string
    }
