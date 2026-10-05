type SomeEvent = {
    Id: string
    Name: string
} with
    member x.BreakWithOtherStuffAs well = ()

type UpdatedName = { PreviousName: string }
