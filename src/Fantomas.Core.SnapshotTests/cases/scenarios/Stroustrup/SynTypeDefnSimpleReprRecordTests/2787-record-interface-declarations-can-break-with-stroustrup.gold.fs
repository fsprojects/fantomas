type IEvent = interface end

type SomeEvent = {
    Id: string
    Name: string
} with
    interface IEvent

type UpdatedName = { PreviousName: string }
