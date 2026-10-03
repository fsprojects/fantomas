type NonEmptyList<'T> = private {
    List: 'T list
} with

    member this.Head = this.List.Head
    member this.Tail = this.List.Tail
    member this.Length = this.List.Length
