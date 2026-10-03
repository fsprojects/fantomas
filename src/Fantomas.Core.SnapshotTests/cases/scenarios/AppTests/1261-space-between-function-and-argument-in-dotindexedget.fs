type Queue<'T>(data: list<'T[]>, length: int) =

    member this.Head =
        if length > 0
        then (List.head data).[0]
        else raise (System.Exception("Queue is empty"))
