(*---
fsharp_multiline_bracket_style = cramped
---*)
{ new TaskDefinition with
    member this.Item
        with get (name: string): obj option = data.TryGet name


        and set (name: string) (v: obj option): unit =
            match v with
            | None -> data.Remove(name) |> ignore
            | Some v -> data.[name] <- v

    override this.``type``: string = "fakerun" }
