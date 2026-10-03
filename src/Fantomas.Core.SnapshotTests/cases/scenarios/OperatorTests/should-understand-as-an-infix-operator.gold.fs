try
    item.MethodInfo.Method.Invoke(null, ipa)
    |> (fun x -> x?Invoke (true))
    |> fun (t: Task) -> t.Wait()
with _ ->
    ()
