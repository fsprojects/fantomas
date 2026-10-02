/// Fold over the array passing the index and element at that index to a folding function
let foldi (folder: 'State -> int -> 'T -> 'State) (state: 'State) (array: 'T[]) =
    checkNonNull "array" array

    if array.Length = 0 then
        state
    else
        let folder = OptimizedClosures.FSharpFunc<_, _, _, _>.Adapt folder

        let mutable state: 'State = state
        let len = array.Length

        for i = 0 to len - 1 do
            state <- folder.Invoke(state, i, array.[i])

        state
