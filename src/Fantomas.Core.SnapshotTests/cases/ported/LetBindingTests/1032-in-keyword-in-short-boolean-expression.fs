(*---
fsharp_max_infix_operator_expression = 50
---*)
let internal sepSpace =
    // ignore multiple spaces, space on start of file, after newline
    // TODO: this is inefficient - maybe remember last char written?
    fun (ctx: Context) ->
        if (not ctx.WriterInitModel.IsDummy && let s = dump ctx in s = "" || s.EndsWith " " || s.EndsWith Environment.NewLine) then ctx
        else (!- " ") ctx
