let inline NonStructural<'TInput
    when 'TInput: (static member (<): 'TInput * 'TInput -> bool)
    and 'TInput: (static member (>): 'TInput * 'TInput -> bool)> : IComparer<'TInput> =
    ()
