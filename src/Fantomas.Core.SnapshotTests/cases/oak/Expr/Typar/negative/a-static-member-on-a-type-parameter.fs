let inline zero<'T when 'T: (static member Zero: 'T)> () = 'T.Zero
