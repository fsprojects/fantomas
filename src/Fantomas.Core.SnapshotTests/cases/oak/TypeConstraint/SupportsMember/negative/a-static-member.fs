let inline zero< ^T when ^T: (static member Zero: ^T)> () = Unchecked.defaultof< ^T>
