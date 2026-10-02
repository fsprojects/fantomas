let inline f<'T when 'T: (static member StaticProperty: int with set)> () = 'T.set_StaticProperty (3)
