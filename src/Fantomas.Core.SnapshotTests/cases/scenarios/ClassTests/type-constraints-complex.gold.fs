type Class4<'T when 'T: (static member staticMethod1: unit -> 'T)> = class end

type Class5<'T when 'T: (member Method1: 'T -> int)> = class end

type Class6<'T when 'T: (member Property1: int)> = class end

type Class7<'T when 'T: (new: unit -> 'T)>() =
    member val Field = new 'T()
