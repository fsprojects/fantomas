type F =
    abstract G: int list -> Map<int,int>

let x: F =
    {new F with
        member __.G _ = Map.empty}

x.G[].TryFind 3
