type A =
    abstract B: ?p1:(float * int) -> unit
    abstract C: ?p1:float * int -> unit
    abstract D: ?p1:(int -> int) -> unit
    abstract E: ?p1:float -> unit
    abstract F: ?p1:float * ?p2:float -> unit
    abstract G: p1:float * ?p2:float -> unit
    abstract H: float * ?p2:float -> unit
    