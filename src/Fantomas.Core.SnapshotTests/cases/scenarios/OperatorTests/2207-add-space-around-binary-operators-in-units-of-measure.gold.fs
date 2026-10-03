type Test =
    { WorkHoursPerWeek: uint<hr * (staff weeks)> }
    static member create = { WorkHoursPerWeek = 40u<hr * (staff weeks)> }
