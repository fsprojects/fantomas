let genericSumUnits (x: float<'u>) (y: float<'u>) = x + y

type vector3D<[<Measure>] 'u> =
    { x: float<'u>
      y: float<'u>
      z: float<'u> }
