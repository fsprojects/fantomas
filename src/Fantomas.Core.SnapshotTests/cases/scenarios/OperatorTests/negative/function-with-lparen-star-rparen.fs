(*---
fsharp_max_infix_operator_expression = 50
---*)
let private distanceBetweenTwoPoints (latA, lngA) (latB, lngB) =
    if latA = latB && lngA = lngB then
        0.
    else
        let theta = lngA - lngB

        let dist =
            Math.Sin(deg2rad (latA))
            * Math.Sin(deg2rad (latB))
            + (Math.Cos(deg2rad (latA))
               * Math.Cos(deg2rad (latB))
               * Math.Cos(deg2rad (theta)))
            |> Math.Acos
            |> rad2deg
            |> (*) (60. * 1.1515 * 1.609344)

        dist
