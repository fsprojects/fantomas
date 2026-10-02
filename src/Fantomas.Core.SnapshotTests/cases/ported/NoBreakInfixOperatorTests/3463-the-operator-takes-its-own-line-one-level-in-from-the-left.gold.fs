let isPointInsidePolygon (polygon: Point list) (p: Point) =
    pairs
    |> Seq.filter (fun (pi, pj) ->
        ((pi.Latitude < p.Latitude && pj.Latitude >= p.Latitude)
         || (pj.Latitude < p.Latitude && pi.Latitude >= p.Latitude))
        && (pi.Longitude
            + (p.Latitude - pi.Latitude) / (pj.Latitude - pi.Latitude)
              * (pj.Longitude - pi.Longitude)
                <
                p.Longitude))
