let examineData x =
    match data with
    | OnePartData(part1 = p1) -> p1
    | TwoPartData(part1 = p1; part2 = p2) -> p1 + p2
