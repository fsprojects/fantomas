(*---
max_line_length = 50
---*)
workstations
|> Seq.sumBy
    _.GetWeeklyValueWithoutAccessCheck(
        year,
        week,
        CapacityAggregateValueType.CostPrice,
        category
    )
