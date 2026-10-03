repo
    .Where(fun customer ->
        customer.IsActive && customer.Region = targetRegion
    )
    .Select(projector)
    .ToList()
