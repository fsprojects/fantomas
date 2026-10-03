query
    .OfType<Customer>()
    .Where(activePredicate)
    .Cast<IEntityWithTimestamp>()
