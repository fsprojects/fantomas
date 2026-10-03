(*---
max_line_length = 60
---*)
query.OfType<Customer>().Where(activePredicate).Cast<IEntityWithTimestamp>()
