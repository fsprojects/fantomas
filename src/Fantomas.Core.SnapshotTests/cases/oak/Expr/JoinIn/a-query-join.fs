let pairs =
    query {
        for customer in customers do
        join order in orders on (customer.Id = order.CustomerId)
        select (customer, order)
    }
