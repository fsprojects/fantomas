(*---
max_line_length = 70
---*)
repo.Where(fun customer -> customer.IsActive && customer.Region = targetRegion).Select(projector).ToList()
