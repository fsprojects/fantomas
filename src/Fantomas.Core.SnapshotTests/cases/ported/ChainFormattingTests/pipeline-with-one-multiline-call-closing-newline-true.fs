(*---
max_line_length = 70
fsharp_multi_line_lambda_closing_newline = true
---*)
repo.Where(fun customer -> customer.IsActive && customer.Region = targetRegion).Select(projector).ToList()
