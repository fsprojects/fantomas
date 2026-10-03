(*---
max_line_length = 85
---*)
let init =
  addDateTimeConverter
    (fun dt -> Date(dt.Year, dt.Month, dt.Day))
    (fun (Date (y, m, d)) ->
      System.DateTime(y, m, d))
