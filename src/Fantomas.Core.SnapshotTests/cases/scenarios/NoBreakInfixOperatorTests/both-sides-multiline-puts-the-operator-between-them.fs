(*---
max_line_length = 80
---*)
let v = xs.ReplaceEverythingEverywhere(11111, 22222).ReplaceEverythingEverywhere(33333, 44444).TrimEnd() = expected.ReplaceEverythingEverywhere(55555, 66666).ReplaceEverythingEverywhere(77777, 88888).TrimEnd()
