(*---
fsharp_space_before_uppercase_invocation = true
---*)
let foo =
    c.P.Add(NpgsqlParameter ("day", NpgsqlTypes.NpgsqlDbType.Date)).Value <- query.Day.Date
    "fooo"
