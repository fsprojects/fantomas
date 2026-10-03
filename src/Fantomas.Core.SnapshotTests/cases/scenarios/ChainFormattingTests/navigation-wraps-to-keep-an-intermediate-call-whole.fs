(*---
max_line_length = 60
---*)
builder.Connect(hostName).Configuration.Database.PrimaryConnection.Settings.Apply(spec).Build()
