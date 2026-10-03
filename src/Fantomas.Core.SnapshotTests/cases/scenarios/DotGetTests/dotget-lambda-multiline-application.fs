(*---
max_line_length = 50
---*)
m.Property(fun p -> p.Name).IsRequired().HasColumnName("ModelName").HasMaxLength
