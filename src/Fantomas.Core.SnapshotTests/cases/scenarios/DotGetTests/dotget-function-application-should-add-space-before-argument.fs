(*---
max_line_length = 70
---*)
m.Property(fun p -> p.Name).IsRequired().HasColumnName("ModelName").HasMaxLength 64
