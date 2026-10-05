m
    .Property(fun p -> p.Name)
    .IsRequired()
    .HasColumnName("ModelName")
    .HasMaxLength
    64
