type System.String with
    member s.IsBlank = System.String.IsNullOrWhiteSpace s
