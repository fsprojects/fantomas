let hasUnEvenAmount regex line =
    (Regex.Matches(line, regex).Count
     - Regex.Matches(line, "\\\\" + regex).Count)
        %
        2
        =
        1
