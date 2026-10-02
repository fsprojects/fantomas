(*---
fsharp_max_infix_operator_expression = 50
---*)
        if startWithMember sel then
            (String.Join(String.Empty, "type T = ", Environment.NewLine, String(' ', startCol), sel), TypeMember)
        elif String.startsWithOrdinal "and" (sel.TrimStart()) then
            // Replace "and" by "type" or "let rec"
            if startLine = endLine then
                (pattern.Replace(sel, replacement, 1), p)
            else
                (String(' ', startCol)
                 + pattern.Replace(sel, replacement, 1),
                 p)
        elif startLine = endLine then
            (sel, Nothing)
        else
            failAndExit ()
