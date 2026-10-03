let pattern =
    (x + y)
        .Replace(
            seperator + "**" + seperator,
            replacementSeparator + "(.|?" + replacementSeparator + ")?"
        )
        .Replace("**" + seperator, ".|(?<=^|" + replacementSeparator + ")")
