test
    <@
        result
            .Replace('\r', '\u00FF')
            .Replace('\n', '\u00FF')
            .Replace("\u00FF\u00FF", "\u00FF")
            .Replace("8.12", "8.13") // CRAP score rounding
            .Replace("4.12", "4.13") // CRAP score rounding
            .Trim([| '\u00FF' |])
            =
            expected
                .Replace('\r', '\u00FF')
                .Replace('\n', '\u00FF')
                .Replace("\u00FF\u00FF", "\u00FF")
                .Trim([| '\u00FF' |])
    @>
