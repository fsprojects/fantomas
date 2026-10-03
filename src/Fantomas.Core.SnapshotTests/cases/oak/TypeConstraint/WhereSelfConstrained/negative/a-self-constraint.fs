let inline parse<'T when IParsable<'T>> (text: string) = 'T.Parse(text, null)
