let myRegexMatch = Regex.matches (input, regex)

let myRegexMatchLong =
    Regex.matches (
        "my longer input string with some interesting content in it",
        "myRegexPattern"
    )

let untypedRes = checker.parseFile (file, source, opts)

let untypedResLong =
    checker.parseFile (
        fileName,
        sourceText,
        parsingOptionsWithDefines,
        somethingElseWithARatherLongVariableName
    )
