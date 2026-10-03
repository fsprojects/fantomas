(*---
max_line_length = 90
fsharp_space_before_uppercase_invocation = true
---*)
let myRegexMatch = Regex.Match(input, regex)

let myRegexMatchLong =
    Regex.Match("my longer input string with some interesting content in it","myRegexPattern")

let untypedRes = checker.ParseFile(file, source, opts)

let untypedResLong =
    checker.ParseFile(fileName, sourceText, parsingOptionsWithDefines, somethingElseWithARatherLongVariableName)
