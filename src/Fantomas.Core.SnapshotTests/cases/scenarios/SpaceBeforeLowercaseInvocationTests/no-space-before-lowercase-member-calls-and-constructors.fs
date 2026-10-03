(*---
max_line_length = 60
fsharp_space_before_lowercase_invocation = false
---*)
let tree1 =
    binaryNode(binaryNode(binaryValue 1, binaryValue 2), binaryNode(binaryValue 3, binaryValue 4))

let person = new person("Jim", 33)
let otherThing =
    new foobar(longname1, longname2, longname3, longname4, longname5, longname6, longname7)

let myRegexMatch = Regex.matches(input, regex)

let myRegexMatchLong =
    Regex.matches("my longer input string with some interesting content in it","myRegexPattern")

let untypedRes = checker.parseFile(file, source, opts)

let untypedResLong =
    checker.parseFile(fileName, sourceText, parsingOptionsWithDefines, somethingElseWithARatherLongVariableName)
