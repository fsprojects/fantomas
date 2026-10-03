if
    sourceCode.EndsWith("\n")
    && not
       <| formattedSourceCode.EndsWith(Environment.NewLine)
then
    return formattedSourceCode + Environment.NewLine
elif
    not <| sourceCode.EndsWith("\n")
    && formattedSourceCode.EndsWith(Environment.NewLine)
then
    return formattedSourceCode.TrimEnd('\r', '\n')
else
    return formattedSourceCode
