let a =
        // check if the current # char is part of an define expression
        // if so add to defines
        let captureHashDefine idx =
                if trimmed.StartsWith("#if")
                then defines.Add(processLine "#if" trimmed lineNumber offset)
                elif trimmed.StartsWith("#elseif")
                then defines.Add(processLine "#elseif" trimmed lineNumber offset)
                elif trimmed.StartsWith("#else")
                then defines.Add(processLine "#else" trimmed lineNumber offset)
                elif trimmed.StartsWith("#endif")
                then defines.Add(processLine "#endif" trimmed lineNumber offset)

        for idx in [ 0 .. lastIndex ] do
            let zero = sourceCode.[idx]
            let plusOne = sourceCode.[idx + 1]
            let plusTwo = sourceCode.[idx + 2]
            ()
