(*---
fsharp_multiline_bracket_style = cramped
---*)
                let errs =
                    (*[omit:(copying of errors omitted)]*)
                    seq {
                        for e in res.Errors ->
                            { StartColumn = e.StartColumn
                              StartLine = e.StartLine
                              Message = e.Message
                              IsError = e.Severity = Error
                              EndColumn = e.EndColumn
                              EndLine = e.EndLine }
                    } (*[/omit]*)
