(*---
fsharp_max_array_or_list_width = 40
fsharp_experimental_elmish = true
---*)
table [ ClassName "table table-striped table-hover mb-0" ]
              [ tbody []
                    [ tokenDetailRow "TokenName" (str tokenName)
                      tokenDetailRow "LeftColumn" (ofInt leftColumn)
                      tokenDetailRow "RightColumn" (ofInt rightColumn)
                      tokenDetailRow "Content" (pre [] [ code [] [ str token.Content ] ])
                      tokenDetailRow "ColorClass" (str colorClass)
                      tokenDetailRow "CharClass" (str charClass)
                      tokenDetailRow "Tag" (ofInt tag)
                      tokenDetailRow "FullMatchedLength"
                          (span [ ClassName "has-text-weight-semibold" ] [ ofInt fullMatchedLength ]) ] ]
