(*---
fsharp_multiline_bracket_style = cramped
fsharp_experimental_elmish = true
---*)
type Point =
    {
        /// Great comment
        X: int
        Y: int
    }

type Model = {
    Points: Point list
}
    
let view dispatch model =
    div
        []
        [
            h1 [] [ str "Some title" ]
            ul
                []
                [
                    for p in model.Points do
                        li [] [ str $"%i{p.X}, %i{p.Y}" ]
                ]
            hr []
        ]
        
let stillCramped = [
    // yow
    x ; y ; z
]
