(*---
fsharp_multiline_bracket_style = cramped
---*)
let useGeolocation : unit ->
    {| latitude: float
       longitude: float
       loading: bool
       error: obj option |} =
        import "useGeolocation" "react-use"

type Viewport =
    { width: string
      height: string
      latitude: float
      longitude: float
      zoom: int }
