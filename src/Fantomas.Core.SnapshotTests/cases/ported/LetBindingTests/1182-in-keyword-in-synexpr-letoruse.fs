(*---
fsharp_max_if_then_else_short_width = 40
fsharp_max_infix_operator_expression = 50
---*)
do
    let _ = ()
      in
     () // note the different indent is allowed here due to `in` use

let escapeEarth myVelocity mySpeed =
    let
        escapeVelocityInKmPerSec = 11.186
    in
    if myVelocity > escapeVelocityInKmPerSec then
        "Godspeed"
    elif mySpeed == orbitalSpeedInKmPerSec then
        "Stay in orbit"
    else
        "Come back"
