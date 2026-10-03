do let _ = () in () // note the different indent is allowed here due to `in` use

let escapeEarth myVelocity mySpeed =
    let escapeVelocityInKmPerSec = 11.186 in

    if myVelocity > escapeVelocityInKmPerSec then
        "Godspeed"
    elif mySpeed == orbitalSpeedInKmPerSec then
        "Stay in orbit"
    else
        "Come back"
