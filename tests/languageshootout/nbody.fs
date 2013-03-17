/// The Computer Language Benchmarks Game
/// http://shootout.alioth.debian.org/
///
/// Contributed by Valentin Kraevskiy

let pi = 3.141592653589793
let daysPerYear = 365.24
let solarMass = 4.0 * pi ** 2.0

type Planet =
    {mutable X: float; mutable Y: float; mutable Z: float
     mutable VX: float; mutable VY: float; mutable VZ: float
     Mass: float}

let jupiter =
    {X = 4.84143144246472090e+00
     Y = -1.16032004402742839e+00
     Z = -1.03622044471123109e-01
     VX = 1.66007664274403694e-03 * daysPerYear
     VY = 7.69901118419740425e-03 * daysPerYear
     VZ = -6.90460016972063023e-05 * daysPerYear
     Mass = 9.54791938424326609e-04 * solarMass}

let saturn =
    {X = 8.34336671824457987e+00
     Y = 4.12479856412430479e+00
     Z = -4.03523417114321381e-01
     VX = -2.76742510726862411e-03 * daysPerYear
     VY = 4.99852801234917238e-03 * daysPerYear
     VZ = 2.30417297573763929e-05 * daysPerYear
     Mass = 2.85885980666130812e-04 * solarMass}

let uranus =
    {X = 1.28943695621391310e+01
     Y = -1.51111514016986312e+01
     Z = -2.23307578892655734e-01
     VX = 2.96460137564761618e-03 * daysPerYear
     VY = 2.37847173959480950e-03 * daysPerYear
     VZ = -2.96589568540237556e-05 * daysPerYear
     Mass = 4.36624404335156298e-05 * solarMass}

let neptune =
    {X = 1.53796971148509165e+01
     Y = -2.59193146099879641e+01
     Z = 1.79258772950371181e-01
     VX = 2.68067772490389322e-03 * daysPerYear
     VY = 1.62824170038242295e-03 * daysPerYear
     VZ = -9.51592254519715870e-05 * daysPerYear
     Mass = 5.15138902046611451e-05 * solarMass}

let sun =
    {X = 0.0; Y = 0.0; Z = 0.0
     VX = 0.0; VY = 0.0; VZ = 0.0
     Mass = solarMass}

let offsetMomentum a =
    let x, y, z =
        Array.fold (fun (x, y, z) body ->
            let c = body.Mass / solarMass
            (x + c * body.VX, y + c * body.VY, z + c * body.VZ))
            (0.0, 0.0, 0.0) a
    a.[0].VX <- - x
    a.[0].VY <- - y
    a.[0].VZ <- - z

let move t =
    Array.iter (fun body ->
        body.X <- body.X + t * body.VX
        body.Y <- body.Y + t * body.VY
        body.Z <- body.Z + t * body.VZ)

let advance a t =
    for i in 0..Array.length a - 2 do
        let b1 = a.[i]
        for j in i + 1..Array.length a - 1 do
            let b2 = a.[j]
            let dx, dy, dz = b1.X - b2.X, b1.Y - b2.Y, b1.Z - b2.Z
            let dist = sqrt (dx * dx + dy * dy + dz * dz)
            let mag = t / (dist * dist * dist)
            b1.VX <- b1.VX - b2.Mass * mag * dx
            b1.VY <- b1.VY - b2.Mass * mag * dy
            b1.VZ <- b1.VZ - b2.Mass * mag * dz
            b2.VX <- b2.VX + b1.Mass * mag * dx
            b2.VY <- b2.VY + b1.Mass * mag * dy
            b2.VZ <- b2.VZ + b1.Mass * mag * dz
    move t a

let rec energy i e a =
    if i < Array.length a
        then let b1 = a.[i]
             let rec energy' a j e =
                if j < Array.length a
                then let b2 = a.[j]
                     let dx, dy, dz = b1.X - b2.X, b1.Y - b2.Y, b1.Z - b2.Z
                     let dist = sqrt (dx * dx + dy * dy + dz * dz)
                     energy' a (j + 1)
                        (e - b1.Mass * b2.Mass / dist )
                else e

             let sq = b1.VX * b1.VX + b1.VY * b1.VY + b1.VZ * b1.VZ
             energy (i + 1)
                (energy' a (i + 1)
                    (e + 0.5 * b1.Mass * sq)) a
        else e

let planets = [|sun; jupiter; saturn; uranus; neptune|]
offsetMomentum planets

let print = energy 0 0.0 >> printf "%.9f\n"

[<EntryPoint>]
let main args =
    let n = try int args.[0] with _ -> 20000000
    print planets
    for i in 1..n do advance planets 0.01
    print planets
    0
