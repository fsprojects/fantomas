/// The Computer Language Benchmarks Game
/// http://shootout.alioth.debian.org/
///
/// Contributed by Valentin Kraevskiy
module Nbody

let pi = 3.141592654

let daysPerYear = 365.24

let solarMass = 4.0 * pi ** 2.0

type Planet = 
  { mutable X : float
    mutable Y : float
    mutable Z : float
    mutable VX : float
    mutable VY : float
    mutable VZ : float
    Mass : float }

let jupiter = 
  { X = 4.841431442
    Y = -1.160320044
    Z = -0.1036220445
    VX = 0.001660076643 * daysPerYear
    VY = 0.007699011184 * daysPerYear
    VZ = -6.90460017e-05 * daysPerYear
    Mass = 0.0009547919384 * solarMass }

let saturn = 
  { X = 8.343366718
    Y = 4.124798564
    Z = -0.4035234171
    VX = -0.002767425107 * daysPerYear
    VY = 0.004998528012 * daysPerYear
    VZ = 2.304172976e-05 * daysPerYear
    Mass = 0.0002858859807 * solarMass }

let uranus = 
  { X = 12.89436956
    Y = -15.1111514
    Z = -0.2233075789
    VX = 0.002964601376 * daysPerYear
    VY = 0.00237847174 * daysPerYear
    VZ = -2.965895685e-05 * daysPerYear
    Mass = 4.366244043e-05 * solarMass }

let neptune = 
  { X = 15.37969711
    Y = -25.91931461
    Z = 0.179258773
    VX = 0.002680677725 * daysPerYear
    VY = 0.0016282417 * daysPerYear
    VZ = -9.515922545e-05 * daysPerYear
    Mass = 5.15138902e-05 * solarMass }

let sun = 
  { X = 0.0
    Y = 0.0
    Z = 0.0
    VX = 0.0
    VY = 0.0
    VZ = 0.0
    Mass = solarMass }

let offsetMomentum a = 
  let x, y, z = 
    Array.fold (fun (x, y, z) body -> 
      let c = body.Mass / solarMass
      (x + c * body.VX, y + c * body.VY, z + c * body.VZ)) (0.0, 0.0, 0.0) a
  a.[0].VX <- -x
  a.[0].VY <- -y
  a.[0].VZ <- -z

let move t = 
  Array.iter(fun body -> 
    body.X <- body.X + t * body.VX
    body.Y <- body.Y + t * body.VY
    body.Z <- body.Z + t * body.VZ)

let advance a t = 
  for i in 0..Array.length a - 2 do
    let b1 = a.[i]
    for j in i + 1..Array.length a - 1 do
      let b2 = a.[j]
      let dx, dy, dz = (b1.X - b2.X, b1.Y - b2.Y, b1.Z - b2.Z)
      let dist = sqrt(dx * dx + dy * dy + dz * dz)
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
  then 
    let b1 = a.[i]
    let rec energy' a j e = 
      if j < Array.length a
      then 
        let b2 = a.[j]
        let dx, dy, dz = (b1.X - b2.X, b1.Y - b2.Y, b1.Z - b2.Z)
        let dist = sqrt(dx * dx + dy * dy + dz * dz)
        energy' a (j + 1) (e - b1.Mass * b2.Mass / dist)
      else e
    let sq = b1.VX * b1.VX + b1.VY * b1.VY + b1.VZ * b1.VZ
    energy (i + 1) (energy' a (i + 1) (e + 0.5 * b1.Mass * sq)) a
  else e

let planets = [|sun; jupiter; saturn; uranus; neptune|]

offsetMomentum planets
let print = 
  energy 0 0.0
  >> printf "%.9f\n"

[<EntryPoint>]
let main args = 
  let n = 
    try 
      int args.[0]
    with
    | _ -> 20000000
  print planets
  for i in 1..n do
    advance planets 0.01
  print planets
  0
