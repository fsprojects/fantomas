namespace X

type MyRecord =
    { Level: int
      Progress: string
      Bar: string
      Street: string
      Number: int }

    member Score: unit -> int

type MyRecord =
    { SomeField: int }

    interface IMyInterface

type Color =
    | Red = 0
    | Green = 1
    | Blue = 2

    member ToInt: unit -> int
