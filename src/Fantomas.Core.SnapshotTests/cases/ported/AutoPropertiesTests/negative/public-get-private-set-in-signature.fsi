module A

type X =
    new: unit -> X
    member internal Y: int with public get, private set
