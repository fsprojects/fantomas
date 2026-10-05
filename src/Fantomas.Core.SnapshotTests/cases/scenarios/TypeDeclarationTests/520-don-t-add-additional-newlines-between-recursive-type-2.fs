(*---
fsharp_multiline_bracket_style = cramped
---*)
module Game

type Exit =
    | Passable of Details * desitnation: Room
    | Locked of Details * key: Item * next: Exit
    | NoExit of Details option

and Exits =
    { North: Exit
      South: Exit
      East: Exit
      West: Exit }

and [<Marker()>] Room =
    { Details: Details
      Items: Item list
      Exits: Exits }
