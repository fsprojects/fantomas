module Game

type Details = { Name: string; Description: string }

type Item = { Details: Details }

type Exit =
    | Passable of Details * desitnation: Room
    | Locked of Details * key: Item * next: Exit
    | NoExit of Details option

and Exits =
    { North: Exit
      South: Exit
      East: Exit
      West: Exit }

and Room =
    { Details: Details
      Items: Item list
      Exits: Exits }
