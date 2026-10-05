open System

type Exception with
    member inline __.FirstLine = __.Message.Split([| Environment.NewLine |], StringSplitOptions.RemoveEmptyEntries).[0]
