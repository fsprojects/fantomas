type Range =
    {
        From : float
        To : float
    }

    member this.Length = this.To - this.From
