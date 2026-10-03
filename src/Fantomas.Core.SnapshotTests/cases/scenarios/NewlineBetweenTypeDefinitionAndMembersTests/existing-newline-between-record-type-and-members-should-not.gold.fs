type Range =
    { From: float
      To: float
      Name: string }

    member this.Length = this.To - this.From
