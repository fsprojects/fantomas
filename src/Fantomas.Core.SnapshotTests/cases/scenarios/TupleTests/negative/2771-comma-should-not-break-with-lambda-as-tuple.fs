let shiftTimes localDate (start: Utc, duration) =
    ZonedDate.create TimeZone.current localDate
    |> Time.ZonedDate.startOf
    |> fun dayStart -> start + dayStart.Duration - refDay.StartTime.Duration
    , duration
