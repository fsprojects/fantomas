let fieldColor (fieldNameX: string) =
  (if f.errors?(fieldNameY) && f.touched?(fieldNameZ) then
     IsDanger
   else
     NoColor)
  |> Input.Color
