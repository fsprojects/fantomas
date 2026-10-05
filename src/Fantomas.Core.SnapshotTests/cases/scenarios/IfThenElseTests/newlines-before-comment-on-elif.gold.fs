if strA.Length = 0 && strB.Length = 0 then
    0

// OPTIMIZATION : If the substrings have the same (identical) underlying string
// and offset, the comparison value will depend only on the length of the substrings.
elif strA.String == strB.String && strA.Offset = strB.Offset then
    compare strA.Length strB.Length

else
    -1
