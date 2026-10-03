(*---
max_line_length = 60
---*)
type SomeWin32Callback = delegate of NastyWinApi32Type * int * int * int * NastyWinApi32Type * int * int * int * int * NastyWinApi32Type * int * int -> bool
    