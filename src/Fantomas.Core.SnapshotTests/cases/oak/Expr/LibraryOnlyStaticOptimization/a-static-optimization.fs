let inline retype (x: 'T) : 'U = (# "" x : 'U #) when 'T : int = 0
