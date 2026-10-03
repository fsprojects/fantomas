type S = S of struct (int * int)
let g: struct (int * int) = struct (1, 1)
let z = fun (S(struct (u, v)): S) -> u + v
let t = struct (1, 2)

match t with
| struct (x, y) -> ()
