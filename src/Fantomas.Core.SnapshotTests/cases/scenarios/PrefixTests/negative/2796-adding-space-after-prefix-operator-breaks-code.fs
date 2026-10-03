let inline (~%%) id = int id

let f a b = a + b

let foo () = f %%"17" %%"42"
