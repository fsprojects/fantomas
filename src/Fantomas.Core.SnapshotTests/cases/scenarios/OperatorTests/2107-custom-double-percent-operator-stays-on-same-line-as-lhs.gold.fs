let inline (%%) x y = ((x % y) + y) % y
let aVeryLongVariableNameThatForceLineBreaking = 0

let a =
    (if aVeryLongVariableNameThatForceLineBreaking = 0 then
         1
     else
         -1)
        %%
        4
