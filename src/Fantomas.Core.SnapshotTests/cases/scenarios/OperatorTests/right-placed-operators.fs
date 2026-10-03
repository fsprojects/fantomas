(*---
fsharp_max_infix_operator_expression = 0
---*)
  // right: @, ::, **, ^, := or starts with combinations
  let allDecls = inheritsL @ iimplsLs @ ctorLs 
  let allDecls = inheritsL :: iimplsLs :: ctorLs
  let allDecls = inheritsL ** iimplsLs ** ctorLs
  let allDecls = inheritsL ^ iimplsLs ^ ctorLs
  let allDecls = inheritsL ^^ iimplsLs ^^ ctorLs
  let allDecls = inheritsL := iimplsLs := ctorLs
  let allDecls = inheritsL @- iimplsLs @- ctorLs 
  let allDecls = inheritsL @+ iimplsLs @+ ctorLs 
