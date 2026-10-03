// right: @, ::, **, ^, := or starts with combinations
let allDecls =
    inheritsL
    @ iimplsLs
    @ ctorLs

let allDecls =
    inheritsL
    :: iimplsLs
    :: ctorLs

let allDecls =
    inheritsL
    ** iimplsLs
    ** ctorLs

let allDecls =
    inheritsL
    ^ iimplsLs
    ^ ctorLs

let allDecls =
    inheritsL
    ^^ iimplsLs
    ^^ ctorLs

let allDecls =
    inheritsL
    := iimplsLs
    := ctorLs

let allDecls =
    inheritsL
    @- iimplsLs
    @- ctorLs

let allDecls =
    inheritsL
    @+ iimplsLs
    @+ ctorLs
