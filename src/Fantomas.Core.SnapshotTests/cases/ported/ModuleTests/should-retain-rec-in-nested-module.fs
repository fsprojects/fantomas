namespace Test

module rec Expression =
    type Add = Expr * Expr

    type Expr =
        | Add of Add
        | Value of int
    