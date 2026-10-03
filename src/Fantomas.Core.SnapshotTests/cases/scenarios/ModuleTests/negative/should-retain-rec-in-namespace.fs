namespace rec Test

type Add = Expr * Expr

type Expr =
    | Add of Add
    | Value of int
