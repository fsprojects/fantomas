# Oak

One folder per union case of `SyntaxOak.fs`, `<Union>/<Case>/`, and one per node class that no union
case holds, `<Node>/`. Every folder has at least one case: a small input that contains the node,
either one formatting changes, with its gold, or one it leaves as it is, in `negative/`. They are a
starting point, not a specification of the node: a folder's cases grow as fixes add theirs.

Without a case, because none can be written:

- `TypeConstraint/DefaultsToType`: `default 'T : int` is only allowed inside FSharp.Core, and
  Fantomas refuses any other source that has it.
- `ExprConstant` and `String`: `ASTTransformer` builds neither node.
- `Oak`: the root of every case.

Two folders hold only an ignored case, because formatting loses code there today:
`MemberDefn/ExternBinding` and `StaticOptimizationConstraint/WhenTyparIsStruct`.
