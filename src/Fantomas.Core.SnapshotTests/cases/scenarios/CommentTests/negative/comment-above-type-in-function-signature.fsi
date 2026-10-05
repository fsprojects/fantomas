/// Compile a pattern into a decision tree and a set of targets.
val internal CompilePattern:
    TcGlobals ->
    DisplayEnv ->
    Import.ImportMap ->
    (ValRef -> ValUseFlag -> TTypes -> range -> Expr * TType) ->
    InfoReader ->
    // range of the expression we are matching on
    range ->
        a
