type ISingleExpressionValue<'p, 'o, 'v
    when 'p :> IProperty and 'o :> IOperator and 'p: equality and 'o: equality and 'v: equality>() =
    abstract Property: 'p
    abstract Operator: 'o
    abstract Value: 'v
