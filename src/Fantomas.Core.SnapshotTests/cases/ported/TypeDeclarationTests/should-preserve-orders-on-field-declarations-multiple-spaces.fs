type CustomGraphControl() =
    inherit UserControl()
    [<DefaultValue      (false)>]
    static val mutable private GraphProperty : DependencyProperty
    