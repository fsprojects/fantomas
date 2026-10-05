type NonEmptyList<'T> = private {
    List: 'T list
    Value: 'T
    Third: string
}
