/// Represents reasons why a text document is saved.
type Meh =
    /// Manually triggered, e.g. by the user pressing save, by starting debugging,
    /// or by an API call.
    | Foo of int

    /// Automatic after a delay.
    | Bar of string

    /// When the editor lost focus.
    | Few of DateTime
