type Subject<'a> private () =

    /// Represents and object that is both an observable sequence as well as an observer.
    /// Each notification is broadcasted to all subscribed observers.
    static member broadcast = new System.Reactive.Subjects.Subject<'a>()

type Subject<'a> private () =

    /// Represents and object that is both an observable sequence as well as an observer.
    /// Each notification is broadcasted to all subscribed observers.
    static member broadcast = new System.Reactive.Subjects.Subject<'a>()
