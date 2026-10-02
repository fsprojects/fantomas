open System

type T() =
    interface IDisposable with
        override x.Dispose() = ()