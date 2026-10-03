type CustomCancelSource() =
    interface IDisposable with
        member self.Dispose() =
            try
                self.Cancel()
            with :? ObjectDisposedException ->
                ()
            // TODO: cleanup also subscribed handlers?
