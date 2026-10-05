module Services =
    /// Builds a Stream Resolve function appropriate to the store being used
    type StreamResolver(storage: Storage.Instance) =
        member __.Resolve
            (
                codec: FsCodec.IEventCodec<'event, byte[], _>,
                fold: ('state -> 'event seq -> 'state),
                initial: 'state,
                snapshot: (('event -> bool) * ('state -> 'event))
            )
            =
            match storage with
            | Storage.MemoryStore store ->
                Equinox.MemoryStore.Resolver(store, FsCodec.Box.Codec.Create(), fold, initial).Resolve
            | Storage.EventStore (gateway, cache) ->
                let accessStrategy =
                    Equinox.EventStore.AccessStrategy.RollingSnapshots snapshot

                let cacheStrategy =
                    Equinox.EventStore.CachingStrategy.SlidingWindow(cache, TimeSpan.FromMinutes 20.)

                Equinox.EventStore.Resolver<'event, 'state, _>(gateway, codec, fold, initial, cacheStrategy, accessStrategy).Resolve
