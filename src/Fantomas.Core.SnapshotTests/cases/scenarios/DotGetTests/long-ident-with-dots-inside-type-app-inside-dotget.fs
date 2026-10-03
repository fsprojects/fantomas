(*---
max_line_length = 100
---*)
                Equinox.EventStore.Resolver<'event, 'state, _>(gateway,
                                                               codec,
                                                               fold,
                                                               initial,
                                                               cacheStrategy,
                                                               accessStrategy)
                    .Resolve
