type Event<'Delegate, 'Args when 'Delegate: delegate<'Args, unit> and 'Delegate :> System.Delegate and 'Delegate: not struct> () =
                        class end
