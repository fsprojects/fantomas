type DiagnosticsLogger with

    member ErrorR: exn: exn -> unit

    member Warning: exn: exn -> unit

    member Error: exn: exn -> 'T

    member SimulateError: diagnostic: PhasedDiagnostic -> 'T

    member ErrorRecovery: exn: exn -> m: range -> unit

    member StopProcessingRecovery: exn: exn -> m: range -> unit

    member ErrorRecoveryNoRange: exn: exn -> unit
