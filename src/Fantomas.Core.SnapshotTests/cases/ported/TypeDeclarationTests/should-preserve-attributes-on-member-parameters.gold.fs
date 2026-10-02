type ILogger =
    abstract DebugFormat: format: String * [<ParamArray>] args: Object[] -> unit
