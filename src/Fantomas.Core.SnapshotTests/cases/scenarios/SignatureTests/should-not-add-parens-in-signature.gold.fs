type Route =
    { Verb: string
      Path: string
      Handler: Map<string, string> -> HttpListenerContext -> string }
    override x.ToString() = sprintf "%s %s" x.Verb x.Path
