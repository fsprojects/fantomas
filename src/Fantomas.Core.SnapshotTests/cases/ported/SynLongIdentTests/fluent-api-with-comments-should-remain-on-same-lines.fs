Log.Logger <-
  LoggerConfiguration()
   // Suave.SerilogExtensions has native destructuring mechanism
   // this helps Serilog deserialize the fsharp types like unions/records
   .Destructure.FSharpTypes()
   // use package Serilog.Sinks.Console
   // https://github.com/serilog/serilog-sinks-console
   .WriteTo.Console()
   // add more sinks etc.
   .CreateLogger()