(*---
max_line_length = 40
---*)
let c =
      builder
        .CaptureStartupErrors(true)
        .UseSerilog(dispose = true)
        .UseStartup<Startup>()
