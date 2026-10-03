(*---
fsharp_space_before_uppercase_invocation = true
fsharp_space_before_member = true
---*)
let x =
                        LoggerConfiguration<Foo>()
                            .Enrich.WithProperty<Bar>("user", Environment.UserName)
                            .Enrich.WithProperty ("application", context.HostingEnvironment.ApplicationName)
