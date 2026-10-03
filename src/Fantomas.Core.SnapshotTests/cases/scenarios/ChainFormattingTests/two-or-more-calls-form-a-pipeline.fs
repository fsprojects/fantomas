(*---
max_line_length = 60
---*)
serviceCollection.AddSingleton<IClock>(systemClock).AddOptions<MyOptions>(configureOptions)
