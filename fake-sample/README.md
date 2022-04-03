# FAKE sample

1. Restore FAKE locally:

> dotnet tool restore

This was installed by:

> dotnet new tool-manifest

> dotnet tool install fake-cli

> dotnet tool install fantomas

2. Execute script:

> dotnet fake run script.fsx -t Format

Check the format afterwards:

> dotnet fake run script.fsx -t CheckFormat
