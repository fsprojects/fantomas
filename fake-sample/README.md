# FAKE sample

Install FAKE locally:

`dotnet tool install --tool-path ".fake" fake-cli --add-source https://api.nuget.org/v3/index.json --framework netcoreapp2.1`

Execute script:

`.fake\fake.exe run script.fsx -t Format` or `.fake/fake run script.fsx -t Format`