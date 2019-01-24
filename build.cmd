if not exist .paket/paket.exe dotnet tool install --tool-path ".paket" Paket --add-source https://api.nuget.org/v3/index.json
if not exist .fake/fake.exe dotnet tool install --tool-path ".fake" fake-cli --add-source https://api.nuget.org/v3/index.json
.fake\fake.exe run build.fsx %*