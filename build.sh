#!/bin/bash
if [ ! -f .paket/paket ]; then
    dotnet tool install --tool-path ".paket" Paket --add-source https://api.nuget.org/v3/index.json
fi
if [ ! -f .fake/fake ]; then
    dotnet tool install --tool-path ".fake" fake-cli --add-source https://api.nuget.org/v3/index.json
fi
.fake/fake run build.fsx $@
