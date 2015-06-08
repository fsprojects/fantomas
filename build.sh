#!/bin/bash

if [ ! -e packages/FAKE/tools/FAKE.exe ]; then 
  mono src/.nuget/NuGet.exe install FAKE -OutputDirectory packages -ExcludeVersion
fi

mono packages/FAKE/tools/FAKE.exe $@ --fsiargs -d:MONO build.fsx