#!/bin/bash

mono zrc/.nuget/nuget.exe install FAKE -OutputDirectory packages -ExcludeVersion

mono packages/FAKE/tools/FAKE.exe build.fsx $@
