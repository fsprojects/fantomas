#!/bin/bash

mono src/.nuget/nuget.exe install FAKE -OutputDirectory packages -ExcludeVersion

mono packages/FAKE/tools/FAKE.exe build.fsx $@
