@echo off
cls
if not exist packages\FAKE\tools\Fake.exe (
  src\.nuget\nuget.exe install FAKE -OutputDirectory packages -ExcludeVersion
)
packages\FAKE\tools\FAKE.exe build.fsx %*