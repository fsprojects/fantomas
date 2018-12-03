pushd ..\src\Fantomas.Cmd\bin\Release
Fantomas.exe  --profile --indent 2 ../../../../tests/languageshootout --out ../../../../tests/languageshootout_output
Fantomas.exe  --profile --indent 2 ../../../../tests/stackexchange --out ../../../../tests/stackexchange_output
popd