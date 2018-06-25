@echo off

REM overrides back the NAMESPACE file which is damaged by the roxygen2 package
echo exportPattern("^[[:alpha:]]+") > NAMESPACE

"D:\R\bin\x64\Rcmd.exe" build ./