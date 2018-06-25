@echo off

REM Usage: build [path/to/Rcmd.exe]

REM %0 is the program name as it was called,
REM %1 is the first command line parameter
SET compiler=%1
SET default="D:\R\bin\x64\Rcmd.exe"

REM echo %compiler%
REM echo %default%

REM overrides back the NAMESPACE file which is damaged by the roxygen2 package
echo exportPattern("^[[:alpha:]]+") > NAMESPACE

REM build R package
REM By default the R compiler is located at "D:\R\bin\x64\Rcmd.exe"
REM in the environment on my computer
if "%compiler%" == "" (
	CALL %default%  build ./
) else (
	CALL %compiler% build ./
)