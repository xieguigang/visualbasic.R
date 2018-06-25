@echo off

SET compiler=%1
SET auto_ver="Utils\App\AutoVersion.exe"

REM please do not modify DESCRIPTION directly, modify DESCRIPTION.template 
REM file at first, and then DESCRIPTION file will be update automatically
%auto_ver% DESCRIPTION.template > DESCRIPTION

CALL "build.bat" %compiler%