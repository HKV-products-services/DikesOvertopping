echo off

echo solution dir	: %1
echo solution file	: %2
echo Configuration name	: %3
echo VS version		: %4
echo Platform (Win32/x64): %5
echo Project name	: VTV Dikes Overtopping

IF %4==VS2012 GOTO VS2012

set devenv_path="c:\Program Files\Microsoft Visual Studio 9.0\Common7\IDE"
IF EXIST "C:\Program Files (x86)\" set devenv_path="c:\Program Files (x86)\Microsoft Visual Studio 9.0\Common7\IDE"
GOTO COMPILE

:VS2012
set devenv_path="c:\Program Files\Microsoft Visual Studio 11.0\Common7\IDE"
IF EXIST "C:\Program Files (x86)\" set devenv_path="c:\Program Files (x86)\Microsoft Visual Studio 11.0\Common7\IDE"

:COMPILE
echo devenv		: %devenv_path%

IF EXIST build.log del build.log
echo on
echo clean build
%devenv_path%\devenv.exe "%1\%2" /Clean "%3|%5" /Out build.log
%devenv_path%\devenv.exe "%1\%2" /Build "%3|%5" /Out build.log
type build.log
