@ECHO OFF
rem runtests.bat --
rem     DOS batch file to control a program that uses ftnunit
rem     Name of the program: first argument
rem
rem     $Id: runtests.bat 8251 2017-07-24 09:26:59Z spee $
rem
set PATH=%IFORT_COMPILER21%\redist\ia32_win\compiler;%IFORT_COMPILER21%\redist\intel64_win\compiler;%PATH%
if exist runtests.log del runtests.log
if not exist %1 (
    echo Not existing: %1
    )
echo ALL >ftnunit.run

set errorlevel=0
:run
%*
if exist ftnunit.lst goto run

del ftnunit.run
if not exist ftnunit.html (
    echo No testresults found 1>&2
    set errorlevel=1
    )
