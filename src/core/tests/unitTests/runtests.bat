@ECHO OFF
rem runtests.bat --
rem     DOS batch file to control a program that uses ftnunit
rem     Name of the program: first argument
rem
rem     $Id: runtests.bat 2352 2012-11-29 10:02:28Z xlin $
rem
if exist runtests.log del runtests.log
echo ALL >ftnunit.run

:run
%1 %2 %3 %4 %5 %6 %7 %8 %9 >runtests.out 2>runtests.err
type runtests.out
type runtests.out >>runtests.log
type runtests.err >>runtests.log
if exist ftnunit.lst goto run

del ftnunit.run
del runtests.out
del runtests.err
