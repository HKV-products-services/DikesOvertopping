setlocal enabledelayedexpansion
@ECHO off
rem ECHO "svn_insert_version.cmd started"

REM REMEMBER ORIGINAL WORKING FOLDER AND GO TO THE SOURCES DIRECTORY
SET ORG_DIR=%CD%
cd ..

SET argcount=0
FOR %%x IN (%*) DO SET /A argcount+=1

IF %argcount%==0 (
    ECHO "invalid arguments: [path] (infile) (outfile)"
    ECHO "usage: replace SVNREV text with revision number, SVNROOT with repository"
    ECHO "path: execute at specific location"
    ECHO "[optional] infile: file to read, defaults to AssemblyInfo.cs.svn"
    ECHO "[optional] outfile: file to create, defaults to AssemblyInfo.cs"
    GOTO EOF
)

rem ECHO "%argcount% arguments received"

IF %argcount%==1 (
    rem ECHO "setting default in and out files."
    SET INTEXTFILE=AssemblyInfo.cs.svn
    SET OUTTEXTFILE=AssemblyInfo.cs
) ELSE (
    rem ECHO "setting custom in and out files."
    SET INTEXTFILE=%2
    SET OUTTEXTFILE=%3
) 

rem ECHO "parsing svn info in directory %CD%"

REM GET THE SVN VERSION NUMBER AND REVISION PATH
FOR /f "tokens=1,* delims=¶" %%A IN ('svn info') DO (
    ECHO %%A | findstr /I /C:"Revision" && SET REV_BUF=%%A
    ECHO %%A | findstr /I "^URL" && SET ROOT_BUF=%%A
    ECHO %%A | findstr /I /C:"Repository Root" && SET AFTER_BUF=%%A
)

IF NOT DEFINED REV_BUF (
    SET SVN_REV=0
    SET SVN_ROOT=
) ELSE (
    SET SVN_REV=%REV_BUF:~18%
    SET SVN_ROOT=%ROOT_BUF:~5%
    SET "FIND=*%AFTER_BUF:~17%
    CALL SET SVN_ROOT=%%SVN_ROOT:!FIND!=%%
)

rem ECHO "using rev %SVN_REV% and root %SVN_ROOT%"

REM SUBSTITUTE THE VERSION NUMBER IN TEMPLATE
CD %1
rem ECHO "modifying %OUTTEXTFILE% in directory %1"
SET SEARCHTEXT=SVNREV
SET SEARCHROOT=SVNROOT
SET OUTPUTLINE=

IF EXIST %OUTTEXTFILE% (
    ECHO "removing %OUTTEXTFILE%"
    DEL %OUTTEXTFILE%
)

rem ECHO "writing new %OUTTEXTFILE%"

call powershell -Command "(gc %INTEXTFILE%) -replace '%SEARCHTEXT%', '%SVN_REV%' | Out-File -encoding ASCII %OUTTEXTFILE%

)

CD %ORG_DIR%
rem ECHO "svn_insert_version.cmd done"
rem timeout 1
:EOF
