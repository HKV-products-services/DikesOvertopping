REM This script is used for adding the GIT (short) hash to the properties of
REM resource files (.rc).
REM The script is called from the pre-build-events in visual studio projects.
REM Use like follow:
REM git_insert_hash.cmd <project dir> <in text file> <out text file>

@ECHO off

SET SEARCHTEXT=GITHASH
SET ORG_DIR=%CD%
SET INTEXTFILE=%2
SET OUTTEXTFILE=%3
SET TEMPTEXTFILE= %OUTTEXTFILE%.temp
CD /D %1

REM GET THE GIT SHORT HASH
FOR /f %%i in ('git rev-parse --short HEAD') do set GIT_HASH=%%i

REM REMOVE PREVIOUS TEMP FILE
IF EXIST %TEMPTEXTFILE% (
    DEL %TEMPTEXTFILE%
)

REM SUBSTITUTE THE GIT SHORT HASH IN TEMPLATE
FOR /F "tokens=* delims=" %%A IN ( '"type %INTEXTFILE%"') DO (
    SET string=%%A
    setlocal EnableDelayedExpansion
    SET modified=!string:%SEARCHTEXT%=%GIT_HASH%!
    ECHO !modified! >> %TEMPTEXTFILE%
    endlocal
)

REM COMPARE TEMP FILE WITH OUTFILE
FC /A /L %TEMPTEXTFILE% %OUTTEXTFILE%

IF %ERRORLEVEL% == 0 (
    REM IF THEY ARE IDENTICAL
    DEL %TEMPTEXTFILE%
) ELSE (
    REM IF DIFFERENT
    MOVE /Y %TEMPTEXTFILE% %OUTTEXTFILE%
)

CD %ORG_DIR%

:EOF