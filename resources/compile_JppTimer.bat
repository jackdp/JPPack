@echo off

call globals
set BaseFileName=JppTimer


%brcc32% -fo%BaseFileName%.dcr %BaseFileName%.rc
copy %BaseFileName%.dcr %OutDir%

pause