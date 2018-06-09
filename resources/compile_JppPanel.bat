@echo off

call globals
set BaseFileName=JppPanel


%brcc32% -fo%BaseFileName%.dcr %BaseFileName%.rc
copy %BaseFileName%.dcr %OutDir%

pause