@echo off

call globals
set BaseFileName=JppBasicSpeedButton


%brcc32% -fo%BaseFileName%.dcr %BaseFileName%.rc
copy %BaseFileName%.dcr %OutDir%

pause