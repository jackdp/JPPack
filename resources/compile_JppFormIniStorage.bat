@echo off

call globals
set BaseFileName=JppFormIniStorage


%brcc32% -fo%BaseFileName%.dcr %BaseFileName%.rc
copy %BaseFileName%.dcr %OutDir%


pause
