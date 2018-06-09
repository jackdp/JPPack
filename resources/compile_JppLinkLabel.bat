@echo off

call globals
set BaseFileName=JppLinkLabel


%brcc32% -fo%BaseFileName%.dcr %BaseFileName%.rc
copy %BaseFileName%.dcr %OutDir%

pause