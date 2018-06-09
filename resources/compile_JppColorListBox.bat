@echo off

call globals
set BaseFileName=JppColorListBox


%brcc32% -fo%BaseFileName%.dcr %BaseFileName%.rc
copy %BaseFileName%.dcr %OutDir%

pause