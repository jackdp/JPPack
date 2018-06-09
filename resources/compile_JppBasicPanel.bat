@echo off

call globals
set BaseFileName=JppBasicPanel


%brcc32% -fo%BaseFileName%.dcr %BaseFileName%.rc
copy %BaseFileName%.dcr "%OutDir%"

pause