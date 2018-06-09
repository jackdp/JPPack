@echo off

call globals
set BaseFileName=JppColorComboBox


%brcc32% -fo%BaseFileName%.dcr %BaseFileName%.rc
copy %BaseFileName%.dcr %OutDir%

pause