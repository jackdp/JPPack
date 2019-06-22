@echo off

call globals
set BaseFileName=JppColorListBox


%brcc32% -fo%BaseFileName%.dcr %BaseFileName%.rc
copy %BaseFileName%.dcr "%OutDir%"


imgconvert %BaseFileName%.bmp -o T%BaseFileName%.png -ov
%lazres% T%BaseFileName%.lrs T%BaseFileName%.png

pause