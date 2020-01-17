@echo off

set brcc32="E:\Embarcadero\RAD Studio\9.0\bin\brcc32.exe"
set OutDir=..\packages\DCR
if not exist %OutDir% mkdir %OutDir%

set LazRes="C:\LAZ\fixes_20190111\lazarus\tools\lazres.exe"
set LazOutDir=.\

set CTRes="C:\codetyphon\typhon\bin32\ctres.exe"
set CTOutDir=.\CodeTyphon
