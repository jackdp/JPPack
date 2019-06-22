@echo off

set brcc32="E:\Embarcadero\RAD Studio\9.0\bin\brcc32.exe"
set OutDir=..\packages\DCR
if not exist %OutDir% mkdir %OutDir%

set LazRes="E:\LAZ\fixes_20190429\lazarus\tools\lazres.exe"
set LazOutDir=.\