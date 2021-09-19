@echo off

set Template=JPPack_TEMPLATE.dpk
set Desc="JPPack VCL Components"
set BaseOutDir=..\Delphi
set ShortDpkName=JPPack.dpk

for %%x in (2009,2010,XE,XE2,XE3,XE4,XE5,XE6,XE7,XE8) do (
  DpkGen -t %Template% -d %Desc% -o %BaseOutDir%_%%x\%ShortDpkName% -s %%x
)

DpkGen -t %Template% -d %Desc% -o %BaseOutDir%_10.0_Seattle\%ShortDpkName% -s Seattle
DpkGen -t %Template% -d %Desc% -o %BaseOutDir%_10.1_Berlin\%ShortDpkName% -s Berlin
DpkGen -t %Template% -d %Desc% -o %BaseOutDir%_10.2_Tokyo\%ShortDpkName% -s Tokyo
DpkGen -t %Template% -d %Desc% -o %BaseOutDir%_10.3_Rio\%ShortDpkName% -s Rio
DpkGen -t %Template% -d %Desc% -o %BaseOutDir%_10.4_Sydney\%ShortDpkName% -s Sydney
DpkGen -t %Template% -d %Desc% -o %BaseOutDir%_11.0_Alexandria\%ShortDpkName% -s Alexandria


StrRep.exe "..\Delphi_2009\%ShortDpkName%" "JPP.FormIniStorage" "//JPP.FormIniStorage" 0 true first

