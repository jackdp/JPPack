{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit jppacklcl;

{$warn 5023 off : no warning about unused units}
interface

uses
  JPP.Common, JPP.Common.Procs, JPP.DoubleLabel, JPP.Gradient, JPP.Graphics, JPP.Types, LazJPPackRegister, JPP.DoubleLineLabel, JPP.LinkLabel, JPP.MemIniFile, 
  JPP.StorageCtrl, JPP.StringStorageCtrl, JPP.PngCollection, JPP.Timer, JPP.BasicPanel, JPP.Panel, JPP.BasicSpeedButton, LDPngFunctions, 
  JPP.ColorControls.Common, JPP.ColorListBox, JPP.ColorComboBox, JPP.ColorSwatch, JPP.Edit, JPP.SimplePanel, JPP.Flash, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('LazJPPackRegister', @LazJPPackRegister.Register);
end;

initialization
  RegisterPackage('jppacklcl', @Register);
end.
