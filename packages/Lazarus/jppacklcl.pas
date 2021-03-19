{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit jppacklcl;

{$warn 5023 off : no warning about unused units}
interface

uses
  JPP.AnchoredControls, JPP.BasicPanel, JPP.BasicSpeedButton, JPP.BrushStyleComboBox, JPP.Common, JPP.Common.Procs, JPP.DoubleLabel, 
  JPP.Gradient, JPP.Graphics, JPP.Types, LazJPPackRegister, JPP.ColorComboBox, JPP.DoubleLineLabel, JPP.LinkLabel, JPP.StorageCtrl, 
  JPP.StringStorageCtrl, JPP.PngCollection, JPP.Timer, JPP.Panel, LDPngFunctions, JPP.ColorControls.Common, JPP.ColorListBox, 
  JPP.ColorSwatch, JPP.Edit, JPP.SimplePanel, JPP.Flash, JPP.PenStyleComboBox, JPP.GPHatchStyleComboBox, JPP.ComboBox, JPP.Memo, 
  JPP.ComboBoxEx, JPP.ProgressBar, JPP.FlipPanel, JPP.Helpers, JPP.BasicPngButtonEx, JPP.Labels, JPP.CheckBox, JPP.RadioButton, 
  JPP.DateTimePicker, LazarusPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('LazJPPackRegister', @LazJPPackRegister.Register);
end;

initialization
  RegisterPackage('jppacklcl', @Register);
end.
