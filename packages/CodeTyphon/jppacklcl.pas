{ This file was automatically created by Typhon IDE. Do not edit!
  This source is only used to compile and install the package.
 }

unit jppacklcl;

{$warn 5023 off : no warning about unused units}
interface

uses
  JPP.AnchoredControls, JPP.BasicPanel, JPP.BasicSpeedButton, JPP.BrushStyleComboBox, JPP.ColorComboBox, JPP.ColorControls.Common, JPP.ColorListBox, 
  JPP.ColorSwatch, JPP.ComboBox, JPP.ComboBoxEx, JPP.Common, JPP.Common.Procs, JPP.DoubleLabel, JPP.DoubleLineLabel, JPP.Edit, JPP.Flash, 
  JPP.Gradient, JPP.Graphics, JPP.LinkLabel, JPP.Memo, JPP.Panel, JPP.PenStyleComboBox, JPP.PngCollection, JPP.SimplePanel, JPP.StorageCtrl, 
  JPP.StringStorageCtrl, JPP.Timer, JPP.Types, LDPngFunctions, CTJPPackRegister, JPP.ProgressBar, JPP.GPHatchStyleComboBox, JPP.FlipPanel, 
  JPP.Helpers, JPP.Labels, JPP.CheckBox, JPP.RadioButton, JPP.DateTimePicker, TyphonPackageIntf;

implementation

procedure Register;
begin
  RegisterUnit('CTJPPackRegister', @CTJPPackRegister.Register);
end;

initialization
  RegisterPackage('jppacklcl', @Register);
end.
