unit CTJPPackRegister;

{$mode objfpc}{$H+}
{$I jpp.inc}

interface

uses
  Forms, LCLIntf, LCLType, LMessages, Classes, Graphics, Controls, SysUtils, StdCtrls,
  TypInfo, lresources,
  JPP.Common, JPP.Common.Procs, JPP.DoubleLabel, JPP.DoubleLineLabel,
  JPP.Labels, JPP.LinkLabel,
  JPP.StorageCtrl, JPP.StringStorageCtrl, JPP.PngCollection, JPP.Timer, JPP.BasicPanel, JPP.Panel,
  JPP.BasicSpeedButton, JPP.ColorListBox, JPP.ComboBox, JPP.ComboBoxEx, JPP.ColorComboBox, JPP.ColorSwatch, JPP.SimplePanel,
  JPP.Edit, JPP.Memo, JPP.FlipPanel,
  JPP.BrushStyleComboBox, JPP.PenStyleComboBox, JPP.ProgressBar,
  JPP.CheckBox, JPP.RadioButton, JPP.DateTimePicker,
  {$IFDEF USE_GDIPLUS_CONTROLS}
  JPP.GPHatchStyleComboBox,
  {$ENDIF}
  //JPP.PngButton, JPP.PngButton.ColorMaps,
  JPP.Types
  ;

procedure Register;

implementation

procedure Register;

begin
  RegisterComponents(JPPackPageName, [TJppDoubleLabel]);
  RegisterComponents(JPPackPageName, [TJppDoubleLineLabel]);
  RegisterComponents(JPPackPageName, [TJppLabel, TJppShadowLabel, TJppLinkLabel]);
  RegisterComponents(JPPackPageName, [TJppStorageCtrl]);
  RegisterComponents(JPPackPageName, [TJppStringStorageCtrl]);
  RegisterComponents(JPPackPageName, [TJppPngCollection]);
  RegisterComponents(JPPackPageName, [TJppTimer]);
  RegisterComponents(JPPackPageName, [TJppBasicPanel]);
  RegisterComponents(JPPackPageName, [TJppPanel]);

  RegisterComponents(JPPackPageName, [TJppBasicSpeedButton]);
  //RegisterComponents(JPPackPageName, [TJppButton]);
  //RegisterComponents(JPPackPageName, [TJppPngButton]);

  RegisterComponents(JPPackPageName, [TJppColorListBox]);
  RegisterComponents(JPPackPageName, [TJppColorComboBox]);
  RegisterComponents(JPPackPageName, [TJppSimplePanel]);
  RegisterComponents(JPPackPageName, [TJppColorSwatch, TJppColorSwatchEx]);
  RegisterComponents(JPPackPageName, [TJppEdit, TJppMemo]);
  RegisterComponents(JPPackPageName, [TJppComboBox, TJppComboBoxEx]);
  RegisterComponents(JPPackPageName, [TJppBrushStyleComboBox, TJppPenStyleComboBox]);
  RegisterComponents(JPPackPageName, [TJppProgressBar]);
  RegisterComponents(JPPackPageName, [TJppFlipPanel]);
  RegisterComponents(JPPackPageName, [TJppCheckBox, TJppRadioButton, TJppDateTimePicker]);

  {$IFDEF USE_GDIPLUS_CONTROLS}
  RegisterComponents(JPPackPageName, [TJppGPHatchStyleComboBox]);
  {$ENDIF}

end;


initialization

  {$I '../resources/TJppBasicPanel.ctrs'}
  {$I '../resources/TJppPanel.ctrs'}
  {$I '../resources/TJppTimer.ctrs'}
  {$I '../resources/TJppLinkLabel.ctrs'}
  {$I '../resources/TJppBasicSpeedButton.ctrs'}
  {$I '../resources/TJppColorListBox.ctrs'}
  {$I '../resources/TJppColorComboBox.ctrs'}
  {$I '../resources/TJppSimplePanel.ctrs'}
  {$I '../resources/TJppColorSwatch.ctrs'}
  {$I '../resources/TJppColorSwatchEx.ctrs'}
  {$I '../resources/TJppEdit.ctrs'}
  {$I '../resources/TJppMemo.ctrs'}
  {$I '../resources/TJppPngCollection.ctrs'}
  {$I '../resources/TJppComboBox.ctrs'}
  {$I '../resources/TJppComboBoxEx.ctrs'}
  {$I '../resources/TJppStdPanel.ctrs'}
  {$I '../resources/TJppDoubleLabel.ctrs'}

end.
