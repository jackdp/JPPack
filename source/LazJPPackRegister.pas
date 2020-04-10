unit LazJPPackRegister;

{$mode objfpc}{$H+}
{$WARN 5023 off : Unit "$1" not used in $2}
{$I JPPack.inc}

interface

uses
    Forms, LCLIntf, LCLType, LMessages, Classes, Graphics, Controls, SysUtils, StdCtrls,
    TypInfo, lresources,
    JPP.Common, JPP.Common.Procs, JPP.DoubleLabel, JPP.DoubleLineLabel, JPP.LinkLabel,
    JPP.StorageCtrl, JPP.StringStorageCtrl, JPP.PngCollection, JPP.Timer, JPP.BasicPanel, JPP.Panel,
    JPP.BasicSpeedButton, JPP.ColorListBox, JPP.ComboBox, JPP.ColorComboBox, JPP.ColorSwatch, JPP.SimplePanel,
    JPP.Edit, JPP.Memo,
    JPP.BrushStyleComboBox, JPP.PenStyleComboBox,
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
  RegisterComponents(JPPackPageName, [TJppLinkLabel]);
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
  RegisterComponents(JPPackPageName, [TJppComboBox]);
  RegisterComponents(JPPackPageName, [TJppBrushStyleComboBox, TJppPenStyleComboBox]);

  {$IFDEF USE_GDIPLUS_CONTROLS}
  RegisterComponents(JPPackPageName, [TJppGPHatchStyleComboBox]);
  {$ENDIF}


end;


initialization

  {$I '../resources/TJppBasicPanel.lrs'}
  {$I '../resources/TJppPanel.lrs'}
  {$I '../resources/TJppTimer.lrs'}
  {$I '../resources/TJppLinkLabel.lrs'}
  {$I '../resources/TJppBasicSpeedButton.lrs'}
  {$I '../resources/TJppColorListBox.lrs'}
  {$I '../resources/TJppColorComboBox.lrs'}
  {$I '../resources/TJppSimplePanel.lrs'}
  {$I '../resources/TJppColorSwatch.lrs'}
  {$I '../resources/TJppColorSwatchEx.lrs'}
  {$I '../resources/TJppEdit.lrs'}
  {$I '../resources/TJppMemo.lrs'}
  {$I '../resources/TJppPngCollection.lrs'}
  {$I '../resources/TJppComboBox.lrs'}
  {$I '../resources/TJppComboBoxEx.lrs'}
  {$I '../resources/TJppStdPanel.lrs'}
  {$I '../resources/TJppDoubleLabellrs'}

end.
