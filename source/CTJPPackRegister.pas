unit CTJPPackRegister;

{$mode objfpc}{$H+}

interface

uses
    Forms, LCLIntf, LCLType, LMessages, Classes, Graphics, Controls, SysUtils, StdCtrls,
    TypInfo, lresources,
    JPP.Common, JPP.Common.Procs, JPP.DoubleLabel, JPP.DoubleLineLabel, JPP.LinkLabel,
    JPP.StorageCtrl, JPP.StringStorageCtrl, JPP.PngCollection, JPP.Timer, JPP.BasicPanel, JPP.Panel,
    JPP.BasicSpeedButton, JPP.ColorListBox, JPP.ColorComboBox, JPP.ColorSwatch, JPP.SimplePanel,
    JPP.Edit,
    JPP.BrushStyleComboBox, JPP.PenStyleComboBox,
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
  RegisterComponents(JPPackPageName, [TJppEdit]);
  RegisterComponents(JPPackPageName, [TJppBrushStyleComboBox, TJppPenStyleComboBox]);


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

end.
