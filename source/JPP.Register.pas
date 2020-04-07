unit JPP.Register;

{$I JPPack.inc}

interface

uses
  System.Classes,

  JPP.Types,
  JPP.BasicPanel, JPP.Panel, JPP.SimplePanel,
  JPP.BasicPngButton, JPP.PngButton, JPP.BasicSpeedButton,
  JPP.ColorComboBox, JPP.ColorListBox, JPP.ColorSwatch,
  JPP.LinkLabel, JPP.Timer, JPP.StorageCtrl, JPP.StringStorageCtrl,
  JPP.FormIniStorage, JPP.PngCollection,
  JPP.DoubleLineLabel, JPP.DoubleLabel,
  JPP.Edit, JPP.EditEx,
  JPP.ComboBox,
  JPP.BrushStyleComboBox, JPP.PenStyleComboBox
  {$IFDEF USE_GDIPLUS_CONTROLS}
  , JPP.GPHatchStyleComboBox
  {$ENDIF}
  ;



procedure Register;


implementation


procedure Register;
begin
  RegisterComponents(
    JPPackPageName,
    [
      TJppBasicPanel, TJppPanel, TJppSimplePanel,
      TJppBasicSpeedButton, TJppBasicPngButton, TJppPngButton,
      TJppColorComboBox, TJppColorListBox, TJppColorSwatch, TJppColorSwatchEx,
      TJppLinkLabel,
      TJppTimer,
      TJppStorageCtrl,
      TJppStringStorageCtrl,
      TJppFormIniStorage,
      TJppPngCollection,
      TJppDoubleLineLabel, TJppDoubleLabel,
      TJppEdit, TJppEditEx,
      TJppComboBox,
      TJppBrushStyleComboBox, TJppPenStyleComboBox
      {$IFDEF USE_GDIPLUS_CONTROLS}
      , TJppGPHatchStyleComboBox
      {$ENDIF}
    ]
  );
end;


end.
