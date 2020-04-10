unit JPP.Register;

{$I JPPack.inc}

interface

uses
  System.Classes,

  JPP.Types,
  JPP.BasicPanel, JPP.Panel, JPP.SimplePanel, JPP.StdPanel,
  JPP.BasicPngButton, JPP.PngButton, JPP.BasicSpeedButton,
  JPP.ColorComboBox, JPP.ColorListBox, JPP.ColorSwatch,
  JPP.LinkLabel, JPP.Timer, JPP.StorageCtrl, JPP.StringStorageCtrl,
  JPP.FormIniStorage, JPP.PngCollection,
  JPP.DoubleLineLabel, JPP.DoubleLabel,
  JPP.Edit, JPP.EditEx,
  JPP.ComboBox, JPP.ComboBoxEx,
  JPP.Memo, JPP.MemoEx,
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
      TJppStdPanel, TJppSimplePanel, TJppBasicPanel, TJppPanel,
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
      TJppComboBox, TJppComboBoxEx,
      TJppMemo, TJppMemoEx,
      TJppBrushStyleComboBox, TJppPenStyleComboBox
      {$IFDEF USE_GDIPLUS_CONTROLS}
      , TJppGPHatchStyleComboBox
      {$ENDIF}
    ]
  );
end;


end.
