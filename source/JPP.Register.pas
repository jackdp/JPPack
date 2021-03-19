unit JPP.Register;

{$I jpp.inc}

interface

uses
  Classes,

  JPP.Types,
  JPP.BasicPanel, JPP.Panel, JPP.SimplePanel, JPP.StdPanel,
  JPP.BasicPngButton, JPP.BasicPngButtonEx, JPP.PngButton, JPP.BasicSpeedButton,
  JPP.ColorComboBox, JPP.ColorListBox, JPP.ColorSwatch,
  JPP.Labels, JPP.LinkLabel, JPP.Timer, JPP.StorageCtrl, JPP.StringStorageCtrl,
  {$IFDEF DELPHI2010_OR_ABOVE}JPP.FormIniStorage,{$ENDIF}
  JPP.PngCollection,
  JPP.DoubleLineLabel, JPP.DoubleLabel,
  JPP.Edit, JPP.EditEx,
  JPP.ComboBox, JPP.ComboBoxEx,
  JPP.Memo, JPP.MemoEx,
  JPP.BrushStyleComboBox, JPP.PenStyleComboBox,
  JPP.ProgressBar, JPP.HtmlHint,
  JPP.FlipPanel,
  JPP.FlatComboBox,
  JPP.CheckBox, JPP.RadioButton, JPP.DateTimePicker
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
      TJppBasicSpeedButton, TJppBasicPngButton, TJppBasicPngButtonEx, TJppPngButton,
      TJppColorComboBox, TJppColorListBox, TJppColorSwatch, TJppColorSwatchEx,
      TJppLabel, TJppShadowLabel, TJppLinkLabel,
      TJppTimer,
      TJppStorageCtrl,
      TJppStringStorageCtrl,
      {$IFDEF DELPHI2010_OR_ABOVE}TJppFormIniStorage,{$ENDIF}
      TJppPngCollection,
      TJppDoubleLineLabel, TJppDoubleLabel,
      TJppEdit, TJppEditEx,
      TJppCheckBox, TJppRadioButton,
      TJppComboBox, TJppComboBoxEx, TJppFlatComboBox,
      TJppMemo, TJppMemoEx,
      TJppBrushStyleComboBox, TJppPenStyleComboBox, TJppDateTimePicker,
      TJppProgressBar, TJppHtmlHint,
      TJppFlipPanel
      {$IFDEF USE_GDIPLUS_CONTROLS}
      , TJppGPHatchStyleComboBox
      {$ENDIF}
    ]
  );
end;


end.
