unit JPP.Register;

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
  JPP.Edit
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
      TJppEdit
    ]
  );
end;


end.
