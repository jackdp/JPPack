unit JPP.Register;

interface

uses
  System.Classes,

  JPP.Types,
  JPP.BasicPanel, JPP.Panel,
  JPP.BasicPngButton, JPP.PngButton, JPP.BasicSpeedButton,
  JPP.ColorComboBox, JPP.ColorListBox,
  JPP.LinkLabel, JPP.Timer, JPP.StorageCtrl, JPP.StringStorageCtrl,
  JPP.FormIniStorage, {JPP.PictureCollection,} JPP.PngCollection
  ;



procedure Register;

  
implementation


procedure Register;
begin
  RegisterComponents(
    JPPackPageName,
    [
      TJppBasicPanel, TJppPanel,
      TJppBasicSpeedButton, TJppBasicPngButton, TJppPngButton,
      TJppColorComboBox, TJppColorListBox,
      TJppLinkLabel,
      TJppTimer,
      TJppStorageCtrl, 
      TJppStringStorageCtrl,
      TJppFormIniStorage,
      //TJppPictureCollection,
      TJppPngCollection
    ]
  );
end;


end.
