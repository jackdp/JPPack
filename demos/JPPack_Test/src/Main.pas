unit Main;

interface

uses
  // Winapi
  Winapi.Windows, Winapi.Messages,

  // System
  System.SysUtils, System.Variants, System.Classes,

  // VCL
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.ExtCtrls, Vcl.StdCtrls, System.Actions, Vcl.ActnList, Vcl.Buttons, Vcl.Dialogs,

  // JPPack units
  JPP.BasicSpeedButton, JPP.ColorComboBox, JPP.Panel, JPP.LinkLabel, JPP.PngButton, JPP.PngButton.ColorMaps;



type
  TFormMain = class(TForm)
    JppPanel1: TJppPanel;
    ActionList1: TActionList;
    btnAero: TJppPngButton;
    ccbBgColor: TJppColorComboBox;
    btnDelphi: TJppPngButton;
    pnToolbar: TJppPanel;
    btnToolbarColor: TJppBasicSpeedButton;
    btnToolbarOpen: TJppBasicSpeedButton;
    btnToolbarSave: TJppBasicSpeedButton;
    actEsc: TAction;
    lblFugueIcons: TJppLinkLabel;
    btnAeroMod1: TJppPngButton;
    btnBlack: TJppPngButton;
    btnBlue: TJppPngButton;
    btnBlueVeryDark: TJppPngButton;
    btnGray: TJppPngButton;
    btnGray_Blue: TJppPngButton;
    btnGray_Yellow: TJppPngButton;
    btnGrayDark: TJppPngButton;
    btnGrayVeryDark: TJppPngButton;
    btnGreen: TJppPngButton;
    btnGreenDark: TJppPngButton;
    btnGreenVeryDark: TJppPngButton;
    btnHighContrastBlack: TJppPngButton;
    btnHighContrastWhite: TJppPngButton;
    btnVclAquaLightSlate: TJppPngButton;
    btnVclAuric: TJppPngButton;
    btnVclCarbon: TJppPngButton;
    btnVclCharcoalDarkSlate: TJppPngButton;
    btnVclCobaltXEMedia: TJppPngButton;
    btnVclCoral: TJppPngButton;
    btnVclEmerald: TJppPngButton;
    btnVclGlossy: TJppPngButton;
    btnVclGoldenGraphite: TJppPngButton;
    btnVclIcebergClassico: TJppPngButton;
    btnVclJet: TJppPngButton;
    btnVclLight: TJppPngButton;
    btnVclLuna: TJppPngButton;
    btnVclRubyGraphite: TJppPngButton;
    btnVclSilver: TJppPngButton;
    btnVclSlateClassico: TJppPngButton;
    btnVclTurquoiseGray: TJppPngButton;
    btnVclMetropolisUIBlack: TJppPngButton;
    btnVclMetropolisUIBlue: TJppPngButton;
    btnVclMetropolisUIDark: TJppPngButton;
    btnVclMetropolisUIGreen: TJppPngButton;
    lbl1: TLabel;
    lblPngComponents: TJppLinkLabel;
    dlgOpen: TOpenDialog;
    lblLoadColorMap: TJppLinkLabel;
    actLoadColorMap: TAction;
    lbl2: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure ButtonClick(Sender: TObject);
    procedure ccbBgColorChange(Sender: TObject);
    procedure actEscExecute(Sender: TObject);
    procedure SetJppPngButtonsFont;
    procedure actLoadColorMapExecute(Sender: TObject);
  end;


const
  APP_NAME = 'JPPack Test Application';
var
  FormMain: TFormMain;


implementation

{$R *.dfm}




procedure TFormMain.FormCreate(Sender: TObject);
begin
  Caption := APP_NAME;
  Application.Title := APP_NAME;
  Application.HintHidePause := 10000;

  SetJppPngButtonFonts(btnDelphi, 'Segoe UI', 14);

  btnToolbarSave.Appearance.Assign(btnToolbarOpen.Appearance);

  ccbBgColor.ButtonCopyColor.Appearance.Assign(ccbBgColor.ButtonChangeColor.Appearance);
  ccbBgColor.ButtonPasteColor.Appearance.Assign(ccbBgColor.ButtonChangeColor.Appearance);

  SetJppLinkLabelFonts(lblFugueIcons, 'Segoe UI', 12);
  SetJppLinkLabelFonts(lblPngComponents, 'Segoe UI', 10);
  SetJppLinkLabelFonts(lblLoadColorMap, 'Segoe UI', 12);

  //Colors: normal, hot , disabled, visited normal, visited hot
  SetJppLinkLabelColors(lblFugueIcons, clSilver, clSilver, clGray, clGray, clSilver);
  SetJppLinkLabelColors(lblLoadColorMap, lblLoadColorMap.FontNormal.Color);


  Color := ccbBgColor.Selected;

  SetJppPngButtonsFont;

end;


procedure TFormMain.SetJppPngButtonsFont;
var
  i: integer;
  btn: TJppPngButton;
begin
  for i := 0 to ControlCount - 1 do
    if Controls[i] is TJppPngButton then
    begin
      btn := Controls[i] as TJppPngButton;
      SetJppPngButtonFonts(btn, Self.Font.Name, btn.Appearance.Normal.Font.Size);
    end;
end;

procedure TFormMain.ccbBgColorChange(Sender: TObject);
begin
  Color := ccbBgColor.Selected;
end;

procedure TFormMain.actEscExecute(Sender: TObject);
begin
  Close;
end;

procedure TFormMain.actLoadColorMapExecute(Sender: TObject);
begin
  if not dlgOpen.Execute then Exit;
  btnDelphi.LoadColorMapFromIniFile(dlgOpen.FileName, 'JppPngButton_ColorMap', TJppPngButtonIniColorFormat.icfDefault);
end;

procedure TFormMain.ButtonClick(Sender: TObject);
begin
  with Sender as TControl do ShowMessage(Name + ': ' + ClassName);
end;

end.
