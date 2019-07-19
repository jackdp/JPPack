unit Main;

interface

uses
  // Winapi
  Winapi.Windows, Winapi.Messages,

  // System
  System.SysUtils, System.Variants, System.Classes,

  // VCL
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.ExtCtrls, Vcl.StdCtrls, System.Actions, Vcl.ActnList, Vcl.Buttons, Vcl.Dialogs,

  // JPLib
  JPL.Strings, JPL.Colors,

  // JPPack units
  JPP.BasicSpeedButton, JPP.ColorComboBox, JPP.Panel, JPP.LinkLabel, JPP.PngButton, JPP.PngButton.ColorMaps, JPP.ColorListBox, JPP.BasicPanel,
  JPP.DoubleLineLabel, JPP.DoubleLabel, JPP.PngCollection, JPP.ColorSwatch, JPP.SimplePanel, JPP.Edit;



type
  TFormMain = class(TForm)
    JppPanel1: TJppPanel;
    ActionList1: TActionList;
    ccb: TJppColorComboBox;
    btnDelphi: TJppPngButton;
    pnToolbar: TJppPanel;
    btnToolbarColor: TJppBasicSpeedButton;
    btnToolbarOpen: TJppBasicSpeedButton;
    btnToolbarSave: TJppBasicSpeedButton;
    actEsc: TAction;
    lblFugueIcons: TJppLinkLabel;
    dlgOpen: TOpenDialog;
    lblLoadColorMap: TJppLinkLabel;
    actLoadColorMap: TAction;
    lbl2: TLabel;
    pnButtons: TJppBasicPanel;
    lbl1: TLabel;
    lblPngComponents: TJppLinkLabel;
    btnAero: TJppPngButton;
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
    pnRight: TJppBasicPanel;
    clb: TJppColorListBox;
    lbl3: TLabel;
    TJppDoubleLineLabel: TJppDoubleLineLabel;
    JppDoubleLabel1: TJppDoubleLabel;
    JppDoubleLineLabel1: TJppDoubleLineLabel;
    JppDoubleLineLabel2: TJppDoubleLineLabel;
    btnToolbarAssignIcon: TJppBasicSpeedButton;
    JppPngCollection1: TJppPngCollection;
    spnColorSwatch: TJppSimplePanel;
    lbl4: TLabel;
    JppColorSwatch1: TJppColorSwatch;
    JppColorSwatch2: TJppColorSwatch;
    cswe: TJppColorSwatchEx;
    JppColorSwatch3: TJppColorSwatch;
    JppColorSwatch4: TJppColorSwatch;
    spnEdit: TJppSimplePanel;
    lbl5: TLabel;
    JppEdit1: TJppEdit;
    sbtnFlashEdit: TJppBasicSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure ButtonClick(Sender: TObject);
    procedure actEscExecute(Sender: TObject);
    procedure SetJppPngButtonsFont;
    procedure actLoadColorMapExecute(Sender: TObject);
    procedure btnToolbarAssignIconClick(Sender: TObject);
    procedure ccbColorChanged(Sender: TObject);
    procedure clbColorChanged(Sender: TObject);
    procedure clbMeasureItem(Control: TWinControl; Index: Integer; var Height: Integer);
    procedure JppColorSwatch1GetBottomColorStrValue(const AColor: TColor; var ColorStr, Prefix, Suffix: string);
    procedure sbtnFlashEditClick(Sender: TObject);
    procedure SetActiveColor(const cl: TColor);
  end;


const
  APP_NAME = 'JPPack Test Application';
var
  FormMain: TFormMain;


implementation

{$R *.dfm}




procedure TFormMain.FormCreate(Sender: TObject);
var
  vl: TJppPanelVerticalLine;
begin
  Caption := APP_NAME;
  Application.Title := APP_NAME;
  Application.HintHidePause := 10000;

  clb.Align := alClient;
  pnRight.Appearance.DrawBorder := False;

  SetJppPngButtonFonts(btnDelphi, 'Segoe UI', 14);

  btnToolbarSave.Appearance.Assign(btnToolbarOpen.Appearance);

  ccb.ButtonCopyColor.Appearance.Assign(ccb.ButtonChangeColor.Appearance);
  ccb.ButtonPasteColor.Appearance.Assign(ccb.ButtonChangeColor.Appearance);

  cswe.ButtonCopyColor.Appearance.Assign(ccb.ButtonChangeColor.Appearance);
  cswe.ButtonPasteColor.Appearance.Assign(ccb.ButtonChangeColor.Appearance);

  SetJppLinkLabelFonts(lblFugueIcons, 'Segoe UI', 12);
  SetJppLinkLabelFonts(lblPngComponents, 'Segoe UI', 10);
  SetJppLinkLabelFonts(lblLoadColorMap, 'Segoe UI', 12);

  //Colors: normal, hot , disabled, visited normal, visited hot
  SetJppLinkLabelColors(lblFugueIcons, clSilver, clWhite, clGray, clGray, clSilver);
  SetJppLinkLabelColors(lblLoadColorMap, lblLoadColorMap.FontNormal.Color);


  SetActiveColor(ccb.SelectedColor);

  SetJppPngButtonsFont;

  vl := pnToolbar.VerticalLines[0];
  vl.PosX := btnToolbarSave.Left + btnToolbarSave.Width + btnToolbarSave.Margins.Right;

  vl := pnToolbar.VerticalLines[1];
  vl.PosX := btnToolbarColor.Left + btnToolbarColor.Width + btnToolbarColor.Margins.Right;
end;


procedure TFormMain.SetActiveColor(const cl: TColor);
var
  cl2: TColor;
begin
  Color := cl;
  cl2 := GetSimilarColor(cl, 20, False);

  pnButtons.Appearance.Borders.Left.Pen.Color := cl2;
  pnButtons.Appearance.Borders.Right.Pen.Color := cl2;
  spnColorSwatch.Appearance.BorderColor := cl2;
  spnEdit.Appearance.BorderColor := cl2;

  cl2 := cl; //GetSimilarColor(cl, 20, True);
  pnButtons.Appearance.UpperGradient.ColorFrom := cl2;
  cl2 := GetSimilarColor(cl, 10, True);
  pnButtons.Appearance.UpperGradient.ColorTo := cl2;
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

procedure TFormMain.actEscExecute(Sender: TObject);
begin
  Close;
end;

procedure TFormMain.actLoadColorMapExecute(Sender: TObject);
begin
  if not dlgOpen.Execute then Exit;
  btnDelphi.LoadColorMapFromIniFile(dlgOpen.FileName, 'JppPngButton_ColorMap', TJppPngButtonIniColorFormat.icfDefault);
end;

procedure TFormMain.btnToolbarAssignIconClick(Sender: TObject);
begin
  if not btnToolbarAssignIcon.PngImage.Empty then Exit;
  btnToolbarAssignIcon.PngImage.Assign(JppPngCollection1.Items[0].PngImage);
  btnToolbarAssignIcon.Caption := '------ FIRE! ------';
end;

procedure TFormMain.ButtonClick(Sender: TObject);
begin
  with Sender as TControl do ShowMessage(Name + ': ' + ClassName);
end;

procedure TFormMain.ccbColorChanged(Sender: TObject);
begin
  SetActiveColor(ccb.SelectedColor);
end;

procedure TFormMain.clbColorChanged(Sender: TObject);
begin
  SetActiveColor(clb.SelectedColor);
end;

procedure TFormMain.clbMeasureItem(Control: TWinControl; Index: Integer; var Height: Integer);
begin
  if clb.IsSeparatorItem(Index) then Height := 22;
end;

procedure TFormMain.JppColorSwatch1GetBottomColorStrValue(const AColor: TColor; var ColorStr, Prefix, Suffix: string);
begin
  ColorStr := InsertNumSep(ColorStr, ' ', 2, 2);
end;

procedure TFormMain.sbtnFlashEditClick(Sender: TObject);
begin
  JppEdit1.FlashBackground;
end;

end.
