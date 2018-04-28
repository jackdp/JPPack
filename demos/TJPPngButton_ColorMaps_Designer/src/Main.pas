unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, System.IniFiles,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Actions, Vcl.ActnList, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls, Vcl.Clipbrd, Vcl.ComCtrls, Vcl.ImgList,

  JPP.PngButton, JPP.Common, JPP.PngButton.ColorMaps, JPP.FormIniStorage, JPP.ColorComboBox

  ;

type
  TFormMain = class(TForm)
    actApplyColors: TAction;
    actCopyToClipboard: TAction;
    actEsc: TAction;
    actGetColors: TAction;
    ActionList1: TActionList;
    actLoadColorMap: TAction;
    actResetColors: TAction;
    actSaveColorMap: TAction;
    actSwitchEnabled: TAction;
    btnCopyAsDfm: TJppPngButton;
    btnCopyDisabled: TJppPngButton;
    btnCopyDown: TJppPngButton;
    btnCopyFocused: TJppPngButton;
    btnCopyHot: TJppPngButton;
    btnCopyNormal: TJppPngButton;
    btnLoad: TJppPngButton;
    btnPasteDisabled: TJppPngButton;
    btnPasteDown: TJppPngButton;
    btnPasteFocused: TJppPngButton;
    btnPasteHot: TJppPngButton;
    btnPasteNormal: TJppPngButton;
    btnResetColors: TJppPngButton;
    btnSave: TJppPngButton;
    btnSwitchEnabled: TJppPngButton;
    btnTest1: TJppPngButton;
    ccbBackground: TJppColorComboBox;
    ccbBorderWhenDefaultColor: TJppColorComboBox;
    ccbDisabled_BorderColor: TJppColorComboBox;
    ccbDisabled_BottomGradientColorFrom: TJppColorComboBox;
    ccbDisabled_BottomGradientColorTo: TJppColorComboBox;
    ccbDisabled_Color: TJppColorComboBox;
    ccbDisabled_FontColor: TJppColorComboBox;
    ccbDisabled_UpperGradientColorFrom: TJppColorComboBox;
    ccbDisabled_UpperGradientColorTo: TJppColorComboBox;
    ccbDown_BorderColor: TJppColorComboBox;
    ccbDown_BottomGradientColorFrom: TJppColorComboBox;
    ccbDown_BottomGradientColorTo: TJppColorComboBox;
    ccbDown_Color: TJppColorComboBox;
    ccbDown_FontColor: TJppColorComboBox;
    ccbDown_UpperGradientColorFrom: TJppColorComboBox;
    ccbDown_UpperGradientColorTo: TJppColorComboBox;
    ccbFocused_BorderColor: TJppColorComboBox;
    ccbFocused_BottomGradientColorFrom: TJppColorComboBox;
    ccbFocused_BottomGradientColorTo: TJppColorComboBox;
    ccbFocused_Color: TJppColorComboBox;
    ccbFocused_FontColor: TJppColorComboBox;
    ccbFocused_UpperGradientColorFrom: TJppColorComboBox;
    ccbFocused_UpperGradientColorTo: TJppColorComboBox;
    ccbFocusRectColor: TJppColorComboBox;
    ccbHot_BorderColor: TJppColorComboBox;
    ccbHot_BottomGradientColorFrom: TJppColorComboBox;
    ccbHot_BottomGradientColorTo: TJppColorComboBox;
    ccbHot_Color: TJppColorComboBox;
    ccbHot_FontColor: TJppColorComboBox;
    ccbHot_UpperGradientColorFrom: TJppColorComboBox;
    ccbHot_UpperGradientColorTo: TJppColorComboBox;
    ccbNormal_BorderColor: TJppColorComboBox;
    ccbNormal_BottomGradientColorFrom: TJppColorComboBox;
    ccbNormal_BottomGradientColorTo: TJppColorComboBox;
    ccbNormal_Color: TJppColorComboBox;
    ccbNormal_FontColor: TJppColorComboBox;
    ccbNormal_UpperGradientColorFrom: TJppColorComboBox;
    ccbNormal_UpperGradientColorTo: TJppColorComboBox;
    chShowFocusRect: TCheckBox;
    chShowGradient: TCheckBox;
    cpBorderFocus: TCategoryPanel;
    cpDisabled: TCategoryPanel;
    cpDown: TCategoryPanel;
    cpMisc: TCategoryPanel;
    cpFocused: TCategoryPanel;
    cpg: TCategoryPanelGroup;
    cpHot: TCategoryPanel;
    cpNormal: TCategoryPanel;
    dlgOpen: TOpenDialog;
    dlgSave: TSaveDialog;
    edBorderToGradientMargin: TEdit;
    JPButton10: TJppPngButton;
    JPButton11: TJppPngButton;
    JPButton1: TJppPngButton;
    JPButton2: TJppPngButton;
    JPButton3: TJppPngButton;
    JPButton4: TJppPngButton;
    JPButton5: TJppPngButton;
    JPButton6: TJppPngButton;
    JPButton7: TJppPngButton;
    JPButton8: TJppPngButton;
    JPButton9: TJppPngButton;
    JPFormIniStorage1: TJppFormIniStorage;
    JPPngButton11: TJppPngButton;
    JPPngButton12: TJppPngButton;
    JPPngButton13: TJppPngButton;
    JPPngButton14: TJppPngButton;
    JPPngButton15: TJppPngButton;
    JPPngButton16: TJppPngButton;
    JPPngButton17: TJppPngButton;
    JPPngButton19: TJppPngButton;
    JPPngButton1: TJppPngButton;
    JPPngButton2: TJppPngButton;
    JPPngButton3: TJppPngButton;
    JPPngButton4: TJppPngButton;
    JPPngButton5: TJppPngButton;
    JPPngButton6: TJppPngButton;
    JPPngButton7: TJppPngButton;
    JPPngButton8: TJppPngButton;
    JPPngButton9: TJppPngButton;
    Label12: TLabel;
    Label33: TLabel;
    Panel1: TPanel;
    pnBackgroundColor: TPanel;
    pnColors_Disabled: TPanel;
    pnColors_Down: TPanel;
    pnColors_Focused: TPanel;
    pnColors_Hot: TPanel;
    pnColors_Normal: TPanel;
    pnTestButtons: TPanel;
    Splitter1: TSplitter;
    tmStart: TTimer;
    udBorderToGradientMargin: TUpDown;
    procedure actEscExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure actSwitchEnabledExecute(Sender: TObject);
    procedure actApplyColorsExecute(Sender: TObject);
    procedure actGetColorsExecute(Sender: TObject);
    procedure chShowGradientClick(Sender: TObject);
    procedure udBorderToGradientMarginClick(Sender: TObject; Button: TUDBtnType);
    procedure actSaveColorMapExecute(Sender: TObject);
    procedure actLoadColorMapExecute(Sender: TObject);
    procedure PerformOpen(FileName: string);
    procedure chShowFocusRectClick(Sender: TObject);
    procedure actCopyToClipboardExecute(Sender: TObject);
    procedure tmStartTimer(Sender: TObject);
    procedure ccbBackgroundChange(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure SaveParamsToIni(FileName: string);
    procedure LoadParamsFromIni(FileName: string);
    procedure actResetColorsExecute(Sender: TObject);
    procedure btnCopyNormalClick(Sender: TObject);
    procedure btnPasteNormalClick(Sender: TObject);
    procedure btnCopyHotClick(Sender: TObject);
    procedure btnPasteHotClick(Sender: TObject);
    procedure btnCopyDownClick(Sender: TObject);
    procedure btnPasteDownClick(Sender: TObject);
    procedure btnCopyFocusedClick(Sender: TObject);
    procedure btnPasteFocusedClick(Sender: TObject);
    procedure btnCopyDisabledClick(Sender: TObject);
    procedure btnPasteDisabledClick(Sender: TObject);
    procedure EnableColorCombosHints;
  end;

const
  AppName = 'TJppPngButton Color Maps Designer';

var
  FormMain: TFormMain;
  LastIni: string;
  brec: TJppPngButtonStateColors;

implementation

{$R *.dfm}




procedure TFormMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SaveParamsToIni(LastIni);
  btnTest1.SaveColorMapToIniFile(LastIni);
end;

procedure TFormMain.FormCreate(Sender: TObject);
begin
  Caption := AppName;
  Application.Title := AppName;
  LastIni := ExtractFileDir(ParamStr(0)) + '\___Last.ini';

  pnTestButtons.BevelOuter := bvNone;
  pnTestButtons.Align := alClient;

  cpg.ExpandAll;
  cpg.VertScrollBar.Position := 0;

  ccbBackground.AddColor('clBtnFace', ColorToRGB(clBtnFace));

  if not FileExists(LastIni) then
  begin
    SaveParamsToIni(LastIni);
    btnTest1.SaveColorMapToIniFile(LastIni, COLORMAP_DEFAULT_INI_SECTION, icfDefault);
  end;
  LoadParamsFromIni(LastIni);
  PerformOpen(LastIni);
  //ccbBackground.Selected := ColorToRgb(pnTestButtons.Color);

  actGetColors.Execute;

  chShowGradient.Checked := True;
  chShowFocusRect.Checked := False;

  EnableColorCombosHints;

  //ccbBackground.Selected := clRed;

  if (ParamCount > 0) and (FileExists(ParamStr(1))) then PerformOpen(ParamStr(1));
end;

procedure TFormMain.tmStartTimer(Sender: TObject);
begin
  tmStart.Enabled := False;
  try
    cpg.VertScrollBar.Position := 0;
  except
  end;
end;

procedure TFormMain.actCopyToClipboardExecute(Sender: TObject);
var
  ColorMap: TJppPngButtonColorMap;
begin
  GetJppPngButtonColorMap(btnTest1, ColorMap);

  Clipboard.AsText := ColorMap.AsDfm;
end;

procedure TFormMain.actEscExecute(Sender: TObject);
begin
  //Close;
end;

procedure TFormMain.actGetColorsExecute(Sender: TObject);
var
  ColorMap: TJppPngButtonColorMap;
begin
  GetJppPngButtonColorMap(btnTest1, ColorMap);

  ccbBorderWhenDefaultColor.Selected := ColorMap.BorderWhenDefaultColor;
  ccbFocusRectColor.Selected := ColorMap.FocusRectColor;

  ccbNormal_BorderColor.Selected := ColorMap.Normal.BorderColor;
  ccbNormal_FontColor.Selected := ColorMap.Normal.FontColor;
  ccbNormal_Color.Selected := ColorMap.Normal.Color;
  ccbNormal_UpperGradientColorFrom.Selected := ColorMap.Normal.UpperGradientColorFrom;
  ccbNormal_UpperGradientColorTo.Selected := ColorMap.Normal.UpperGradientColorTo;
  ccbNormal_BottomGradientColorFrom.Selected := ColorMap.Normal.BottomGradientColorFrom;
  ccbNormal_BottomGradientColorTo.Selected := ColorMap.Normal.BottomGradientColorTo;

  ccbHot_BorderColor.Selected := ColorMap.Hot.BorderColor;
  ccbHot_FontColor.Selected := ColorMap.Hot.FontColor;
  ccbHot_Color.Selected := ColorMap.Hot.Color;
  ccbHot_UpperGradientColorFrom.Selected := ColorMap.Hot.UpperGradientColorFrom;
  ccbHot_UpperGradientColorTo.Selected := ColorMap.Hot.UpperGradientColorTo;
  ccbHot_BottomGradientColorFrom.Selected := ColorMap.Hot.BottomGradientColorFrom;
  ccbHot_BottomGradientColorTo.Selected := ColorMap.Hot.BottomGradientColorTo;

  ccbDown_BorderColor.Selected := ColorMap.Down.BorderColor;
  ccbDown_FontColor.Selected := ColorMap.Down.FontColor;
  ccbDown_Color.Selected := ColorMap.Down.Color;
  ccbDown_UpperGradientColorFrom.Selected := ColorMap.Down.UpperGradientColorFrom;
  ccbDown_UpperGradientColorTo.Selected := ColorMap.Down.UpperGradientColorTo;
  ccbDown_BottomGradientColorFrom.Selected := ColorMap.Down.BottomGradientColorFrom;
  ccbDown_BottomGradientColorTo.Selected := ColorMap.Down.BottomGradientColorTo;

  ccbFocused_BorderColor.Selected := ColorMap.Focused.BorderColor;
  ccbFocused_FontColor.Selected := ColorMap.Focused.FontColor;
  ccbFocused_Color.Selected := ColorMap.Focused.Color;
  ccbFocused_UpperGradientColorFrom.Selected := ColorMap.Focused.UpperGradientColorFrom;
  ccbFocused_UpperGradientColorTo.Selected := ColorMap.Focused.UpperGradientColorTo;
  ccbFocused_BottomGradientColorFrom.Selected := ColorMap.Focused.BottomGradientColorFrom;
  ccbFocused_BottomGradientColorTo.Selected := ColorMap.Focused.BottomGradientColorTo;

  ccbDisabled_BorderColor.Selected := ColorMap.Disabled.BorderColor;
  ccbDisabled_FontColor.Selected := ColorMap.Disabled.FontColor;
  ccbDisabled_Color.Selected := ColorMap.Disabled.Color;
  ccbDisabled_UpperGradientColorFrom.Selected := ColorMap.Disabled.UpperGradientColorFrom;
  ccbDisabled_UpperGradientColorTo.Selected := ColorMap.Disabled.UpperGradientColorTo;
  ccbDisabled_BottomGradientColorFrom.Selected := ColorMap.Disabled.BottomGradientColorFrom;
  ccbDisabled_BottomGradientColorTo.Selected := ColorMap.Disabled.BottomGradientColorTo;
end;

procedure TFormMain.actApplyColorsExecute(Sender: TObject);
var
  ColorMap: TJppPngButtonColorMap;
  i: integer;
  Button: TJppPngButton;
begin
  ColorMap.BorderWhenDefaultColor := ccbBorderWhenDefaultColor.Selected;
  ColorMap.FocusRectColor := ccbFocusRectColor.Selected;

  ColorMap.Normal.BorderColor := ccbNormal_BorderColor.Selected;
  ColorMap.Normal.FontColor := ccbNormal_FontColor.Selected;
  ColorMap.Normal.Color := ccbNormal_Color.Selected;
  ColorMap.Normal.UpperGradientColorFrom := ccbNormal_UpperGradientColorFrom.Selected;
  ColorMap.Normal.UpperGradientColorTo := ccbNormal_UpperGradientColorTo.Selected;
  ColorMap.Normal.BottomGradientColorFrom := ccbNormal_BottomGradientColorFrom.Selected;
  ColorMap.Normal.BottomGradientColorTo := ccbNormal_BottomGradientColorTo.Selected;

  ColorMap.Hot.BorderColor := ccbHot_BorderColor.Selected;
  ColorMap.Hot.FontColor := ccbHot_FontColor.Selected;
  ColorMap.Hot.Color := ccbHot_Color.Selected;
  ColorMap.Hot.UpperGradientColorFrom := ccbHot_UpperGradientColorFrom.Selected;
  ColorMap.Hot.UpperGradientColorTo := ccbHot_UpperGradientColorTo.Selected;
  ColorMap.Hot.BottomGradientColorFrom := ccbHot_BottomGradientColorFrom.Selected;
  ColorMap.Hot.BottomGradientColorTo := ccbHot_BottomGradientColorTo.Selected;

  ColorMap.Down.BorderColor := ccbDown_BorderColor.Selected;
  ColorMap.Down.FontColor := ccbDown_FontColor.Selected;
  ColorMap.Down.Color := ccbDown_Color.Selected;
  ColorMap.Down.UpperGradientColorFrom := ccbDown_UpperGradientColorFrom.Selected;
  ColorMap.Down.UpperGradientColorTo := ccbDown_UpperGradientColorTo.Selected;
  ColorMap.Down.BottomGradientColorFrom := ccbDown_BottomGradientColorFrom.Selected;
  ColorMap.Down.BottomGradientColorTo := ccbDown_BottomGradientColorTo.Selected;

  ColorMap.Focused.BorderColor := ccbFocused_BorderColor.Selected;
  ColorMap.Focused.FontColor := ccbFocused_FontColor.Selected;
  ColorMap.Focused.Color := ccbFocused_Color.Selected;
  ColorMap.Focused.UpperGradientColorFrom := ccbFocused_UpperGradientColorFrom.Selected;
  ColorMap.Focused.UpperGradientColorTo := ccbFocused_UpperGradientColorTo.Selected;
  ColorMap.Focused.BottomGradientColorFrom := ccbFocused_BottomGradientColorFrom.Selected;
  ColorMap.Focused.BottomGradientColorTo := ccbFocused_BottomGradientColorTo.Selected;

  ColorMap.Disabled.BorderColor := ccbDisabled_BorderColor.Selected;
  ColorMap.Disabled.FontColor := ccbDisabled_FontColor.Selected;
  ColorMap.Disabled.Color := ccbDisabled_Color.Selected;
  ColorMap.Disabled.UpperGradientColorFrom := ccbDisabled_UpperGradientColorFrom.Selected;
  ColorMap.Disabled.UpperGradientColorTo := ccbDisabled_UpperGradientColorTo.Selected;
  ColorMap.Disabled.BottomGradientColorFrom := ccbDisabled_BottomGradientColorFrom.Selected;
  ColorMap.Disabled.BottomGradientColorTo := ccbDisabled_BottomGradientColorTo.Selected;

  for i := 0 to ComponentCount - 1 do
    if Components[i] is TJppPngButton then
    begin
      Button := Components[i] as TJppPngButton;
      if Button.Parent <> pnTestButtons then Continue;
      Button.ApplyColorMap(ColorMap);
    end;
  //btnTest1.ApplyColorMap(ColorMap);
end;

procedure TFormMain.actLoadColorMapExecute(Sender: TObject);
begin
  if not dlgOpen.Execute then Exit;
  PerformOpen(dlgOpen.FileName);
end;

procedure TFormMain.actResetColorsExecute(Sender: TObject);
var
  i: integer;
  Button: TJppPngButton;
begin
  if MessageBox(Handle, 'Are you sure you want to reset all colors?', PChar(AppName), MB_ICONQUESTION or MB_YESNO) = IDNO then Exit;

  for i := 0 to ComponentCount - 1 do
    if Components[i] is TJppPngButton then
    begin
      Button := Components[i] as TJppPngButton;
      if Button.Parent <> pnTestButtons then Continue;
      Button.ColorMapType := cmtAero;
    end;
  actGetColors.Execute;
  ccbBackground.Selected := ColorToRGB(clBtnFace);
  ccbBackground.OnChange(Self);
end;

procedure TFormMain.PerformOpen(FileName: string);
var
  Format: TJppPngButtonIniColorFormat;
  fIni: string;
begin
  if UpperCase(ExtractFileExt(FileName)) = '.COLORS' then Format := icfJPColors else Format := icfDefault;
  btnTest1.LoadColorMapFromIniFile(FileName, COLORMAP_DEFAULT_INI_SECTION, Format);
  actGetColors.Execute;
  actApplyColors.Execute;

  fIni := ChangeFileExt(FileName, '.ini');
  dlgSave.FileName := fIni;
  if FileExists(fIni) then LoadParamsFromIni(fIni);
  Caption := AppName + ' - ' + FileName;
end;

procedure TFormMain.SaveParamsToIni(FileName: string);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(FileName);
  try
    Ini.WriteString('MAIN', 'AppName', AppName);
    Ini.WriteInteger('MAIN', 'BackgroundColor', ColorToRGB(pnTestButtons.Color));
  finally
    Ini.Free;
  end;
end;

procedure TFormMain.LoadParamsFromIni(FileName: string);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(FileName);
  try
    pnTestButtons.Color := Ini.ReadInteger('MAIN', 'BackgroundColor', pnTestButtons.Color);
    ccbBackground.Selected := pnTestButtons.Color;
  finally
    Ini.Free;
  end;
end;

procedure TFormMain.actSaveColorMapExecute(Sender: TObject);
begin
  if not dlgSave.Execute then Exit;

  SaveParamsToIni(dlgSave.FileName);
  btnTest1.SaveColorMapToIniFile(dlgSave.FileName, COLORMAP_DEFAULT_INI_SECTION, icfDefault);
  btnTest1.SaveColorMapToIniFile(ChangeFileExt(dlgSave.FileName, '.colors'), COLORMAP_DEFAULT_INI_SECTION, icfJPColors);

  Caption := AppName + ' - ' + dlgSave.FileName;
end;

procedure TFormMain.actSwitchEnabledExecute(Sender: TObject);
var
  i: integer;
  Button: TJppPngButton;
begin
  for i := 0 to ComponentCount - 1 do
    if Components[i] is TJppPngButton then
    begin
      Button := Components[i] as TJppPngButton;
      if Button.Parent <> pnTestButtons then Continue;
      if Button.Tag = 1 then Continue;
      Button.Enabled := not Button.Enabled;
    end;
end;



procedure TFormMain.udBorderToGradientMarginClick(Sender: TObject; Button: TUDBtnType);
var
  i, x: integer;
  Btn: TJppPngButton;
begin
  x := udBorderToGradientMargin.Position;

  for i := 0 to ComponentCount - 1 do
    if Components[i] is TJppPngButton then
    begin
      Btn := Components[i] as TJppPngButton;
      if Btn.Parent <> pnTestButtons then Continue;
      Btn.Appearance.Normal.BorderToGradientMargin := x;
      Btn.Appearance.Hot.BorderToGradientMargin := x;
      Btn.Appearance.Down.BorderToGradientMargin := x;
      Btn.Appearance.Focused.BorderToGradientMargin := x;
      Btn.Appearance.Disabled.BorderToGradientMargin := x;
    end;
end;

procedure TFormMain.ccbBackgroundChange(Sender: TObject);
begin
  pnTestButtons.Color := ccbBackground.Selected;
end;

procedure TFormMain.chShowFocusRectClick(Sender: TObject);
var
  i: integer;
  Button: TJppPngButton;
begin
  for i := 0 to ComponentCount - 1 do
    if Components[i] is TJppPngButton then
    begin
      Button := Components[i] as TJppPngButton;
      if Button.Parent <> pnTestButtons then Continue;
      if chShowFocusRect.Checked then Button.Appearance.FocusRect.FocusType := frtCustom else Button.Appearance.FocusRect.FocusType := frtNone;
    end;
end;

procedure TFormMain.chShowGradientClick(Sender: TObject);
var
  i: integer;
  Button: TJppPngButton;
begin
  for i := 0 to ComponentCount - 1 do
    if Components[i] is TJppPngButton then
    begin
      Button := Components[i] as TJppPngButton;
      if Button.Parent <> pnTestButtons then Continue;
      Button.Appearance.Normal.GradientEnabled := chShowGradient.Checked;
      Button.Appearance.Hot.GradientEnabled := chShowGradient.Checked;
      Button.Appearance.Down.GradientEnabled := chShowGradient.Checked;
      Button.Appearance.Focused.GradientEnabled := chShowGradient.Checked;
      Button.Appearance.Disabled.GradientEnabled := chShowGradient.Checked;
    end;
end;


procedure TFormMain.EnableColorCombosHints;
var
  i: integer;
begin
  for i := 0 to ComponentCount - 1 do
    if Components[i] is TJppColorComboBox then
    with Components[i] as TJppColorComboBox do
    begin
      ButtonChangeColor.ShowHint := True;
      ButtonCopyColor.ShowHint := True;
      ButtonPasteColor.ShowHint := True;
    end;
end;

{$region ' ---------------------- Copy & Paste ------------------------- '}
procedure TFormMain.btnCopyDisabledClick(Sender: TObject);
begin
  with brec do
  begin
    BorderColor := ccbDisabled_BorderColor.Selected;
    FontColor := ccbDisabled_FontColor.Selected;
    Color := ccbDisabled_Color.Selected;
    BottomGradientColorFrom := ccbDisabled_BottomGradientColorFrom.Selected;
    BottomGradientColorTo := ccbDisabled_BottomGradientColorTo.Selected;
    UpperGradientColorFrom := ccbDisabled_UpperGradientColorFrom.Selected;
    UpperGradientColorTo := ccbDisabled_UpperGradientColorTo.Selected;
  end;
end;

procedure TFormMain.btnCopyDownClick(Sender: TObject);
begin
  with brec do
  begin
    BorderColor := ccbDown_BorderColor.Selected;
    FontColor := ccbDown_FontColor.Selected;
    Color := ccbDown_Color.Selected;
    BottomGradientColorFrom := ccbDown_BottomGradientColorFrom.Selected;
    BottomGradientColorTo := ccbDown_BottomGradientColorTo.Selected;
    UpperGradientColorFrom := ccbDown_UpperGradientColorFrom.Selected;
    UpperGradientColorTo := ccbDown_UpperGradientColorTo.Selected;
  end;
end;

procedure TFormMain.btnCopyFocusedClick(Sender: TObject);
begin
  with brec do
  begin
    BorderColor := ccbFocused_BorderColor.Selected;
    FontColor := ccbFocused_FontColor.Selected;
    Color := ccbFocused_Color.Selected;
    BottomGradientColorFrom := ccbFocused_BottomGradientColorFrom.Selected;
    BottomGradientColorTo := ccbFocused_BottomGradientColorTo.Selected;
    UpperGradientColorFrom := ccbFocused_UpperGradientColorFrom.Selected;
    UpperGradientColorTo := ccbFocused_UpperGradientColorTo.Selected;
  end;
end;

procedure TFormMain.btnCopyHotClick(Sender: TObject);
begin
  with brec do
  begin
    BorderColor := ccbHot_BorderColor.Selected;
    FontColor := ccbHot_FontColor.Selected;
    Color := ccbHot_Color.Selected;
    BottomGradientColorFrom := ccbHot_BottomGradientColorFrom.Selected;
    BottomGradientColorTo := ccbHot_BottomGradientColorTo.Selected;
    UpperGradientColorFrom := ccbHot_UpperGradientColorFrom.Selected;
    UpperGradientColorTo := ccbHot_UpperGradientColorTo.Selected;
  end;
end;

procedure TFormMain.btnCopyNormalClick(Sender: TObject);
begin
  with brec do
  begin
    BorderColor := ccbNormal_BorderColor.Selected;
    FontColor := ccbNormal_FontColor.Selected;
    Color := ccbNormal_Color.Selected;
    BottomGradientColorFrom := ccbNormal_BottomGradientColorFrom.Selected;
    BottomGradientColorTo := ccbNormal_BottomGradientColorTo.Selected;
    UpperGradientColorFrom := ccbNormal_UpperGradientColorFrom.Selected;
    UpperGradientColorTo := ccbNormal_UpperGradientColorTo.Selected;
  end;
end;

procedure TFormMain.btnPasteDisabledClick(Sender: TObject);
begin
  ccbDisabled_BorderColor.Selected := brec.BorderColor;
  ccbDisabled_FontColor.Selected := brec.FontColor;
  ccbDisabled_Color.Selected := brec.Color;
  ccbDisabled_BottomGradientColorFrom.Selected := brec.BottomGradientColorFrom;
  ccbDisabled_BottomGradientColorTo.Selected := brec.BottomGradientColorTo;
  ccbDisabled_UpperGradientColorFrom.Selected := brec.UpperGradientColorFrom;
  ccbDisabled_UpperGradientColorTo.Selected := brec.UpperGradientColorTo;
  actApplyColors.Execute;
end;

procedure TFormMain.btnPasteDownClick(Sender: TObject);
begin
  ccbDown_BorderColor.Selected := brec.BorderColor;
  ccbDown_FontColor.Selected := brec.FontColor;
  ccbDown_Color.Selected := brec.Color;
  ccbDown_BottomGradientColorFrom.Selected := brec.BottomGradientColorFrom;
  ccbDown_BottomGradientColorTo.Selected := brec.BottomGradientColorTo;
  ccbDown_UpperGradientColorFrom.Selected := brec.UpperGradientColorFrom;
  ccbDown_UpperGradientColorTo.Selected := brec.UpperGradientColorTo;
  actApplyColors.Execute;
end;

procedure TFormMain.btnPasteFocusedClick(Sender: TObject);
begin
  ccbFocused_BorderColor.Selected := brec.BorderColor;
  ccbFocused_FontColor.Selected := brec.FontColor;
  ccbFocused_Color.Selected := brec.Color;
  ccbFocused_BottomGradientColorFrom.Selected := brec.BottomGradientColorFrom;
  ccbFocused_BottomGradientColorTo.Selected := brec.BottomGradientColorTo;
  ccbFocused_UpperGradientColorFrom.Selected := brec.UpperGradientColorFrom;
  ccbFocused_UpperGradientColorTo.Selected := brec.UpperGradientColorTo;
  actApplyColors.Execute;
end;

procedure TFormMain.btnPasteHotClick(Sender: TObject);
begin
  ccbHot_BorderColor.Selected := brec.BorderColor;
  ccbHot_FontColor.Selected := brec.FontColor;
  ccbHot_Color.Selected := brec.Color;
  ccbHot_BottomGradientColorFrom.Selected := brec.BottomGradientColorFrom;
  ccbHot_BottomGradientColorTo.Selected := brec.BottomGradientColorTo;
  ccbHot_UpperGradientColorFrom.Selected := brec.UpperGradientColorFrom;
  ccbHot_UpperGradientColorTo.Selected := brec.UpperGradientColorTo;
  actApplyColors.Execute;
end;

procedure TFormMain.btnPasteNormalClick(Sender: TObject);
begin
  ccbNormal_BorderColor.Selected := brec.BorderColor;
  ccbNormal_FontColor.Selected := brec.FontColor;
  ccbNormal_Color.Selected := brec.Color;
  ccbNormal_BottomGradientColorFrom.Selected := brec.BottomGradientColorFrom;
  ccbNormal_BottomGradientColorTo.Selected := brec.BottomGradientColorTo;
  ccbNormal_UpperGradientColorFrom.Selected := brec.UpperGradientColorFrom;
  ccbNormal_UpperGradientColorTo.Selected := brec.UpperGradientColorTo;
  actApplyColors.Execute;
end;

{$endregion}

end.
