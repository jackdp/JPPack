unit Main;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics, System.IniFiles,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, System.Actions, Vcl.ActnList, Vcl.StdCtrls, Vcl.Buttons, Vcl.ExtCtrls, Vcl.Clipbrd, Vcl.ComCtrls, Vcl.ImgList,

  JPP.PngButton, JPP.Common, JPP.Common.Procs, JPP.PngButton.ColorMaps, JPP.FormIniStorage, JPP.ColorComboBox, JPP.SimplePanel

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
    pnBorderFocus: TPanel;
    pnColors_Disabled: TPanel;
    pnColors_Down: TPanel;
    pnColors_Focused: TPanel;
    pnColors_Hot: TPanel;
    pnColors_Normal: TPanel;
    pnTestButtons: TJppSimplePanel;
    Splitter1: TSplitter;
    tmStart: TTimer;
    udBorderToGradientMargin: TUpDown;
    pnBackgroundColor: TJppSimplePanel;
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
    procedure SetCombosParams;
    procedure PrepareControls;
  end;

const
  AppName = 'TJppPngButton Color Maps Designer';

var
  FormMain: TFormMain;
  LastIni: string;
  brec: TJppPngButtonStateColors;

implementation

{$R *.dfm}



procedure TFormMain.FormCreate(Sender: TObject);
begin
  Caption := AppName;
  Application.Title := AppName;
  LastIni := ExtractFileDir(ParamStr(0)) + '\___Last.ini';

  PrepareControls;

  if not FileExists(LastIni) then
  begin
    SaveParamsToIni(LastIni);
    btnTest1.SaveColorMapToIniFile(LastIni, COLORMAP_DEFAULT_INI_SECTION, icfDefault);
  end;
  LoadParamsFromIni(LastIni);
  PerformOpen(LastIni);

  actGetColors.Execute;

  chShowGradient.Checked := True;
  chShowFocusRect.Checked := False;

  if (ParamCount > 0) and (FileExists(ParamStr(1))) then PerformOpen(ParamStr(1));
end;

procedure TFormMain.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  SaveParamsToIni(LastIni);
  btnTest1.SaveColorMapToIniFile(LastIni);
end;

procedure TFormMain.PrepareControls;
var
  i: integer;
  Button: TJppPngButton;
begin
  pnTestButtons.Align := alClient;

  cpg.ExpandAll;
  cpg.VertScrollBar.Position := 0;

  ccbBackground.AddColor('clBtnFace', ColorToRGB(clBtnFace));

  SetCombosParams;
  EnableColorCombosHints;

  pnBorderFocus.Align := alClient;
  pnColors_Normal.Align := alClient;
  pnColors_Hot.Align := alClient;
  pnColors_Focused.Align := alClient;
  pnColors_Down.Align := alClient;
  pnColors_Disabled.Align := alClient;

  Self.Font.Name := GetFontName(['Segoe UI', 'Tahoma']);
  Self.Font.Size := 9;

  // Set fonts for all buttons
  for i := 0 to ComponentCount - 1 do
    if Components[i] is TJppPngButton then
    begin
      Button := Components[i] as TJppPngButton;
      Button.Appearance.Normal.Font.Name := Font.Name;
      Button.Appearance.Normal.Font.Size := Font.Size;
      Button.Appearance.Hot.Font.Name := Font.Name;
      Button.Appearance.Hot.Font.Size := Font.Size;
      Button.Appearance.Focused.Font.Name := Font.Name;
      Button.Appearance.Focused.Font.Size := Font.Size;
      Button.Appearance.Down.Font.Name := Font.Name;
      Button.Appearance.Down.Font.Size := Font.Size;
      Button.Appearance.Disabled.Font.Name := Font.Name;
      Button.Appearance.Disabled.Font.Size := Font.Size;
    end;
end;

procedure TFormMain.SetCombosParams;
var
  i: integer;
  ccb, ccbT: TJppColorComboBox;
begin
  ccbT := ccbBorderWhenDefaultColor;

  for i := 0 to ComponentCount - 1 do
    if Components[i] is TJppColorComboBox then
    begin
      ccb := Components[i] as TJppColorComboBox;
      if ccb = ccbT then Continue;
      ccb.AssignParams(ccbT);
      ccb.Width := ccbT.Width;
    end;
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

  ccbBorderWhenDefaultColor.SelectedColor := ColorMap.BorderWhenDefaultColor;
  ccbFocusRectColor.SelectedColor := ColorMap.FocusRectColor;

  ccbNormal_BorderColor.SelectedColor := ColorMap.Normal.BorderColor;
  ccbNormal_FontColor.SelectedColor := ColorMap.Normal.FontColor;
  ccbNormal_Color.SelectedColor := ColorMap.Normal.Color;
  ccbNormal_UpperGradientColorFrom.SelectedColor := ColorMap.Normal.UpperGradientColorFrom;
  ccbNormal_UpperGradientColorTo.SelectedColor := ColorMap.Normal.UpperGradientColorTo;
  ccbNormal_BottomGradientColorFrom.SelectedColor := ColorMap.Normal.BottomGradientColorFrom;
  ccbNormal_BottomGradientColorTo.SelectedColor := ColorMap.Normal.BottomGradientColorTo;

  ccbHot_BorderColor.SelectedColor := ColorMap.Hot.BorderColor;
  ccbHot_FontColor.SelectedColor := ColorMap.Hot.FontColor;
  ccbHot_Color.SelectedColor := ColorMap.Hot.Color;
  ccbHot_UpperGradientColorFrom.SelectedColor := ColorMap.Hot.UpperGradientColorFrom;
  ccbHot_UpperGradientColorTo.SelectedColor := ColorMap.Hot.UpperGradientColorTo;
  ccbHot_BottomGradientColorFrom.SelectedColor := ColorMap.Hot.BottomGradientColorFrom;
  ccbHot_BottomGradientColorTo.SelectedColor := ColorMap.Hot.BottomGradientColorTo;

  ccbDown_BorderColor.SelectedColor := ColorMap.Down.BorderColor;
  ccbDown_FontColor.SelectedColor := ColorMap.Down.FontColor;
  ccbDown_Color.SelectedColor := ColorMap.Down.Color;
  ccbDown_UpperGradientColorFrom.SelectedColor := ColorMap.Down.UpperGradientColorFrom;
  ccbDown_UpperGradientColorTo.SelectedColor := ColorMap.Down.UpperGradientColorTo;
  ccbDown_BottomGradientColorFrom.SelectedColor := ColorMap.Down.BottomGradientColorFrom;
  ccbDown_BottomGradientColorTo.SelectedColor := ColorMap.Down.BottomGradientColorTo;

  ccbFocused_BorderColor.SelectedColor := ColorMap.Focused.BorderColor;
  ccbFocused_FontColor.SelectedColor := ColorMap.Focused.FontColor;
  ccbFocused_Color.SelectedColor := ColorMap.Focused.Color;
  ccbFocused_UpperGradientColorFrom.SelectedColor := ColorMap.Focused.UpperGradientColorFrom;
  ccbFocused_UpperGradientColorTo.SelectedColor := ColorMap.Focused.UpperGradientColorTo;
  ccbFocused_BottomGradientColorFrom.SelectedColor := ColorMap.Focused.BottomGradientColorFrom;
  ccbFocused_BottomGradientColorTo.SelectedColor := ColorMap.Focused.BottomGradientColorTo;

  ccbDisabled_BorderColor.SelectedColor := ColorMap.Disabled.BorderColor;
  ccbDisabled_FontColor.SelectedColor := ColorMap.Disabled.FontColor;
  ccbDisabled_Color.SelectedColor := ColorMap.Disabled.Color;
  ccbDisabled_UpperGradientColorFrom.SelectedColor := ColorMap.Disabled.UpperGradientColorFrom;
  ccbDisabled_UpperGradientColorTo.SelectedColor := ColorMap.Disabled.UpperGradientColorTo;
  ccbDisabled_BottomGradientColorFrom.SelectedColor := ColorMap.Disabled.BottomGradientColorFrom;
  ccbDisabled_BottomGradientColorTo.SelectedColor := ColorMap.Disabled.BottomGradientColorTo;
end;

procedure TFormMain.actApplyColorsExecute(Sender: TObject);
var
  ColorMap: TJppPngButtonColorMap;
  i: integer;
  Button: TJppPngButton;
begin
  ColorMap.BorderWhenDefaultColor := ccbBorderWhenDefaultColor.SelectedColor;
  ColorMap.FocusRectColor := ccbFocusRectColor.SelectedColor;

  ColorMap.Normal.BorderColor := ccbNormal_BorderColor.SelectedColor;
  ColorMap.Normal.FontColor := ccbNormal_FontColor.SelectedColor;
  ColorMap.Normal.Color := ccbNormal_Color.SelectedColor;
  ColorMap.Normal.UpperGradientColorFrom := ccbNormal_UpperGradientColorFrom.SelectedColor;
  ColorMap.Normal.UpperGradientColorTo := ccbNormal_UpperGradientColorTo.SelectedColor;
  ColorMap.Normal.BottomGradientColorFrom := ccbNormal_BottomGradientColorFrom.SelectedColor;
  ColorMap.Normal.BottomGradientColorTo := ccbNormal_BottomGradientColorTo.SelectedColor;

  ColorMap.Hot.BorderColor := ccbHot_BorderColor.SelectedColor;
  ColorMap.Hot.FontColor := ccbHot_FontColor.SelectedColor;
  ColorMap.Hot.Color := ccbHot_Color.SelectedColor;
  ColorMap.Hot.UpperGradientColorFrom := ccbHot_UpperGradientColorFrom.SelectedColor;
  ColorMap.Hot.UpperGradientColorTo := ccbHot_UpperGradientColorTo.SelectedColor;
  ColorMap.Hot.BottomGradientColorFrom := ccbHot_BottomGradientColorFrom.SelectedColor;
  ColorMap.Hot.BottomGradientColorTo := ccbHot_BottomGradientColorTo.SelectedColor;

  ColorMap.Down.BorderColor := ccbDown_BorderColor.SelectedColor;
  ColorMap.Down.FontColor := ccbDown_FontColor.SelectedColor;
  ColorMap.Down.Color := ccbDown_Color.SelectedColor;
  ColorMap.Down.UpperGradientColorFrom := ccbDown_UpperGradientColorFrom.SelectedColor;
  ColorMap.Down.UpperGradientColorTo := ccbDown_UpperGradientColorTo.SelectedColor;
  ColorMap.Down.BottomGradientColorFrom := ccbDown_BottomGradientColorFrom.SelectedColor;
  ColorMap.Down.BottomGradientColorTo := ccbDown_BottomGradientColorTo.SelectedColor;

  ColorMap.Focused.BorderColor := ccbFocused_BorderColor.SelectedColor;
  ColorMap.Focused.FontColor := ccbFocused_FontColor.SelectedColor;
  ColorMap.Focused.Color := ccbFocused_Color.SelectedColor;
  ColorMap.Focused.UpperGradientColorFrom := ccbFocused_UpperGradientColorFrom.SelectedColor;
  ColorMap.Focused.UpperGradientColorTo := ccbFocused_UpperGradientColorTo.SelectedColor;
  ColorMap.Focused.BottomGradientColorFrom := ccbFocused_BottomGradientColorFrom.SelectedColor;
  ColorMap.Focused.BottomGradientColorTo := ccbFocused_BottomGradientColorTo.SelectedColor;

  ColorMap.Disabled.BorderColor := ccbDisabled_BorderColor.SelectedColor;
  ColorMap.Disabled.FontColor := ccbDisabled_FontColor.SelectedColor;
  ColorMap.Disabled.Color := ccbDisabled_Color.SelectedColor;
  ColorMap.Disabled.UpperGradientColorFrom := ccbDisabled_UpperGradientColorFrom.SelectedColor;
  ColorMap.Disabled.UpperGradientColorTo := ccbDisabled_UpperGradientColorTo.SelectedColor;
  ColorMap.Disabled.BottomGradientColorFrom := ccbDisabled_BottomGradientColorFrom.SelectedColor;
  ColorMap.Disabled.BottomGradientColorTo := ccbDisabled_BottomGradientColorTo.SelectedColor;

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
  ccbBackground.SelectedColor := ColorToRGB(clBtnFace);
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
    Ini.WriteInteger('MAIN', 'BackgroundColor', ColorToRGB(pnTestButtons.Appearance.BackgroundColor));
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
    pnTestButtons.Appearance.BackgroundColor := Ini.ReadInteger('MAIN', 'BackgroundColor', pnTestButtons.Appearance.BackgroundColor);
    ccbBackground.SelectedColor := pnTestButtons.Appearance.BackgroundColor;
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
  pnTestButtons.Appearance.BackgroundColor := ccbBackground.SelectedColor;
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
    BorderColor := ccbDisabled_BorderColor.SelectedColor;
    FontColor := ccbDisabled_FontColor.SelectedColor;
    Color := ccbDisabled_Color.SelectedColor;
    BottomGradientColorFrom := ccbDisabled_BottomGradientColorFrom.SelectedColor;
    BottomGradientColorTo := ccbDisabled_BottomGradientColorTo.SelectedColor;
    UpperGradientColorFrom := ccbDisabled_UpperGradientColorFrom.SelectedColor;
    UpperGradientColorTo := ccbDisabled_UpperGradientColorTo.SelectedColor;
  end;
end;

procedure TFormMain.btnCopyDownClick(Sender: TObject);
begin
  with brec do
  begin
    BorderColor := ccbDown_BorderColor.SelectedColor;
    FontColor := ccbDown_FontColor.SelectedColor;
    Color := ccbDown_Color.SelectedColor;
    BottomGradientColorFrom := ccbDown_BottomGradientColorFrom.SelectedColor;
    BottomGradientColorTo := ccbDown_BottomGradientColorTo.SelectedColor;
    UpperGradientColorFrom := ccbDown_UpperGradientColorFrom.SelectedColor;
    UpperGradientColorTo := ccbDown_UpperGradientColorTo.SelectedColor;
  end;
end;

procedure TFormMain.btnCopyFocusedClick(Sender: TObject);
begin
  with brec do
  begin
    BorderColor := ccbFocused_BorderColor.SelectedColor;
    FontColor := ccbFocused_FontColor.SelectedColor;
    Color := ccbFocused_Color.SelectedColor;
    BottomGradientColorFrom := ccbFocused_BottomGradientColorFrom.SelectedColor;
    BottomGradientColorTo := ccbFocused_BottomGradientColorTo.SelectedColor;
    UpperGradientColorFrom := ccbFocused_UpperGradientColorFrom.SelectedColor;
    UpperGradientColorTo := ccbFocused_UpperGradientColorTo.SelectedColor;
  end;
end;

procedure TFormMain.btnCopyHotClick(Sender: TObject);
begin
  with brec do
  begin
    BorderColor := ccbHot_BorderColor.SelectedColor;
    FontColor := ccbHot_FontColor.SelectedColor;
    Color := ccbHot_Color.SelectedColor;
    BottomGradientColorFrom := ccbHot_BottomGradientColorFrom.SelectedColor;
    BottomGradientColorTo := ccbHot_BottomGradientColorTo.SelectedColor;
    UpperGradientColorFrom := ccbHot_UpperGradientColorFrom.SelectedColor;
    UpperGradientColorTo := ccbHot_UpperGradientColorTo.SelectedColor;
  end;
end;

procedure TFormMain.btnCopyNormalClick(Sender: TObject);
begin
  with brec do
  begin
    BorderColor := ccbNormal_BorderColor.SelectedColor;
    FontColor := ccbNormal_FontColor.SelectedColor;
    Color := ccbNormal_Color.SelectedColor;
    BottomGradientColorFrom := ccbNormal_BottomGradientColorFrom.SelectedColor;
    BottomGradientColorTo := ccbNormal_BottomGradientColorTo.SelectedColor;
    UpperGradientColorFrom := ccbNormal_UpperGradientColorFrom.SelectedColor;
    UpperGradientColorTo := ccbNormal_UpperGradientColorTo.SelectedColor;
  end;
end;

procedure TFormMain.btnPasteDisabledClick(Sender: TObject);
begin
  ccbDisabled_BorderColor.SelectedColor := brec.BorderColor;
  ccbDisabled_FontColor.SelectedColor := brec.FontColor;
  ccbDisabled_Color.SelectedColor := brec.Color;
  ccbDisabled_BottomGradientColorFrom.SelectedColor := brec.BottomGradientColorFrom;
  ccbDisabled_BottomGradientColorTo.SelectedColor := brec.BottomGradientColorTo;
  ccbDisabled_UpperGradientColorFrom.SelectedColor := brec.UpperGradientColorFrom;
  ccbDisabled_UpperGradientColorTo.SelectedColor := brec.UpperGradientColorTo;
  actApplyColors.Execute;
end;

procedure TFormMain.btnPasteDownClick(Sender: TObject);
begin
  ccbDown_BorderColor.SelectedColor := brec.BorderColor;
  ccbDown_FontColor.SelectedColor := brec.FontColor;
  ccbDown_Color.SelectedColor := brec.Color;
  ccbDown_BottomGradientColorFrom.SelectedColor := brec.BottomGradientColorFrom;
  ccbDown_BottomGradientColorTo.SelectedColor := brec.BottomGradientColorTo;
  ccbDown_UpperGradientColorFrom.SelectedColor := brec.UpperGradientColorFrom;
  ccbDown_UpperGradientColorTo.SelectedColor := brec.UpperGradientColorTo;
  actApplyColors.Execute;
end;

procedure TFormMain.btnPasteFocusedClick(Sender: TObject);
begin
  ccbFocused_BorderColor.SelectedColor := brec.BorderColor;
  ccbFocused_FontColor.SelectedColor := brec.FontColor;
  ccbFocused_Color.SelectedColor := brec.Color;
  ccbFocused_BottomGradientColorFrom.SelectedColor := brec.BottomGradientColorFrom;
  ccbFocused_BottomGradientColorTo.SelectedColor := brec.BottomGradientColorTo;
  ccbFocused_UpperGradientColorFrom.SelectedColor := brec.UpperGradientColorFrom;
  ccbFocused_UpperGradientColorTo.SelectedColor := brec.UpperGradientColorTo;
  actApplyColors.Execute;
end;

procedure TFormMain.btnPasteHotClick(Sender: TObject);
begin
  ccbHot_BorderColor.SelectedColor := brec.BorderColor;
  ccbHot_FontColor.SelectedColor := brec.FontColor;
  ccbHot_Color.SelectedColor := brec.Color;
  ccbHot_BottomGradientColorFrom.SelectedColor := brec.BottomGradientColorFrom;
  ccbHot_BottomGradientColorTo.SelectedColor := brec.BottomGradientColorTo;
  ccbHot_UpperGradientColorFrom.SelectedColor := brec.UpperGradientColorFrom;
  ccbHot_UpperGradientColorTo.SelectedColor := brec.UpperGradientColorTo;
  actApplyColors.Execute;
end;

procedure TFormMain.btnPasteNormalClick(Sender: TObject);
begin
  ccbNormal_BorderColor.SelectedColor := brec.BorderColor;
  ccbNormal_FontColor.SelectedColor := brec.FontColor;
  ccbNormal_Color.SelectedColor := brec.Color;
  ccbNormal_BottomGradientColorFrom.SelectedColor := brec.BottomGradientColorFrom;
  ccbNormal_BottomGradientColorTo.SelectedColor := brec.BottomGradientColorTo;
  ccbNormal_UpperGradientColorFrom.SelectedColor := brec.UpperGradientColorFrom;
  ccbNormal_UpperGradientColorTo.SelectedColor := brec.UpperGradientColorTo;
  actApplyColors.Execute;
end;



{$endregion}

end.
