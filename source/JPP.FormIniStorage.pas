unit JPP.FormIniStorage;

{
  DEPRECATED
  DO NOT USE!
}

{$I jpp.inc}

interface


uses
  {$IFDEF MSWINDOWS}Windows,{$ENDIF}
  SysUtils, Classes, IniFiles, AnsiStrings, {$IFDEF HAS_SYSTEM_UITYPES}System.UITypes,{$ENDIF} RTTI,
  Controls, StdCtrls, ExtCtrls, ComCtrls, Forms, Dialogs, Graphics,
  JPP.Types, JPL.MemIniFile, JPL.Strings;


const
  varExeName = '<ExeName>';
  varExeDir = '<ExeDir>';
  varFormName = '<FormName>';

type

  TJppTextEncoding = (teASCII, teANSI, teUnicode, teUTF8, teDefault);

  {$region ' ---------------------- INT - TJPControlBasicParams ----------------------- '}
  TJppControlBasicStorageParams = class(TPersistent)
  private
    FWidth: Boolean;
    FHeight: Boolean;
    FLeft: Boolean;
    FTop: Boolean;
    FEnabled: Boolean;
    FVisible: Boolean;
    FTag: Boolean;
    FHint: Boolean;
    FShowHint: Boolean;
    FHelpType: Boolean;
    FHelpKeyword: Boolean;
    FHelpContext: Boolean;
    procedure SetWidth(const Value: Boolean);
    procedure SetHeight(const Value: Boolean);
    procedure SetLeft(const Value: Boolean);
    procedure SetTop(const Value: Boolean);
    procedure SetEnabled(const Value: Boolean);
    procedure SetVisible(const Value: Boolean);
    procedure SetTag(const Value: boolean);
    procedure SetHint(const Value: Boolean);
    procedure SetShowHint(const Value: Boolean);
    procedure SetHelpContext(const Value: Boolean);
    procedure SetHelpKeyword(const Value: Boolean);
    procedure SetHelpType(const Value: Boolean);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
  protected
  published
    property Width: Boolean read FWidth write SetWidth default False;
    property Height: Boolean read FHeight write SetHeight default False;
    property Left: Boolean read FLeft write SetLeft default False;
    property Top: Boolean read FTop write SetTop default False;
    property Enabled: Boolean read FEnabled write SetEnabled default False;
    property Visible: Boolean read FVisible write SetVisible default False;
    property Tag: Boolean read FTag write SetTag default False;
    property Hint: Boolean read FHint write SetHint default False;
    property ShowHint: Boolean read FShowHint write SetShowHint default False;
    property HelpContext: Boolean read FHelpContext write SetHelpContext default False;
    property HelpKeyword: Boolean read FHelpKeyword write SetHelpKeyword default False;
    property HelpType: Boolean read FHelpType write SetHelpType default False;
  end;
  {$endregion}

  {$region ' ---------------------- INT - TJppFontStorageParams -------------------------- '}
  TJppFontStorageParams = class(TPersistent)
  private
    FName: Boolean;
    FColor: Boolean;
    FSize: Boolean;
    FStyle: Boolean;
    procedure SetColor(const Value: Boolean);
    procedure SetName(const Value: Boolean);
    procedure SetSize(const Value: Boolean);
    procedure SetStyle(const Value: Boolean);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
  published
    property Color: Boolean read FColor write SetColor default False;
    property Name: Boolean read FName write SetName default False;
    property Size: Boolean read FSize write SetSize default False;
    property Style: Boolean read FStyle write SetStyle default False;
  end;
  {$endregion}

  {$region ' ---------------------- INT - TJppFormStorageParams -------------------------- '}
  TJppFormStorageParams = class(TPersistent)
  private
    FBasicParams: TJppControlBasicStorageParams;
    FBiDiMode: Boolean;
    FCaption: Boolean;
    FColor: Boolean;
    FFont: TJppFontStorageParams;
    FMaximized: Boolean;
    procedure SetBasicParams(const Value: TJppControlBasicStorageParams);
    procedure SetBiDiMode(const Value: Boolean);
    procedure SetCaption(const Value: Boolean);
    procedure SetColor(const Value: Boolean);
    procedure SetFont(const Value: TJppFontStorageParams);
    procedure SetMaximized(const Value: Boolean);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
  published
    property BasicParams: TJppControlBasicStorageParams read FBasicParams write SetBasicParams;
    property BiDiMode: Boolean read FBiDiMode write SetBiDiMode default False;
    property Caption: Boolean read FCaption write SetCaption default False;
    property Color: Boolean read FColor write SetColor default False;
    property Maximized: Boolean read FMaximized write SetMaximized default True;
    property Font: TJppFontStorageParams read FFont write SetFont;
  end;
  {$endregion}

  {$region ' ---------------------- INT - TJppCheckBoxStorageParams -------------------------- '}
  TJppCheckBoxStorageParams = class(TPersistent)
  private
    FState: Boolean;
    FFont: TJppFontStorageParams;
    FCaption: Boolean;
    FBasicParams: TJppControlBasicStorageParams;
    FAllowGrayed: Boolean;
    procedure SetBasicParams(const Value: TJppControlBasicStorageParams);
    procedure SetCaption(const Value: Boolean);
    procedure SetFont(const Value: TJppFontStorageParams);
    procedure SetState(const Value: Boolean);
    procedure SetAllowGrayed(const Value: Boolean);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
  published
    property BasicParams: TJppControlBasicStorageParams read FBasicParams write SetBasicParams;
    property Caption: Boolean read FCaption write SetCaption default False;
    property AllowGrayed: Boolean read FAllowGrayed write SetAllowGrayed default True;
    property State: Boolean read FState write SetState default True;
    property Font: TJppFontStorageParams read FFont write SetFont;
  end;
  {$endregion}

  {$region ' ---------------------- INT - TJppRadioButtonStorageParams -------------------------- '}
  TJppRadioButtonStorageParams = class(TPersistent)
  private
    FFont: TJppFontStorageParams;
    FCaption: Boolean;
    FBasicParams: TJppControlBasicStorageParams;
    FChecked: Boolean;
    procedure SetBasicParams(const Value: TJppControlBasicStorageParams);
    procedure SetCaption(const Value: Boolean);
    procedure SetChecked(const Value: Boolean);
    procedure SetFont(const Value: TJppFontStorageParams);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
  published
    property BasicParams: TJppControlBasicStorageParams read FBasicParams write SetBasicParams;
    property Caption: Boolean read FCaption write SetCaption default False;
    property Checked: Boolean read FChecked write SetChecked default True;
    property Font: TJppFontStorageParams read FFont write SetFont;
  end;
  {$endregion}

  {$region ' ---------------------- INT - TJppEditStorageParams -------------------------- '}
  TJppEditStorageParams = class(TPersistent)
  private
    FMaxLength: Boolean;
    FNumbersOnly: Boolean;
    FColor: Boolean;
    FFont: TJppFontStorageParams;
    FBasicParams: TJppControlBasicStorageParams;
    FText: Boolean;
    FReadOnly: Boolean;
    FTextHint: Boolean;
    procedure SetBasicParams(const Value: TJppControlBasicStorageParams);
    procedure SetColor(const Value: Boolean);
    procedure SetFont(const Value: TJppFontStorageParams);
    procedure SetMaxLength(const Value: Boolean);
    procedure SetNumbersOnly(const Value: Boolean);
    procedure SetReadOnly(const Value: Boolean);
    procedure SetText(const Value: Boolean);
    procedure SetTextHint(const Value: Boolean);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
  published
    property BasicParams: TJppControlBasicStorageParams read FBasicParams write SetBasicParams;
    property Font: TJppFontStorageParams read FFont write SetFont;
    property Color: Boolean read FColor write SetColor default False;
    property MaxLength: Boolean read FMaxLength write SetMaxLength default False;
    property NumbersOnly: Boolean read FNumbersOnly write SetNumbersOnly default False;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default False;
    property Text: Boolean read FText write SetText default True;
    property TextHint: Boolean read FTextHint write SetTextHint default False;
  end;
  {$endregion}

  {$region ' ---------------------- INT - TJppComboBoxStorageParams -------------------------- '}
  TJppComboBoxStorageParams = class(TPersistent)
  private
    FItemIndex: Boolean;
    FSorted: Boolean;
    FItems: Boolean;
    FColor: Boolean;
    FFont: TJppFontStorageParams;
    FBasicParams: TJppControlBasicStorageParams;
    FText: Boolean;
    FStyle: Boolean;
    FTextHint: Boolean;
    procedure SetBasicParams(const Value: TJppControlBasicStorageParams);
    procedure SetColor(const Value: Boolean);
    procedure SetFont(const Value: TJppFontStorageParams);
    procedure SetItemIndex(const Value: Boolean);
    procedure SetItems(const Value: Boolean);
    procedure SetSorted(const Value: Boolean);
    procedure SetStyle(const Value: Boolean);
    procedure SetText(const Value: Boolean);
    procedure SetTextHint(const Value: Boolean);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
  published
    property BasicParams: TJppControlBasicStorageParams read FBasicParams write SetBasicParams;
    property Font: TJppFontStorageParams read FFont write SetFont;
    property Color: Boolean read FColor write SetColor default False;
    property ItemIndex: Boolean read FItemIndex write SetItemIndex default True;
    property Items: Boolean read FItems write SetItems default False;
    property Sorted: Boolean read FSorted write SetSorted default False;
    property Style: Boolean read FStyle write SetStyle default False;
    property Text: Boolean read FText write SetText default True;
    property TextHint: Boolean read FTextHint write SetTextHint default False;
  end;
  {$endregion}

  {$region ' ---------------------- INT - TJMemoStorageParams -------------------------- '}
  TJppMemoStorageParams = class(TPersistent)
  private
    FLines: Boolean;
    FFont: TJppFontStorageParams;
    FBasicParams: TJppControlBasicStorageParams;
    FColor: Boolean;
    FWordWrap: Boolean;
    FCompressLines: Boolean;
    procedure SetBasicParams(const Value: TJppControlBasicStorageParams);
    procedure SetLines(const Value: Boolean);
    procedure SetFont(const Value: TJppFontStorageParams);
    procedure SetColor(const Value: Boolean);
    procedure SetWordWrap(const Value: Boolean);
    procedure SetCompressLines(const Value: Boolean);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
  published
    property BasicParams: TJppControlBasicStorageParams read FBasicParams write SetBasicParams;
    property Lines: Boolean read FLines write SetLines default False;
    property Font: TJppFontStorageParams read FFont write SetFont;
    property Color: Boolean read FColor write SetColor default False;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default False;
    property CompressLines: Boolean read FCompressLines write SetCompressLines default False;
  end;
  {$endregion}


  TJPStorageParams = class(TPersistent)
  private
    FForm: TJppFormStorageParams;
    FCheckBoxes: TJppCheckBoxStorageParams;
    FRadioButtons: TJppRadioButtonStorageParams;
    FEdits: TJppEditStorageParams;
    FComboBoxes: TJppComboBoxStorageParams;
    FMemos: TJppMemoStorageParams;
    procedure SetForm(const Value: TJppFormStorageParams);
    procedure SetCheckBoxes(const Value: TJppCheckBoxStorageParams);
    procedure SetRadioButtons(const Value: TJppRadioButtonStorageParams);
    procedure SetEdits(const Value: TJppEditStorageParams);
    procedure SetComboBoxes(const Value: TJppComboBoxStorageParams);
    procedure SetMemos(const Value: TJppMemoStorageParams);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
  published
    property CheckBoxes: TJppCheckBoxStorageParams read FCheckBoxes write SetCheckBoxes;
    property ComboBoxes: TJppComboBoxStorageParams read FComboBoxes write SetComboBoxes;
    property Edits: TJppEditStorageParams read FEdits write SetEdits;
    property Form: TJppFormStorageParams read FForm write SetForm;
    property RadioButtons: TJppRadioButtonStorageParams read FRadioButtons write SetRadioButtons;
    property Memos: TJppMemoStorageParams read FMemos write SetMemos;
  end;


  {$region ' ------------------------ INT - TJppFormIniStorage -------------------------------- '}
  TJppFormIniStorage = class(TComponent)
  private
    LoadingParams: Boolean;
    ControlList: TStringList;
    IniEncoding: TEncoding;

    FEnabled: Boolean;
    FFileName: string;
    FEncoding: TJppTextEncoding;
    FForm: TForm;
    FSection: string;
    FKeyPrefix: string;
    FBeforeLoadIni: TNotifyEvent;
    FAfterLoadIni: TNotifyEvent;
    FParamsToStore: TJPStorageParams;
    FParamsToLoad: TJPStorageParams;
    procedure SetEnabled(const Value: Boolean);
    procedure SetEncoding(const Value: TJppTextEncoding);
    procedure SetForm(const Value: TForm);
    procedure SetFileName(const Value: string);
    procedure SetSection(const Value: string);
    function CanLoadSave: Boolean;
    function GetFileName: string;
    function GetSectionName: string;
    function GetKeyPrefix: string;
    procedure SetKeyPrefix(const Value: string);
    procedure DetectControls;
    function ExpandStrVars(s: string): string;
    procedure SaveControlParamsToIni(Ini: TJppMemIniFile; Section: string);
    procedure LoadControlParamsFromIni(Ini: TJppMemIniFile; Section: string);
    function GetControl(ControlName: string): TWinControl;
    procedure SetBeforeLoadIni(const Value: TNotifyEvent);
    procedure SetAfterLoadIni(const Value: TNotifyEvent);
    procedure SetParamsToStore(const Value: TJPStorageParams);
    procedure SetParamsToLoad(const Value: TJPStorageParams);
  protected
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SaveToFile;
    procedure LoadFromFile;
  published
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Endoding: TJppTextEncoding read FEncoding write SetEncoding default teDefault;
    property Form: TForm read FForm write SetForm;
    property FileName: string read FFileName write SetFileName;
    property Section: string read FSection write SetSection;
    property KeyPrefix: string read FKeyPrefix write SetKeyPrefix;
    property ParamsToStore: TJPStorageParams read FParamsToStore write SetParamsToStore;
    property ParamsToLoad: TJPStorageParams read FParamsToLoad write SetParamsToLoad;

    // events
    property BeforeLoadIni: TNotifyEvent read FBeforeLoadIni write SetBeforeLoadIni;
    property AfterLoadIni: TNotifyEvent read FAfterLoadIni write SetAfterLoadIni;
  end;
  {$endregion}




implementation



{$region ' ------------------ helpers ------------------ '}


procedure ___Enum_GetControl(WinControl: TWinControl; ControlToFind: string; var ResultControl: TWinControl);
var
  i: integer;
begin
  if ResultControl <> nil then Exit;

  for i := 0 to WinControl.ControlCount - 1 do
    if WinControl.Controls[i] is TWinControl then
      if WinControl.Controls[i].Name = ControlToFind then
      begin
        ResultControl := TWinControl(WinControl.Controls[i]);
        Break;
      end
      else ___Enum_GetControl(TWinControl(WinControl.Controls[i]), ControlToFind, ResultControl);
end;


procedure ___Enum_GetControlList(Control: TWinControl; var sl: TStringList);
var
  i: integer;
begin
  for i := 0 to Control.ControlCount - 1 do
  begin
    if Control.Controls[i] is TWinControl then
    begin
      sl.Add(Control.Controls[i].Name);
      ___Enum_GetControlList(TWinControl(Control.Controls[i]), sl);
    end;
  end;
end;


{$endregion helpers}


{$region ' ---------------------------------------------------- IMP - TJppFormIniStorage ------------------------------------------------------------ '}

constructor TJppFormIniStorage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  if AOwner is TForm then FForm := TForm(AOwner);
  FEnabled := True;
  FFileName := varExeDir + '\' + varExeName + '.ini';
  FSection := varFormName;
  IniEncoding := TEncoding.Default;
  FKeyPrefix := '';

  FParamsToStore := TJPStorageParams.Create(AOwner);
  FParamsToLoad := TJPStorageParams.Create(AOwner);

//  FParamsToStoreOLD := [
//    cpCheckBox_Checked, cpRadioButton_Checked, cpRadioGroup_ItemIndex, cpEdit_Text, cpLabeledEdit_Text, cpButtonedEdit_Text,
//    cpComboBox_Text, cpComboBox_ItemIndex, cpListBox_Items, cpListBox_ItemIndex, cpMemo_Lines,
//    cpPageControl_ActivePageIndex, cpUpDown_Position, cpTrackBar_Position,
//    cpListView_ColWidths
//  ];
//  FParamsToLoadOLD := FParamsToStoreOLD;

  ControlList := TStringList.Create;
  LoadingParams := False;

end;

destructor TJppFormIniStorage.Destroy;
begin
  ControlList.Free;
  FParamsToStore.Free;
  FParamsToLoad.Free;
  inherited;
end;

procedure TJppFormIniStorage.Loaded;
begin
  inherited;
  if Owner <> nil then Owner.FreeNotification(Self);
  LoadFromFile;
end;

procedure TJppFormIniStorage.Notification(AComponent: TComponent; Operation: TOperation);
begin

  if (csDestroying in Owner.ComponentState) and (not (csDesigning in ComponentState)) then
  begin
    SaveToFile;
    FEnabled := False;
  end;

  inherited Notification(AComponent, Operation);
end;

procedure TJppFormIniStorage.DetectControls;
begin
  if not Assigned(FForm) then Exit;
  ControlList.Clear;

  ___Enum_GetControlList(FForm, ControlList);
  ControlList.Sort;
end;

function TJppFormIniStorage.ExpandStrVars(s: string): string;
begin
  s := StringReplace(s, VarExeName, ChangeFileExt(ExtractFileName(ParamStr(0)), ''), [rfReplaceAll, rfIgnoreCase]);
  s := StringReplace(s, VarExeDir, ExtractFileDir(ParamStr(0)), [rfReplaceAll, rfIgnoreCase]);
  if Assigned(FForm) then s := StringReplace(s, VarFormName, FForm.Name, [rfReplaceAll, rfIgnoreCase]);
  Result := s;
end;

function TJppFormIniStorage.GetFileName: string;
var
  fName: string;
begin
  fName := Trim(FFileName);
  fName := ExpandStrVars(fName);
  Result := fName;
end;

function TJppFormIniStorage.GetKeyPrefix: string;
begin
  Result := Trim(FKeyPrefix);
  Result := StringReplace(Result, ' ', '_', [rfReplaceAll]);
end;

function TJppFormIniStorage.GetSectionName: string;
var
  s: string;
begin
  s := Trim(FSection);
  if Assigned(FForm) then s := ExpandStrVars(s)
  else s := 'MAIN';
  Result := s;
end;

function TJppFormIniStorage.CanLoadSave: Boolean;
begin
  Result := not (csDesigning in ComponentState) and FEnabled and Assigned(FForm) and (Trim(FFileName) <> '');
end;

function TJppFormIniStorage.GetControl(ControlName: string): TWinControl;
var
  C: TWinControl;
begin
  Result := nil;
  if not Assigned(FForm) then Exit;

  C := nil;
  ___Enum_GetControl(FForm, ControlName, C);
  Result := C;
end;

procedure ___WriteBasicParams(Control: TWinControl; Ini: TJppMemIniFile; Section: string; BaseName: string; Params: TJppControlBasicStorageParams; bWritePos: Boolean = True);
begin
  if bWritePos then
  begin
    if Params.Width then Ini.WriteInteger(Section, BaseName + '.Width', Control.Width);
    if Params.Height then Ini.WriteInteger(Section, BaseName + '.Height', Control.Height);
    if Params.Left then Ini.WriteInteger(Section, BaseName + '.Left', Control.Left);
    if Params.Top then Ini.WriteInteger(Section, BaseName + '.Top', Control.Top);
  end;
  if Params.Enabled then Ini.WriteBool(Section, BaseName + '.Enabled', Control.Enabled);
  if Params.Hint then Ini.WriteString(Section, BaseName + '.Hint', Control.Hint);
  if Params.ShowHint then Ini.WriteBool(Section, BaseName + '.ShowHint', Control.ShowHint);
  if Params.Tag then Ini.WriteInteger(Section, BaseName + '.Tag', Control.Tag);
  if Params.Visible then Ini.WriteBool(Section, BaseName + '.Visible', Control.Visible);
  if Params.HelpContext then Ini.WriteInteger(Section, BaseName + '.HelpContext', Control.HelpContext);
  if Params.HelpKeyword then Ini.WriteString(Section, BaseName + '.HelpKeyword', Control.HelpKeyword);
  if Params.HelpType then Ini.WriteInteger(Section, BaseName + '.HelpType', integer(Control.HelpType));
end;

procedure ___WriteFontParams(Control: TWinControl; Ini: TJppMemIniFile; Section: string; BaseName: string; FontParams: TJppFontStorageParams);
var
  RContext: TRttiContext;
  RType: TRttiType;
  RProperty: TRttiProperty;
  Value: TValue;
  Font: TFont;
begin
  try

    RContext := TRttiContext.Create;
    try
      RType := RContext.GetType(Control.ClassType);

      RProperty := RType.GetProperty('Font');
      if Assigned(RProperty) and (RProperty.PropertyType.ToString = 'TFont') and (RProperty.IsReadable) then
      begin
        Value := RProperty.GetValue(Control);
        Font := TFont(Value.AsObject);
        if FontParams.Name then Ini.WriteString(Section, BaseName + '.Font.Name', Font.Name);
        if FontParams.Color then Ini.WriteColor(Section, BaseName + '.Font.Color', Font.Color);
        if FontParams.Size then Ini.WriteInteger(Section, BaseName + '.Font.Size', Font.Size);
        if FontParams.Style then Ini.WriteFontStyles(Section, BaseName + '.Font.Style', Font.Style);
      end;

    finally
      RContext.Free;
    end;

  except
  end;
end;

procedure TJppFormIniStorage.SaveControlParamsToIni(Ini: TJppMemIniFile; Section: string);
var
  i: integer;
  Control: TWinControl;
  ControlName, BaseName: string;
  Prefix, MemoSection: string;
  //lv: TListView;
  Params: TJPStorageParams;
begin
  if not Assigned(FForm) then Exit;

  Prefix := GetKeyPrefix;
  Params := FParamsToStore;

  for i := 0 to ControlList.Count - 1 do
  begin

    ControlName := ControlList[i];

    Control := GetControl(ControlName);

    if Assigned(Control) then
    begin
      BaseName := Prefix + ControlName;

      if Control.ClassName = 'TGroupButton' then begin end {RadioGroup button}

      else if (Control is TCheckBox) then with Control as TCheckBox do
      begin
        if Params.CheckBoxes.Caption then Ini.WriteString(Section, BaseName + '.Caption', Caption);
        if Params.CheckBoxes.AllowGrayed then Ini.WriteBool(Section, BaseName + '.AllowGrayed', AllowGrayed);
        if Params.CheckBoxes.State then Ini.WriteInteger(Section, BaseName + '.State', integer(State));
        ___WriteBasicParams(Control, Ini, Section, BaseName, Params.CheckBoxes.BasicParams);
        ___WriteFontParams(Control, Ini, Section, BaseName, Params.CheckBoxes.Font);
      end

      else if (Control is TComboBox) then with Control as TComboBox do
      begin
        if Params.ComboBoxes.Color then Ini.WriteColor(Section, BaseName + '.Color', Color);
        if Params.ComboBoxes.ItemIndex then Ini.WriteInteger(Section, BaseName + '.ItemIndex', ItemIndex);
        if Params.ComboBoxes.Items then Ini.WriteStrings(GetSectionName + '.' + BaseName + '.Items', Items);
        if Params.ComboBoxes.Sorted then Ini.WriteBool(Section, BaseName + '.Sorted', Sorted);
        if Params.ComboBoxes.Style then Ini.WriteInteger(Section, BaseName + '.Style', integer(Style));
        if Params.ComboBoxes.Text then Ini.WriteString(Section, BaseName + '.Text', Text);
        if Params.ComboBoxes.TextHint then Ini.WriteString(Section, BaseName + '.TextHint', TextHint);
        ___WriteBasicParams(Control, Ini, Section, BaseName, Params.ComboBoxes.BasicParams);
        ___WriteFontParams(Control, Ini, Section, BaseName, Params.ComboBoxes.Font);
      end

      else if (Control is TEdit) then with Control as TEdit do
      begin
        if Params.Edits.Color then Ini.WriteColor(Section, BaseName + '.Color', Color);
        if Params.Edits.MaxLength then Ini.WriteInteger(Section, BaseName + '.MaxLength', MaxLength);
        if Params.Edits.NumbersOnly then Ini.WriteBool(Section, BaseName + '.NumbersOnly', NumbersOnly);
        if Params.Edits.ReadOnly then Ini.WriteBool(Section, BaseName + '.ReadOnly', ReadOnly);
        if Params.Edits.Text then Ini.WriteString(Section, BaseName + '.Text', Text);
        if Params.Edits.TextHint then Ini.WriteString(Section, BaseName + '.TextHint', TextHint);
        ___WriteBasicParams(Control, Ini, Section, BaseName, Params.Edits.BasicParams);
        ___WriteFontParams(Control, Ini, Section, BaseName, Params.Edits.Font);
      end

      else if (Control is TRadioButton) then with Control as TRadioButton do
      begin
        if Params.RadioButtons.Caption then Ini.WriteString(Section, BaseName + '.Caption', Caption);
        if Params.RadioButtons.Checked then Ini.WriteBool(Section, BaseName + '.Checked', Checked);
        ___WriteBasicParams(Control, Ini, Section, BaseName, Params.RadioButtons.BasicParams);
        ___WriteFontParams(Control, Ini, Section, BaseName, Params.RadioButtons.Font);
      end

      else if (Control is TMemo) then with Control as TMemo do
      begin
        if Params.Memos.Color then Ini.WriteColor(Section, BaseName + '.Color', Color);
        if Params.Memos.WordWrap then Ini.WriteBool(Section, BaseName + '.WordWrap', WordWrap);
        if Params.Memos.Lines then
        begin
          MemoSection := Section + '.' + BaseName + '.Lines';
          Ini.EraseSection(MemoSection);
          Ini.WriteStrings(MemoSection, Lines, 5000, Params.Memos.CompressLines);
        end;
        ___WriteBasicParams(Control, Ini, Section, BaseName, Params.Memos.BasicParams);
        ___WriteFontParams(Control, Ini, Section, BaseName, Params.Memos.Font);
      end

//      else if (Control is TRadioGroup) and (cpRadioGroup_ItemIndex in FParamsToStoreOLD) then
//        Ini.WriteInteger(Section, KeyName, (Control as TRadioGroup).ItemIndex)
//
//      else if (Control is TLabeledEdit) and (cpLabeledEdit_Text in FParamsToStoreOLD) then
//        Ini.WriteString(Section, KeyName, (Control as TLabeledEdit).Text)
//
//      else if (Control is TButtonedEdit) and (cpButtonedEdit_Text in FParamsToStoreOLD) then
//        Ini.WriteString(Section, KeyName, (Control as TButtonedEdit).Text)
//
//      else if (Control is TPageControl) and (cpPageControl_ActivePageIndex in FParamsToStoreOLD) then
//        Ini.WriteInteger(Section, KeyName, (Control as TPageControl).ActivePageIndex)
//
//      else if (Control is TUpDown) and (cpUpDown_Position in FParamsToStoreOLD) then
//        Ini.WriteInteger(Section, KeyName, (Control as TUpDown).Position)
//
//      else if (Control is TTrackBar) and (cpTrackBar_Position in FParamsToStoreOLD) then
//        Ini.WriteInteger(Section, KeyName, (Control as TTrackBar).Position
//
//      else if (Control is TListView) and (cpListView_ColWidths in FParamsToStoreOLD) then
//      begin
//        lv := Control as TListView;
//        s := '';
//        for y := 0 to lv.Columns.Count - 1 do
//          s := s + IntToStr(lv.Column[y].Width) + ',';
//        if Copy(s, Length(s), 1) = ',' then Delete(s, Length(s), 1);
//        Ini.WriteString(Section, KeyName, s);
//      end

    end; // if Assigned


  end; // for
end;

procedure TJppFormIniStorage.SaveToFile;
var
  fName, Section, Prefix, BaseName: string;
  Ini: TJppMemIniFile;
  Params: TJPStorageParams;
begin
  if not CanLoadSave then Exit;
  if LoadingParams then Exit;

  fName := GetFileName;
  Section := GetSectionName;
  Prefix := GetKeyPrefix;
  BaseName := Prefix + Section;

  Ini := TJppMemIniFile.Create(fName, IniEncoding);
  try

    Params := FParamsToStore;


    if Params.Form.Maximized then
    begin
      Ini.WriteBool(Section, BaseName + '.Maximized', FForm.WindowState = wsMaximized);
      ___WriteBasicParams(FForm, Ini, Section, BaseName, Params.Form.BasicParams, FForm.WindowState <> wsMaximized);
      //Showmessage(IntToStr(FForm.Width));
    end
    else
    begin
      ___WriteBasicParams(FForm, Ini, Section, BaseName, Params.Form.BasicParams, True);
    end;
    if Params.Form.BiDiMode then Ini.WriteInteger(Section, BaseName + '.BiDiMode', integer(FForm.BiDiMode));
    if Params.Form.Caption then Ini.WriteString(Section, BaseName + '.Caption', FForm.Caption);
    if Params.Form.Color then Ini.WriteColor(Section, BaseName + '.Color', FForm.Color);

    ___WriteFontParams(FForm, Ini, Section, BaseName, Params.Form.Font);

    DetectControls;
    Section := Section; // + '_Controls';
    //Ini.EraseSection(Section); //<--  usun¹æ !!!!!!!!!!!!
    SaveControlParamsToIni(Ini, Section);

    Ini.UpdateFile;  //showmessage('Ini file updated');

  finally
    Ini.Free;
  end;

end;

procedure ___ReadBasicParams(Control: TWinControl; Ini: TJppMemIniFile; Section: string; BaseName: string; Params: TJppControlBasicStorageParams);
begin
  if Params.Width then Control.Width := Ini.ReadInteger(Section, BaseName + '.Width', Control.Width);
  if Params.Height then Control.Height := Ini.ReadInteger(Section, BaseName + '.Height', Control.Height);
  if Params.Left then Control.Left := Ini.ReadInteger(Section, BaseName + '.Left', Control.Left);
  if Params.Top then Control.Top := Ini.ReadInteger(Section, BaseName + '.Top', Control.Top);

  if Params.Enabled then Control.Enabled := Ini.ReadBool(Section, BaseName + '.Enabled', Control.Enabled);
  if Params.Hint then Control.Hint := Ini.ReadString(Section, BaseName + '.Hint', Control.Hint);
  if Params.ShowHint then Control.ShowHint := Ini.ReadBool(Section, BaseName + '.ShowHint', Control.ShowHint);
  if Params.Tag then Control.Tag := Ini.ReadInteger(Section, BaseName + '.Tag', Control.Tag);
  if Params.Visible then Control.Visible := Ini.ReadBool(Section, BaseName + '.Visible', Control.Visible);
  if Params.HelpContext then Control.HelpContext := Ini.ReadInteger(Section, BaseName + '.HelpContext', Control.HelpContext);
  if Params.HelpKeyword then Control.HelpKeyword := Ini.ReadString(Section, BaseName + '.HelpKeyword', Control.HelpKeyword);
  if Params.HelpType then Control.HelpType := THelpType(Ini.ReadInteger(Section, BaseName + '.HelpType', integer(Control.HelpType)));
end;

procedure ___ReadFontParams(Control: TWinControl; Ini: TJppMemIniFile; Section: string; BaseName: string; FontParams: TJppFontStorageParams);
var
  RContext: TRttiContext;
  RType: TRttiType;
  RProperty: TRttiProperty;
  Value: TValue;
  Font: TFont;
begin
  try

    RContext := TRttiContext.Create;
    try
      RType := RContext.GetType(Control.ClassType);

      RProperty := RType.GetProperty('Font');
      if Assigned(RProperty) and (RProperty.PropertyType.ToString = 'TFont') and (RProperty.IsWritable) then
      begin
        Value := RProperty.GetValue(Control);
        Font := TFont(Value.AsObject);
        if FontParams.Name then Font.Name := Ini.ReadString(Section, BaseName + '.Font.Name', Font.Name);
        if FontParams.Color then Font.Color := Ini.ReadColor(Section, BaseName + '.Font.Color', Font.Color);
        if FontParams.Size then Font.Size := Ini.ReadInteger(Section, BaseName + '.Font.Size', Font.Size);
        if FontParams.Style then Font.Style := Ini.ReadFontStyles(Section, BaseName + '.Font.Style', Font.Style);
      end;

    finally
      RContext.Free;
    end;

  except
  end;
end;

procedure TJppFormIniStorage.LoadControlParamsFromIni(Ini: TJppMemIniFile; Section: string);
var
  i: integer;
  Control: TWinControl;
  BaseName, ControlName: string;
  Prefix: string;
  Params: TJPStorageParams;
  //sl: TStringList;
  //lv: TListView;


  function CheckValueExists: Boolean;
  begin
    if Ini.ValueExists(Section, BaseName) then Result := True
    else if Ini.ValueExists(Section, BaseName + '___ItemIndex') then Result := True
    else if Ini.ValueExists(Section, BaseName + '___Text') then Result := True
    else if Ini.SectionExists(Section + '_' + ControlName + '_Lines') then Result := True
    else Result := False;
  end;

begin
  if not Assigned(FForm) then Exit;


  Prefix := GetKeyPrefix;
  Params := FParamsToLoad;

  for i := 0 to ControlList.Count - 1 do
  begin

    ControlName := ControlList[i];
    BaseName := Prefix + ControlName;
    //if not CheckValueExists then Continue;

    Control := GetControl(ControlName);

    if Assigned(Control) then
    begin

      if (Control is TCheckBox) then with Control as TCheckBox do
      begin
        if Params.CheckBoxes.AllowGrayed then AllowGrayed := Ini.ReadBool(Section, BaseName + '.AllowGrayed', AllowGrayed);
        if Params.CheckBoxes.Caption then Caption := Ini.ReadString(Section, BaseName + '.Caption', Caption);
        if Params.CheckBoxes.State then State := TCheckBoxState( Ini.ReadInteger(Section, BaseName + '.State', integer(State)) );
        ___ReadBasicParams(Control, Ini, Section, BaseName, Params.CheckBoxes.BasicParams);
        ___ReadFontParams(Control, Ini, Section, BaseName, Params.CheckBoxes.Font);
      end

      else if (Control is TComboBox) then with Control as TComboBox do
      begin
        if Params.ComboBoxes.Color then Color := Ini.ReadColor(Section, BaseName + '.Color', Color);
        if Params.ComboBoxes.ItemIndex then ItemIndex := Ini.ReadInteger(Section, BaseName + '.ItemIndex', ItemIndex);
        if Params.ComboBoxes.Items then Ini.ReadStrings(GetSectionName + '.' + BaseName + '.Items', Items);
        if Params.ComboBoxes.Sorted then Sorted := Ini.ReadBool(Section, BaseName + '.Sorted', Sorted);
        if Params.ComboBoxes.Style then Style := TComboBoxStyle(Ini.ReadInteger(Section, BaseName + '.Style', integer(Style)));
        if Params.ComboBoxes.Text then Text := Ini.ReadString(Section, BaseName + '.Text', Text);
        if Params.ComboBoxes.TextHint then TextHint := Ini.ReadString(Section, BaseName + '.TextHint', TextHint);
        ___ReadBasicParams(Control, Ini, Section, BaseName, Params.ComboBoxes.BasicParams);
        ___ReadFontParams(Control, Ini, Section, BaseName, Params.ComboBoxes.Font);
      end

      else if (Control is TEdit) then with Control as TEdit do
      begin
        if Params.Edits.Color then Color := Ini.ReadColor(Section, BaseName + '.Color', Color);
        if Params.Edits.MaxLength then MaxLength := Ini.ReadInteger(Section, BaseName + '.MaxLength', MaxLength);
        if Params.Edits.NumbersOnly then NumbersOnly := Ini.ReadBool(Section, BaseName + '.NumbersOnly', NumbersOnly);
        if Params.Edits.ReadOnly then ReadOnly := Ini.ReadBool(Section, BaseName + '.ReadOnly', ReadOnly);
        if Params.Edits.Text then Text := Ini.ReadString(Section, BaseName + '.Text', Text);
        if Params.Edits.TextHint then TextHint := Ini.ReadString(Section, BaseName + '.TextHint', TextHint);
        ___ReadBasicParams(Control, Ini, Section, BaseName, Params.Edits.BasicParams);
        ___ReadFontParams(Control, Ini, Section, BaseName, Params.Edits.Font);
      end

      else if (Control is TRadioButton) then with Control as TRadioButton do
      begin
        if Params.RadioButtons.Checked then Checked := Ini.ReadBool(Section, BaseName + '.Checked', Checked);
        if Params.RadioButtons.Caption then Caption := Ini.ReadString(Section, BaseName + '.Caption', Caption);
        ___ReadBasicParams(Control, Ini, Section, BaseName, Params.RadioButtons.BasicParams);
        ___ReadFontParams(Control, Ini, Section, BaseName, Params.RadioButtons.Font);
      end

      else if (Control is TMemo) then with Control as TMemo do
      begin
        if Params.Memos.Color then Color := Ini.ReadColor(Section, BaseName + '.Color', Color);
        if Params.Memos.WordWrap then WordWrap := Ini.ReadBool(Section, BaseName + '.WordWrap', WordWrap);
        if Params.Memos.Lines then Ini.ReadStrings(Section + '.' + BaseName + '.Lines', Lines, Params.Memos.CompressLines);
        ___ReadBasicParams(Control, Ini, Section, BaseName, Params.Memos.BasicParams);
        ___ReadFontParams(Control, Ini, Section, BaseName, Params.Memos.Font);
      end

//
//      else if (Control is TRadioGroup) and (cpRadioGroup_ItemIndex in FParamsToLoadOLD) then
//        (Control as TRadioGroup).ItemIndex := Ini.ReadInteger(Section, KeyName, (Control as TRadioGroup).ItemIndex)
//
//      else if (Control is TLabeledEdit) and (cpLabeledEdit_Text in FParamsToLoadOLD) then
//        (Control as TLabeledEdit).Text := Ini.ReadString(Section, KeyName, (Control as TLabeledEdit).Text)
//
//      else if (Control is TButtonedEdit) and (cpButtonedEdit_Text in FParamsToLoadOLD) then
//        (Control as TButtonedEdit).Text := Ini.ReadString(Section, KeyName, (Control as TButtonedEdit).Text)
//
//      else if (Control is TPageControl) and (cpPageControl_ActivePageIndex in FParamsToLoadOLD) then
//        (Control as TPageControl).ActivePageIndex := Ini.ReadInteger(Section, KeyName, (Control as TPageControl).ActivePageIndex)
//
//      else if (Control is TUpDown) and (cpUpDown_Position in FParamsToLoadOLD) then
//        (Control as TUpDown).Position := Ini.ReadInteger(Section, KeyName, (Control as TUpDown).Position)
//
//      else if (Control is TTrackBar) and (cpTrackBar_Position in FParamsToLoadOLD) then
//        (Control as TTrackBar).Position := Ini.ReadInteger(Section, KeyName, (Control as TTrackbar).Position)
//
//      else if (Control is TListView) then
//      begin
//        lv := Control as TListView;
//        s := Ini.ReadString(Section, KeyName, '');
//
//        sl := TStringList.Create;
//        try
//          ___MakeListFromStr(s, sl, ',');
//          for y := 0 to sl.Count - 1 do
//          begin
//            if y > lv.Columns.Count - 1 then Break;
//            sv := sl[y];
//            if TryStrToInt(sv, x) then lv.Columns[y].Width := x;
//          end;
//        finally
//          sl.Free;
//        end;
//
//      end; // ListView


    end; // if Assigned


  end; // for

end;

procedure TJppFormIniStorage.LoadFromFile;
var
  fName, Section, Prefix, BaseName: string;
  Ini: TJppMemIniFile;
  Params: TJPStorageParams;
begin
  if Assigned(FBeforeLoadIni) then FBeforeLoadIni(Self);
  LoadingParams := True;

  Params := FParamsToLoad;

  try

    if CanLoadSave then
    begin

      fName := GetFileName;
      Section := GetSectionName;
      Prefix := GetKeyPrefix;
      BaseName := Prefix + Section;

      Ini := TJppMemIniFile.Create(fName, IniEncoding);
      try

        if Params.Form.BasicParams.Width then FForm.Width := Ini.ReadInteger(Section, BaseName + '.Width', FForm.Width);   //showmessage(IntToStr(FForm.Width));
        if Params.Form.BasicParams.Height then FForm.Height := Ini.ReadInteger(Section, BaseName + '.Height', FForm.Height);
        if Params.Form.BasicParams.Left then FForm.Left := Ini.ReadInteger(Section, BaseName + '.Left', FForm.Left);
        if Params.Form.BasicParams.Top then FForm.Top := Ini.ReadInteger(Section, BaseName + '.Top', FForm.Top);
        if Params.Form.Maximized then if Ini.ReadBool(Section, BaseName + '.Maximized', False) then
        begin    //ShowMessage('max');
          //ShowWindow(FForm.Handle, SW_SHOWMAXIMIZED);

          FForm.WindowState := wsMaximized;

        end;
        if Params.Form.BiDiMode then FForm.BiDiMode := TBiDiMode( Ini.ReadInteger(Section, BaseName + '.BiDiMode', integer(FForm.BiDiMode)) );
        if Params.Form.Caption then FForm.Caption := Ini.ReadString(Section, BaseName + '.Caption', FForm.Caption);
        if Params.Form.Color then FForm.Color := Ini.ReadColor(Section, BaseName + '.Color', FForm.Color);
        ___ReadBasicParams(FForm, Ini, Section, BaseName, FParamsToLoad.Form.BasicParams);
        ___ReadFontParams(FForm, Ini, Section, BaseName, FParamsToLoad.Form.Font);

        DetectControls;
        Section := Section; // + '_Controls';
        LoadControlParamsFromIni(Ini, Section);

      finally
        Ini.Free;
      end;
    end;


  finally
    if Assigned(FAfterLoadIni) then FAfterLoadIni(Self);
    LoadingParams := False;
  end;

end;

procedure TJppFormIniStorage.SetAfterLoadIni(const Value: TNotifyEvent);
begin
  FAfterLoadIni := Value;
end;

procedure TJppFormIniStorage.SetBeforeLoadIni(const Value: TNotifyEvent);
begin
  FBeforeLoadIni := Value;
end;

procedure TJppFormIniStorage.SetEnabled(const Value: Boolean);
begin
  FEnabled := Value;
end;

procedure TJppFormIniStorage.SetEncoding(const Value: TJppTextEncoding);
begin
  FEncoding := Value;
  case Value of
    teASCII: IniEncoding := TEncoding.ASCII;
    teANSI: IniEncoding := {$IFDEF HAS_ANSI_ENCODING}TEncoding.ANSI;{$ELSE}TEncoding.Default;{$ENDIF}
    teUnicode: IniEncoding := TEncoding.Unicode;
    teUTF8: IniEncoding := TEncoding.UTF8;
  else
    IniEncoding := TEncoding.Default;
  end;
end;

procedure TJppFormIniStorage.SetForm(const Value: TForm);
begin
  FForm := Value;
end;

procedure TJppFormIniStorage.SetKeyPrefix(const Value: string);
begin
  FKeyPrefix := Value;
end;

procedure TJppFormIniStorage.SetParamsToLoad(const Value: TJPStorageParams);
begin
  FParamsToLoad := Value;
end;

procedure TJppFormIniStorage.SetParamsToStore(const Value: TJPStorageParams);
begin
  FParamsToStore := Value;
end;

procedure TJppFormIniStorage.SetFileName(const Value: string);
begin
  FFileName := Value;
end;

procedure TJppFormIniStorage.SetSection(const Value: string);
begin
  FSection := Value;
end;


{$endregion TJppFormIniStorage}


{$region ' ---------------------------------- IMP - TJPControlBasicParams ------------------------------------------- '}

constructor TJppControlBasicStorageParams.Create(AOwner: TComponent);
begin
  inherited Create;
  FWidth := False;
  FHeight := False;
  FLeft := False;
  FTop := False;
  FEnabled := False;
  FVisible := False;
  FTag := False;
  FHint := False;
  FShowHint := False;
  FHelpContext := False;
  FHelpKeyword := False;
  FHelpType := False;
end;

destructor TJppControlBasicStorageParams.Destroy;
begin
  inherited;
end;



procedure TJppControlBasicStorageParams.SetEnabled(const Value: Boolean);
begin
  FEnabled := Value;
end;

procedure TJppControlBasicStorageParams.SetHeight(const Value: Boolean);
begin
  FHeight := Value;
end;

procedure TJppControlBasicStorageParams.SetHelpContext(const Value: Boolean);
begin
  FHelpContext := Value;
end;

procedure TJppControlBasicStorageParams.SetHelpKeyword(const Value: Boolean);
begin
  FHelpKeyword := Value;
end;

procedure TJppControlBasicStorageParams.SetHelpType(const Value: Boolean);
begin
  FHelpType := Value;
end;

procedure TJppControlBasicStorageParams.SetHint(const Value: Boolean);
begin
  FHint := Value;
end;

procedure TJppControlBasicStorageParams.SetLeft(const Value: Boolean);
begin
  FLeft := Value;
end;

procedure TJppControlBasicStorageParams.SetShowHint(const Value: Boolean);
begin
  FShowHint := Value;
end;

procedure TJppControlBasicStorageParams.SetTag(const Value: Boolean);
begin
  FTag := Value;
end;

procedure TJppControlBasicStorageParams.SetTop(const Value: Boolean);
begin
  FTop := Value;
end;

procedure TJppControlBasicStorageParams.SetVisible(const Value: Boolean);
begin
  FVisible := Value;
end;

procedure TJppControlBasicStorageParams.SetWidth(const Value: Boolean);
begin
  FWidth := Value;
end;

{$endregion TJPControlBasicParams}


{$region ' -------------------------------- IMP - TJPFormParams -------------------------------------- '}

constructor TJppFormStorageParams.Create(AOwner: TComponent);
begin
  inherited Create;
  FBasicParams := TJppControlBasicStorageParams.Create(AOwner);
  FFont := TJppFontStorageParams.Create(AOwner);

  FBiDiMode := False;
  FCaption := False;
  FColor := False;
  FMaximized := True;

  FBasicParams.FWidth := True;
  FBasicParams.FHeight := True;
  FBasicParams.FLeft := True;
  FBasicParams.FTop := True;
end;

destructor TJppFormStorageParams.Destroy;
begin
  FBasicParams.Free;
  FFont.Free;
  inherited;
end;

procedure TJppFormStorageParams.SetBasicParams(const Value: TJppControlBasicStorageParams);
begin
  FBasicParams := Value;
end;

procedure TJppFormStorageParams.SetBiDiMode(const Value: Boolean);
begin
  FBiDiMode := Value;
end;

procedure TJppFormStorageParams.SetCaption(const Value: Boolean);
begin
  FCaption := Value;
end;


procedure TJppFormStorageParams.SetColor(const Value: Boolean);
begin
  FColor := Value;
end;

procedure TJppFormStorageParams.SetFont(const Value: TJppFontStorageParams);
begin
  FFont := Value;
end;

procedure TJppFormStorageParams.SetMaximized(const Value: Boolean);
begin
  FMaximized := Value;
end;

{$endregion}


{$region ' ------------------------- IMP - TJppFontStorageParams ------------------------------ '}

constructor TJppFontStorageParams.Create(AOwner: TComponent);
begin
  inherited Create;
  FColor := False;
  FName := False;
  FSize := False;
  FStyle := False;
end;

destructor TJppFontStorageParams.Destroy;
begin
  inherited;
end;

procedure TJppFontStorageParams.SetColor(const Value: Boolean);
begin
  FColor := Value;
end;

procedure TJppFontStorageParams.SetName(const Value: Boolean);
begin
  FName := Value;
end;

procedure TJppFontStorageParams.SetSize(const Value: Boolean);
begin
  FSize := Value;
end;

procedure TJppFontStorageParams.SetStyle(const Value: Boolean);
begin
  FStyle := Value;
end;
{$endregion}


{$region ' -------------------------------- IMP - TJPStorageParams ---------------------------------------------- '}

constructor TJPStorageParams.Create(AOwner: TComponent);
begin
  inherited Create;
  FForm := TJppFormStorageParams.Create(AOwner);
  FCheckBoxes := TJppCheckBoxStorageParams.Create(AOwner);
  FRadioButtons := TJppRadioButtonStorageParams.Create(AOwner);
  FEdits := TJppEditStorageParams.Create(AOwner);
  FComboBoxes := TJppComboBoxStorageParams.Create(AOwner);
  FMemos := TJppMemoStorageParams.Create(AOwner);
end;

destructor TJPStorageParams.Destroy;
begin
  FComboBoxes.Free;
  FEdits.Free;
  FRadioButtons.Free;
  FCheckBoxes.Free;
  FMemos.Free;
  FForm.Free;
  inherited;
end;

procedure TJPStorageParams.SetCheckBoxes(const Value: TJppCheckBoxStorageParams);
begin
  FCheckBoxes := Value;
end;

procedure TJPStorageParams.SetComboBoxes(const Value: TJppComboBoxStorageParams);
begin
  FComboBoxes := Value;
end;

procedure TJPStorageParams.SetEdits(const Value: TJppEditStorageParams);
begin
  FEdits := Value;
end;

procedure TJPStorageParams.SetForm(const Value: TJppFormStorageParams);
begin
  FForm := Value;
end;
procedure TJPStorageParams.SetMemos(const Value: TJppMemoStorageParams);
begin
  FMemos := Value;
end;

procedure TJPStorageParams.SetRadioButtons(const Value: TJppRadioButtonStorageParams);
begin
  FRadioButtons := Value;
end;

{$endregion}


{$region ' ---------------------------- IMP - TJppCheckBoxStorageParams ---------------------------------- '}

constructor TJppCheckBoxStorageParams.Create(AOwner: TComponent);
begin
  inherited Create;
  FBasicParams := TJppControlBasicStorageParams.Create(AOwner);
  FFont := TJppFontStorageParams.Create(AOwner);
  FCaption := False;
  FAllowGrayed := True;
  FState := True;
end;

destructor TJppCheckBoxStorageParams.Destroy;
begin
  FBasicParams.Free;
  FFont.Free;
  inherited;
end;

procedure TJppCheckBoxStorageParams.SetAllowGrayed(const Value: Boolean);
begin
  FAllowGrayed := Value;
end;

procedure TJppCheckBoxStorageParams.SetBasicParams(const Value: TJppControlBasicStorageParams);
begin
  FBasicParams := Value;
end;

procedure TJppCheckBoxStorageParams.SetCaption(const Value: Boolean);
begin
  FCaption := Value;
end;

procedure TJppCheckBoxStorageParams.SetFont(const Value: TJppFontStorageParams);
begin
  FFont := Value;
end;

procedure TJppCheckBoxStorageParams.SetState(const Value: Boolean);
begin
  FState := Value;
end;
{$endregion}

{$region ' ---------------------------- IMP - TJppRadioButtonStorageParams -------------------------------------- '}

constructor TJppRadioButtonStorageParams.Create(AOwner: TComponent);
begin
  inherited Create;
  FBasicParams := TJppControlBasicStorageParams.Create(AOwner);
  FFont := TJppFontStorageParams.Create(AOwner);
  FChecked := True;
  FCaption := False;
end;

destructor TJppRadioButtonStorageParams.Destroy;
begin
  FBasicParams.Free;
  FFont.Free;
  inherited;
end;

procedure TJppRadioButtonStorageParams.SetBasicParams(const Value: TJppControlBasicStorageParams);
begin
  FBasicParams := Value;
end;

procedure TJppRadioButtonStorageParams.SetCaption(const Value: Boolean);
begin
  FCaption := Value;
end;

procedure TJppRadioButtonStorageParams.SetChecked(const Value: Boolean);
begin
  FChecked := Value;
end;

procedure TJppRadioButtonStorageParams.SetFont(const Value: TJppFontStorageParams);
begin
  FFont := Value;
end;
{$endregion}

{$region ' ---------------------------- IMP - TJppEditStorageParams -------------------------------------- '}

constructor TJppEditStorageParams.Create(AOwner: TComponent);
begin
  inherited Create;
  FBasicParams := TJppControlBasicStorageParams.Create(AOwner);
  FFont := TJppFontStorageParams.Create(AOwner);
  FColor := False;
  FMaxLength := False;
  FNumbersOnly := False;
  FReadOnly := False;
  FText := True;
  FTextHint := False;
end;

destructor TJppEditStorageParams.Destroy;
begin
  FFont.Free;
  FBasicParams.Free;
  inherited;
end;

procedure TJppEditStorageParams.SetBasicParams(const Value: TJppControlBasicStorageParams);
begin
  FBasicParams := Value;
end;

procedure TJppEditStorageParams.SetColor(const Value: Boolean);
begin
  FColor := Value;
end;

procedure TJppEditStorageParams.SetFont(const Value: TJppFontStorageParams);
begin
  FFont := Value;
end;

procedure TJppEditStorageParams.SetMaxLength(const Value: Boolean);
begin
  FMaxLength := Value;
end;

procedure TJppEditStorageParams.SetNumbersOnly(const Value: Boolean);
begin
  FNumbersOnly := Value;
end;

procedure TJppEditStorageParams.SetReadOnly(const Value: Boolean);
begin
  FReadOnly := Value;
end;

procedure TJppEditStorageParams.SetText(const Value: Boolean);
begin
  FText := Value;
end;

procedure TJppEditStorageParams.SetTextHint(const Value: Boolean);
begin
  FTextHint := Value;
end;
{$endregion}

{$region ' ---------------------------- IMP - TJppComboBoxStorageParams -------------------------------------- '}

constructor TJppComboBoxStorageParams.Create(AOwner: TComponent);
begin
  inherited Create;
  FBasicParams := TJppControlBasicStorageParams.Create(AOwner);
  Font := TJppFontStorageParams.Create(AOwner);
  FColor := False;
  FItemIndex := True;
  FItems := False;
  FSorted := False;
  FStyle := False;
  FText := True;
  FTextHint := False;
end;

destructor TJppComboBoxStorageParams.Destroy;
begin
  FBasicParams.Free;
  Font.Free;
  inherited;
end;

procedure TJppComboBoxStorageParams.SetBasicParams(const Value: TJppControlBasicStorageParams);
begin
  FBasicParams := Value;
end;

procedure TJppComboBoxStorageParams.SetColor(const Value: Boolean);
begin
  FColor := Value;
end;

procedure TJppComboBoxStorageParams.SetFont(const Value: TJppFontStorageParams);
begin
  FFont := Value;
end;

procedure TJppComboBoxStorageParams.SetItemIndex(const Value: Boolean);
begin
  FItemIndex := Value;
end;

procedure TJppComboBoxStorageParams.SetItems(const Value: Boolean);
begin
  FItems := Value;
end;

procedure TJppComboBoxStorageParams.SetSorted(const Value: Boolean);
begin
  FSorted := Value;
end;

procedure TJppComboBoxStorageParams.SetStyle(const Value: Boolean);
begin
  FStyle := Value;
end;

procedure TJppComboBoxStorageParams.SetText(const Value: Boolean);
begin
  FText := Value;
end;

procedure TJppComboBoxStorageParams.SetTextHint(const Value: Boolean);
begin
  FTextHint := Value;
end;
{$endregion}


{$region ' ---------------------------- IMP - TJppMemoStorageParams -------------------------------------- '}

constructor TJppMemoStorageParams.Create(AOwner: TComponent);
begin
  inherited Create;
  FBasicParams := TJppControlBasicStorageParams.Create(AOwner);
  Font := TJppFontStorageParams.Create(AOwner);
  FColor := False;
  FLines := False;
  FCompressLines := False;
end;

destructor TJppMemoStorageParams.Destroy;
begin
  FBasicParams.Free;
  Font.Free;
  inherited;
end;

procedure TJppMemoStorageParams.SetBasicParams(const Value: TJppControlBasicStorageParams);
begin
  FBasicParams := Value;
end;

procedure TJppMemoStorageParams.SetColor(const Value: Boolean);
begin
  FColor := Value;
end;

procedure TJppMemoStorageParams.SetCompressLines(const Value: Boolean);
begin
  FCompressLines := Value;
end;

procedure TJppMemoStorageParams.SetFont(const Value: TJppFontStorageParams);
begin
  FFont := Value;
end;

procedure TJppMemoStorageParams.SetLines(const Value: Boolean);
begin
  FLines := Value;
end;

procedure TJppMemoStorageParams.SetWordWrap(const Value: Boolean);
begin
  FWordWrap := Value;
end;

{$endregion}


end.
