unit JPP.ColorComboBox;

{ My old ColorCombo from 2006 }

interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, Winapi.Messages, System.Types,
  Vcl.Controls, Vcl.StdCtrls, Vcl.Graphics, Vcl.Dialogs, Vcl.Buttons, Vcl.Clipbrd, Vcl.ExtCtrls,
  JPP.BasicSpeedButton, JPP.Graphics,
  JPP.Types
  ;


type

  TRGBRec = record
    R, G, B: Byte;
  end;

  //TColorNamesLang = (cnPL, cnEN);

  PJpPJpPJppColorComboParams = ^TJpPJpPJppColorComboParams;
  TJpPJpPJppColorComboParams = record
    ColorRectWidth: integer;
    LeftMargin: integer;
    ColorToDescDistance: integer;
    //Lang: TColorNamesLang;
    ShowRGB: Boolean;
  end;



  {$region ' ------------------ TJppComboBoundLabel -------------------- '}
  TJppComboBoundLabel = class(TCustomLabel)
  private
    function GetTop: Integer;
    function GetLeft: Integer;
    function GetWidth: Integer;
    function GetHeight: Integer;
    procedure SetHeight(const Value: Integer);
    procedure SetWidth(const Value: Integer);
  protected
    procedure AdjustBounds; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property BiDiMode;
    property Caption;
    property Color;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Font;
    property Height: Integer read GetHeight write SetHeight;
    property Left: Integer read GetLeft;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowAccelChar;
    property ShowHint;
    property Top: Integer read GetTop;
    property Touch;
    property Transparent;
    property Layout;
    property WordWrap;
    property Width: Integer read GetWidth write SetWidth;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnGesture;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;
  {$endregion}


  {$region ' --------------- TJppComboButton -------------------- '}
  TJppComboButton = class(TJppBasicSpeedButton)
  private
    FVisible: Boolean;
    FOnVisibleChanged: TNotifyEvent;
    procedure SetVisible(const Value: Boolean);
    procedure SetOnVisibleChanged(const Value: TNotifyEvent);
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Visible: Boolean read FVisible write SetVisible;
    property OnVisibleChanged: TNotifyEvent read FOnVisibleChanged write SetOnVisibleChanged;
  end;
  {$endregion}

  {$region ' ---------------- TJppColorComboBox --------------- '}
  TJppColorComboBox = class(TComboBox)
  private
    FSelected: TColor;
    FParams: TJpPJpPJppColorComboParams;
    FBoundLabel: TJppComboBoundLabel;
    FBoundLabelPosition: TLabelPosition;
    FBoundLabelSpacing: Integer;
    FButtonCopyColor: TJppComboButton;
    FButtonChangeColor: TJppComboButton;
    FButtonsSpacing: integer;
    FButtonPasteColor: TJppComboButton;
    procedure SetShowRGB(const Value: Boolean);
    procedure SetColorToDescDistance(const Value: integer);
    procedure SetColorRectWidth(const Value: integer);
    procedure SetLeftMargin(const Value: integer);
    procedure SetParams(const Value: TJpPJpPJppColorComboParams);
    procedure ButtonCopyColorClick(Sender: TObject);
    procedure ButtonPasteColorClick(Sender: TObject);
    procedure ButtonChangeColorClick(Sender: TObject);
    procedure SetBoundLabelPosition(const Value: TLabelPosition);
    procedure SetBoundLabelSpacing(const Value: Integer);
    procedure SetButtonCopyColorPosition;
    procedure SetButtonChangeColorPosition;
    procedure SetButtonPasteColorPosition;
    procedure SetButtonsSpacing(const Value: integer);
    procedure ButtonPositionChanged(Sender: TObject);
  protected
    procedure SetParent(AParent: TWinControl); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMBiDiModeChanged(var Message: TMessage); message CM_BIDIMODECHANGED;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    procedure MeasureItem(Index: Integer; var Height: Integer); override;
    procedure GetColorFromStr(sVal: string; var ColorName: string; var Color: TColor);
    procedure FillItems;
    procedure SetActiveColor(Color: TColor);
    function ColorExists(Color: TColor): Boolean;
    procedure AddColor(ColorName: string; Color: TColor);
    procedure Change; override;
    procedure ShowColorDlg;
    procedure SetupInternalLabel;
    procedure SetupButtonCopyColor;
    procedure SetupButtonChangeColor;
    procedure SetupButtonPasteColor;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer; AHeight: Integer); override;
    procedure SetButtonsPosition;
  published
    property Selected: TColor read FSelected write SetActiveColor;
    property Params: TJpPJpPJppColorComboParams read FParams write SetParams;
    property Param_ColorRectWidth: integer read FParams.ColorRectWidth write SetColorRectWidth default 24;
    property Param_LeftMargin: integer read FParams.LeftMargin write SetLeftMargin default 4;
    property Param_ColorToDescDistance: integer read FParams.ColorToDescDistance write SetColorToDescDistance default 0;
    property Param_ShowRGB: Boolean read FParams.ShowRGB write SetShowRGB default False;

    property BoundLabel: TJppComboBoundLabel read FBoundLabel;
    property BoundLabelPosition: TLabelPosition read FBoundLabelPosition write SetBoundLabelPosition default lpLeft;
    property BoundLabelSpacing: Integer read FBoundLabelSpacing write SetBoundLabelSpacing default 4;

    property ButtonsSpacing: integer read FButtonsSpacing write SetButtonsSpacing;
    property ButtonChangeColor: TJppComboButton read FButtonChangeColor;
    property ButtonCopyColor: TJppComboButton read FButtonCopyColor;
    property ButtonPasteColor: TJppComboButton read FButtonPasteColor;

  end;
  {$endregion}




function IsValidInteger(s: string): Boolean;
function Pad(Text: string; i: integer; znak: char = ' '): string;
function AvgColor(Color1, Color2: TColor): TColor;

procedure Register;



implementation



{$region ' -------------- helpers -------------- '}
function AvgColor(Color1, Color2: TColor): TColor;
var
  r1, r2: TRGBRec;
begin
  Move(Color1, r1, 3);
  Move(Color2, r2, 3);

  Result := RGB(
    (r1.R + r2.R) div 2,
    (r1.G + r2.G) div 2,
    (r1.B + r2.B) div 2
    );
end;

function Pad(Text: string; i: integer; znak: char = ' '): string;
var
  x, y, k: integer;
  s: string;
begin
  if length(Text) < i then
  begin
    x := length(Text);
    y := i - x;
    for k := 1 to y do s := s + znak;
    Text := s + Text;
  end;
  Result := Text;
end;

function IsValidInteger(s: string): Boolean;
begin
  Result := True;
  try
    StrToInt(s);
  except
    Result := False;
  end;
end;

{$endregion}


{$region ' ------------------------------------------------- TJppColorComboBox ------------------------------------------------ '}

  {$region ' ------------------- TJppColorComboBox Create & Destroy ----------------- '}
constructor TJppColorComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  Parent := TWinControl(AOwner);

  Style := csOwnerDrawFixed;
  DropDownCount := 16;

  FillChar(FParams, SizeOf(FParams), 0);
  FParams.ColorRectWidth := 24;
  FParams.LeftMargin := 4;
  FParams.ColorToDescDistance := 0;
  FParams.ShowRGB := False;
  FillItems;
  ItemIndex := 0;


  FBoundLabelPosition := lpLeft;
  FBoundLabelSpacing := 4;
  SetupInternalLabel;

  FButtonsSpacing := 2;
  SetupButtonChangeColor;
  SetupButtonCopyColor;
  SetupButtonPasteColor;
end;

destructor TJppColorComboBox.Destroy;
begin
  inherited;
end;
  {$endregion}

procedure TJppColorComboBox.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);

  if FBoundLabel <> nil then
  begin
    FBoundLabel.Parent := AParent;
    FBoundLabel.Visible := True;
  end;

  if FButtonChangeColor <> nil then
  begin
    FButtonChangeColor.Parent := AParent;
    FButtonChangeColor.Visible := True;
  end;

  if FButtonCopyColor <> nil then
  begin
    FButtonCopyColor.Parent := AParent;
    FButtonCopyColor.Visible := True;
  end;

  if FButtonPasteColor <> nil then
  begin
    FButtonPasteColor.Parent := AParent;
    FButtonPasteColor.Visible := True;
  end;

end;



  {$region ' --------- internal controls ------------- '}


procedure TJppColorComboBox.SetupButtonChangeColor;
begin
  if Assigned(FButtonChangeColor) then Exit;
  FButtonChangeColor := TJppComboButton.Create(Self);
  FButtonChangeColor.Name := 'BtnChangeColor';
  FButtonChangeColor.FreeNotification(Self);
  FButtonChangeColor.OnClick := ButtonChangeColorClick;
  FButtonChangeColor.Height := Height;
  FButtonChangeColor.Width := FButtonChangeColor.Height;
  FButtonChangeColor.Caption := '...';
  FButtonChangeColor.Appearance.Normal.BorderColor := GetSimilarColor(clBtnFace, 15, False);
  FButtonChangeColor.Hint := 'Change color...';
  FButtonChangeColor.OnVisibleChanged := ButtonPositionChanged;
end;

procedure TJppColorComboBox.SetupButtonCopyColor;
begin
  if Assigned(FButtonCopyColor) then Exit;
  FButtonCopyColor := TJppComboButton.Create(Self);
  FButtonCopyColor.Name := 'BtnCopyColor';
  FButtonCopyColor.FreeNotification(Self);
  FButtonCopyColor.OnClick := ButtonCopyColorClick;
  FButtonCopyColor.Height := Height;
  FButtonCopyColor.Width := FButtonCopyColor.Height;
  FButtonCopyColor.Caption := 'C';
  FButtonCopyColor.Appearance.Normal.BorderColor := GetSimilarColor(clBtnFace, 15, False);
  FButtonCopyColor.Hint := 'Copy color';
  FButtonCopyColor.OnVisibleChanged := ButtonPositionChanged;
end;

procedure TJppColorComboBox.SetupButtonPasteColor;
begin
  if Assigned(FButtonPasteColor) then Exit;
  FButtonPasteColor := TJppComboButton.Create(Self);
  FButtonPasteColor.Name := 'BtnPasteColor';
  FButtonPasteColor.FreeNotification(Self);
  FButtonPasteColor.OnClick := ButtonPasteColorClick;
  FButtonPasteColor.Height := Height;
  FButtonPasteColor.Width := FButtonPasteColor.Height;
  FButtonPasteColor.Caption := 'P';
  FButtonPasteColor.Appearance.Normal.BorderColor := GetSimilarColor(clBtnFace, 15, False);
  FButtonPasteColor.Hint := 'Paste color';
  FButtonPasteColor.OnVisibleChanged := ButtonPositionChanged;
end;

procedure TJppColorComboBox.SetButtonChangeColorPosition;
begin
  if not Assigned(FButtonChangeColor) then Exit;
  FButtonChangeColor.Left := Left + Width + FButtonsSpacing;
  FButtonChangeColor.Top := Top;
end;

procedure TJppColorComboBox.SetButtonCopyColorPosition;
var
  x: integer;
begin
  if not Assigned(FButtonCopyColor) then Exit;
  if (Assigned(FButtonChangeColor)) and (FButtonChangeColor.Visible) then x := FButtonChangeColor.Left + FButtonChangeColor.Width
  else x := Left + Width;
  x := x + FButtonsSpacing;
  FButtonCopyColor.Left := x;
  FButtonCopyColor.Top := Top;
end;

procedure TJppColorComboBox.SetButtonPasteColorPosition;
var
  x: integer;
begin
  if not Assigned(FButtonPasteColor) then Exit;
  if (Assigned(FButtonCopyColor)) and (FButtonCopyColor.Visible) then x := FButtonCopyColor.Left + FButtonCopyColor.Width
  else if (Assigned(FButtonChangeColor)) and (FButtonChangeColor.Visible) then x := FButtonChangeColor.Left + FButtonChangeColor.Width
  else x := Left + Width;
  x := x + FButtonsSpacing;
  FButtonPasteColor.Left := x;
  FButtonPasteColor.Top := Top;
end;

procedure TJppColorComboBox.SetButtonsPosition;
begin
  SetButtonChangeColorPosition;
  SetButtonCopyColorPosition;
  SetButtonPasteColorPosition;
end;

procedure TJppColorComboBox.SetButtonsSpacing(const Value: integer);
begin
  FButtonsSpacing := Value;
  SetButtonsPosition;
end;

procedure TJppColorComboBox.ButtonChangeColorClick(Sender: TObject);
begin
  ShowColorDlg;
  if Assigned(OnChange) then OnChange(Self);
end;

procedure TJppColorComboBox.ButtonCopyColorClick(Sender: TObject);
begin
  Clipboard.AsText := IntToStr(ColorToRGB(Selected));
end;

procedure TJppColorComboBox.ButtonPasteColorClick(Sender: TObject);
var
  x: integer;
begin
  if TryStrToInt(Clipboard.AsText, x) then
  begin
    Selected := TColor(x);
    if Assigned(OnChange) then OnChange(Self);
  end;
end;

procedure TJppColorComboBox.ButtonPositionChanged(Sender: TObject);
begin
  SetButtonsPosition;
end;

procedure TJppColorComboBox.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  SetBoundLabelPosition(FBoundLabelPosition);
  SetButtonChangeColorPosition;
  SetButtonCopyColorPosition;
  SetButtonPasteColorPosition;
end;


procedure TJppColorComboBox.SetupInternalLabel;
begin
  if Assigned(FBoundLabel) then Exit;
  FBoundLabel := TJppComboBoundLabel.Create(Self);
  FBoundLabel.FreeNotification(Self);
  FBoundLabel.FocusControl := Self;
  //FBoundLabel.Parent := Self.Parent;
end;

procedure TJppColorComboBox.SetBoundLabelPosition(const Value: TLabelPosition);
var
  P: TPoint;
begin
  if FBoundLabel = nil then Exit;
  FBoundLabelPosition := Value;
  case Value of
    lpAbove: P := Point(Left, Top - FBoundLabel.Height - FBoundLabelSpacing);
    lpBelow: P := Point(Left, Top + Height + FBoundLabelSpacing);
    lpLeft : P := Point(Left - FBoundLabel.Width - FBoundLabelSpacing, Top + ((Height - FBoundLabel.Height) div 2));
    lpRight: P := Point(Left + Width + FBoundLabelSpacing, Top + ((Height - FBoundLabel.Height) div 2));
  end;

  FBoundLabel.SetBounds(P.x, P.y, FBoundLabel.Width, FBoundLabel.Height);
end;

procedure TJppColorComboBox.SetBoundLabelSpacing(const Value: Integer);
begin
  FBoundLabelSpacing := Value;
  SetBoundLabelPosition(FBoundLabelPosition);
end;


procedure TJppColorComboBox.CMBiDiModeChanged(var Message: TMessage);
begin
  inherited;
  if FBoundLabel <> nil then FBoundLabel.BiDiMode := BiDiMode;
  if FButtonChangeColor <> nil then FButtonChangeColor.BiDiMode := BiDiMode;
  if FButtonCopyColor <> nil then FButtonCopyColor.BiDiMode := BiDiMode;
  if FButtonPasteColor <> nil then FButtonPasteColor.BiDiMode := BiDiMode;
end;

procedure TJppColorComboBox.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if FBoundLabel <> nil then FBoundLabel.Enabled := Enabled;
  if FButtonChangeColor <> nil then FButtonChangeColor.Enabled := Enabled;
  if FButtonCopyColor <> nil then FButtonCopyColor.Enabled := Enabled;
  if FButtonPasteColor <> nil then FButtonPasteColor.Enabled := Enabled;
end;

procedure TJppColorComboBox.CMVisibleChanged(var Message: TMessage);
begin
  inherited;
  if FBoundLabel <> nil then FBoundLabel.Visible := Visible;
  if FButtonChangeColor <> nil then FButtonChangeColor.Visible := Visible;
  if FButtonCopyColor <> nil then FButtonCopyColor.Visible := Visible;
  if FButtonPasteColor <> nil then FButtonPasteColor.Visible := Visible;
end;

procedure TJppColorComboBox.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = FBoundLabel then FBoundLabel := nil
    else if AComponent = FButtonChangeColor then FButtonChangeColor := nil
    else if AComponent = FButtonCopyColor then FButtonCopyColor := nil
    else if AComponent = FButtonPasteColor then FButtonPasteColor := nil;
  end;
end;


  {$endregion internal controls}



  {$region ' ------------ TJppColorComboBox.FillItems -------------- '}
procedure TJppColorComboBox.FillItems;
begin
  //if csDesigning in ComponentState then Exit;
  Items.Clear;
  //if Items.Text = '' then
  with Items do
  begin

    Add('Custom...');

    Add('Black=0,0,0');
    Add('Gray 80%=51,51,51');
    Add('Gray 50%=128,128,128');
    Add('Gray 35%=165,165,165');
    Add('Gray 25%=192,192,192');
    Add('Gray 15%=217,217,217');
    Add('Gray 5%=242,242,242');
    Add('Cream=255,251,240');
    Add('White=255,255,255');

    Add('Dark Brown=51,51,0');
    Add('Maroon=128,0,0');
    Add('Brown=153,51,0');
    Add('Red=255,0,0');
    Add('Orange=255,102,0');
    Add('Light Orange=255,153,0');
    Add('Gold=255,204,0');
    Add('Beige=255,204,153');
    Add('Yellow=255,255,0');
    Add('Light Yellow=255,255,153');

    Add('Olive=128,128,0');
      
    Add('Dark Green=0,51,0');
    Add('Green=0,128,0');
    Add('Teal=0,128,128');
    Add('Sea=51,153,102');
    Add('Light Green=153,204,0');
    Add('Lime=0,255,0');
    Add('Money Green=192,220,192');
    Add('Pale Green=204,255,204');

    Add('Navy=0,0,128');
    Add('Indigo=51,51,153');
    Add('Blue=0,0,255');
    Add('Light Blue=51,102,255');
    Add('Azure=0,204,255');
    Add('Pale Blue=153,204,255');
    Add('Aqua=0,255,255');
    Add('Aquamarine=51,204,204');

    Add('Purple=128,0,128');
    Add('Pink=255,0,255');
    Add('Plum=153,51,102');


    Add('-');

  end;
end;
  {$endregion}

  {$region ' -------------- TJppColorComboBox misc --------------- '}
procedure TJppColorComboBox.Change;
var
  s: string;
  cl: TColor;
begin

  if ItemIndex = 0 then ShowColorDlg;
  if ItemIndex > 0 then
  begin
    if Items[ItemIndex] = '-' then Exit;
    GetColorFromStr(Items[ItemIndex], s, cl);
    SetActiveColor(cl);
  end;

  inherited Change;
end;

procedure TJppColorComboBox.ShowColorDlg;
var
  dlgColor: TColorDialog;
begin
  dlgColor := TColorDialog.Create(Self);
  try
    dlgColor.Options := dlgColor.Options + [cdFullOpen, cdAnyColor];
    dlgColor.Color := FSelected;
    if not dlgColor.Execute then
    begin
      SetActiveColor(FSelected);
      Exit;
    end;
    SetActiveColor(dlgColor.Color);
  finally
    dlgColor.Free;
  end;
end;

procedure TJppColorComboBox.SetActiveColor(Color: TColor);
var
  i: integer;
  s: string;
  cl: TColor;
begin
  FSelected := Color;
  if not ColorExists(Color) then AddColor('', Color);

  for i := 1 to Items.Count - 1 do
  begin
    GetColorFromStr(Items[i], s, cl);
    if cl = Color then
    begin
      ItemIndex := i;
      Break;
    end;
  end;
end;

procedure TJppColorComboBox.SetColorRectWidth(const Value: integer);
begin
  FParams.ColorRectWidth := Value;
  Invalidate;
end;

procedure TJppColorComboBox.SetColorToDescDistance(const Value: integer);
begin
  FParams.ColorToDescDistance := Value;
  Invalidate;
end;

procedure TJppColorComboBox.SetLeftMargin(const Value: integer);
begin
  FParams.LeftMargin := Value;
  Invalidate;
end;

procedure TJppColorComboBox.SetParams(const Value: TJpPJpPJppColorComboParams);
begin
  FParams := Value;
  Invalidate;
end;

procedure TJppColorComboBox.SetShowRGB(const Value: Boolean);
begin
  FParams.ShowRGB := Value;
  Invalidate;
end;

procedure TJppColorComboBox.AddColor(ColorName: string; Color: TColor);
var
  s: string;
begin
  s :=
    ColorName + '=' +
    IntToStr(GetRValue(Color)) + ',' +
    IntToStr(GetGValue(Color)) + ',' +
    IntToStr(GetBValue(Color));
  Items.Add(s);
end;

function TJppColorComboBox.ColorExists(Color: TColor): Boolean;
var
  i: integer;
  cl: TColor;
  s: string;
begin
  Result := False;
  if Items.Count = 0 then Exit;
  if Items.Count = 1 then Exit;

  for i := 1 to Items.Count - 1 do
  begin
    GetColorFromStr(Items[i], s, cl);
    if cl = Color then
    begin
      Result := True;
      Break;
    end;
  end;
end;

procedure TJppColorComboBox.MeasureItem(Index: Integer; var Height: Integer);
begin
  inherited;
end;


{$endregion}

  {$region ' ------------- TJppColorComboBox.GetColorFromStr --------------- '}
procedure TJppColorComboBox.GetColorFromStr(sVal: string; var ColorName: string; var Color: TColor);
var
  s: string;
  r, g, b: Byte;
  xp: integer;
begin
  s := Trim(sVal);
  r := 0;
  g := 0;
  b := 0;

  xp := Pos('=', s);
  if xp <= 0 then Exit;
  ColorName := Copy(s, 1, xp - 1);
  s := Copy(s, xp + 1, Length(s));

  xp := AnsiPos(',', s);
  if xp <= 0 then Exit;
  sVal := Copy(s, 1, xp - 1);
  s := Copy(s, xp + 1, Length(s));
  if IsValidInteger(sVal) then r := StrToInt(sVal);

  xp := AnsiPos(',', s);
  if xp <= 0 then Exit;
  sVal := Copy(s, 1, xp - 1);
  s := Copy(s, xp + 1, Length(s));
  if IsValidInteger(sVal) then g := StrToInt(sVal);

  if IsValidInteger(s) then b := StrToInt(s);

  Color := RGB(r, g, b);
end;
  {$endregion}

  {$region ' ------------------------------ TJppColorComboBox.DrawItem ---------------------------------------------- '}
procedure TJppColorComboBox.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
const
  pc = '0';
var
  s, sName: string;
  r, g, b: Byte;
  x, y: integer;
  R2: TRect;
  cl, clBg: TColor;
begin
  s := Trim(Items[Index]);

  with Canvas do
  begin


    Font := Self.Font;
    clBg := Self.Color;
    Brush.Style := bsClear;
    if odFocused in State then clBg := clHighlight;
    if s = '-' then
    begin
      Brush.Style := bsDiagCross;
      clBg := clBtnFace;
      Pen.Color := clBg;
    end;

    Brush.Color := clBg;
    ///////////////
    if s = '-' then Rectangle(Rect) else FillRect(Rect);
    ///////////////

    if s = '-' then
    begin
      //Brush.Color := clRed;
      //x := 0;
      //y := Rect.Top + ((Rect.Bottom - Rect.Top) div 2);
      //Pen.Color := FParams.ColorLine;
      //Pen.Width := 4;
      //MoveTo(x, y);
      //LineTo(x + Rect.Right, y);
      Exit;
    end;


    GetColorFromStr(s, sName, cl);


    if odFocused in State then Font.Color := clHighlightText;
    R2 := Rect;

    R2.Left := FParams.LeftMargin;
    R2.Right := FParams.ColorRectWidth;
    Pen.Width := 0;
    Brush.Color := cl;
    R2.Top := R2.Top + 1;
    R2.Bottom := R2.Bottom - 1;

    Pen.Color := AvgColor(cl, clBg);

    if Index > 0 then Rectangle(R2);

    r := GetRValue(cl);
    g := GetGValue(cl);
    b := GetBValue(cl);

    if FParams.ShowRGB then
    begin
      sName :=
        Pad(IntToStr(r), 3, pc) + ',' +
        Pad(IntToStr(g), 3, pc) + ',' +
        Pad(IntToStr(b), 3, pc) + ' - ' + sName;
      if Copy(sName, Length(sName) - 2, 3) = ' - ' then Delete(sName, Length(sName) - 2, 2);
    end;

    if sName = '' then
      sName :=
        Pad(IntToStr(r), 3, pc) + ',' +
        Pad(IntToStr(g), 3, pc) + ',' +
        Pad(IntToStr(b), 3, pc);

    x := 1 * FParams.LeftMargin + FParams.ColorRectWidth + FParams.ColorToDescDistance;

    Brush.Color := clBg;
    y := R2.Top + ((R2.Bottom - R2.Top) div 2) - TextHeight(sName) div 2;

    if Index > 0 then TextOut(x, y, sName)
    else TextOut(6, y, Items[0]);


  end;
end;
  {$endregion}


{$endregion TJppColorComboBox}


procedure Register;
begin
  RegisterComponents(JPPackPageName, [TJppColorComboBox]);
end;


{$region ' ------------------------------------------ TJppComboBoundLabel ---------------------------------------------- '}

constructor TJppComboBoundLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Name := 'SubLabel';  { do not localize }
  SetSubComponent(True);
  if Assigned(AOwner) then Caption := AOwner.Name;
end;

procedure TJppComboBoundLabel.AdjustBounds;
begin
  inherited AdjustBounds;
  //if Owner is TCustomLabeledEdit then with Owner as TCustomLabeledEdit do SetLabelPosition(LabelPosition);
  if Owner is TJppColorComboBox then with Owner as TJppColorComboBox do SetBoundLabelPosition(BoundLabelPosition);
end;

function TJppComboBoundLabel.GetHeight: Integer;
begin
  Result := inherited Height;
end;

function TJppComboBoundLabel.GetLeft: Integer;
begin
  Result := inherited Left;
end;

function TJppComboBoundLabel.GetTop: Integer;
begin
  Result := inherited Top;
end;

function TJppComboBoundLabel.GetWidth: Integer;
begin
  Result := inherited Width;
end;

procedure TJppComboBoundLabel.SetHeight(const Value: Integer);
begin
  SetBounds(Left, Top, Width, Value);
end;

procedure TJppComboBoundLabel.SetWidth(const Value: Integer);
begin
  SetBounds(Left, Top, Value, Height);
end;
{$endregion TJppComboBoundLabel}


{$region ' ------------------------ TJppComboButton ----------------------------- '}
constructor TJppComboButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Name := 'ComboButton' + IntToStr(Random(10000)) + IntToStr(Random(10000));
  SetSubComponent(True);
end;


procedure TJppComboButton.SetOnVisibleChanged(const Value: TNotifyEvent);
begin
  FOnVisibleChanged := Value;
end;

procedure TJppComboButton.SetVisible(const Value: Boolean);
begin
  inherited;
  FVisible := Value;
  if Value then Show else Hide;
  if Assigned(OnVisibleChanged) then OnVisibleChanged(Self);
end;


{$endregion TJppComboButton}
end.

