unit JPP.BrushStyleComboBox;

{
  Jacek Pazera
  http://www.pazera-software.com
  https://github.com/jackdp
}

{$I jpp.inc}
{$IFDEF FPC} {$mode delphi} {$ENDIF}

interface

uses
  {$IFDEF MSWINDOWS}Windows,{$ENDIF}
  Messages, SysUtils, Classes, Types, {$IFDEF DCC}{$IFDEF HAS_SYSTEM_UITYPES}System.UITypes,{$ENDIF}{$ENDIF}
  Controls, StdCtrls, Graphics, Dialogs, Buttons, Clipbrd, ExtCtrls,
  {$IFDEF FPC}LCLType, LCLIntf, LMessages,{$ENDIF}
  JPL.Strings, JPL.Rects,
  JPP.Common, JPP.Common.Procs, JPP.AnchoredControls;


type

  TJppBrushStyleComboBoxGetDisplayName = procedure(BrushStyle: TBrushStyle; var BrushStyleName: string) of object;


  {$region ' ---------- TJppBrushStyleComboAppearance ----------- '}
  TJppBrushStyleComboAppearance = class(TJppPersistent)
  private
    FOwner: TComponent;
    FColor: TColor;
    FColorDisabled: TColor;
    FColorSelectedBg: TColor;
    FShowStyleNames: Boolean;
    FStyleNameFont: TFont;
    FBrushStyleRectWidth: integer;
    FColorSelectedText: TColor;
    FColorDisabledText: TColor;
    FRectMargins: TJppMargins;
    FTextMargin: integer;
    FColorSelected: TColor;
    {$IFDEF DCC}FDisableFocusRect: Boolean;{$ENDIF}
    procedure SetColor(const Value: TColor);
    procedure SetColorDisabled(const Value: TColor);
    procedure SetColorSelectedBg(const Value: TColor);
    procedure SetShowStyleNames(const Value: Boolean);
    procedure SetStyleNameFont(const Value: TFont);
    procedure SetBrushStyleRectWidth(const Value: integer);
    procedure SetColorSelectedText(const Value: TColor);
    procedure SetColorDisabledText(const Value: TColor);
    procedure SetRectMargins(const Value: TJppMargins);
    procedure SetTextMargin(const Value: integer);
    procedure SetColorSelected(const Value: TColor);
    {$IFDEF DCC}procedure SetDisableFocusRect(const Value: Boolean);{$ENDIF}
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(app: TJppBrushStyleComboAppearance); reintroduce;
  published
    property Color: TColor read FColor write SetColor default clBlack;
    property ColorDisabled: TColor read FColorDisabled write SetColorDisabled default clGray;
    property ColorSelected: TColor read FColorSelected write SetColorSelected default clHighlightText;
    property ColorSelectedBg: TColor read FColorSelectedBg write SetColorSelectedBg default clHighlight;
    property ShowStyleNames: Boolean read FShowStyleNames write SetShowStyleNames default True;
    property StyleNameFont: TFont read FStyleNameFont write SetStyleNameFont;
    property BrushStyleRectWidth: integer read FBrushStyleRectWidth write SetBrushStyleRectWidth default 50;
    property ColorSelectedText: TColor read FColorSelectedText write SetColorSelectedText default clHighlightText;
    property ColorDisabledText: TColor read FColorDisabledText write SetColorDisabledText default clGrayText;
    property RectMargins: TJppMargins read FRectMargins write SetRectMargins;
    property TextMargin: integer read FTextMargin write SetTextMargin default 8;
    {$IFDEF DCC}property DisableFocusRect: Boolean read FDisableFocusRect write SetDisableFocusRect default False;{$ENDIF}
  end;
  {$endregion TJppBrushStyleComboAppearance}


  {$region ' ----------- TJppCustomBrushStyleComboBox ---------- '}
  TJppCustomBrushStyleComboBox = class(TCustomComboBox)
  private
    FUpdateCounter: integer;
    FBoundLabel: TJppControlBoundLabel;
    FBoundLabelPosition: TLabelPosition;
    FBoundLabelSpacing: Integer;
    FTagExt: TJppTagExt;
    FAppearance: TJppBrushStyleComboAppearance;
    FSelected: TBrushStyle;
    FOnGetDisplayName: TJppBrushStyleComboBoxGetDisplayName;
    FAnchoredControls: TJppAnchoredControls;
    procedure SetBoundLabelPosition(const Value: TLabelPosition);
    procedure SetBoundLabelSpacing(const Value: Integer);
    procedure AdjustLabelBounds(Sender: TObject);
    procedure SetTagExt(const Value: TJppTagExt);
    procedure SetAppearance(const Value: TJppBrushStyleComboAppearance);
    procedure SetOnGetDisplayName(const Value: TJppBrushStyleComboBoxGetDisplayName);
    procedure SetAnchoredControls(const Value: TJppAnchoredControls);
  protected
    procedure SetParent(AParent: TWinControl); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMBiDiModeChanged(var Message: TMessage); message CM_BIDIMODECHANGED;
    {$IFDEF DCC}procedure CNDrawItem(var Message: TWMDrawItem); message CN_DRAWITEM;{$ENDIF}
    procedure PropsChanged(Sender: TObject);
    procedure Loaded; override;
    procedure RecreateItems;
    function IsValidIndex(const ItemIndex: integer): Boolean;
    function BrushStyleExists(const bs: TBrushStyle): Boolean;
    function GetBrushStyleIndex(const bs: TBrushStyle): integer;
    function GetBrushStyleDisplayName(const bs: TBrushStyle): string;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateWnd; override;


    function TryGetItemBrushStyle(const ItemIndex: integer; var BrushStyle: TBrushStyle): Boolean;
    function AddBrushStyle(const bs: TBrushStyle): integer; // Returns the index of the added item


    procedure BeginUpdate;
    procedure EndUpdate(bCallOnChangeAfterUpdate: Boolean = True; bResetUpdatingState: Boolean = False);
    function UpdatingControl: Boolean;

    ////////////////////////////////////////////////////////////////////////////////////////
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    ////////////////////////////////////////////////////////////////////////////////////////


    function GetSelectedBrushStyle: TBrushStyle;
    procedure SetSelectedBrushStyle(const Value: TBrushStyle);

    procedure Change; override;
    procedure SetupInternalLabel;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer; AHeight: Integer); override;

    property Selected: TBrushStyle read GetSelectedBrushStyle write SetSelectedBrushStyle;

    property BoundLabel: TJppControlBoundLabel read FBoundLabel;
    property BoundLabelPosition: TLabelPosition read FBoundLabelPosition write SetBoundLabelPosition default lpLeft;
    property BoundLabelSpacing: Integer read FBoundLabelSpacing write SetBoundLabelSpacing default 4;

    property Appearance: TJppBrushStyleComboAppearance read FAppearance write SetAppearance;
    property TagExt: TJppTagExt read FTagExt write SetTagExt;

    property OnGetDisplayName: TJppBrushStyleComboBoxGetDisplayName read FOnGetDisplayName write SetOnGetDisplayName;
    property AnchoredControls: TJppAnchoredControls read FAnchoredControls write SetAnchoredControls;

  end;
  {$endregion TJppCustomBrushStyleComboBox}


  {$Region ' ---------- TJppBrushStyleComboBox ---------- '}
  TJppBrushStyleComboBox = class(TJppCustomBrushStyleComboBox)
    property Align;
    property AutoComplete default True;
    {$IFDEF DCC}property AutoCompleteDelay default 500;{$ENDIF}
    property AutoDropDown default False;
    {$IFDEF DCC}
    property AutoCloseUp default False;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    {$ENDIF}
    property Style; // Must be published before Items
    property Anchors;
    property BiDiMode;
    property CharCase;
    property Color;
    property Constraints;
    {$IFDEF DCC}property Ctl3D;{$ENDIF}
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownCount;
    property Enabled;
    property Font;
    {$IFDEF DCC}property ImeMode;{$ENDIF}
    {$IFDEF DCC}property ImeName;{$ENDIF}
    property ItemHeight;
    property ItemIndex default -1;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    {$IFDEF DCC}property ParentCtl3D;{$ENDIF}
    {$IFDEF DCC}property ParentDoubleBuffered;{$ENDIF}
    {$IFDEF FPC}{$IFDEF HAS_WINCONTROL_WITH_PARENTDOUBLEBUFFERED}
    property ParentDoubleBuffered;
    {$ENDIF}{$ENDIF}
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    //property Text;
    {$IFDEF DCC}property TextHint;{$ENDIF}
    {$IFDEF DELPHI2010_OR_ABOVE}property Touch;{$ENDIF}
    property Visible;
    {$IFDEF DCC}{$IF RTLVersion > 23} property StyleElements; {$IFEND}{$ENDIF}
    property OnChange;
    property OnClick;
    property OnCloseUp;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    {$IFDEF DELPHI2010_OR_ABOVE}property OnGesture;{$ENDIF}
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnSelect;
    property OnStartDock;
    property OnStartDrag;
    property Items; // Must be published after OnMeasureItem

    property Selected;

    property BoundLabel;
    property BoundLabelPosition;
    property BoundLabelSpacing;

    property Appearance;
    property TagExt;
    property AnchoredControls;

    //property OnSelectChangeColorItem;
    //property OnColorChanged;
    property OnGetDisplayName;
    {$IFDEF FPC}
    property BorderSpacing;
    {$ENDIF}
  end;
  {$endregion TJppBrushStyleCombo}




function BrushStyleToStr(const bs: TBrushStyle): string;
function StrToBrushStyle(const BrushStyleName: string; Default: TBrushStyle = bsSolid): TBrushStyle;
function TryStrToBrushStyle(const BrushStyleName: string; var bs: TBrushStyle): Boolean;


implementation



{$region '       helpers         '}

function BrushStyleToStr(const bs: TBrushStyle): string;
begin
  case bs of
    bsSolid: Result := 'Solid';
    bsClear: Result := 'Clear';
    bsHorizontal: Result := 'Horizontal';
    bsVertical: Result := 'Vertical';
    bsFDiagonal: Result := 'FDiagonal';
    bsBDiagonal: Result := 'BDiagonal';
    bsCross: Result := 'Cross';
    bsDiagCross: Result := 'DiagCross';
  else
    Result := 'Solid';
  end;
end;

function TryStrToBrushStyle(const BrushStyleName: string; var bs: TBrushStyle): Boolean;
var
  s: string;
begin
  Result := False;
  s := BrushStyleName;

  s := TrimFromStart(s, 'bs');
  s := TrimUp(s);

  if s = 'SOLID' then bs := bsSolid
  else if s = 'CLEAR' then bs := bsClear
  else if s = 'HORIZONTAL' then bs := bsHorizontal
  else if s = 'VERTICAL' then bs := bsVertical
  else if s = 'FDIAGONAL' then bs := bsFDiagonal
  else if s = 'BDIAGONAL' then bs := bsBDiagonal
  else if s = 'CROSS' then bs := bsCross
  else if s = 'DIAGCROSS' then bs := bsDiagCross
  else Exit;

  Result := True;
end;


function StrToBrushStyle(const BrushStyleName: string; Default: TBrushStyle = bsSolid): TBrushStyle;
begin
  Result := Default;
  if not TryStrToBrushStyle(BrushStyleName, Result) then Result := Default;
end;

{$endregion helpers}




{$region ' ------------------------------------------------- TJppCustomBrushStyleComboBox ------------------------------------------------ '}


  {$region ' ------------------- TJppCustomBrushStyleComboBox Create / Destroy ----------------- '}
constructor TJppCustomBrushStyleComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Parent := TWinControl(AOwner);

  {$IFDEF FPC}Color := clWindow;{$ENDIF}

  FUpdateCounter := 0;

  FAppearance := TJppBrushStyleComboAppearance.Create(Self);
  FAppearance.OnChange := PropsChanged;

  FOnGetDisplayName := nil;

  FTagExt := TJppTagExt.Create(Self);
  FAnchoredControls := TJppAnchoredControls.Create(Self);

  Style := csOwnerDrawVariable;
  DropDownCount := 16;

  ItemIndex := 0;

  FBoundLabelPosition := lpLeft;
  FBoundLabelSpacing := 4;
  SetupInternalLabel;

  ItemHeight := 24;

  if csDesigning in ComponentState then RecreateItems;
  //SetSelectedBrushStyle(bsSolid);
  ItemIndex := 0;
end;

procedure TJppCustomBrushStyleComboBox.CreateWnd;
begin
  inherited CreateWnd;
end;

destructor TJppCustomBrushStyleComboBox.Destroy;
begin
  FAppearance.Free;
  FTagExt.Free;
  FAnchoredControls.Free;
  inherited;
end;


  {$endregion Create / Destroy}


procedure TJppCustomBrushStyleComboBox.Loaded;
begin
  inherited Loaded;
end;

procedure TJppCustomBrushStyleComboBox.SetAnchoredControls(const Value: TJppAnchoredControls);
begin
  FAnchoredControls := Value;
end;

procedure TJppCustomBrushStyleComboBox.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  SetBoundLabelPosition(FBoundLabelPosition);
  if not (csDestroying in ComponentState) then
    if Assigned(FAnchoredControls) then FAnchoredControls.UpdateAllControlsPos;
end;

procedure TJppCustomBrushStyleComboBox.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    if not (csDestroying in ComponentState) then
      if Assigned(FAnchoredControls) then
      begin
        if AComponent = FBoundLabel then FBoundLabel := nil
        else if AComponent = FAnchoredControls.Top.Control then FAnchoredControls.Top.Control := nil
        else if AComponent = FAnchoredControls.Bottom.Control then FAnchoredControls.Bottom.Control := nil
        else if AComponent = FAnchoredControls.Left.Control then FAnchoredControls.Left.Control := nil
        else if AComponent = FAnchoredControls.Right.Control then FAnchoredControls.Right.Control := nil;
      end;
end;

procedure TJppCustomBrushStyleComboBox.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);

  if FBoundLabel <> nil then
  begin
    FBoundLabel.Parent := AParent;
    FBoundLabel.Visible := True;
  end;

end;

procedure TJppCustomBrushStyleComboBox.BeginUpdate;
begin
  Inc(FUpdateCounter);
end;

procedure TJppCustomBrushStyleComboBox.EndUpdate(bCallOnChangeAfterUpdate, bResetUpdatingState: Boolean);
begin
  if bResetUpdatingState then FUpdateCounter := 0
  else
  begin
    Dec(FUpdateCounter);
    if FUpdateCounter < 0 then FUpdateCounter := 0;
  end;

  if (FUpdateCounter = 0) and bCallOnChangeAfterUpdate then PropsChanged(Self);
end;

function TJppCustomBrushStyleComboBox.UpdatingControl: Boolean;
begin
  Result := FUpdateCounter > 0;
end;

function TJppCustomBrushStyleComboBox.GetBrushStyleDisplayName(const bs: TBrushStyle): string;
begin
  case bs of
    bsSolid: Result := 'Solid';
    bsClear: Result := 'Clear';
    bsHorizontal: Result := 'Horizontal';
    bsVertical: Result := 'Vertical';
    bsFDiagonal: Result := 'Forward Diagonal';
    bsBDiagonal: Result := 'Backward Diagonal';
    bsCross: Result := 'Cross';
    bsDiagCross: Result := 'Diagonal Cross';
  else
    Result := 'Solid';
  end;
end;

function TJppCustomBrushStyleComboBox.GetBrushStyleIndex(const bs: TBrushStyle): integer;
var
  i: integer;
  bsItem: TBrushStyle;
begin
  Result := -1;
  bsItem := bsSolid;
  for i := 0 to Items.Count - 1 do
  begin
    if not TryGetItemBrushStyle(i, bsItem) then Continue;
    if bsItem = bs then
    begin
      Result := i;
      Break;
    end;
  end;
end;

function TJppCustomBrushStyleComboBox.BrushStyleExists(const bs: TBrushStyle): Boolean;
begin
  Result := GetBrushStyleIndex(bs) >= 0;
end;

function TJppCustomBrushStyleComboBox.IsValidIndex(const ItemIndex: integer): Boolean;
begin
  Result := (ItemIndex >= 0) and (ItemIndex < Items.Count);
end;


  {$Region ' ---------------- Change, Get & Set Selected ------------------- '}

procedure TJppCustomBrushStyleComboBox.Change;
var
  bs: TBrushStyle;
begin
  bs := bsSolid;
  if ItemIndex >= 0 then
  begin
    if not TryGetItemBrushStyle(ItemIndex, bs) then Exit;
    SetSelectedBrushStyle(bs);
  end;
  inherited Change;
end;

function TJppCustomBrushStyleComboBox.GetSelectedBrushStyle: TBrushStyle;
begin
  Result := bsSolid;
  if HandleAllocated then TryGetItemBrushStyle(ItemIndex, Result);
end;

procedure TJppCustomBrushStyleComboBox.SetSelectedBrushStyle(const Value: TBrushStyle);
var
  xInd: integer;
begin
  if FSelected = Value then Exit;
  FSelected := Value;

  xInd := GetBrushStyleIndex(FSelected);
  if xInd < 0 then Exit;
  ItemIndex := xInd;
end;
  {$endregion Change, Get & Set Selected}


procedure TJppCustomBrushStyleComboBox.PropsChanged(Sender: TObject);
begin
  if csLoading in ComponentState then Exit;
  Invalidate;
end;


procedure TJppCustomBrushStyleComboBox.RecreateItems;
begin
  Items.BeginUpdate;
  try
    AddBrushStyle(bsSolid);
    AddBrushStyle(bsHorizontal);
    AddBrushStyle(bsVertical);
    AddBrushStyle(bsFDiagonal);
    AddBrushStyle(bsBDiagonal);
    AddBrushStyle(bsCross);
    AddBrushStyle(bsDiagCross);
  finally
    Items.EndUpdate;
  end;
end;

procedure TJppCustomBrushStyleComboBox.SetAppearance(const Value: TJppBrushStyleComboAppearance);
begin
  FAppearance := Value;
  PropsChanged(Self);
end;

procedure TJppCustomBrushStyleComboBox.SetOnGetDisplayName(const Value: TJppBrushStyleComboBoxGetDisplayName);
begin
  FOnGetDisplayName := Value;
end;

procedure TJppCustomBrushStyleComboBox.SetTagExt(const Value: TJppTagExt);
begin
  FTagExt := Value;
end;

function TJppCustomBrushStyleComboBox.TryGetItemBrushStyle(const ItemIndex: integer; var BrushStyle: TBrushStyle): Boolean;
begin
  Result := False;
  if not IsValidIndex(ItemIndex) then Exit;
  Result := TryStrToBrushStyle(Items[ItemIndex], BrushStyle);
end;

function TJppCustomBrushStyleComboBox.AddBrushStyle(const bs: TBrushStyle): integer;
var
  x: integer;
  s: string;
begin
  x := GetBrushStyleIndex(bs);
  if x >= 0 then Exit(x);
  s := BrushStyleToStr(bs);
  Result := Items.Add(s);
end;


  {$region ' ------------------------------ TJppBrushStyleComboEx.DrawItem ---------------------------------------------- '}

procedure TJppCustomBrushStyleComboBox.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  bs: TBrushStyle;
  cl: TColor;
  bSelected, {bFocused,} bShowNames: Boolean;
  R: TRect;
  th, x, y: integer;
  sName: string;
begin

  if UpdatingControl then Exit;

  bSelected := odSelected in State;
  //bFocused := odSelected in State;
  bShowNames := FAppearance.ShowStyleNames;

  with Canvas do
  begin

    // Clear background
    cl := Color;
    if bSelected then cl := FAppearance.ColorSelectedBg;
    Brush.Color := cl;
    Pen.Style := psSolid;
    Pen.Color := cl;
    R := Rect;
    //InflateRect(R, 1, 1);

    Rectangle(R);


    // Brush style rect
    if not Enabled then cl := FAppearance.ColorDisabled
    else if bSelected then cl := FAppearance.ColorSelected
    else cl := FAppearance.Color;

    Brush.Color := cl;
    bs := bsSolid;
    if not TryGetItemBrushStyle(Index, bs) then bs := bsSolid;
    Brush.Style := bs;
    Pen.Style := psSolid;
    Pen.Color := cl;
    R := Rect;

    if bShowNames then R.Width := FAppearance.BrushStyleRectWidth;

    R.Left := R.Left + FAppearance.RectMargins.Left;
    R.Top := R.Top + FAppearance.RectMargins.Top;
    R.Width := R.Width - FAppearance.RectMargins.Right;
    R.Bottom := R.Bottom - FAppearance.RectMargins.Bottom;

    Rectangle(R);


    // Brush style name
    if bShowNames then
    begin
      Brush.Style := bsClear;
      Font.Assign(FAppearance.StyleNameFont);
      if bSelected then Font.Color := FAppearance.ColorSelectedText;
      if not Enabled then Font.Color := FAppearance.ColorDisabledText;
      sName := GetBrushStyleDisplayName(bs); // BrushStyleToStr(bs);
      if Assigned(FOnGetDisplayName) then FOnGetDisplayName(bs, sName);
      th := TextHeight(sName);
      x := R.Right + FAppearance.TextMargin;
      y := R.Top + (R.Height div 2) - (th div 2);
      TextOut(x, y, sName);
    end;

  end;


end;



{$endregion DrawItem}


  {$region ' --------- internal controls ------------- '}

procedure TJppCustomBrushStyleComboBox.SetupInternalLabel;
begin
  if Assigned(FBoundLabel) then Exit;
  FBoundLabel := TJppControlBoundLabel.Create(Self);
  FBoundLabel.FreeNotification(Self);
  FBoundLabel.OnAdjustBounds := AdjustLabelBounds;
  FBoundLabel.FocusControl := Self;
  {$IFDEF FPC}FBoundLabel.OnChangeBounds := AdjustLabelBounds;{$ENDIF}
end;

procedure TJppCustomBrushStyleComboBox.AdjustLabelBounds(Sender: TObject);
begin
  SetBoundLabelPosition(FBoundLabelPosition);
end;

procedure TJppCustomBrushStyleComboBox.SetBoundLabelPosition(const Value: TLabelPosition);
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

  FBoundLabel.SetBounds({%H-}P.x, {%H-}P.y, FBoundLabel.Width, FBoundLabel.Height);
end;

procedure TJppCustomBrushStyleComboBox.SetBoundLabelSpacing(const Value: Integer);
begin
  FBoundLabelSpacing := Value;
  SetBoundLabelPosition(FBoundLabelPosition);
end;

procedure TJppCustomBrushStyleComboBox.CMBiDiModeChanged(var Message: TMessage);
begin
  inherited;
  if FBoundLabel <> nil then FBoundLabel.BiDiMode := BiDiMode;
end;

procedure TJppCustomBrushStyleComboBox.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if FBoundLabel <> nil then FBoundLabel.Enabled := Enabled;
end;

procedure TJppCustomBrushStyleComboBox.CMVisibleChanged(var Message: TMessage);
begin
  inherited;
  if FBoundLabel <> nil then FBoundLabel.Visible := Visible;
end;

{$IFDEF DCC}
procedure TJppCustomBrushStyleComboBox.CNDrawItem(var Message: TWMDrawItem);
var
  State: TOwnerDrawState;
begin
  inherited;
  // XOR-owanie ju¿ narysowanego FocusRect w metodzie TCustomComboBox.CNDrawItem.
  // https://stackoverflow.com/questions/28649219/how-to-remove-the-dotted-focus-rectangle-from-tlistbox
  if FAppearance.DisableFocusRect then
  begin
    State := TOwnerDrawState(LoWord(Message.DrawItemStruct.itemState));
    if (odFocused in State) then DrawFocusRect(Message.DrawItemStruct.hDC, Message.DrawItemStruct.rcItem);
  end;
end;
{$ENDIF}


  {$endregion internal controls}



{$endregion TJppBrushStyleCombo}






{$Region ' -----------------------------  TJppBrushStyleComboAppearance ---------------------- '}

constructor TJppBrushStyleComboAppearance.Create(AOwner: TComponent);
begin
  inherited Create;
  FOwner := AOwner;
  FColor := clBlack;
  FColorDisabled := clGray;
  FColorSelected := clHighlightText;
  FColorSelectedBg := clHighlight;
  FShowStyleNames := True;
  FStyleNameFont := TFont.Create;
  FStyleNameFont.OnChange := PropsChanged;
  FBrushStyleRectWidth := 50;
  FColorSelectedText := clHighlightText;
  FColorDisabledText := clGrayText;
  FRectMargins := TJppMargins.Create(FOwner);
  FRectMargins.OnChange := PropsChanged;
  FTextMargin := 8;
  {$IFDEF DCC}FDisableFocusRect := False;{$ENDIF}
end;

destructor TJppBrushStyleComboAppearance.Destroy;
begin
  FStyleNameFont.Free;
  FRectMargins.Free;
  inherited;
end;

procedure TJppBrushStyleComboAppearance.Assign(app: TJppBrushStyleComboAppearance);
begin
  BeginUpdate;
  try
    FColor := app.Color;
    FColorDisabled := app.ColorDisabled;
    FColorSelected := app.ColorSelected;
    FColorSelectedBg := app.ColorSelectedBg;
    FShowStyleNames := app.ShowStyleNames;
    FStyleNameFont.Assign(app.StyleNameFont);
    FBrushStyleRectWidth := app.BrushStyleRectWidth;
    FColorSelectedText := app.ColorSelectedText;
    FColorDisabledText := app.ColorDisabledText;
    FRectMargins.Assign(app.RectMargins);
    FTextMargin := app.TextMargin;
    {$IFDEF DCC}FDisableFocusRect := app.DisableFocusRect;{$ENDIF}
  finally
    EndUpdate;
  end;
end;

procedure TJppBrushStyleComboAppearance.SetBrushStyleRectWidth(const Value: integer);
begin
  if FBrushStyleRectWidth = Value then Exit;
  FBrushStyleRectWidth := Value;
  if FShowStyleNames then PropsChanged(Self);
end;

procedure TJppBrushStyleComboAppearance.SetColor(const Value: TColor);
begin
  if FColor = Value then Exit;
  FColor := Value;
  PropsChanged(Self);
end;

procedure TJppBrushStyleComboAppearance.SetColorDisabled(const Value: TColor);
begin
  if FColorDisabled = Value then Exit;
  FColorDisabled := Value;
  PropsChanged(Self);
end;

procedure TJppBrushStyleComboAppearance.SetColorDisabledText(const Value: TColor);
begin
  if FColorDisabledText = Value then Exit;
  FColorDisabledText := Value;
  PropsChanged(Self);
end;

procedure TJppBrushStyleComboAppearance.SetColorSelected(const Value: TColor);
begin
  if FColorSelected = Value then Exit;
  FColorSelected := Value;
  PropsChanged(Self);
end;

procedure TJppBrushStyleComboAppearance.SetColorSelectedBg(const Value: TColor);
begin
  if FColorSelectedBg = Value then Exit;
  FColorSelectedBg := Value;
  PropsChanged(Self);
end;

procedure TJppBrushStyleComboAppearance.SetColorSelectedText(const Value: TColor);
begin
  if FColorSelectedText = Value then Exit;
  FColorSelectedText := Value;
  PropsChanged(Self);
end;

{$IFDEF DCC}
procedure TJppBrushStyleComboAppearance.SetDisableFocusRect(const Value: Boolean);
begin
  if FDisableFocusRect = Value then Exit;
  FDisableFocusRect := Value;
  PropsChanged(Self);
end;
{$ENDIF}

procedure TJppBrushStyleComboAppearance.SetRectMargins(const Value: TJppMargins);
begin
  FRectMargins := Value;
  FRectMargins.OnChange := PropsChanged;
end;

procedure TJppBrushStyleComboAppearance.SetShowStyleNames(const Value: Boolean);
begin
  if FShowStyleNames = Value then Exit;
  FShowStyleNames := Value;
  PropsChanged(Self);
end;

procedure TJppBrushStyleComboAppearance.SetStyleNameFont(const Value: TFont);
begin
  if Assigned(FStyleNameFont) and Assigned(Value) then
  begin
    FStyleNameFont.Assign(Value);
    PropsChanged(Self);
  end;
end;

procedure TJppBrushStyleComboAppearance.SetTextMargin(const Value: integer);
begin
  if FTextMargin = Value then Exit;
  FTextMargin := Value;
  PropsChanged(Self);
end;




{$endregion TJppBrushStyleComboAppearance}



end.

