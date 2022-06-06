unit JPP.PenStyleComboBox;

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
  Messages,
  SysUtils, Classes, Types, {$IFDEF DCC}{$IFDEF HAS_SYSTEM_UITYPES}System.UITypes,{$ENDIF}{$ENDIF}
  Controls, StdCtrls, Graphics, Dialogs, Buttons, Clipbrd, ExtCtrls,
  {$IFDEF FPC}LCLType, LCLIntf, LMessages,{$ENDIF}
  JPL.Strings, JPL.Conversion, JPL.Rects,
  JPP.Common, JPP.Common.Procs, JPP.AnchoredControls, JPP.Graphics;


type

  TJppPenStyleComboBoxGetDisplayName = procedure(PenStyle: TPenStyle; var PenStyleName: string) of object;

  {$region ' --- TJppPenStyleComboAppearance --- '}
  TJppPenStyleComboAppearance = class(TJppPersistent)
  private
    FOwner: TComponent;
    FColor: TColor;
    FColorDisabled: TColor;
    FColorSelectedBg: TColor;
    FShowStyleNames: Boolean;
    FStyleNameFont: TFont;
    FColorSelectedText: TColor;
    FColorDisabledText: TColor;
    FTextMargin: integer;
    FColorSelected: TColor;
    {$IFDEF DCC}FDisableFocusRect: Boolean;{$ENDIF}
    FPenWidth: integer;
    FLineWidth: integer;
    FLeftMargin: integer;
    procedure SetColor(const Value: TColor);
    procedure SetColorDisabled(const Value: TColor);
    procedure SetColorSelectedBg(const Value: TColor);
    procedure SetShowStyleNames(const Value: Boolean);
    procedure SetStyleNameFont(const Value: TFont);
    procedure SetColorSelectedText(const Value: TColor);
    procedure SetColorDisabledText(const Value: TColor);
    procedure SetTextMargin(const Value: integer);
    procedure SetColorSelected(const Value: TColor);
    {$IFDEF DCC}procedure SetDisableFocusRect(const Value: Boolean);{$ENDIF}
    procedure SetPenWidth(const Value: integer);
    procedure SetLineWidth(const Value: integer);
    procedure SetLeftMargin(const Value: integer);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(app: TJppPenStyleComboAppearance); reintroduce;
  published
    property PenWidth: integer read FPenWidth write SetPenWidth default 1;
    property LineWidth: integer read FLineWidth write SetLineWidth default 80; // Used when ShowStyleNames = True
    property LeftMargin: integer read FLeftMargin write SetLeftMargin default 4;
    property Color: TColor read FColor write SetColor default clBlack;
    property ColorDisabled: TColor read FColorDisabled write SetColorDisabled default clGray;
    property ColorSelected: TColor read FColorSelected write SetColorSelected default clHighlightText;
    property ColorSelectedBg: TColor read FColorSelectedBg write SetColorSelectedBg default clHighlight;
    property ShowStyleNames: Boolean read FShowStyleNames write SetShowStyleNames default True;
    property StyleNameFont: TFont read FStyleNameFont write SetStyleNameFont;
    property ColorSelectedText: TColor read FColorSelectedText write SetColorSelectedText default clHighlightText;
    property ColorDisabledText: TColor read FColorDisabledText write SetColorDisabledText default clGrayText;
    property TextMargin: integer read FTextMargin write SetTextMargin default 8;
    {$IFDEF DCC}property DisableFocusRect: Boolean read FDisableFocusRect write SetDisableFocusRect default False;{$ENDIF}
  end;
  {$endregion TJppPenStyleComboAppearance}


  {$region ' ----------- TJppCustomPenStyleComboBox ---------- '}
  TJppCustomPenStyleComboBox = class(TCustomComboBox)
  private
    FUpdateCounter: integer;
    FBoundLabel: TJppControlBoundLabel;
    FBoundLabelPosition: TLabelPosition;
    FBoundLabelSpacing: Integer;
    FTagExt: TJppTagExt;
    FAppearance: TJppPenStyleComboAppearance;
    FSelected: TPenStyle;
    FOnGetDisplayName: TJppPenStyleComboBoxGetDisplayName;
    FAnchoredControls: TJppAnchoredControls;
    procedure SetBoundLabelPosition(const Value: TLabelPosition);
    procedure SetBoundLabelSpacing(const Value: Integer);
    procedure AdjustLabelBounds(Sender: TObject);
    procedure SetTagExt(const Value: TJppTagExt);
    procedure SetAppearance(const Value: TJppPenStyleComboAppearance);
    procedure SetOnGetDisplayName(const Value: TJppPenStyleComboBoxGetDisplayName);
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
    function PenStyleExists(const ps: TPenStyle): Boolean;
    function GePenStyleIndex(const ps: TPenStyle): integer;
    function GePenStyleDisplayName(const ps: TPenStyle): string;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateWnd; override;


    function TryGetItemPenStyle(const ItemIndex: integer; var PenStyle: TPenStyle): Boolean;
    function AddPenStyle(const ps: TPenStyle): integer; // Returns the index of the added item


    procedure BeginUpdate;
    procedure EndUpdate(bCallOnChangeAfterUpdate: Boolean = True; bResetUpdatingState: Boolean = False);
    function UpdatingControl: Boolean;

    ////////////////////////////////////////////////////////////////////////////////////////
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    ////////////////////////////////////////////////////////////////////////////////////////


    function GetSelectedPenStyle: TPenStyle;
    procedure SetSelectedPenStyle(const Value: TPenStyle);

    procedure Change; override;
    procedure SetupInternalLabel;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer; AHeight: Integer); override;

    property Selected: TPenStyle read GetSelectedPenStyle write SetSelectedPenStyle;

    property BoundLabel: TJppControlBoundLabel read FBoundLabel;
    property BoundLabelPosition: TLabelPosition read FBoundLabelPosition write SetBoundLabelPosition default lpLeft;
    property BoundLabelSpacing: Integer read FBoundLabelSpacing write SetBoundLabelSpacing default 4;

    property Appearance: TJppPenStyleComboAppearance read FAppearance write SetAppearance;
    property TagExt: TJppTagExt read FTagExt write SetTagExt;
    property AnchoredControls: TJppAnchoredControls read FAnchoredControls write SetAnchoredControls;

    property OnGetDisplayName: TJppPenStyleComboBoxGetDisplayName read FOnGetDisplayName write SetOnGetDisplayName;

  end;
  {$endregion TJppCustomPenStyleComboBox}


  {$Region ' ---------- TJppPenStyleComboBox ---------- '}
  TJppPenStyleComboBox = class(TJppCustomPenStyleComboBox)
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
    {$IFDEF DCC}property TextHint;{$ENDIF}
    {$IFDEF DELPHI2010_OR_ABOVE}property Touch;{$ENDIF}
    property Visible;
    {$IFDEF HAS_STYLE_ELEMENTS} property StyleElements; {$ENDIF}
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

    property OnGetDisplayName;
    {$IFDEF FPC}
    property BorderSpacing;
    {$ENDIF}
  end;
  {$endregion TJppPenStyleCombo}




function PenStyleToStr(const ps: TPenStyle): string;
function StrToPenStyle(const PenStyleName: string; Default: TPenStyle = psSolid): TPenStyle;
function TryStrToPenStyle(const PenStyleName: string; var ps: TPenStyle): Boolean;


implementation



{$region '       helpers         '}

function PenStyleToStr(const ps: TPenStyle): string;
begin
  case ps of
    psSolid: Result := 'Solid';
    psDash: Result := 'Dash';
    psDot: Result := 'Dot';
    psDashDot: Result := 'DashDot';
    psDashDotDot: Result := 'DashDotDot';
    psClear: Result := 'Clear';
    psInsideFrame: Result := 'InsideFrame';
    {$IFDEF DCC}
    psUserStyle: Result := 'UserStyle';
    psAlternate: Result := 'Alternate';
    {$ELSE}
    psPattern: Result := 'Pattern';
    {$ENDIF}
  else
    Result := 'Solid'{%H-};
  end;
end;

function TryStrToPenStyle(const PenStyleName: string; var ps: TPenStyle): Boolean;
var
  s: string;
begin
  Result := False;
  s := PenStyleName;

  s := TrimFromStart(s, 'ps');
  s := TrimUp(s);

  if s = 'SOLID' then ps := psSolid
  else if s = 'DASH' then ps := psDash
  else if s = 'DOT' then ps := psDot
  else if s = 'DASHDOT' then ps := psDashDot
  else if s = 'DASHDOTDOT' then ps := psDashDotDot
  else if s = 'CLEAR' then ps := psClear
  else if s = 'INSIDEFRAME' then ps := psInsideFrame
  {$IFDEF DCC}
  else if s = 'USERSTYLE' then ps := psUserStyle
  else if s = 'ALTERNATE' then ps := psAlternate
  {$ELSE}
  else if s = 'PATTERN' then ps := psPattern
  {$ENDIF}
  else Exit;

  Result := True;
end;


function StrToPenStyle(const PenStyleName: string; Default: TPenStyle = psSolid): TPenStyle;
begin
  if not TryStrToPenStyle(PenStyleName, Result) then Result := Default;
end;

{$endregion helpers}




{$region ' ------------------------------------------------- TJppCustomPenStyleComboBox ------------------------------------------------ '}


  {$region ' ------------------- TJppCustomPenStyleComboBox Create / Destroy ----------------- '}
constructor TJppCustomPenStyleComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Parent := TWinControl(AOwner);

  {$IFDEF FPC}Color := clWindow;{$ENDIF}

  FUpdateCounter := 0;

  FAppearance := TJppPenStyleComboAppearance.Create(Self);
  FAppearance.OnChange := PropsChanged;

  FOnGetDisplayName := nil;

  FTagExt := TJppTagExt.Create(Self);
  FAnchoredControls := TJppAnchoredControls.Create(Self);

  Style := csOwnerDrawVariable;
  DropDownCount := 16;

  FBoundLabelPosition := lpLeft;
  FBoundLabelSpacing := 4;
  SetupInternalLabel;

  //ItemHeight := 24;

  if csDesigning in ComponentState then RecreateItems;
  ItemIndex := 0;
end;

procedure TJppCustomPenStyleComboBox.CreateWnd;
begin
  inherited CreateWnd;
end;

destructor TJppCustomPenStyleComboBox.Destroy;
begin
  FAppearance.Free;
  FTagExt.Free;
  FAnchoredControls.Free;
  inherited;
end;

  {$endregion Create / Destroy}


procedure TJppCustomPenStyleComboBox.Loaded;
begin
  inherited Loaded;
end;

procedure TJppCustomPenStyleComboBox.SetAnchoredControls(const Value: TJppAnchoredControls);
begin
  FAnchoredControls := Value;
end;

procedure TJppCustomPenStyleComboBox.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  SetBoundLabelPosition(FBoundLabelPosition);
  if not (csDestroying in ComponentState) then
    if Assigned(FAnchoredControls) then FAnchoredControls.UpdateAllControlsPos;
end;

procedure TJppCustomPenStyleComboBox.Notification(AComponent: TComponent; Operation: TOperation);
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

procedure TJppCustomPenStyleComboBox.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);

  if FBoundLabel <> nil then
  begin
    FBoundLabel.Parent := AParent;
    FBoundLabel.Visible := True;
  end;

end;

procedure TJppCustomPenStyleComboBox.BeginUpdate;
begin
  Inc(FUpdateCounter);
end;

procedure TJppCustomPenStyleComboBox.EndUpdate(bCallOnChangeAfterUpdate, bResetUpdatingState: Boolean);
begin
  if bResetUpdatingState then FUpdateCounter := 0
  else
  begin
    Dec(FUpdateCounter);
    if FUpdateCounter < 0 then FUpdateCounter := 0;
  end;

  if (FUpdateCounter = 0) and bCallOnChangeAfterUpdate then PropsChanged(Self);
end;

function TJppCustomPenStyleComboBox.UpdatingControl: Boolean;
begin
  Result := FUpdateCounter > 0;
end;


function TJppCustomPenStyleComboBox.GePenStyleDisplayName(const ps: TPenStyle): string;
begin
  case ps of
    psSolid: Result := 'Solid';
    psDash: Result := 'Dash';
    psDot: Result := 'Dot';
    psDashDot: Result := 'Dash Dot';
    psDashDotDot: Result := 'Dash Dot Dot';
    psClear: Result := 'Clear';
    psInsideFrame: Result := 'Inside Frame';
    {$IFDEF DCC}
    psUserStyle: Result := 'User Style';
    psAlternate: Result := 'Alternate';
    {$ELSE}
    psPattern: Result := 'Pattern';
    {$ENDIF}
  else
    Result := 'Solid'{%H-};
  end;
end;

function TJppCustomPenStyleComboBox.GePenStyleIndex(const ps: TPenStyle): integer;
var
  i: integer;
  bsItem: TPenStyle;
begin
  Result := -1;
  for i := 0 to Items.Count - 1 do
  begin
    if not TryGetItemPenStyle(i, bsItem) then Continue;
    if bsItem = ps then
    begin
      Result := i;
      Break;
    end;
  end;
end;

function TJppCustomPenStyleComboBox.PenStyleExists(const ps: TPenStyle): Boolean;
begin
  Result := GePenStyleIndex(ps) >= 0;
end;

function TJppCustomPenStyleComboBox.IsValidIndex(const ItemIndex: integer): Boolean;
begin
  Result := (ItemIndex >= 0) and (ItemIndex < Items.Count);
end;


  {$Region ' ---------------- Change, Get & Set Selected ------------------- '}

procedure TJppCustomPenStyleComboBox.Change;
var
  bs: TPenStyle;
begin
  if ItemIndex >= 0 then
  begin
    if not TryGetItemPenStyle(ItemIndex, bs) then Exit;
    SetSelectedPenStyle(bs);
  end;
  inherited Change;
end;

function TJppCustomPenStyleComboBox.GetSelectedPenStyle: TPenStyle;
begin
  Result := psSolid;
  if HandleAllocated then TryGetItemPenStyle(ItemIndex, Result);
end;

procedure TJppCustomPenStyleComboBox.SetSelectedPenStyle(const Value: TPenStyle);
var
  xInd: integer;
begin
  if FSelected = Value then Exit;
  FSelected := Value;

  xInd := GePenStyleIndex(FSelected);
  if xInd < 0 then Exit;
  ItemIndex := xInd;
end;
  {$endregion Change, Get & Set Selected}


procedure TJppCustomPenStyleComboBox.PropsChanged(Sender: TObject);
begin
  if csLoading in ComponentState then Exit;
  Invalidate;
end;


procedure TJppCustomPenStyleComboBox.RecreateItems;
begin
  Items.BeginUpdate;
  try
    AddPenStyle(psSolid);
    AddPenStyle(psDash);
    AddPenStyle(psDot);
    AddPenStyle(psDashDot);
    AddPenStyle(psDashDotDot);
  finally
    Items.EndUpdate;
  end;
end;

procedure TJppCustomPenStyleComboBox.SetAppearance(const Value: TJppPenStyleComboAppearance);
begin
  FAppearance := Value;
  PropsChanged(Self);
end;

procedure TJppCustomPenStyleComboBox.SetOnGetDisplayName(const Value: TJppPenStyleComboBoxGetDisplayName);
begin
  FOnGetDisplayName := Value;
end;

procedure TJppCustomPenStyleComboBox.SetTagExt(const Value: TJppTagExt);
begin
  FTagExt := Value;
end;

function TJppCustomPenStyleComboBox.TryGetItemPenStyle(const ItemIndex: integer; var PenStyle: TPenStyle): Boolean;
begin
  Result := False;
  if not IsValidIndex(ItemIndex) then Exit;
  Result := TryStrToPenStyle(Items[ItemIndex], PenStyle);
end;

function TJppCustomPenStyleComboBox.AddPenStyle(const ps: TPenStyle): integer;
var
  x: integer;
  s: string;
begin
  x := GePenStyleIndex(ps);
  if x >= 0 then Exit(x);
  s := PenStyleToStr(ps);
  Result := Items.Add(s);
end;


  {$region ' ------------------------------ TJppCustomPenStyleComboBox.DrawItem ---------------------------------------------- '}

procedure TJppCustomPenStyleComboBox.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  ps: TPenStyle;
  cl: TColor;
  bSelected, bShowNames: Boolean;
  R: TRect;
  th, x, y: integer;
  sName: string;
  P1, P2: TPoint;
begin

  if UpdatingControl then Exit;

  bSelected := odSelected in State;
  bShowNames := FAppearance.ShowStyleNames;

  with Canvas do
  begin

    // Clear background
    cl := Color;
    if bSelected then cl := FAppearance.ColorSelectedBg;
    Brush.Color := cl;
    Brush.Style := bsSolid;
    Pen.Style := psSolid;
    Pen.Color := cl;
    R := Rect;

    Rectangle(R);


    // Pen style rect
    if not Enabled then cl := FAppearance.ColorDisabled
    else if bSelected then cl := FAppearance.ColorSelected
    else cl := FAppearance.Color;

    Brush.Style := bsClear;
    if not TryGetItemPenStyle(Index, ps) then ps := psSolid;
    Pen.Style := ps;
    Pen.Color := cl;
    Pen.Width := FAppearance.PenWidth;
    R := Rect;

    if bShowNames then R.Width := FAppearance.LineWidth;

    R.Left := R.Left + FAppearance.LeftMargin;

    P1.X := R.Left; // + FAppearance.LeftMargin;
    P1.Y := R.Top + (R.Height div 2) - (FAppearance.PenWidth div 2);

    P2.X := R.Right;
    P2.Y := P1.Y;

    MoveTo(P1.X, P1.Y);
    LineTo(P2.X, P2.Y);



    // Pen style name
    if bShowNames then
    begin
      Brush.Style := bsClear;
      Font.Assign(FAppearance.StyleNameFont);
      if bSelected then Font.Color := FAppearance.ColorSelectedText;
      if not Enabled then Font.Color := FAppearance.ColorDisabledText;
      sName := GePenStyleDisplayName(ps);
      if Assigned(FOnGetDisplayName) then FOnGetDisplayName(ps, sName);
      th := TextHeight(sName);
      x := R.Right + FAppearance.TextMargin;
      y := R.Top + (R.Height div 2) - (th div 2);
      TextOut(x, y, sName);
    end;

  end;


end;



{$endregion DrawItem}


  {$region ' --------- internal controls ------------- '}

procedure TJppCustomPenStyleComboBox.SetupInternalLabel;
begin
  if Assigned(FBoundLabel) then Exit;
  FBoundLabel := TJppControlBoundLabel.Create(Self);
  FBoundLabel.FreeNotification(Self);
  FBoundLabel.OnAdjustBounds := AdjustLabelBounds;
  FBoundLabel.FocusControl := Self;
  {$IFDEF FPC}FBoundLabel.OnChangeBounds := AdjustLabelBounds;{$ENDIF}
end;

procedure TJppCustomPenStyleComboBox.AdjustLabelBounds(Sender: TObject);
begin
  SetBoundLabelPosition(FBoundLabelPosition);
end;

procedure TJppCustomPenStyleComboBox.SetBoundLabelPosition(const Value: TLabelPosition);
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

procedure TJppCustomPenStyleComboBox.SetBoundLabelSpacing(const Value: Integer);
begin
  FBoundLabelSpacing := Value;
  SetBoundLabelPosition(FBoundLabelPosition);
end;

procedure TJppCustomPenStyleComboBox.CMBiDiModeChanged(var Message: TMessage);
begin
  inherited;
  if FBoundLabel <> nil then FBoundLabel.BiDiMode := BiDiMode;
end;

procedure TJppCustomPenStyleComboBox.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if FBoundLabel <> nil then FBoundLabel.Enabled := Enabled;
end;

procedure TJppCustomPenStyleComboBox.CMVisibleChanged(var Message: TMessage);
begin
  inherited;
  if FBoundLabel <> nil then FBoundLabel.Visible := Visible;
end;

{$IFDEF DCC}
procedure TJppCustomPenStyleComboBox.CNDrawItem(var Message: TWMDrawItem);
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



{$endregion TJppCustomPenStyleComboBox}






{$Region ' -----------------------------  TJppPenStyleComboAppearance ---------------------- '}

constructor TJppPenStyleComboAppearance.Create(AOwner: TComponent);
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
  FColorSelectedText := clHighlightText;
  FColorDisabledText := clGrayText;
  FTextMargin := 8;
  FPenWidth := 1;
  FLineWidth := 80;
  FLeftMargin := 4;
  {$IFDEF DCC}FDisableFocusRect := False;{$ENDIF}
end;

destructor TJppPenStyleComboAppearance.Destroy;
begin
  FStyleNameFont.Free;
  inherited;
end;

procedure TJppPenStyleComboAppearance.Assign(app: TJppPenStyleComboAppearance);
begin
  BeginUpdate;
  try
    FColor := app.Color;
    FColorDisabled := app.ColorDisabled;
    FColorSelected := app.ColorSelected;
    FColorSelectedBg := app.ColorSelectedBg;
    FShowStyleNames := app.ShowStyleNames;
    FStyleNameFont.Assign(app.StyleNameFont);
    FColorSelectedText := app.ColorSelectedText;
    FColorDisabledText := app.ColorDisabledText;
    FTextMargin := app.TextMargin;
    FPenWidth := app.PenWidth;
    FLineWidth := app.LineWidth;
    FLeftMargin := app.LeftMargin;
    {$IFDEF DCC}FDisableFocusRect := app.DisableFocusRect;{$ENDIF}
  finally
    EndUpdate;
  end;
end;


procedure TJppPenStyleComboAppearance.SetColor(const Value: TColor);
begin
  if FColor = Value then Exit;
  FColor := Value;
  PropsChanged(Self);
end;

procedure TJppPenStyleComboAppearance.SetColorDisabled(const Value: TColor);
begin
  if FColorDisabled = Value then Exit;
  FColorDisabled := Value;
  PropsChanged(Self);
end;

procedure TJppPenStyleComboAppearance.SetColorDisabledText(const Value: TColor);
begin
  if FColorDisabledText = Value then Exit;
  FColorDisabledText := Value;
  PropsChanged(Self);
end;

procedure TJppPenStyleComboAppearance.SetColorSelected(const Value: TColor);
begin
  if FColorSelected = Value then Exit;
  FColorSelected := Value;
  PropsChanged(Self);
end;

procedure TJppPenStyleComboAppearance.SetColorSelectedBg(const Value: TColor);
begin
  if FColorSelectedBg = Value then Exit;
  FColorSelectedBg := Value;
  PropsChanged(Self);
end;

procedure TJppPenStyleComboAppearance.SetColorSelectedText(const Value: TColor);
begin
  if FColorSelectedText = Value then Exit;
  FColorSelectedText := Value;
  PropsChanged(Self);
end;

{$IFDEF DCC}
procedure TJppPenStyleComboAppearance.SetDisableFocusRect(const Value: Boolean);
begin
  if FDisableFocusRect = Value then Exit;
  FDisableFocusRect := Value;
  PropsChanged(Self);
end;
{$ENDIF}

procedure TJppPenStyleComboAppearance.SetLeftMargin(const Value: integer);
begin
  if FLeftMargin = Value then Exit;
  FLeftMargin := Value;
  PropsChanged(Self);
end;

procedure TJppPenStyleComboAppearance.SetLineWidth(const Value: integer);
begin
  if FLineWidth = Value then Exit;
  FLineWidth := Value;
  PropsChanged(Self);
end;

procedure TJppPenStyleComboAppearance.SetPenWidth(const Value: integer);
begin
  if FPenWidth = Value then Exit;
  FPenWidth := Value;
  PropsChanged(Self);
end;


procedure TJppPenStyleComboAppearance.SetShowStyleNames(const Value: Boolean);
begin
  if FShowStyleNames = Value then Exit;
  FShowStyleNames := Value;
  PropsChanged(Self);
end;

procedure TJppPenStyleComboAppearance.SetStyleNameFont(const Value: TFont);
begin
  FStyleNameFont := Value;
  PropsChanged(Self);
end;

procedure TJppPenStyleComboAppearance.SetTextMargin(const Value: integer);
begin
  if FTextMargin = Value then Exit;
  FTextMargin := Value;
  PropsChanged(Self);
end;




{$endregion TJppPenStyleComboAppearance}



end.

