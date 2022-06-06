unit JPP.GPHatchStyleComboBox;

{
  Jacek Pazera
  http://www.pazera-software.com
  https://github.com/jackdp

  TJppGPHatchStyleComboBox - combo box with hatch styles
  GDI+ control (Windows only).

  Last mods:
    2020.01.22
    2020.04.10 - Anchored controls
}


{$I jpp.inc}
{$IFDEF FPC} {$mode delphi} {$ENDIF}

interface

{$IFDEF MSWINDOWS}
uses
  Windows,
  {$IFDEF DCC}
  Messages,
  System.SysUtils, System.Classes, System.Types, System.UITypes,
  Vcl.Controls, Vcl.StdCtrls, Vcl.Graphics, Vcl.Dialogs, Vcl.Buttons, Vcl.Clipbrd, Vcl.ExtCtrls,
  {$ELSE}
  SysUtils, Classes, Types, Controls, StdCtrls, Graphics, Dialogs, Buttons, Clipbrd, ExtCtrls, LCLType, LCLIntf, Messages, LMessages,
  {$IFDEF DCC}{$IFDEF HAS_SYSTEM_UITYPES}System.UITypes,{$ENDIF}{$ENDIF}
  {$ENDIF}

  IGDIPlus, IGDIPlusHelpers,
  JPP.Common, JPP.AnchoredControls;


type

  TJppGPHatchStyleComboBoxGetDisplayName = procedure(HatchStyle: TIGPHatchStyle; var HatchStyleName: string) of object;

  {$region ' ----------- TJppGPHatchColors ---------- '}
  TJppGPHatchColors = class(TJppPersistent)
  private
    FOwner: TComponent;
    FForeColor: TColor;
    FForeColorAlpha: Byte;
    FBackColor: TColor;
    FBackColorAlpha: Byte;
    FFrameColor: TColor;
    FFrameColorAlpha: Byte;
    procedure SetForeColor(const Value: TColor);
    procedure SetForeColorAlpha(const Value: Byte);
    procedure SetBackColor(const Value: TColor);
    procedure SetBackColorAlpha(const Value: Byte);
    function GetForeAlphaColor: TAlphaColor;
    procedure SetForeAlphaColor(const Value: TAlphaColor);
    function GetBackAlphaColor: TAlphaColor;
    procedure SetBackAlphaColor(const Value: TAlphaColor);
    procedure SetFrameColor(const Value: TColor);
    procedure SetFrameColorAlpha(const Value: Byte);
    function GetFrameAlphaColor: TAlphaColor;
    procedure SetFrameAlphaColor(const Value: TAlphaColor);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;

    procedure Assign(Colors: TJppGPHatchColors); reintroduce;

  protected
    property ForeAlphaColor: TAlphaColor read GetForeAlphaColor write SetForeAlphaColor;
    property BackAlphaColor: TAlphaColor read GetBackAlphaColor write SetBackAlphaColor;
    property FrameAlphaColor: TAlphaColor read GetFrameAlphaColor write SetFrameAlphaColor;
  published
    property ForeColor: TColor read FForeColor write SetForeColor default clBlack;
    property ForeColorAlpha: Byte read FForeColorAlpha write SetForeColorAlpha default 255;
    property BackColor: TColor read FBackColor write SetBackColor default clWhite;
    property BackColorAlpha: Byte read FBackColorAlpha write SetBackColorAlpha default 255;
    property FrameColor: TColor read FFrameColor write SetFrameColor default clBlack;
    property FrameColorAlpha: Byte read FFrameColorAlpha write SetFrameColorAlpha default 255;
  end;
  {$endregion TJppGPHatchColors}


  {$region ' ------------ TJppGPHatchStyleComboAppearance ----------- '}
  TJppGPHatchStyleComboAppearance = class(TJppPersistent)
  private
    FOwner: TComponent;
    FBackColor: TColor;
    FDisabledBackColor: TColor;
    FSelectedBackColor: TColor;
    FShowStyleNames: Boolean;
    FStyleNameFont: TFont;
    FHatchStyleRectWidth: integer;
    FSelectedTextColor: TColor;
    FDisabledTextColor: TColor;
    FRectMargins: TJppMargins;
    FTextMargin: integer;
    {$IFDEF DCC}FDisableFocusRect: Boolean;{$ENDIF}
    FHatchColors: TJppGPHatchColors;
    FSelectedHatchColors: TJppGPHatchColors;
    FDisabledHatchColors: TJppGPHatchColors;
    procedure SetBackColor(const Value: TColor);
    procedure SetDisabledBackColor(const Value: TColor);
    procedure SetSelectedBackColor(const Value: TColor);
    procedure SetShowStyleNames(const Value: Boolean);
    procedure SetStyleNameFont(const Value: TFont);
    procedure SetHatchStyleRectWidth(const Value: integer);
    procedure SetSelectedTextColor(const Value: TColor);
    procedure SetDisabledTextColor(const Value: TColor);
    procedure SetRectMargins(const Value: TJppMargins);
    procedure SetTextMargin(const Value: integer);
    {$IFDEF DCC}procedure SetDisableFocusRect(const Value: Boolean);{$ENDIF}
    procedure SetHatchColors(const Value: TJppGPHatchColors);
    procedure SetSelectedHatchColors(const Value: TJppGPHatchColors);
    procedure SetDisabledHatchColors(const Value: TJppGPHatchColors);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(app: TJppGPHatchStyleComboAppearance); reintroduce;
  published

    property HatchColors: TJppGPHatchColors read FHatchColors write SetHatchColors;
    property SelectedHatchColors: TJppGPHatchColors read FSelectedHatchColors write SetSelectedHatchColors;
    property DisabledHatchColors: TJppGPHatchColors read FDisabledHatchColors write SetDisabledHatchColors;

    property BackColor: TColor read FBackColor write SetBackColor default clWhite;
    property DisabledBackColor: TColor read FDisabledBackColor write SetDisabledBackColor default clWhite;
    property SelectedBackColor: TColor read FSelectedBackColor write SetSelectedBackColor default clHighlightText;

    property ShowStyleNames: Boolean read FShowStyleNames write SetShowStyleNames default True;
    property StyleNameFont: TFont read FStyleNameFont write SetStyleNameFont;
    property HatchStyleRectWidth: integer read FHatchStyleRectWidth write SetHatchStyleRectWidth default 50;

    property SelectedTextColor: TColor read FSelectedTextColor write SetSelectedTextColor default clHighlightText;
    property DisabledTextColor: TColor read FDisabledTextColor write SetDisabledTextColor default clGrayText;

    property RectMargins: TJppMargins read FRectMargins write SetRectMargins;
    property TextMargin: integer read FTextMargin write SetTextMargin default 8;



    {$IFDEF DCC}property DisableFocusRect: Boolean read FDisableFocusRect write SetDisableFocusRect default False;{$ENDIF}
  end;
  {$endregion TJppGPHatchStyleComboAppearance}


  {$region ' ----------- TJppCustomGPHatchStyleComboBox ---------- '}
  TJppCustomGPHatchStyleComboBox = class(TCustomComboBox)
  private
    FUpdateCounter: integer;
    FBoundLabel: TJppControlBoundLabel;
    FBoundLabelPosition: TLabelPosition;
    FBoundLabelSpacing: Integer;
    FTagExt: TJppTagExt;
    FAppearance: TJppGPHatchStyleComboAppearance;
    FOnGetDisplayName: TJppGPHatchStyleComboBoxGetDisplayName;
    FSelectedHatch: TIGPHatchStyle;
    FAnchoredControls: TJppAnchoredControls;
    procedure SetBoundLabelPosition(const Value: TLabelPosition);
    procedure SetBoundLabelSpacing(const Value: Integer);
    procedure AdjustLabelBounds(Sender: TObject);
    procedure SetTagExt(const Value: TJppTagExt);
    procedure SetAppearance(const Value: TJppGPHatchStyleComboAppearance);
    procedure SetOnGetDisplayName(const Value: TJppGPHatchStyleComboBoxGetDisplayName);
    procedure SetSelectedHatchStyle(const Value: TIGPHatchStyle);
    function GetSelectedHatchStyle: TIGPHatchStyle;
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

    function HatchStyleExists(const hs: TIGPHatchStyle): Boolean;
    function GetHatchStyleIndex(const hs: TIGPHatchStyle): integer;
    function GetHatchStyleDisplayName(const hs: TIGPHatchStyle): string;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateWnd; override;

    function TryGetItemHatchStyle(const Index: integer; var HatchStyle: TIGPHatchStyle): Boolean;
    function AddHatchStyle(const hs: TIGPHatchStyle): integer; // Returns the index of the added item
    function AddHatchStyles(const Arr: TGPHatchStyleDynArray): integer; // Resturns then number of the added items


    procedure BeginUpdate;
    procedure EndUpdate(bCallOnChangeAfterUpdate: Boolean = True; bResetUpdatingState: Boolean = False);
    function UpdatingControl: Boolean;

    ////////////////////////////////////////////////////////////////////////////////////////
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    ////////////////////////////////////////////////////////////////////////////////////////


    procedure Change; override;
    procedure SetupInternalLabel;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer; AHeight: Integer); override;


    property BoundLabel: TJppControlBoundLabel read FBoundLabel;
    property BoundLabelPosition: TLabelPosition read FBoundLabelPosition write SetBoundLabelPosition default lpLeft;
    property BoundLabelSpacing: Integer read FBoundLabelSpacing write SetBoundLabelSpacing default 4;

    property Appearance: TJppGPHatchStyleComboAppearance read FAppearance write SetAppearance;
    property TagExt: TJppTagExt read FTagExt write SetTagExt;

    property OnGetDisplayName: TJppGPHatchStyleComboBoxGetDisplayName read FOnGetDisplayName write SetOnGetDisplayName;
    property SelectedHatchStyle: TIGPHatchStyle read GetSelectedHatchStyle write SetSelectedHatchStyle;

    property AnchoredControls: TJppAnchoredControls read FAnchoredControls write SetAnchoredControls;

  end;
  {$endregion TJppCustomGPHatchStyleComboBox}


  {$Region ' ---------- TJppGPHatchStyleComboBox ---------- '}
  TJppGPHatchStyleComboBox = class(TJppCustomGPHatchStyleComboBox)
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
    property Style default csOwnerDrawVariable; // Must be published before Items
    property Anchors;
    property BiDiMode;
    property CharCase;
    //property Color; // Use Appearance.BackColor instead
    property Constraints;
    property Width default 230;
    {$IFDEF DCC}property Ctl3D;{$ENDIF}
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownCount default 16;
    property Enabled;
    property Font;
    {$IFDEF DCC}property ImeMode;{$ENDIF}
    {$IFDEF DCC}property ImeName;{$ENDIF}
    property ItemHeight default 24;
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
    {$IFDEF DCC}property Touch;{$ENDIF}
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
    {$IFDEF DCC}property OnGesture;{$ENDIF}
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

    property SelectedHatchStyle;

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
  {$endregion TJppGPHatchStyleCombo}


{$ENDIF} // MSWINDOWS


implementation


{$IFDEF MSWINDOWS}


{$region ' ------------------------------------------------- TJppCustomGPHatchStyleComboBox ------------------------------------------------ '}


  {$region ' ------------------- TJppCustomGPHatchStyleComboBox Create / Destroy ----------------- '}
constructor TJppCustomGPHatchStyleComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Parent := TWinControl(AOwner);

  {$IFDEF FPC}Color := clWindow;{$ENDIF}

  FUpdateCounter := 0;

  FAppearance := TJppGPHatchStyleComboAppearance.Create(Self);
  FAppearance.OnChange := PropsChanged;

  FOnGetDisplayName := nil;

  FTagExt := TJppTagExt.Create(Self);
  FAnchoredControls := TJppAnchoredControls.Create(Self);

  Style := csOwnerDrawVariable;
  DropDownCount := 16;

  FBoundLabelPosition := lpLeft;
  FBoundLabelSpacing := 4;
  SetupInternalLabel;

  ItemHeight := 24;
  Width := 230;

  if csDesigning in ComponentState then
  begin
    RecreateItems;
    SetSelectedHatchStyle(HatchStyleHorizontal);
    ItemIndex := 0;
  end;

end;

procedure TJppCustomGPHatchStyleComboBox.CreateWnd;
begin
  inherited CreateWnd;
end;

destructor TJppCustomGPHatchStyleComboBox.Destroy;
begin
  FAppearance.Free;
  FTagExt.Free;
  FAnchoredControls.Free;
  inherited;
end;

  {$endregion Create / Destroy}


procedure TJppCustomGPHatchStyleComboBox.Loaded;
begin
  inherited Loaded;
end;

procedure TJppCustomGPHatchStyleComboBox.SetAnchoredControls(const Value: TJppAnchoredControls);
begin
  FAnchoredControls := Value;
end;

procedure TJppCustomGPHatchStyleComboBox.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  SetBoundLabelPosition(FBoundLabelPosition);
  if not (csDestroying in ComponentState) then
    if Assigned(FAnchoredControls) then FAnchoredControls.UpdateAllControlsPos;
end;

procedure TJppCustomGPHatchStyleComboBox.Notification(AComponent: TComponent; Operation: TOperation);
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

procedure TJppCustomGPHatchStyleComboBox.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);

  if FBoundLabel <> nil then
  begin
    FBoundLabel.Parent := AParent;
    FBoundLabel.Visible := True;
  end;
end;

procedure TJppCustomGPHatchStyleComboBox.BeginUpdate;
begin
  Inc(FUpdateCounter);
end;

procedure TJppCustomGPHatchStyleComboBox.EndUpdate(bCallOnChangeAfterUpdate, bResetUpdatingState: Boolean);
begin
  if bResetUpdatingState then FUpdateCounter := 0
  else
  begin
    Dec(FUpdateCounter);
    if FUpdateCounter < 0 then FUpdateCounter := 0;
  end;

  if (FUpdateCounter = 0) and bCallOnChangeAfterUpdate then PropsChanged(Self);
end;

function TJppCustomGPHatchStyleComboBox.UpdatingControl: Boolean;
begin
  Result := FUpdateCounter > 0;
end;


function TJppCustomGPHatchStyleComboBox.GetHatchStyleDisplayName(const hs: TIGPHatchStyle): string;
begin
  Result := GPHatchStyleToDisplayName(hs);
end;

function TJppCustomGPHatchStyleComboBox.GetHatchStyleIndex(const hs: TIGPHatchStyle): integer;
var
  i: integer;
  hsItem: TIGPHatchStyle;
begin
  Result := -1;
  hsItem := HatchStyleHorizontal;
  for i := 0 to Items.Count - 1 do
  begin
    if not TryGetItemHatchStyle(i, hsItem) then Continue;
    if hsItem = hs then
    begin
      Result := i;
      Break;
    end;
  end;
end;

function TJppCustomGPHatchStyleComboBox.HatchStyleExists(const hs: TIGPHatchStyle): Boolean;
begin
  Result := GetHatchStyleIndex(hs) >= 0;
end;

function TJppCustomGPHatchStyleComboBox.IsValidIndex(const ItemIndex: integer): Boolean;
begin
  Result := (ItemIndex >= 0) and (ItemIndex < Items.Count);
end;


  {$Region ' ---------------- Change, Get & Set Selected ------------------- '}

procedure TJppCustomGPHatchStyleComboBox.Change;
var
  hs: TIGPHatchStyle;
begin
  if ItemIndex >= 0 then
  begin
    hs := HatchStyleHorizontal;
    if not TryGetItemHatchStyle(ItemIndex, hs) then Exit;
    SetSelectedHatchStyle(hs);
  end;
  inherited Change;
end;


function TJppCustomGPHatchStyleComboBox.GetSelectedHatchStyle: TIGPHatchStyle;
begin
  Result := HatchStyleHorizontal;
  if HandleAllocated then TryGetItemHatchStyle(ItemIndex, Result);
end;


procedure TJppCustomGPHatchStyleComboBox.SetSelectedHatchStyle(const Value: TIGPHatchStyle);
var
  xInd: integer;
begin
  if FSelectedHatch = Value then Exit;
  FSelectedHatch := Value;

  xInd := GetHatchStyleIndex(FSelectedHatch);
  if xInd < 0 then Exit;
  ItemIndex := xInd;
end;

{$endregion Change, Get & Set Selected}


procedure TJppCustomGPHatchStyleComboBox.PropsChanged(Sender: TObject);
begin
  if csLoading in ComponentState then Exit;
  Invalidate;
end;

procedure TJppCustomGPHatchStyleComboBox.RecreateItems;
begin
  AddHatchStyles(GPGetAllHatchStylesArray);
end;

procedure TJppCustomGPHatchStyleComboBox.SetAppearance(const Value: TJppGPHatchStyleComboAppearance);
begin
  FAppearance := Value;
  PropsChanged(Self);
end;

procedure TJppCustomGPHatchStyleComboBox.SetOnGetDisplayName(const Value: TJppGPHatchStyleComboBoxGetDisplayName);
begin
  FOnGetDisplayName := Value;
end;

procedure TJppCustomGPHatchStyleComboBox.SetTagExt(const Value: TJppTagExt);
begin
  FTagExt := Value;
end;


function TJppCustomGPHatchStyleComboBox.TryGetItemHatchStyle(const Index: integer; var HatchStyle: TIGPHatchStyle): Boolean;
begin
  Result := False;
  if not IsValidIndex(Index) then Exit;
  Result := GPTryStrIDToHatchStyle(Items[Index], HatchStyle);
end;

function TJppCustomGPHatchStyleComboBox.AddHatchStyle(const hs: TIGPHatchStyle): integer;
var
  x: integer;
  s: string;
begin
  x := GetHatchStyleIndex(hs);
  if x >= 0 then Exit(x);
  s := GPHatchStyleToStrID(hs);
  Result := Items.Add(s);
end;

function TJppCustomGPHatchStyleComboBox.AddHatchStyles(const Arr: TGPHatchStyleDynArray): integer;
var
  hs: TIGPHatchStyle;
  i, xCount: integer;
begin
  xCount := 0;
  Items.BeginUpdate;
  try
    for i := 0 to High(Arr) do
    begin
      hs := Arr[i];
      if GetHatchStyleIndex(hs) >= 0 then Continue;
      AddHatchStyle(hs);
      Inc(xCount);
    end;
  finally
    Items.EndUpdate;
  end;
  Result := xCount;
end;


  {$region ' ------------------------------ TJppGPHatchStyleComboEx.DrawItem ---------------------------------------------- '}

procedure TJppCustomGPHatchStyleComboBox.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  cl: TColor;
  bSelected, {bFocused,} bShowNames: Boolean;
  sName: string;

  StrID: string;
  gr: IGPGraphics;
  Pen: IGPPen;
  Brush: IGPBrush;
  HatchRect: TIGPRectF;
  HatchStyle: TIGPHatchStyle;
  AFont: IGPFont;

  clHatchForeColor, clHatchBackColor: TAlphaColor;
  clPen: TAlphaColor;
  clText: TAlphaColor;
  clFrame: TAlphaColor;
  Point: TPointF;
  FontName: string;
  FontSize: Single;
begin

  if UpdatingControl then Exit;

  StrID := Items[Index];
  HatchStyle := HatchStyleHorizontal;
  if not GPTryStrIDToHatchStyle(StrID, HatchStyle) then Exit;

  bSelected := odSelected in State;
  //bFocused := odFocused in State;
  bShowNames := FAppearance.ShowStyleNames;


  gr := TIGPGraphics.Create(Canvas.Handle);



  // ------------ Clear background -------------
  if not Enabled then cl := FAppearance.DisabledBackColor
  else if bSelected then cl := FAppearance.SelectedBackColor
  else cl := FAppearance.BackColor;
  Brush := TIGPSolidBrush.Create(GPColor(cl));
  gr.FillRectangle(Brush, TIGPRect.Create(Rect));



  // --------------- HatchStyle rect ---------------
  Brush := nil;

  if not Enabled then
  begin
    clHatchForeColor := FAppearance.DisabledHatchColors.ForeAlphaColor;
    clHatchBackColor := FAppearance.DisabledHatchColors.BackAlphaColor;
    clFrame := FAppearance.DisabledHatchColors.FrameAlphaColor;
  end
  else if bSelected then
  begin
    clHatchForeColor := FAppearance.SelectedHatchColors.ForeAlphaColor;
    clHatchBackColor := FAppearance.SelectedHatchColors.BackAlphaColor;
    clFrame := FAppearance.SelectedHatchColors.FrameAlphaColor;
  end
  else
  begin
    clHatchForeColor := FAppearance.HatchColors.ForeAlphaColor;
    clHatchBackColor := FAppearance.HatchColors.BackAlphaColor;
    clFrame := FAppearance.HatchColors.FrameAlphaColor;
  end;

  Brush := TIGPHatchBrush.Create(HatchStyle, clHatchForeColor, clHatchBackColor);


  HatchRect := TIGPRectF.Create(Rect);
  if bShowNames then HatchRect.Width := FAppearance.HatchStyleRectWidth;

  HatchRect.X := HatchRect.X + FAppearance.RectMargins.Left;
  HatchRect.Y := HatchRect.Y + FAppearance.RectMargins.Top;
  HatchRect.Width := HatchRect.Width - FAppearance.RectMargins.Right - FAppearance.RectMargins.Left;
  HatchRect.Height := HatchRect.Height - FAppearance.RectMargins.Bottom - FAppearance.RectMargins.Top;

  gr.FillRectangleF(Brush, HatchRect);


  // ------------ Frame ---------------
  clPen := clFrame;
  Pen := TIGPPen.Create(clPen);
  gr.DrawRectangleF(Pen, HatchRect);



  // ---------------------- Text -----------------------
  if bShowNames then
  begin
    Brush := nil;

    if not Enabled then clText := GPColor(FAppearance.DisabledTextColor, 255)
    else if bSelected then clText := GPColor(FAppearance.SelectedTextColor, 255)
    else clText := GPColor(FAppearance.StyleNameFont.Color, 255);

    Brush := TIGPSolidBrush.Create(clText); // Font color = Brush color


    {$IFDEF DCC}
    FontName := FAppearance.StyleNameFont.Name;
    FontSize := FAppearance.StyleNameFont.Size;
    {$ELSE}
    FontName := GetFontData(FAppearance.StyleNameFont.Handle).Name;
    FontSize := Appearance.StyleNameFont.Size;
    if FontSize <= 0 then FontSize := GetFontData(Appearance.StyleNameFont.Handle).Height;
    {$ENDIF}
    if FontSize <= 0 then FontSize := 8; // tak na wszelki wypadek

    try
      AFont := TIGPFont.Create(WideString(FontName), FontSize, FAppearance.StyleNameFont.Style, UnitPoint);
    except
      AFont := TIGPFont.Create('Arial', FontSize, FAppearance.StyleNameFont.Style, UnitPoint);
    end;

    sName := GetHatchStyleDisplayName(HatchStyle);
    if Assigned(FOnGetDisplayName) then FOnGetDisplayName(HatchStyle, sName);

    Point.X := HatchRect.X + HatchRect.Width + FAppearance.TextMargin;
    Point.Y := Rect.Top + (Rect.Height / 2) - (GPTextHeightF(gr, sName, AFont) / 2);
    gr.DrawStringF(WideString(sName), AFont, Point, Brush);

  end;


end;



{$endregion DrawItem}


  {$region ' --------- internal controls ------------- '}

procedure TJppCustomGPHatchStyleComboBox.SetupInternalLabel;
begin
  if Assigned(FBoundLabel) then Exit;
  FBoundLabel := TJppControlBoundLabel.Create(Self);
  FBoundLabel.FreeNotification(Self);
  FBoundLabel.OnAdjustBounds := AdjustLabelBounds;
  FBoundLabel.FocusControl := Self;
  {$IFDEF FPC}FBoundLabel.OnChangeBounds := AdjustLabelBounds;{$ENDIF}
end;



procedure TJppCustomGPHatchStyleComboBox.AdjustLabelBounds(Sender: TObject);
begin
  SetBoundLabelPosition(FBoundLabelPosition);
end;

procedure TJppCustomGPHatchStyleComboBox.SetBoundLabelPosition(const Value: TLabelPosition);
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

procedure TJppCustomGPHatchStyleComboBox.SetBoundLabelSpacing(const Value: Integer);
begin
  FBoundLabelSpacing := Value;
  SetBoundLabelPosition(FBoundLabelPosition);
end;

procedure TJppCustomGPHatchStyleComboBox.CMBiDiModeChanged(var Message: TMessage);
begin
  inherited;
  if FBoundLabel <> nil then FBoundLabel.BiDiMode := BiDiMode;
end;

procedure TJppCustomGPHatchStyleComboBox.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if FBoundLabel <> nil then FBoundLabel.Enabled := Enabled;
end;

procedure TJppCustomGPHatchStyleComboBox.CMVisibleChanged(var Message: TMessage);
begin
  inherited;
  if FBoundLabel <> nil then FBoundLabel.Visible := Visible;
end;

{$IFDEF DCC}
procedure TJppCustomGPHatchStyleComboBox.CNDrawItem(var Message: TWMDrawItem);
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



{$endregion TJppGPHatchStyleCombo}






{$Region ' -----------------------------  TJppGPHatchStyleComboAppearance ---------------------- '}

constructor TJppGPHatchStyleComboAppearance.Create(AOwner: TComponent);
begin
  inherited Create;
  FOwner := AOwner;
  FBackColor := clWhite;
  FDisabledBackColor := clWhite;
  FSelectedBackColor := clHighlight;
  FShowStyleNames := True;
  FStyleNameFont := TFont.Create;
  FStyleNameFont.OnChange := PropsChanged;
  FHatchStyleRectWidth := 50;
  FSelectedTextColor := clHighlightText;
  FDisabledTextColor := clGrayText;

  FRectMargins := TJppMargins.Create(FOwner);
//  FRectMargins.Left := 2;
//  FRectMargins.Top := 2;
//  FRectMargins.Bottom := 2;
  FRectMargins.OnChange := PropsChanged;

  FTextMargin := 8;

  FHatchColors := TJppGPHatchColors.Create(FOwner);
  FHatchColors.OnChange := PropsChanged;

  FSelectedHatchColors := TJppGPHatchColors.Create(FOwner);
  FSelectedHatchColors.OnChange := PropsChanged;

  FDisabledHatchColors := TJppGPHatchColors.Create(FOwner);
  FDisabledHatchColors.ForeColor := clGray;
  FDisabledHatchColors.FrameColor := clGray;
  FDisabledHatchColors.OnChange := PropsChanged;
  {$IFDEF DCC}FDisableFocusRect := False;{$ENDIF}
end;

destructor TJppGPHatchStyleComboAppearance.Destroy;
begin
  FStyleNameFont.Free;
  FRectMargins.Free;
  FHatchColors.Free;
  FSelectedHatchColors.Free;
  FDisabledHatchColors.Free;
  inherited;
end;

procedure TJppGPHatchStyleComboAppearance.Assign(app: TJppGPHatchStyleComboAppearance);
begin
  BeginUpdate;
  try
    FBackColor := app.BackColor;
    FDisabledBackColor := app.DisabledBackColor;
    FSelectedBackColor := app.SelectedBackColor;
    FShowStyleNames := app.ShowStyleNames;
    FStyleNameFont.Assign(app.StyleNameFont);
    FHatchStyleRectWidth := app.HatchStyleRectWidth;
    FSelectedTextColor := app.SelectedTextColor;
    FDisabledTextColor := app.DisabledTextColor;
    FRectMargins.Assign(app.RectMargins);
    FTextMargin := app.TextMargin;
    {$IFDEF DCC}FDisableFocusRect := app.DisableFocusRect;{$ENDIF}
    FHatchColors.Assign(app.HatchColors);
    FSelectedHatchColors.Assign(app.SelectedHatchColors);
    FDisabledHatchColors.Assign(app.DisabledHatchColors);
  finally
    EndUpdate;
  end;
end;

procedure TJppGPHatchStyleComboAppearance.SetHatchColors(const Value: TJppGPHatchColors);
begin
  FHatchColors := Value;
  PropsChanged(Self);
end;

procedure TJppGPHatchStyleComboAppearance.SetSelectedHatchColors(const Value: TJppGPHatchColors);
begin
  FSelectedHatchColors := Value;
  PropsChanged(Self);
end;

procedure TJppGPHatchStyleComboAppearance.SetHatchStyleRectWidth(const Value: integer);
begin
  if FHatchStyleRectWidth = Value then Exit;
  FHatchStyleRectWidth := Value;
  if FShowStyleNames then PropsChanged(Self);
end;

procedure TJppGPHatchStyleComboAppearance.SetBackColor(const Value: TColor);
begin
  if FBackColor = Value then Exit;
  FBackColor := Value;
  PropsChanged(Self);
end;

procedure TJppGPHatchStyleComboAppearance.SetDisabledBackColor(const Value: TColor);
begin
  if FDisabledBackColor = Value then Exit;
  FDisabledBackColor := Value;
  PropsChanged(Self);
end;

procedure TJppGPHatchStyleComboAppearance.SetDisabledHatchColors(const Value: TJppGPHatchColors);
begin
  FDisabledHatchColors := Value;
  PropsChanged(Self);
end;

procedure TJppGPHatchStyleComboAppearance.SetDisabledTextColor(const Value: TColor);
begin
  if FDisabledTextColor = Value then Exit;
  FDisabledTextColor := Value;
  PropsChanged(Self);
end;

procedure TJppGPHatchStyleComboAppearance.SetSelectedBackColor(const Value: TColor);
begin
  if FSelectedBackColor = Value then Exit;
  FSelectedBackColor := Value;
  PropsChanged(Self);
end;

procedure TJppGPHatchStyleComboAppearance.SetSelectedTextColor(const Value: TColor);
begin
  if FSelectedTextColor = Value then Exit;
  FSelectedTextColor := Value;
  PropsChanged(Self);
end;

{$IFDEF DCC}
procedure TJppGPHatchStyleComboAppearance.SetDisableFocusRect(const Value: Boolean);
begin
  if FDisableFocusRect = Value then Exit;
  FDisableFocusRect := Value;
  PropsChanged(Self);
end;
{$ENDIF}

procedure TJppGPHatchStyleComboAppearance.SetRectMargins(const Value: TJppMargins);
begin
  FRectMargins := Value;
  FRectMargins.OnChange := PropsChanged;
end;

procedure TJppGPHatchStyleComboAppearance.SetShowStyleNames(const Value: Boolean);
begin
  if FShowStyleNames = Value then Exit;
  FShowStyleNames := Value;
  PropsChanged(Self);
end;

procedure TJppGPHatchStyleComboAppearance.SetStyleNameFont(const Value: TFont);
begin
  FStyleNameFont := Value;
  PropsChanged(Self);
end;

procedure TJppGPHatchStyleComboAppearance.SetTextMargin(const Value: integer);
begin
  if FTextMargin = Value then Exit;
  FTextMargin := Value;
  PropsChanged(Self);
end;




{$endregion TJppGPHatchStyleComboAppearance}



{$region '                       TJppGPHatchColors                          '}

constructor TJppGPHatchColors.Create(AOwner: TComponent);
begin
  inherited Create;
  FOwner := AOwner;
  FForeColor := clBlack;
  FForeColorAlpha := 255;
  FBackColor := clWhite;
  FBackColorAlpha := 255;
  FFrameColor := clBlack;
  FFrameColorAlpha := 255;
end;

destructor TJppGPHatchColors.Destroy;
begin
  inherited;
end;

procedure TJppGPHatchColors.Assign(Colors: TJppGPHatchColors);
begin
  FForeColor := Colors.ForeColor;
  FForeColorAlpha := Colors.ForeColorAlpha;
  FBackColor := Colors.BackColor;
  FBackColorAlpha := Colors.BackColorAlpha;
  FFrameColor := Colors.FrameColor;
  FFrameColorAlpha := Colors.FrameColorAlpha;
end;


procedure TJppGPHatchColors.SetBackColor(const Value: TColor);
begin
  if FBackColor = Value then Exit;
  FBackColor := Value;
  PropsChanged(Self);
end;

procedure TJppGPHatchColors.SetBackColorAlpha(const Value: Byte);
begin
  if FBackColorAlpha = Value then Exit;
  FBackColorAlpha := Value;
  PropsChanged(Self);
end;

function TJppGPHatchColors.GetBackAlphaColor: TAlphaColor;
begin
  Result := GPColor(FBackColor, FBackColorAlpha);
end;

procedure TJppGPHatchColors.SetBackAlphaColor(const Value: TAlphaColor);
begin
  FBackColor := GPGetColor(Value);
  FBackColorAlpha := GetAlpha(Value);
  PropsChanged(Self);
end;


procedure TJppGPHatchColors.SetForeColor(const Value: TColor);
begin
  if FForeColor = Value then Exit;
  FForeColor := Value;
  PropsChanged(Self);
end;

procedure TJppGPHatchColors.SetForeColorAlpha(const Value: Byte);
begin
  if FForeColorAlpha = Value then Exit;
  FForeColorAlpha := Value;
  PropsChanged(Self);
end;

function TJppGPHatchColors.GetForeAlphaColor: TAlphaColor;
begin
  Result := GPColor(FForeColor, FForeColorAlpha);
end;

procedure TJppGPHatchColors.SetForeAlphaColor(const Value: TAlphaColor);
begin
  FForeColor := GPGetColor(Value);
  FForeColorAlpha := GetAlpha(Value);
  PropsChanged(Self);
end;

procedure TJppGPHatchColors.SetFrameColor(const Value: TColor);
begin
  if FFrameColor = Value then Exit;
  FFrameColor := Value;
  PropsChanged(Self);
end;

procedure TJppGPHatchColors.SetFrameColorAlpha(const Value: Byte);
begin
  if FFrameColorAlpha = Value then Exit;
  FFrameColorAlpha := Value;
  PropsChanged(Self);
end;

function TJppGPHatchColors.GetFrameAlphaColor: TAlphaColor;
begin
  Result := GPColor(FFrameColor, FFrameColorAlpha);
end;

procedure TJppGPHatchColors.SetFrameAlphaColor(const Value: TAlphaColor);
begin
  FFrameColor := GPGetColor(Value);
  FFrameColorAlpha := GetAlpha(Value);
  PropsChanged(Self);
end;

{$endregion TJppGPHatchColors}


{$ENDIF} // MSWINDOWS

end.

