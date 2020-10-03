unit JPP.FlatComboBox;

{
  Jacek Pazera
  http://www.pazera-software.com
  https://github.com/jackdp

  Based on the TFlatComboBox from https://sourceforge.net/projects/flatstyle/
}


{$I jpp.inc}
{$IFDEF FPC} {$mode delphi} {$ENDIF}

interface


uses
  Windows, Messages, Classes, Forms, Controls, Graphics, StdCtrls, ExtCtrls,
  SysUtils, ShellApi, CommCtrl, ComCtrls, Consts, Types,
  JPP.Common, JPP.Common.Procs, JPP.AnchoredControls, JPP.Flash;

type

  {$Region '   TJppFlatComboBoxAppearance   '}
  TJppFlatComboBoxAppearance = class(TJppPersistent)
  private
    FOwner: TComponent;
    FNormalBgColor: TColor;
    FFocusedBgColor: TColor;
    FNormalTextColor: TColor;
    FFocusedTextColor: TColor;
    FHotBgColor: TColor;
    FHotTextColor: TColor;
    FDisabledBgColor: TColor;
    FDisabledTextColor: TColor;
    FNormalBorderColor: TColor;
    FFocusedBorderColor: TColor;
    FHotBorderColor: TColor;
    FDisabledBorderColor: TColor;
    FNormalButtonBgColor: TColor;
    FNormalButtonArrowColor: TColor;
    FHotButtonBgColor: TColor;
    FHotButtonArrowColor: TColor;
    FFocusedButtonBgColor: TColor;
    FFocusedButtonArrowColor: TColor;
    FDisabledButtonBgColor: TColor;
    FDisabledButtonArrowColor: TColor;
    FExpandedComboBgColor: TColor;
    procedure SetNormalBgColor(const Value: TColor);
    procedure SetFocusedBgColor(const Value: TColor);
    procedure SetNormalTextColor(const Value: TColor);
    procedure SetFocusedTextColor(const Value: TColor);
    procedure SetHotBgColor(const Value: TColor);
    procedure SetHotTextColor(const Value: TColor);
    procedure SetDisabledBgColor(const Value: TColor);
    procedure SetDisabledTextColor(const Value: TColor);
    procedure SetNormalBorderColor(const Value: TColor);
    procedure SetFocusedBorderColor(const Value: TColor);
    procedure SetHotBorderColor(const Value: TColor);
    procedure SetDisabledBorderColor(const Value: TColor);
    procedure SetNormalButtonBgColor(const Value: TColor);
    procedure SetNormalButtonArrowColor(const Value: TColor);
    procedure SetHotButtonBgColor(const Value: TColor);
    procedure SetHotButtonArrowColor(const Value: TColor);
    procedure SetFocusedButtonBgColor(const Value: TColor);
    procedure SetFocusedButtonArrowColor(const Value: TColor);
    procedure SetDisabledButtonBgColor(const Value: TColor);
    procedure SetDisabledButtonArrowColor(const Value: TColor);
    procedure SetExpandedComboBgColor(const Value: TColor);
  protected
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(Source: TJppFlatComboBoxAppearance); reintroduce;
  published
    property ExpandedComboBgColor: TColor read FExpandedComboBgColor write SetExpandedComboBgColor default clWindow;

    property NormalBgColor: TColor read FNormalBgColor write SetNormalBgColor default clWindow;
    property NormalTextColor: TColor read FNormalTextColor write SetNormalTextColor default clWindowText;
    property NormalBorderColor: TColor read FNormalBorderColor write SetNormalBorderColor default $007A7A7A;
    property NormalButtonBgColor: TColor read FNormalButtonBgColor write SetNormalButtonBgColor default clBtnFace;
    property NormalButtonArrowColor: TColor read FNormalButtonArrowColor write SetNormalButtonArrowColor default clBlack;

    property FocusedBgColor: TColor read FFocusedBgColor write SetFocusedBgColor default clWindow;
    property FocusedTextColor: TColor read FFocusedTextColor write SetFocusedTextColor default clWindowText;
    property FocusedBorderColor: TColor read FFocusedBorderColor write SetFocusedBorderColor default $00434343;
    property FocusedButtonBgColor: TColor read FFocusedButtonBgColor write SetFocusedButtonBgColor default $00E2E2E2;
    property FocusedButtonArrowColor: TColor read FFocusedButtonArrowColor write SetFocusedButtonArrowColor default clBlack;

    property HotBgColor: TColor read FHotBgColor write SetHotBgColor default clWindow;
    property HotTextColor: TColor read FHotTextColor write SetHotTextColor default clWindowText;
    property HotBorderColor: TColor read FHotBorderColor write SetHotBorderColor default $00434343;
    property HotButtonBgColor: TColor read FHotButtonBgColor write SetHotButtonBgColor default $00E2E2E2;
    property HotButtonArrowColor: TColor read FHotButtonArrowColor write SetHotButtonArrowColor default clBlack;

    property DisabledBgColor: TColor read FDisabledBgColor write SetDisabledBgColor default $00F3F3F3;
    property DisabledTextColor: TColor read FDisabledTextColor write SetDisabledTextColor default clGrayText; // TODO: disabled text color not working!
    property DisabledBorderColor: TColor read FDisabledBorderColor write SetDisabledBorderColor default $00CAC8C8;
    property DisabledButtonBgColor: TColor read FDisabledButtonBgColor write SetDisabledButtonBgColor default $00F3F3F3;
    property DisabledButtonArrowColor: TColor read FDisabledButtonArrowColor write SetDisabledButtonArrowColor default clGray;
  end;
  {$endregion TJppFlatComboBoxAppearance}


  {$Region '   TJppCustomFlatComboBox   '}
  TJppCustomFlatComboBox = class(TCustomComboBox)
  type
    TColorRec = record
      Background: TColor;
      Border: TColor;
      Button: TColor;
      Arrow: TColor;
      Text: TColor;
    end;
  private
    FBoundLabel: TJppControlBoundLabel;
    FShowLabel: Boolean;
    FBoundLabelSpacing: Integer;
    FBoundLabelPosition: TLabelPosition;
    FAnchoredControls: TJppAnchoredControls;
    FButtonWidth: Integer;
    FChildHandle: HWND;
    FDefListProc: Pointer;
    FListHandle: HWND;
    FListInstance: Pointer;
    FSysBtnWidth: Integer;
    FSolidBorder: Boolean;
    FMouseOverControl: Boolean;
    FAppearance: TJppFlatComboBoxAppearance;
    FButtonColor: TColor;
    FArrowColor: TColor;
    FTagExt: TJppTagExt;
    function GetButtonRect: TRect;
    procedure PaintButton;
    procedure PaintBorder;
    procedure RedrawBorders;
    procedure InvalidateSelection;
    function GetSolidBorder: Boolean;
    procedure SetSolidBorder;
    procedure ListWndProc(var Message: TMessage);
    procedure WMSetFocus(var Message: TMessage); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TMessage); message WM_KILLFOCUS;
    procedure WMKeyDown(var Message: TMessage); message WM_KEYDOWN;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMNCPaint(var Message: TMessage); message WM_NCPAINT;
    procedure CMEnabledChanged(var Msg: TMessage); message CM_ENABLEDCHANGED;
    procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;
    procedure CMBiDiModeChanged(var Message: TMessage); message CM_BIDIMODECHANGED;
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMSysColorChange(var Message: TMessage); message CM_SYSCOLORCHANGE;
    procedure CMParentColorChanged(var Message: TWMNoParams); message CM_PARENTCOLORCHANGED;

    procedure CMMouseEnter (var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave (var Message: TMessage); message CM_MOUSELEAVE;
    procedure SetAppearance(const Value: TJppFlatComboBoxAppearance);
    procedure GetColors(var ColorRec: TColorRec);
    procedure SetButtonWidth(const Value: integer);
    procedure SetTagExt(const Value: TJppTagExt);
    procedure SetShowLabel(const Value: Boolean);
    procedure SetBoundLabelPosition(const Value: TLabelPosition);
    procedure SetBoundLabelSpacing(const Value: Integer);
    procedure AdjustLabelBounds(Sender: TObject);
    procedure SetAnchoredControls(const Value: TJppAnchoredControls);
  protected
    procedure SetParent(AParent: TWinControl); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure PropsChanged(Sender: TObject);
    procedure WndProc(var Message: TMessage); override;
    procedure ComboWndProc(var Message: TMessage; ComboWnd: HWND; ComboProc: Pointer); override;
    property SolidBorder: Boolean read FSolidBorder;
    procedure Loaded; override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure ApplyAppearance;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetupInternalLabel;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer; AHeight: Integer); override;
  protected
    property BoundLabel: TJppControlBoundLabel read FBoundLabel;
    property BoundLabelPosition: TLabelPosition read FBoundLabelPosition write SetBoundLabelPosition default lpLeft;
    property BoundLabelSpacing: Integer read FBoundLabelSpacing write SetBoundLabelSpacing default 4;
    property Appearance: TJppFlatComboBoxAppearance read FAppearance write SetAppearance;
    property ButtonWidth: integer read FButtonWidth write SetButtonWidth default 13;
    property TagExt: TJppTagExt read FTagExt write SetTagExt;
    property ShowLabel: Boolean read FShowLabel write SetShowLabel default True;
    property AnchoredControls: TJppAnchoredControls read FAnchoredControls write SetAnchoredControls;
  end;
  {$endregion TJppCustomFlatComboBox}


  {$Region '   TJppFlatComboBox   '}
  TJppFlatComboBox = class(TJppCustomFlatComboBox)
  published
    property Appearance;
    property ButtonWidth;
    property TagExt;
    property BoundLabel;
    property BoundLabelPosition;
    property BoundLabelSpacing;
    property ShowLabel;
    property AnchoredControls;

    property Style;
    //property Color; // Use Appearance.NotmalBgColor
    property DragMode;
    property DragCursor;
    property DropDownCount default 16;
    property Enabled;
    property Font;
    property ItemHeight;
    property Items;
    property MaxLength;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property ItemIndex;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnStartDrag;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property OnEndDock;
    property OnStartDock;
    property AlignWithMargins;
    property CharCase;
    property Ctl3D;
    property DoubleBuffered;
    property ImeMode;
    property ImeName;
    property ParentColor;
    property ParentCtl3D;
    property ParentDoubleBuffered;
    {$IF CompilerVersion > 23}property StyleElements;{$IFEND}
    property TextHint;
    {$IFDEF DELPHI2010_OR_ABOVE}property Touch;{$ENDIF}
    property Align;
    property AutoCloseUp;
    property AutoComplete;
    property AutoCompleteDelay;
    property AutoDropDown;

    property OnCloseUp;
    property OnContextPopup;
    {$IFDEF DELPHI2010_OR_ABOVE}property OnGesture;{$ENDIF}
    property OnMouseEnter;
    property OnMouseLeave;
    property OnSelect;
  end;
  {$endregion TJppFlatComboBox}



implementation


{$Region '         helpers          '}
function Min(val1, val2: Word): Word;
begin
  Result := val1;
  if val1 > val2 then Result := val2;
end;

function GetFontMetrics(Font: TFont): TTextMetric;
var
  DC: HDC;
  SaveFont: HFont;
begin
  DC := GetDC(0);
  SaveFont := SelectObject(DC, Font.Handle);
  GetTextMetrics(DC, Result);
  SelectObject(DC, SaveFont);
  ReleaseDC(0, DC);
end;

function GetFontHeight(Font: TFont): Integer;
begin
  with GetFontMetrics(Font) do Result := Round(tmHeight + tmHeight / 8);
end;

function RectInRect(R1, R2: TRect): Boolean;
begin
  Result := IntersectRect(R1, R1, R2);
end;
{$endregion helpers}


{$Region '                        TJppCustomFlatComboBox                             '}

constructor TJppCustomFlatComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FBoundLabelPosition := lpLeft;
  FBoundLabelSpacing := 4;
  SetupInternalLabel;

  ControlStyle := ControlStyle - [csFixedHeight] + [csOpaque];
  TControlCanvas(Canvas).Control := self;
  FButtonWidth := 13; // 11;
  FSysBtnWidth := GetSystemMetrics(SM_CXVSCROLL);
  FListInstance := MakeObjectInstance(ListWndProc);
  FDefListProc := nil;
  ItemHeight := 13;
  DropDownCount := 16;

  FAppearance := TJppFlatComboBoxAppearance.Create(AOwner);
  FAppearance.OnChange := PropsChanged;

  FTagExt := TJppTagExt.Create(Self);
  FShowLabel := True;
  FAnchoredControls := TJppAnchoredControls.Create(Self);

  //Font.OnChange := nil;
end;

destructor TJppCustomFlatComboBox.Destroy;
begin
  FAppearance.Free;
  FTagExt.Free;
  FAnchoredControls.Free;
  FreeObjectInstance(FListInstance);
  inherited;
end;

procedure TJppCustomFlatComboBox.SetAnchoredControls(const Value: TJppAnchoredControls);
begin
  FAnchoredControls := Value;
end;

procedure TJppCustomFlatComboBox.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
    if not (csDestroying in ComponentState) then
    begin
      if AComponent = FBoundLabel then FBoundLabel := nil;
      if Assigned(FAnchoredControls) then
      begin
        if AComponent = FAnchoredControls.Top.Control then FAnchoredControls.Top.Control := nil
        else if AComponent = FAnchoredControls.Bottom.Control then FAnchoredControls.Bottom.Control := nil
        else if AComponent = FAnchoredControls.Left.Control then FAnchoredControls.Left.Control := nil
        else if AComponent = FAnchoredControls.Right.Control then FAnchoredControls.Right.Control := nil;
      end;
    end;
end;

procedure TJppCustomFlatComboBox.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  SetBoundLabelPosition(FBoundLabelPosition);
  if not (csDestroying in ComponentState) then
    if Assigned(FAnchoredControls) then FAnchoredControls.UpdateAllControlsPos;
end;

procedure TJppCustomFlatComboBox.AdjustLabelBounds(Sender: TObject);
begin
  SetBoundLabelPosition(FBoundLabelPosition);
end;

procedure TJppCustomFlatComboBox.SetBoundLabelPosition(const Value: TLabelPosition);
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
  FBoundLabel.Visible := FShowLabel and Visible;
end;

procedure TJppCustomFlatComboBox.SetBoundLabelSpacing(const Value: Integer);
begin
  FBoundLabelSpacing := Value;
  SetBoundLabelPosition(FBoundLabelPosition);
end;

procedure TJppCustomFlatComboBox.SetParent(AParent: TWinControl);
begin
  inherited;
  if FBoundLabel <> nil then
  begin
    FBoundLabel.Parent := AParent;
    FBoundLabel.Visible := True;
  end;
end;

procedure TJppCustomFlatComboBox.SetShowLabel(const Value: Boolean);
begin
  if FBoundLabel.Visible = Value then Exit;
  FShowLabel := Value;
  FBoundLabel.Visible := FShowLabel;
end;

procedure TJppCustomFlatComboBox.SetupInternalLabel;
begin
  if Assigned(FBoundLabel) then Exit;
  FBoundLabel := TJppControlBoundLabel.Create(Self);
  FBoundLabel.FreeNotification(Self);
  FBoundLabel.OnAdjustBounds := AdjustLabelBounds;
  FBoundLabel.FocusControl := Self;
end;

procedure TJppCustomFlatComboBox.CMBiDiModeChanged(var Message: TMessage);
begin
  inherited;
  if FBoundLabel <> nil then FBoundLabel.BiDiMode := BiDiMode;
end;

procedure TJppCustomFlatComboBox.SetAppearance(const Value: TJppFlatComboBoxAppearance);
begin
  FAppearance := Value;
  PropsChanged(Self);
end;

procedure TJppCustomFlatComboBox.SetButtonWidth(const Value: integer);
begin
  if FButtonWidth = Value then Exit;
  FButtonWidth := Value;
  PropsChanged(Self);
end;


procedure TJppCustomFlatComboBox.CMSysColorChange(var Message: TMessage);
begin
  Invalidate;
end;

procedure TJppCustomFlatComboBox.CMParentColorChanged(var Message: TWMNoParams);
begin
  Invalidate;
end;

procedure TJppCustomFlatComboBox.WndProc(var Message: TMessage);
var
  cl: TColor;
begin
  if (Message.Msg = WM_PARENTNOTIFY) then
    case LoWord(Message.wParam) of

      WM_CREATE:
        if FDefListProc <> nil then
        begin
          SetWindowLong(FListHandle, GWL_WNDPROC, Longint(FDefListProc));
          FDefListProc := nil;
          FChildHandle := Message.lParam;
        end
        else if FChildHandle = 0 then FChildHandle := Message.lParam
        else FListHandle := Message.lParam;
    end

  else if (Message.Msg = WM_WINDOWPOSCHANGING) then
    if Style in [csDropDown, csSimple] then
      SetWindowPos(EditHandle, 0, 0, 0, ClientWidth - FButtonWidth - 2 * 2 - 4, Height - 2 * 2 - 2, SWP_NOMOVE + SWP_NOZORDER + SWP_NOACTIVATE + SWP_NOREDRAW);

  inherited;

  // Expanded combo background color
  if Message.Msg = WM_CTLCOLORLISTBOX then
  begin
    cl := FAppearance.ExpandedComboBgColor;
    SetBkColor(Message.wParam, ColorToRGB(cl));
    Message.Result := CreateSolidBrush(ColorToRGB(cl));
  end;
end;


procedure TJppCustomFlatComboBox.ListWndProc(var Message: TMessage);
begin
  case Message.Msg of

    WM_WINDOWPOSCHANGING:
      with TWMWindowPosMsg(Message).WindowPos^ do
      begin
        // size of the drop down list
        if Style in [csDropDown, csDropDownList] then cy := (GetFontHeight(Font) - 2) * Min(DropDownCount, Items.Count) + 4
        else cy := (ItemHeight) * Min(DropDownCount, Items.Count) + 4;
        if cy <= 4 then cy := 10;
      end;

  else
    with Message do
      Result := CallWindowProc(FDefListProc, FListHandle, Msg, wParam, lParam);
  end;
end;

procedure TJppCustomFlatComboBox.Loaded;
begin
  inherited;
  Font.Color := FAppearance.NormalTextColor;
  Color := FAppearance.NormalBgColor;
  //ApplyAppearance;
end;

procedure TJppCustomFlatComboBox.ComboWndProc(var Message: TMessage; ComboWnd: HWND; ComboProc: Pointer);
begin
  inherited;
  if (ComboWnd = EditHandle) then
    case Message.Msg of
      WM_SETFOCUS, WM_KILLFOCUS: SetSolidBorder;
    end;
end;


procedure TJppCustomFlatComboBox.CMEnabledChanged(var Msg: TMessage);
begin
  inherited;
  if FBoundLabel <> nil then FBoundLabel.Enabled := Enabled;
  ApplyAppearance;
  Invalidate;
end;

procedure TJppCustomFlatComboBox.CMVisibleChanged(var Message: TMessage);
begin
  inherited;
  if FBoundLabel <> nil then FBoundLabel.Visible := Visible;
end;

procedure TJppCustomFlatComboBox.CNCommand(var Message: TWMCommand);
var
  R: TRect;
begin
  inherited;

  if Message.NotifyCode in [1, 9, CBN_DROPDOWN, CBN_SELCHANGE] then
    if not(Style in [csSimple, csDropDown]) then InvalidateSelection;

  if (Message.NotifyCode in [CBN_CLOSEUP]) then
  begin
    R := GetButtonRect;
    Dec(R.Left, 2);
    InvalidateRect(Handle, @R, False);
  end;
end;

procedure TJppCustomFlatComboBox.WMKeyDown(var Message: TMessage);
var
  S: String;
begin
  S := Text;
  inherited;
  if not(Style in [csSimple, csDropDown]) and (Text <> S) then InvalidateSelection;
end;

procedure TJppCustomFlatComboBox.WMPaint(var Message: TWMPaint);
var
  R: TRect;
  DC: HDC;
  PS: TPaintStruct;
begin
  DC := BeginPaint(Handle, PS);
  try
    R := PS.rcPaint;
    if R.Right > Width - FButtonWidth - 4 then R.Right := Width - FButtonWidth - 4;

    // jacek
    // Poni¿szy IF eliminuje migotanie
    if Style <> csDropDown then
      FillRect(DC, R, Brush.Handle);

    if RectInRect(GetButtonRect, PS.rcPaint) then PaintButton;
    ExcludeClipRect(DC, ClientWidth - FSysBtnWidth - 2, 0, ClientWidth, ClientHeight);

    // jacek
    // Poni¿szy IF eliminuje migotanie
    if Style <> csDropDown then
      PaintWindow(DC);

    if (Style = csDropDown) and DroppedDown then
    begin
      R := ClientRect;
      InflateRect(R, -2, -2);
      R.Right := Width - FButtonWidth - 3;
      Canvas.Brush.Color := clWindow;
      Canvas.FrameRect(R);
    end
    else if Style <> csDropDown then InvalidateSelection;

  finally
    EndPaint(Handle, PS);
  end;

  RedrawBorders;
  Message.Result := 0;
end;

procedure TJppCustomFlatComboBox.WMNCPaint(var Message: TMessage);
var
  R: TRect;
  ColorRec: TColorRec;
begin
  inherited;

  with Canvas do
  begin
    R := ClientRect;
    GetColors(ColorRec);
    Brush.Color := ColorRec.Background;// Color; // DONE: WMNCPaint - fix color
    FillRect(R);
  end;

  RedrawBorders;
end;

procedure TJppCustomFlatComboBox.CMFontChanged(var Message: TMessage);
begin
  inherited;
  ItemHeight := 13;
  RecreateWnd;
end;

procedure TJppCustomFlatComboBox.ApplyAppearance;
var
  ColorRec: TColorRec;
  FontChange: TNotifyEvent;
begin
  GetColors(ColorRec);
  if Color <> ColorRec.Background then Color := ColorRec.Background;
  if Font.Color <> ColorRec.Text then
  begin
    FontChange := Font.OnChange;
    Font.OnChange := nil;
    Font.Color := ColorRec.Text;
    Font.OnChange := FontChange;
  end;
end;

procedure TJppCustomFlatComboBox.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  FMouseOverControl := True;
  ApplyAppearance;
  RedrawBorders;
end;

procedure TJppCustomFlatComboBox.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  FMouseOverControl := False;
  ApplyAppearance;
  RedrawBorders;
end;

procedure TJppCustomFlatComboBox.DoEnter;
var
  FontChange: TNotifyEvent;
begin
  if Color <> FAppearance.FocusedBgColor then Color := FAppearance.FocusedBgColor;
  if Font.Color <> FAppearance.FocusedTextColor then
  begin
    FontChange := Font.OnChange;
    Font.OnChange := nil;
    Font.Color := FAppearance.FocusedTextColor;
    Font.OnChange := FontChange;
  end;

  inherited;
end;

procedure TJppCustomFlatComboBox.DoExit;
var
  FontChange: TNotifyEvent;
begin
  if Color <> FAppearance.NormalBgColor then Color := FAppearance.NormalBgColor;
  if Font.Color <> FAppearance.NormalTextColor then
  begin
    FontChange := Font.OnChange;
    Font.OnChange := nil;
    Font.Color := FAppearance.NormalTextColor;
    Font.OnChange := FontChange;
  end;

  inherited;
end;

procedure TJppCustomFlatComboBox.WMSetFocus(var Message: TMessage);
begin
  inherited;
  if not(csDesigning in ComponentState) then
  begin
    ApplyAppearance;
    SetSolidBorder;
    if not(Style in [csSimple, csDropDown]) then InvalidateSelection;
  end;
end;

procedure TJppCustomFlatComboBox.WMKillFocus(var Message: TMessage);
begin
  inherited;
  if not(csDesigning in ComponentState) then
  begin
    ApplyAppearance;
    SetSolidBorder;
    if not(Style in [csSimple, csDropDown]) then InvalidateSelection; ApplyAppearance;

  end;
end;

procedure TJppCustomFlatComboBox.PropsChanged(Sender: TObject);
begin
  if csLoading in ComponentState then Exit;
  //ApplyAppearance;
  Invalidate;
end;

procedure TJppCustomFlatComboBox.InvalidateSelection;
var
  R: TRect;
  ColorRec: TColorRec;
begin
  R := ClientRect;
  InflateRect(R, -2, -3);
  R.Left := R.Right - FButtonWidth - 8;
  Dec(R.Right, FButtonWidth + 3);
  GetColors(ColorRec);

  if (GetFocus = Handle) and not DroppedDown then Canvas.Brush.Color := clHighlight
  else Canvas.Brush.Color := Color;

  Canvas.Brush.Style := bsSolid;
  Canvas.FillRect(R);

  if (GetFocus = Handle) and not DroppedDown then
  begin
    R := ClientRect;
    InflateRect(R, -3, -3);
    Dec(R.Right, FButtonWidth + 2);
    Canvas.FrameRect(R);
    Canvas.Brush.Color := ColorRec.Background; // clWindow;
  end;

  ExcludeClipRect(Canvas.Handle, ClientWidth - FSysBtnWidth - 2, 0, ClientWidth, ClientHeight);
end;

function TJppCustomFlatComboBox.GetButtonRect: TRect;
begin
  GetWindowRect(Handle, Result);
  OffsetRect(Result, -Result.Left, -Result.Top);
  Inc(Result.Left, ClientWidth - FButtonWidth);
  OffsetRect(Result, -1, 0);
end;


  {$Region '         GetColors          '}
procedure TJppCustomFlatComboBox.GetColors(var ColorRec: TColorRec);
var
  bNotDesigning, bHot, bFocused, bDisabled: Boolean;
begin
  bDisabled := not Enabled;
  bNotDesigning := not (csDesigning in ComponentState);
  bHot := FMouseOverControl and not Focused;
  bFocused := Focused;

  if bDisabled then
  begin
    ColorRec.Background := FAppearance.DisabledBgColor;
    ColorRec.Border := FAppearance.DisabledBorderColor;
    ColorRec.Button := FAppearance.DisabledButtonBgColor;
    ColorRec.Arrow := FAppearance.DisabledButtonArrowColor;
    ColorRec.Text := FAppearance.DisabledTextColor;
  end
  else if (bFocused and bNotDesigning) then
  begin
    ColorRec.Background := FAppearance.FocusedBgColor;
    ColorRec.Border := FAppearance.FocusedBorderColor;
    ColorRec.Button := FAppearance.FocusedButtonBgColor;
    ColorRec.Arrow := FAppearance.FocusedButtonArrowColor;
    ColorRec.Text := FAppearance.FocusedTextColor;
  end
  else if (bHot and bNotDesigning and (Style = csDropDownList)) then // <-- flickering for other styles
  //else if (bHot and bNotDesigning) then
  begin
    ColorRec.Background := FAppearance.HotBgColor;
    ColorRec.Border := FAppearance.HotBorderColor;
    ColorRec.Button := FAppearance.HotButtonBgColor;
    ColorRec.Arrow := FAppearance.HotButtonArrowColor;
    ColorRec.Text := FAppearance.HotTextColor;
  end
  else
  begin
    ColorRec.Background := FAppearance.NormalBgColor;
    ColorRec.Border := FAppearance.NormalBorderColor;
    ColorRec.Button := FAppearance.NormalButtonBgColor;
    ColorRec.Arrow := FAppearance.NormalButtonArrowColor;
    ColorRec.Text := FAppearance.NormalTextColor;
  end;
end;
  {$endregion GetColors}


  {$Region '          PaintButton            '}
procedure TJppCustomFlatComboBox.PaintButton;
var
  R: TRect;
  x, y: Integer;
  ColorRec: TColorRec;
  clBorder, clButton, clArrow: TColor;
begin
  GetColors(ColorRec);
  clBorder := ColorRec.Border;
  clButton := ColorRec.Button;
  clArrow := ColorRec.Arrow;

  //if (FButtonColor = clButton) and (FArrowColor = clArrow) then Exit;

  FButtonColor := clButton;
  FArrowColor := clArrow;

  R := GetButtonRect;
  InflateRect(R, 1, 0);

  Canvas.Brush.Color := clButton;
  Canvas.FillRect(R);
  Canvas.Brush.Color := clBorder;
  Canvas.FrameRect(R);

  x := (R.Right - R.Left) div 2 - 6 + R.Left;

  if DroppedDown then y := (R.Bottom - R.Top) div 2 - 1 + R.Top
  else y := (R.Bottom - R.Top) div 2 - 1 + R.Top;

  Canvas.Brush.Color := clArrow;
  Canvas.Pen.Color := clArrow;
  if DroppedDown then Canvas.Polygon([Point(x + 4, y + 2), Point(x + 8, y + 2), Point(x + 6, y)])  // arrow up
  else
  begin
    //if (FButtonColor <> clButton) or (FArrowColor <> clArrow) then
    Canvas.Polygon([Point(x + 4, y), Point(x + 8, y), Point(x + 6, y + 2)]);                    // arrow down
  end;

  ExcludeClipRect(Canvas.Handle, ClientWidth - FSysBtnWidth, 0, ClientWidth, ClientHeight);
end;
  {$endregion PaintButton}


  {$Region '       PaintBorder        '}
procedure TJppCustomFlatComboBox.PaintBorder;
var
  DC: HDC;
  R: TRect;
  BtnFaceBrush, WindowBrush: HBRUSH;
  ColorRec: TColorRec;
begin
  GetColors(ColorRec);

  DC := GetWindowDC(Handle);

  GetWindowRect(Handle, R);
  OffsetRect(R, -R.Left, -R.Top);
  Dec(R.Right, FButtonWidth + 1);
  try
    BtnFaceBrush := CreateSolidBrush(ColorToRGB(ColorRec.Border));
    WindowBrush := CreateSolidBrush(ColorToRGB(ColorRec.Background));

    FrameRect(DC, R, BtnFaceBrush);
    InflateRect(R, -1, -1);
    FrameRect(DC, R, WindowBrush);
    InflateRect(R, -1, -1);
    FrameRect(DC, R, WindowBrush);
  finally
    ReleaseDC(Handle, DC);
  end;
  DeleteObject(WindowBrush);
  DeleteObject(BtnFaceBrush);
end;
  {$endregion PaintBorder}


function TJppCustomFlatComboBox.GetSolidBorder: Boolean;
begin
  Result := ((csDesigning in ComponentState) and Enabled) or
    (not(csDesigning in ComponentState) and (DroppedDown or (GetFocus = Handle)
    or (GetFocus = EditHandle)));
end;

procedure TJppCustomFlatComboBox.SetSolidBorder;
var
  sb: Boolean;
begin
  sb := GetSolidBorder;
  if sb <> FSolidBorder then
  begin
    FSolidBorder := sb;
    RedrawBorders;
  end;
end;

procedure TJppCustomFlatComboBox.SetTagExt(const Value: TJppTagExt);
begin
  FTagExt := Value;
end;

procedure TJppCustomFlatComboBox.RedrawBorders;
begin
  PaintBorder;
  if Style <> csSimple then PaintButton;
end;

{$endregion TJppCustomFlatComboBox}



{$Region '                  TJppFlatComboBoxAppearance                     '}

constructor TJppFlatComboBoxAppearance.Create(AOwner: TComponent);
begin
  inherited Create;
  FOwner := AOwner;

  FExpandedComboBgColor := clWindow;

  FNormalBgColor := clWindow;
  FNormalTextColor := clWindowText;
  FNormalBorderColor := $007A7A7A;
  FNormalButtonBgColor := clBtnFace;
  FNormalButtonArrowColor := clBlack;

  FFocusedBgColor := clWindow;
  FFocusedTextColor := clWindowText;
  FFocusedBorderColor := $00434343;
  FFocusedButtonBgColor := $00E2E2E2;
  FFocusedButtonArrowColor := clBlack;

  FHotBgColor := clWindow;
  FHotTextColor := clWindowText;
  FHotBorderColor := $00434343;
  FHotButtonBgColor := $00E2E2E2;
  FHotButtonArrowColor := clBlack;

  FDisabledBgColor := $00F3F3F3;
  FDisabledTextColor := clGrayText;
  FDisabledBorderColor := $00CAC8C8;
  FDisabledButtonBgColor := $00F3F3F3;
  FDisabledButtonArrowColor := clGray;
end;

destructor TJppFlatComboBoxAppearance.Destroy;
begin
  inherited;
end;

procedure TJppFlatComboBoxAppearance.Assign(Source: TJppFlatComboBoxAppearance);
begin
  FExpandedComboBgColor := Source.ExpandedComboBgColor;

  FNormalBgColor := Source.NormalBgColor;
  FNormalTextColor := Source.NormalTextColor;
  FNormalBorderColor := Source.NormalBorderColor;
  FNormalButtonBgColor := Source.NormalButtonBgColor;
  FNormalButtonArrowColor := Source.NormalButtonArrowColor;

  FFocusedBgColor := Source.FocusedBgColor;
  FFocusedTextColor := Source.FocusedTextColor;
  FFocusedBorderColor := Source.FocusedBorderColor;
  FFocusedButtonBgColor := Source.FocusedButtonBgColor;
  FFocusedButtonArrowColor := Source.FocusedButtonArrowColor;

  FHotBgColor := Source.HotBgColor;
  FHotTextColor := Source.HotTextColor;
  FHotBorderColor := Source.HotBorderColor;
  FHotButtonBgColor := Source.HotButtonBgColor;
  FHotButtonArrowColor := Source.HotButtonArrowColor;

  FDisabledBgColor := Source.DisabledBgColor;
  FDisabledTextColor := Source.DisabledTextColor;
  FDisabledBorderColor := Source.DisabledBorderColor;
  FDisabledButtonBgColor := Source.DisabledButtonBgColor;
  FDisabledButtonArrowColor := Source.DisabledButtonArrowColor;

  PropsChanged(Self);
end;

procedure TJppFlatComboBoxAppearance.SetNormalTextColor(const Value: TColor);
begin
  if FNormalTextColor = Value then Exit;
  FNormalTextColor := Value;
  PropsChanged(Self);
end;

procedure TJppFlatComboBoxAppearance.SetDisabledBgColor(const Value: TColor);
begin
  if FDisabledBgColor = Value then Exit;
  FDisabledBgColor := Value;
  PropsChanged(Self);
end;

procedure TJppFlatComboBoxAppearance.SetDisabledBorderColor(const Value: TColor);
begin
  if FDisabledBorderColor = Value then Exit;
  FDisabledBorderColor := Value;
  PropsChanged(Self);
end;

procedure TJppFlatComboBoxAppearance.SetDisabledButtonArrowColor(const Value: TColor);
begin
  if FDisabledButtonArrowColor = Value then Exit;
  FDisabledButtonArrowColor := Value;
  PropsChanged(Self);
end;

procedure TJppFlatComboBoxAppearance.SetDisabledButtonBgColor(const Value: TColor);
begin
  if FDisabledButtonBgColor = Value then Exit;
  FDisabledButtonBgColor := Value;
  PropsChanged(Self);
end;

procedure TJppFlatComboBoxAppearance.SetDisabledTextColor(const Value: TColor);
begin
  if FDisabledTextColor = Value then Exit;
  FDisabledTextColor := Value;
  PropsChanged(Self);
end;

procedure TJppFlatComboBoxAppearance.SetExpandedComboBgColor(const Value: TColor);
begin
  if FExpandedComboBgColor = Value then Exit;
  FExpandedComboBgColor := Value;
  PropsChanged(Self);
end;

procedure TJppFlatComboBoxAppearance.SetFocusedBgColor(const Value: TColor);
begin
  if FFocusedBgColor = Value then Exit;
  FFocusedBgColor := Value;
  PropsChanged(Self);
end;

procedure TJppFlatComboBoxAppearance.SetFocusedBorderColor(const Value: TColor);
begin
  if FFocusedBorderColor = Value then Exit;
  FFocusedBorderColor := Value;
  PropsChanged(Self);
end;

procedure TJppFlatComboBoxAppearance.SetFocusedButtonArrowColor(const Value: TColor);
begin
  if FFocusedButtonArrowColor = Value then Exit;
  FFocusedButtonArrowColor := Value;
  PropsChanged(Self);
end;

procedure TJppFlatComboBoxAppearance.SetFocusedButtonBgColor(const Value: TColor);
begin
  if FFocusedButtonBgColor = Value then Exit;
  FFocusedButtonBgColor := Value;
  PropsChanged(Self);
end;

procedure TJppFlatComboBoxAppearance.SetFocusedTextColor(const Value: TColor);
begin
  if FFocusedTextColor = Value then Exit;
  FFocusedTextColor := Value;
  PropsChanged(Self);
end;

procedure TJppFlatComboBoxAppearance.SetHotBgColor(const Value: TColor);
begin
  if FHotBgColor = Value then Exit;
  FHotBgColor := Value;
  PropsChanged(Self);
end;

procedure TJppFlatComboBoxAppearance.SetHotBorderColor(const Value: TColor);
begin
  if FHotBorderColor = Value then Exit;
  FHotBorderColor := Value;
  PropsChanged(Self);
end;

procedure TJppFlatComboBoxAppearance.SetHotButtonArrowColor(const Value: TColor);
begin
  if FHotButtonArrowColor = Value then Exit;
  FHotButtonArrowColor := Value;
  PropsChanged(Self);
end;

procedure TJppFlatComboBoxAppearance.SetHotButtonBgColor(const Value: TColor);
begin
  if FHotButtonBgColor = Value then Exit;
  FHotButtonBgColor := Value;
  PropsChanged(Self);
end;

procedure TJppFlatComboBoxAppearance.SetHotTextColor(const Value: TColor);
begin
  if FHotTextColor = Value then Exit;
  FHotTextColor := Value;
  PropsChanged(Self);
end;

procedure TJppFlatComboBoxAppearance.SetNormalBgColor(const Value: TColor);
begin
  if FNormalBgColor = Value then Exit;
  FNormalBgColor := Value;
  PropsChanged(Self);
end;

procedure TJppFlatComboBoxAppearance.SetNormalBorderColor(const Value: TColor);
begin
  if FNormalBorderColor = Value then Exit;
  FNormalBorderColor := Value;
  PropsChanged(Self);
end;

procedure TJppFlatComboBoxAppearance.SetNormalButtonArrowColor(const Value: TColor);
begin
  if FNormalButtonArrowColor = Value then Exit;
  FNormalButtonArrowColor := Value;
  PropsChanged(Self);
end;

procedure TJppFlatComboBoxAppearance.SetNormalButtonBgColor(const Value: TColor);
begin
  if FNormalButtonBgColor = Value then Exit;
  FNormalButtonBgColor := Value;
  PropsChanged(Self);
end;

{$endregion TJppFlatComboBoxAppearance}

end.