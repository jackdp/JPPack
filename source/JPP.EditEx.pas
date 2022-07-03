unit JPP.EditEx;

{
  Jacek Pazera
  http://www.pazera-software.com
  https://github.com/jackdp

  Border drawing from TFlatEditUnit.pas: https://sourceforge.net/projects/flatstyle/
}

{$I jpp.inc}
{$IFDEF FPC} {$mode delphi} {$ENDIF}

interface


uses
  {$IFDEF MSWINDOWS}Windows,{$ENDIF}
  Messages,
  SysUtils, Classes, Types, {$IFDEF HAS_SYSTEM_UITYPES}System.UITypes,{$ENDIF}
  Controls, Graphics, StdCtrls, ExtCtrls, Forms,
  {$IFDEF FPC}LCLType, LCLIntf, LMessages,{$ENDIF}
  JPP.Common, JPP.Common.Procs, JPP.AnchoredControls, JPP.Flash;


type

  {$region ' --- TJppCustomEditExAppearance --- '}
  TJppCustomEditExAppearance = class(TJppPersistent)
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
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(Source: TJppCustomEditExAppearance); reintroduce;
    procedure SetBackgroundColor(const AColor: TColor; SkipDisabledState: Boolean = True);
    procedure SetTextColor(const AColor: TColor; SkipDisabledState: Boolean = True);
    procedure SetBorderColor(const AColor: TColor; SkipDisabledState: Boolean = True);
    procedure SetColors(const Background, Text, Border: TColor; SkipDisabledState: Boolean = True);
  published
    property NormalBgColor: TColor read FNormalBgColor write SetNormalBgColor default clWindow;
    property NormalTextColor: TColor read FNormalTextColor write SetNormalTextColor default clWindowText;
    property NormalBorderColor: TColor read FNormalBorderColor write SetNormalBorderColor default clMedGray;
    property FocusedBgColor: TColor read FFocusedBgColor write SetFocusedBgColor default clWindow;
    property FocusedTextColor: TColor read FFocusedTextColor write SetFocusedTextColor default clWindowText;
    property FocusedBorderColor: TColor read FFocusedBorderColor write SetFocusedBorderColor default clMedGray;
    property HotBgColor: TColor read FHotBgColor write SetHotBgColor default clWindow;
    property HotTextColor: TColor read FHotTextColor write SetHotTextColor default clWindowText;
    property HotBorderColor: TColor read FHotBorderColor write SetHotBorderColor default clMedGray;
    property DisabledBgColor: TColor read FDisabledBgColor write SetDisabledBgColor default $00F3F3F3;
    property DisabledTextColor: TColor read FDisabledTextColor write SetDisabledTextColor default clGrayText; // TODO: disabled text color not working!
    property DisabledBorderColor: TColor read FDisabledBorderColor write SetDisabledBorderColor default $00CAC8C8;
  end;
  {$endregion TJppCustomEditExAppearance}

  TJppCustomEditEx = class;

  {$Region ' --- TJppFlashJppEditEx --- '}
  TJppFlashJppEditEx = class(TJppFlashBase) // nazwa TJppFlashEdit ju¿ jest zajêta
  private
    FEdit: TJppCustomEditEx;
    FOriginalNormalBgColor: TColor;
    FOriginalHotBgColor: TColor;
    FOriginalFocusedBgColor: TColor;
    procedure SetOriginalNormalBgColor(const Value: TColor);
    procedure SetOriginalHotBgColor(const Value: TColor);
    procedure SetOriginalFocusedBgColor(const Value: TColor);
  protected
    procedure FlashColorChanged(Sender: TObject; const AColor: TColor);
    procedure FlashFinished(Sender: TObject);
    property OriginalNormalBgColor: TColor read FOriginalNormalBgColor write SetOriginalNormalBgColor;
    property OriginalHotBgColor: TColor read FOriginalHotBgColor write SetOriginalHotBgColor;
    property OriginalFocusedBgColor: TColor read FOriginalFocusedBgColor write SetOriginalFocusedBgColor;

  public
    constructor Create(Edit: TJppCustomEditEx);
  published
    property Enabled;
    property FlashColor;
    property FlashCount;
    property FlashInterval;
    property OnFlashFinished;
  end;
  {$endregion TJppFlashJppEditEx}


  {$region ' --- TJppCustomEditEx --- '}
  TJppCustomEditEx = class(TCustomEdit) // class(TCustomLabeledEdit)
  private
    FBoundLabel: TJppControlBoundLabel;
    FMouseOverControl: Boolean;
    {$IFDEF MSWINDOWS} FTabOnEnter: Boolean; {$ENDIF}
    FTagExt: TJppTagExt;
    FShowLabel: Boolean;
    FAppearance: TJppCustomEditExAppearance;
    FFlash: TJppFlashJppEditEx;
    FBoundLabelSpacing: Integer;
    FBoundLabelPosition: TLabelPosition;
    FAnchoredControls: TJppAnchoredControls;
    procedure SetTagExt(const Value: TJppTagExt);
    procedure SetShowLabel(const Value: Boolean);
    procedure CMFontChanged (var Message: TMessage); message CM_FONTCHANGED;
    procedure CMMouseEnter (var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave (var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMSetFocus (var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus (var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMNCCalcSize (var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMNCPaint (var Message: TMessage); message WM_NCPAINT;
    procedure SetAppearance(const Value: TJppCustomEditExAppearance);
    procedure SetFlash(const Value: TJppFlashJppEditEx);
    procedure SetBoundLabelPosition(const Value: TLabelPosition);
    procedure SetBoundLabelSpacing(const Value: Integer);
    procedure AdjustLabelBounds(Sender: TObject);
    procedure RedrawBorder(Clip: HRGN = 0);
    procedure NewAdjustHeight;
    procedure SetAnchoredControls(const Value: TJppAnchoredControls);
  protected
    procedure SetParent(AParent: TWinControl); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;
    procedure CMBiDiModeChanged(var Message: TMessage); message CM_BIDIMODECHANGED;
    procedure DoEnter; override;
    procedure DoExit; override;
    {$IFDEF MSWINDOWS} procedure KeyPress(var Key: Char); override; {$ENDIF}
    procedure PropsChanged(Sender: TObject);
    procedure Loaded; override;

    procedure ApplyAppearance;
    property MouseOverControl: Boolean read FMouseOverControl;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure FlashBackground;
    procedure SetupInternalLabel;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer; AHeight: Integer); override;
    procedure ApplyFontColor(const TextColor: TColor; Normal: Boolean = True; Hot: Boolean = True; Focused: Boolean = True; Disabled: Boolean = False);
  protected
    property BoundLabel: TJppControlBoundLabel read FBoundLabel;
    property BoundLabelPosition: TLabelPosition read FBoundLabelPosition write SetBoundLabelPosition default lpLeft;
    property BoundLabelSpacing: Integer read FBoundLabelSpacing write SetBoundLabelSpacing default 4;

    property Appearance: TJppCustomEditExAppearance read FAppearance write SetAppearance;
    {$IFDEF MSWINDOWS}
    property TabOnEnter: Boolean read FTabOnEnter write FTabOnEnter; // if True, after pressing the Enter key, the focus is moved to the next control
    {$ENDIF}
    property TagExt: TJppTagExt read FTagExt write SetTagExt;
    property ShowLabel: Boolean read FShowLabel write SetShowLabel default True;
    property Flash: TJppFlashJppEditEx read FFlash write SetFlash;
    property AnchoredControls: TJppAnchoredControls read FAnchoredControls write SetAnchoredControls;
  end;
  {$endregion TJppCustomEditEx}


  {$region ' --- TJppEditEx --- '}
  TJppEditEx = class(TJppCustomEditEx)
  public
    property MouseOverControl;
  published
    property Align;
    property Alignment;
    property Anchors;
    property AutoSelect;
    property AutoSize;
    {$IFDEF DCC}
//    property BevelEdges;
//    property BevelInner;
//    property BevelKind;
//    property BevelOuter;
    {$ENDIF}
    property BiDiMode;
    {$IFDEF FPC}property BorderSpacing;{$ENDIF}
//    property BorderStyle;
    property CharCase;
    //property Color;  Use Appearance.NormalBgColor instead
    property Constraints;
    property Cursor;
//    {$IFDEF DCC} property Ctl3D; {$ENDIF}
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    {$IFDEF FPC}property EchoMode;{$ENDIF}
    property Enabled;
    property Font;
    property Height;
    property HelpContext;
    property HelpKeyword;
    property HelpType;
    property HideSelection;
    {$IFDEF DCC}
    property ImeMode;
    property ImeName;
    {$ENDIF}
    property Hint;
    property Left;
    property MaxLength;
    {$IFDEF DCC}
    property OEMConvert;
    {$ENDIF}
    property NumbersOnly;
    property ParentBiDiMode;
//    property ParentColor;
//    {$IFDEF DCC} property ParentCtl3D; {$ENDIF}
    {$IFDEF DCC}property ParentDoubleBuffered;{$ENDIF}
    {$IFDEF FPC}{$IFDEF HAS_WINCONTROL_WITH_PARENTDOUBLEBUFFERED}
    property ParentDoubleBuffered;
    {$ENDIF}{$ENDIF}
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    {$IFDEF DCC}{$IF RTLVersion > 23}property StyleElements;{$IFEND}{$ENDIF}
    property TabOrder;
    property TabStop;
    property Tag;
    property Text;
    property TextHint;
    property Top;
    {$IFDEF DELPHI2010_OR_ABOVE} property Touch; {$ENDIF}
    property Visible;
    property Width;

    // -------- Events --------
    property OnChange;
    {$IFDEF FPC}property OnChangeBounds;{$ENDIF}
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    {$IFDEF FPC}property OnEditingDone;{$ENDIF}
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    {$IFDEF DELPHI2010_OR_ABOVE}property OnGesture;{$ENDIF}
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    {$IFDEF DCC}property OnMouseActivate;{$ENDIF}
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    {$IFDEF FPC}
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    {$ENDIF}
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    {$IFDEF FPC}property OnUTF8KeyPress;{$ENDIF}

    // -------- Custom Properties -------
    property Appearance;
    {$IFDEF MSWINDOWS} property TabOnEnter; {$ENDIF}
    property ShowLabel;
    property Flash;
    property TagExt;

    property BoundLabel;
    property BoundLabelPosition;
    property BoundLabelSpacing;

    property AnchoredControls;
  end;
  {$endregion TJppEditEx}


implementation


{$region ' --------------------------------- TJppCustomEditEx ------------------------------- '}

constructor TJppCustomEditEx.Create(AOwner: TComponent);
begin
  inherited;

  FBoundLabelPosition := lpLeft;
  FBoundLabelSpacing := 4;
  SetupInternalLabel;

  FAppearance := TJppCustomEditExAppearance.Create(AOwner);
  FAppearance.OnChange := PropsChanged;

  FFlash := TJppFlashJppEditEx.Create(Self);

  FAnchoredControls := TJppAnchoredControls.Create(Self);

  FTagExt := TJppTagExt.Create(Self);
  FShowLabel := True;
  FMouseOverControl := False;

  AutoSize := False;
  {$IFDEF DCC}Ctl3D := False;{$ENDIF}

  BorderStyle := bsNone;
  ControlStyle := ControlStyle - [csFramed];
  SetBounds(0, 0, 121, 19);
end;

destructor TJppCustomEditEx.Destroy;
begin
  FAppearance.Free;
  FTagExt.Free;
  FFlash.Free;
  FAnchoredControls.Free;
  inherited;
end;

procedure TJppCustomEditEx.Loaded;
begin
  inherited;
  Color := FAppearance.NormalBgColor;
  Font.Color := FAppearance.NormalTextColor;
  //EditLabel.Visible := FShowLabel;
  if not(csDesigning in ComponentState) then NewAdjustHeight;
end;

procedure TJppCustomEditEx.SetAnchoredControls(const Value: TJppAnchoredControls);
begin
  FAnchoredControls := Value;
end;

procedure TJppCustomEditEx.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  SetBoundLabelPosition(FBoundLabelPosition);
  if not (csDestroying in ComponentState) then
    if Assigned(FAnchoredControls) then FAnchoredControls.UpdateAllControlsPos;
end;

procedure TJppCustomEditEx.NewAdjustHeight;
var
  DC: HDC;
  FontHandle: HFONT;
  Metrics: TTextMetric;
begin
  DC := GetDC(0);
  FontHandle := SelectObject(DC, Font.Handle);
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, FontHandle);
  ReleaseDC(0, DC);
  Height := Metrics.tmHeight + 6;
end;

procedure TJppCustomEditEx.Notification(AComponent: TComponent; Operation: TOperation);
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

procedure TJppCustomEditEx.ApplyAppearance;
var
  BgColor, TextColor: TColor;
begin
  if not Enabled then
  begin
    BgColor := FAppearance.DisabledBgColor;
    TextColor := FAppearance.DisabledTextColor;
  end
  else if Self.Focused then
  begin
    BgColor := FAppearance.FocusedBgColor;
    TextColor := FAppearance.FocusedTextColor;
  end
  else if FMouseOverControl then
  begin
    BgColor := FAppearance.HotBgColor;
    TextColor := FAppearance.HotTextColor;
  end
  else
  begin
    BgColor := FAppearance.NormalBgColor;
    TextColor := FAppearance.NormalTextColor;
  end;

  if Color <> BgColor then Color := BgColor;
  if Font.Color <> TextColor then Font.Color := TextColor;
  RedrawBorder;
end;


procedure TJppCustomEditEx.ApplyFontColor(const TextColor: TColor; Normal, Hot, Focused, Disabled: Boolean);
begin
  Font.Color := TextColor;
  if Normal then FAppearance.NormalTextColor := TextColor;
  if Hot then FAppearance.HotTextColor := TextColor;
  if Focused then FAppearance.FocusedTextColor := TextColor;
  if Disabled then FAppearance.DisabledTextColor := TextColor;
end;

procedure TJppCustomEditEx.PropsChanged(Sender: TObject);
begin
  if csLoading in ComponentState then Exit;
  ApplyAppearance;
end;

procedure TJppCustomEditEx.RedrawBorder(Clip: HRGN);
var
  DC: HDC;
  R: TRect;
  brBackground, brBorder: HBRUSH;
  bNotDesigning, bHot, bFocused, bDisabled: Boolean;
  clBackground, clBorder: TColor;
begin

  bDisabled := not Enabled;
  bNotDesigning := not (csDesigning in ComponentState);
  bHot := FMouseOverControl;
  bFocused := Focused;

  if bDisabled then
  begin
    clBackground := FAppearance.DisabledBgColor;
    clBorder := FAppearance.DisabledBorderColor;
  end
  else if (bFocused and bNotDesigning) then
  begin
    clBackground := FAppearance.FocusedBgColor;
    clBorder := FAppearance.FocusedBorderColor
  end
  else if (bHot and bNotDesigning) then
  begin
    clBackground := FAppearance.HotBgColor;
    clBorder := FAppearance.HotBorderColor;
  end
  else //if bNotDesigning then
  begin
    clBackground := FAppearance.NormalBgColor;
    clBorder := FAppearance.NormalBorderColor;
  end;

  brBackground := CreateSolidBrush(ColorToRGB(clBackground));
  brBorder := CreateSolidBrush(ColorToRGB(clBorder));

  DC := GetWindowDC(Handle);
  try
    GetWindowRect(Handle, R);
    OffsetRect(R, -R.Left, -R.Top);

    Color := clBackground;
    FrameRect(DC, R, brBorder);
    InflateRect(R, -1, -1);
    FrameRect(DC, R, brBackground);
    InflateRect(R, -1, -1);
    FrameRect(DC, R, brBackground);

  finally
    ReleaseDC(Handle, DC);
    DeleteObject(brBackground);
    DeleteObject(brBorder);
  end;

end;


procedure TJppCustomEditEx.CMBiDiModeChanged(var Message: TMessage);
begin
  inherited;
  if FBoundLabel <> nil then FBoundLabel.BiDiMode := BiDiMode;
end;

procedure TJppCustomEditEx.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if FBoundLabel <> nil then FBoundLabel.Enabled := Enabled;
  ApplyAppearance;
end;

procedure TJppCustomEditEx.CMFontChanged(var Message: TMessage);
begin
  inherited;
  if not((csDesigning in ComponentState) and (csLoading in ComponentState)) then NewAdjustHeight;
end;

procedure TJppCustomEditEx.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  {$IFDEF MSWINDOWS}
  if (GetActiveWindow <> 0) then
  {$ENDIF}
  begin
    FMouseOverControl := True;
    ApplyAppearance;
  end;
end;

procedure TJppCustomEditEx.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  FMouseOverControl := False;
  ApplyAppearance;
end;

procedure TJppCustomEditEx.CMVisibleChanged(var Message: TMessage);
begin
  inherited;
  if FBoundLabel <> nil then FBoundLabel.Visible := Visible;
end;

procedure TJppCustomEditEx.DoEnter;
begin
  Color := FAppearance.FocusedBgColor;
  Font.Color := FAppearance.FocusedTextColor;
  inherited;
end;

procedure TJppCustomEditEx.DoExit;
begin
  Color := FAppearance.NormalBgColor;
  Font.Color := FAppearance.NormalTextColor;
  inherited DoExit;
end;

procedure TJppCustomEditEx.FlashBackground;
begin
  FFlash.OriginalNormalBgColor := FAppearance.NormalBgColor;
  FFlash.OriginalHotBgColor := FAppearance.HotBgColor;
  FFlash.OriginalFocusedBgColor := FAppearance.FocusedBgColor;
  FFlash.OriginalColor := FAppearance.NormalBgColor;
  FFlash.Flash;
end;

procedure TJppCustomEditEx.SetAppearance(const Value: TJppCustomEditExAppearance);
begin
  FAppearance := Value;
  PropsChanged(Self);
end;

procedure TJppCustomEditEx.AdjustLabelBounds(Sender: TObject);
begin
  SetBoundLabelPosition(FBoundLabelPosition);
end;

procedure TJppCustomEditEx.SetBoundLabelPosition(const Value: TLabelPosition);
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

procedure TJppCustomEditEx.SetBoundLabelSpacing(const Value: Integer);
begin
  FBoundLabelSpacing := Value;
  SetBoundLabelPosition(FBoundLabelPosition);
end;

procedure TJppCustomEditEx.SetFlash(const Value: TJppFlashJppEditEx);
begin
  FFlash := Value;
end;

procedure TJppCustomEditEx.SetParent(AParent: TWinControl);
begin
  inherited;
  if FBoundLabel <> nil then
  begin
    FBoundLabel.Parent := AParent;
    FBoundLabel.Visible := True;
  end;
end;

procedure TJppCustomEditEx.SetShowLabel(const Value: Boolean);
begin
  if FBoundLabel.Visible = Value then Exit;
  FShowLabel := Value;
  FBoundLabel.Visible := FShowLabel;
end;

procedure TJppCustomEditEx.SetTagExt(const Value: TJppTagExt);
begin
  FTagExt := Value;
end;

procedure TJppCustomEditEx.SetupInternalLabel;
begin
  if Assigned(FBoundLabel) then Exit;
  FBoundLabel := TJppControlBoundLabel.Create(Self);
  FBoundLabel.FreeNotification(Self);
  FBoundLabel.OnAdjustBounds := AdjustLabelBounds;
  FBoundLabel.FocusControl := Self;
end;


procedure TJppCustomEditEx.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  if not(csDesigning in ComponentState) then ApplyAppearance;
end;

procedure TJppCustomEditEx.WMNCCalcSize(var Message: TWMNCCalcSize);
begin
  inherited;
  InflateRect(Message.CalcSize_Params^.rgrc[0], -3, -3);
end;

procedure TJppCustomEditEx.WMNCPaint(var Message: TMessage);
begin
  RedrawBorder(HRGN(Message.WParam));
end;

procedure TJppCustomEditEx.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  if not(csDesigning in ComponentState) then ApplyAppearance;
end;

{$IFDEF MSWINDOWS}
procedure TJppCustomEditEx.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);

  if TabOnEnter and (Owner is TWinControl) then
  begin
    if Key = Char(VK_RETURN) then
      if HiWord(GetKeyState(VK_SHIFT)) <> 0 then PostMessage((Owner as TWinControl).Handle, WM_NEXTDLGCTL, 1, 0)
      else PostMessage((Owner as TWinControl).Handle, WM_NEXTDLGCTL, 0, 0);
    Key := #0;
  end;
end;
{$ENDIF}


{$endregion TJppCustomEditEx}





{$region ' ---------------------- TJppCustomEditExAppearance ----------------------- '}

constructor TJppCustomEditExAppearance.Create(AOwner: TComponent);
begin
  inherited Create;
  FOwner := AOwner;

  FNormalBgColor := clWindow;
  FNormalTextColor := clWindowText;
  FNormalBorderColor := clMedGray;

  FFocusedBgColor := clWindow;
  FFocusedTextColor := clWindowText;
  FFocusedBorderColor := clMedGray;

  FHotBgColor := clWindow;
  FHotTextColor := clWindowText;
  FHotBorderColor := clMedGray;

  FDisabledBgColor := $00F3F3F3;
  FDisabledTextColor := clGrayText;
  FDisabledBorderColor := $00CAC8C8;
end;

destructor TJppCustomEditExAppearance.Destroy;
begin
  inherited;
end;

procedure TJppCustomEditExAppearance.Assign(Source: TJppCustomEditExAppearance);
begin
  FNormalBgColor := Source.NormalBgColor;
  FNormalTextColor := Source.NormalTextColor;
  FNormalBorderColor := Source.NormalBorderColor;

  FFocusedBgColor := Source.FocusedBgColor;
  FFocusedTextColor := Source.FocusedTextColor;
  FFocusedBorderColor := Source.FocusedBorderColor;

  FHotBgColor := Source.HotBgColor;
  FHotTextColor := Source.HotTextColor;
  FHotBorderColor := Source.HotBorderColor;

  FDisabledBgColor := Source.DisabledBgColor;
  FDisabledTextColor := Source.DisabledTextColor;
  FDisabledBorderColor := Source.DisabledBorderColor;

  PropsChanged(Self);
end;

procedure TJppCustomEditExAppearance.SetBackgroundColor(const AColor: TColor; SkipDisabledState: Boolean = True);
begin
  FNormalBgColor := AColor;
  FHotBgColor := AColor;
  FFocusedBgColor := AColor;
  if not SkipDisabledState then FDisabledBgColor := AColor;
  PropsChanged(Self);
end;

procedure TJppCustomEditExAppearance.SetTextColor(const AColor: TColor; SkipDisabledState: Boolean = True);
begin
  FNormalTextColor := AColor;
  FHotTextColor := AColor;
  FFocusedTextColor := AColor;
  if not SkipDisabledState then FDisabledTextColor := AColor;
  PropsChanged(Self);
end;

procedure TJppCustomEditExAppearance.SetBorderColor(const AColor: TColor; SkipDisabledState: Boolean);
begin
  FNormalBorderColor := AColor;
  FHotBorderColor := AColor;
  FFocusedBorderColor := AColor;
  if not SkipDisabledState then FDisabledBorderColor := AColor;
  PropsChanged(Self);
end;

procedure TJppCustomEditExAppearance.SetColors(const Background, Text, Border: TColor; SkipDisabledState: Boolean = True);
begin
  FNormalBgColor := Background;
  FNormalTextColor := Text;
  FNormalBorderColor := Border;

  FHotBgColor := Background;
  FHotTextColor := Text;
  FHotBorderColor := Border;

  FFocusedBgColor := Background;
  FFocusedTextColor := Text;
  FFocusedBorderColor := Border;

  if not SkipDisabledState then
  begin
    FDisabledBgColor := Background;
    FDisabledTextColor := Text;
    FDisabledBorderColor := Border;
  end;

  PropsChanged(Self);
end;

procedure TJppCustomEditExAppearance.SetNormalTextColor(const Value: TColor);
begin
  if FNormalTextColor = Value then Exit;
  FNormalTextColor := Value;
  PropsChanged(Self);
end;


procedure TJppCustomEditExAppearance.SetDisabledBgColor(const Value: TColor);
begin
  if FDisabledBgColor = Value then Exit;
  FDisabledBgColor := Value;
  PropsChanged(Self);
end;

procedure TJppCustomEditExAppearance.SetDisabledBorderColor(const Value: TColor);
begin
  if FDisabledBorderColor = Value then Exit;
  FDisabledBorderColor := Value;
  PropsChanged(Self);
end;

procedure TJppCustomEditExAppearance.SetDisabledTextColor(const Value: TColor);
begin
  if FDisabledTextColor = Value then Exit;
  FDisabledTextColor := Value;
  PropsChanged(Self);
end;

procedure TJppCustomEditExAppearance.SetFocusedBgColor(const Value: TColor);
begin
  if FFocusedBgColor = Value then Exit;
  FFocusedBgColor := Value;
  PropsChanged(Self);
end;

procedure TJppCustomEditExAppearance.SetFocusedBorderColor(const Value: TColor);
begin
  if FFocusedBorderColor = Value then Exit;
  FFocusedBorderColor := Value;
  PropsChanged(Self);
end;

procedure TJppCustomEditExAppearance.SetFocusedTextColor(const Value: TColor);
begin
  if FFocusedTextColor = Value then Exit;
  FFocusedTextColor := Value;
  PropsChanged(Self);
end;

procedure TJppCustomEditExAppearance.SetHotBgColor(const Value: TColor);
begin
  if FHotBgColor = Value then Exit;
  FHotBgColor := Value;
  PropsChanged(Self);
end;

procedure TJppCustomEditExAppearance.SetHotBorderColor(const Value: TColor);
begin
  if FHotBorderColor = Value then Exit;
  FHotBorderColor := Value;
  PropsChanged(Self);
end;

procedure TJppCustomEditExAppearance.SetHotTextColor(const Value: TColor);
begin
  if FHotTextColor = Value then Exit;
  FHotTextColor := Value;
  PropsChanged(Self);
end;

procedure TJppCustomEditExAppearance.SetNormalBgColor(const Value: TColor);
begin
  if FNormalBgColor = Value then Exit;
  FNormalBgColor := Value;
  PropsChanged(Self);
end;

procedure TJppCustomEditExAppearance.SetNormalBorderColor(const Value: TColor);
begin
  if FNormalBorderColor = Value then Exit;
  FNormalBorderColor := Value;
  PropsChanged(Self);
end;

{$endregion TJppCustomEditExAppearance}



{$region ' -------------------- TJppFlashJppEditEx ---------------------- '}

constructor TJppFlashJppEditEx.Create(Edit: TJppCustomEditEx);
begin
  inherited Create(Edit);
  FEdit := Edit;
  Enabled := True;
  FlashCount := 3;
  FlashInterval := 150;

  OnFlashColorChanged := FlashColorChanged;
  OnFlashFinished := FlashFinished;
  FreeOnFlashFinished := False;

  if FEdit is TJppCustomEditEx then
    with (FEdit as TJppCustomEditEx).Appearance do
    begin
      FOriginalNormalBgColor := NormalBgColor;
      FOriginalHotBgColor := HotBgColor;
      FOriginalFocusedBgColor := FocusedBgColor;
    end;
end;

procedure TJppFlashJppEditEx.FlashColorChanged(Sender: TObject; const AColor: TColor);
begin
  if FEdit is TJppCustomEditEx then
    with (FEdit as TJppCustomEditEx) do
    begin
      Appearance.NormalBgColor := AColor;
      Appearance.HotBgColor := AColor;
      Appearance.FocusedBgColor := AColor;
      ApplyAppearance;
    end;
end;

procedure TJppFlashJppEditEx.FlashFinished(Sender: TObject);
begin
  if FEdit is TJppCustomEditEx then
    with (FEdit as TJppCustomEditEx) do
    begin
      Appearance.NormalBgColor := FOriginalNormalBgColor;
      Appearance.HotBgColor := FOriginalHotBgColor;
      Appearance.FocusedBgColor := FOriginalFocusedBgColor;
      ApplyAppearance;
    end;
end;


procedure TJppFlashJppEditEx.SetOriginalFocusedBgColor(const Value: TColor);
begin
  FOriginalFocusedBgColor := Value;
end;

procedure TJppFlashJppEditEx.SetOriginalHotBgColor(const Value: TColor);
begin
  FOriginalHotBgColor := Value;
end;

procedure TJppFlashJppEditEx.SetOriginalNormalBgColor(const Value: TColor);
begin
  FOriginalNormalBgColor := Value;
end;

{$endregion TJppFlashJppEditEx}





end.



