unit JPP.MemoEx;

{
  Jacek Pazera
  http://www.pazera-software.com
  https://github.com/jackdp

  Based on the TFlatMemo from https://sourceforge.net/projects/flatstyle/
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

  {$region ' --- TJppCustomMemoExAppearance --- '}
  TJppCustomMemoExAppearance = class(TJppPersistent)
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
    procedure Assign(Source: TJppCustomMemoExAppearance); reintroduce;
  published
    property NormalBgColor: TColor read FNormalBgColor write SetNormalBgColor default clWindow;
    property NormalBorderColor: TColor read FNormalBorderColor write SetNormalBorderColor default clMedGray;
    property NormalTextColor: TColor read FNormalTextColor write SetNormalTextColor default clWindowText;

    property FocusedBgColor: TColor read FFocusedBgColor write SetFocusedBgColor default clWindow;
    property FocusedBorderColor: TColor read FFocusedBorderColor write SetFocusedBorderColor default clMedGray;
    property FocusedTextColor: TColor read FFocusedTextColor write SetFocusedTextColor default clWindowText;

    property HotBgColor: TColor read FHotBgColor write SetHotBgColor default clWindow;
    property HotBorderColor: TColor read FHotBorderColor write SetHotBorderColor default clMedGray;
    property HotTextColor: TColor read FHotTextColor write SetHotTextColor default clWindowText;

    property DisabledBgColor: TColor read FDisabledBgColor write SetDisabledBgColor default $00F3F3F3;
    property DisabledBorderColor: TColor read FDisabledBorderColor write SetDisabledBorderColor default $00CAC8C8;
    property DisabledTextColor: TColor read FDisabledTextColor write SetDisabledTextColor default clGrayText;
  end;
  {$endregion TJppCustomMemoExAppearance}


  TJppCustomMemoEx = class;

  {$Region ' --- TJppFlashMemoEx --- '}
  TJppFlashMemoEx = class(TJppFlashBase)
  private
    FMemo: TJppCustomMemoEx;
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
    constructor Create(Memo: TJppCustomMemoEx);
  published
    property Enabled;
    property FlashColor;
    property FlashCount;
    property FlashInterval;
    property OnFlashFinished;
  end;
  {$endregion TJppFlashMemoEx}


  TJppCustomMemoEx = class(TCustomMemo)
  private
    FParentColor: Boolean;
    FMouseOverControl: Boolean;

    FAppearance: TJppCustomMemoExAppearance;
    FFlash: TJppFlashMemoEx;
    FBoundLabel: TJppControlBoundLabel;
    FBoundLabelSpacing: Integer;
    FBoundLabelPosition: TLabelPosition;
    FShowLabel: Boolean;
    FTagExt: TJppTagExt;
    FAnchoredControls: TJppAnchoredControls;
    procedure SetParentColor (Value: Boolean);

    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMNCCalcSize(var Message: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure WMNCPaint(var Message: TMessage); message WM_NCPAINT;
    procedure CMSysColorChange(var Message: TMessage); message CM_SYSCOLORCHANGE;
    procedure CMParentColorChanged(var Message: TWMNoParams); message CM_PARENTCOLORCHANGED;

    procedure SetAppearance(const Value: TJppCustomMemoExAppearance);
    procedure SetFlash(const Value: TJppFlashMemoEx);
    procedure SetBoundLabelPosition(const Value: TLabelPosition);
    procedure SetBoundLabelSpacing(const Value: Integer);
    procedure SetShowLabel(const Value: Boolean);
    procedure AdjustLabelBounds(Sender: TObject);
    procedure SetTagExt(const Value: TJppTagExt);
    procedure SetAnchoredControls(const Value: TJppAnchoredControls);
  protected
    procedure RedrawBorder (Clip: HRGN = 0);
    procedure PropsChanged(Sender: TObject);
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure CMBiDiModeChanged(var Message: TMessage); message CM_BIDIMODECHANGED;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;
    procedure SetParent(AParent: TWinControl); override;
  public
    constructor Create (AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetupInternalLabel;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer; AHeight: Integer); override;
    procedure FlashBackground;
    procedure CopyAppearance(Memo: TJppCustomMemoEx);
    procedure ApplyAppearance;
    procedure AssignColors(const ColorBg, ColorText, ColorBorder: TColor);

  protected
    property Appearance: TJppCustomMemoExAppearance read FAppearance write SetAppearance;

    property BoundLabel: TJppControlBoundLabel read FBoundLabel;
    property BoundLabelPosition: TLabelPosition read FBoundLabelPosition write SetBoundLabelPosition default lpLeft;
    property BoundLabelSpacing: Integer read FBoundLabelSpacing write SetBoundLabelSpacing default 4;
    property ShowLabel: Boolean read FShowLabel write SetShowLabel default True;

    property Flash: TJppFlashMemoEx read FFlash write SetFlash;
    property TagExt: TJppTagExt read FTagExt write SetTagExt;
    property MouseOverControl: Boolean read FMouseOverControl;
    property AnchoredControls: TJppAnchoredControls read FAnchoredControls write SetAnchoredControls;

    property ParentColor: Boolean read FParentColor write SetParentColor default False;

  end;



  TJppMemoEx = class(TJppCustomMemoEx)
  public
    property MouseOverControl;
  published
    property Align;
    property Alignment;
    property AlignWithMargins;
    property Anchors;
    property BiDiMode;
    property CharCase;
    property Constraints;
    property Ctl3D;
    property Cursor;
    property CustomHint;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property Height;
    property HelpContext;
    property HelpKeyword;
    property HelpType;
    property HideSelection;
    property Hint;
    property ImeMode;
    property ImeName;
    property Left;
    property Lines;
    property Margins;
    property MaxLength;
    property OEMConvert;
    property ParentBiDiMode;
//    property ParentColor;
    property ParentCtl3D;
    property ParentCustomHint;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ScrollBars;
    property ShowHint;
    {$IFDEF HAS_STYLE_ELEMENTS}property StyleElements;{$ENDIF}
    property TabOrder;
    property TabStop;
    property Tag;
    property Text;
    property Top;
    {$IFDEF DELPHI2010_OR_ABOVE}property Touch;{$ENDIF}
    property Visible;
    property WantReturns;
    property WantTabs;
    property Width;
    property WordWrap;

    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;

    property OnEndDock;
    property OnStartDock;


    property Appearance;
    property Flash;
    property BoundLabel;
    property BoundLabelPosition;
    property BoundLabelSpacing;
    property AnchoredControls;

  end;


implementation


constructor TJppCustomMemoEx.Create (AOwner: TComponent);
begin
  inherited;

  FBoundLabelPosition := lpLeft;
  FBoundLabelSpacing := 4;
  SetupInternalLabel;
  FShowLabel := True;

  FTagExt := TJppTagExt.Create(Self);

  FMouseOverControl := False;
  ParentFont := True;

  FAppearance := TJppCustomMemoExAppearance.Create(AOwner);
  FAppearance.OnChange := PropsChanged;

  FFlash := TJppFlashMemoEx.Create(Self);
  FAnchoredControls := TJppAnchoredControls.Create(Self);

  FParentColor := False;
  AutoSize := False;
  Ctl3D := False;
  BorderStyle := bsNone;
  ControlStyle := ControlStyle - [csFramed];
  SetBounds(0, 0, 185, 89);
end;

destructor TJppCustomMemoEx.Destroy;
begin
  FAppearance.Free;
  FFlash.Free;
  FTagExt.Free;
  FAnchoredControls.Free;
  inherited;
end;

procedure TJppCustomMemoEx.Loaded;
begin
  inherited;
  Color := FAppearance.NormalBgColor;
  Font.Color := FAppearance.NormalTextColor;
end;

procedure TJppCustomMemoEx.SetAnchoredControls(const Value: TJppAnchoredControls);
begin
  FAnchoredControls := Value;
end;

procedure TJppCustomMemoEx.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
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

procedure TJppCustomMemoEx.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  SetBoundLabelPosition(FBoundLabelPosition);
  if not (csDestroying in ComponentState) then
    if Assigned(FAnchoredControls) then FAnchoredControls.UpdateAllControlsPos;
end;

procedure TJppCustomMemoEx.PropsChanged(Sender: TObject);
begin
  if csLoading in ComponentState then Exit;
  ApplyAppearance;
end;

procedure TJppCustomMemoEx.FlashBackground;
begin
  FFlash.OriginalNormalBgColor := FAppearance.NormalBgColor;
  FFlash.OriginalHotBgColor := FAppearance.HotBgColor;
  FFlash.OriginalFocusedBgColor := FAppearance.FocusedBgColor;
  FFlash.OriginalColor := FAppearance.NormalBgColor;
  FFlash.Flash;
end;

procedure TJppCustomMemoEx.SetParent(AParent: TWinControl);
begin
  inherited;
  if FBoundLabel <> nil then
  begin
    FBoundLabel.Parent := AParent;
    FBoundLabel.Visible := True;
  end;
end;

procedure TJppCustomMemoEx.SetParentColor (Value: Boolean);
begin
  if Value <> FParentColor then
  begin
    FParentColor := Value;
    if FParentColor then
    begin
      //if Parent <> nil then FColorNormalBg := TForm(Parent).Color;
      ApplyAppearance;
    end;
  end;
end;

procedure TJppCustomMemoEx.SetShowLabel(const Value: Boolean);
begin
  if FBoundLabel.Visible = Value then Exit;
  FShowLabel := Value;
  FBoundLabel.Visible := FShowLabel;
end;

procedure TJppCustomMemoEx.SetTagExt(const Value: TJppTagExt);
begin
  FTagExt := Value;
end;

procedure TJppCustomMemoEx.SetupInternalLabel;
begin
  if Assigned(FBoundLabel) then Exit;
  FBoundLabel := TJppControlBoundLabel.Create(Self);
  FBoundLabel.FreeNotification(Self);
  FBoundLabel.OnAdjustBounds := AdjustLabelBounds;
  FBoundLabel.FocusControl := Self;
end;

procedure TJppCustomMemoEx.CMSysColorChange (var Message: TMessage);
begin
//  if FParentColor then
//    if Parent <> nil then FColorNormalBg := TForm(Parent).Color;
  PropsChanged(Self);
end;

procedure TJppCustomMemoEx.CMVisibleChanged(var Message: TMessage);
begin
  inherited;
  if FBoundLabel <> nil then FBoundLabel.Visible := Visible;
end;

procedure TJppCustomMemoEx.CopyAppearance(Memo: TJppCustomMemoEx);
begin
  FAppearance.Assign(Memo.Appearance);
end;

procedure TJppCustomMemoEx.CMParentColorChanged (var Message: TWMNoParams);
begin
//  if FParentColor then
//    if Parent <> nil then FColorNormalBg := TForm(Parent).Color;
  PropsChanged(Self);
end;

procedure TJppCustomMemoEx.SetAppearance(const Value: TJppCustomMemoExAppearance);
begin
  FAppearance := Value;
  PropsChanged(Self);
end;

procedure TJppCustomMemoEx.AdjustLabelBounds(Sender: TObject);
begin
  SetBoundLabelPosition(FBoundLabelPosition);
end;

procedure TJppCustomMemoEx.SetBoundLabelPosition(const Value: TLabelPosition);
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

procedure TJppCustomMemoEx.SetBoundLabelSpacing(const Value: Integer);
begin
  FBoundLabelSpacing := Value;
  SetBoundLabelPosition(FBoundLabelPosition);
end;

procedure TJppCustomMemoEx.SetFlash(const Value: TJppFlashMemoEx);
begin
  FFlash := Value;
end;

procedure TJppCustomMemoEx.CMMouseEnter (var Message: TMessage);
begin
  inherited;
  if (GetActiveWindow <> 0) then
  begin
    FMouseOverControl := True;
    ApplyAppearance;
  end;
end;

procedure TJppCustomMemoEx.CMMouseLeave (var Message: TMessage);
begin
  inherited;
  FMouseOverControl := False;
  ApplyAppearance;
end;

procedure TJppCustomMemoEx.WMSetFocus (var Message: TWMSetFocus);
begin
  inherited;
  if not (csDesigning in ComponentState) then ApplyAppearance;
end;

procedure TJppCustomMemoEx.WMKillFocus (var Message: TWMKillFocus);
begin
  inherited;
  if not (csDesigning in ComponentState) then ApplyAppearance;
end;

procedure TJppCustomMemoEx.WMNCCalcSize (var Message: TWMNCCalcSize);
begin
  inherited;
  InflateRect(Message.CalcSize_Params^.rgrc[0], -3, -3);
end;

procedure TJppCustomMemoEx.WMNCPaint (var Message: TMessage);
begin
  inherited;
  RedrawBorder(HRGN(Message.WParam));
end;

procedure TJppCustomMemoEx.CMBiDiModeChanged(var Message: TMessage);
begin
  inherited;
  if FBoundLabel <> nil then FBoundLabel.BiDiMode := BiDiMode;
end;

procedure TJppCustomMemoEx.CMEnabledChanged (var Message: TMessage);
begin
  inherited;
  if FBoundLabel <> nil then FBoundLabel.Enabled := Enabled;
  ApplyAppearance;
end;

procedure TJppCustomMemoEx.ApplyAppearance;
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
  Font.Color := TextColor;

  RedrawBorder;
end;

procedure TJppCustomMemoEx.AssignColors(const ColorBg, ColorText, ColorBorder: TColor);
begin
  FAppearance.NormalBgColor := ColorBg;
  FAppearance.NormalBorderColor := ColorBorder;
  FAppearance.NormalTextColor := ColorText;

  FAppearance.HotBgColor := ColorBg;
  FAppearance.HotBorderColor := ColorBorder;
  FAppearance.HotTextColor := ColorText;

  FAppearance.FocusedBgColor := ColorBg;
  FAppearance.FocusedBorderColor := ColorBorder;
  FAppearance.FocusedTextColor := ColorText;

  FAppearance.DisabledBgColor := ColorBg;
  FAppearance.DisabledBorderColor := ColorBorder;
  FAppearance.DisabledTextColor := ColorText;
end;

procedure TJppCustomMemoEx.RedrawBorder (Clip: HRGN = 0);
var
  DC: HDC;
  R: TRect;
  brBackground, brBorder, brTemp: HBRUSH;
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
    clBorder := FAppearance.FocusedBorderColor;
  end
  else if (bHot and bNotDesigning) then
  begin
    clBackground := FAppearance.HotBgColor;
    clBorder := FAppearance.HotBorderColor;
  end
  else
  begin
    clBackground := FAppearance.NormalBgColor;
    clBorder := FAppearance.NormalBorderColor;
  end;

  brBackground := CreateSolidBrush(ColorToRGB(clBackground));
  brBorder := CreateSolidBrush(ColorToRGB(clBorder));
  brTemp := CreateSolidBrush(ColorToRGB(clBtnFace));

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

    // Bottom-Right rectangle
    if ScrollBars = ssBoth then FillRect(DC, Rect(R.Right - 17, R.Bottom - 17, R.Right - 1, R.Bottom - 1), brTemp);

  finally
    ReleaseDC(Handle, DC);
    DeleteObject(brBackground);
    DeleteObject(brBorder);
    DeleteObject(brTemp);
  end;

end;



{$region ' ---------------------- TJppCustomMemoExAppearance ----------------------- '}

constructor TJppCustomMemoExAppearance.Create(AOwner: TComponent);
begin
  inherited Create;
  FOwner := AOwner;

  FNormalBgColor := clWindow;
  FNormalBorderColor := clMedGray;
  FNormalTextColor := clWindowText;

  FFocusedBgColor := clWindow;
  FFocusedBorderColor := clMedGray;
  FFocusedTextColor := clWindowText;

  FHotBgColor := clWindow;
  FHotBorderColor := clMedGray;
  FHotTextColor := clWindowText;

  FDisabledBgColor := $00F3F3F3;
  FDisabledBorderColor := $00CAC8C8;
  FDisabledTextColor := clGrayText;
end;

destructor TJppCustomMemoExAppearance.Destroy;
begin
  inherited;
end;

procedure TJppCustomMemoExAppearance.Assign(Source: TJppCustomMemoExAppearance);
begin
  FNormalBgColor := Source.NormalBgColor;
  FNormalBorderColor := Source.NormalBorderColor;
  FNormalTextColor := Source.NormalTextColor;

  FFocusedBgColor := Source.FocusedBgColor;
  FFocusedBorderColor := Source.FocusedBorderColor;
  FFocusedTextColor := Source.FocusedTextColor;

  FHotBgColor := Source.HotBgColor;
  FHotBorderColor := Source.HotBorderColor;
  FHotTextColor := Source.HotTextColor;

  FDisabledBgColor := Source.DisabledBgColor;
  FDisabledBorderColor := Source.DisabledBorderColor;
  FDisabledTextColor := Source.DisabledTextColor;

  PropsChanged(Self);
end;

procedure TJppCustomMemoExAppearance.SetNormalTextColor(const Value: TColor);
begin
  if FNormalTextColor = Value then Exit;
  FNormalTextColor := Value;
  PropsChanged(Self);
end;

procedure TJppCustomMemoExAppearance.SetDisabledBgColor(const Value: TColor);
begin
  if FDisabledBgColor = Value then Exit;
  FDisabledBgColor := Value;
  PropsChanged(Self);
end;

procedure TJppCustomMemoExAppearance.SetDisabledBorderColor(const Value: TColor);
begin
  if FDisabledBorderColor = Value then Exit;
  FDisabledBorderColor := Value;
  PropsChanged(Self);
end;

procedure TJppCustomMemoExAppearance.SetDisabledTextColor(const Value: TColor);
begin
  if FDisabledTextColor = Value then Exit;
  FDisabledTextColor := Value;
  PropsChanged(Self);
end;

procedure TJppCustomMemoExAppearance.SetFocusedBgColor(const Value: TColor);
begin
  if FFocusedBgColor = Value then Exit;
  FFocusedBgColor := Value;
  PropsChanged(Self);
end;

procedure TJppCustomMemoExAppearance.SetFocusedBorderColor(const Value: TColor);
begin
  if FFocusedBorderColor = Value then Exit;
  FFocusedBorderColor := Value;
  PropsChanged(Self);
end;

procedure TJppCustomMemoExAppearance.SetFocusedTextColor(const Value: TColor);
begin
  if FFocusedTextColor = Value then Exit;
  FFocusedTextColor := Value;
  PropsChanged(Self);
end;

procedure TJppCustomMemoExAppearance.SetHotBgColor(const Value: TColor);
begin
  if FHotBgColor = Value then Exit;
  FHotBgColor := Value;
  PropsChanged(Self);
end;

procedure TJppCustomMemoExAppearance.SetHotBorderColor(const Value: TColor);
begin
  if FHotBorderColor = Value then Exit;
  FHotBorderColor := Value;
  PropsChanged(Self);
end;

procedure TJppCustomMemoExAppearance.SetHotTextColor(const Value: TColor);
begin
  if FHotTextColor = Value then Exit;
  FHotTextColor := Value;
  PropsChanged(Self);
end;

procedure TJppCustomMemoExAppearance.SetNormalBgColor(const Value: TColor);
begin
  if FNormalBgColor = Value then Exit;
  FNormalBgColor := Value;
  PropsChanged(Self);
end;

procedure TJppCustomMemoExAppearance.SetNormalBorderColor(const Value: TColor);
begin
  if FNormalBorderColor = Value then Exit;
  FNormalBorderColor := Value;
  PropsChanged(Self);
end;

{$endregion TJppCustomMemoExAppearance}



{$region ' -------------------- TJppFlashMemoEx ---------------------- '}

constructor TJppFlashMemoEx.Create(Memo: TJppCustomMemoEx);
begin
  inherited Create(Memo);
  FMemo := Memo;
  Enabled := True;
  FlashCount := 3;
  FlashInterval := 150;

  OnFlashColorChanged := FlashColorChanged;
  OnFlashFinished := FlashFinished;
  FreeOnFlashFinished := False;

  if FMemo is TJppCustomMemoEx then
    with (FMemo as TJppCustomMemoEx).Appearance do
    begin
      FOriginalNormalBgColor := NormalBgColor;
      FOriginalHotBgColor := HotBgColor;
      FOriginalFocusedBgColor := FocusedBgColor;
    end;
end;

procedure TJppFlashMemoEx.FlashColorChanged(Sender: TObject; const AColor: TColor);
begin
  if FMemo is TJppCustomMemoEx then
    with (FMemo as TJppCustomMemoEx) do
    begin
      Appearance.NormalBgColor := AColor;
      Appearance.HotBgColor := AColor;
      Appearance.FocusedBgColor := AColor;
      ApplyAppearance;
    end;
end;

procedure TJppFlashMemoEx.FlashFinished(Sender: TObject);
begin
  if FMemo is TJppCustomMemoEx then
    with (FMemo as TJppCustomMemoEx) do
    begin
      Appearance.NormalBgColor := FOriginalNormalBgColor;
      Appearance.HotBgColor := FOriginalHotBgColor;
      Appearance.FocusedBgColor := FOriginalFocusedBgColor;
      ApplyAppearance;
    end;
end;


procedure TJppFlashMemoEx.SetOriginalFocusedBgColor(const Value: TColor);
begin
  FOriginalFocusedBgColor := Value;
end;

procedure TJppFlashMemoEx.SetOriginalHotBgColor(const Value: TColor);
begin
  FOriginalHotBgColor := Value;
end;

procedure TJppFlashMemoEx.SetOriginalNormalBgColor(const Value: TColor);
begin
  FOriginalNormalBgColor := Value;
end;

{$endregion TJppFlashMemoEx}



end.
