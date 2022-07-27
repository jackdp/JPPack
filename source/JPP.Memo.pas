unit JPP.Memo;

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
  Controls, Graphics, StdCtrls, ExtCtrls,
  {$IFDEF FPC}LCLType, LCLIntf, LMessages,{$ENDIF}
  JPP.Common, JPP.Common.Procs, JPP.AnchoredControls, JPP.Flash;


type

  {$region ' --- TJppCustomMemoAppearance --- '}
  TJppCustomMemoAppearance = class(TJppPersistent)
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
    procedure SetNormalBgColor(const Value: TColor);
    procedure SetFocusedBgColor(const Value: TColor);
    procedure SetNormalTextColor(const Value: TColor);
    procedure SetFocusedTextColor(const Value: TColor);
    procedure SetHotBgColor(const Value: TColor);
    procedure SetHotTextColor(const Value: TColor);
    procedure SetDisabledBgColor(const Value: TColor);
    procedure SetDisabledTextColor(const Value: TColor);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(Source: TJppCustomMemoAppearance); reintroduce;
  published
    property NormalBgColor: TColor read FNormalBgColor write SetNormalBgColor default clWindow;
    property NormalTextColor: TColor read FNormalTextColor write SetNormalTextColor default clWindowText;
    property FocusedBgColor: TColor read FFocusedBgColor write SetFocusedBgColor default clWindow;
    property FocusedTextColor: TColor read FFocusedTextColor write SetFocusedTextColor default clWindowText;
    property HotBgColor: TColor read FHotBgColor write SetHotBgColor default clWindow;
    property HotTextColor: TColor read FHotTextColor write SetHotTextColor default clWindowText;
    property DisabledBgColor: TColor read FDisabledBgColor write SetDisabledBgColor default clWindow;
    property DisabledTextColor: TColor read FDisabledTextColor write SetDisabledTextColor default clGrayText;
  end;
  {$endregion TJppCustomMemoAppearance}

  TJppCustomMemo = class;

  {$Region ' --- TJppFlashMemo --- '}
  TJppFlashMemo = class(TJppFlashBase)
  private
    FEdit: TJppCustomMemo;
  protected
    procedure FlashColorChanged(Sender: TObject; const AColor: TColor);
    procedure FlashFinished(Sender: TObject);
  public
    constructor Create(Edit: TJppCustomMemo);
  published
    property Enabled;
    property FlashColor;
    property FlashCount;
    property FlashInterval;
    property OnFlashFinished;
  end;
  {$endregion TJppFlashMemo}


  {$region ' --- TJppCustomMemo --- '}
  TJppCustomMemo = class(TCustomMemo)
  private
    FBoundLabel: TJppControlBoundLabel;
    FMouseOverControl: Boolean;
    {$IFDEF MSWINDOWS} FTabOnEnter: Boolean; {$ENDIF}
    FTagExt: TJppTagExt;
    FShowLabel: Boolean;
    FFlash: TJppFlashMemo;
    FBoundLabelSpacing: Integer;
    FBoundLabelPosition: TLabelPosition;
    FAppearance: TJppCustomMemoAppearance;
    FAnchoredControls: TJppAnchoredControls;
    procedure SetTagExt(const Value: TJppTagExt);
    procedure SetShowLabel(const Value: Boolean);
    procedure CMMouseEnter (var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave (var Message: TMessage); message CM_MOUSELEAVE;
    procedure SetFlash(const Value: TJppFlashMemo);
    procedure SetBoundLabelPosition(const Value: TLabelPosition);
    procedure SetBoundLabelSpacing(const Value: Integer);
    procedure AdjustLabelBounds(Sender: TObject);
    procedure SetAppearance(const Value: TJppCustomMemoAppearance);
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

    function Add(const s: string): integer;
    procedure AddStringArray(const Arr: TStringDynArray);
    function AddInteger(const x: integer; Prefix: string = ''; Postfix: string = ''): integer;

    procedure FlashBackground;
    procedure SetupInternalLabel;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer; AHeight: Integer); override;
  protected
    property BoundLabel: TJppControlBoundLabel read FBoundLabel;
    property BoundLabelPosition: TLabelPosition read FBoundLabelPosition write SetBoundLabelPosition default lpLeft;
    property BoundLabelSpacing: Integer read FBoundLabelSpacing write SetBoundLabelSpacing default 4;

    property Appearance: TJppCustomMemoAppearance read FAppearance write SetAppearance;
    {$IFDEF MSWINDOWS}
    property TabOnEnter: Boolean read FTabOnEnter write FTabOnEnter default False; // if True, after pressing the Enter key, the focus is moved to the next control
    {$ENDIF}
    property TagExt: TJppTagExt read FTagExt write SetTagExt;
    property ShowLabel: Boolean read FShowLabel write SetShowLabel default True;
    property Flash: TJppFlashMemo read FFlash write SetFlash;
    property AnchoredControls: TJppAnchoredControls read FAnchoredControls write SetAnchoredControls;
  end;
  {$endregion TJppCustomMemo}


  {$region ' --- TJppMemo --- '}
  TJppMemo = class(TJppCustomMemo)
  public
    property MouseOverControl;
  published
    property Align;
    property Alignment;
    {$IFDEF DCC}property AlignWithMargins;{$ENDIF}
    property Anchors;
    {$IFDEF DCC}
    property BevelEdges;
    property BevelInner;
    property BevelKind;
    property BevelOuter;
    {$ENDIF}
    property BiDiMode;
    {$IFDEF FPC}property BorderSpacing;{$ENDIF}
    property BorderStyle;
    property CharCase;
    //property Color;  Use Appearance.NormalBgColor instead
    property Constraints;
    {$IFDEF DCC} property Ctl3D; {$ENDIF}
    property Cursor;
    {$IFDEF DCC}property CustomHint;{$ENDIF}
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
    {$IFDEF DCC}
    property ImeMode;
    property ImeName;
    {$ENDIF}
    property Left;
    property Lines;
    {$IFDEF DCC}property Margins;{$ENDIF}
    property MaxLength;
    {$IFDEF DCC}property OEMConvert;{$ENDIF}
    property ParentBiDiMode;
    property ParentColor;
    {$IFDEF DCC}
    property ParentCtl3D;
    property ParentCustomHint;
    property ParentDoubleBuffered;
    {$ENDIF}
    {$IFDEF FPC}{$IFDEF HAS_WINCONTROL_WITH_PARENTDOUBLEBUFFERED}
    property ParentDoubleBuffered;
    {$ENDIF}{$ENDIF}
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    {$IFDEF FPC}property ReadOnly;{$ENDIF}
    property ScrollBars;
    property ShowHint;
    {$IFDEF DCC}{$IF RTLVersion > 23}property StyleElements;{$IFEND}{$ENDIF}
    property TabOrder;
    property TabStop;
    property Tag;
    property Text;
    {$IFDEF DCC}property TextHint;{$ENDIF}
    property Top;
    {$IFDEF DELPHI2010_OR_ABOVE} property Touch; {$ENDIF}
    property Visible;
    property WantReturns;
    property WantTabs;
    property Width;
    property WordWrap;

    // -------- Events ----------
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
    property OnStartDock;
    property OnStartDrag;
    {$IFDEF FPC}property OnUTF8KeyPress;{$ENDIF}
    {$IFDEF MSWINDOWS} property TabOnEnter; {$ENDIF}

    // ------- Custom Properties ---------
    property Appearance;
    property ShowLabel;
    property Flash;
    property TagExt;

    property BoundLabel;
    property BoundLabelPosition;
    property BoundLabelSpacing;

    property AnchoredControls;
  end;
  {$endregion TJppMemo}


implementation


{$region ' --------------------------------- TJppCustomMemo ------------------------------- '}

constructor TJppCustomMemo.Create(AOwner: TComponent);
begin
  inherited;

  FBoundLabelPosition := lpLeft;
  FBoundLabelSpacing := 4;
  SetupInternalLabel;

  FAppearance := TJppCustomMemoAppearance.Create(AOwner);
  FAppearance.OnChange := PropsChanged;

  FFlash := TJppFlashMemo.Create(Self);

  FTagExt := TJppTagExt.Create(Self);
  FShowLabel := True;
  FMouseOverControl := False;

  {$IFDEF MSWINDOWS}FTabOnEnter := False;{$ENDIF}

  FAnchoredControls := TJppAnchoredControls.Create(Self);
end;

destructor TJppCustomMemo.Destroy;
begin
  FAppearance.Free;
  FTagExt.Free;
  FFlash.Free;
  FAnchoredControls.Free;
  inherited;
end;

procedure TJppCustomMemo.Loaded;
begin
  inherited;
  Color := FAppearance.NormalBgColor;
  Font.Color := FAppearance.NormalTextColor;
end;

procedure TJppCustomMemo.SetAnchoredControls(const Value: TJppAnchoredControls);
begin
  FAnchoredControls := Value;
end;

procedure TJppCustomMemo.Notification(AComponent: TComponent; Operation: TOperation);
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

procedure TJppCustomMemo.SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer; AHeight: Integer);
begin
  inherited;
  SetBoundLabelPosition(FBoundLabelPosition);
  if not (csDestroying in ComponentState) then
    if Assigned(FAnchoredControls) then FAnchoredControls.UpdateAllControlsPos;
end;

procedure TJppCustomMemo.PropsChanged(Sender: TObject);
begin
  if csLoading in ComponentState then Exit;
  ApplyAppearance;
end;

procedure TJppCustomMemo.ApplyAppearance;
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
end;

procedure TJppCustomMemo.CMBiDiModeChanged(var Message: TMessage);
begin
  inherited;
  if FBoundLabel <> nil then FBoundLabel.BiDiMode := BiDiMode;
end;

procedure TJppCustomMemo.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if FBoundLabel <> nil then FBoundLabel.Enabled := Enabled;
  ApplyAppearance;
end;

procedure TJppCustomMemo.CMMouseEnter(var Message: TMessage);
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

procedure TJppCustomMemo.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  FMouseOverControl := False;
  ApplyAppearance;
end;

procedure TJppCustomMemo.CMVisibleChanged(var Message: TMessage);
begin
  inherited;
  if FBoundLabel <> nil then FBoundLabel.Visible := Visible;
end;

procedure TJppCustomMemo.DoEnter;
begin
  Color := FAppearance.FocusedBgColor;
  Font.Color := FAppearance.FocusedTextColor;
  inherited;
end;

procedure TJppCustomMemo.DoExit;
begin
  Color := FAppearance.NormalBgColor;
  Font.Color := FAppearance.NormalTextColor;
  inherited DoExit;
end;

procedure TJppCustomMemo.FlashBackground;
begin
  FFlash.OriginalColor := Color;
  FFlash.Flash;
end;

procedure TJppCustomMemo.AdjustLabelBounds(Sender: TObject);
begin
  SetBoundLabelPosition(FBoundLabelPosition);
end;

procedure TJppCustomMemo.SetAppearance(const Value: TJppCustomMemoAppearance);
begin
  FAppearance := Value;
  PropsChanged(Self);
end;

procedure TJppCustomMemo.SetBoundLabelPosition(const Value: TLabelPosition);
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

procedure TJppCustomMemo.SetBoundLabelSpacing(const Value: Integer);
begin
  FBoundLabelSpacing := Value;
  SetBoundLabelPosition(FBoundLabelPosition);
end;

procedure TJppCustomMemo.SetFlash(const Value: TJppFlashMemo);
begin
  FFlash := Value;
end;

procedure TJppCustomMemo.SetParent(AParent: TWinControl);
begin
  inherited;
  if FBoundLabel <> nil then
  begin
    FBoundLabel.Parent := AParent;
    FBoundLabel.Visible := True;
  end;
end;

procedure TJppCustomMemo.SetShowLabel(const Value: Boolean);
begin
  if FBoundLabel.Visible = Value then Exit;
  FShowLabel := Value;
  FBoundLabel.Visible := FShowLabel;
end;

procedure TJppCustomMemo.SetTagExt(const Value: TJppTagExt);
begin
  FTagExt := Value;
end;

procedure TJppCustomMemo.SetupInternalLabel;
begin
  if Assigned(FBoundLabel) then Exit;
  FBoundLabel := TJppControlBoundLabel.Create(Self);
  FBoundLabel.FreeNotification(Self);
  FBoundLabel.OnAdjustBounds := AdjustLabelBounds;
  FBoundLabel.FocusControl := Self;
  {$IFDEF FPC}FBoundLabel.OnChangeBounds := AdjustLabelBounds;{$ENDIF}
end;

{$IFDEF MSWINDOWS}
procedure TJppCustomMemo.KeyPress(var Key: Char);
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

function TJppCustomMemo.Add(const s: string): integer;
begin
  Result := Lines.Add(s);
end;

procedure TJppCustomMemo.AddStringArray(const Arr: TStringDynArray);
var
  i: integer;
begin
  Lines.BeginUpdate;
  try
    for i := 0 to Length(Arr) - 1 do
      Lines.Add(Arr[i]);
  finally
    Lines.EndUpdate;
  end;
end;

function TJppCustomMemo.AddInteger(const x: integer; Prefix: string = ''; Postfix: string = ''): integer;
begin
  Result := Lines.Add(Prefix + IntToStr(x) + Postfix);
end;

{$endregion TJppCustomMemo}



{$region ' ---------------------- TJppCustomMemoAppearance ----------------------- '}

constructor TJppCustomMemoAppearance.Create(AOwner: TComponent);
begin
  inherited Create;
  FOwner := AOwner;
  FNormalBgColor := clWindow;
  FNormalTextColor := clWindowText;
  FFocusedBgColor := clWindow;
  FFocusedTextColor := clWindowText;
  FHotBgColor := clWindow;
  FHotTextColor := clWindowText;
  FDisabledBgColor := clWindow;
  FDisabledTextColor := clGrayText;
end;

destructor TJppCustomMemoAppearance.Destroy;
begin
  inherited;
end;

procedure TJppCustomMemoAppearance.Assign(Source: TJppCustomMemoAppearance);
begin
  FNormalBgColor := Source.NormalBgColor;
  FNormalTextColor := Source.NormalTextColor;
  FFocusedBgColor := Source.FocusedBgColor;
  FFocusedTextColor := Source.FocusedTextColor;
  FHotBgColor := Source.HotBgColor;
  FHotTextColor := Source.HotTextColor;
  FDisabledBgColor := Source.DisabledBgColor;
  FDisabledTextColor := Source.DisabledTextColor;
end;

procedure TJppCustomMemoAppearance.SetNormalTextColor(const Value: TColor);
begin
  if FNormalTextColor = Value then Exit;
  FNormalTextColor := Value;
  PropsChanged(Self);
end;

procedure TJppCustomMemoAppearance.SetDisabledBgColor(const Value: TColor);
begin
  if FDisabledBgColor = Value then Exit;
  FDisabledBgColor := Value;
  PropsChanged(Self);
end;

procedure TJppCustomMemoAppearance.SetDisabledTextColor(const Value: TColor);
begin
  if FDisabledTextColor = Value then Exit;
  FDisabledTextColor := Value;
  PropsChanged(Self);
end;

procedure TJppCustomMemoAppearance.SetFocusedBgColor(const Value: TColor);
begin
  if FFocusedBgColor = Value then Exit;
  FFocusedBgColor := Value;
  PropsChanged(Self);
end;

procedure TJppCustomMemoAppearance.SetFocusedTextColor(const Value: TColor);
begin
  if FFocusedTextColor = Value then Exit;
  FFocusedTextColor := Value;
  PropsChanged(Self);
end;

procedure TJppCustomMemoAppearance.SetHotBgColor(const Value: TColor);
begin
  if FHotBgColor = Value then Exit;
  FHotBgColor := Value;
  PropsChanged(Self);
end;

procedure TJppCustomMemoAppearance.SetHotTextColor(const Value: TColor);
begin
  if FHotTextColor = Value then Exit;
  FHotTextColor := Value;
  PropsChanged(Self);
end;

procedure TJppCustomMemoAppearance.SetNormalBgColor(const Value: TColor);
begin
  if FNormalBgColor = Value then Exit;
  FNormalBgColor := Value;
  PropsChanged(Self);
end;

{$endregion TJppCustomMemoAppearance}


{$region ' -------------------- TJppFlashMemo ---------------------- '}

constructor TJppFlashMemo.Create(Edit: TJppCustomMemo);
begin
  inherited Create(Edit);
  FEdit := Edit;
  Enabled := True;
  FlashCount := 3;
  FlashInterval := 150;

  OnFlashColorChanged := FlashColorChanged;
  OnFlashFinished := FlashFinished;
  FreeOnFlashFinished := False;

  if FEdit is TJppCustomMemo then
    with (FEdit as TJppCustomMemo) do
    begin
      OriginalColor := Color;
    end;
end;

procedure TJppFlashMemo.FlashColorChanged(Sender: TObject; const AColor: TColor);
begin
  if FEdit is TJppCustomMemo then
    (FEdit as TJppCustomMemo).Color := AColor;
end;

procedure TJppFlashMemo.FlashFinished(Sender: TObject);
begin
  if FEdit is TJppCustomMemo then
    with (FEdit as TJppCustomMemo) do
    begin
      Color := OriginalColor;
    end;
end;



{$endregion TJppFlashMemo}

end.



