unit JPP.Edit;

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

  {$region ' --- TJppCustomEditAppearance --- '}
  TJppCustomEditAppearance = class(TJppPersistent)
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
    procedure Assign(Source: TJppCustomEditAppearance); reintroduce;
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
  {$endregion TJppCustomEditAppearance}

  TJppCustomEdit = class;

  {$Region ' --- TJppFlashJppEdit --- '}
  TJppFlashJppEdit = class(TJppFlashBase) // nazwa TJppFlashEdit ju¿ jest zajêta
  private
    FEdit: TJppCustomEdit;
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
    constructor Create(Edit: TJppCustomEdit);
  published
    property Enabled;
    property FlashColor;
    property FlashCount;
    property FlashInterval;
    property OnFlashFinished;
  end;
  {$endregion TJppFlashJppEdit}


  {$region ' --- TJppCustomEdit --- '}
  TJppCustomEdit = class(TCustomEdit)
  private
    FBoundLabel: TJppControlBoundLabel;
    FMouseOverControl: Boolean;
    {$IFDEF MSWINDOWS} FTabOnEnter: Boolean; {$ENDIF}
    FTagExt: TJppTagExt;
    FShowLabel: Boolean;
    FAppearance: TJppCustomEditAppearance;
    FFlash: TJppFlashJppEdit;
    FBoundLabelSpacing: Integer;
    FBoundLabelPosition: TLabelPosition;
    FAnchoredControls: TJppAnchoredControls;
    procedure SetTagExt(const Value: TJppTagExt);
    procedure SetShowLabel(const Value: Boolean);
    procedure CMMouseEnter (var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave (var Message: TMessage); message CM_MOUSELEAVE;
    procedure SetAppearance(const Value: TJppCustomEditAppearance);
    procedure SetFlash(const Value: TJppFlashJppEdit);
    procedure SetBoundLabelPosition(const Value: TLabelPosition);
    procedure SetBoundLabelSpacing(const Value: Integer);
    procedure AdjustLabelBounds(Sender: TObject);
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

    property Appearance: TJppCustomEditAppearance read FAppearance write SetAppearance;
    {$IFDEF MSWINDOWS}
    property TabOnEnter: Boolean read FTabOnEnter write FTabOnEnter; // if True, after pressing the Enter key, the focus is moved to the next control
    {$ENDIF}
    property TagExt: TJppTagExt read FTagExt write SetTagExt;
    property ShowLabel: Boolean read FShowLabel write SetShowLabel default True;
    property Flash: TJppFlashJppEdit read FFlash write SetFlash;

    property AnchoredControls: TJppAnchoredControls read FAnchoredControls write SetAnchoredControls;
  end;
  {$endregion TJppCustomEdit}


  {$region ' --- TJppEdit --- '}
  TJppEdit = class(TJppCustomEdit)
  public
    property MouseOverControl;
  published
    property Align;
    property Alignment;
    property Anchors;
    property AutoSelect;
    property AutoSize;
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
    property Cursor;
    {$IFDEF DCC} property Ctl3D; {$ENDIF}
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
    property MaxLength;
    {$IFDEF DCC}property OEMConvert;{$ENDIF}
    property NumbersOnly;
    property ParentBiDiMode;
    property ParentColor;
    {$IFDEF DCC} property ParentCtl3D; {$ENDIF}
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
    {$IFDEF DELPHI2010_OR_ABOVE}property Touch;{$ENDIF}
    property Visible;
    property Width;

    // --------- Events ----------
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
    {$IFDEF FPC}
    property EchoMode;
    property OnUTF8KeyPress;
    {$ENDIF}

    // --------- Custom Properties ---------
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
  {$endregion TJppEdit}


implementation


{$region ' --------------------------------- TJppCustomEdit ------------------------------- '}

constructor TJppCustomEdit.Create(AOwner: TComponent);
begin
  inherited;

  FBoundLabelPosition := lpLeft;
  FBoundLabelSpacing := 4;
  SetupInternalLabel;

  FAppearance := TJppCustomEditAppearance.Create(AOwner);
  FAppearance.OnChange := PropsChanged;

  FFlash := TJppFlashJppEdit.Create(Self);

  FTagExt := TJppTagExt.Create(Self);
  FShowLabel := True;
  FMouseOverControl := False;

  FAnchoredControls := TJppAnchoredControls.Create(Self);
end;

destructor TJppCustomEdit.Destroy;
begin
  FAppearance.Free;
  FTagExt.Free;
  FFlash.Free;
  FAnchoredControls.Free;
  inherited;
end;

procedure TJppCustomEdit.Loaded;
begin
  inherited;
  Color := FAppearance.NormalBgColor;
  Font.Color := FAppearance.NormalTextColor;
  //EditLabel.Visible := FShowLabel;
end;

procedure TJppCustomEdit.SetAnchoredControls(const Value: TJppAnchoredControls);
begin
  FAnchoredControls := Value;
end;

procedure TJppCustomEdit.Notification(AComponent: TComponent; Operation: TOperation);
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

procedure TJppCustomEdit.PropsChanged(Sender: TObject);
begin
  if csLoading in ComponentState then Exit;
  ApplyAppearance;
end;

procedure TJppCustomEdit.ApplyAppearance;
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

procedure TJppCustomEdit.ApplyFontColor(const TextColor: TColor; Normal, Hot, Focused, Disabled: Boolean);
begin
  Font.Color := TextColor;
  if Normal then FAppearance.NormalTextColor := TextColor;
  if Hot then FAppearance.HotTextColor := TextColor;
  if Focused then FAppearance.FocusedTextColor := TextColor;
  if Disabled then FAppearance.DisabledTextColor := TextColor;
end;

procedure TJppCustomEdit.CMBiDiModeChanged(var Message: TMessage);
begin
  inherited;
  if FBoundLabel <> nil then FBoundLabel.BiDiMode := BiDiMode;
end;

procedure TJppCustomEdit.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if FBoundLabel <> nil then FBoundLabel.Enabled := Enabled;
  ApplyAppearance;
end;

procedure TJppCustomEdit.CMMouseEnter(var Message: TMessage);
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

procedure TJppCustomEdit.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  FMouseOverControl := False;
  ApplyAppearance;
end;

procedure TJppCustomEdit.CMVisibleChanged(var Message: TMessage);
begin
  inherited;
  if FBoundLabel <> nil then FBoundLabel.Visible := Visible;
end;

procedure TJppCustomEdit.DoEnter;
begin
  Color := FAppearance.FocusedBgColor;
  Font.Color := FAppearance.FocusedTextColor;
  inherited;
end;

procedure TJppCustomEdit.DoExit;
begin
  Color := FAppearance.NormalBgColor;
  Font.Color := FAppearance.NormalTextColor;
  inherited DoExit;
end;

procedure TJppCustomEdit.FlashBackground;
begin
  FFlash.OriginalNormalBgColor := FAppearance.NormalBgColor;
  FFlash.OriginalHotBgColor := FAppearance.HotBgColor;
  FFlash.OriginalFocusedBgColor := FAppearance.FocusedBgColor;
  FFlash.OriginalColor := FAppearance.NormalBgColor;
  FFlash.Flash;
end;

procedure TJppCustomEdit.SetAppearance(const Value: TJppCustomEditAppearance);
begin
  FAppearance := Value;
  PropsChanged(Self);
end;

procedure TJppCustomEdit.AdjustLabelBounds(Sender: TObject);
begin
  SetBoundLabelPosition(FBoundLabelPosition);
end;

procedure TJppCustomEdit.SetBoundLabelPosition(const Value: TLabelPosition);
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
  FBoundLabel.Visible := FShowLabel;
end;

procedure TJppCustomEdit.SetBoundLabelSpacing(const Value: Integer);
begin
  FBoundLabelSpacing := Value;
  SetBoundLabelPosition(FBoundLabelPosition);
end;

procedure TJppCustomEdit.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  SetBoundLabelPosition(FBoundLabelPosition);
  if not (csDestroying in ComponentState) then
    if Assigned(FAnchoredControls) then FAnchoredControls.UpdateAllControlsPos;
end;

procedure TJppCustomEdit.SetFlash(const Value: TJppFlashJppEdit);
begin
  FFlash := Value;
end;

procedure TJppCustomEdit.SetParent(AParent: TWinControl);
begin
  inherited;
  if FBoundLabel <> nil then
  begin
    FBoundLabel.Parent := AParent;
    FBoundLabel.Visible := True;
  end;
end;

procedure TJppCustomEdit.SetShowLabel(const Value: Boolean);
begin
  if FBoundLabel.Visible = Value then Exit;
  FShowLabel := Value;
  FBoundLabel.Visible := FShowLabel;
end;

procedure TJppCustomEdit.SetTagExt(const Value: TJppTagExt);
begin
  FTagExt := Value;
end;

procedure TJppCustomEdit.SetupInternalLabel;
begin
  if Assigned(FBoundLabel) then Exit;
  FBoundLabel := TJppControlBoundLabel.Create(Self);
  FBoundLabel.FreeNotification(Self);
  FBoundLabel.OnAdjustBounds := AdjustLabelBounds;
  FBoundLabel.FocusControl := Self;
  {$IFDEF FPC}FBoundLabel.OnChangeBounds := AdjustLabelBounds;{$ENDIF}
end;


{$IFDEF MSWINDOWS}
procedure TJppCustomEdit.KeyPress(var Key: Char);
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


{$endregion TJppCustomEdit}


{$region ' ---------------------- TJppCustomEditAppearance ----------------------- '}

constructor TJppCustomEditAppearance.Create(AOwner: TComponent);
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

destructor TJppCustomEditAppearance.Destroy;
begin
  inherited;
end;

procedure TJppCustomEditAppearance.Assign(Source: TJppCustomEditAppearance);
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

procedure TJppCustomEditAppearance.SetNormalTextColor(const Value: TColor);
begin
  if FNormalTextColor = Value then Exit;
  FNormalTextColor := Value;
  PropsChanged(Self);
end;

procedure TJppCustomEditAppearance.SetDisabledBgColor(const Value: TColor);
begin
  if FDisabledBgColor = Value then Exit;
  FDisabledBgColor := Value;
  PropsChanged(Self);
end;

procedure TJppCustomEditAppearance.SetDisabledTextColor(const Value: TColor);
begin
  if FDisabledTextColor = Value then Exit;
  FDisabledTextColor := Value;
  PropsChanged(Self);
end;

procedure TJppCustomEditAppearance.SetFocusedBgColor(const Value: TColor);
begin
  if FFocusedBgColor = Value then Exit;
  FFocusedBgColor := Value;
  PropsChanged(Self);
end;

procedure TJppCustomEditAppearance.SetFocusedTextColor(const Value: TColor);
begin
  if FFocusedTextColor = Value then Exit;
  FFocusedTextColor := Value;
  PropsChanged(Self);
end;

procedure TJppCustomEditAppearance.SetHotBgColor(const Value: TColor);
begin
  if FHotBgColor = Value then Exit;
  FHotBgColor := Value;
  PropsChanged(Self);
end;

procedure TJppCustomEditAppearance.SetHotTextColor(const Value: TColor);
begin
  if FHotTextColor = Value then Exit;
  FHotTextColor := Value;
  PropsChanged(Self);
end;

procedure TJppCustomEditAppearance.SetNormalBgColor(const Value: TColor);
begin
  if FNormalBgColor = Value then Exit;
  FNormalBgColor := Value;
  PropsChanged(Self);
end;

{$endregion TJppCustomEditAppearance}



{$region ' -------------------- TJppFlashJppEdit ---------------------- '}

constructor TJppFlashJppEdit.Create(Edit: TJppCustomEdit);
begin
  inherited Create(Edit);
  FEdit := Edit;
  Enabled := True;
  FlashCount := 3;
  FlashInterval := 150;

  OnFlashColorChanged := FlashColorChanged;
  OnFlashFinished := FlashFinished;
  FreeOnFlashFinished := False;

  if FEdit is TJppCustomEdit then
    with (FEdit as TJppCustomEdit).Appearance do
    begin
      FOriginalNormalBgColor := NormalBgColor;
      FOriginalHotBgColor := HotBgColor;
      FOriginalFocusedBgColor := FocusedBgColor;
    end;
end;

procedure TJppFlashJppEdit.FlashColorChanged(Sender: TObject; const AColor: TColor);
begin
  if FEdit is TJppCustomEdit then
    with (FEdit as TJppCustomEdit) do
    begin
      Appearance.NormalBgColor := AColor;
      Appearance.HotBgColor := AColor;
      Appearance.FocusedBgColor := AColor;
      ApplyAppearance;
    end;
end;

procedure TJppFlashJppEdit.FlashFinished(Sender: TObject);
begin
  if FEdit is TJppCustomEdit then
    with (FEdit as TJppCustomEdit) do
    begin
      Appearance.NormalBgColor := FOriginalNormalBgColor;
      Appearance.HotBgColor := FOriginalHotBgColor;
      Appearance.FocusedBgColor := FOriginalFocusedBgColor;
      ApplyAppearance;
    end;
end;


procedure TJppFlashJppEdit.SetOriginalFocusedBgColor(const Value: TColor);
begin
  FOriginalFocusedBgColor := Value;
end;

procedure TJppFlashJppEdit.SetOriginalHotBgColor(const Value: TColor);
begin
  FOriginalHotBgColor := Value;
end;

procedure TJppFlashJppEdit.SetOriginalNormalBgColor(const Value: TColor);
begin
  FOriginalNormalBgColor := Value;
end;

{$endregion TJppFlashJppEdit}

end.



