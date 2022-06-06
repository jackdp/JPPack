unit JPP.ComboBox;

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
  JPP.Common, JPP.Common.Procs, JPP.AnchoredControls, JPP.Flash
  ;


type

  TJppCustomComboBox = class;

  {$Region ' --- TJppFlashJppComboBox --- '}
  TJppFlashJppComboBox = class(TJppFlashBase)
  private
    FEdit: TJppCustomComboBox;
  protected
    procedure FlashColorChanged(Sender: TObject; const AColor: TColor);
    procedure FlashFinished(Sender: TObject);
  public
    constructor Create(Edit: TJppCustomComboBox);
  published
    property Enabled;
    property FlashColor;
    property FlashCount;
    property FlashInterval;
    property OnFlashFinished;
  end;
  {$endregion TJppFlashJppComboBox}


  {$region ' --- TJppCustomComboBox --- '}
  TJppCustomComboBox = class(TCustomComboBox)
  private
    FBoundLabel: TJppControlBoundLabel;
    FMouseOverControl: Boolean;
    {$IFDEF MSWINDOWS} FTabOnEnter: Boolean; {$ENDIF}
    FTagExt: TJppTagExt;
    FShowLabel: Boolean;
    FFlash: TJppFlashJppComboBox;
    FBoundLabelSpacing: Integer;
    FBoundLabelPosition: TLabelPosition;
    FAnchoredControls: TJppAnchoredControls;
    FColorEnabled: TColor;
    FColorDisabled: TColor;
    procedure SetTagExt(const Value: TJppTagExt);
    procedure SetShowLabel(const Value: Boolean);
    procedure CMMouseEnter (var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave (var Message: TMessage); message CM_MOUSELEAVE;
    procedure SetFlash(const Value: TJppFlashJppComboBox);
    procedure SetBoundLabelPosition(const Value: TLabelPosition);
    procedure SetBoundLabelSpacing(const Value: Integer);
    procedure AdjustLabelBounds(Sender: TObject);
    procedure SetAnchoredControls(const Value: TJppAnchoredControls);
    procedure SetColorEnabled(const Value: TColor);
    procedure SetColorDisabled(const Value: TColor);
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
    property MouseOverControl: Boolean read FMouseOverControl;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure FlashBackground;
    procedure SetupInternalLabel;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer; AHeight: Integer); override;
    procedure UpdateLabelPosition;
  protected
    property BoundLabel: TJppControlBoundLabel read FBoundLabel;
    property BoundLabelPosition: TLabelPosition read FBoundLabelPosition write SetBoundLabelPosition default lpLeft;
    property BoundLabelSpacing: Integer read FBoundLabelSpacing write SetBoundLabelSpacing default 4;

    {$IFDEF MSWINDOWS}
    property TabOnEnter: Boolean read FTabOnEnter write FTabOnEnter default False; // if True, after pressing the Enter key, the focus is moved to the next control
    {$ENDIF}
    property TagExt: TJppTagExt read FTagExt write SetTagExt;
    property ShowLabel: Boolean read FShowLabel write SetShowLabel default True;
    property Flash: TJppFlashJppComboBox read FFlash write SetFlash;
    property AnchoredControls: TJppAnchoredControls read FAnchoredControls write SetAnchoredControls;
    property ColorEnabled: TColor read FColorEnabled write SetColorEnabled default clWindow;
    property ColorDisabled: TColor read FColorDisabled write SetColorDisabled;
  end;
  {$endregion TJppCustomComboBox}


  {$region ' --- TJppComboBox --- '}
  TJppComboBox = class(TJppCustomComboBox)
  public
    property MouseOverControl;
  published
    property Align;
    {$IFDEF DCC}property AlignWithMargins;{$ENDIF}
    property Anchors;
    {$IFDEF FPC}property ArrowKeysTraverseList;{$ENDIF}
    property AutoComplete;
    {$IFDEF FPC}property AutoCompleteText;{$ENDIF}
    {$IFDEF DCC}property AutoCloseUp;{$ENDIF}
    {$IFDEF DCC}property AutoCompleteDelay;{$ENDIF}
    property AutoDropDown;
    property AutoSize;
    {$IFDEF DCC}
    property BevelEdges;
    property BevelInner;
    property BevelKind;
    property BevelOuter;
    {$ENDIF}
    property BiDiMode;
    {$IFDEF FPC}property BorderSpacing;{$ENDIF}
    {$IFDEF FPC}property BorderStyle;{$ENDIF}
    property CharCase;
    //property Color;
    property ColorEnabled;
    property ColorDisabled;
    property Constraints;
    {$IFDEF DCC} property Ctl3D; {$ENDIF}
    property Cursor;
    {$IFDEF DCC}property CustomHint;{$ENDIF}
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownCount default 16;
    property Enabled;
    property Font;
    property Height;
    property HelpContext;
    property HelpKeyword;
    property HelpType;
    property Hint;
    {$IFDEF DCC}
    property ImeMode;
    property ImeName;
    {$ENDIF}
    property ItemHeight;
    property ItemIndex;
    property Items;
    {$IFDEF FPC}property ItemWidth;{$ENDIF}
    property Left;
    {$IFDEF DCC}property Margins;{$ENDIF}
    property MaxLength;
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
    property ShowHint;
    property Sorted;
    property Style;
    {$IFDEF DCC}{$IF RTLVersion > 23}property StyleElements;{$IFEND}{$ENDIF}
    property TabOrder;
    property TabStop;
    property Tag;
    property Text;
    {$IFDEF DCC}property TextHint;{$ENDIF}
    property Top;
    {$IFDEF DELPHI2010_OR_ABOVE} property Touch; {$ENDIF}
    property Visible;
    property Width;

    // -------- Events ----------
    property OnChange;
    {$IFDEF FPC}property OnChangeBounds;{$ENDIF}
    property OnClick;
    property OnCloseUp;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    {$IFDEF FPC}property OnEditingDone;{$ENDIF}
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    {$IFDEF DELPHI2010_OR_ABOVE}property OnGesture;{$ENDIF}
    {$IFDEF FPC}property OnGetItems;{$ENDIF}
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
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
    property OnSelect;
    property OnStartDock;
    property OnStartDrag;
    {$IFDEF FPC}property OnUTF8KeyPress;{$ENDIF}
    {$IFDEF MSWINDOWS} property TabOnEnter; {$ENDIF}

    // ------- Custom Properties ---------
    property ShowLabel;
    property Flash;
    property TagExt;

    property BoundLabel;
    property BoundLabelPosition;
    property BoundLabelSpacing;

    property AnchoredControls;
  end;
  {$endregion TJppComboBox}


implementation


{$region ' --------------------------------- TJppCustomComboBox ------------------------------- '}

constructor TJppCustomComboBox.Create(AOwner: TComponent);
begin
  inherited;

  FBoundLabelPosition := lpLeft;
  FBoundLabelSpacing := 4;
  SetupInternalLabel;

  FFlash := TJppFlashJppComboBox.Create(Self);

  FTagExt := TJppTagExt.Create(Self);
  FShowLabel := True;
  FMouseOverControl := False;

  {$IFDEF MSWINDOWS}FTabOnEnter := False;{$ENDIF}
  DropDownCount := 16;

  FAnchoredControls := TJppAnchoredControls.Create(Self);

  FColorEnabled := clWindow;
  FColorDisabled := $00F0F0F0;
end;

destructor TJppCustomComboBox.Destroy;
begin
  FTagExt.Free;
  FFlash.Free;
  FAnchoredControls.Free;
  inherited;
end;

procedure TJppCustomComboBox.Loaded;
begin
  inherited;
end;

procedure TJppCustomComboBox.SetAnchoredControls(const Value: TJppAnchoredControls);
begin
  FAnchoredControls := Value;
end;

procedure TJppCustomComboBox.Notification(AComponent: TComponent; Operation: TOperation);
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

procedure TJppCustomComboBox.SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer; AHeight: Integer);
begin
  inherited;
  SetBoundLabelPosition(FBoundLabelPosition);
  if not (csDestroying in ComponentState) then
    if Assigned(FAnchoredControls) then FAnchoredControls.UpdateAllControlsPos;
end;

procedure TJppCustomComboBox.UpdateLabelPosition;
begin
  SetBoundLabelPosition(FBoundLabelPosition);
end;

procedure TJppCustomComboBox.SetColorDisabled(const Value: TColor);
begin
  if FColorDisabled = Value then Exit;
  FColorDisabled := Value;
  if not Enabled then Invalidate;
end;

procedure TJppCustomComboBox.SetColorEnabled(const Value: TColor);
begin
  if FColorEnabled = Value then Exit;
  FColorEnabled := Value;
  if Enabled then Invalidate;
end;

procedure TJppCustomComboBox.PropsChanged(Sender: TObject);
begin
  if csLoading in ComponentState then Exit;
end;

procedure TJppCustomComboBox.CMBiDiModeChanged(var Message: TMessage);
begin
  inherited;
  if FBoundLabel <> nil then FBoundLabel.BiDiMode := BiDiMode;
end;

procedure TJppCustomComboBox.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if FBoundLabel <> nil then FBoundLabel.Enabled := Enabled;
  if Enabled then Color := FColorEnabled else Color := FColorDisabled;
end;

procedure TJppCustomComboBox.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  {$IFDEF MSWINDOWS}
  if (GetActiveWindow <> 0) then
  {$ENDIF}
  begin
    FMouseOverControl := True;
    //ApplyAppearance;
  end;
end;

procedure TJppCustomComboBox.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  FMouseOverControl := False;
  //ApplyAppearance;
end;

procedure TJppCustomComboBox.CMVisibleChanged(var Message: TMessage);
begin
  inherited;
  if FBoundLabel <> nil then FBoundLabel.Visible := Visible;
end;


procedure TJppCustomComboBox.DoEnter;
begin
  inherited;
end;

procedure TJppCustomComboBox.DoExit;
begin
  inherited DoExit;
end;

procedure TJppCustomComboBox.FlashBackground;
begin
  FFlash.OriginalColor := Color;
  FFlash.Flash;
end;

procedure TJppCustomComboBox.AdjustLabelBounds(Sender: TObject);
begin
  SetBoundLabelPosition(FBoundLabelPosition);
end;

procedure TJppCustomComboBox.SetBoundLabelPosition(const Value: TLabelPosition);
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

procedure TJppCustomComboBox.SetBoundLabelSpacing(const Value: Integer);
begin
  FBoundLabelSpacing := Value;
  SetBoundLabelPosition(FBoundLabelPosition);
end;

procedure TJppCustomComboBox.SetFlash(const Value: TJppFlashJppComboBox);
begin
  FFlash := Value;
end;

procedure TJppCustomComboBox.SetParent(AParent: TWinControl);
begin
  inherited;
  if FBoundLabel <> nil then
  begin
    FBoundLabel.Parent := AParent;
    FBoundLabel.Visible := True;
  end;
end;

procedure TJppCustomComboBox.SetShowLabel(const Value: Boolean);
begin
  if FBoundLabel.Visible = Value then Exit;
  FShowLabel := Value;
  FBoundLabel.Visible := FShowLabel;
end;

procedure TJppCustomComboBox.SetTagExt(const Value: TJppTagExt);
begin
  FTagExt := Value;
end;

procedure TJppCustomComboBox.SetupInternalLabel;
begin
  if Assigned(FBoundLabel) then Exit;
  FBoundLabel := TJppControlBoundLabel.Create(Self);
  FBoundLabel.FreeNotification(Self);
  FBoundLabel.OnAdjustBounds := AdjustLabelBounds;
  FBoundLabel.FocusControl := Self;
  {$IFDEF FPC}FBoundLabel.OnChangeBounds := AdjustLabelBounds;{$ENDIF}
end;

{$IFDEF MSWINDOWS}
procedure TJppCustomComboBox.KeyPress(var Key: Char);
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


{$endregion TJppCustomComboBox}



{$region ' -------------------- TJppFlashJppComboBox ---------------------- '}

constructor TJppFlashJppComboBox.Create(Edit: TJppCustomComboBox);
begin
  inherited Create(Edit);
  FEdit := Edit;
  Enabled := True;
  FlashCount := 3;
  FlashInterval := 150;

  OnFlashColorChanged := FlashColorChanged;
  OnFlashFinished := FlashFinished;
  FreeOnFlashFinished := False;

  if FEdit is TJppCustomComboBox then
    with (FEdit as TJppCustomComboBox) do
    begin
      OriginalColor := Color;
    end;
end;

procedure TJppFlashJppComboBox.FlashColorChanged(Sender: TObject; const AColor: TColor);
begin
  if FEdit is TJppCustomComboBox then
    (FEdit as TJppCustomComboBox).Color := AColor;
end;

procedure TJppFlashJppComboBox.FlashFinished(Sender: TObject);
begin
  if FEdit is TJppCustomComboBox then
    with (FEdit as TJppCustomComboBox) do
    begin
      Color := OriginalColor;
    end;
end;



{$endregion TJppFlashJppComboBox}

end.



