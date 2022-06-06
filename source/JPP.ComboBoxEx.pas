unit JPP.ComboBoxEx;

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
  Controls, Graphics, StdCtrls, ExtCtrls, ComCtrls,
  {$IFDEF FPC}ComboEx, LCLType, LCLIntf, LMessages,{$ENDIF}
  JPP.Common, JPP.Common.Procs, JPP.AnchoredControls, JPP.Flash
  ;


type

  TJppCustomComboBoxEx = class;

  {$Region ' --- TJppFlashJppComboBoxEx --- '}
  TJppFlashJppComboBoxEx = class(TJppFlashBase)
  private
    FEdit: TJppCustomComboBoxEx;
  protected
    procedure FlashColorChanged(Sender: TObject; const AColor: TColor);
    procedure FlashFinished(Sender: TObject);
  public
    constructor Create(Edit: TJppCustomComboBoxEx);
  published
    property Enabled;
    property FlashColor;
    property FlashCount;
    property FlashInterval;
    property OnFlashFinished;
  end;
  {$endregion TJppFlashJppComboBoxEx}


  {$region ' --- TJppCustomComboBoxEx --- '}
  TJppCustomComboBoxEx = class(TCustomComboBoxEx)
  private
    FBoundLabel: TJppControlBoundLabel;
    FMouseOverControl: Boolean;
    {$IFDEF MSWINDOWS} FTabOnEnter: Boolean; {$ENDIF}
    FTagExt: TJppTagExt;
    FShowLabel: Boolean;
    FFlash: TJppFlashJppComboBoxEx;
    FBoundLabelSpacing: Integer;
    FBoundLabelPosition: TLabelPosition;
    FAnchoredControls: TJppAnchoredControls;
    procedure SetTagExt(const Value: TJppTagExt);
    procedure SetShowLabel(const Value: Boolean);
    procedure CMMouseEnter (var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave (var Message: TMessage); message CM_MOUSELEAVE;
    procedure SetFlash(const Value: TJppFlashJppComboBoxEx);
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

    procedure PropsChanged(Sender: TObject);
    procedure Loaded; override;

    property MouseOverControl: Boolean read FMouseOverControl;

    {$IFDEF DCC}function GetItemHt: Integer; override;{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure FlashBackground;
    procedure SetupInternalLabel;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer; AHeight: Integer); override;

    {$IFDEF MSWINDOWS} procedure KeyPress(var Key: Char); override; {$ENDIF}
  protected
    property BoundLabel: TJppControlBoundLabel read FBoundLabel;
    property BoundLabelPosition: TLabelPosition read FBoundLabelPosition write SetBoundLabelPosition default lpLeft;
    property BoundLabelSpacing: Integer read FBoundLabelSpacing write SetBoundLabelSpacing default 4;

    {$IFDEF MSWINDOWS}
    property TabOnEnter: Boolean read FTabOnEnter write FTabOnEnter default False; // if True, after pressing the Enter key, the focus is moved to the next control
    {$ENDIF}
    property TagExt: TJppTagExt read FTagExt write SetTagExt;
    property ShowLabel: Boolean read FShowLabel write SetShowLabel default True;
    property Flash: TJppFlashJppComboBoxEx read FFlash write SetFlash;
    property AnchoredControls: TJppAnchoredControls read FAnchoredControls write SetAnchoredControls;
  end;
  {$endregion TJppCustomComboBoxEx}


  {$region ' --- TJppComboBoxEx --- '}
  TJppComboBoxEx = class(TJppCustomComboBoxEx)
  public
    property MouseOverControl;
  published

    property Style;
    property StyleEx;

    property Action;
    property Align;
    {$IFDEF DCC}property AlignWithMargins;{$ENDIF}
    property Anchors;
    property AutoCompleteOptions default [acoAutoAppend];
    {$IFDEF FPC}property ArrowKeysTraverseList;{$ENDIF}
//    property AutoComplete;
    {$IFDEF FPC}property AutoCompleteText;{$ENDIF}
//    {$IFDEF DCC}property AutoCloseUp;{$ENDIF}
//    {$IFDEF DCC}property AutoCompleteDelay;{$ENDIF}
//    property AutoDropDown;
//    property AutoSize;
    {$IFDEF DCC}
    property BevelEdges;
    property BevelInner;
    property BevelKind;
    property BevelOuter;
    {$ENDIF}
    property BiDiMode;
    {$IFDEF FPC}property BorderSpacing;{$ENDIF}
    {$IFDEF FPC}property BorderStyle;{$ENDIF}
//    property CharCase;
    property Color;
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
    property Images;
    {$IFDEF DCC}
    property ImeMode;
    property ImeName;
    {$ENDIF}
    property ItemHeight;
    property ItemIndex;
    property Items;
    property ItemsEx;
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
//    property Sorted;

    {$IFDEF DCC}{$IF RTLVersion > 23}property StyleElements;{$IFEND}{$ENDIF}
    property TabOrder;
    property TabStop;
    property Tag;
    property Text;
//    {$IFDEF DCC}property TextHint;{$ENDIF}
    property Top;
    {$IFDEF DELPHI2010_OR_ABOVE} property Touch; {$ENDIF}
    property Visible;
    property Width;

    // -------- Events ----------
    {$IFDEF DCC}property OnBeginEdit;{$ENDIF}
    property OnChange;
    {$IFDEF FPC}property OnChangeBounds;{$ENDIF}
    property OnClick;
//    property OnCloseUp;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
//    property OnDrawItem;
    property OnDropDown;
    {$IFDEF FPC}property OnEditingDone;{$ENDIF}
    property OnEndDock;
    property OnEndDrag;
    {$IFDEF DCC}property OnEndEdit;{$ENDIF}
    property OnEnter;
    property OnExit;
    {$IFDEF DELPHI2010_OR_ABOVE}property OnGesture;{$ENDIF}
    {$IFDEF FPC}property OnGetItems;{$ENDIF}
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
//    property OnMeasureItem;
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
  {$endregion TJppComboBoxEx}


implementation


{$region ' --------------------------------- TJppCustomComboBoxEx ------------------------------- '}

constructor TJppCustomComboBoxEx.Create(AOwner: TComponent);
begin
  inherited;

  FBoundLabelPosition := lpLeft;
  FBoundLabelSpacing := 4;
  SetupInternalLabel;

  FFlash := TJppFlashJppComboBoxEx.Create(Self);

  FTagExt := TJppTagExt.Create(Self);
  FShowLabel := True;
  FMouseOverControl := False;

  {$IFDEF MSWINDOWS}FTabOnEnter := False;{$ENDIF}
  DropDownCount := 16;

  FAnchoredControls := TJppAnchoredControls.Create(Self);
end;

destructor TJppCustomComboBoxEx.Destroy;
begin
  FTagExt.Free;
  FFlash.Free;
  FAnchoredControls.Free;
  inherited;
end;

procedure TJppCustomComboBoxEx.Loaded;
begin
  inherited;
end;

procedure TJppCustomComboBoxEx.SetAnchoredControls(const Value: TJppAnchoredControls);
begin
  FAnchoredControls := Value;
end;

procedure TJppCustomComboBoxEx.Notification(AComponent: TComponent; Operation: TOperation);
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

procedure TJppCustomComboBoxEx.PropsChanged(Sender: TObject);
begin
  if csLoading in ComponentState then Exit;
end;

procedure TJppCustomComboBoxEx.CMBiDiModeChanged(var Message: TMessage);
begin
  inherited;
  if FBoundLabel <> nil then FBoundLabel.BiDiMode := BiDiMode;
end;

procedure TJppCustomComboBoxEx.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if FBoundLabel <> nil then FBoundLabel.Enabled := Enabled;
  //ApplyAppearance;
end;

procedure TJppCustomComboBoxEx.CMMouseEnter(var Message: TMessage);
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

procedure TJppCustomComboBoxEx.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  FMouseOverControl := False;
  //ApplyAppearance;
end;

procedure TJppCustomComboBoxEx.CMVisibleChanged(var Message: TMessage);
begin
  inherited;
  if FBoundLabel <> nil then FBoundLabel.Visible := Visible;
end;

procedure TJppCustomComboBoxEx.DoEnter;
begin
  inherited;
end;

procedure TJppCustomComboBoxEx.DoExit;
begin
  inherited DoExit;
end;

procedure TJppCustomComboBoxEx.FlashBackground;
begin
  FFlash.OriginalColor := Color;
  FFlash.Flash;
end;

{$IFDEF DCC}
function TJppCustomComboBoxEx.GetItemHt: Integer;
begin
  Result := Perform(CB_GETITEMHEIGHT, 0, 0); // returns 0 if Handle = 0
end;
{$ENDIF}

procedure TJppCustomComboBoxEx.AdjustLabelBounds(Sender: TObject);
begin
  SetBoundLabelPosition(FBoundLabelPosition);
end;

procedure TJppCustomComboBoxEx.SetBoundLabelPosition(const Value: TLabelPosition);
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

procedure TJppCustomComboBoxEx.SetBoundLabelSpacing(const Value: Integer);
begin
  FBoundLabelSpacing := Value;
  SetBoundLabelPosition(FBoundLabelPosition);
end;

procedure TJppCustomComboBoxEx.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  SetBoundLabelPosition(FBoundLabelPosition);
  if not (csDestroying in ComponentState) then
    if Assigned(FAnchoredControls) then FAnchoredControls.UpdateAllControlsPos;
end;

procedure TJppCustomComboBoxEx.SetFlash(const Value: TJppFlashJppComboBoxEx);
begin
  FFlash := Value;
end;

procedure TJppCustomComboBoxEx.SetParent(AParent: TWinControl);
begin
  inherited;
  if FBoundLabel <> nil then
  begin
    FBoundLabel.Parent := AParent;
    FBoundLabel.Visible := True;
  end;
end;

procedure TJppCustomComboBoxEx.SetShowLabel(const Value: Boolean);
begin
  if FBoundLabel.Visible = Value then Exit;
  FShowLabel := Value;
  FBoundLabel.Visible := FShowLabel;
end;

procedure TJppCustomComboBoxEx.SetTagExt(const Value: TJppTagExt);
begin
  FTagExt := Value;
end;

procedure TJppCustomComboBoxEx.SetupInternalLabel;
begin
  if Assigned(FBoundLabel) then Exit;
  FBoundLabel := TJppControlBoundLabel.Create(Self);
  FBoundLabel.FreeNotification(Self);
  FBoundLabel.OnAdjustBounds := AdjustLabelBounds;
  FBoundLabel.FocusControl := Self;
  {$IFDEF FPC}FBoundLabel.OnChangeBounds := AdjustLabelBounds;{$ENDIF}
end;

{$IFDEF MSWINDOWS}
procedure TJppCustomComboBoxEx.KeyPress(var Key: Char);
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


{$endregion TJppCustomComboBoxEx}



{$region ' -------------------- TJppFlashJppComboBoxEx ---------------------- '}

constructor TJppFlashJppComboBoxEx.Create(Edit: TJppCustomComboBoxEx);
begin
  inherited Create(Edit);
  FEdit := Edit;
  Enabled := True;
  FlashCount := 3;
  FlashInterval := 150;

  OnFlashColorChanged := FlashColorChanged;
  OnFlashFinished := FlashFinished;
  FreeOnFlashFinished := False;

  if FEdit is TJppCustomComboBoxEx then
    with (FEdit as TJppCustomComboBoxEx) do
    begin
      OriginalColor := Color;
    end;
end;

procedure TJppFlashJppComboBoxEx.FlashColorChanged(Sender: TObject; const AColor: TColor);
begin
  if FEdit is TJppCustomComboBoxEx then
    (FEdit as TJppCustomComboBoxEx).Color := AColor;
end;

procedure TJppFlashJppComboBoxEx.FlashFinished(Sender: TObject);
begin
  if FEdit is TJppCustomComboBoxEx then
    with (FEdit as TJppCustomComboBoxEx) do
    begin
      Color := OriginalColor;
    end;
end;



{$endregion TJppFlashJppComboBoxEx}

end.



