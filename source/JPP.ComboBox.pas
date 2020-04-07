unit JPP.ComboBox;

{
  Jacek Pazera
  http://www.pazera-software.com
  https://github.com/jackdp
}

{$IFDEF FPC} {$mode delphi} {$ENDIF}
{$I JPPack.inc}

interface


uses
  {$IFDEF MSWINDOWS}Windows,{$ENDIF}
  {$IFDEF DCC}
  Winapi.Messages,
  System.SysUtils, System.Classes, System.Types, System.UITypes,
  Vcl.Controls, Vcl.Graphics, Vcl.StdCtrls, Vcl.ExtCtrls,
  {$ELSE}
  SysUtils, Classes, Types, Controls, Graphics, StdCtrls, ExtCtrls, LCLType, LCLIntf, Messages, LMessages,
  {$ENDIF}

  JPP.Common, JPP.Common.Procs, JPP.Flash
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
    FBoundControl1: TJppBoundControl;
    FBoundControl2: TJppBoundControl;
    procedure SetTagExt(const Value: TJppTagExt);
    procedure SetShowLabel(const Value: Boolean);
    procedure CMMouseEnter (var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave (var Message: TMessage); message CM_MOUSELEAVE;
    procedure SetFlash(const Value: TJppFlashJppComboBox);
    procedure SetBoundLabelPosition(const Value: TLabelPosition);
    procedure SetBoundLabelSpacing(const Value: Integer);
    procedure AdjustLabelBounds(Sender: TObject);
    procedure SetBoundControl1(const Value: TJppBoundControl);
    procedure SetBoundControl2(const Value: TJppBoundControl);
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
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

    procedure SetBoundControlPosition(BoundCtrl: TJppBoundControl);
    procedure SetupBoundControl1(Sender: TObject);
    procedure SetupBoundControl2(Sender: TObject);
    procedure BoundControl1Changed(Sender: TObject);
    procedure BoundControl2Changed(Sender: TObject);

    property MouseOverControl: Boolean read FMouseOverControl;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure FlashBackground;
    procedure SetupInternalLabel;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer; AHeight: Integer); override;

    procedure UpdateBoundControl1Pos;
    procedure UpdateBoundControl2Pos;
    procedure UpdateBoundControlsPos;
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
    property BoundControl1: TJppBoundControl read FBoundControl1 write SetBoundControl1;
    property BoundControl2: TJppBoundControl read FBoundControl2 write SetBoundControl2;
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
    {$IFDEF DCC} property Touch; {$ENDIF}
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
    {$IFDEF DCC}property OnGesture;{$ENDIF}
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

    property BoundControl1;
    property BoundControl2;
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

  FBoundControl1 := TJppBoundControl.Create(Self);
  FBoundControl1.OnChange := SetupBoundControl1;
  FBoundControl1.OnBoundControlChanged := BoundControl1Changed;

  FBoundControl2 := TJppBoundControl.Create(Self);
  FBoundControl2.OnChange := SetupBoundControl2;
  FBoundControl2.OnBoundControlChanged := BoundControl2Changed;
end;

destructor TJppCustomComboBox.Destroy;
begin
  FTagExt.Free;
  FFlash.Free;
  FBoundControl1.Free;
  FBoundControl2.Free;
  inherited;
end;

procedure TJppCustomComboBox.Loaded;
begin
  inherited;
end;

procedure TJppCustomComboBox.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = FBoundLabel then FBoundLabel := nil
    else if AComponent = FBoundControl1.BoundControl then FBoundControl1.BoundControl := nil
    else if AComponent = FBoundControl2.BoundControl then FBoundControl2.BoundControl := nil;
  end;
end;

procedure TJppCustomComboBox.PropsChanged(Sender: TObject);
begin
  if csLoading in ComponentState then Exit;
end;

procedure TJppCustomComboBox.BoundControl1Changed(Sender: TObject);
begin
  if not Assigned(FBoundControl1.BoundControl) then Exit;
  FBoundControl1.BoundControl.FreeNotification(Self);
end;

procedure TJppCustomComboBox.BoundControl2Changed(Sender: TObject);
begin
  if not Assigned(FBoundControl2.BoundControl) then Exit;
  FBoundControl2.BoundControl.FreeNotification(Self);
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
  //ApplyAppearance;
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

procedure TJppCustomComboBox.SetBoundControl1(const Value: TJppBoundControl);
begin
  FBoundControl1 := Value;
end;

procedure TJppCustomComboBox.SetBoundControl2(const Value: TJppBoundControl);
begin
  FBoundControl2 := Value;
end;

procedure TJppCustomComboBox.SetBoundControlPosition(BoundCtrl: TJppBoundControl);
begin
  SetBoundedCtrlPos(Self, BoundCtrl, True);
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
  FBoundLabel.Visible := FShowLabel;
end;

procedure TJppCustomComboBox.SetBoundLabelSpacing(const Value: Integer);
begin
  FBoundLabelSpacing := Value;
  SetBoundLabelPosition(FBoundLabelPosition);
end;

procedure TJppCustomComboBox.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
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

procedure TJppCustomComboBox.SetupBoundControl1(Sender: TObject);
begin
  SetBoundControlPosition(FBoundControl1);
end;

procedure TJppCustomComboBox.SetupBoundControl2(Sender: TObject);
begin
  SetBoundControlPosition(FBoundControl2);
end;

procedure TJppCustomComboBox.SetupInternalLabel;
begin
  if Assigned(FBoundLabel) then Exit;
  FBoundLabel := TJppControlBoundLabel.Create(Self);
  FBoundLabel.FreeNotification(Self);
  FBoundLabel.OnAdjustBounds := AdjustLabelBounds;
  FBoundLabel.FocusControl := Self;
end;

procedure TJppCustomComboBox.UpdateBoundControl1Pos;
begin
  SetupBoundControl1(Self);
end;

procedure TJppCustomComboBox.UpdateBoundControl2Pos;
begin
  SetupBoundControl2(Self);
end;

procedure TJppCustomComboBox.UpdateBoundControlsPos;
begin
  UpdateBoundControl1Pos;
  UpdateBoundControl2Pos;
end;

procedure TJppCustomComboBox.WMWindowPosChanged(var Message: TWMWindowPosChanged);
begin
  inherited;
  UpdateBoundControlsPos;
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



