unit JPP.DateTimePicker;

{
  Jacek Pazera
  https://www.pazera-software.com
  https://github.com/jackdp
}

{$I jpp.inc}
{$IFDEF FPC} {$mode delphi} {$ENDIF}


interface

uses
  {$IFDEF MSWINDOWS}Windows,{$ENDIF}
  Messages,
  SysUtils, Classes, Types, {$IFDEF DCC}{$IFDEF HAS_SYSTEM_UITYPES}System.UITypes,{$ENDIF}{$ENDIF}
  Controls, Graphics, ExtCtrls, ComCtrls,
  {$IFDEF FPC}LCLType, LCLIntf, LMessages, DateTimePicker,{$ENDIF}
  JPP.Common, JPP.Common.Procs, JPP.AnchoredControls;


type


  {$region ' --- TJppCustomDateTimePicker --- '}
  TJppCustomDateTimePicker = class(TDateTimePicker)
  private
    FBoundLabel: TJppControlBoundLabel;
    FMouseOverControl: Boolean;
    FTagExt: TJppTagExt;
    FShowLabel: Boolean;
    FBoundLabelSpacing: Integer;
    FBoundLabelPosition: TLabelPosition;
    FAnchoredControls: TJppAnchoredControls;
    procedure SetTagExt(const Value: TJppTagExt);
    procedure SetShowLabel(const Value: Boolean);
    procedure CMMouseEnter (var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave (var Message: TMessage); message CM_MOUSELEAVE;
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

    property MouseOverControl: Boolean read FMouseOverControl;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure SetupInternalLabel;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer; AHeight: Integer); override;
  protected
    property BoundLabel: TJppControlBoundLabel read FBoundLabel;
    property BoundLabelPosition: TLabelPosition read FBoundLabelPosition write SetBoundLabelPosition default lpLeft;
    property BoundLabelSpacing: Integer read FBoundLabelSpacing write SetBoundLabelSpacing default 4;

    property TagExt: TJppTagExt read FTagExt write SetTagExt;
    property ShowLabel: Boolean read FShowLabel write SetShowLabel default True;

    property AnchoredControls: TJppAnchoredControls read FAnchoredControls write SetAnchoredControls;
  end;
  {$endregion TJppCustomDateTimePicker}


  {$region ' --- TJppDateTimePicker --- '}
  TJppDateTimePicker = class(TJppCustomDateTimePicker)
  public
    property MouseOverControl;
  published
    {$IFDEF FPC}
    property ArrowShape;
    property CenturyFrom;
    property DateDisplayOrder;
    property ReadOnly;
    property AutoSize;
    property BorderStyle;
    property BorderSpacing;
    property DateSeparator;
    property TrailingSeparator;
    property TextForNullDate;
    property LeadingZeros;
    property Cursor;
    property NullInputAllowed;
    property TimeSeparator;
    property TimeFormat;
    property TimeDisplay;
    property UseDefaultSeparators;
    property Cascade;
    property AutoButtonSize;
    property AutoAdvance;
    property HideDateTimeParts;
    property MonthNames;
    property ShowMonthNames;
    property Options;
    property OnCheckBoxChange;
    property OnChangeBounds;
    property OnDblClick;
    property OnEditingDone;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnUTF8KeyPress;
    {$ENDIF}
    property Align;
    property Anchors;
    {$IFDEF DCC}
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind default bkNone;
    property BevelWidth;
    {$ENDIF}
    property BiDiMode;
    property CalAlignment;
    {$IFDEF DCC}property CalColors;{$ENDIF}
    property Constraints;
    //DELPHI: The Date, Time, ShowCheckbox, and Checked properties must be in this order:
    property Date;
    {$IFDEF DCC}property Format;{$ENDIF}
    property Time;
    property ShowCheckbox;
    property Checked;
    property Color;
    {$IFDEF DCC}property DateFormat;{$ENDIF}
    property DateMode;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    {$IFDEF DCC}
    property ImeMode;
    property ImeName;
    {$ENDIF}
    property Kind;
    property MaxDate;
    property MinDate;
    {$IFDEF DCC}property ParseInput;{$ENDIF}
    property ParentBiDiMode;
    property ParentColor default False;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    {$IFDEF DELPHI2010_OR_ABOVE}property Touch;{$ENDIF}
    property Visible;
    {$IFDEF HAS_STYLE_ELEMENTS}property StyleElements;{$ENDIF}
    property OnClick;
    property OnCloseUp;
    property OnChange;
    property OnContextPopup;
    property OnDropDown;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    {$IFDEF DELPHI2010_OR_ABOVE}property OnGesture;{$ENDIF}
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnStartDock;
    property OnStartDrag;
    {$IFDEF DCC}property OnUserInput;{$ENDIF}

    // --------- Custom Properties ---------
    property ShowLabel;
    property TagExt;

    property BoundLabel;
    property BoundLabelPosition;
    property BoundLabelSpacing;

    property AnchoredControls;
  end;
  {$endregion TJppDateTimePicker}


implementation


{$region ' --------------------------------- TJppCustomDateTimePicker ------------------------------- '}

constructor TJppCustomDateTimePicker.Create(AOwner: TComponent);
begin
  inherited;

  FBoundLabelPosition := lpLeft;
  FBoundLabelSpacing := 4;
  SetupInternalLabel;

  FTagExt := TJppTagExt.Create(Self);
  FShowLabel := True;
  FMouseOverControl := False;

  FAnchoredControls := TJppAnchoredControls.Create(Self);
end;

destructor TJppCustomDateTimePicker.Destroy;
begin
  FTagExt.Free;
  FAnchoredControls.Free;
  inherited;
end;

procedure TJppCustomDateTimePicker.Loaded;
begin
  inherited;
end;

procedure TJppCustomDateTimePicker.SetAnchoredControls(const Value: TJppAnchoredControls);
begin
  FAnchoredControls := Value;
end;

procedure TJppCustomDateTimePicker.Notification(AComponent: TComponent; Operation: TOperation);
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

procedure TJppCustomDateTimePicker.PropsChanged(Sender: TObject);
begin
  if csLoading in ComponentState then Exit;
end;

procedure TJppCustomDateTimePicker.CMBiDiModeChanged(var Message: TMessage);
begin
  inherited;
  if FBoundLabel <> nil then FBoundLabel.BiDiMode := BiDiMode;
end;

procedure TJppCustomDateTimePicker.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if FBoundLabel <> nil then FBoundLabel.Enabled := Enabled;
end;

procedure TJppCustomDateTimePicker.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  {$IFDEF MSWINDOWS}
  if (GetActiveWindow <> 0) then
  {$ENDIF}
  begin
    FMouseOverControl := True;
  end;
end;

procedure TJppCustomDateTimePicker.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  FMouseOverControl := False;
end;

procedure TJppCustomDateTimePicker.CMVisibleChanged(var Message: TMessage);
begin
  inherited;
  if FBoundLabel <> nil then FBoundLabel.Visible := Visible;
end;

procedure TJppCustomDateTimePicker.DoEnter;
begin
  inherited;
end;

procedure TJppCustomDateTimePicker.DoExit;
begin
  inherited DoExit;
end;

procedure TJppCustomDateTimePicker.AdjustLabelBounds(Sender: TObject);
begin
  SetBoundLabelPosition(FBoundLabelPosition);
end;

procedure TJppCustomDateTimePicker.SetBoundLabelPosition(const Value: TLabelPosition);
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

procedure TJppCustomDateTimePicker.SetBoundLabelSpacing(const Value: Integer);
begin
  FBoundLabelSpacing := Value;
  SetBoundLabelPosition(FBoundLabelPosition);
end;

procedure TJppCustomDateTimePicker.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  SetBoundLabelPosition(FBoundLabelPosition);
  if not (csDestroying in ComponentState) then
    if Assigned(FAnchoredControls) then FAnchoredControls.UpdateAllControlsPos;
end;

procedure TJppCustomDateTimePicker.SetParent(AParent: TWinControl);
begin
  inherited;
  if FBoundLabel <> nil then
  begin
    FBoundLabel.Parent := AParent;
    FBoundLabel.Visible := True;
  end;
end;

procedure TJppCustomDateTimePicker.SetShowLabel(const Value: Boolean);
begin
  if FBoundLabel.Visible = Value then Exit;
  FShowLabel := Value;
  FBoundLabel.Visible := FShowLabel;
end;

procedure TJppCustomDateTimePicker.SetTagExt(const Value: TJppTagExt);
begin
  FTagExt := Value;
end;

procedure TJppCustomDateTimePicker.SetupInternalLabel;
begin
  if Assigned(FBoundLabel) then Exit;
  FBoundLabel := TJppControlBoundLabel.Create(Self);
  FBoundLabel.FreeNotification(Self);
  FBoundLabel.OnAdjustBounds := AdjustLabelBounds;
  FBoundLabel.FocusControl := Self;
  {$IFDEF FPC}FBoundLabel.OnChangeBounds := AdjustLabelBounds;{$ENDIF}
end;


{$IFDEF MSWINDOWS}
procedure TJppCustomDateTimePicker.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
end;
{$ENDIF}


{$endregion TJppCustomDateTimePicker}



end.



