unit JPP.AnchoredControls;

{
  Jacek Pazera
  https://github.com/jackdp
  https://www.pazera-software.com

  TODO: Block circular anchoring: A.B + B.C + C.A
}

{$I jpp.inc}
{$IFDEF FPC} {$mode delphi} {$ENDIF}

interface

uses
  {$IFDEF MSWINDOWS}Windows,{$ENDIF}
  SysUtils, Classes, Controls, {$IFDEF HAS_SYSTEM_UITYPES}System.UITypes,{$ENDIF}
  {$IFDEF FPC}LCLType, LCLIntf,{$ENDIF}
  JPP.Common;

type

  TJppTopControlPos = (tcpTopLeftCorner, tcpTopLeft, tcpTopCenter, tcpTopRight, tcpTopRightCorner);
  TJppBottomControlPos = (bcpBottomLeftCorner, bcpBottomLeft, bcpBottomCenter, bcpBottomRight, bcpBottomRightCorner);
  TJppLeftControlPos = (lcpLeftTopCorner, lcpLeftTop, lcpLeftCenter, lcpLeftBottom, lcpLeftBottomCorner);
  TJppRightControlPos = (rcpRightTopCorner, rcpRightTop, rcpRightCenter, rcpRightBottom, rcpRightBottomCorner);

  TJppControlPos = (
    cpTopLeftCorner, cpTopLeft, cpTopCenter, cpTopRight, cpTopRightCorner,
    cpBottomLeftCorner, cpBottomLeft, cpBottomCenter, cpBottomRight, cpBottomRightCorner,
    cpLeftTopCorner, cpLeftTop, cpLeftCenter, cpLeftBottom, cpLeftBottomCorner,
    cpRightTopCorner, cpRightTop, cpRightCenter, cpRightBottom, cpRightBottomCorner
  );

  TJppControlPositionMode = (cpmDefault, cpmCustom);


  {$region ' --- TJppAnchoredControl --- '}
  TJppAnchoredControl = class(TJppPersistent)
  private
    FOwner: TComponent;
    FControl: TControl;
    FSpacing: integer;
    FDeltaPosX: integer;
    FDeltaPosY: integer;
    FControlPositionMode: TJppControlPositionMode;
    FCustomPosition: TJppControlPos;
    FEnabled: Boolean;
    procedure SetControl(const Value: TControl);
    procedure SetSpacing(const Value: integer);
    procedure SetDeltaPosX(const Value: integer);
    procedure SetDeltaPosY(const Value: integer);
    procedure SetControlPositionMode(const Value: TJppControlPositionMode);
    procedure SetCustomPosition(const Value: TJppControlPos);
    procedure SetEnabled(const Value: Boolean);
  protected
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(Source: TJppAnchoredControl); reintroduce;
  published
    property Control: TControl read FControl write SetControl;
    property Spacing: integer read FSpacing write SetSpacing default 3;
    property DeltaPosX: integer read FDeltaPosX write SetDeltaPosX default 0;
    property DeltaPosY: integer read FDeltaPosY write SetDeltaPosY default 0;
    property ControlPositionMode: TJppControlPositionMode read FControlPositionMode write SetControlPositionMode default cpmDefault;
    property CustomPosition: TJppControlPos read FCustomPosition write SetCustomPosition default cpTopLeft;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
  end;
  {$endregion TJppAnchoredControl}


  {$region ' --- TJppTopAnchoredControl --- '}
  TJppTopAnchoredControl = class(TJppAnchoredControl)
  private
    FPosition: TJppTopControlPos;
    procedure SetPosition(const Value: TJppTopControlPos);
  public
    constructor Create(AOwner: TComponent);
    procedure Assign(Source: TJppTopAnchoredControl);
  published
    property Position: TJppTopControlPos read FPosition write SetPosition default tcpTopLeft;
  end;
  {$endregion TJppTopAnchoredControl}


  {$region ' --- TJppBottomAnchoredControl --- '}
  TJppBottomAnchoredControl = class(TJppAnchoredControl)
  private
    FPosition: TJppBottomControlPos;
    procedure SetPosition(const Value: TJppBottomControlPos);
  public
    constructor Create(AOwner: TComponent);
    procedure Assign(Source: TJppBottomAnchoredControl);
  published
    property Position: TJppBottomControlPos read FPosition write SetPosition default bcpBottomLeft;
  end;
  {$endregion TJppBottomAnchoredControl}


  {$region ' --- TJppLeftAnchoredControl --- '}
  TJppLeftAnchoredControl = class(TJppAnchoredControl)
  private
    FPosition: TJppLeftControlPos;
    procedure SetPosition(const Value: TJppLeftControlPos);
  public
    constructor Create(AOwner: TComponent);
    procedure Assign(Source: TJppLeftAnchoredControl);
  published
    property Position: TJppLeftControlPos read FPosition write SetPosition default lcpLeftTop;
  end;
  {$endregion TJppLeftAnchoredControl}


  {$region ' --- TJppRightAnchoredControl --- '}
  TJppRightAnchoredControl = class(TJppAnchoredControl)
  private
    FPosition: TJppRightControlPos;
    procedure SetPosition(const Value: TJppRightControlPos);
  public
    constructor Create(AOwner: TComponent);
    procedure Assign(Source: TJppRightAnchoredControl);
  published
    property Position: TJppRightControlPos read FPosition write SetPosition default rcpRightTop;
  end;
  {$endregion TJppRightAnchoredControl}




  {$region ' --- TJppAnchoredControls --- '}
  TJppAnchoredControls = class(TJppPersistent)
  private
    FOwner: TComponent;
    FTop: TJppTopAnchoredControl;
    FBottom: TJppBottomAnchoredControl;
    FLeft: TJppLeftAnchoredControl;
    FRight: TJppRightAnchoredControl;
    procedure AnchoredControlChanged(Sender: TObject);
    procedure SetTop(const Value: TJppTopAnchoredControl);
    procedure SetBottom(const Value: TJppBottomAnchoredControl);
    procedure SetLeft(const Value: TJppLeftAnchoredControl);
    procedure SetRight(const Value: TJppRightAnchoredControl);
  public
    procedure UpdateTopControlPos;
    procedure UpdateBottomControlPos;
    procedure UpdateLeftControlPos;
    procedure UpdateRightControlPos;
    procedure UpdateAllControlsPos;
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
  published
    property Top: TJppTopAnchoredControl read FTop write SetTop;
    property Bottom: TJppBottomAnchoredControl read FBottom write SetBottom;
    property Left: TJppLeftAnchoredControl read FLeft write SetLeft;
    property Right: TJppRightAnchoredControl read FRight write SetRight;
  end;
  {$endregion TJppAnchoredControls}



procedure SetTopControlPos(MainCtrl, SubCtrl: TControl; TopControlPos: TJppTopControlPos; const Spacing, DeltaX, DeltaY: integer);
procedure SetBottomControlPos(MainCtrl, SubCtrl: TControl; BottomControlPos: TJppBottomControlPos; const Spacing, DeltaX, DeltaY: integer);
procedure SetLeftControlPos(MainCtrl, SubCtrl: TControl; LeftControlPos: TJppLeftControlPos; const Spacing, DeltaX, DeltaY: integer);
procedure SetRightControlPos(MainCtrl, SubCtrl: TControl; RightControlPos: TJppRightControlPos; const Spacing, DeltaX, DeltaY: integer);

procedure SetControlPos(MainCtrl, SubCtrl: TControl; ControlPos: TJppControlPos; const Spacing, DeltaX, DeltaY: integer);


implementation



{$region '                        SetControlPos x 5                           '}
procedure SetTopControlPos(MainCtrl, SubCtrl: TControl; TopControlPos: TJppTopControlPos; const Spacing, DeltaX, DeltaY: integer);
var
  ControlPos: TJppControlPos;
begin
  case TopControlPos of
    tcpTopLeftCorner: ControlPos := cpTopLeftCorner;
    tcpTopLeft: ControlPos := cpTopLeft;
    tcpTopCenter: ControlPos := cpTopCenter;
    tcpTopRight: ControlPos := cpTopRight;
    tcpTopRightCorner: ControlPos := cpTopRightCorner;
  else
    ControlPos := cpTopLeft;
  end;

  SetControlPos(MainCtrl, SubCtrl, ControlPos, Spacing, DeltaX, DeltaY);
end;

procedure SetBottomControlPos(MainCtrl, SubCtrl: TControl; BottomControlPos: TJppBottomControlPos; const Spacing, DeltaX, DeltaY: integer);
var
  ControlPos: TJppControlPos;
begin
  case BottomControlPos of
    bcpBottomLeftCorner: ControlPos := cpBottomLeftCorner;
    bcpBottomLeft: ControlPos := cpBottomLeft;
    bcpBottomCenter: ControlPos := cpBottomCenter;
    bcpBottomRight: ControlPos := cpBottomRight;
    bcpBottomRightCorner: ControlPos := cpBottomRightCorner;
  else
    ControlPos := cpBottomLeft;
  end;

  SetControlPos(MainCtrl, SubCtrl, ControlPos, Spacing, DeltaX, DeltaY);
end;

procedure SetLeftControlPos(MainCtrl, SubCtrl: TControl; LeftControlPos: TJppLeftControlPos; const Spacing, DeltaX, DeltaY: integer);
var
  ControlPos: TJppControlPos;
begin
  case LeftControlPos of
    lcpLeftTopCorner: ControlPos := cpLeftTopCorner;
    lcpLeftTop: ControlPos := cpLeftTop;
    lcpLeftCenter: ControlPos := cpLeftCenter;
    lcpLeftBottom: ControlPos := cpLeftBottom;
    lcpLeftBottomCorner: ControlPos := cpLeftBottomCorner;
  else
    ControlPos := cpLeftTop;
  end;

  SetControlPos(MainCtrl, SubCtrl, ControlPos, Spacing, DeltaX, DeltaY);
end;

procedure SetRightControlPos(MainCtrl, SubCtrl: TControl; RightControlPos: TJppRightControlPos; const Spacing, DeltaX, DeltaY: integer);
var
  ControlPos: TJppControlPos;
begin
  case RightControlPos of
    rcpRightTopCorner: ControlPos := cpRightTopCorner;
    rcpRightTop: ControlPos := cpRightTop;
    rcpRightCenter: ControlPos := cpRightCenter;
    rcpRightBottom: ControlPos := cpRightBottom;
    rcpRightBottomCorner: ControlPos := cpRightBottomCorner;
  else
    ControlPos := cpRightTop;
  end;

  SetControlPos(MainCtrl, SubCtrl, ControlPos, Spacing, DeltaX, DeltaY);
end;

procedure SetControlPos(MainCtrl, SubCtrl: TControl; ControlPos: TJppControlPos; const Spacing, DeltaX, DeltaY: integer);
var
  x, y: integer;
begin
  case ControlPos of

    {$region ' --- TOP --- '}
    cpTopLeftCorner:
      begin
        x := MainCtrl.Left - SubCtrl.Width + DeltaX;
        y := MainCtrl.Top - SubCtrl.Height - Spacing + DeltaY;
      end;

    cpTopLeft:
      begin
        x := MainCtrl.Left + DeltaX;
        y := MainCtrl.Top - SubCtrl.Height - Spacing + DeltaY;
      end;

    cpTopCenter:
      begin
        x := MainCtrl.Left + (MainCtrl.Width div 2) - (SubCtrl.Width div 2) + DeltaX;
        y := MainCtrl.Top - SubCtrl.Height - Spacing + DeltaY;
      end;

    cpTopRight:
      begin
        x := MainCtrl.Left + MainCtrl.Width - SubCtrl.Width + DeltaX;
        y := MainCtrl.Top - SubCtrl.Height - Spacing + DeltaY;
      end;

    cpTopRightCorner:
      begin
        x := MainCtrl.Left + MainCtrl.Width + DeltaX;
        y := MainCtrl.Top - SubCtrl.Height - Spacing + DeltaY;
      end;
    {$endregion TOP}


    {$region ' --- BOTTOM --- '}
    cpBottomLeftCorner:
      begin
        x := MainCtrl.Left - SubCtrl.Width + DeltaX;
        y := MainCtrl.Top + MainCtrl.Height + Spacing + DeltaY;
      end;

    cpBottomLeft:
      begin
        x := MainCtrl.Left + DeltaX;
        y := MainCtrl.Top + MainCtrl.Height + Spacing + DeltaY;
      end;

    cpBottomCenter:
      begin
        x := MainCtrl.Left + (MainCtrl.Width div 2) - (SubCtrl.Width div 2) + DeltaX;
        y := MainCtrl.Top + MainCtrl.Height + Spacing + DeltaY;
      end;

    cpBottomRight:
      begin
        x := MainCtrl.Left + MainCtrl.Width - SubCtrl.Width + DeltaX;
        y := MainCtrl.Top + MainCtrl.Height + Spacing + DeltaY;
      end;

    cpBottomRightCorner:
      begin
        x := MainCtrl.Left + MainCtrl.Width + DeltaX;
        y := MainCtrl.Top + MainCtrl.Height + Spacing + DeltaY;
      end;
    {$endregion BOTTOM}


    {$region ' --- LEFT --- '}
    cpLeftTopCorner:
      begin
        x := MainCtrl.Left - SubCtrl.Width - Spacing + DeltaX;
        y := MainCtrl.Top - SubCtrl.Height + DeltaY;
      end;

    cpLeftTop:
      begin
        x := MainCtrl.Left - SubCtrl.Width - Spacing + DeltaX;
        y := MainCtrl.Top + DeltaY;
      end;

    cpLeftCenter:
      begin
        x := MainCtrl.Left - SubCtrl.Width - Spacing + DeltaX;
        y := MainCtrl.Top + (MainCtrl.Height div 2) - (SubCtrl.Height div 2) + DeltaY;
      end;

    cpLeftBottom:
      begin
        x := MainCtrl.Left - SubCtrl.Width - Spacing + DeltaX;
        y := MainCtrl.Top + MainCtrl.Height - SubCtrl.Height + DeltaY;
      end;

    cpLeftBottomCorner:
      begin
        x := MainCtrl.Left - SubCtrl.Width - Spacing + DeltaX;
        y := MainCtrl.Top + MainCtrl.Height + DeltaY;
      end;
    {$endregion LEFT}


    {$region ' --- RIGHT --- '}
    cpRightTopCorner:
      begin
        x := MainCtrl.Left + MainCtrl.Width + Spacing + DeltaX;
        y := MainCtrl.Top - SubCtrl.Height + DeltaY;
      end;

    cpRightTop:
      begin
        x := MainCtrl.Left + MainCtrl.Width + Spacing + DeltaX;
        y := MainCtrl.Top + DeltaY;
      end;

    cpRightCenter:
      begin
        x := MainCtrl.Left + MainCtrl.Width + Spacing + DeltaX;
        y := MainCtrl.Top + (MainCtrl.Height div 2) - (SubCtrl.Height div 2) + DeltaY;
      end;

    cpRightBottom:
      begin
        x := MainCtrl.Left + MainCtrl.Width + Spacing + DeltaX;
        y := MainCtrl.Top + MainCtrl.Height - SubCtrl.Height + DeltaY;
      end;

    cpRightBottomCorner:
      begin
        x := MainCtrl.Left + MainCtrl.Width + Spacing + DeltaX;
        y := MainCtrl.Top + MainCtrl.Height + DeltaY;
      end;
    {$endregion RIGHT}

  else
    begin
      x := 0;
      y := 0;
    end;

  end;

//  if SubCtrl.Left <> x then SubCtrl.Left := x;
//  if SubCtrl.Top <> y then SubCtrl.Top := y;
  if (SubCtrl.Left <> x) or (SubCtrl.Top <> y) then SubCtrl.SetBounds(x, y, SubCtrl.Width, SubCtrl.Height);
end;
{$endregion SetControlPos x 5}


{$region '                            TJppAnchoredControl                               '}
constructor TJppAnchoredControl.Create(AOwner: TComponent);
begin
  inherited Create;
  FOwner := AOwner;
  FControl := nil;
  FSpacing := 3;
  FDeltaPosX := 0;
  FDeltaPosY := 0;
  FControlPositionMode := cpmDefault;
  FCustomPosition := cpTopLeft;
  FEnabled := True;
end;

destructor TJppAnchoredControl.Destroy;
begin
  inherited;
end;

procedure TJppAnchoredControl.Assign(Source: TJppAnchoredControl);
begin
  FControl := Source.Control;
  FSpacing := Source.Spacing;
  FDeltaPosX := Source.DeltaPosX;
  FDeltaPosY := Source.DeltaPosY;
  FControlPositionMode := Source.ControlPositionMode;
  FCustomPosition := Source.CustomPosition;
  FEnabled := Source.Enabled;
end;

// -----------------------------------------------------------------------------------------------
procedure TJppAnchoredControl.SetControl(const Value: TControl);
var
  OwnerAsWinControl: TWinControl;
begin
  if Value = FOwner then Exit;
  FControl := Value;

  if Assigned(FControl) then
  begin
    FControl.FreeNotification(FOwner);

    if FOwner is TWinControl then
    begin
      OwnerAsWinControl := FOwner as TWinControl;
      if Assigned(OwnerAsWinControl.Parent) then
        if FControl.Parent <> OwnerAsWinControl.Parent then FControl.Parent := OwnerAsWinControl.Parent;
    end;
  end;

  PropsChanged(Self);
end;
// -----------------------------------------------------------------------------------------------

procedure TJppAnchoredControl.SetControlPositionMode(const Value: TJppControlPositionMode);
begin
  if FControlPositionMode = Value then Exit;
  FControlPositionMode := Value;
  PropsChanged(Self);
end;

procedure TJppAnchoredControl.SetCustomPosition(const Value: TJppControlPos);
begin
  if FCustomPosition = Value then Exit;
  FCustomPosition := Value;
  PropsChanged(Self);
end;

procedure TJppAnchoredControl.SetDeltaPosX(const Value: integer);
begin
  if FDeltaPosX = Value then Exit;
  FDeltaPosX := Value;
  PropsChanged(Self);
end;

procedure TJppAnchoredControl.SetDeltaPosY(const Value: integer);
begin
  if FDeltaPosY = Value then Exit;
  FDeltaPosY := Value;
  PropsChanged(Self);
end;

procedure TJppAnchoredControl.SetEnabled(const Value: Boolean);
begin
  if FEnabled = Value then Exit;
  FEnabled := Value;
  PropsChanged(Self);
end;

procedure TJppAnchoredControl.SetSpacing(const Value: integer);
begin
  if FSpacing = Value then Exit;
  FSpacing := Value;
  PropsChanged(Self);
end;
{$endregion TJppAnchoredControl}



{$region '                         TJppTopAnchoredControl                         '}
constructor TJppTopAnchoredControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPosition := tcpTopLeft;
end;

procedure TJppTopAnchoredControl.Assign(Source: TJppTopAnchoredControl);
begin
  inherited Assign(TJppAnchoredControl(Source));
  FPosition := Source.Position;
end;

procedure TJppTopAnchoredControl.SetPosition(const Value: TJppTopControlPos);
begin
  if FPosition = Value then Exit;
  FPosition := Value;
  PropsChanged(Self);
end;
{$endregion TJppTopAnchoredControl}


{$region '                         TJppBottomAnchoredControl                         '}
constructor TJppBottomAnchoredControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPosition := bcpBottomLeft;
end;

procedure TJppBottomAnchoredControl.Assign(Source: TJppBottomAnchoredControl);
begin
  inherited Assign(TJppAnchoredControl(Source));
  FPosition := Source.Position;
end;

procedure TJppBottomAnchoredControl.SetPosition(const Value: TJppBottomControlPos);
begin
  if FPosition = Value then Exit;
  FPosition := Value;
  PropsChanged(Self);
end;
{$endregion TJppBottomAnchoredControl}


{$region '                         TJppLeftAnchoredControl                         '}
constructor TJppLeftAnchoredControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPosition := lcpLeftTop;
end;

procedure TJppLeftAnchoredControl.Assign(Source: TJppLeftAnchoredControl);
begin
  inherited Assign(TJppAnchoredControl(Source));
  FPosition := Source.Position;
end;

procedure TJppLeftAnchoredControl.SetPosition(const Value: TJppLeftControlPos);
begin
  if FPosition = Value then Exit;
  FPosition := Value;
  PropsChanged(Self);
end;
{$endregion TJppTopAnchoredControl}


{$region '                         TJppRightAnchoredControl                         '}
constructor TJppRightAnchoredControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FPosition := rcpRightTop;
end;

procedure TJppRightAnchoredControl.Assign(Source: TJppRightAnchoredControl);
begin
  inherited Assign(TJppAnchoredControl(Source));
  FPosition := Source.Position;
end;

procedure TJppRightAnchoredControl.SetPosition(const Value: TJppRightControlPos);
begin
  if FPosition = Value then Exit;
  FPosition := Value;
  PropsChanged(Self);
end;
{$endregion TJppRightAnchoredControl}




{$region '                           TJppAnchoredControls                              '}

constructor TJppAnchoredControls.Create(AOwner: TComponent);
begin
  inherited Create;
  FOwner := AOwner;

  FTop := TJppTopAnchoredControl.Create(AOwner);
  FTop.OnChange := AnchoredControlChanged;

  FBottom := TJppBottomAnchoredControl.Create(AOwner);
  FBottom.OnChange := AnchoredControlChanged;

  FLeft := TJppLeftAnchoredControl.Create(AOwner);
  FLeft.OnChange := AnchoredControlChanged;

  FRight := TJppRightAnchoredControl.Create(AOwner);
  FRight.OnChange := AnchoredControlChanged;
end;

destructor TJppAnchoredControls.Destroy;
begin
  FTop.Free;
  FBottom.Free;
  FLeft.Free;
  FRight.Free;
  inherited;
end;

procedure TJppAnchoredControls.AnchoredControlChanged(Sender: TObject);
begin
  if Sender = FTop then UpdateTopControlPos
  else if Sender = FBottom then UpdateBottomControlPos
  else if Sender = FLeft then UpdateLeftControlPos
  else if Sender = FRight then UpdateRightControlPos;
end;

procedure TJppAnchoredControls.UpdateAllControlsPos;
begin
  UpdateTopControlPos;
  UpdateBottomControlPos;
  UpdateLeftControlPos;
  UpdateRightControlPos;
end;

procedure TJppAnchoredControls.UpdateTopControlPos;
begin
  if not Assigned(FTop.Control) then Exit;
  if not FTop.Enabled then Exit;
  if not (FOwner is TControl) then Exit;

  if FTop.ControlPositionMode = cpmDefault then
    SetTopControlPos((FOwner as TControl), FTop.Control, FTop.Position, FTop.Spacing, FTop.DeltaPosX, FTop.DeltaPosY)
  else
    SetControlPos((FOwner as TControl), FTop.Control, FTop.CustomPosition, FTop.Spacing, FTop.DeltaPosX, FTop.DeltaPosY);
end;

procedure TJppAnchoredControls.UpdateBottomControlPos;
begin
  if not Assigned(FBottom.Control) then Exit;
  if not FBottom.Enabled then Exit;
  if not (FOwner is TControl) then Exit;

  if FBottom.ControlPositionMode = cpmDefault then
    SetBottomControlPos((FOwner as TControl), FBottom.Control, FBottom.Position, FBottom.Spacing, FBottom.DeltaPosX, FBottom.DeltaPosY)
  else
    SetControlPos((FOwner as TControl), FBottom.Control, FBottom.CustomPosition, FBottom.Spacing, FBottom.DeltaPosX, FBottom.DeltaPosY);
end;

procedure TJppAnchoredControls.UpdateLeftControlPos;
begin
  if not Assigned(FLeft.Control) then Exit;
  if not FLeft.Enabled then Exit;
  if not (FOwner is TControl) then Exit;

  if FLeft.ControlPositionMode = cpmDefault then
    SetLeftControlPos((FOwner as TControl), FLeft.Control, FLeft.Position, FLeft.Spacing, FLeft.DeltaPosX, FLeft.DeltaPosY)
  else
    SetControlPos((FOwner as TControl), FLeft.Control, FLeft.CustomPosition, FLeft.Spacing, FLeft.DeltaPosX, FLeft.DeltaPosY);
end;

procedure TJppAnchoredControls.UpdateRightControlPos;
begin
  if not Assigned(FRight.Control) then Exit;
  if not FRight.Enabled then Exit;
  if not (FOwner is TControl) then Exit;

  if FRight.ControlPositionMode = cpmDefault then
    SetRightControlPos((FOwner as TControl), FRight.Control, FRight.Position, FRight.Spacing, FRight.DeltaPosX, FRight.DeltaPosY)
  else
    SetControlPos((FOwner as TControl), FRight.Control, FRight.CustomPosition, FRight.Spacing, FRight.DeltaPosX, FRight.DeltaPosY);
end;

procedure TJppAnchoredControls.SetBottom(const Value: TJppBottomAnchoredControl);
begin
  if not Assigned(Value) then Exit;
//  FBottom := Value;
  FBottom.Assign(Value);
  UpdateBottomControlPos;
  PropsChanged(Self);
end;

procedure TJppAnchoredControls.SetLeft(const Value: TJppLeftAnchoredControl);
begin
  if not Assigned(Value) then Exit;
//  FLeft := Value;
  FLeft.Assign(Value);
  UpdateLeftControlPos;
  PropsChanged(Self);
end;

procedure TJppAnchoredControls.SetRight(const Value: TJppRightAnchoredControl);
begin
  if not Assigned(Value) then Exit;
//  FRight := Value;
  FRight.Assign(Value);
  UpdateRightControlPos;
  PropsChanged(Self);
end;

procedure TJppAnchoredControls.SetTop(const Value: TJppTopAnchoredControl);
begin
  if not Assigned(Value) then Exit;
//  FTop := Value;
  FTop.Assign(Value);
  UpdateTopControlPos;
  PropsChanged(Self);
end;


{$endregion TJppAnchoredControls}


end.


