unit JPP.Flash;

{$I jpp.inc}
{$IFDEF FPC} {$mode delphi} {$ENDIF}

interface

uses
  {$IFDEF MSWINDOWS}Windows,{$ENDIF}
  SysUtils, Classes,
  Graphics, Controls, StdCtrls, ExtCtrls,
  JPP.Common, JPP.Timer;


type

  TJppFlashCountInt = 1..255;
  TJppFlashIntervalInt = 20..2000;

  TJppFlashColorChanged = procedure(Sender: TObject; const AColor: TColor) of object;

  {$region ' --- TJppFlashBase --- '}
  TJppFlashBase = class(TJppPersistent)
  private
    FTimer: TJppTimer;
    FFlashColor: TColor;
    FFlashing: Boolean;
    FFlashCount: TJppFlashCountInt;
    FEnabled: Boolean;
    FOriginalColor: TColor;
    FCurrentColor: TColor;
    FOnFlashColorChanged: TJppFlashColorChanged;
    FOnFlashFinished: TNotifyEvent;
    FFlashInterval: TJppFlashIntervalInt;
    FFreeOnFlashFinished: Boolean;
    procedure SetFlashColor(const Value: TColor);
    procedure SetFlashCount(const Value: TJppFlashCountInt);
    procedure SetEnabled(const Value: Boolean);
    procedure StartFlash;
    procedure StopFlash;
    procedure SetCurrentColor(const Value: TColor);
    procedure SetOriginalColor(const Value: TColor);
    procedure OnFlash(Sender: TObject);
    procedure SetOnFlashColorChanged(const Value: TJppFlashColorChanged);
    procedure SetOnFlashFinished(const Value: TNotifyEvent);
    procedure RepeatCountReached(Sender: TObject);
    procedure SetFlashInterval(const Value: TJppFlashIntervalInt);
    procedure SetFreeOnFlashFinished(const Value: Boolean);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Flash;
  protected
    property CurrentColor: TColor read FCurrentColor write SetCurrentColor;

    property OnFlashColorChanged: TJppFlashColorChanged read FOnFlashColorChanged write SetOnFlashColorChanged;
    property OnFlashFinished: TNotifyEvent read FOnFlashFinished write SetOnFlashFinished;

    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property FlashColor: TColor read FFlashColor write SetFlashColor default $007878FF; // RGB 255 120 120
    property FlashCount: TJppFlashCountInt read FFlashCount write SetFlashCount default 3;
    property FlashInterval: TJppFlashIntervalInt read FFlashInterval write SetFlashInterval default 150;
  public
    property Flashing: Boolean read FFlashing;
    property OriginalColor: TColor read FOriginalColor write SetOriginalColor;
    property FreeOnFlashFinished: Boolean read FFreeOnFlashFinished write SetFreeOnFlashFinished;
  end;
  {$endregion TJppFlashBase}


  {$region ' --- TJppFlashEdit --- '}
  TJppFlashEdit = class(TJppFlashBase)
  private
    FEdit: TCustomEdit;
  protected
    procedure FlashColorChanged(Sender: TObject; const AColor: TColor);
  public
    constructor Create(Edit: TCustomEdit);
  published
    property Enabled;
    property FlashColor;
    property FlashCount;
    property FlashInterval;
    property OnFlashFinished;
  end;
  {$endregion TJppFlashEdit}


  {$region ' --- TJppFlashLabel --- '}
  TJppFlashLabel = class(TJppFlashBase)
  private
    FLabel: TCustomLabel;
    FTransparent: Boolean;
  protected
    procedure FlashColorChanged(Sender: TObject; const AColor: TColor);
    procedure StopFlash;
  public
    constructor Create(ALabel: TCustomLabel);
  published
    property Enabled;
    property FlashColor;
    property FlashCount;
    property FlashInterval;
    property OnFlashFinished;
  end;
  {$endregion TJppFlashLabel}

  
implementation



{$region ' -------------------------- TJppFlashBase ------------------------- '}

constructor TJppFlashBase.Create(AOwner: TComponent);
begin
  inherited Create;
  FTimer := TJppTimer.Create(AOwner);
  FTimer.Enabled := False;
  FTimer.OnTimer := OnFlash;
  FTimer.OnRepeatCountLimitReached := RepeatCountReached;
  FFlashing := False;
  FOnFlashColorChanged := nil;
  FEnabled := True;
  FFlashColor := $007878FF;
  FFreeOnFlashFinished := True;
  FFlashCount := 3;
  FFlashInterval := 150;
  FTimer.Interval := FFlashInterval;
end;

destructor TJppFlashBase.Destroy;
begin
  FTimer.Stop;
  FTimer.Free;
  inherited;
end;

procedure TJppFlashBase.Flash;
begin
  if not Enabled then Exit;
  if FTimer.Enabled then FTimer.Stop;
  StartFlash;
end;

procedure TJppFlashBase.StartFlash;
begin
  FFlashing := True;
  FTimer.Start;
end;

procedure TJppFlashBase.OnFlash(Sender: TObject);
begin
  if not Enabled then Exit;
  if Odd(FTimer.Counter) then CurrentColor := FlashColor
  else CurrentColor := OriginalColor;
  if Assigned(FOnFlashColorChanged) then FOnFlashColorChanged(Self, CurrentColor);
end;

procedure TJppFlashBase.RepeatCountReached(Sender: TObject);
begin
  StopFlash;
end;

procedure TJppFlashBase.StopFlash;
begin
  FFlashing := False;
  if FTimer.Enabled then FTimer.Stop;
  if Assigned(FOnFlashFinished) then FOnFlashFinished(Self);
  if FFreeOnFlashFinished then Self.Free;
end;

procedure TJppFlashBase.SetCurrentColor(const Value: TColor);
begin
  FCurrentColor := Value;
end;

procedure TJppFlashBase.SetEnabled(const Value: Boolean);
begin
  if FEnabled = Value then Exit;
  FEnabled := Value;
  if FFlashing then StopFlash;
end;

procedure TJppFlashBase.SetFlashColor(const Value: TColor);
begin
  if FFlashColor = Value then Exit;
  FFlashColor := Value;
end;

procedure TJppFlashBase.SetFlashCount(const Value: TJppFlashCountInt);
begin
  if FFlashColor = Value then Exit;
  FFlashCount := Value;
  FTimer.RepeatCountLimit := FFlashCount * 2;
end;

procedure TJppFlashBase.SetFlashInterval(const Value: TJppFlashIntervalInt);
begin
  FFlashInterval := Value;
  FTimer.Interval := FFlashInterval;
end;

procedure TJppFlashBase.SetFreeOnFlashFinished(const Value: Boolean);
begin
  FFreeOnFlashFinished := Value;
end;

procedure TJppFlashBase.SetOnFlashColorChanged(const Value: TJppFlashColorChanged);
begin
  FOnFlashColorChanged := Value;
end;

procedure TJppFlashBase.SetOnFlashFinished(const Value: TNotifyEvent);
begin
  FOnFlashFinished := Value;
end;

procedure TJppFlashBase.SetOriginalColor(const Value: TColor);
begin
  FOriginalColor := Value;
end;

{$endregion TJppFlashBase}




{$region ' -------------------- TJppFlashEdit --------------------- '}

constructor TJppFlashEdit.Create(Edit: TCustomEdit);
begin
  inherited Create(Edit);
  FEdit := Edit;
  FOnFlashColorChanged := FlashColorChanged;
  if (FEdit is TEdit) then OriginalColor := (FEdit as TEdit).Color
  else if (FEdit is TLabeledEdit) then OriginalColor := (FEdit as TLabeledEdit).Color;
end;


procedure TJppFlashEdit.FlashColorChanged(Sender: TObject; const AColor: TColor);
begin
  if (FEdit is TEdit) then
    with (FEdit as TEdit) do Color := AColor
  else if (FEdit is TLabeledEdit) then
    with (FEdit as TLabeledEdit) do Color := AColor;
end;

{$endregion TJppFlashEdit}


{$region ' ------------------- TJppFlashLabel ------------------ '}

constructor TJppFlashLabel.Create(ALabel: TCustomLabel);
begin
  inherited Create(ALabel);
  FLabel := ALabel;
  FOnFlashColorChanged := FlashColorChanged;

  if FLabel is TLabel then
    with FLabel as TLabel do
    begin
      OriginalColor := Color;
      FTransparent := Transparent;
      Transparent := False;
    end;
end;

procedure TJppFlashLabel.FlashColorChanged(Sender: TObject; const AColor: TColor);
begin
  if FLabel is TLabel then (FLabel as TLabel).Color := AColor;
end;

procedure TJppFlashLabel.StopFlash;
begin
  inherited;
  if FLabel is TLabel then
    with FLabel as TLabel do
    begin
      Transparent := FTransparent;
    end;
end;

{$endregion TJppFlashLabel}

end.
