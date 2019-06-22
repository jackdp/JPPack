unit JPP.Timer;

{
  Jacek Pazera
  http://www.pazera-software.com
  https://github.com/jackdp
  Last mod: 2019.05.25
}

{$IFDEF FPC} {$mode objfpc}{$H+} {$ENDIF}

interface

uses
  {$IFDEF DCC}
  Winapi.Windows, Winapi.Messages,
  System.SysUtils, System.Classes,
  Vcl.Forms, Vcl.Controls, Vcl.Graphics, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Consts,
  {$ELSE}
  SysUtils, Classes, Controls, StdCtrls, ExtCtrls,
  {$ENDIF}

  JPP.Types, JPP.Common;

type


  TJppCustomTimer = class(TTimer)
  private
    FRepeatCountLimit: Cardinal;
    FCounter: Cardinal;
    FTagExt: TJppTagExt;
    FOnRepeatCountLimitReached: TNotifyEvent;
    FClearCounterOnStart: Boolean;
    procedure SetRepeatCountLimit(const Value: Cardinal);
    procedure SetTagExt(const Value: TJppTagExt);
    procedure SetOnRepeatCountLimitReached(const Value: TNotifyEvent);
    procedure SetClearCounterOnStart(const Value: Boolean);
  protected
    {$IFDEF DCC}procedure Timer; override; {$ENDIF} //dynamic;
    {$IFDEF FPC}procedure DoOnTimer; override;{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Start;
    procedure Stop;

    property Counter: Cardinal read FCounter write FCounter;
    property ClearCounterOnStart: Boolean read FClearCounterOnStart write SetClearCounterOnStart default True;
    property RepeatCountLimit: Cardinal read FRepeatCountLimit write SetRepeatCountLimit default 0; // 0 = no repeat limit
    property OnRepeatCountLimitReached: TNotifyEvent read FOnRepeatCountLimitReached write SetOnRepeatCountLimitReached;

    property TagExt: TJppTagExt read FTagExt write SetTagExt;
  end;


  TJppTimer = class(TJppCustomTimer)
  published
    property Counter;
    property Enabled;
    property Interval;
    property RepeatCountLimit;
    property ClearCounterOnStart;
    property TagExt;
    property OnTimer;
    property OnRepeatCountLimitReached;
  end;





implementation


constructor TJppCustomTimer.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FRepeatCountLimit := 0;
  FCounter := 0;
  FClearCounterOnStart := True;
  FTagExt := TJppTagExt.Create(Self);
end;

destructor TJppCustomTimer.Destroy;
begin
  Enabled := False;
  FTagExt.Free;
  inherited Destroy;
end;

procedure TJppCustomTimer.Start;
begin
  if FClearCounterOnStart then FCounter := 0;
  Enabled := True;
end;

procedure TJppCustomTimer.Stop;
begin
  Enabled := False;
end;

{$IFDEF DCC}procedure TJppCustomTimer.Timer;{$ENDIF}
{$IFDEF FPC}procedure TJppCustomTimer.DoOnTimer;{$ENDIF}
begin
  if (FRepeatCountLimit > 0) and (FCounter >= FRepeatCountLimit) then
  begin
    Stop;
    if Assigned(FOnRepeatCountLimitReached) then FOnRepeatCountLimitReached(Self);
    Exit;
  end;

  Inc(FCounter);
  if Assigned(OnTimer) then OnTimer(Self);

  if (FRepeatCountLimit > 0) and (FCounter >= FRepeatCountLimit) then
  begin
    Stop;
    if Assigned(FOnRepeatCountLimitReached) then FOnRepeatCountLimitReached(Self);
  end;
end;

procedure TJppCustomTimer.SetClearCounterOnStart(const Value: Boolean);
begin
  FClearCounterOnStart := Value;
end;

procedure TJppCustomTimer.SetOnRepeatCountLimitReached(const Value: TNotifyEvent);
begin
  FOnRepeatCountLimitReached := Value;
end;

procedure TJppCustomTimer.SetRepeatCountLimit(const Value: Dword);
begin
  FRepeatCountLimit := Value;
end;

procedure TJppCustomTimer.SetTagExt(const Value: TJppTagExt);
begin
  FTagExt := Value;
end;



end.
