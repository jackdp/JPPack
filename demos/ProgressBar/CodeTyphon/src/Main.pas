unit Main;

{$MODE Delphi}

interface

uses
  LCLIntf, LCLType,
  //LMessages, Messages,
  SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, ActnList, StdCtrls, JPP.ProgressBar, ExtCtrls,
  JPP.Timer;

type

  { TForm1 }

  TForm1 = class(TForm)
    Actions: TActionList;
    actEsc: TAction;
    btnPauseResume: TButton;
    Button1: TButton;
    pb3: TJppProgressBar;
    tmProgress: TJppTimer;
    pb1: TJppProgressBar;
    pb2: TJppProgressBar;
    lblProgress: TLabel;
    btnEnableDisable: TButton;
    lblMin: TLabel;
    lblMax: TLabel;
    pb4: TJppProgressBar;
    lblTextOnly: TLabel;
    pb5: TJppProgressBar;
    lblStage: TLabel;
    procedure actEscExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnPauseResumeClick(Sender: TObject);
    procedure tmProgressTimer(Sender: TObject);
    procedure pb1ProgressChanged(Sender: TObject);
    procedure btnEnableDisableClick(Sender: TObject);
    procedure pb5ProgressChanged(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

{$R *.frm}





procedure TForm1.FormCreate(Sender: TObject);
begin
  lblMin.Caption := 'Min = ' + IntToStr(pb1.Min);
  lblMax.Caption := 'Max = ' + IntToStr(pb1.Max);
  pb1.AnchoredControls.UpdateAllControlsPos;
end;

procedure TForm1.pb1ProgressChanged(Sender: TObject);
var
  pb: TJppProgressBar;
begin
  pb := Sender as TJppProgressBar;
  lblProgress.Caption := 'Position: ' + IntToStr(pb.Position) + ' / Percentage: ' + IntToStr(pb.PercentDone) + '% / Pixel pos: ' + IntToStr(pb.PixelPos) + ' pix';
end;

procedure TForm1.pb5ProgressChanged(Sender: TObject);
var
  s: string;
begin
  if pb5.PercentDone >= pb5.Appearance.FinalStage.PercentThreshold then s := 'Final stage ( >= ' + IntToStr(pb5.Appearance.FinalStage.PercentThreshold) + '% )'
  else if pb5.PercentDone >= pb5.Appearance.MidStage.PercentThreshold then s := 'Middle stage ( >= ' + IntToStr(pb5.Appearance.MidStage.PercentThreshold) + '% )'
  else s := 'Please wait...';
  lblStage.Caption := s;
end;

procedure TForm1.tmProgressTimer(Sender: TObject);
var
  Arr: array[0..4] of TJppProgressBar;
  i: integer;
  pb: TJppProgressBar;
begin
  Arr[0] := pb1;
  Arr[1] := pb2;
  Arr[2] := pb3;
  Arr[3] := pb4;
  Arr[4] := pb5;

  for i := Low(Arr) to High(Arr) do
  begin
    pb := Arr[i];
    if pb.Position >= pb.Max then pb.Position := pb.Min;
    pb.Position := pb.Position + 1;
  end;

//  if pb1.PercentDone > 98 then tmProgress.Interval := 1200
//  else if (pb1.PercentDone < 98) and (tmProgress.Interval = 1200) then tmProgress.Interval := 50;
end;

procedure TForm1.btnEnableDisableClick(Sender: TObject);
var
  Arr: array[0..4] of TJppProgressBar;
  i: integer;
  pb: TJppProgressBar;
begin
  Arr[0] := pb1;
  Arr[1] := pb2;
  Arr[2] := pb3;
  Arr[3] := pb4;
  Arr[4] := pb5;

  for i := Low(Arr) to High(Arr) do
  begin
    pb := Arr[i];
    pb.Enabled := not pb.Enabled;
  end;
end;

procedure TForm1.btnPauseResumeClick(Sender: TObject);
begin
  if tmProgress.Enabled then tmProgress.Stop else tmProgress.Start;
end;

procedure TForm1.actEscExecute(Sender: TObject);
begin
  Close;
end;

end.
