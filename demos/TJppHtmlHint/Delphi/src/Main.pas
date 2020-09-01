unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Types,
  Controls, Forms, Dialogs, StdCtrls, ExtCtrls, ActnList, Buttons,

  JPL.Strings, JPL.Strings.Ext,

  JPP.HtmlHint,

  JPP.SimplePanel, JPP.Common.Procs, JPP.Memo, JPP.Timer;

type
  TForm1 = class(TForm)
    ActionList1: TActionList;
    meHint: TJppMemo;
    pnTest: TJppSimplePanel;
    btnUpdateHint: TButton;
    actUpdateHint: TAction;
    actEsc: TAction;
    actSetLightMode: TAction;
    actSetDarkMode: TAction;
    actLoadFile: TAction;
    actSaveFile: TAction;
    btnLoad: TButton;
    btnSave: TButton;
    btnSetLightMode: TButton;
    btnSetDarkMode: TButton;
    tmMoveCursor: TJppTimer;
    htHint: TJppHtmlHint;
    procedure FormCreate(Sender: TObject);
    procedure actEscExecute(Sender: TObject);
    procedure actUpdateHintExecute(Sender: TObject);
    procedure actSetLightModeExecute(Sender: TObject);
    procedure actSetDarkModeExecute(Sender: TObject);
    procedure actLoadFileExecute(Sender: TObject);
    procedure actSaveFileExecute(Sender: TObject);
    procedure tmMoveCursorTimer(Sender: TObject);
  end;


const
  APP_NAME = 'TJppHtmlHint Demo';

var
  Form1: TForm1;

implementation

var
  HintFile: string = 'hint.txt';

{$R *.dfm}


procedure TForm1.FormCreate(Sender: TObject);
begin
  Caption := APP_NAME;
  Application.Title := APP_NAME;
  Application.HintHidePause := 4000000;

  meHint.Font.Name := GetFontName(['Fira Mono', 'Roboto Mono', 'Consolas']);
  actLoadFile.Execute;
  actUpdateHint.Execute;
end;



procedure TForm1.actEscExecute(Sender: TObject);
begin
  Close;
end;

procedure TForm1.actLoadFileExecute(Sender: TObject);
begin
  if not FileExists(HintFile) then JPL.Strings.Ext.SaveStringToFile(HintFile, meHint.Text, TEncoding.UTF8);
  if not FileExists(HintFile) then
  begin
    ShowMessage('File "' + HintFile + '" does not exists!');
    Exit;
  end;

  meHint.Lines.LoadFromFile(HintFile);
  actUpdateHint.Execute;
end;

procedure TForm1.actSaveFileExecute(Sender: TObject);
begin
  meHint.Lines.SaveToFile(HintFile, TEncoding.UTF8);
  actUpdateHint.Execute;
end;

procedure TForm1.actSetDarkModeExecute(Sender: TObject);
begin
  htHint.Appearance.ColorMode := hhcmDark;
end;

procedure TForm1.actSetLightModeExecute(Sender: TObject);
begin
  htHint.Appearance.ColorMode := hhcmLight;
end;

procedure TForm1.actUpdateHintExecute(Sender: TObject);
begin
  pnTest.Hint := meHint.Text;
  tmMoveCursor.Start;
end;


procedure TForm1.tmMoveCursorTimer(Sender: TObject);
var
  pt: TPoint;
begin
  pt := pnTest.ClientToScreen(Point(10, 10));
  SetCursorPos(pt.X, pt.Y);
  Application.ActivateHint(pt);
end;

end.
