program JppProgressBarDemo;

{$MODE Delphi}

{$IFDEF DCC}
  {$IF CompilerVersion >= 21.0} // >= Delphi 2010
    {$WEAKLINKRTTI ON}
    {$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
    {$SetPEFlags 1}   // IMAGE_FILE_RELOCS_STRIPPED
  {$IFEND}
{$ENDIF}


{$SetPEFlags $20} // IMAGE_FILE_LARGE_ADDRESS_AWARE

uses
  Forms, Interfaces,
  Main in 'Main.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  {$IFDEF MSWINDOWS}Application.{%H-}MainFormOnTaskbar := True;{$ENDIF}
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
