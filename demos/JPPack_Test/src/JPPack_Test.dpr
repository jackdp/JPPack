program JPPack_Test;

{$IF CompilerVersion >= 21.0}
  {$WEAKLINKRTTI ON}
  {$RTTI EXPLICIT METHODS([]) PROPERTIES([]) FIELDS([])}
{$IFEND}

{$SetPEFlags 1} // IMAGE_FILE_RELOCS_STRIPPED
{$SetPeFlags $20} // IMAGE_FILE_LARGE_ADDRESS_AWARE

uses
  Vcl.Forms,
  Main in 'Main.pas' {FormMain};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
