unit JPP.Types;

{$IFDEF FPC} {$mode objfpc}{$H+} {$ENDIF}

interface

uses
  {$IFDEF DCC}
  Winapi.Windows, System.SysUtils;
  {$ELSE}
  SysUtils;
  {$ENDIF}



const

  {$IFDEF FPC}
  JPPackPageName = 'JPPackLCL';
  {$ELSE}
  JPPackPageName = 'JPPack';
  {$ENDIF}

  ENDL = sLineBreak;


  
implementation

end.
