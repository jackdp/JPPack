unit JPP.Types;

{$I jpp.inc}
{$IFDEF FPC} {$mode objfpc}{$H+} {$ENDIF}

interface

uses
  SysUtils;

const

  {$IFDEF FPC}
  JPPackPageName = 'JPPackLCL';
  {$ELSE}
  JPPackPageName = 'JPPack';
  {$ENDIF}

  ENDL = sLineBreak;


  
implementation

end.
