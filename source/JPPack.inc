
// --------------- Free Pascal -------------------
{$IFDEF FPC}

  {$I JPPack_FPC.inc}

{$ENDIF}



// --------------------- Delphi ----------------------
{$IFDEF DCC}

  {$DEFINE USE_GDIPLUS_CONTROLS}

  {$IFDEF VER200}
    {$DEFINE DELPHI2009_OR_ABOVE}
  {$ENDIF}

{$ENDIF} // DCC
