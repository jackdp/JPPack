unit JPP.Helpers;

{$I jpp.inc}
{$IFDEF FPC}
  {$mode delphi}
  {$MODESWITCH ADVANCEDRECORDS}
{$ENDIF}

interface

uses
  {$IFDEF MSWINDOWS}Windows,{$ENDIF}
  SysUtils, Types;


type

  TSizeHelper = record helper for TSize
  private
    {$IFNDEF HAS_ADVANCED_TSIZE}
    procedure SetWidth(const Value: integer);
    function GetWidth: integer;
    function GetHeight: integer;
    procedure SetHeight(const Value: integer);
    {$ENDIF}
  public
    {$IFNDEF HAS_ADVANCED_TSIZE}
    constructor Create(const P: TSmallPoint); overload;
    constructor Create(const X, Y: Integer); overload;

    function Add(const Point: TSize): TSize; overload;
    function Distance(const P2: TSize): Double;
    function IsZero: Boolean;
    function Subtract(const Point: TSize): TSize; overload;
    {$ENDIF}

    function Add(const X, Y: integer): TSize; overload;
    function Subtract(const X, Y: integer): TSize; overload;
    procedure SetZero;
    function AsString: string;

    {$IFNDEF HAS_ADVANCED_TSIZE}
    property Width: integer read GetWidth write SetWidth;
    property Height: integer read GetHeight write SetHeight;
    {$ENDIF}
  end;

  {$IFNDEF HAS_ADVANCED_TPOINT}
  TPointHelper = record helper for TPoint
  public
    constructor Create(P: TPoint); overload;
    constructor Create(const X, Y: Integer); overload;
    procedure Offset(const DX, DY: Integer); overload;
    procedure Offset(const Point: TPoint); overload;
  end;
  {$ENDIF}



implementation


{$region '                 TSizeHelper                     '}

{$IFNDEF HAS_ADVANCED_TSIZE}
constructor TSizeHelper.Create(const P: TSmallPoint);
begin
  cx := P.x;
  cy := P.y;
end;

constructor TSizeHelper.Create(const X, Y: Integer);
begin
  cx := X;
  cy := Y;
end;

function TSizeHelper.GetHeight: integer;
begin
  Result := cy;
end;

procedure TSizeHelper.SetHeight(const Value: integer);
begin
  cy := Value;
end;

function TSizeHelper.GetWidth: integer;
begin
  Result := cx;
end;

procedure TSizeHelper.SetWidth(const Value: integer);
begin
  cx := Value;
end;

function TSizeHelper.Add(const Point: TSize): TSize;
begin
  Result.cx := cx + Point.cx;
  Result.cy := cy + Point.cy;
end;

function TSizeHelper.Distance(const P2: TSize): Double;
begin
  Result := Sqrt(Sqr(0.0 + Self.cx - P2.cx) + Sqr(0.0 + Self.cy - P2.cy));
end;

function TSizeHelper.IsZero: Boolean;
begin
  Result := (cx = 0) and (cy = 0);
end;

function TSizeHelper.Subtract(const Point: TSize): TSize;
begin
  Result.cx := cx - Point.cx;
  Result.cy := cy - Point.cy;
end;
{$ENDIF} // HAS_ADVANCED_TSIZE


function TSizeHelper.Add(const X, Y: integer): TSize;
begin
  Result.cx := cx + X;
  Result.cy := cy + Y;
end;

function TSizeHelper.Subtract(const X, Y: integer): TSize;
begin
  Result.cx := cx - X;
  Result.cy := cy - Y;
end;

procedure TSizeHelper.SetZero;
begin
  cx := 0;
  cy := 0;
end;

function TSizeHelper.AsString: string;
begin
  Result := IntToStr(cx) + ',' + IntToStr(cy);
end;

{$endregion TSizeHelper}



{$IFNDEF HAS_ADVANCED_TPOINT}
constructor TPointHelper.Create(P: TPoint);
begin
  Self.X := P.X;
  Self.Y := P.Y;
end;

constructor TPointHelper.Create(const X, Y: Integer);
begin
  Self.X := X;
  Self.Y := Y;
end;

procedure TPointHelper.Offset(const DX, DY: Integer);
begin
  Inc(Self.X, DX);
  Inc(Self.Y, DY);
end;

procedure TPointHelper.Offset(const Point: TPoint);
begin
  Self.Offset(Point.X, Point.Y);
end;
{$ENDIF} // HAS_ADVANCED_TPOINT


end.
