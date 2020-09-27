unit JPP.Helpers;

{$I jpp.inc}
{$IFDEF FPC} {$mode delphi} {$ENDIF}

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


  TRectHelper = record helper for TRect
  private
    {$IFNDEF HAS_ADVANCED_TRECT}
    function GetWidth: integer;
    procedure SetWidth(const Value: integer);
    function GetHeight: integer;
    procedure SetHeight(const Value: integer);
    {$ENDIF}
  public
    {$IFNDEF HAS_ADVANCED_TRECT}
    constructor Create(const Origin: TPoint); overload;                              // empty rect at given origin
    constructor Create(const Origin: TPoint; Width, Height: Integer); overload;
    constructor Create(const Left, Top, Right, Bottom: Integer); overload;
    constructor Create(const P1, P2: TPoint; Normalize: Boolean = False); overload;
    constructor Create(const R: TRect; Normalize: Boolean = False); overload;

    class function Empty: TRect; inline; static;
    procedure NormalizeRect;

    function Contains(const Pt: TPoint): Boolean; overload;
    function Contains(const R: TRect): Boolean; overload;

    class function Union(const R1: TRect; const R2: TRect): TRect; overload; static;
    procedure Union(const R: TRect); overload;
    class function Union(const Points: array of TPoint): TRect; overload; static;

    procedure Offset(const DX, DY: Integer); overload;
    procedure Offset(const Point: TPoint); overload;

    procedure Inflate(const DX, DY: Integer); overload;
    procedure Inflate(const DL, DT, DR, DB: Integer); overload;
    {$ENDIF}

    function InflatedRect(const dx, dy: integer): TRect; overload;

    {$IFNDEF HAS_ADVANCED_TRECT}
    property Width: integer read GetWidth write SetWidth;
    property Height: integer read GetHeight write SetHeight;
    {$ENDIF}
  end;


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

procedure TSizeHelper.SetHeight(const Value: integer);
begin
  cy := Value;
end;

function TSizeHelper.GetWidth: integer;
begin
  Result := cx;
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

function TSizeHelper.GetHeight: integer;
begin
  Result := cy;
end;

function TSizeHelper.IsZero: Boolean;
begin
  Result := (cx = 0) and (cy = 0);
end;

procedure TSizeHelper.SetWidth(const Value: integer);
begin
  cx := Value;
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



{$region '               TRectHelper                '}

{$IFNDEF HAS_ADVANCED_TRECT}

constructor TRectHelper.Create(const Origin: TPoint; Width, Height: Integer);
begin
  Create(Origin.X, Origin.Y, Origin.X + Width, Origin.Y + Height);
end;

constructor TRectHelper.Create(const Origin: TPoint);
begin
  Create(Origin.X, Origin.Y, Origin.X, Origin.Y);
end;

constructor TRectHelper.Create(const Left, Top, Right, Bottom: Integer);
begin
  Self.Left := Left;
  Self.Top := Top;
  Self.Right := Right;
  Self.Bottom := Bottom;
end;

constructor TRectHelper.Create(const R: TRect; Normalize: Boolean);
begin
  Self.TopLeft := R.TopLeft;
  Self.BottomRight := R.BottomRight;
  if Normalize then Self.NormalizeRect;
end;

constructor TRectHelper.Create(const P1, P2: TPoint; Normalize: Boolean);
begin
  Self.TopLeft := P1;
  Self.BottomRight := P2;
  if Normalize then Self.NormalizeRect;
end;

class function TRectHelper.Empty: TRect;
begin
  Result := TRect.Create(0, 0, 0, 0);
end;

procedure TRectHelper.NormalizeRect;
begin
  if Top > Bottom then
  begin
    Top := Top xor Bottom;
    Bottom := Top xor Bottom;
    Top := Top xor Bottom;
  end;
  if Left > Right then
  begin
    Left := Left xor Right;
    Right:= Left xor Right;
    Left := Left xor Right;
  end
end;

function TRectHelper.Contains(const Pt: TPoint): Boolean;
begin
  Result := (Left <= Pt.X) and (Pt.X < Right) and (Top <= Pt.Y) and (Pt.Y < Bottom);
end;

function TRectHelper.Contains(const R: TRect): Boolean;
begin
  Result := Contains(R.TopLeft) and Contains(R.BottomRight);
end;

class function TRectHelper.Union(const R1, R2: TRect): TRect;
begin
  UnionRect(Result, R1, R2);
end;

procedure TRectHelper.Union(const R: TRect);
begin
  Self := Union(Self, R);
end;

class function TRectHelper.Union(const Points: array of TPoint): TRect;
var
  i: Integer;
begin
  if Length(Points) > 0 then
  begin
    Result.TopLeft := Points[Low(Points)];
    Result.BottomRight := Points[Low(Points)];

    for i := Low(Points) + 1 to High(Points) do
    begin
      if Points[i].X < Result.Left then Result.Left := Points[i].X;
      if Points[i].X > Result.Right then Result.Right := Points[i].X;
      if Points[i].Y < Result.Top then Result.Top := Points[i].Y;
      if Points[i].Y > Result.Bottom then Result.Bottom := Points[i].Y;
    end;
  end
  else
    Result := Empty;
end;

procedure TRectHelper.Offset(const DX, DY: Integer);
begin
  Inc(Left, DX);
  Inc(Right, DX);
  Inc(Top, DY);
  Inc(Bottom, DY);
end;

procedure TRectHelper.Offset(const Point: TPoint);
begin
  Inc(Left, Point.X);
  Inc(Right, Point.X);
  Inc(Top, Point.Y);
  Inc(Bottom, Point.Y);
end;

function TRectHelper.GetWidth: integer;
begin
  Result := Self.Right - Self.Left;
end;

procedure TRectHelper.Inflate(const DX, DY: Integer);
begin
  Dec(Left, DX);
  Dec(Top, DY);
  Inc(Right, DX);
  Inc(Bottom, DY);
end;

procedure TRectHelper.Inflate(const DL, DT, DR, DB: Integer);
begin
  Dec(Left, DL);
  Dec(Top, DT);
  Inc(Right, DR);
  Inc(Bottom, DB);
end;

procedure TRectHelper.SetWidth(const Value: integer);
begin
  Self.Right := Self.Left + Value;
end;

function TRectHelper.GetHeight: integer;
begin
  Result := Self.Bottom - Self.Top;
end;

procedure TRectHelper.SetHeight(const Value: integer);
begin
  Self.Bottom := Self.Top + Value;
end;

{$ENDIF} // HAS_ADVANCED_TRECT


function TRectHelper.InflatedRect(const dx, dy: integer): TRect;
begin
  Result.Left := Left - dx;
  Result.Right := Right + dx;
  Result.Top := Top - dy;
  Result.Bottom := Bottom + dy;
end;

{$endregion TRectHelper}



end.
