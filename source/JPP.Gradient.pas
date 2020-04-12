unit JPP.Gradient;


{
  Gradient procedures from Cindy Components: http://sourceforge.net/projects/tcycomponents/    (2015.01.16)
  VCL.cyGraphics
  VCL.cyTypes
}

{$I jpp.inc}
{$IFDEF FPC} {$mode objfpc}{$H+} {$ENDIF}

interface

uses
  {$IFDEF MSWINDOWS}Windows,{$ENDIF}
  SysUtils, Classes, Graphics
  {$IFDEF HAS_SYSTEM_UITYPES}, System.UITypes{$ENDIF}
  {$IFDEF FPC}, LCLType, LCLIntf{$ENDIF}
  ;

const
  GRADIENT_BALANCE = 50;
  GRADIENT_SPEED_PERCENT = 100;

type
  TJppGradientType = (gtVertical, gtHorizontal, gtDiagonal, gtDiagonal2, gtVerticalBar, gtHorizontalBar, gtDiagonalBar, gtDiagonalBar2, gtRadial, gtTopBar,
    gtBottomBar);

procedure JppGradientFill(Canvas: TCanvas; StartColor, EndColor: TColor; GradientType: TJppGradientType; Steps: Byte = 255); overload;
procedure JppGradientFill(Canvas: TCanvas; Rect: TRect; StartColor, EndColor: TColor; GradientType: TJppGradientType; Steps: Byte = 255); overload;


{$REGION ' --------------- CINDY CODE ---------------------- '}
type

  TDgradOrientation = (dgdVertical, dgdHorizontal, dgdAngle, dgdRadial, dgdRectangle);
  TDgradOrientationShape = (osRadial, osRectangle);
  TDgradBalanceMode = (bmNormal, bmMirror, bmReverse, bmReverseFromColor, bmInvertReverse, bmInvertReverseFromColor);

procedure cyGradientFill(aCanvas: TCanvas; aRect: TRect; fromColor, toColor: TColor; adgradOrientation: TDgradOrientation; Balance, AngleDegree: Word;
  balanceMode: TDgradBalanceMode; Maxdegrade: Byte; SpeedPercent: Integer; const AngleClipRect: Boolean = true; const AngleBuffer: TBitmap = Nil);
procedure MakeRotation(Bmp: TBitmap; const AngleDegree: Word);
procedure GradientFillHorizontal(aCanvas: TCanvas; aRect: TRect; fromColor, toColor: TColor; MaxDegrad: Byte);
procedure GradientFillVertical(aCanvas: TCanvas; aRect: TRect; fromColor, toColor: TColor; MaxDegrad: Byte);
procedure GradientFillAngle(aCanvas: TCanvas; aRect: TRect; fromColor, toColor: TColor; MaxDegrad: Byte; AngleDegree: Word; const ClipRect: Boolean = true; const Buffer: TBitmap = Nil);
procedure GradientFillShape(aCanvas: TCanvas; aRect: TRect; fromColor, toColor: TColor; MaxDegrad: Byte; toRect: TRect; OrientationShape: TDgradOrientationShape);

procedure InflateRectPercent(var aRect: TRect; withPercent: Double);
function CombineRectKeepingCenterPosition(RectPos, AddRect: TRect): TRect;
function GetIntermediateRect(Rect1, Rect2: TRect; Percent: Double): TRect;
{$ENDREGION}

implementation

function DegToRad(const Degrees: Single): Single; { Radians := Degrees * PI / 180 }
begin
  Result := Degrees * (PI / 180);
end;

{$REGION ' --------------------- JppGradientFill ------------------------- '}
procedure JppGradientFill(Canvas: TCanvas; Rect: TRect; StartColor, EndColor: TColor; GradientType: TJppGradientType; Steps: Byte); overload;
var
  Orientation: TDgradOrientation;
  balanceMode: TDgradBalanceMode;
  Angle: Byte;
  Balance: Word;
begin
  case GradientType of
    gtHorizontalBar, gtVerticalBar, gtDiagonalBar, gtDiagonalBar2: balanceMode := bmMirror;
  else
    balanceMode := bmNormal;
  end;

  case GradientType of
    gtHorizontal, gtVerticalBar: Orientation := dgdHorizontal;
    gtRadial: Orientation := dgdRadial;
    gtDiagonal, gtDiagonal2, gtDiagonalBar, gtDiagonalBar2: Orientation := dgdAngle;
  else
    Orientation := dgdVertical;
  end;

  case GradientType of
    gtDiagonal2, gtDiagonalBar2: Angle := 135;
  else
    Angle := 45;
  end;

  case GradientType of
    gtTopBar: Balance := 30;
    gtBottomBar: Balance := 70;
  else
    Balance := GRADIENT_BALANCE;
  end;

  if StartColor = EndColor then
  begin
    Canvas.Brush.Style := bsSolid;
    Canvas.Brush.Color := StartColor;
    Canvas.FillRect(Rect);
  end
  else cyGradientFill(Canvas, Rect, StartColor, EndColor, Orientation, Balance, Angle, balanceMode, Steps, GRADIENT_SPEED_PERCENT);
end;

procedure JppGradientFill(Canvas: TCanvas; StartColor, EndColor: TColor; GradientType: TJppGradientType; Steps: Byte); overload;
begin
  JppGradientFill(Canvas, Canvas.ClipRect, StartColor, EndColor, GradientType, Steps);
end;
{$ENDREGION JppGradientFill}


{$REGION ' ------------- CINDY CODE -------------------- '}

  {$REGION ' --------------------------------------- CINDY - cyGradientFill - MAIN PROCEDURE ---------------------------------------- '}

procedure cyGradientFill(aCanvas: TCanvas; aRect: TRect; fromColor, toColor: TColor; adgradOrientation: TDgradOrientation; Balance, AngleDegree: Word;
  balanceMode: TDgradBalanceMode; Maxdegrade: Byte; SpeedPercent: Integer; const AngleClipRect: Boolean = true; const AngleBuffer: TBitmap = Nil);
var
  IntermediateRect, Eye: TRect;
  fromColorRGB, toColorRGB: Integer;
  MaxDegradeBalance1, MaxDegradeBalance2: Byte;
  Arr_StartRGB, Arr_Med1RGB, Arr_Med2RGB, Arr_EndRGB: Array [0 .. 2] of Byte;
  ProgressionRGB: Array [0 .. 2] of SmallInt;

  // Variables for gradient angle design :
  aBmp: TBitmap;
  Size: Integer;
  Rgn: HRGN;
  P: TPoint;
begin
  if (Balance = 50) and (balanceMode = bmNormal) and (SpeedPercent = 100) then
  begin
    case adgradOrientation of
      dgdVertical: GradientFillVertical(aCanvas, aRect, fromColor, toColor, Maxdegrade);

      dgdHorizontal: GradientFillHorizontal(aCanvas, aRect, fromColor, toColor, Maxdegrade);

      dgdAngle:
        begin
          if AngleBuffer = Nil then aBmp := TBitmap.Create
          else aBmp := AngleBuffer;

          GradientFillAngle(aCanvas, aRect, fromColor, toColor, Maxdegrade, AngleDegree, AngleClipRect, aBmp);

          if AngleBuffer = Nil then aBmp.Free;
        end;

      dgdRadial:
        begin
          Eye := aRect;
          InflateRectPercent(Eye, -1); // Inflate Rect -100%
          aCanvas.Brush.Color := fromColor;
          aCanvas.FillRect(aRect);
        // Retrieve Rect from witch, we will draw the radial :
          aRect := CombineRectKeepingCenterPosition(Eye, aRect);
          GradientFillShape(aCanvas, aRect, fromColor, toColor, Maxdegrade, Eye, osRadial);
        end;

      dgdRectangle:
        begin
          Eye := aRect;
          InflateRectPercent(Eye, -1); // Inflate Rect -100%
          aCanvas.Brush.Color := fromColor;
          aCanvas.FillRect(aRect);
          GradientFillShape(aCanvas, aRect, fromColor, toColor, Maxdegrade, Eye, osRectangle);
        end;
    end;
  end
  else
  begin // Execute 2 Fills because of balance :
    MaxDegradeBalance1 := MulDiv(Maxdegrade, Balance, 100);
    MaxDegradeBalance2 := Maxdegrade - MaxDegradeBalance1;

    if balanceMode <> bmMirror then
    begin
      // Get Start and end color:
      fromColorRGB := ColorToRGB(fromColor);
      toColorRGB := ColorToRGB(toColor);

      Arr_StartRGB[0] := GetRValue(fromColorRGB);
      Arr_StartRGB[1] := GetGValue(fromColorRGB);
      Arr_StartRGB[2] := GetBValue(fromColorRGB);

      Arr_EndRGB[0] := GetRValue(toColorRGB);
      Arr_EndRGB[1] := GetGValue(toColorRGB);
      Arr_EndRGB[2] := GetBValue(toColorRGB);

      // Calc the 2 intermediate colors :
      ProgressionRGB[0] := MulDiv(Arr_EndRGB[0] - Arr_StartRGB[0], SpeedPercent - 50, 100);
      ProgressionRGB[1] := MulDiv(Arr_EndRGB[1] - Arr_StartRGB[1], SpeedPercent - 50, 100);
      ProgressionRGB[2] := MulDiv(Arr_EndRGB[2] - Arr_StartRGB[2], SpeedPercent - 50, 100);

      Arr_Med1RGB[0] := Arr_StartRGB[0] + ProgressionRGB[0];
      Arr_Med1RGB[1] := Arr_StartRGB[1] + ProgressionRGB[1];
      Arr_Med1RGB[2] := Arr_StartRGB[2] + ProgressionRGB[2];

      Arr_Med2RGB[0] := Arr_EndRGB[0] - ProgressionRGB[0];
      Arr_Med2RGB[1] := Arr_EndRGB[1] - ProgressionRGB[1];
      Arr_Med2RGB[2] := Arr_EndRGB[2] - ProgressionRGB[2];
    end;

    case adgradOrientation of
      dgdVertical:
        begin
          IntermediateRect := Rect(aRect.Left, aRect.Top, aRect.Right, aRect.Top + MulDiv(Balance, aRect.Bottom - aRect.Top, 100));

          case balanceMode of
            bmNormal:
              begin
                GradientFillVertical(aCanvas, IntermediateRect, fromColor, RGB(Arr_Med1RGB[0], Arr_Med1RGB[1], Arr_Med1RGB[2]), MaxDegradeBalance1);
                IntermediateRect.Top := IntermediateRect.Bottom;
                IntermediateRect.Bottom := aRect.Bottom;
                GradientFillVertical(aCanvas, IntermediateRect, RGB(Arr_Med2RGB[0], Arr_Med2RGB[1], Arr_Med2RGB[2]), toColor, MaxDegradeBalance2);
              end;

            bmMirror:
              begin
                GradientFillVertical(aCanvas, IntermediateRect, fromColor, toColor, MaxDegradeBalance1);
                IntermediateRect.Top := IntermediateRect.Bottom;
                IntermediateRect.Bottom := aRect.Bottom;
                GradientFillVertical(aCanvas, IntermediateRect, toColor, fromColor, MaxDegradeBalance2);
              end;

            bmReverse:
              begin
                GradientFillVertical(aCanvas, IntermediateRect, fromColor, RGB(Arr_Med1RGB[0], Arr_Med1RGB[1], Arr_Med1RGB[2]), MaxDegradeBalance1);
                IntermediateRect.Top := IntermediateRect.Bottom;
                IntermediateRect.Bottom := aRect.Bottom;
                GradientFillVertical(aCanvas, IntermediateRect, toColor, RGB(Arr_Med2RGB[0], Arr_Med2RGB[1], Arr_Med2RGB[2]), MaxDegradeBalance2);
              end;

            bmReverseFromColor:
              begin
                GradientFillVertical(aCanvas, IntermediateRect, fromColor, RGB(Arr_Med1RGB[0], Arr_Med1RGB[1], Arr_Med1RGB[2]), MaxDegradeBalance1);
                IntermediateRect.Top := IntermediateRect.Bottom;
                IntermediateRect.Bottom := aRect.Bottom;
                GradientFillVertical(aCanvas, IntermediateRect, toColor, fromColor, MaxDegradeBalance2);
              end;

            bmInvertReverse:
              begin
                GradientFillVertical(aCanvas, IntermediateRect, RGB(Arr_Med2RGB[0], Arr_Med2RGB[1], Arr_Med2RGB[2]), toColor, MaxDegradeBalance1);
                IntermediateRect.Top := IntermediateRect.Bottom;
                IntermediateRect.Bottom := aRect.Bottom;
                GradientFillVertical(aCanvas, IntermediateRect, RGB(Arr_Med1RGB[0], Arr_Med1RGB[1], Arr_Med1RGB[2]), fromColor, MaxDegradeBalance2);
              end;

            bmInvertReverseFromColor:
              begin
                GradientFillVertical(aCanvas, IntermediateRect, fromColor, toColor, MaxDegradeBalance1);
                IntermediateRect.Top := IntermediateRect.Bottom;
                IntermediateRect.Bottom := aRect.Bottom;
                GradientFillVertical(aCanvas, IntermediateRect, RGB(Arr_Med1RGB[0], Arr_Med1RGB[1], Arr_Med1RGB[2]), fromColor, MaxDegradeBalance2);
              end;
          end;
        end;

      dgdHorizontal:
        begin
          IntermediateRect := Rect(aRect.Left, aRect.Top, aRect.Left + MulDiv(Balance, aRect.Right - aRect.Left, 100), aRect.Bottom);

          case balanceMode of
            bmNormal:
              begin
                GradientFillHorizontal(aCanvas, IntermediateRect, fromColor, RGB(Arr_Med1RGB[0], Arr_Med1RGB[1], Arr_Med1RGB[2]), MaxDegradeBalance1);
                IntermediateRect.Left := IntermediateRect.Right;
                IntermediateRect.Right := aRect.Right;
                GradientFillHorizontal(aCanvas, IntermediateRect, RGB(Arr_Med2RGB[0], Arr_Med2RGB[1], Arr_Med2RGB[2]), toColor, MaxDegradeBalance2);
              end;

            bmMirror:
              begin
                GradientFillHorizontal(aCanvas, IntermediateRect, fromColor, toColor, MaxDegradeBalance1);
                IntermediateRect.Left := IntermediateRect.Right;
                IntermediateRect.Right := aRect.Right;
                GradientFillHorizontal(aCanvas, IntermediateRect, toColor, fromColor, MaxDegradeBalance2);
              end;

            bmReverse:
              begin
                GradientFillHorizontal(aCanvas, IntermediateRect, fromColor, RGB(Arr_Med1RGB[0], Arr_Med1RGB[1], Arr_Med1RGB[2]), MaxDegradeBalance1);
                IntermediateRect.Left := IntermediateRect.Right;
                IntermediateRect.Right := aRect.Right;
                GradientFillHorizontal(aCanvas, IntermediateRect, toColor, RGB(Arr_Med2RGB[0], Arr_Med2RGB[1], Arr_Med2RGB[2]), MaxDegradeBalance2);
              end;

            bmReverseFromColor:
              begin
                GradientFillHorizontal(aCanvas, IntermediateRect, fromColor, RGB(Arr_Med1RGB[0], Arr_Med1RGB[1], Arr_Med1RGB[2]), MaxDegradeBalance1);
                IntermediateRect.Left := IntermediateRect.Right;
                IntermediateRect.Right := aRect.Right;
                GradientFillHorizontal(aCanvas, IntermediateRect, toColor, fromColor, MaxDegradeBalance2);
              end;

            bmInvertReverse:
              begin
                GradientFillHorizontal(aCanvas, IntermediateRect, RGB(Arr_Med2RGB[0], Arr_Med2RGB[1], Arr_Med2RGB[2]), toColor, MaxDegradeBalance1);
                IntermediateRect.Left := IntermediateRect.Right;
                IntermediateRect.Right := aRect.Right;
                GradientFillHorizontal(aCanvas, IntermediateRect, RGB(Arr_Med1RGB[0], Arr_Med1RGB[1], Arr_Med1RGB[2]), fromColor, MaxDegradeBalance2);
              end;

            bmInvertReverseFromColor:
              begin
                GradientFillHorizontal(aCanvas, IntermediateRect, fromColor, toColor, MaxDegradeBalance1);
                IntermediateRect.Left := IntermediateRect.Right;
                IntermediateRect.Right := aRect.Right;
                GradientFillHorizontal(aCanvas, IntermediateRect, RGB(Arr_Med1RGB[0], Arr_Med1RGB[1], Arr_Med1RGB[2]), fromColor, MaxDegradeBalance2);
              end;
          end;
        end;

      dgdAngle:
        begin
          Size := Round(Sqrt(Sqr(aRect.Right - aRect.Left) + Sqr(aRect.Bottom - aRect.Top)));
          IntermediateRect := Rect(0, 0, MulDiv(Balance, Size, 100), Size);

          if AngleBuffer = Nil then aBmp := TBitmap.Create
          else aBmp := AngleBuffer;

          try
            if aBmp.Width <> Size then aBmp.Width := Size;
            if aBmp.Height <> Size then aBmp.Height := Size;

            case balanceMode of
              bmNormal:
                begin
                  GradientFillHorizontal(aBmp.Canvas, IntermediateRect, fromColor, RGB(Arr_Med1RGB[0], Arr_Med1RGB[1], Arr_Med1RGB[2]), MaxDegradeBalance1);
                  IntermediateRect.Left := IntermediateRect.Right;
                  IntermediateRect.Right := Size;
                  GradientFillHorizontal(aBmp.Canvas, IntermediateRect, RGB(Arr_Med2RGB[0], Arr_Med2RGB[1], Arr_Med2RGB[2]), toColor, MaxDegradeBalance2);
                end;

              bmMirror:
                begin
                  GradientFillHorizontal(aBmp.Canvas, IntermediateRect, fromColor, toColor, MaxDegradeBalance1);
                  IntermediateRect.Left := IntermediateRect.Right;
                  IntermediateRect.Right := Size;
                  GradientFillHorizontal(aBmp.Canvas, IntermediateRect, toColor, fromColor, MaxDegradeBalance2);
                end;

              bmReverse:
                begin
                  GradientFillHorizontal(aBmp.Canvas, IntermediateRect, fromColor, RGB(Arr_Med1RGB[0], Arr_Med1RGB[1], Arr_Med1RGB[2]), MaxDegradeBalance1);
                  IntermediateRect.Left := IntermediateRect.Right;
                  IntermediateRect.Right := Size;
                  GradientFillHorizontal(aBmp.Canvas, IntermediateRect, toColor, RGB(Arr_Med2RGB[0], Arr_Med2RGB[1], Arr_Med2RGB[2]), MaxDegradeBalance2);
                end;

              bmReverseFromColor:
                begin
                  GradientFillHorizontal(aBmp.Canvas, IntermediateRect, fromColor, RGB(Arr_Med1RGB[0], Arr_Med1RGB[1], Arr_Med1RGB[2]), MaxDegradeBalance1);
                  IntermediateRect.Left := IntermediateRect.Right;
                  IntermediateRect.Right := Size;
                  GradientFillHorizontal(aBmp.Canvas, IntermediateRect, toColor, fromColor, MaxDegradeBalance2);
                end;

              bmInvertReverse:
                begin
                  GradientFillHorizontal(aBmp.Canvas, IntermediateRect, RGB(Arr_Med2RGB[0], Arr_Med2RGB[1], Arr_Med2RGB[2]), toColor, MaxDegradeBalance1);
                  IntermediateRect.Left := IntermediateRect.Right;
                  IntermediateRect.Right := Size;
                  GradientFillHorizontal(aBmp.Canvas, IntermediateRect, RGB(Arr_Med1RGB[0], Arr_Med1RGB[1], Arr_Med1RGB[2]), fromColor, MaxDegradeBalance2);
                end;

              bmInvertReverseFromColor:
                begin
                  GradientFillHorizontal(aBmp.Canvas, IntermediateRect, fromColor, toColor, MaxDegradeBalance1);
                  IntermediateRect.Left := IntermediateRect.Right;
                  IntermediateRect.Right := Size;
                  GradientFillHorizontal(aBmp.Canvas, IntermediateRect, RGB(Arr_Med1RGB[0], Arr_Med1RGB[1], Arr_Med1RGB[2]), fromColor, MaxDegradeBalance2);
                end;
            end;

          // Rotate the bitmap :
            MakeRotation(aBmp, AngleDegree);

          // Draw rotated bitmap :
            Rgn := 0;
            if AngleClipRect then
            begin
              Rgn := CreateRectRgn(aRect.Left, aRect.Top, aRect.Right, aRect.Bottom);
              GetWindowOrgEx(aCanvas.Handle, P);
              OffsetRgn(Rgn, -P.X, -P.Y);
              SelectClipRgn(aCanvas.Handle, Rgn);
            end;
            aCanvas.Draw(aRect.Left + ((aRect.Right - aRect.Left) - Size) div 2, aRect.Top + ((aRect.Bottom - aRect.Top) - Size) div 2, aBmp);
            if AngleClipRect then
            begin
              SelectClipRgn(aCanvas.Handle, HRGN(nil));
              DeleteObject(Rgn);
            end;
          except

          end;

          if AngleBuffer = Nil then aBmp.Free;
        end;

      dgdRadial:
        begin
          Eye := aRect;
          InflateRectPercent(Eye, -1); // Inflate Rect -100%

          aCanvas.Brush.Color := fromColor;
          aCanvas.FillRect(aRect);

        // Retrieve Rect from witch, we will draw the radial :
          aRect := CombineRectKeepingCenterPosition(Eye, aRect);

          IntermediateRect := GetIntermediateRect(aRect, Eye, Balance / 100);
          InflateRectPercent(IntermediateRect, (-1) * Balance div 100);

          case balanceMode of
            bmNormal:
              begin
                GradientFillShape(aCanvas, aRect, fromColor, RGB(Arr_Med1RGB[0], Arr_Med1RGB[1], Arr_Med1RGB[2]), MaxDegradeBalance1, IntermediateRect,
                  osRadial);
                GradientFillShape(aCanvas, IntermediateRect, RGB(Arr_Med2RGB[0], Arr_Med2RGB[1], Arr_Med2RGB[2]), toColor, MaxDegradeBalance2, Eye, osRadial);
              end;

            bmMirror:
              begin
                GradientFillShape(aCanvas, aRect, fromColor, toColor, MaxDegradeBalance1, IntermediateRect, osRadial);
                GradientFillShape(aCanvas, IntermediateRect, toColor, fromColor, MaxDegradeBalance2, Eye, osRadial);
              end;

            bmReverse:
              begin
                GradientFillShape(aCanvas, aRect, fromColor, RGB(Arr_Med1RGB[0], Arr_Med1RGB[1], Arr_Med1RGB[2]), MaxDegradeBalance1, IntermediateRect,
                  osRadial);
                GradientFillShape(aCanvas, IntermediateRect, toColor, RGB(Arr_Med2RGB[0], Arr_Med2RGB[1], Arr_Med2RGB[2]), MaxDegradeBalance2, Eye, osRadial);
              end;

            bmReverseFromColor:
              begin
                GradientFillShape(aCanvas, aRect, fromColor, RGB(Arr_Med1RGB[0], Arr_Med1RGB[1], Arr_Med1RGB[2]), MaxDegradeBalance1, IntermediateRect,
                  osRadial);
                GradientFillShape(aCanvas, IntermediateRect, toColor, fromColor, MaxDegradeBalance2, Eye, osRadial);
              end;

            bmInvertReverse:
              begin
                GradientFillShape(aCanvas, aRect, RGB(Arr_Med2RGB[0], Arr_Med2RGB[1], Arr_Med2RGB[2]), toColor, MaxDegradeBalance1, IntermediateRect, osRadial);
                GradientFillShape(aCanvas, IntermediateRect, RGB(Arr_Med1RGB[0], Arr_Med1RGB[1], Arr_Med1RGB[2]), fromColor, MaxDegradeBalance2, Eye, osRadial);
              end;

            bmInvertReverseFromColor:
              begin
                GradientFillShape(aCanvas, aRect, fromColor, toColor, MaxDegradeBalance1, IntermediateRect, osRadial);
                GradientFillShape(aCanvas, IntermediateRect, RGB(Arr_Med1RGB[0], Arr_Med1RGB[1], Arr_Med1RGB[2]), fromColor, MaxDegradeBalance2, Eye, osRadial);
              end;
          end;
        end;

      dgdRectangle:
        begin
          Eye := aRect;
          InflateRectPercent(Eye, -1); // Inflate Rect -100%
          aCanvas.Brush.Color := fromColor;
          aCanvas.FillRect(aRect);
        // Retrieve Rect from witch, we will end and begin the rectangle :
          IntermediateRect := GetIntermediateRect(aRect, Eye, Balance / 100);
          InflateRectPercent(IntermediateRect, (-1) * Balance div 100);

          case balanceMode of
            bmNormal:
              begin
                GradientFillShape(aCanvas, aRect, fromColor, RGB(Arr_Med1RGB[0], Arr_Med1RGB[1], Arr_Med1RGB[2]), MaxDegradeBalance1, IntermediateRect,
                  osRectangle);
                GradientFillShape(aCanvas, IntermediateRect, RGB(Arr_Med2RGB[0], Arr_Med2RGB[1], Arr_Med2RGB[2]), toColor, MaxDegradeBalance2, Eye,
                  osRectangle);
              end;

            bmMirror:
              begin
                GradientFillShape(aCanvas, aRect, fromColor, toColor, MaxDegradeBalance1, IntermediateRect, osRectangle);
                GradientFillShape(aCanvas, IntermediateRect, toColor, fromColor, MaxDegradeBalance2, Eye, osRectangle);
              end;

            bmReverse:
              begin
                GradientFillShape(aCanvas, aRect, fromColor, RGB(Arr_Med1RGB[0], Arr_Med1RGB[1], Arr_Med1RGB[2]), MaxDegradeBalance1, IntermediateRect,
                  osRectangle);
                GradientFillShape(aCanvas, IntermediateRect, toColor, RGB(Arr_Med2RGB[0], Arr_Med2RGB[1], Arr_Med2RGB[2]), MaxDegradeBalance2, Eye,
                  osRectangle);
              end;

            bmReverseFromColor:
              begin
                GradientFillShape(aCanvas, aRect, fromColor, RGB(Arr_Med1RGB[0], Arr_Med1RGB[1], Arr_Med1RGB[2]), MaxDegradeBalance1, IntermediateRect,
                  osRectangle);
                GradientFillShape(aCanvas, IntermediateRect, toColor, fromColor, MaxDegradeBalance2, Eye, osRectangle);
              end;

            bmInvertReverse:
              begin
                GradientFillShape(aCanvas, aRect, RGB(Arr_Med2RGB[0], Arr_Med2RGB[1], Arr_Med2RGB[2]), toColor, MaxDegradeBalance1, IntermediateRect,
                  osRectangle);
                GradientFillShape(aCanvas, IntermediateRect, RGB(Arr_Med1RGB[0], Arr_Med1RGB[1], Arr_Med1RGB[2]), fromColor, MaxDegradeBalance2, Eye,
                  osRectangle);
              end;

            bmInvertReverseFromColor:
              begin
                GradientFillShape(aCanvas, aRect, fromColor, toColor, MaxDegradeBalance1, IntermediateRect, osRectangle);
                GradientFillShape(aCanvas, IntermediateRect, RGB(Arr_Med1RGB[0], Arr_Med1RGB[1], Arr_Med1RGB[2]), fromColor, MaxDegradeBalance2, Eye,
                  osRectangle);
              end;
          end;
        end;
    end; // End case adgradOrientation of ...
  end;
end;
  {$ENDREGION CINDY - cyGradientFill}

  {$REGION ' --------------------------- CINDY - helper functions --------------------------------------- '}
function CombineRectKeepingCenterPosition(RectPos, AddRect: TRect): TRect;
var
  MaxDif, DifLeft, DifTop, DifRight, DifBottom: Integer;
begin
  // Calculate position diference between the 2 rects :
  DifLeft := abs(RectPos.Left - AddRect.Left);
  DifRight := abs(RectPos.Right - AddRect.Right);
  DifTop := abs(RectPos.Top - AddRect.Top);
  DifBottom := abs(RectPos.Bottom - AddRect.Bottom);

  if DifLeft > DifRight then MaxDif := DifLeft
  else MaxDif := DifRight;

  if DifTop > MaxDif then MaxDif := DifTop;

  if DifBottom > MaxDif then MaxDif := DifBottom;

  Result := Rect(RectPos.Left - MaxDif, RectPos.Top - MaxDif, RectPos.Right + MaxDif, RectPos.Bottom + MaxDif);
end;

procedure InflateRectPercent(var aRect: TRect; withPercent: Double);
var
  dx, dy: Integer;
begin
  dx := Round((aRect.Right - aRect.Left) * withPercent) div 2;
  dy := Round((aRect.Bottom - aRect.Top) * withPercent) div 2;
  InflateRect(aRect, dx, dy);
end;

function GetIntermediateRect(Rect1, Rect2: TRect; Percent: Double): TRect;
begin
  Result := Rect(Rect1.Left + Round((Rect2.Left - Rect1.Left) * Percent), Rect1.Top + Round((Rect2.Top - Rect1.Top) * Percent),
    Rect1.Right + Round((Rect2.Right - Rect1.Right) * Percent), Rect1.Bottom + Round((Rect2.Bottom - Rect1.Bottom) * Percent));
end;

{$IFNDEF MSWINDOWS}

  {$region ' ---------- from CodeTyphon 6.8 pl_Cindy - cyGraphics.pas ----------- '}
Const PixelMax = 32768;
Type
   pPixelArray = ^TPixelArray;
   TPixelArray = Array[0..PixelMax-1] Of TRGBTriple;

//=============== ct9999 for CodeTyphon Studio ===============================
Procedure RotateBitmap_ads(SourceBitmap : TBitmap; out DestBitmap : TBitmap; Center : TPoint; Angle : Double);
Var
   cosRadians : Double;
   inX : Integer;
   inXOriginal : Integer;
   inXPrime : Integer;
   inXPrimeRotated : Integer;
   inY : Integer;
   inYOriginal : Integer;
   inYPrime : Integer;
   inYPrimeRotated : Integer;
   OriginalRow : pPixelArray;
   Radians : Double;
   RotatedRow : pPixelArray;
   sinRadians : Double;
begin
   DestBitmap.Width := SourceBitmap.Width;
   DestBitmap.Height := SourceBitmap.Height;
   DestBitmap.PixelFormat := pf24bit;
   Radians := -(Angle) * PI / 180;
   sinRadians := Sin(Radians) ;
   cosRadians := Cos(Radians) ;
   For inX := DestBitmap.Height-1 Downto 0 Do
   Begin
     RotatedRow := DestBitmap.Scanline[inX];
     inXPrime := 2*(inX - Center.y) + 1;
     For inY := DestBitmap.Width-1 Downto 0 Do
     Begin
       inYPrime := 2*(inY - Center.x) + 1;
       inYPrimeRotated := Round(inYPrime * CosRadians - inXPrime * sinRadians) ;
       inXPrimeRotated := Round(inYPrime * sinRadians + inXPrime * cosRadians) ;
       inYOriginal := (inYPrimeRotated - 1) Div 2 + Center.x;
       inXOriginal := (inXPrimeRotated - 1) Div 2 + Center.y;
       If
         (inYOriginal >= 0) And
         (inYOriginal <= SourceBitmap.Width-1) And
         (inXOriginal >= 0) And
         (inXOriginal <= SourceBitmap.Height-1)
       Then
       Begin
         OriginalRow := SourceBitmap.Scanline[inXOriginal];
         RotatedRow[inY] := OriginalRow[inYOriginal]
       End
       Else
       Begin
         RotatedRow^[inY].rgbtBlue := 255;
         RotatedRow^[inY].rgbtGreen := 0;
         RotatedRow^[inY].rgbtRed := 0
       End;
     End;
   End;
End;
//=========================================== ct9999

procedure MakeRotation(Bmp: TBitmap; const AngleDegree: Word);
 var CenterP:Tpoint;
begin
  if Bmp=nil then exit;

  CenterP.x:=bmp.Width div 2;
  CenterP.y:=bmp.Height div 2;

  RotateBitmap_ads(Bmp,Bmp,CenterP,AngleDegree);
end;
  {$endregion from CodeTyphon 6.8 pl_Cindy - cyGraphics.pas}

{$ELSE}

procedure MakeRotation(Bmp: TBitmap; const AngleDegree: Word);
var
  Rad: Single;
  C: Single;
  S: Single;
  OffsetX: Single;
  OffsetY: Single;
  Points: array [0 .. 2] of TPoint;
begin
  Rad := DegToRad(AngleDegree);

  C := Cos(Rad);
  S := Sin(Rad);
  try
    OffsetX := (Bmp.Width - Bmp.Width * C + Bmp.Height * S) / 2;
    OffsetY := (Bmp.Height - Bmp.Width * S - Bmp.Height * C) / 2;

    Points[0].X := Round(OffsetX);
    Points[0].Y := Round(OffsetY);
    Points[1].X := Round(OffsetX + Bmp.Width * C);
    Points[1].Y := Round(OffsetY + Bmp.Width * S);
    Points[2].X := Round(OffsetX - Bmp.Height * S);
    Points[2].Y := Round(OffsetY + Bmp.Height * C);
    PlgBlt(Bmp.Canvas.Handle, Points, Bmp.Canvas.Handle, 0, 0, Bmp.Width, Bmp.Height, 0, 0, 0);
  finally
  end;
end;
{$ENDIF} // IFNDEF MSWINDOWS


  {$ENDREGION CINDY - helper functions}

  {$REGION ' -------------------------- CINDY - GradientFillXXX procs -------------------------------------------- '}

procedure GradientFillHorizontal(aCanvas: TCanvas; aRect: TRect; fromColor, toColor: TColor; MaxDegrad: Byte);
var
  aLimit: TRect;
  fromColorRGB, toColorRGB, i, nbDgrad: Integer;
  Arr_StartRGB: Array [0 .. 2] of Byte;
  Arr_DifRGB: Array [0 .. 2] of Integer;
  Arr_CurRGB: Array [0 .. 2] of Byte;
begin
  fromColorRGB := ColorToRGB(fromColor);
  toColorRGB := ColorToRGB(toColor);

  Arr_StartRGB[0] := GetRValue(fromColorRGB);
  Arr_StartRGB[1] := GetGValue(fromColorRGB);
  Arr_StartRGB[2] := GetBValue(fromColorRGB);

  Arr_DifRGB[0] := GetRValue(toColorRGB) - Arr_StartRGB[0];
  Arr_DifRGB[1] := GetGValue(toColorRGB) - Arr_StartRGB[1];
  Arr_DifRGB[2] := GetBValue(toColorRGB) - Arr_StartRGB[2];

  if aRect.Right - aRect.Left < MaxDegrad then nbDgrad := aRect.Right - aRect.Left
  else nbDgrad := MaxDegrad;

  with aCanvas do
  begin
    aLimit.Top := aRect.Top;
    aLimit.Bottom := aRect.Bottom;

    for i := 1 to nbDgrad do
    begin
      if i = 1 then aLimit.Left := aRect.Left
      else aLimit.Left := aLimit.Right;

      if i = nbDgrad then aLimit.Right := aRect.Right
      else aLimit.Right := aRect.Left + MulDiv(i + 1, aRect.Right - aRect.Left, nbDgrad);

      Arr_CurRGB[0] := Arr_StartRGB[0] + MulDiv(i, Arr_DifRGB[0], nbDgrad);
      Arr_CurRGB[1] := Arr_StartRGB[1] + MulDiv(i, Arr_DifRGB[1], nbDgrad);
      Arr_CurRGB[2] := Arr_StartRGB[2] + MulDiv(i, Arr_DifRGB[2], nbDgrad);

      Brush.Color := RGB(Arr_CurRGB[0], Arr_CurRGB[1], Arr_CurRGB[2]);
      FillRect(aLimit);
    end;
  end;
end;

procedure GradientFillVertical(aCanvas: TCanvas; aRect: TRect; fromColor, toColor: TColor; MaxDegrad: Byte);
var
  aLimit: TRect;
  fromColorRGB, toColorRGB, i, nbDgrad: Integer;
  Arr_StartRGB: Array [0 .. 2] of Byte;
  Arr_DifRGB: Array [0 .. 2] of Integer;
  Arr_CurRGB: Array [0 .. 2] of Byte;
begin
  fromColorRGB := ColorToRGB(fromColor);
  toColorRGB := ColorToRGB(toColor);

  Arr_StartRGB[0] := GetRValue(fromColorRGB);
  Arr_StartRGB[1] := GetGValue(fromColorRGB);
  Arr_StartRGB[2] := GetBValue(fromColorRGB);

  Arr_DifRGB[0] := GetRValue(toColorRGB) - Arr_StartRGB[0];
  Arr_DifRGB[1] := GetGValue(toColorRGB) - Arr_StartRGB[1];
  Arr_DifRGB[2] := GetBValue(toColorRGB) - Arr_StartRGB[2];

  if aRect.Bottom - aRect.Top < MaxDegrad then nbDgrad := aRect.Bottom - aRect.Top
  else nbDgrad := MaxDegrad;

  with aCanvas do
  begin
    aLimit.Left := aRect.Left;
    aLimit.Right := aRect.Right;

    for i := 1 to nbDgrad do
    begin
      if i = 1 then aLimit.Top := aRect.Top
      else aLimit.Top := aLimit.Bottom;

      if i = nbDgrad then aLimit.Bottom := aRect.Bottom
      else aLimit.Bottom := aRect.Top + MulDiv(i + 1, aRect.Bottom - aRect.Top, nbDgrad);

      Arr_CurRGB[0] := Arr_StartRGB[0] + MulDiv(i, Arr_DifRGB[0], nbDgrad);
      Arr_CurRGB[1] := Arr_StartRGB[1] + MulDiv(i, Arr_DifRGB[1], nbDgrad);
      Arr_CurRGB[2] := Arr_StartRGB[2] + MulDiv(i, Arr_DifRGB[2], nbDgrad);

      Brush.Color := RGB(Arr_CurRGB[0], Arr_CurRGB[1], Arr_CurRGB[2]);
      FillRect(aLimit);
    end;
  end;
end;

procedure GradientFillAngle(aCanvas: TCanvas; aRect: TRect; fromColor, toColor: TColor; MaxDegrad: Byte; AngleDegree: Word; const ClipRect: Boolean = true;
  const Buffer: TBitmap = Nil);
var
  Size: Integer;
  aBmp: TBitmap;
  Rgn: HRGN;
  P: TPoint;
begin
  Size := Round(Sqrt(Sqr(aRect.Right - aRect.Left) + Sqr(aRect.Bottom - aRect.Top)));

  if Buffer = Nil then aBmp := TBitmap.Create
  else aBmp := Buffer;

  try
    if aBmp.Width <> Size then aBmp.Width := Size;
    if aBmp.Height <> Size then aBmp.Height := Size;

    // Draw into the Bitmap.Canvas :
    GradientFillHorizontal(aBmp.Canvas, Rect(0, 0, Size, Size), fromColor, toColor, MaxDegrad);

    // Rotate the bitmap :
    MakeRotation(aBmp, AngleDegree);

    Rgn := 0;
    // Draw rotated bitmap :
    if ClipRect then
    begin
      Rgn := CreateRectRgn(aRect.Left, aRect.Top, aRect.Right, aRect.Bottom);
      GetWindowOrgEx(aCanvas.Handle, P);
      OffsetRgn(Rgn, -P.X, -P.Y);
      SelectClipRgn(aCanvas.Handle, Rgn);
    end;
    aCanvas.Draw(aRect.Left + ((aRect.Right - aRect.Left) - Size) div 2, aRect.Top + ((aRect.Bottom - aRect.Top) - Size) div 2, aBmp);
    if ClipRect then
    begin
      SelectClipRgn(aCanvas.Handle, HRGN(nil));
      DeleteObject(Rgn);
    end;
  except
  end;

  if Buffer = Nil then aBmp.Free;
end;

procedure GradientFillShape(aCanvas: TCanvas; aRect: TRect; fromColor, toColor: TColor; MaxDegrad: Byte; toRect: TRect;
  OrientationShape: TDgradOrientationShape);
var
  aLimit: TRect;
  InflateX, InflateY: Integer;
  fromColorRGB, toColorRGB, DifX, DifY, i, nbDgrad: Integer;
  Arr_StartRGB, Arr_EndRGB, Arr_CurRGB: Array [0 .. 2] of Byte;
  Arr_DifRGB: Array [0 .. 2] of Integer;
begin
  fromColorRGB := ColorToRGB(fromColor);
  toColorRGB := ColorToRGB(toColor);

  Arr_StartRGB[0] := GetRValue(fromColorRGB);
  Arr_StartRGB[1] := GetGValue(fromColorRGB);
  Arr_StartRGB[2] := GetBValue(fromColorRGB);

  Arr_EndRGB[0] := GetRValue(toColorRGB);
  Arr_EndRGB[1] := GetGValue(toColorRGB);
  Arr_EndRGB[2] := GetBValue(toColorRGB);

  Arr_DifRGB[0] := Arr_EndRGB[0] - Arr_StartRGB[0];
  Arr_DifRGB[1] := Arr_EndRGB[1] - Arr_StartRGB[1];
  Arr_DifRGB[2] := Arr_EndRGB[2] - Arr_StartRGB[2];

  DifX := abs((aRect.Right - aRect.Left) - (toRect.Right - toRect.Left));
  DifX := DifX div 2;
  DifY := abs((aRect.Bottom - aRect.Top) - (toRect.Bottom - toRect.Top));
  DifY := DifY div 2;

  if DifX > DifY then
  begin
    if DifX < MaxDegrad then nbDgrad := DifX + 1 // Because of DifX := DifX div 2 for small Rects ...
    else nbDgrad := MaxDegrad;
  end
  else
  begin
    if DifY < MaxDegrad then nbDgrad := DifY + 1
    else nbDgrad := MaxDegrad;
  end;

  with aCanvas do
  begin
    for i := 1 to nbDgrad do
    begin
      if i = 1 then
      begin
        aLimit := aRect;
        Arr_CurRGB[0] := Arr_StartRGB[0];
        Arr_CurRGB[1] := Arr_StartRGB[1];
        Arr_CurRGB[2] := Arr_StartRGB[2];
      end
      else if i = nbDgrad then
      begin
        aLimit := toRect;
        Arr_CurRGB[0] := Arr_EndRGB[0];
        Arr_CurRGB[1] := Arr_EndRGB[1];
        Arr_CurRGB[2] := Arr_EndRGB[2];

        if (aLimit.Right - aLimit.Left = 1) or (aLimit.Bottom - aLimit.Top = 1) then OrientationShape := osRectangle; // Draw 1 pixel or 1 line ...
      end
      else
      begin
        aLimit := aRect;
        InflateX := (-1) * MulDiv(i - 1, DifX, nbDgrad);
        InflateY := (-1) * MulDiv(i - 1, DifY, nbDgrad);
        InflateRect(aLimit, InflateX, InflateY);

        Arr_CurRGB[0] := Arr_StartRGB[0] + MulDiv(i - 1, Arr_DifRGB[0], nbDgrad);
        Arr_CurRGB[1] := Arr_StartRGB[1] + MulDiv(i - 1, Arr_DifRGB[1], nbDgrad);
        Arr_CurRGB[2] := Arr_StartRGB[2] + MulDiv(i - 1, Arr_DifRGB[2], nbDgrad);
      end;

      case OrientationShape of
        osRadial:
          begin
            Brush.Color := RGB(Arr_CurRGB[0], Arr_CurRGB[1], Arr_CurRGB[2]);
            Pen.Color := Brush.Color;
            Ellipse(aLimit);
          end;

        osRectangle:
          begin
            Brush.Color := RGB(Arr_CurRGB[0], Arr_CurRGB[1], Arr_CurRGB[2]);
            FillRect(aLimit);
          end;
      end;
    end;
  end;
end;
  {$ENDREGION CINDY - GradientFillXXX procs}

{$ENDREGION CINDY CODE}

end.
