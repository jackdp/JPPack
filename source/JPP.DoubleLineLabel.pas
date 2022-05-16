unit JPP.DoubleLineLabel;

{
  Jacek Pazera
  https://github.com/jackdp

  A label component composed of 3 parts:
  1. Left caption (property Caption)
  2. Right caption (property RightCaption)
  3. Line drawn between the captions.

  Based on TPegtopLineLabel from Pegtop Common Components

  My modifications:

  TPegtopLineLabel renamed to TJppDoubleLineLabel
  Annex renamed to RightCaption
  [+] RightCaptionFont
  [+] LinePosDeltaY
  [+] LineSizeDeltaX1
  [+] LineSizeDeltaX2
  [+] RightCaptionColor
  [+] RightCaptionBorderColor
  [+] AutoHeight
  [+] RightCaptionPosDeltaY
  [+] TagExt
  [+] Prefixes: Jpp

  The RightCaption has its own font, background and border color.
  The RightCaption can be positioned vertically by RightCaptionPosDeltaY.
  The line can be positioned vertically by LinePosDeltaY.
  The length of the line can be modified by LineSizeDeltaX1 and LineSizeDeltaX2.

}

// --------------------------------------------------------------------------------------
//
//  Original Jen's comment:
//
////////////////////////////////////////////////////////////////////////////////
// File:       PegtopLineLabels.pas
// Components: TPegtopLineLabel
// Version:    1.00
// Date:       03 Jul 2005
// Author:     Jens Gruschel (GRU)
// Copyright:  (c) 2005 Jens Gruschel
// Website:    http://www.pegtop.net/delphi
////////////////////////////////////////////////////////////////////////////////
// Description:
// TPegtopLineLabel is a simple control that displays caption (left aligned),
// an optional annex (right aligned), and a line in-between.
////////////////////////////////////////////////////////////////////////////////
// License:
// Please read the license.txt file that comes with this software
////////////////////////////////////////////////////////////////////////////////
// Documentation / Installation:
// Please read the readme.txt file that comes with this software
////////////////////////////////////////////////////////////////////////////////
//
// -------------------------------------------------------------------------------------
// Jen's license (the most important part):
//
//   This software is distributed as a freeware. You are free to use it as part of
//   your application for any purpose including freeware, commercial and shareware
//   applications, provided some credit is given to the author.
//
//   The origin of this software must not be misrepresented; you must not claim
//   your authorship. All redistributions must retain the original copyright
//   notice and web site addresses.
//
//   Commercial redistribution of the library source is allowed only with an
//   explicit written permission from the author.
//
//   This software is provided 'as-is', without warranty of any kind, either
//   expressed or implied. In no event shall the author be held liable for any
//   damages arising from the use of this software.


{$I jpp.inc}
{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

uses
  {$IFDEF MSWINDOWS}Windows,{$ENDIF}
  Messages, Classes, Graphics, Controls, StdCtrls, {$IFDEF DCC}{$IFDEF HAS_SYSTEM_UITYPES}System.UItypes,{$ENDIF}{$ENDIF}
  {$IFDEF FPC}LCLType, Types, LCLIntf, {$ENDIF}
  JPL.Rects,
  JPP.Common, JPP.AnchoredControls
  ;

type
  TJppDoubleLabelLineStyle = (dllsNone, dllsSolid, dllsDash, dllsDot);

  TJppDoubleLabelLineEvent = procedure(Sender: TObject; Canvas: TCanvas; X1, X2, Y: Integer) of object;


  TJppDoubleLineLabel = class(TGraphicControl)
  private
    FRightCaption: TCaption;
    FLayout: TTextLayout;
    FLineStyle: TJppDoubleLabelLineStyle;
    FLineColor: TColor;
    FLinePeriod: Integer;
    FLineSeparation: Integer;
    FOnDrawLine: TJppDoubleLabelLineEvent;
    FRightCaptionFont: TFont;
    FLinePosDeltaY: SmallInt;
    FRightCaptionColor: TColor;
    FRightCaptionBorderColor: TColor;
    FLineSizeDeltaX1: integer;
    FLineSizeDeltaX2: integer;
    FAutoHeight: Boolean;
    FRightCaptionPosDeltaY: ShortInt;
    FTagExt: TJppTagExt;
    FDisabledTextColor: TColor;
    FRightCaptionDisabledTextColor: TColor;
    FRightCaptionDiabledBorderColor: TColor;
    FRightCaptionDisabledColor: TColor;
    FDisabledLineColor: TColor;
    FAnchoredControls: TJppAnchoredControls;
    procedure CMTextChanged(var Msg: TMessage); message CM_TEXTCHANGED;
    procedure SetRightCaption(Value: TCaption);
    procedure SetLayout(Value: TTextLayout);
    procedure SetLineStyle(Value: TJppDoubleLabelLineStyle);
    procedure SetLineColor(Value: TColor);
    procedure SetLinePeriod(Value: Integer);
    procedure SetLineSeparation(Value: Integer);
    procedure SetRightCaptionFont(const Value: TFont);
    procedure PropsChanged(Sender: TObject);
    procedure SetLinePosDeltaY(const Value: SmallInt);
    procedure SetRightCaptionColor(const Value: TColor);
    procedure SetRightCaptionBorderColor(const Value: TColor);
    procedure SetLineSizeDeltaX1(const Value: integer);
    procedure SetLineSizeDeltaX2(const Value: integer);
    procedure SetAutoHeight(const Value: Boolean);
    procedure SetRightCaptionPosDeltaY(const Value: ShortInt);
    procedure SetTagExt(const Value: TJppTagExt);
    procedure SetDisabledTextColor(const Value: TColor);
    procedure SetRightCaptionDiabledBorderColor(const Value: TColor);
    procedure SetRightCaptionDisabledColor(const Value: TColor);
    procedure SetRightCaptionDisabledTextColor(const Value: TColor);
    procedure SetDisabledLineColor(const Value: TColor);
    procedure SetAnchoredControls(const Value: TJppAnchoredControls);
  protected
    procedure Paint; override;
    procedure PaintLine(X1, X2, Y: Integer); virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  published
    property RightCaption: TCaption read FRightCaption write SetRightCaption;
    property Layout: TTextLayout read FLayout write SetLayout default tlTop;
    property LineStyle: TJppDoubleLabelLineStyle read FLineStyle write SetLineStyle default dllsDot;
    property LineColor: TColor read FLineColor write SetLineColor default clGrayText;
    property DisabledLineColor: TColor read FDisabledLineColor write SetDisabledLineColor default clGrayText;
    property LinePeriod: Integer read FLinePeriod write SetLinePeriod default 3;
    property LineSeparation: Integer read FLineSeparation write SetLineSeparation default 2;
    property OnDrawLine: TJppDoubleLabelLineEvent read FOnDrawLine write FOnDrawLine;
    property Align;
    property Anchors;
    property Constraints;
    property Caption;
    property Color;
    property DragKind;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    {$IFDEF HAS_STYLE_ELEMENTS}property StyleElements;{$ENDIF}
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    {$IFDEF DCC}property OnMouseActivate;{$ENDIF}
    property OnMouseEnter;
    property OnMouseLeave;
    property RightCaptionFont: TFont read FRightCaptionFont write SetRightCaptionFont;
    property LinePosDeltaY: SmallInt read FLinePosDeltaY write SetLinePosDeltaY default 0;
    property LineSizeDeltaX1: integer read FLineSizeDeltaX1 write SetLineSizeDeltaX1 default 0;
    property LineSizeDeltaX2: integer read FLineSizeDeltaX2 write SetLineSizeDeltaX2 default 0;
    property RightCaptionColor: TColor read FRightCaptionColor write SetRightCaptionColor default clNone;
    property RightCaptionBorderColor: TColor read FRightCaptionBorderColor write SetRightCaptionBorderColor default clNone;
    property AutoHeight: Boolean read FAutoHeight write SetAutoHeight default True;
    property RightCaptionPosDeltaY: ShortInt read FRightCaptionPosDeltaY write SetRightCaptionPosDeltaY default 0;
    property TagExt: TJppTagExt read FTagExt write SetTagExt;

    property DisabledTextColor: TColor read FDisabledTextColor write SetDisabledTextColor default clGrayText;
    property RightCaptionDisabledTextColor: TColor read FRightCaptionDisabledTextColor write SetRightCaptionDisabledTextColor default clGrayText;
    property RightCaptionDisabledColor: TColor read FRightCaptionDisabledColor write SetRightCaptionDisabledColor default clNone;
    property RightCaptionDiabledBorderColor: TColor read FRightCaptionDiabledBorderColor write SetRightCaptionDiabledBorderColor default clNone;

    property AnchoredControls: TJppAnchoredControls read FAnchoredControls write SetAnchoredControls;

    {$IFDEF FPC}
    property BorderSpacing;
    {$ENDIF}
  end;


implementation


constructor TJppDoubleLineLabel.Create(AOwner: TComponent);
begin
  inherited;
  Width := 190;
  Height := 13;
  FLineStyle := dllsDot;
  FLineColor := clGrayText;
  FDisabledLineColor := clGrayText;
  FLinePeriod := 3;
  FLineSeparation := 2;
  FRightCaption := 'RightCaption';
  FRightCaptionFont := TFont.Create;
  FRightCaptionFont.OnChange := PropsChanged;
  //FRightCaptionFont.Assign(Font);
  LinePosDeltaY := 0;
  FRightCaptionColor := clNone;
  FRightCaptionBorderColor := clNone;
  FRightCaptionPosDeltaY := 0;
  FLineSizeDeltaX1 := 0;
  FLineSizeDeltaX2 := 0;
  FAutoHeight := True;
  FTagExt := TJppTagExt.Create(Self);

  FDisabledTextColor := clGrayText;
  FRightCaptionDisabledTextColor := clGrayText;
  FRightCaptionDisabledColor := clNone;
  FRightCaptionDiabledBorderColor := clNone;

  FAnchoredControls := TJppAnchoredControls.Create(Self);
end;

destructor TJppDoubleLineLabel.Destroy;
begin
  FRightCaptionFont.Free;
  FTagExt.Free;
  FAnchoredControls.Free;
  inherited;
end;

procedure TJppDoubleLineLabel.SetAnchoredControls(const Value: TJppAnchoredControls);
begin
  FAnchoredControls := Value;
end;

procedure TJppDoubleLineLabel.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited;
  if Operation = opRemove then
    if not (csDestroying in ComponentState) then
      if Assigned(FAnchoredControls) then
      begin
        if AComponent = FAnchoredControls.Top.Control then FAnchoredControls.Top.Control := nil
        else if AComponent = FAnchoredControls.Bottom.Control then FAnchoredControls.Bottom.Control := nil
        else if AComponent = FAnchoredControls.Left.Control then FAnchoredControls.Left.Control := nil
        else if AComponent = FAnchoredControls.Right.Control then FAnchoredControls.Right.Control := nil;
      end;
end;

procedure TJppDoubleLineLabel.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  if not (csDestroying in ComponentState) then
    if Assigned(FAnchoredControls) then FAnchoredControls.UpdateAllControlsPos;
end;

procedure TJppDoubleLineLabel.Paint;
var
  CaptionSize, RightCaptionSize: TSize;
  CaptionRect, RightCaptionRect: TRect;
  CaptionTop, RightCaptionTop: Integer;
  Metric: TTextMetric;
  X1, X2, Y: Integer;
  xCaptionHeight, xRightCaptionHeight: integer;
  clBg, clPen: TColor;
begin
  inherited;

  // get size:
  Canvas.Font.Assign(Font);
  xCaptionHeight := Canvas.TextHeight(Caption);
  {$IFDEF FPC}Metric.tmAscent := 1; {$ENDIF} // suppress stupid message
  GetTextMetrics(Canvas.Handle, Metric);
  CaptionSize := Canvas.TextExtent(Caption);

  Canvas.Font.Assign(FRightCaptionFont);
  RightCaptionSize := Canvas.TextExtent(FRightCaption);
  xRightCaptionHeight := RightCaptionSize.cy;

  Canvas.Font.Assign(Font);

  if not Enabled then Canvas.Font.Color := FDisabledTextColor;


  if CaptionSize.CX > ClientWidth - RightCaptionSize.CX - 2 then CaptionSize.CX := ClientWidth - RightCaptionSize.CX - 2;


  // assign rects:
  case FLayout of

    tlTop:
      begin
        CaptionTop := 0;
        RightCaptionTop := 0;
        if xRightCaptionHeight > xCaptionHeight then CaptionTop := xRightCaptionHeight - xCaptionHeight
        else RightCaptionTop := xCaptionHeight - xRightCaptionHeight;
      end;

    tlBottom:
      begin
        CaptionTop := ClientHeight - CaptionSize.CY;
        RightCaptionTop := ClientHeight - xRightCaptionHeight;
      end;

    else // tlCenter

      begin
        if xRightCaptionHeight > xCaptionHeight then
        begin
          RightCaptionTop := (ClientHeight - xRightCaptionHeight) div 2;
          CaptionTop := RightCaptionTop + xRightCaptionHeight - xCaptionHeight;
        end
        else
        begin
          CaptionTop := (ClientHeight - xCaptionHeight) div 2;
          RightCaptionTop := CaptionTop + xCaptionHeight - xRightCaptionHeight;
        end;
      end;

  end; // case


  CaptionRect := Rect(0, CaptionTop, CaptionSize.CX, CaptionTop + CaptionSize.CY);

  RightCaptionRect := Rect(
    ClientWidth - RightCaptionSize.CX, RightCaptionTop,
    ClientWidth, RightCaptionTop + xRightCaptionHeight
  );

  if (FAutoHeight) and (Align <> alLeft) and (Align <> alRight) and (Align <> alClient) then
    if CaptionRect.Height > RightCaptionRect.Height then Self.Height := CaptionRect.Height
    else Self.Height := RightCaptionRect.Height;

  // draw:
  if not ParentColor then
  begin
    Canvas.Brush.Color := Color;
    Canvas.FillRect(ClientRect);
  end;

  Canvas.Brush.Style := bsClear;

  // --------------------- Caption text ----------------------
  if Caption <> '' then
  begin
    //{$IFDEF MSWINDOWS}Windows.{$ENDIF}
    DrawText(
      Canvas.Handle, PChar(Caption), Length(Caption), CaptionRect, DT_LEFT or DT_END_ELLIPSIS or DT_EXPANDTABS or DT_NOCLIP
    );
  end;

  // ------------------ RightCaption text ------------------------
  if FRightCaption <> '' then
  with Canvas do
  begin

    Font.Assign(RightCaptionFont);
    if not Enabled then
    begin
      Font.Color := FRightCaptionDisabledTextColor;
      clBg := FRightCaptionDisabledColor;
      clPen := FRightCaptionDiabledBorderColor;
    end
    else
    begin
      clBg := FRightCaptionColor;
      clPen := FRightCaptionBorderColor;
    end;

    if (clBg <> clNone) or (clPen <> clNone) then
    begin

      if clBg <> clNone then
      begin
        Brush.Style := bsSolid;
        Brush.Color := clBg;
        Pen.Style := psSolid;
        Pen.Color := clBg;
      end
      else Brush.Style := bsClear;

      if clPen <> clNone then
      begin
        Pen.Style := psSolid;
        if Enabled then Pen.Color := FRightCaptionBorderColor
        else Pen.Color := FRightCaptionDiabledBorderColor;
      end;

      Rectangle(RightCaptionRect);

      Brush.Style := bsClear;
      Pen.Style := psClear;

    end;


    RightCaptionRect.Top := RightCaptionRect.Top + FRightCaptionPosDeltaY;

    //{$IFDEF MSWINDOWS}Windows.{$ENDIF}
    DrawText(
      Canvas.Handle, PChar(FRightCaption), Length(FRightCaption), RightCaptionRect, DT_LEFT or DT_NOPREFIX or DT_EXPANDTABS or DT_NOCLIP
    );

  end; // with Canvas


  // ----------------------- Line -------------------------------
  if Caption = '' then X1 := 0 else X1 := CaptionSize.CX + FLineSeparation;
  X1 := X1 + FLineSizeDeltaX1;

  if FRightCaption = '' then X2 := ClientWidth
  else X2 := ClientWidth - RightCaptionSize.CX - FLineSeparation + FLineSizeDeltaX2;

  if X1 < X2 then
  begin
    Canvas.Pen.Style := psSolid;
    if not Enabled then Canvas.Pen.Color := FDisabledLineColor
    else Canvas.Pen.Color := FLineColor;
    Y := CaptionTop + Metric.tmHeight - Metric.tmDescent - 1 + LinePosDeltaY;
    PaintLine(X1, X2, Y);
  end;

end;

procedure TJppDoubleLineLabel.PaintLine(X1, X2, Y: Integer);
var
  X: Integer;
begin
  case FLineStyle of
    dllsSolid:
      begin
        Canvas.MoveTo(X1, Y);
        Canvas.LineTo(X2, Y);
      end;
    dllsDash:
      begin
        X := X2 - FLinePeriod;
        while X >= X1 do
        begin
          Canvas.MoveTo(X, Y);
          Canvas.LineTo(X + FLinePeriod, Y);
          Dec(X, FLinePeriod * 2);
        end;
      end;
    dllsDot:
      begin
        X := X2 - 1;
        while X >= X1 do
        begin
          Canvas.Pixels[X, Y] := Canvas.Pen.Color; // FLineColor;
          Dec(X, FLinePeriod);
        end;
      end;
  end;
  if Assigned(FOnDrawLine) then FOnDrawLine(Self, Canvas, X1, X2, Y);
end;

procedure TJppDoubleLineLabel.PropsChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TJppDoubleLineLabel.CMTextChanged(var Msg: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TJppDoubleLineLabel.SetRightCaption(Value: TCaption);
begin
  if FRightCaption <> Value then
  begin
    FRightCaption := Value;
    Invalidate;
  end;
end;

procedure TJppDoubleLineLabel.SetRightCaptionBorderColor(const Value: TColor);
begin
  if FRightCaptionBorderColor = Value then Exit;
  FRightCaptionBorderColor := Value;
  Invalidate;
end;

procedure TJppDoubleLineLabel.SetRightCaptionColor(const Value: TColor);
begin
  if FRightCaptionColor = Value then Exit;
  FRightCaptionColor := Value;
  Invalidate;
end;

procedure TJppDoubleLineLabel.SetRightCaptionDiabledBorderColor(const Value: TColor);
begin
  FRightCaptionDiabledBorderColor := Value;
  if not Enabled then Invalidate;
end;

procedure TJppDoubleLineLabel.SetRightCaptionDisabledColor(const Value: TColor);
begin
  FRightCaptionDisabledColor := Value;
  if not Enabled then Invalidate;
end;

procedure TJppDoubleLineLabel.SetRightCaptionDisabledTextColor(const Value: TColor);
begin
  FRightCaptionDisabledTextColor := Value;
  if not Enabled then Invalidate;
end;

procedure TJppDoubleLineLabel.SetRightCaptionFont(const Value: TFont);
begin
  //FRightCaptionFont := Value;
  if Assigned(FRightCaptionFont) and Assigned(Value) then
  begin
    FRightCaptionFont.Assign(Value);
    Invalidate;
  end;
end;

procedure TJppDoubleLineLabel.SetRightCaptionPosDeltaY(const Value: ShortInt);
begin
  if FRightCaptionPosDeltaY = Value then Exit;
  FRightCaptionPosDeltaY := Value;
  Invalidate;
end;

procedure TJppDoubleLineLabel.SetTagExt(const Value: TJppTagExt);
begin
  FTagExt := Value;
end;

procedure TJppDoubleLineLabel.SetLinePosDeltaY(const Value: SmallInt);
begin
  if LinePosDeltaY = Value then Exit;
  FLinePosDeltaY := Value;
  Invalidate;
end;

procedure TJppDoubleLineLabel.SetAutoHeight(const Value: Boolean);
begin
  if FAutoHeight = Value then Exit;
  FAutoHeight := Value;
  Invalidate;
end;

procedure TJppDoubleLineLabel.SetDisabledLineColor(const Value: TColor);
begin
  FDisabledLineColor := Value;
  if not Enabled then Invalidate;
end;

procedure TJppDoubleLineLabel.SetDisabledTextColor(const Value: TColor);
begin
  FDisabledTextColor := Value;
  if not Enabled then Invalidate;
end;

procedure TJppDoubleLineLabel.SetLayout(Value: TTextLayout);
begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    Invalidate;
  end;
end;

procedure TJppDoubleLineLabel.SetLineStyle(Value: TJppDoubleLabelLineStyle);
begin
  if FLineStyle <> Value then
  begin
    FLineStyle := Value;
    Invalidate;
  end;
end;

procedure TJppDoubleLineLabel.SetLineColor(Value: TColor);
begin
  if FLineColor <> Value then
  begin
    FLineColor := Value;
    Invalidate;
  end;
end;

procedure TJppDoubleLineLabel.SetLinePeriod(Value: Integer);
begin
  if Value < 2 then Value := 2
  else if Value > 40 then Value := 40;
  if FLinePeriod <> Value then
  begin
    FLinePeriod := Value;
    Invalidate;
  end;
end;

procedure TJppDoubleLineLabel.SetLineSeparation(Value: Integer);
begin
  if Value < 0 then Value := 0
  else if Value > 10 then Value := 10;
  if FLineSeparation <> Value then
  begin
    FLineSeparation := Value;
    Invalidate;
  end;
end;

procedure TJppDoubleLineLabel.SetLineSizeDeltaX1(const Value: integer);
begin
  if FLineSizeDeltaX1 = Value then Exit;
  FLineSizeDeltaX1 := Value;
  Invalidate;
end;

procedure TJppDoubleLineLabel.SetLineSizeDeltaX2(const Value: integer);
begin
  if FLineSizeDeltaX2 = Value then Exit;
  FLineSizeDeltaX2 := Value;
  Invalidate;
end;

end.
