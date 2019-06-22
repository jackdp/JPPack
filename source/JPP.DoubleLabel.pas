unit JPP.DoubleLabel;

{
  Jacek Pazera
  https://github.com/jackdp
  Last mod: 2019.05.25

  A simple label component composed of 2 captions: left (property Caption) and right (property RightCaption).
  The space between captions can be modified by Spacing property.

  Based on TJppDoubleLineLabel which is based on TPegtopLineLabel from Pegtop Common Components by Jens Gruschel (http://www.pegtop.net/delphi).
  More information in file JPP.DoubleLineLabel.pas
}

{$IFDEF FPC} {$mode objfpc}{$H+} {$ENDIF}

interface

uses
  {$IFDEF MSWINDOWS}Windows,{$ENDIF}
  Messages, Classes, Graphics, Controls, StdCtrls, JPP.Common {$IFDEF FPC}, LCLType, LCLIntf, LMessages, Types{$ENDIF};

type

  TJppDoubleLabel = class(TGraphicControl)
  private
    FRightCaption: TCaption;
    FLayout: TTextLayout;
    FRightCaptionFont: TFont;
    FRightCaptionColor: TColor;
    FRightCaptionBorderColor: TColor;
    FSpacing: integer;
    FAutoWidth: Boolean;
    FAutoHeight: Boolean;
    {$IFDEF MSWINDOWS}FEllipsisPosition: TEllipsisPosition;{$ENDIF}
    FShowAccelChar: Boolean;
    FRightCaptionPosDeltaY: ShortInt;
    FTagExt: TJppTagExt;
    procedure CMTextChanged(var Msg: TMessage); message CM_TEXTCHANGED;
    procedure SetRightCaption(Value: TCaption);
    procedure SetLayout(Value: TTextLayout);
    procedure SetRightCaptionFont(const Value: TFont);
    procedure PropsChanged(Sender: TObject);
    procedure SetRightCaptionColor(const Value: TColor);
    procedure SetRightCaptionBorderColor(const Value: TColor);
    procedure SetSpacing(const Value: integer);
    procedure SetAutoWidth(const Value: Boolean);
    procedure SetAutoHeight(const Value: Boolean);
    {$IFDEF MSWINDOWS}procedure SetEllipsisPosition(const Value: TEllipsisPosition);{$ENDIF}
    procedure SetShowAccelChar(const Value: Boolean);
    procedure SetRightCaptionPosDeltaY(const Value: ShortInt);
    procedure SetTagExt(const Value: TJppTagExt);
  protected
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property RightCaption: TCaption read FRightCaption write SetRightCaption;
    property Layout: TTextLayout read FLayout write SetLayout default tlTop;
    property Align;
    property Anchors;
    property Constraints;
    property Caption;
    property Color;
    property DragKind;
    property DragCursor;
    property DragMode;
    property Font;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    {$IFDEF DCC}{$IF CompilerVersion > 23}property StyleElements;{$IFEND}{$ENDIF}
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
    property RightCaptionColor: TColor read FRightCaptionColor write SetRightCaptionColor default clNone;
    property RightCaptionBorderColor: TColor read FRightCaptionBorderColor write SetRightCaptionBorderColor default clNone;
    property Spacing: integer read FSpacing write SetSpacing default 6;
    property AutoWidth: Boolean read FAutoWidth write SetAutoWidth default True;
    property AutoHeight: Boolean read FAutoHeight write SetAutoHeight default True;
    {$IFDEF MSWINDOWS}property EllipsisPosition: TEllipsisPosition read FEllipsisPosition write SetEllipsisPosition default epEndEllipsis;{$ENDIF}
    property ShowAccelChar: Boolean read FShowAccelChar write SetShowAccelChar default False;
    property RightCaptionPosDeltaY: ShortInt read FRightCaptionPosDeltaY write SetRightCaptionPosDeltaY default 0;
    property TagExt: TJppTagExt read FTagExt write SetTagExt;
  end;


implementation


constructor TJppDoubleLabel.Create(AOwner: TComponent);
begin
  inherited;
  Width := 121;
  Height := 13;
  FRightCaption := 'RightCaption';
  FRightCaptionFont := TFont.Create;
  FRightCaptionFont.OnChange := {$IFDEF FPC}@{$ENDIF}PropsChanged;
  FRightCaptionColor := clNone;
  FRightCaptionBorderColor := clNone;
  FSpacing := 6;
  FAutoWidth := True;
  FAutoHeight := True;
  {$IFDEF MSWINDOWS}FEllipsisPosition := epEndEllipsis;{$ENDIF}
  FShowAccelChar := False;
  FRightCaptionPosDeltaY := 0;
  FTagExt := TJppTagExt.Create(Self);
end;

destructor TJppDoubleLabel.Destroy;
begin
  FRightCaptionFont.Free;
  FTagExt.Free;
  inherited;
end;

procedure TJppDoubleLabel.Paint;
var
  CaptionSize, RightCaptionSize: TSize;
  CaptionRect, RightCaptionRect: TRect;
  CaptionTop, RightCaptionTop: Integer;
  Metric: TTextMetric;
  //X1, X2, Y: Integer;
  xCaptionHeight, xRightCaptionHeight: integer;
  clBg, clPen: TColor;
  dtFormat: Cardinal;
  PosDeltaY: integer;
begin
  inherited;

  PosDeltaY := FRightCaptionPosDeltaY;
  // get size:
  Canvas.Font.Assign(Font);
  xCaptionHeight := Canvas.TextHeight(Caption);
  GetTextMetrics(Canvas.Handle, Metric{%H-});
  CaptionSize := Canvas.TextExtent(Caption);

  Canvas.Font.Assign(FRightCaptionFont);
  RightCaptionSize := Canvas.TextExtent(FRightCaption);
  xRightCaptionHeight := RightCaptionSize.cy;

  Canvas.Font.Assign(Font);


  //if CaptionSize.CX > ClientWidth - RightCaptionSize.CX - 2 then CaptionSize.CX := ClientWidth - RightCaptionSize.CX - 2;


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

  RightCaptionRect.Left := CaptionRect.Left + CaptionRect.Width + FSpacing;
  RightCaptionRect.Top := RightCaptionTop;
  RightCaptionRect.Width := RightCaptionSize.cx;
  RightCaptionRect.Bottom := RightCaptionTop + xRightCaptionHeight + 0;

  if not FAutoWidth then
  begin
    if RightCaptionRect.Right > ClientWidth then RightCaptionRect.Right := ClientWidth;
  end;

  if FAutoHeight then
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
    {$IFDEF MSWINDOWS}Windows.{$ENDIF}DrawText(
      Canvas.Handle, PChar(Caption), Length(Caption), CaptionRect, DT_LEFT or DT_END_ELLIPSIS or DT_EXPANDTABS or DT_NOCLIP
    );
  end;

  // ------------------ RightCaption text ------------------------
  if FRightCaption <> '' then
  with Canvas do
  begin
    RightCaptionRect.Top := RightCaptionRect.Top + PosDeltaY;
    Font.Assign(RightCaptionFont);

    clBg := FRightCaptionColor;
    clPen := FRightCaptionBorderColor;

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
        Pen.Color := FRightCaptionBorderColor;
      end;

      Rectangle(RightCaptionRect);

      Brush.Style := bsClear;
      Pen.Style := psClear;

    end;


    dtFormat := DT_LEFT or DT_NOCLIP;

    {$IFDEF MSWINDOWS}
    case FEllipsisPosition of
      epPathEllipsis: dtFormat := dtFormat or DT_PATH_ELLIPSIS;
      epEndEllipsis: dtFormat := dtFormat or DT_END_ELLIPSIS;
      epWordEllipsis: dtFormat := dtFormat or DT_WORD_ELLIPSIS;
    end;
    {$ENDIF}

    if not FShowAccelChar then dtFormat := dtFormat + DT_NOPREFIX;

    {$IFDEF MSWINDOWS}Windows.{$ENDIF}DrawText(Canvas.Handle, PChar(FRightCaption), Length(FRightCaption), RightCaptionRect, dtFormat);

  end; // with Canvas


  if FAutoWidth then Self.Width := CaptionRect.Width + FSpacing + RightCaptionRect.Width;

end;


procedure TJppDoubleLabel.PropsChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TJppDoubleLabel.CMTextChanged(var Msg: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TJppDoubleLabel.SetRightCaption(Value: TCaption);
begin
  if FRightCaption <> Value then
  begin
    FRightCaption := Value;
    Invalidate;
  end;
end;

procedure TJppDoubleLabel.SetRightCaptionBorderColor(const Value: TColor);
begin
  if FRightCaptionBorderColor = Value then Exit;
  FRightCaptionBorderColor := Value;
  Invalidate;
end;

procedure TJppDoubleLabel.SetRightCaptionColor(const Value: TColor);
begin
  if FRightCaptionColor = Value then Exit;
  FRightCaptionColor := Value;
  Invalidate;
end;

procedure TJppDoubleLabel.SetRightCaptionFont(const Value: TFont);
begin
  FRightCaptionFont := Value;
  Invalidate;
end;

procedure TJppDoubleLabel.SetRightCaptionPosDeltaY(const Value: ShortInt);
begin
  if FRightCaptionPosDeltaY = Value then Exit;
  FRightCaptionPosDeltaY := Value;
  Invalidate;
end;

procedure TJppDoubleLabel.SetShowAccelChar(const Value: Boolean);
begin
  if FShowAccelChar = Value then Exit;
  FShowAccelChar := Value;
  Invalidate;
end;

procedure TJppDoubleLabel.SetSpacing(const Value: integer);
begin
  if FSpacing = Value then Exit;
  FSpacing := Value;
  Invalidate;
end;

procedure TJppDoubleLabel.SetTagExt(const Value: TJppTagExt);
begin
  FTagExt := Value;
end;

procedure TJppDoubleLabel.SetAutoHeight(const Value: Boolean);
begin
  if FAutoHeight = Value then Exit;
  FAutoHeight := Value;
  Invalidate;
end;

procedure TJppDoubleLabel.SetAutoWidth(const Value: Boolean);
begin
  if FAutoWidth = Value then Exit;
  FAutoWidth := Value;
  Invalidate;
end;

{$IFDEF MSWINDOWS}
procedure TJppDoubleLabel.SetEllipsisPosition(const Value: TEllipsisPosition);
begin
  if FEllipsisPosition = Value then Exit;
  FEllipsisPosition := Value;
  Invalidate;
end;
{$ENDIF}

procedure TJppDoubleLabel.SetLayout(Value: TTextLayout);
begin
  if FLayout <> Value then
  begin
    FLayout := Value;
    Invalidate;
  end;
end;


end.
