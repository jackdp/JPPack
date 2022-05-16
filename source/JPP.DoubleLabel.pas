unit JPP.DoubleLabel;

{
  Jacek Pazera
  https://github.com/jackdp

  A simple label component composed of 2 captions: left (property Caption) and right (property RightCaption).
  The space between captions can be modified by Spacing property.

  Based on TJppDoubleLineLabel which is based on TPegtopLineLabel from Pegtop Common Components by Jens Gruschel (http://www.pegtop.net/delphi).
  More information in file JPP.DoubleLineLabel.pas

  2021.01.14
  Windows.DrawText
}

{$I jpp.inc}
{$IFDEF FPC}{$mode delphi}{$ENDIF}

interface

uses
  {$IFDEF MSWINDOWS}Windows,{$ENDIF} SysUtils,
  Messages, Classes, Graphics, Controls, StdCtrls, JPL.Rects, JPP.Common, JPP.AnchoredControls {$IFDEF FPC}, LCLType, LCLIntf, LMessages, Types{$ENDIF};

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
    FLeftCaptionWidth: integer;
    FLeftCaptionAlignment: TAlignment;
    FRightCaptionPrefix: string;
    FRightCaptionPostfix: string;
    FDisabledTextColor: TColor;
    FRightCaptionDisabledTextColor: TColor;
    FRightCaptionDisabledColor: TColor;
    FRightCaptionDiabledBorderColor: TColor;
    FAnchoredControls: TJppAnchoredControls;
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
    procedure SetLeftCaptionWidth(const Value: integer);
    procedure SetLeftCaptionAlignment(const Value: TAlignment);
    procedure SetRightCaptionPrefix(const Value: string);
    procedure SetRightCaptionPostfix(const Value: string);
    function FullRightCaptionText: string;
    procedure SetDisabledTextColor(const Value: TColor);
    procedure SetRightCaptionDisabledTextColor(const Value: TColor);
    procedure SetRightCaptionDisabledColor(const Value: TColor);
    procedure SetRightCaptionDiabledBorderColor(const Value: TColor);
    procedure SetAnchoredControls(const Value: TJppAnchoredControls);
  protected
    procedure Paint; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
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
    property Enabled;
    property Font;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    {$IFDEF DCC}{$IFDEF HAS_STYLE_ELEMENTS}property StyleElements;{$ENDIF}{$ENDIF}
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
    property LeftCaptionWidth: integer read FLeftCaptionWidth write SetLeftCaptionWidth default -1;
    // LeftCaptionAlignment is used when LeftCaptionWidth > 0
    property LeftCaptionAlignment: TAlignment read FLeftCaptionAlignment write SetLeftCaptionAlignment default taLeftJustify;
    property RightCaptionPrefix: string read FRightCaptionPrefix write SetRightCaptionPrefix;
    property RightCaptionPostfix: string read FRightCaptionPostfix write SetRightCaptionPostfix;

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



constructor TJppDoubleLabel.Create(AOwner: TComponent);
begin
  inherited;
  Width := 121;
  Height := 13;
  FRightCaption := 'RightCaption';

  FRightCaptionFont := TFont.Create;
  FRightCaptionFont.OnChange := PropsChanged;
  FRightCaptionColor := clNone;
  FRightCaptionBorderColor := clNone;

//  FRightCaptionFont.Name := Self.Font.Name;
//  FRightCaptionFont.Size := Self.Font.SIZE;
  {$IFDEF MSWINDOWS}
  FRightCaptionFont.Name := 'Segoe UI';
  {$ENDIF}

  FSpacing := 6;
  FAutoWidth := True;
  FAutoHeight := True;
  {$IFDEF MSWINDOWS}FEllipsisPosition := epEndEllipsis;{$ENDIF}
  FShowAccelChar := False;
  FRightCaptionPosDeltaY := 0;
  FTagExt := TJppTagExt.Create(Self);

  FLeftCaptionWidth := -1;
  FLeftCaptionAlignment := taLeftJustify;

  FDisabledTextColor := clGrayText;
  FRightCaptionDisabledTextColor := clGrayText;
  FRightCaptionDisabledColor := clNone;
  FRightCaptionDiabledBorderColor := clNone;

  FAnchoredControls := TJppAnchoredControls.Create(Self);
end;

destructor TJppDoubleLabel.Destroy;
begin
  FRightCaptionFont.Free;
  FTagExt.Free;
  FAnchoredControls.Free;
  inherited;
end;

procedure TJppDoubleLabel.SetAnchoredControls(const Value: TJppAnchoredControls);
begin
  FAnchoredControls := Value;
end;

procedure TJppDoubleLabel.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  if not (csDestroying in ComponentState) then
    if Assigned(FAnchoredControls) then FAnchoredControls.UpdateAllControlsPos;
end;

procedure TJppDoubleLabel.Notification(AComponent: TComponent; Operation: TOperation);
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

function TJppDoubleLabel.FullRightCaptionText: string;
begin
  Result := FRightCaptionPrefix + FRightCaption + FRightCaptionPostfix;
end;

procedure TJppDoubleLabel.Paint;
var
  CaptionSize, RightCaptionSize: TSize;
  LeftCaptionRect, RightCaptionRect: TRect;
  CaptionTop, RightCaptionTop: Integer;
  Metric: TTextMetric;
  //X1, X2, Y: Integer;
  xCaptionHeight, xRightCaptionHeight: integer;
  clBg, clPen: TColor;
  dtFormat: Cardinal;
  PosDeltaY: integer;
  TextFormat: Cardinal;
  s: string;
begin
  inherited;

  PosDeltaY := FRightCaptionPosDeltaY;
  // get size:
  Canvas.Font.Assign(Font);
  xCaptionHeight := Canvas.TextHeight(Caption);
  GetTextMetrics(Canvas.Handle, Metric{%H-});
  CaptionSize := Canvas.TextExtent(Caption);

  Canvas.Font.Assign(FRightCaptionFont);
  RightCaptionSize := Canvas.TextExtent(FullRightCaptionText);
  xRightCaptionHeight := RightCaptionSize.cy;

  Canvas.Font.Assign(Font);

  if not Enabled then Canvas.Font.Color := FDisabledTextColor;

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


  LeftCaptionRect := Rect(0, CaptionTop, CaptionSize.CX, CaptionTop + CaptionSize.CY);
  if FLeftCaptionWidth > 0 then LeftCaptionRect.Width := FLeftCaptionWidth;

  RightCaptionRect.Left := LeftCaptionRect.Left + LeftCaptionRect.Width + FSpacing;
  RightCaptionRect.Top := RightCaptionTop;
  RightCaptionRect.Width := RightCaptionSize.cx;
  RightCaptionRect.Bottom := RightCaptionTop + xRightCaptionHeight + 0;//1;

  if not FAutoWidth then
  begin
    if RightCaptionRect.Right > ClientWidth then RightCaptionRect.Right := ClientWidth;
  end;

  if (FAutoHeight) and (Align <> alLeft) and (Align <> alRight) and (Align <> alClient) then
    if LeftCaptionRect.Height > RightCaptionRect.Height then Self.Height := LeftCaptionRect.Height
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
    if FLeftCaptionWidth > 0 then
    begin
      case FLeftCaptionAlignment of
        taLeftJustify: TextFormat := DT_LEFT;
        taCenter: TextFormat := DT_CENTER;
        taRightJustify: TextFormat := DT_RIGHT;
      {$IFDEF DCC}
      else TextFormat := DT_LEFT;
      {$ENDIF}
      end;
      TextFormat := TextFormat or DT_END_ELLIPSIS or DT_EXPANDTABS or DT_NOCLIP;
    end
    else TextFormat := DT_LEFT or DT_END_ELLIPSIS or DT_EXPANDTABS or DT_NOCLIP;
    //{$IFDEF MSWINDOWS}Windows.{$ENDIF}DrawText(Canvas.Handle, PChar(Caption), Length(Caption), LeftCaptionRect, TextFormat);
    DrawText(Canvas.Handle, PChar(Caption), Length(Caption), LeftCaptionRect, TextFormat);
  end;

  // ------------------ RightCaption text ------------------------
  if FRightCaption <> '' then
  with Canvas do
  begin
    RightCaptionRect.Top := RightCaptionRect.Top + PosDeltaY;
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
        RightCaptionRect.Top := RightCaptionRect.Top -1; // 2021.07.17 - bez tej poprawki dolna krawędź obramowania często jest niewidoczna
      end;

      Rectangle(RightCaptionRect);

      Brush.Style := bsClear;
      Pen.Style := psClear;

    end;


    dtFormat := DT_LEFT or DT_NOCLIP;

    {$IFDEF MSWINDOWS}
    {%H-}case FEllipsisPosition of
      epPathEllipsis: dtFormat := dtFormat or DT_PATH_ELLIPSIS;
      epEndEllipsis: dtFormat := dtFormat or DT_END_ELLIPSIS;
      epWordEllipsis: dtFormat := dtFormat or DT_WORD_ELLIPSIS;
    end;
    {$ENDIF}

    if not FShowAccelChar then dtFormat := dtFormat + DT_NOPREFIX;

    s := FullRightCaptionText;
    //{$IFDEF MSWINDOWS}Windows.{$ENDIF}DrawText(Canvas.Handle, PChar(s), Length(s), RightCaptionRect, dtFormat);
    DrawText(Canvas.Handle, PChar(s), Length(s), RightCaptionRect, dtFormat);

  end; // with Canvas


  if FAutoWidth and (Align <> alTop) and (Align <> alBottom) then Self.Width := LeftCaptionRect.Width + FSpacing + RightCaptionRect.Width;

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

procedure TJppDoubleLabel.SetRightCaptionDiabledBorderColor(const Value: TColor);
begin
  FRightCaptionDiabledBorderColor := Value;
  if not Enabled then Invalidate;
end;

procedure TJppDoubleLabel.SetRightCaptionDisabledColor(const Value: TColor);
begin
  FRightCaptionDisabledColor := Value;
  if not Enabled then Invalidate;
end;

procedure TJppDoubleLabel.SetRightCaptionDisabledTextColor(const Value: TColor);
begin
  FRightCaptionDisabledTextColor := Value;
  if not Enabled then Invalidate;
end;

procedure TJppDoubleLabel.SetRightCaptionFont(const Value: TFont);
begin
  //FRightCaptionFont := Value;
  if Assigned(FRightCaptionFont) and Assigned(Value) then
  begin
    FRightCaptionFont.Assign(Value);
    Invalidate;
  end;
end;

procedure TJppDoubleLabel.SetRightCaptionPosDeltaY(const Value: ShortInt);
begin
  if FRightCaptionPosDeltaY = Value then Exit;
  FRightCaptionPosDeltaY := Value;
  Invalidate;
end;

procedure TJppDoubleLabel.SetRightCaptionPostfix(const Value: string);
begin
  if FRightCaptionPostfix = Value then Exit;
  FRightCaptionPostfix := Value;
  Invalidate;
end;

procedure TJppDoubleLabel.SetRightCaptionPrefix(const Value: string);
begin
  if FRightCaptionPrefix = Value then Exit;
  FRightCaptionPrefix := Value;
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

procedure TJppDoubleLabel.SetDisabledTextColor(const Value: TColor);
begin
  FDisabledTextColor := Value;
  if not Enabled then Invalidate;
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


procedure TJppDoubleLabel.SetLeftCaptionAlignment(const Value: TAlignment);
begin
  if FLeftCaptionAlignment = Value then Exit;
  FLeftCaptionAlignment := Value;
  Invalidate;
end;

procedure TJppDoubleLabel.SetLeftCaptionWidth(const Value: integer);
begin
  if FLeftCaptionWidth = Value then Exit;
  FLeftCaptionWidth := Value;
  Invalidate;
end;

end.
