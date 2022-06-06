unit JPP.Labels;

{
  Jacek Pazera
  https://www.pazera-software.com
  https://github.com/jackdp
}


{$I jpp.inc}
{$IFDEF FPC}
  {$mode delphi}
  {$MODESWITCH ADVANCEDRECORDS}
{$ENDIF}

interface

uses 
  {$IFDEF MSWINDOWS}Windows,{$ENDIF}
  SysUtils, Classes, StdCtrls, Graphics, Controls,
  {$IFDEF DCC}{$IFDEF HAS_SYSTEM_UITYPES}System.UITypes,{$ENDIF}{$ENDIF} Types,
  {$IFDEF FPC}LCLIntf, LCLType,{$ENDIF}
  JPL.Rects,
  JPP.Common, JPP.Common.Procs, JPP.AnchoredControls, JPP.Flash;


type

  TJppCustomLabel = class;

  {$Region ' --- TJppFlashLabel --- '}
  TJppFlashLabel = class(TJppFlashBase)
  private
    FLabel: TJppCustomLabel;
    FTransparent: Boolean;
    procedure SetTransparent(const Value: Boolean);
  protected
    procedure FlashColorChanged(Sender: TObject; const AColor: TColor);
    procedure FlashFinished(Sender: TObject);
    property Transparent: Boolean read FTransparent write SetTransparent;
  public
    constructor Create(ALabel: TJppCustomLabel);
  published
    property Enabled;
    property FlashColor;
    property FlashCount;
    property FlashInterval;
    property OnFlashFinished;
  end;
  {$endregion TJppFlashLabel}


  {$region '   TJppCustomLabel   '}
  TJppCustomLabel = class(TCustomLabel)
  private
    {$IFDEF FPC}FEllipsisPosition: TEllipsisPosition;{$ENDIF}
    FTagExt: TJppTagExt;
    FAnchoredControls: TJppAnchoredControls;
    FFlash: TJppFlashLabel;
    {$IFDEF FPC}procedure SetEllipsisPosition(AValue: TEllipsisPosition);{$ENDIF}
    procedure SetTagExt(const Value: TJppTagExt);
    procedure SetAnchoredControls(const Value: TJppAnchoredControls);
    procedure SetFlash(const Value: TJppFlashLabel);
  protected
    procedure PropsChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure FlashBackground;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  protected
    property TagExt: TJppTagExt read FTagExt write SetTagExt;
    property AnchoredControls: TJppAnchoredControls read FAnchoredControls write SetAnchoredControls;
    {$IFDEF FPC}
    property EllipsisPosition: TEllipsisPosition read FEllipsisPosition write SetEllipsisPosition default epNone;
    {$ENDIF}
    property Flash: TJppFlashLabel read FFlash write SetFlash;
  end;
  {$endregion TJppCustomLabel}


  {$region '   TJppLabel   '}
  TJppLabel = class(TJppCustomLabel)
  published
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Caption;
    property Color nodefault;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    {$IFDEF DCC}property EllipsisPosition;{$ENDIF}
    property Enabled;
    property FocusControl;
    property Font;
    {$IFDEF DCC}property GlowSize;{$ENDIF}
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowAccelChar;
    property ShowHint;
    {$IFDEF DELPHI2010_OR_ABOVE}property Touch;{$ENDIF}
    property Transparent;
    property Layout;
    property Visible;
    property WordWrap;
    {$IFDEF HAS_STYLE_ELEMENTS}property StyleElements;{$ENDIF}
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    {$IFDEF DELPHI2010_OR_ABOVE}property OnGesture;{$ENDIF}
    {$IFDEF DCC}property OnMouseActivate;{$ENDIF}
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnStartDock;
    property OnStartDrag;

    property Flash;

    property TagExt;
    property AnchoredControls;
    {$IFDEF FPC}
    property BorderSpacing;
    {$ENDIF}
  end;
  {$endregion TJppLabel}


  {$Region '   TJppShadowLabelAppearance   '}
  TJppShadowLabelAppearance = class(TJppPersistent)
  private
    FOwner: TComponent;
    FCaptionShadow: TJppTextShadowParams;
    FDisabledCaptionColor: TColor;
    FDisabledCaptionShadow: TJppTextShadowParams;
    FBorderColor: TColor;
    FBorderWidth: Byte;
    FBorderStyle: TPenStyle;
    FDisabledBorderColor: TColor;
    FPadding: TJppPadding;
    FDrawLeftBorder: Boolean;
    FDrawTopBorder: Boolean;
    FDrawBottomBorder: Boolean;
    FDrawRightBorder: Boolean;
    FBorder3D: Boolean;
    procedure SetCaptionShadow(const Value: TJppTextShadowParams);
    procedure SetDisabledCaptionColor(const Value: TColor);
    procedure SetDisabledCaptionShadow(const Value: TJppTextShadowParams);
    procedure SetBorderColor(const Value: TColor);
    procedure SetBorderWidth(const Value: Byte);
    procedure SetBorderStyle(const Value: TPenStyle);
    procedure SetDisabledBorderColor(const Value: TColor);
    procedure SetPadding(const Value: TJppPadding);
    procedure SetDrawLeftBorder(const Value: Boolean);
    procedure SetDrawTopBorder(const Value: Boolean);
    procedure SetDrawBottomBorder(const Value: Boolean);
    procedure SetDrawRightBorder(const Value: Boolean);
    procedure SetBorder3D(const Value: Boolean);
  protected
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(const Source: TJppShadowLabelAppearance); reintroduce;
  published
    property CaptionShadow: TJppTextShadowParams read FCaptionShadow write SetCaptionShadow;
    property DisabledCaptionColor: TColor read FDisabledCaptionColor write SetDisabledCaptionColor default clBtnShadow;
    property DisabledCaptionShadow: TJppTextShadowParams read FDisabledCaptionShadow write SetDisabledCaptionShadow;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clBlack;
    property BorderWidth: Byte read FBorderWidth write SetBorderWidth default 0;
    property BorderStyle: TPenStyle read FBorderStyle write SetBorderStyle default psSolid;
    property DrawLeftBorder: Boolean read FDrawLeftBorder write SetDrawLeftBorder default True;
    property DrawRightBorder: Boolean read FDrawRightBorder write SetDrawRightBorder default True;
    property DrawTopBorder: Boolean read FDrawTopBorder write SetDrawTopBorder default True;
    property DrawBottomBorder: Boolean read FDrawBottomBorder write SetDrawBottomBorder default True;
    property Border3D: Boolean read FBorder3D write SetBorder3D default True;
    property DisabledBorderColor: TColor read FDisabledBorderColor write SetDisabledBorderColor default clSilver;
    property Padding: TJppPadding read FPadding write SetPadding;
  end;
  {$endregion TJppShadowLabelAppearance}


  {$region '   TJppCustomShadowLabel   '}
  TJppCustomShadowLabel = class(TJppCustomLabel)
  private
    FAppearance: TJppShadowLabelAppearance;
    procedure SetAppearance(const Value: TJppShadowLabelAppearance);
  protected
    {$IFDEF DCC}
    procedure DoDrawText(var Rect: TRect; Flags: Longint); override;
    {$ELSE}
    procedure DoDrawText(var Rect: TRect; Flags: Longint); {$IFDEF FPC322_OR_ABOVE}override;{$ENDIF}
    {$ENDIF}
    {$IFDEF DCC}procedure AdjustBounds; override;{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Paint; override;
  protected
    property Appearance: TJppShadowLabelAppearance read FAppearance write SetAppearance;
  end;
  {$endregion TJppCustomShadowLabel}


  {$region '   TJppShadowLabel   '}
  TJppShadowLabel = class(TJppCustomShadowLabel)
  published
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BiDiMode;
    property Caption;
    property Color nodefault;
    property Constraints;
    property DragCursor;
    property DragKind;
    property DragMode;
    property EllipsisPosition;
    property Enabled;
    property FocusControl;
    property Font;
    {$IFDEF DCC}property GlowSize;{$ENDIF}
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowAccelChar;
    property ShowHint;
    {$IFDEF DELPHI2010_OR_ABOVE}property Touch;{$ENDIF}
    property Transparent;
    property Layout;
    property Visible;
    property WordWrap;
    {$IFDEF HAS_STYLE_ELEMENTS}property StyleElements;{$ENDIF}
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    {$IFDEF DELPHI2010_OR_ABOVE}property OnGesture;{$ENDIF}
    {$IFDEF DCC}property OnMouseActivate;{$ENDIF}
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnStartDock;
    property OnStartDrag;

    property TagExt;
    property AnchoredControls;
    property Appearance;
    {$IFDEF FPC}
    property BorderSpacing;
    {$ENDIF}
  end;
  {$endregion TJppShadowLabel}



implementation




{$region '                       TJppCustomLabel                            '}

constructor TJppCustomLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTagExt := TJppTagExt.Create(Self);
  FAnchoredControls := TJppAnchoredControls.Create(Self);
  {$IFDEF FPC}
  FEllipsisPosition := epNone;
  {$ENDIF}
  FFlash := TJppFlashLabel.Create(Self);
end;

destructor TJppCustomLabel.Destroy;
begin;
  FTagExt.Free;
  FAnchoredControls.Free;
  FFlash.Free;
  inherited;
end;

procedure TJppCustomLabel.FlashBackground;
begin
  FFlash.OriginalColor := Color;
  FFlash.Transparent := Transparent;
  Transparent := False; // Transparent state is restored in the FlashFinished
  FFlash.Flash;
end;



procedure TJppCustomLabel.PropsChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TJppCustomLabel.SetAnchoredControls(const Value: TJppAnchoredControls);
begin
  FAnchoredControls := Value;
end;

procedure TJppCustomLabel.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  if not (csDestroying in ComponentState) then
    if Assigned(FAnchoredControls) then FAnchoredControls.UpdateAllControlsPos;
end;

procedure TJppCustomLabel.SetFlash(const Value: TJppFlashLabel);
begin
  FFlash := Value;
end;

procedure TJppCustomLabel.Notification(AComponent: TComponent; Operation: TOperation);
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

procedure TJppCustomLabel.SetTagExt(const Value: TJppTagExt);
begin
  FTagExt := Value;
end;

{$IFDEF FPC}
procedure TJppCustomLabel.SetEllipsisPosition(AValue: TEllipsisPosition);
begin
  if FEllipsisPosition = AValue then Exit;
  FEllipsisPosition := AValue;
  PropsChanged(Self);
end;
{$ENDIF}

{$endregion TJppCustomLabel}



{$region '                       TJppCustomShadowLabel                         '}

constructor TJppCustomShadowLabel.Create(AOwner: TComponent);
begin
  inherited;
  FAppearance := TJppShadowLabelAppearance.Create(Self);
  FAppearance.OnChange := PropsChanged;
end;

destructor TJppCustomShadowLabel.Destroy;
begin
  FAppearance.Free;
  inherited;
end;

{$IFDEF DCC}
procedure TJppCustomShadowLabel.AdjustBounds;
begin
  inherited;
  if AutoSize then
    Width := Width + FAppearance.Padding.Left + FAppearance.Padding.Right;
end;
{$ENDIF}

procedure TJppCustomShadowLabel.Paint;
var
  Rect: TRect;
  clBorder: TColor;
  Flags: UINT;
begin
  with Canvas do
  begin
    Rect := ClientRect;

    if not Transparent then
    begin
      Brush.Color := Self.Color;
      Brush.Style := bsSolid;
      FillRect(Rect);
    end;

    Brush.Style := bsClear;

    if Enabled then clBorder := FAppearance.BorderColor else clBorder := FAppearance.DisabledBorderColor;
    if (clBorder <> clNone) and (FAppearance.BorderWidth > 0) then
    begin
      Canvas.Pen.Style := FAppearance.BorderStyle;
      Canvas.Pen.Color := clBorder;
      Canvas.Pen.Width := FAppearance.BorderWidth;
      DrawRectEx(Canvas, Rect, FAppearance.DrawLeftBorder, FAppearance.DrawRightBorder, FAppearance.DrawTopBorder, FAppearance.DrawBottomBorder, FAppearance.Border3D);
      Canvas.Pen.Style := psSolid;
    end;

    if not FAppearance.Padding.IsZero then InflateRectWithMargins(Rect, FAppearance.Padding);

    if (EllipsisPosition <> epNone) and not AutoSize then Flags := 0
    else Flags := DT_EXPANDTABS;

    DoDrawText(Rect, Flags);
  end;
end;

procedure TJppCustomShadowLabel.DoDrawText(var Rect: TRect; Flags: Longint);
begin
  if Caption = '' then Exit;

  {$IFDEF FPC}
  Flags := DrawCtrlTextBiDiModeFlags(Self, Flags);
  {$ELSE}
  Flags := DrawTextBiDiModeFlags(Flags);
  {$ENDIF}
  Canvas.Font.Assign(Font);

  if Enabled then
    DrawTextEx(Canvas, Caption, Rect, Flags, Layout, Alignment, WordWrap, EllipsisPosition, ShowAccelChar, FAppearance.CaptionShadow.AsShadowParamsRec)
  else
  begin
    Canvas.Font.Color := FAppearance.DisabledCaptionColor;
    DrawTextEx(Canvas, Caption, Rect, Flags, Layout, Alignment, WordWrap, EllipsisPosition, ShowAccelChar, FAppearance.DisabledCaptionShadow.AsShadowParamsRec);
    Canvas.Font.Color := Font.Color;
  end;
end;

procedure TJppCustomShadowLabel.SetAppearance(const Value: TJppShadowLabelAppearance);
begin
  FAppearance := Value;
end;

{$endregion TJppCustomShadowLabel}


{$Region '                   TJppShadowLabelAppearance                  '}

constructor TJppShadowLabelAppearance.Create(AOwner: TComponent);
begin
  inherited Create;
  FOwner := AOwner;

  FCaptionShadow := TJppTextShadowParams.Create(AOwner);
  FCaptionShadow.OnChange := PropsChanged;

  FDisabledCaptionColor := clBtnShadow;

  FDisabledCaptionShadow := TJppTextShadowParams.Create(AOwner);
  FDisabledCaptionShadow.Color := clBtnHighlight;
  FDisabledCaptionShadow.OnChange := PropsChanged;

  FBorderColor := clBlack;
  FBorderWidth := 0;
  FBorderStyle := psSolid;
  FDisabledBorderColor := clSilver;
  FDrawLeftBorder := True;
  FDrawTopBorder := True;
  FDrawBottomBorder := True;
  FDrawRightBorder := True;
  FBorder3D := True;

  FPadding := TJppPadding.Create(AOwner);
  FPadding.OnChange := PropsChanged;
end;

destructor TJppShadowLabelAppearance.Destroy;
begin
  FCaptionShadow.Free;
  FDisabledCaptionShadow.Free;
  FPadding.Free;
  inherited;
end;

procedure TJppShadowLabelAppearance.Assign(const Source: TJppShadowLabelAppearance);
begin
  FCaptionShadow.Assign(Source.CaptionShadow);
  FDisabledCaptionColor := Source.DisabledCaptionColor;
  FDisabledCaptionShadow.Assign(Source.DisabledCaptionShadow);
  FBorderColor := Source.BorderColor;
  FBorderWidth := Source.BorderWidth;
  FBorderStyle := Source.BorderStyle;
  FDrawLeftBorder := Source.DrawLeftBorder;
  FDrawTopBorder := Source.DrawTopBorder;
  FDrawBottomBorder := Source.DrawBottomBorder;
  FDrawRightBorder := Source.DrawRightBorder;
  FBorder3D := Source.Border3D;
  FDisabledBorderColor := Source.DisabledBorderColor;
  PropsChanged(Self);
end;

procedure TJppShadowLabelAppearance.SetBorder3D(const Value: Boolean);
begin
  if FBorder3D = Value then Exit;
  FBorder3D := Value;
  PropsChanged(Self);
end;

procedure TJppShadowLabelAppearance.SetBorderColor(const Value: TColor);
begin
  if FBorderColor = Value then Exit;
  FBorderColor := Value;
  PropsChanged(Self);
end;

procedure TJppShadowLabelAppearance.SetBorderStyle(const Value: TPenStyle);
begin
  if FBorderStyle = Value then Exit;
  FBorderStyle := Value;
  PropsChanged(Self);
end;

procedure TJppShadowLabelAppearance.SetBorderWidth(const Value: Byte);
begin
  if FBorderWidth = Value then Exit;
  FBorderWidth := Value;
  PropsChanged(Self);
end;

procedure TJppShadowLabelAppearance.SetCaptionShadow(const Value: TJppTextShadowParams);
begin
  FCaptionShadow := Value;
  PropsChanged(Self);
end;

procedure TJppShadowLabelAppearance.SetDisabledBorderColor(const Value: TColor);
begin
  if FDisabledBorderColor = Value then Exit;
  FDisabledBorderColor := Value;
  PropsChanged(Self);
end;

procedure TJppShadowLabelAppearance.SetDisabledCaptionColor(const Value: TColor);
begin
  if FDisabledCaptionColor = Value then Exit;
  FDisabledCaptionColor := Value;
  PropsChanged(Self);
end;


procedure TJppShadowLabelAppearance.SetDisabledCaptionShadow(const Value: TJppTextShadowParams);
begin
  FDisabledCaptionShadow := Value;
  PropsChanged(Self);
end;

procedure TJppShadowLabelAppearance.SetDrawBottomBorder(const Value: Boolean);
begin
  if FDrawBottomBorder = Value then Exit;
  FDrawBottomBorder := Value;
  PropsChanged(Self);
end;

procedure TJppShadowLabelAppearance.SetDrawLeftBorder(const Value: Boolean);
begin
  if FDrawLeftBorder = Value then Exit;
  FDrawLeftBorder := Value;
  PropsChanged(Self);
end;

procedure TJppShadowLabelAppearance.SetDrawRightBorder(const Value: Boolean);
begin
  if FDrawRightBorder = Value then Exit;
  FDrawRightBorder := Value;
  PropsChanged(Self);
end;

procedure TJppShadowLabelAppearance.SetDrawTopBorder(const Value: Boolean);
begin
  if FDrawTopBorder = Value then Exit;
  FDrawTopBorder := Value;
  PropsChanged(Self);
end;

procedure TJppShadowLabelAppearance.SetPadding(const Value: TJppPadding);
begin
  FPadding := Value;
  PropsChanged(Self);
end;

{$endregion TJppShadowLabelAppearance}




{$region ' -------------------- TJppFlashLabel ---------------------- '}

constructor TJppFlashLabel.Create(ALabel: TJppCustomLabel);
begin
  inherited Create(ALabel);
  FLabel := ALabel;
  Enabled := True;
  FlashCount := 3;
  FlashInterval := 150;

  OnFlashColorChanged := FlashColorChanged;
  OnFlashFinished := FlashFinished;
  FreeOnFlashFinished := False;

  if FLabel is TJppCustomLabel then
    with (FLabel as TJppCustomLabel) do
    begin
      OriginalColor := Color;
      FTransparent := Transparent;
    end;
end;

procedure TJppFlashLabel.FlashColorChanged(Sender: TObject; const AColor: TColor);
begin
  if FLabel is TJppCustomLabel then
    with (FLabel as TJppCustomLabel) do
    begin
      Color := AColor;
    end;
end;

procedure TJppFlashLabel.FlashFinished(Sender: TObject);
begin
  if FLabel is TJppCustomLabel then
    with (FLabel as TJppCustomLabel) do
    begin
      Color := OriginalColor;
      Transparent := FTransparent;
    end;
end;

procedure TJppFlashLabel.SetTransparent(const Value: Boolean);
begin
  FTransparent := Value;
end;

{$endregion TJppFlashJppEdit}

end.
