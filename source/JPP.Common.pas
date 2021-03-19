unit JPP.Common;

{$I jpp.inc}
{$IFDEF FPC}
  {$mode delphi}
  {$MODESWITCH ADVANCEDRECORDS}
{$ENDIF}

interface

uses
  {$IFDEF MSWINDOWS}Windows,{$ENDIF}
  SysUtils, Classes,
  Controls, StdCtrls, Buttons, Graphics, Dialogs, Types,
  {$IFDEF HAS_SYSTEM_UITYPES}System.UITypes,{$ENDIF}
  {$IFDEF FPC}LCLType, LCLIntf,{$ENDIF}
  {$IFDEF DELPHI2009}Generics.Collections,{$ENDIF}
  JPL.Colors,
  JPP.Types, JPP.Gradient, JPP.Graphics;


{$IFDEF FPC}
type
  TVerticalAlignment = (taAlignTop, taAlignBottom, taVerticalCenter);
  TEllipsisPosition = (epNone, epPathEllipsis, epEndEllipsis, epWordEllipsis);

{.$IFDEF UNIX}
const
  DT_PATH_ELLIPSIS = $4000;
  DT_END_ELLIPSIS = $8000;
  DT_MODIFYSTRING = $10000;
  DT_RTLREADING = $20000;
  DT_WORD_ELLIPSIS = $40000;
{.$ENDIF}

{$ENDIF} // FPC


type

  TJppFocusRectType = (frtSystem, frtCustom, frtNone);
  TJppTextAlignment = (talTopLeft, talTopCenter, talTopRight, talLeft, talCenter, talRight, talBottomLeft, talBottomCenter, talBottomRight);


  {$IFDEF DELPHI2009}
  TList<T> = class(Generics.Collections.TList<T>)
  public
    function First: T; inline;
    function Last: T; inline;
  end;

  TObjectList<T: class> = class(Generics.Collections.TObjectList<T>)
  public
    function Last: T; inline;
  end;
  {$ENDIF}

  TJppShadowParamsRec = record
    Color: TColor;
    ShiftX: ShortInt;
    ShiftY: ShortInt;
    procedure Initialize(const AColor: TColor; const AShiftX, AShiftY: ShortInt);
    procedure Clear;
  end;


  {$region ' ------------ TJppPersistent ------------- '}
  TJppPersistent = class(TPersistent)
  private
    FUpdateCounter: integer;
    FOnChange: TNotifyEvent;
    procedure SetOnChange(const Value: TNotifyEvent);
  protected
    function Updating: Boolean;
    procedure PropsChanged(Sender: TObject);
  public
    constructor Create;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure EndUpdate(bCallOnChangeAfterUpdate: Boolean = True; bResetUpdatingState: Boolean = False);
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
  end;
  {$endregion TJppPersistent}


  {$region ' ----------- TJppMargins -------------- '}
  TJppMargins = class(TJppPersistent)
  private
    FLeft: integer;
    FRight: integer;
    FTop: integer;
    FBottom: integer;
    procedure SetLeft(const Value: integer);
    procedure SetRight(const Value: integer);
    procedure SetTop(const Value: integer);
    procedure SetBottom(const Value: integer);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(Margins: TJppMargins); reintroduce;
    procedure SetMargins(const xLeft, xRight, xTop, xBottom: integer);
    procedure Initialize(const xLeft, xRight, xTop, xBottom: integer);
    function IsZero: Boolean;
  published
    property Left: integer read FLeft write SetLeft default 0;
    property Right: integer read FRight write SetRight default 0;
    property Top: integer read FTop write SetTop default 0;
    property Bottom: integer read FBottom write SetBottom default 0;
  end;
  {$endregion TJppMargins}


  TJppPadding = class(TJppMargins);


  {$region ' --------- TJppTagExt ------------- '}
  TJppTagExt = class(TPersistent)
  private
    FIntValue: integer;
    FStrValue: string;
    FRealValue: Real;
    FDateValue: TDateTime;
    FPointerValue: Pointer;
    procedure SetIntValue(const Value: integer);
    procedure SetStrValue(const Value: string);
    procedure SetRealValue(const Value: Real);
    procedure SetDateValue(const Value: TDateTime);
    procedure SetPointerValue(const Value: Pointer);
  public
    constructor Create({%H-}AOwner: TComponent);
    destructor Destroy; override;
    property PointerValue: Pointer read FPointerValue write SetPointerValue;
    property DateValue: TDateTime read FDateValue write SetDateValue;
  published
    property IntValue: integer read FIntValue write SetIntValue default 0;
    property StrValue: string read FStrValue write SetStrValue;
    property RealValue: Real read FRealValue write SetRealValue;
  end;
  {$endregion}


  {$region ' ------------------ TJppControlBoundLabel -------------------- '}
  TJppControlBoundLabel = class(TCustomLabel)
  private
    FOnAdjustBounds: TNotifyEvent;
    function GetTop: Integer;
    function GetLeft: Integer;
    function GetWidth: Integer;
    function GetHeight: Integer;
    procedure SetHeight(const Value: Integer);
    procedure SetWidth(const Value: Integer);
    procedure SetOnAdjustBounds(const Value: TNotifyEvent);
  protected
    {$IFDEF DCC}procedure AdjustBounds; override;{$ENDIF}
  public
    constructor Create(AOwner: TComponent); overload; override;
    constructor Create(AOwner: TComponent; AName: string); {overload;} reintroduce; overload;  //XE5: TCustomLabel - TGraphicControl - TControl - TComponent.Create (virtual)
    // http://docwiki.embarcadero.com/RADStudio/Sydney/en/W1010_Method_'%25s'_hides_virtual_method_of_base_type_'%25s'_(Delphi)
  published
    property OnAdjustBounds: TNotifyEvent read FOnAdjustBounds write SetOnAdjustBounds;

    property BiDiMode;
    property Caption;
    property Color;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Font;
    property Height: Integer read GetHeight write SetHeight;
    property Left: Integer read GetLeft;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowAccelChar;
    property ShowHint;
    property Top: Integer read GetTop;
    {$IFDEF DELPHI2010_OR_ABOVE}property Touch;{$ENDIF}
    property Transparent;
    property Layout;
    property WordWrap;
    property Width: Integer read GetWidth write SetWidth;
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
    property OnStartDock;
    property OnStartDrag;
    property FocusControl;
  end;
  {$endregion TJppControlBoundLabel}


  {$region ' ---------- TJppGradient -------------- '}
  TJppGradient = class(TJppPersistent)
  private
    FColorStart: TColor;
    FColorEnd: TColor;
    FGradientType: TJppGradientType;
    FSteps: Byte;
    //FOnChange: TNotifyEvent;
    procedure SetColorStart(const Value: TColor);
    procedure SetColorEnd(const Value: TColor);
    procedure SetGradientType(const Value: TJppGradientType);
    procedure SetSteps(const Value: Byte);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(gr: TJppGradient); reintroduce;
  published
    property ColorStart: TColor read FColorStart write SetColorStart;
    property ColorEnd: TColor read FColorEnd write SetColorEnd;
    property GradientType: TJppGradientType read FGradientType write SetGradientType;
    property Steps: Byte read FSteps write SetSteps default 255;
    //property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;
  {$endregion}


  {$region ' --------------- TJppGradientEx ---------------- '}
  // TcyGradient extracted from VCL.cyClasses.pas (Cindy Components http://sourceforge.net/projects/tcycomponents/)
  // Class name changed to TJppGradientEx to avoid conflicts
  // FromColor -> ColorFrom
  // ToColor -> ColorTo
  TJppGradientEx = class(TJppPersistent)
  private
    FAngleViewCache: TBitmap; // Buffer with angle gradient
    FAngleViewModified: Boolean;
    FAngleClipRect: Boolean;
    FBalance: Word;
    FColorFrom: TColor;
    FColorTo: TColor;
    FOrientation: TDgradOrientation;
    //FOnChange: TNotifyEvent;
    FAngleDegree: Word;
    FBalanceMode: TDgradBalanceMode;
    FMaxDegrade: byte;
    FSpeedPercent: Integer;
    procedure SetBalance(const Value: Word);
    procedure SetColorFrom(const Value: TColor);
    procedure SetColorTo(const Value: TColor);
    procedure SetOrientation(const Value: TDgradOrientation);
    procedure SetAngleDegree(const Value: Word);
    procedure SetBalanceMode(const Value: TDgradBalanceMode);
    procedure SetMaxDegrade(const Value: byte);
    procedure SetSpeedPercent(const Value: Integer);
  protected
  public
    constructor Create(AOwner: TComponent); virtual;
    destructor Destroy; override;
    procedure Assign(grex: TJppGradientEx); reintroduce;
    procedure Draw(aCanvas: TCanvas; aRect: TRect);
    property AngleClipRect: Boolean read FAngleClipRect write FAngleClipRect default false;
  published
    property AngleDegree: Word read FAngleDegree Write SetAngleDegree default 45;
    property Balance: Word read FBalance write SetBalance default 50;
    property BalanceMode: TDgradBalanceMode read FBalanceMode write SetBalanceMode default bmNormal;
    property MaxDegrade: byte read FMaxDegrade write SetMaxDegrade default 255;
    property Orientation: TDgradOrientation read FOrientation write SetOrientation default dgdVertical;
    property SpeedPercent: Integer read FSpeedPercent write SetSpeedPercent; // default 100; Cannot set default because SpeedPercent modified in some components
    property ColorFrom: TColor read FColorFrom write SetColorFrom;
    property ColorTo: TColor read FColorTo write SetColorTo;
    //property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;
  {$endregion}


  {$region ' ---------- TJppBorder ------------ '}
  TJppBorder = class(TJppPersistent)
  private
    FOwner: TComponent;
    FColor: TColor;
    FWidth: integer;
    FVisible: Boolean;
    FStyle: TPenStyle;
    procedure SetColor(const Value: TColor);
    procedure SetWidth(const Value: integer);
    procedure SetVisible(const Value: Boolean);
    procedure SetStyle(const Value: TPenStyle);
  protected
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(Border: TJppBorder); reintroduce;
  published
    property Color: TColor read FColor write SetColor default clSilver;
    property Width: integer read FWidth write SetWidth default 1;
    property Style: TPenStyle read FStyle write SetStyle default TPenStyle.psSolid;
    property Visible: Boolean read FVisible write SetVisible default True;
  end;
  {$endregion TJppBorder}


  {$region ' ----------- TJppBorders ------------ '}
  TJppBorders = class(TJppPersistent)
  private
    FOwner: TComponent;
    //FOnChange: TNotifyEvent;
    FLeft: TJppBorder;
    FRight: TJppBorder;
    FTop: TJppBorder;
    FBottom: TJppBorder;
    //procedure SetOnChange(const Value: TNotifyEvent);
    procedure SetLeft(const Value: TJppBorder);
    procedure SetRight(const Value: TJppBorder);
    procedure SetTop(const Value: TJppBorder);
    procedure SetBottom(const Value: TJppBorder);
  protected
    //procedure PropsChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(Borders: TJppBorders); reintroduce;
  published
    //property OnChange: TNotifyEvent read FOnChange write SetOnChange;
    property Left: TJppBorder read FLeft write SetLeft;
    property Right: TJppBorder read FRight write SetRight;
    property Top: TJppBorder read FTop write SetTop;
    property Bottom: TJppBorder read FBottom write SetBottom;
  end;
  {$endregion TJppBorders}


  {$region ' ---------- TJppGradientBackground ----------- '}

  TJppGradientBackground = class(TJppPersistent)
  private
    FOwner: TComponent;
    //FOnChange: TNotifyEvent;
    FGradient: TJppGradientEx;
    FColor: TColor;
    FBorders: TJppBorders;
    FDrawGradient: Boolean;
    FDrawBorders: Boolean;
    //procedure SetOnChange(const Value: TNotifyEvent);
    procedure SetGradient(const Value: TJppGradientEx);
    procedure SetColor(const Value: TColor);
    procedure SetBorders(const Value: TJppBorders);
    procedure SetDrawGradient(const Value: Boolean);
    procedure SetDrawBorders(const Value: Boolean);
  protected
    //procedure PropsChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(grb: TJppGradientBackground); reintroduce;
  published
    //property OnChange: TNotifyEvent read FOnChange write SetOnChange;
    property Gradient: TJppGradientEx read FGradient write SetGradient;
    property Color: TColor read FColor write SetColor default clBtnFace;
    property Borders: TJppBorders read FBorders write SetBorders;
    property DrawGradient: Boolean read FDrawGradient write SetDrawGradient default True;
    property DrawBorders: Boolean read FDrawBorders write SetDrawBorders default True;
  end;
  {$endregion TJppGradientBackground}


  {$region ' ------- TJppFocusRectParams ---------- '}
  TJppFocusRectParams = class(TJppPersistent)
  private
    FPen: TPen;
    FSpacing: integer;
    FFocusType: TJppFocusRectType;
    //FOnChange: TNotifyEvent;
    procedure SetPen(const Value: TPen);
    procedure SetSpacing(const Value: integer);
    procedure SetFocusType(const Value: TJppFocusRectType);
    //procedure SetOnChange(const Value: TNotifyEvent);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
  protected
    //procedure PropsChanged(Sender: TObject);
  published
    property FocusType: TJppFocusRectType read FFocusType write SetFocusType default frtNone;
    property Pen: TPen read FPen write SetPen;
    property Spacing: integer read FSpacing write SetSpacing default 2;
    //property OnChange: TNotifyEvent read FOnChange write SetOnChange;
  end;
  {$endregion}


  {$Region '   TJppTextShadowParams   '}
  TJppTextShadowParams = class(TJppPersistent)
  private
    FShiftX: ShortInt;
    FShiftY: ShortInt;
    FColor: TColor;
    FEnabled: Boolean;
    procedure SetShiftX(const Value: ShortInt);
    procedure SetShiftY(const Value: ShortInt);
    procedure SetColor(const Value: TColor);
    procedure SetEnabled(const Value: Boolean);
  protected
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(const Source: TJppTextShadowParams); reintroduce;
    function AsShadowParamsRec: TJppShadowParamsRec;
  published
    property ShiftX: ShortInt read FShiftX write SetShiftX default 1;
    property ShiftY: ShortInt read FShiftY write SetShiftY default 1;
    property Color: TColor read FColor write SetColor default clSilver;
    property Enabled: Boolean read FEnabled write SetEnabled default False;
  end;
  {$endregion TJppTextShadowParams}


  
implementation



{$region ' -----------------------------  TJppPersistent ----------------------------- '}

constructor TJppPersistent.Create;
begin
  inherited Create;
  FUpdateCounter := 0;
  FOnChange := nil;
end;

destructor TJppPersistent.Destroy;
begin
  inherited Destroy;
end;

procedure TJppPersistent.BeginUpdate;
begin
  Inc(FUpdateCounter);
end;

procedure TJppPersistent.EndUpdate(bCallOnChangeAfterUpdate: Boolean = True; bResetUpdatingState: Boolean = False);
begin
  if bResetUpdatingState then FUpdateCounter := 0
  else
  begin
    Dec(FUpdateCounter);
    if FUpdateCounter < 0 then FUpdateCounter := 0;
  end;

  if (FUpdateCounter = 0) and bCallOnChangeAfterUpdate then PropsChanged(Self);
end;

function TJppPersistent.Updating: Boolean;
begin
  Result := FUpdateCounter > 0;
end;

procedure TJppPersistent.PropsChanged(Sender: TObject);
begin
  if Assigned(FOnChange) then FOnChange(Sender);
end;

procedure TJppPersistent.SetOnChange(const Value: TNotifyEvent);
begin
  FOnChange := Value;
end;


{$endregion TJppPersistent}


{$region ' ------------------------------ TJppTagExt ------------------------------ '}
constructor TJppTagExt.Create(AOwner: TComponent);
begin
  inherited Create;
  FIntValue := 0;
  FStrValue := '';
  FRealValue := 0;
  FPointerValue := nil;
  FDateValue := Now;
end;

destructor TJppTagExt.Destroy;
begin
  inherited;
end;


procedure TJppTagExt.SetDateValue(const Value: TDateTime);
begin
  FDateValue := Value;
end;

procedure TJppTagExt.SetIntValue(const Value: integer);
begin
  FIntValue := Value;
end;

procedure TJppTagExt.SetPointerValue(const Value: Pointer);
begin
  FPointerValue := Value;
end;

procedure TJppTagExt.SetRealValue(const Value: Real);
begin
  FRealValue := Value;
end;

procedure TJppTagExt.SetStrValue(const Value: string);
begin
  FStrValue := Value;
end;
{$endregion}


{$region ' ------------------------------------------ TJppControlBoundLabel ---------------------------------------------- '}

constructor TJppControlBoundLabel.Create(AOwner: TComponent);
begin
  Create(AOwner, 'SubLabel');
end;

constructor TJppControlBoundLabel.Create(AOwner: TComponent; AName: string);
begin
  inherited Create(AOwner);
  Name := AName;
  SetSubComponent(True);
  if Assigned(AOwner) then Caption := AOwner.Name;
end;

{$IFDEF DCC}
procedure TJppControlBoundLabel.AdjustBounds;
begin
  inherited AdjustBounds;
  if Assigned(FOnAdjustBounds) then FOnAdjustBounds(Self);
end;
{$ENDIF}

function TJppControlBoundLabel.GetHeight: Integer;
begin
  Result := inherited Height;
end;

function TJppControlBoundLabel.GetLeft: Integer;
begin
  Result := inherited Left;
end;

function TJppControlBoundLabel.GetTop: Integer;
begin
  Result := inherited Top;
end;

function TJppControlBoundLabel.GetWidth: Integer;
begin
  Result := inherited Width;
end;

procedure TJppControlBoundLabel.SetHeight(const Value: Integer);
begin
  SetBounds(Left, Top, Width, Value);
end;

procedure TJppControlBoundLabel.SetOnAdjustBounds(const Value: TNotifyEvent);
begin
  FOnAdjustBounds := Value;
end;

procedure TJppControlBoundLabel.SetWidth(const Value: Integer);
begin
  SetBounds(Left, Top, Value, Height);
end;
{$endregion TJppControlBoundLabel}


{$region ' ---------------------- TJppGradient --------------------------- '}

constructor TJppGradient.Create(AOwner: TComponent);
begin
  inherited Create;
  ColorStart := clWindow;
  ColorEnd := clBtnFace;
  GradientType := gtHorizontalBar;
  Steps := 255;
end;

destructor TJppGradient.Destroy;
begin
  inherited;
end;

procedure TJppGradient.Assign(gr: TJppGradient);
begin
  BeginUpdate;
  try
    FColorStart := gr.ColorStart;
    FColorEnd := gr.ColorEnd;
    FGradientType := gr.GradientType;
    FSteps := gr.Steps;
  finally
    EndUpdate;
  end;
end;

procedure TJppGradient.SetColorEnd(const Value: TColor);
begin
  FColorEnd := Value;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TJppGradient.SetColorStart(const Value: TColor);
begin
  FColorStart := Value;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TJppGradient.SetGradientType(const Value: TJppGradientType);
begin
  FGradientType := Value;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TJppGradient.SetSteps(const Value: Byte);
begin
  FSteps := Value;
  if Assigned(FOnChange) then FOnChange(Self);
end;

{$endregion TJppGradient}


{$region ' ---------------------------- TJppFocusRectParams ---------------------------------- '}
constructor TJppFocusRectParams.Create(AOwner: TComponent);
begin
  inherited Create;
  FPen := TPen.Create;
  FSpacing := 2;
  FFocusType := frtNone;
  FPen.OnChange := PropsChanged;
end;

destructor TJppFocusRectParams.Destroy;
begin
  FPen.Free;
  inherited;
end;

//procedure TJppFocusRectParams.PropsChanged(Sender: TObject);
//begin
//  if Assigned(OnChange) then OnChange(Self);
//end;

procedure TJppFocusRectParams.SetPen(const Value: TPen);
begin
  FPen := Value;
  PropsChanged(Self);
end;

procedure TJppFocusRectParams.SetFocusType(const Value: TJppFocusRectType);
begin
  FFocusType := Value;
  PropsChanged(Self);
end;

//procedure TJppFocusRectParams.SetOnChange(const Value: TNotifyEvent);
//begin
//  FOnChange := Value;
//end;

procedure TJppFocusRectParams.SetSpacing(const Value: integer);
begin
  FSpacing := Value;
  PropsChanged(Self);
end;

{$endregion}


{$region ' ------------------------------------ TJppGradientEx -------------------------------- '}
constructor TJppGradientEx.Create(AOwner: TComponent);
begin
  FBalance := 50;
  FColorFrom := clWhite;
  FColorTo := clBtnFace;
  FOrientation := dgdVertical;
  FBalanceMode := bmNormal;
  FMaxDegrade := 255;
  FSpeedPercent := 100;
  FAngleDegree := 45;
  FAngleViewModified := true;
  FAngleViewCache := Nil;
  FAngleClipRect := false;
end;

destructor TJppGradientEx.Destroy;
begin
  if Assigned(FAngleViewCache) then
  begin
    FAngleViewCache.Free;
    FAngleViewCache := Nil;
  end;

  inherited;
end;

procedure TJppGradientEx.Draw(aCanvas: TCanvas; aRect: TRect);
var
  Size: Integer;
  Rgn: HRGN;
  P: TPoint;
begin
  Rgn := 0;
  if FColorFrom = FColorTo then
  begin
    aCanvas.Brush.Color := FColorFrom;
    aCanvas.Brush.Style := bsSolid;
    aCanvas.FillRect(aRect);
  end
  else
  begin
    if FOrientation = dgdAngle then
    begin
      // We use FAngleViewCache bitmap as an image buffer in order to avoid recalculating gradient for each time Draw() is called :
      Size := Round( Sqrt( Sqr(aRect.Right - aRect.Left) + Sqr(aRect.Bottom - aRect.Top) ) );

      if FAngleViewModified or (FAngleViewCache.Width <> Size) then
      begin
        cyGradientFill(aCanvas, aRect, FColorFrom, FColorTo, FOrientation, FBalance, FAngleDegree, FBalanceMode, FMaxDegrade, FSpeedPercent, FAngleClipRect, FAngleViewCache);
        FAngleViewModified := false;
      end
      else begin
        if FAngleClipRect then
        begin
          Rgn := CreateRectRgn(aRect.Left, aRect.Top, aRect.Right, aRect.Bottom);
          GetWindowOrgEx(aCanvas.Handle, P);
          OffsetRgn(Rgn, -P.X, -P.Y);
          SelectClipRgn(aCanvas.Handle, Rgn);
        end;
        aCanvas.Draw( (aRect.Left+(aRect.Right-aRect.Left) - FAngleViewCache.Width) div 2, (aRect.Top+(aRect.Bottom-aRect.Top) - FAngleViewCache.Height) div 2, FAngleViewCache );
        if FAngleClipRect then
        begin
          SelectClipRgn(aCanvas.Handle, HRGN(nil));
          DeleteObject(Rgn);
        end;
      end;
    end
    else
      cyGradientFill(aCanvas, aRect, FColorFrom, FColorTo, FOrientation, FBalance, FAngleDegree, FBalanceMode, FMaxDegrade, FSpeedPercent);
  end;
end;

procedure TJppGradientEx.Assign(grex: TJppGradientEx);
begin
  BeginUpdate;
  try
    FAngleDegree := grex.AngleDegree;
    FBalance := grex.Balance;
    FBalanceMode := grex.BalanceMode;
    FMaxDegrade := grex.MaxDegrade;
    FOrientation := grex.Orientation;
    FSpeedPercent := grex.SpeedPercent;
    FColorFrom := grex.ColorFrom;
    FColorTo := grex.ColorTo;
    FAngleViewModified := True;
    { Do not assign
    FAngleViewCache
    FAngleClipRect
    }
  finally
    EndUpdate;
  end;

  //if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TJppGradientEx.SetColorFrom(const Value: TColor);
begin
  if Value <> FColorFrom then
  begin
    FColorFrom:= Value;
    FAngleViewModified := true;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TJppGradientEx.SetColorTo(const Value: TColor);
begin
  if Value <> FColorTo then
  begin
    FColorTo:= Value;
    FAngleViewModified := true;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TJppGradientEx.SetOrientation(const Value: TDgradOrientation);
begin
  if Value = fOrientation then Exit;

  FOrientation := Value;
  FAngleViewModified := true;

  if Value <> dgdAngle then
  begin
    if Assigned(FAngleViewCache) then
    begin
      FAngleViewCache.Free;
      FAngleViewCache := Nil;
    end;
  end
  else
    FAngleViewCache := TBitmap.Create;

  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TJppGradientEx.SetSpeedPercent(const Value: Integer);
begin
  if (Value <> FSpeedPercent) and (Value In [0..100]) then
  begin
    FSpeedPercent := Value;
    FAngleViewModified := true;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TJppGradientEx.SetBalance(const Value: Word);
begin
  if (Value <> FBalance) and (Value In [0..100]) then
  begin
    FBalance := Value;
    FAngleViewModified := true;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TJppGradientEx.SetBalanceMode(const Value: TDgradBalanceMode);
begin
  if (Value <> FBalanceMode) then
  begin
    FBalanceMode := Value;
    FAngleViewModified := true;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TJppGradientEx.SetAngleDegree(const Value: Word);
begin
  if (Value <> FAngleDegree) and (Value < 360) then
  begin
    FAngleDegree := Value;
    FAngleViewModified := true;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;

procedure TJppGradientEx.SetMaxDegrade(const Value: byte);
begin
  if (Value <> FMaxDegrade) and (Value <> 0) then
  begin
    FMaxDegrade := Value;
    FAngleViewModified := true;
    if Assigned(FOnChange) then FOnChange(Self);
  end;
end;
{$endregion TJppGradientEx}


{$region ' -------------------------------- TJppGradientBackground --------------------------- '}

constructor TJppGradientBackground.Create(AOwner: TComponent);
begin
  inherited Create;
  FOwner := AOwner;

  FGradient := TJppGradientEx.Create(AOwner);
  FGradient.OnChange := PropsChanged;

  FBorders := TJppBorders.Create(AOwner);
  FBorders.OnChange := PropsChanged;

  FColor := clBtnFace;
  FDrawGradient := True;
  FDrawBorders := True;
end;

destructor TJppGradientBackground.Destroy;
begin
  FBorders.Free;
  FGradient.Free;
  inherited;
end;

procedure TJppGradientBackground.Assign(grb: TJppGradientBackground);
begin
  BeginUpdate;
  try
    FGradient.Assign(grb.Gradient);
    FColor := grb.Color;
    FBorders.Assign(grb.Borders);
    FDrawGradient := grb.DrawGradient;
    FDrawBorders := grb.DrawBorders;
  finally
    EndUpdate;
  end;
end;

//procedure TJppGradientBackground.PropsChanged(Sender: TObject);
//begin
//  if Assigned(FOnChange) then FOnChange(Self);
//end;

procedure TJppGradientBackground.SetBorders(const Value: TJppBorders);
begin
  FBorders := Value;
  PropsChanged(Self);
end;

procedure TJppGradientBackground.SetColor(const Value: TColor);
begin
  FColor := Value;
  PropsChanged(Self);
end;

procedure TJppGradientBackground.SetDrawBorders(const Value: Boolean);
begin
  if FDrawBorders = Value then Exit;
  FDrawBorders := Value;
  PropsChanged(Self);
end;

procedure TJppGradientBackground.SetDrawGradient(const Value: Boolean);
begin
  if FDrawGradient = Value then Exit;
  FDrawGradient := Value;
  PropsChanged(Self);
end;

procedure TJppGradientBackground.SetGradient(const Value: TJppGradientEx);
begin
  FGradient := Value;
  PropsChanged(Self);
end;

//procedure TJppGradientBackground.SetOnChange(const Value: TNotifyEvent);
//begin
//  FOnChange := Value;
//end;

{$endregion TJppGradientBackground}


{$region ' ---------------------- TJppBorder ---------------------- '}

constructor TJppBorder.Create(AOwner: TComponent);
begin
  inherited Create;
  FOwner := AOwner;
  FColor := clSilver;
  FWidth := 1;
  FVisible := True;
  FStyle := TPenStyle.psSolid;
end;

destructor TJppBorder.Destroy;
begin
  inherited;
end;

procedure TJppBorder.Assign(Border: TJppBorder);
begin
  BeginUpdate;
  try
    FColor := Border.Color;
    FWidth := Border.Width;
    FVisible := Border.Visible;
    FStyle := Border.Style;
  finally
    EndUpdate(True);
  end;
end;

procedure TJppBorder.SetColor(const Value: TColor);
begin
  if FColor = Value then Exit;
  FColor := Value;
  PropsChanged(Self);
end;

procedure TJppBorder.SetStyle(const Value: TPenStyle);
begin
  if FStyle = Value then Exit;
  FStyle := Value;
  PropsChanged(Self);
end;

procedure TJppBorder.SetVisible(const Value: Boolean);
begin
  if FVisible = Value then Exit;
  FVisible := Value;
  PropsChanged(Self);
end;

procedure TJppBorder.SetWidth(const Value: integer);
begin
  if FWidth = Value then Exit;
  FWidth := Value;
  PropsChanged(Self);
end;

{$endregion TJppBorder}


{$region ' ----------------------------- TJppBorders --------------------------- '}

constructor TJppBorders.Create(AOwner: TComponent);
begin
  inherited Create;
  FOwner := AOwner;

  FLeft := TJppBorder.Create(AOwner);
  FLeft.OnChange := PropsChanged;
  FRight := TJppBorder.Create(AOwner);
  FRight.OnChange := PropsChanged;
  FTop := TJppBorder.Create(AOwner);
  FTop.OnChange := PropsChanged;
  FBottom := TJppBorder.Create(AOwner);
  FBottom.OnChange := PropsChanged;
end;

destructor TJppBorders.Destroy;
begin
  FLeft.Free;
  FRight.Free;
  FTop.Free;
  FBottom.Free;
  inherited;
end;

procedure TJppBorders.Assign(Borders: TJppBorders);
begin
  BeginUpdate;
  try
    FLeft.Assign(Borders.Left);
    FRight.Assign(Borders.Right);
    FTop.Assign(Borders.Top);
    FBottom.Assign(Borders.Bottom);
  finally
    EndUpdate(True);
  end;
end;

//procedure TJppBorders.PropsChanged(Sender: TObject);
//begin
//  if Assigned(FOnChange) then FOnChange(Self);
//end;

procedure TJppBorders.SetBottom(const Value: TJppBorder);
begin
  FBottom := Value;
end;

procedure TJppBorders.SetLeft(const Value: TJppBorder);
begin
  FLeft := Value;
end;

//procedure TJppBorders.SetOnChange(const Value: TNotifyEvent);
//begin
//  FOnChange := Value;
//end;

procedure TJppBorders.SetRight(const Value: TJppBorder);
begin
  FRight := Value;
end;

procedure TJppBorders.SetTop(const Value: TJppBorder);
begin
  FTop := Value;
end;

{$endregion TJppBorders}


{$region ' ------------------------- TJppMargins ---------------------- '}

constructor TJppMargins.Create(AOwner: TComponent);
begin
  inherited Create;
  FLeft := 0;
  FRight := 0;
  FTop := 0;
  FBottom := 0;
end;

destructor TJppMargins.Destroy;
begin
  inherited;
end;

procedure TJppMargins.Assign(Margins: TJppMargins);
begin
  FLeft := Margins.Left;
  FRight := Margins.Right;
  FTop := Margins.Top;
  FBottom := Margins.Bottom;
end;

function TJppMargins.IsZero: Boolean;
begin
  Result := (FLeft = 0) and (FRight = 0) and (FTop = 0) and (FBottom = 0);
end;

procedure TJppMargins.Initialize(const xLeft, xRight, xTop, xBottom: integer);
begin
  FLeft := xLeft;
  FRight := xRight;
  FTop := xTop;
  FBottom := xBottom;
  PropsChanged(Self);
end;

procedure TJppMargins.SetMargins(const xLeft, xRight, xTop, xBottom: integer);
begin
  Initialize(xLeft, xRight, xTop, xBottom);
end;

procedure TJppMargins.SetLeft(const Value: integer);
begin
  if FLeft = Value then Exit;
  FLeft := Value;
  PropsChanged(Self);
end;

procedure TJppMargins.SetRight(const Value: integer);
begin
  if FRight = Value then Exit;
  FRight := Value;
  PropsChanged(Self);
end;

procedure TJppMargins.SetTop(const Value: integer);
begin
  if FTop = Value then Exit;
  FTop := Value;
  PropsChanged(Self);
end;

procedure TJppMargins.SetBottom(const Value: integer);
begin
  if FBottom = Value then Exit;
  FBottom := Value;
  PropsChanged(Self);
end;

{$endregion TJppMargins}


{$Region '                    TJppTextShadowParams                    '}

constructor TJppTextShadowParams.Create(AOwner: TComponent);
begin
  inherited Create;
  FShiftX := 1;
  FShiftY := 1;
  FColor := clSilver;
  FEnabled := False;
end;

destructor TJppTextShadowParams.Destroy;
begin
  inherited;
end;

function TJppTextShadowParams.AsShadowParamsRec: TJppShadowParamsRec;
begin
  if not FEnabled then Result.Clear
  else
  begin
    Result.Color := FColor;
    Result.ShiftX := FShiftX;
    Result.ShiftY := FShiftY;
  end;
end;

procedure TJppTextShadowParams.Assign(const Source: TJppTextShadowParams);
begin
  FShiftX := Source.ShiftX;
  FShiftY := Source.ShiftY;
  FColor := Source.Color;
  FEnabled := Source.Enabled;
end;

procedure TJppTextShadowParams.SetColor(const Value: TColor);
begin
  if FColor = Value then Exit;
  FColor := Value;
  PropsChanged(Self);
end;

procedure TJppTextShadowParams.SetEnabled(const Value: Boolean);
begin
  if FEnabled = Value then Exit;
  FEnabled := Value;
  PropsChanged(Self);
end;

procedure TJppTextShadowParams.SetShiftX(const Value: ShortInt);
begin
  if FShiftX = Value then Exit;
  FShiftX := Value;
  PropsChanged(Self);
end;

procedure TJppTextShadowParams.SetShiftY(const Value: ShortInt);
begin
  if FShiftY = Value then Exit;
  FShiftY := Value;
  PropsChanged(Self);
end;

{$endregion TJppTextShadowParams}



{$IFDEF DELPHI2009}
function TList<T>.First: T;
begin
  Result := Items[0];
end;

function TList<T>.Last: T;
begin
  Result := Items[Count - 1];
end;


function TObjectList<T>.Last: T;
begin
  Result := Items[Count - 1];
end;
{$ENDIF}





{ TJppShadowParamsRec }

procedure TJppShadowParamsRec.Clear;
begin
  Self.Color := clNone;
  Self.ShiftX := 0;
  Self.ShiftY := 0;
end;

procedure TJppShadowParamsRec.Initialize(const AColor: TColor; const AShiftX, AShiftY: ShortInt);
begin
  Self.Color := AColor;
  Self.ShiftX := AShiftX;
  Self.ShiftY := AShiftY;
end;

end.


