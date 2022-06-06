unit JPP.ColorSwatch;

{
  Jacek Pazera
  http://www.pazera-software.com
  https://github.com/jackdp
}

{$I jpp.inc}
{$IFDEF FPC} {$mode delphi} {$ENDIF}


interface

uses
  {$IFDEF MSWINDOWS}Windows,{$ENDIF}
  Messages,
  SysUtils, Classes, Types, {$IFDEF DCC}{$IFDEF HAS_SYSTEM_UITYPES}System.UITypes,{$ENDIF}{$ENDIF}
  Controls, Forms, Graphics, StdCtrls, ExtCtrls, Dialogs, Clipbrd,
  {$IFDEF FPC}LCLType, LCLIntf, LMessages,{$ENDIF}
  JPL.Strings, JPL.Conversion, JPL.Colors, JPL.Rects,
  JPP.Types, JPP.Graphics, JPP.Common, JPP.Common.Procs, JPP.AnchoredControls, JPP.ColorControls.Common, JPP.BasicSpeedButton;


type

  TOnGetColorStrValue = procedure(const AColor: TColor; var ColorStr, Prefix, Suffix: string) of object;
  TOnSelectedColorChange = procedure(const CurrentColor: TColor; var NewColor: TColor) of object;

  {$region ' ------------- TJppColorSwatchColorRect ---------------- '}
  TJppColorSwatchColorRect = class(TJppPersistent)
  private
    FWidth: integer;
    FVisible: Boolean;
    FMargins: TJppMargins;
    FDrawBorder: Boolean;
    FBorderColor: TColor;
    FBorderColorMixed: Boolean;
    FBorderStyle: TPenStyle;
    procedure SetWidth(const Value: integer);
    procedure SetVisible(const Value: Boolean);
    procedure SetMargins(const Value: TJppMargins);
    procedure SetDrawBorder(const Value: Boolean);
    procedure SetBorderColor(const Value: TColor);
    procedure SetBorderColorMixed(const Value: Boolean);
    procedure SetBorderStyle(const Value: TPenStyle);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(cscr: TJppColorSwatchColorRect); reintroduce;
  published
    property Width: integer read FWidth write SetWidth default 34;
    property Visible: Boolean read FVisible write SetVisible default True;
    property Margins: TJppMargins read FMargins write SetMargins;
    property DrawBorder: Boolean read FDrawBorder write SetDrawBorder default True;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clGray;
    property BorderColorMixed: Boolean read FBorderColorMixed write SetBorderColorMixed default True;
    property BorderStyle: TPenStyle read FBorderStyle write SetBorderStyle default psSolid;
  end;
  {$endregion TJppColorSwatchColorRect}


  {$region ' ---------------- TJppColorSwatchColorValue ---------------- '}
  TJppColorSwatchColorValue = class(TJppPersistent)
  private
    FColorType: TColorType;
    FFont: TFont;
    FPrefix: string;
    FSuffix: string;
    FBackgroundColor: TColor;
    FTextPosYDelta: ShortInt;
    FVisible: Boolean;
    procedure SetColorType(const Value: TColorType);
    procedure SetFont(const Value: TFont);
    procedure SetPrefix(const Value: string);
    procedure SetSuffix(const Value: string);
    procedure SetBackgroundColor(const Value: TColor);
    procedure SetTextPosYDelta(const Value: ShortInt);
    procedure SetVisible(const Value: Boolean);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(cscv: TJppColorSwatchColorValue); reintroduce;
  published
    property Prefix: string read FPrefix write SetPrefix;
    property Suffix: string read FSuffix write SetSuffix;
    property ColorType: TColorType read FColorType write SetColorType default ctRgb;
    property Font: TFont read FFont write SetFont;
    property TextPosYDelta: ShortInt read FTextPosYDelta write SetTextPosYDelta default 0;
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor;
    property Visible: Boolean read FVisible write SetVisible default True;
  end;
  {$endregion TJppColorSwatchColorValue}


  {$region ' ----------- TJppColorSwatchAppearance ---------- '}
  TJppColorSwatchAppearance = class(TJppPersistent)
  private
    FOwner: TComponent;
    FBorderColor: TColor;
    FDrawTopBorder: Boolean;
    FDrawBottomBorder: Boolean;
    FDrawLeftBorder: Boolean;
    FDrawRightBorder: Boolean;
    FBorderStyle: TPenStyle;
    FTopColorValue: TJppColorSwatchColorValue;
    FBottomColorValue: TJppColorSwatchColorValue;
    FBackgroundColor: TColor;
    FColorRect: TJppColorSwatchColorRect;
    procedure SetBorderColor(const Value: TColor);
    procedure SetDrawTopBorder(const Value: Boolean);
    procedure SetDrawBottomBorder(const Value: Boolean);
    procedure SetDrawLeftBorder(const Value: Boolean);
    procedure SetDrawRightBorder(const Value: Boolean);
    procedure SetBorderStyle(const Value: TPenStyle);
    procedure SetTopColorValue(const Value: TJppColorSwatchColorValue);
    procedure SetBottomColorValue(const Value: TJppColorSwatchColorValue);
    procedure SetBackgroundColor(const Value: TColor);
    procedure SetColorRect(const Value: TJppColorSwatchColorRect);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(Source: TJppColorSwatchAppearance); reintroduce;
  published
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor default clBtnFace;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clGray;
    property DrawTopBorder: Boolean read FDrawTopBorder write SetDrawTopBorder default True;
    property DrawBottomBorder: Boolean read FDrawBottomBorder write SetDrawBottomBorder default True;
    property DrawLeftBorder: Boolean read FDrawLeftBorder write SetDrawLeftBorder default True;
    property DrawRightBorder: Boolean read FDrawRightBorder write SetDrawRightBorder default True;
    property BorderStyle: TPenStyle read FBorderStyle write SetBorderStyle default psSolid;

    property TopColorValue: TJppColorSwatchColorValue read FTopColorValue write SetTopColorValue;
    property BottomColorValue: TJppColorSwatchColorValue read FBottomColorValue write SetBottomColorValue;
    property ColorRect: TJppColorSwatchColorRect read FColorRect write SetColorRect;
  end;
  {$endregion}


  {$region ' --------------------------- TJppCustomBaseColorSwatch ---------------------------------- '}

  {$IFDEF MSWINDOWS}TProcOnOnDragDropFiles = procedure (Sender: TObject; var msg: TWMDropFiles) of object;{$ENDIF}

  TJppCustomBaseColorSwatch = class(TCustomPanel)
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnPaint: TNotifyEvent;
    FOnVisibleChanging: TNotifyEvent;
    FOnAfterDrawBackground: TNotifyEvent;
    {$IFDEF MSWINDOWS}FOnDragDropFiles: TProcOnOnDragDropFiles;{$ENDIF}
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    {$IFDEF MSWINDOWS}procedure CMDropFiles(var msg: TWMDropFiles); message WM_DROPFILES;{$ENDIF}
    procedure PropsChanged(Sender: TObject);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure AdjustClientRect(var Rect: TRect); override;
    procedure Paint; override;
    procedure Loaded; override;
    procedure VisibleChanging; override;
    procedure DrawBackground(ARect: TRect); virtual; // empty
    procedure MouseEnter(Sender: TObject); virtual;
    procedure MouseLeave(Sender: TObject); virtual;
    property OnAfterDrawBackground: TNotifyEvent read FOnAfterDrawBackground write FOnAfterDrawBackground;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property OnVisibleChanging: TNotifyEvent read FOnVisibleChanging write FOnVisibleChanging;
    {$IFDEF MSWINDOWS}property OnDragDropFiles: TProcOnOnDragDropFiles read FOnDragDropFiles write FOnDragDropFiles;{$ENDIF}
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
  end;
  {$endregion}


  {$region ' --------------------------------- TJppCustomColorSwatch ---------------------------------------- '}
  TJppCustomColorSwatch = class(TJppCustomBaseColorSwatch)
  private
    FAppearance: TJppColorSwatchAppearance;
    FTagExt: TJppTagExt;
    FSelectedColor: TColor;
    FOnGetTopColorStrValue: TOnGetColorStrValue;
    FOnGetBottomColorStrValue: TOnGetColorStrValue;
    FOnSelectedColorChange: TOnSelectedColorChange;
    FOnSelectedColorChanged: TNotifyEvent;
    FAnchoredControls: TJppAnchoredControls;
    procedure SetAppearance(const Value: TJppColorSwatchAppearance);
    procedure SetTagExt(const Value: TJppTagExt);
    procedure SetSelectedColor(const Value: TColor);
    procedure SetOnGetTopColorStrValue(const Value: TOnGetColorStrValue);
    procedure SetOnGetBottomColorStrValue(const Value: TOnGetColorStrValue);
    procedure SetOnSelectedColorChange(const Value: TOnSelectedColorChange);
    procedure SetOnSelectedColorChanged(const Value: TNotifyEvent);
    procedure SetAnchoredControls(const Value: TJppAnchoredControls);
  protected
    procedure DrawBackground(ARect: TRect); override;
    procedure DrawColorValue(ColorValue: TJppColorSwatchColorValue; ARect: TRect);
    procedure DrawColorRect;
    procedure DrawBorders(ARect: TRect);
    procedure PropsChanged(Sender: TObject);
    property Appearance: TJppColorSwatchAppearance read FAppearance write SetAppearance;
    procedure Paint; override;
    procedure AdjustClientRect(var Rect: TRect); override;
    property TagExt: TJppTagExt read FTagExt write SetTagExt;

    function TopColorStr: string;
    function BottomColorStr: string;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;

    property Canvas;
    property DockManager;

    property SelectedColor: TColor read FSelectedColor write SetSelectedColor default clSilver;
    property OnGetTopColorStrValue: TOnGetColorStrValue read FOnGetTopColorStrValue write SetOnGetTopColorStrValue;
    property OnGetBottomColorStrValue: TOnGetColorStrValue read FOnGetBottomColorStrValue write SetOnGetBottomColorStrValue;
    property OnSelectedColorChange: TOnSelectedColorChange read FOnSelectedColorChange write SetOnSelectedColorChange;
    property OnSelectedColorChanged: TNotifyEvent read FOnSelectedColorChanged write SetOnSelectedColorChanged;

    property AnchoredControls: TJppAnchoredControls read FAnchoredControls write SetAnchoredControls;
  published
  end;
  {$endregion}


  {$region ' ------------------------ TJppColorSwatch ------------------------------- '}
  TJppColorSwatch = class(TJppCustomColorSwatch)
  published
    property Align;
    property Anchors;
    property BiDiMode;
    property Constraints;
    {$IFDEF DCC} property Ctl3D; {$ENDIF}
    property UseDockManager default True;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FullRepaint;
    property Font;
    {$IFDEF DCC} property Locked; {$ENDIF}
    {$IFDEF DELPHI2009_OR_ABOVE} property Padding; {$ENDIF}
    property ParentBiDiMode;
    {$IFDEF DCC} property ParentCtl3D; {$ENDIF}
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    {$IFDEF DCC} property OnCanResize; {$ENDIF}
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
    property OnVisibleChanging;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnAfterDrawBackground;
    property OnPaint;
    {$IFDEF MSWINDOWS}property OnDragDropFiles;{$ENDIF}
    property Appearance;
    property TagExt;
    {$IFDEF DCC}{$IF RTLVersion > 23} property StyleElements; {$IFEND}{$ENDIF}
    {$IFDEF DELPHI2010_OR_ABOVE} property Touch; {$ENDIF}
    property DoubleBuffered;
    {$IFDEF DCC}property ParentDoubleBuffered;{$ENDIF}
    {$IFDEF FPC}{$IFDEF HAS_WINCONTROL_WITH_PARENTDOUBLEBUFFERED}
    property ParentDoubleBuffered;
    {$ENDIF}{$ENDIF}
    {$IFDEF FPC}
    property BorderSpacing;
    property ChildSizing;
    property OnGetDockCaption;
    {$ENDIF}
    property SelectedColor;
    property OnGetTopColorStrValue;
    property OnGetBottomColorStrValue;
    property OnSelectedColorChange;
    property OnSelectedColorChanged;

    property AnchoredControls;
  end;
  {$endregion}



  {$region ' ------------ TJppColorSwatchButton ------------ '}
  TJppColorSwatchButton = class(TJppBasicSpeedButton)
  private
    FVisible: Boolean;
    FOnVisibleChanged: TNotifyEvent;
    procedure SetVisible(const Value: Boolean);
    procedure SetOnVisibleChanged(const Value: TNotifyEvent);
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Visible: Boolean read FVisible write SetVisible;
    property OnVisibleChanged: TNotifyEvent read FOnVisibleChanged write SetOnVisibleChanged;
  end;
  {$endregion}

  {$region ' -------------- TJppCustomColorSwatchEx ---------------- '}
  TJppCustomColorSwatchEx = class(TJppCustomColorSwatch)
  private
    FBoundLabel: TJppControlBoundLabel;
    FBoundLabelPosition: TLabelPosition;
    FBoundLabelSpacing: Integer;
    FButtonCopyColor: TJppColorSwatchButton;
    FButtonChangeColor: TJppColorSwatchButton;
    FButtonsSpacing: integer;
    FButtonPasteColor: TJppColorSwatchButton;
    FButtonsAlignment: TVerticalAlignment;
    {$IFDEF DCC}FColorDialogOptions: TColorDialogOptions;{$ENDIF}
    procedure ButtonCopyColorClick(Sender: TObject);
    procedure ButtonPasteColorClick(Sender: TObject);
    procedure ButtonChangeColorClick(Sender: TObject);
    procedure SetBoundLabelPosition(const Value: TLabelPosition);
    procedure SetBoundLabelSpacing(const Value: Integer);
    procedure SetButtonCopyColorPosition;
    procedure SetButtonChangeColorPosition;
    procedure SetButtonPasteColorPosition;
    function GetButtonTopPosition(Button: TJppColorSwatchButton): integer;
    procedure SetButtonsSpacing(const Value: integer);
    procedure ButtonPositionChanged(Sender: TObject);
    procedure AdjustLabelBounds(Sender: TObject);
    procedure SetButtonsAlignment(const Value: TVerticalAlignment);
    {$IFDEF DCC}procedure SetColorDialogOptions(const Value: TColorDialogOptions);{$ENDIF}
  protected
    procedure SetParent(AParent: TWinControl); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMBiDiModeChanged(var Message: TMessage); message CM_BIDIMODECHANGED;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetupInternalLabel;
    procedure SetupButtonCopyColor;
    procedure SetupButtonChangeColor;
    procedure SetupButtonPasteColor;
    procedure SetButtonsPosition;
    procedure ShowColorDlg;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer; AHeight: Integer); override;

    procedure ShowLabel;
    procedure HideLabel;
    procedure ShowButtonChangeColor;
    procedure HideButtonChangeColor;
    procedure ShowButtonCopyColor;
    procedure HideButtonCopyColor;
    procedure ShowButtonPasteColor;
    procedure HideButtonPasteColor;
  protected
    property BoundLabel: TJppControlBoundLabel read FBoundLabel;
    property BoundLabelPosition: TLabelPosition read FBoundLabelPosition write SetBoundLabelPosition default lpLeft;
    property BoundLabelSpacing: Integer read FBoundLabelSpacing write SetBoundLabelSpacing default 4;

    property ButtonsSpacing: integer read FButtonsSpacing write SetButtonsSpacing;
    property ButtonChangeColor: TJppColorSwatchButton read FButtonChangeColor;
    property ButtonCopyColor: TJppColorSwatchButton read FButtonCopyColor;
    property ButtonPasteColor: TJppColorSwatchButton read FButtonPasteColor;
    property ButtonsAlignment: TVerticalAlignment read FButtonsAlignment write SetButtonsAlignment default taAlignTop;

    {$IFDEF DCC}property ColorDialogOptions: TColorDialogOptions read FColorDialogOptions write SetColorDialogOptions default [cdFullOpen, cdAnyColor];{$ENDIF}
  end;
  {$endregion TJppCustomColorSwatchEx}


  {$region ' ------------------------ TJppColorSwatchEx ------------------------------- '}
  TJppColorSwatchEx = class(TJppCustomColorSwatchEx)
  published
    property Align;
    property Anchors;
    property BiDiMode;
    property Constraints;
    {$IFDEF DCC} property Ctl3D; {$ENDIF}
    property UseDockManager default True;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FullRepaint;
    property Font;
    {$IFDEF DCC} property Locked; {$ENDIF}
    {$IFDEF DELPHI2009_OR_ABOVE} property Padding; {$ENDIF}
    property ParentBiDiMode;
    {$IFDEF DCC} property ParentCtl3D; {$ENDIF}
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    {$IFDEF DCC} property OnCanResize; {$ENDIF}
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
    property OnVisibleChanging;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnAfterDrawBackground;
    property OnPaint;
    {$IFDEF MSWINDOWS}property OnDragDropFiles;{$ENDIF}
    property Appearance;
    property TagExt;
    {$IFDEF DCC}{$IF RTLVersion > 23} property StyleElements; {$IFEND}{$ENDIF}
    {$IFDEF DELPHI2010_OR_ABOVE} property Touch; {$ENDIF}
    property DoubleBuffered;
    {$IFDEF DCC}property ParentDoubleBuffered;{$ENDIF}
    {$IFDEF FPC}{$IFDEF HAS_WINCONTROL_WITH_PARENTDOUBLEBUFFERED}
    property ParentDoubleBuffered;
    {$ENDIF}{$ENDIF}
    {$IFDEF FPC}
    property BorderSpacing;
    property ChildSizing;
    property OnGetDockCaption;
    {$ENDIF}
    property SelectedColor;
    property OnGetTopColorStrValue;
    property OnGetBottomColorStrValue;
    property OnSelectedColorChange;
    property OnSelectedColorChanged;
    // Ex
    property BoundLabel;
    property BoundLabelPosition;
    property BoundLabelSpacing;

    property ButtonsSpacing;
    property ButtonChangeColor;
    property ButtonCopyColor;
    property ButtonPasteColor;
    property ButtonsAlignment;
    {$IFDEF DCC}property ColorDialogOptions;{$ENDIF}
    property AnchoredControls;

  end;
  {$endregion}

implementation


{$region ' ---------------------------------------- TJppCustomBaseColorSwatch ----------------------------------------------------- '}
constructor TJppCustomBaseColorSwatch.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BevelOuter := bvNone;
  Width := 170;
  Height := 34;
end;

destructor TJppCustomBaseColorSwatch.Destroy;
begin
  inherited Destroy;
end;

procedure TJppCustomBaseColorSwatch.Loaded;
begin
  Inherited;
end;

procedure TJppCustomBaseColorSwatch.PropsChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TJppCustomBaseColorSwatch.AdjustClientRect(var Rect: TRect);
begin
  inherited AdjustClientRect(Rect);
end;


  {$region ' --------------------------- PAINT & DRAW ------------------------------------ '}
procedure TJppCustomBaseColorSwatch.Paint;
var
  Rect {, TextRect}: TRect;
begin
  Rect := GetClientRect;
  DrawBackground(Rect);

  if Assigned(FOnAfterDrawBackground) then FOnAfterDrawBackground(Self);

  if Assigned(FOnPaint) then FOnPaint(Self);
end;

procedure TJppCustomBaseColorSwatch.DrawBackground(ARect: TRect);
begin
  //
end;

  {$endregion}

  {$region ' ------------------- reszta ------------------------------- '}

procedure TJppCustomBaseColorSwatch.VisibleChanging;
begin
  inherited;
  if Assigned(FOnVisibleChanging) then FOnVisibleChanging(Self);
end;

{$IFDEF MSWINDOWS}
procedure TJppCustomBaseColorSwatch.CMDropFiles(var msg: TWMDropFiles);
begin
  if Assigned(FOnDragDropFiles) then FOnDragDropFiles(Self, msg);
end;
{$ENDIF}

procedure TJppCustomBaseColorSwatch.CMMouseEnter(var Msg: TMessage);
begin
  inherited;
  MouseEnter(Self);
end;

procedure TJppCustomBaseColorSwatch.CMMouseLeave(var Msg: TMessage);
begin
  inherited;
  MouseLeave(Self);
end;

procedure TJppCustomBaseColorSwatch.MouseEnter(Sender: TObject);
begin
  if Assigned(FOnMouseEnter) then FOnMouseEnter(Self);
end;

procedure TJppCustomBaseColorSwatch.MouseLeave(Sender: TObject);
begin
  if Assigned(FonMouseLeave) then FOnMouseLeave(Self);
end;

procedure TJppCustomBaseColorSwatch.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
end;

procedure TJppCustomBaseColorSwatch.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
end;

procedure TJppCustomBaseColorSwatch.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
end;
  {$endregion}


{$endregion TJppCustomBaseColorSwatch}


{$region ' -------------------------------------------------------- TJppCustomColorSwatch ------------------------------------------------------------ '}

constructor TJppCustomColorSwatch.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {$IFDEF DCC}ParentBackground := False;{$ENDIF}
  {$IFDEF FPC}{$IFDEF HAS_PANEL_WITH_PARENTBACKGROUND}
  ParentBackground := False;
  {$ENDIF}{$ENDIF}

  FTagExt := TJppTagExt.Create(Self);
  FAppearance := TJppColorSwatchAppearance.Create(Self);
  FAppearance.OnChange := PropsChanged;

  FSelectedColor := clSilver;

  FOnSelectedColorChange := nil;
  FOnSelectedColorChanged := nil;

  FAnchoredControls := TJppAnchoredControls.Create(Self);
end;

destructor TJppCustomColorSwatch.Destroy;
begin
  FTagExt.Free;
  FAppearance.Free;
  FAnchoredControls.Free;
  inherited Destroy;
end;

procedure TJppCustomColorSwatch.PropsChanged(Sender: TObject);
begin
  if csLoading in ComponentState then Exit;
  Invalidate;
end;

procedure TJppCustomColorSwatch.SetAnchoredControls(const Value: TJppAnchoredControls);
begin
  FAnchoredControls := Value;
end;

procedure TJppCustomColorSwatch.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  if not (csDestroying in ComponentState) then
    if Assigned(FAnchoredControls) then FAnchoredControls.UpdateAllControlsPos;
end;

procedure TJppCustomColorSwatch.Notification(AComponent: TComponent; Operation: TOperation);
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

procedure TJppCustomColorSwatch.SetAppearance(const Value: TJppColorSwatchAppearance);
begin
  FAppearance := Value;
  Invalidate;
end;

procedure TJppCustomColorSwatch.SetOnGetBottomColorStrValue(const Value: TOnGetColorStrValue);
begin
  FOnGetBottomColorStrValue := Value;
end;

procedure TJppCustomColorSwatch.SetOnGetTopColorStrValue(const Value: TOnGetColorStrValue);
begin
  FOnGetTopColorStrValue := Value;
end;

procedure TJppCustomColorSwatch.SetOnSelectedColorChange(const Value: TOnSelectedColorChange);
begin
  FOnSelectedColorChange := Value;
end;

procedure TJppCustomColorSwatch.SetOnSelectedColorChanged(const Value: TNotifyEvent);
begin
  FOnSelectedColorChanged := Value;
end;

procedure TJppCustomColorSwatch.SetSelectedColor(const Value: TColor);
var
  NewColor: TColor;
begin
  if FSelectedColor = Value then Exit;

  NewColor := Value;
  if Assigned(OnSelectedColorChange) then OnSelectedColorChange(FSelectedColor, NewColor);
  FSelectedColor := NewColor;

  PropsChanged(Self);
  if Assigned(FOnSelectedColorChanged) then FOnSelectedColorChanged(Self);
end;

procedure TJppCustomColorSwatch.SetTagExt(const Value: TJppTagExt);
begin
  FTagExt := Value;
end;

function TJppCustomColorSwatch.TopColorStr: string;
begin
  if FSelectedColor = clNone then Result := ''
  else Result := ColorToStrEx(FSelectedColor, FAppearance.TopColorValue.ColorType);
end;

function TJppCustomColorSwatch.BottomColorStr: string;
begin
  if FSelectedColor = clNone then Result := ''
  else Result := ColorToStrEx(FSelectedColor, FAppearance.BottomColorValue.ColorType);
end;

procedure TJppCustomColorSwatch.AdjustClientRect(var Rect: TRect);
begin
  inherited AdjustClientRect(Rect);
end;

procedure TJppCustomColorSwatch.Paint;
begin
  inherited;
end;

  {$region ' ---------------- TJppCustomColorSwatch.DrawBackground ---------------------- '}
procedure TJppCustomColorSwatch.DrawBackground(ARect: TRect);
var
  R, RTop, RBottom: TRect;
  bTopVisible, bBottomVisible, bColorRectVisible: Boolean;
begin
  Canvas.Brush.Style := bsClear;
  R := ARect;
  Canvas.Brush.Color := FAppearance.BackgroundColor;
  Canvas.FillRect(R);

  bTopVisible := FAppearance.TopColorValue.Visible;
  bBottomVisible := FAppearance.BottomColorValue.Visible;
  bColorRectVisible := FAppearance.ColorRect.Visible;

  if bTopVisible then
  begin
    RTop := R;
    if bColorRectVisible then RTop.Left := RTop.Left + FAppearance.ColorRect.Width;
    if bBottomVisible then RTop.Height := RTop.Height div 2;
    DrawColorValue(FAppearance.TopColorValue, RTop);
  end;

  if bBottomVisible then
  begin
    RBottom := R;
    if bColorRectVisible then RBottom.Left := RBottom.Left + FAppearance.ColorRect.Width;
    if bTopVisible then RBottom.Top := RBottom.Top + (RBottom.Height div 2);
    DrawColorValue(FAppearance.BottomColorValue, RBottom);
  end;

  DrawBorders(ARect);

  if FAppearance.ColorRect.Visible then DrawColorRect;
end;
  {$endregion DrawBackground}


  {$region ' ---------------- TJppCustomColorSwatch.DrawBorders ---------------- '}
procedure TJppCustomColorSwatch.DrawBorders(ARect: TRect);
begin
  if Appearance.BorderColor = clNone then Exit;
  Canvas.Brush.Style := bsClear;

  Canvas.Pen.Style := Appearance.BorderStyle;
  Canvas.Pen.Color := Appearance.BorderColor;
  if Appearance.DrawLeftBorder then DrawLeftBorder(Canvas, ARect, Canvas.Pen, False);
  if Appearance.DrawRightBorder then DrawRightBorder(Canvas, ARect, Canvas.Pen, False);
  if Appearance.DrawTopBorder then DrawTopBorder(Canvas, ARect, Canvas.Pen, False);
  if Appearance.DrawBottomBorder then DrawBottomBorder(Canvas, ARect, Canvas.Pen, False);
end;
  {$endregion DrawBorders}


  {$Region ' ---------------- TJppCustomColorSwatch.DrawColorRect ---------------- '}
procedure TJppCustomColorSwatch.DrawColorRect;
var
  R: TRect;
begin
  R := ClientRect;
  if FAppearance.TopColorValue.Visible or FAppearance.BottomColorValue.Visible then R.Width := FAppearance.ColorRect.Width;

  InflateRectWithMargins(R, FAppearance.ColorRect.Margins);

  with Canvas do
  begin
    Brush.Color := FSelectedColor;
    Pen.Color := Brush.Color;
    Brush.Style := bsSolid;
    Pen.Style := FAppearance.ColorRect.BorderStyle;

    if FAppearance.ColorRect.DrawBorder then
    begin
      if FAppearance.ColorRect.BorderColorMixed then Pen.Color := AvgColor(FSelectedColor, FAppearance.ColorRect.BorderColor)
      else Pen.Color := FAppearance.ColorRect.BorderColor;
    end;

    Rectangle(R);
  end;
end;
  {$endregion TJppCustomColorSwatch.DrawColorRect}


  {$Region ' ----------------- TJppCustomColorSwatch.DrawColorValue ------------------ '}
procedure TJppCustomColorSwatch.DrawColorValue(ColorValue: TJppColorSwatchColorValue; ARect: TRect);
var
  sColor, sPrefix, sSuffix, sText: string;
begin
  if FSelectedColor = clNone then sColor := ''
  else sColor := ColorToStrEx(FSelectedColor, ColorValue.ColorType);
  sPrefix := ColorValue.Prefix;
  sSuffix := ColorValue.Suffix;

  if (ColorValue = FAppearance.TopColorValue) and Assigned(FOnGetTopColorStrValue) then
    FOnGetTopColorStrValue(FSelectedColor, sColor, sPrefix, sSuffix)
  else if (ColorValue = FAppearance.BottomColorValue) and Assigned(FOnGetBottomColorStrValue) then
    FOnGetBottomColorStrValue(FSelectedColor, sColor, sPrefix, sSuffix);

  sText := sPrefix + sColor + sSuffix;

  with Canvas do
  begin

    Brush.Color := ColorValue.BackgroundColor;
    Pen.Style := psSolid;
    Pen.Color := Brush.Color;
    Rectangle(ARect);

    Brush.Style := bsClear;
    Font.Assign(ColorValue.Font);
    DrawCenteredText(Canvas, ARect, sText, 0, ColorValue.TextPosYDelta);
  end;

end;



{$endregion TJppCustomColorSwatch.DrawColorValue}



{$endregion TJppCustomColorSwatch}



{$region ' ------------------------------------- TJppColorSwatchAppearance ------------------------------------------- '}

constructor TJppColorSwatchAppearance.Create(AOwner: TComponent);
begin
  inherited Create;
  FOwner := AOwner;

  FBackgroundColor := clBtnFace;
  FBorderColor := clGray;
  FDrawTopBorder := True;
  FDrawBottomBorder := True;
  FDrawLeftBorder := True;
  FDrawRightBorder := True;
  FBorderStyle := psSolid;

  FTopColorValue := TJppColorSwatchColorValue.Create(AOwner);
  FTopColorValue.ColorType := ctRgb;
  FTopColorValue.BackgroundColor := RGB3(80);
  FTopColorValue.Font.Color := RGB3(244);
  FTopColorValue.Font.Name := 'Consolas';
  FTopColorValue.Font.Size := 9;
  FTopColorValue.OnChange := PropsChanged;

  FBottomColorValue := TJppColorSwatchColorValue.Create(AOwner);
  FBottomColorValue.ColorType := ctHtml;
  FBottomColorValue.BackgroundColor := RGB3(140);
  FBottomColorValue.Font.Color := RGB3(244);
  FBottomColorValue.Font.Name := 'Consolas';
  FBottomColorValue.Font.Size := 9;
  FBottomColorValue.OnChange := PropsChanged;

  FColorRect := TJppColorSwatchColorRect.Create(AOwner);
  FColorRect.OnChange := PropsChanged;
end;

destructor TJppColorSwatchAppearance.Destroy;
begin
  FTopColorValue.Free;
  FBottomColorValue.Free;
  FColorRect.Free;
  inherited;
end;

procedure TJppColorSwatchAppearance.Assign(Source: TJppColorSwatchAppearance);
begin
  FBackgroundColor := Source.BackgroundColor;
  FBorderColor := Source.BorderColor;
  FBorderStyle := Source.BorderStyle;

  FDrawTopBorder := Source.DrawTopBorder;
  FDrawBottomBorder := Source.DrawBottomBorder;
  FDrawLeftBorder := Source.DrawLeftBorder;
  FDrawRightBorder := Source.DrawRightBorder;

  FBottomColorValue.Assign(Source.BottomColorValue);
  FTopColorValue.Assign(Source.TopColorValue);
  FColorRect.Assign(Source.ColorRect);

  PropsChanged(Self);
end;

procedure TJppColorSwatchAppearance.SetBackgroundColor(const Value: TColor);
begin
  if FBackgroundColor = Value then Exit;
  FBackgroundColor := Value;
  PropsChanged(Self);
end;

procedure TJppColorSwatchAppearance.SetBorderColor(const Value: TColor);
begin
  if FBorderColor = Value then Exit;
  FBorderColor := Value;
  PropsChanged(Self);
end;

procedure TJppColorSwatchAppearance.SetBorderStyle(const Value: TPenStyle);
begin
  if FBorderStyle = Value then Exit;
  FBorderStyle := Value;
  PropsChanged(Self);
end;

procedure TJppColorSwatchAppearance.SetDrawBottomBorder(const Value: Boolean);
begin
  if FDrawBottomBorder = Value then Exit;
  FDrawBottomBorder := Value;
  PropsChanged(Self);
end;

procedure TJppColorSwatchAppearance.SetDrawLeftBorder(const Value: Boolean);
begin
  if FDrawLeftBorder = Value then Exit;
  FDrawLeftBorder := Value;
  PropsChanged(Self);
end;

procedure TJppColorSwatchAppearance.SetDrawRightBorder(const Value: Boolean);
begin
  if FDrawRightBorder = Value then Exit;
  FDrawRightBorder := Value;
  PropsChanged(Self);
end;

procedure TJppColorSwatchAppearance.SetDrawTopBorder(const Value: Boolean);
begin
  if FDrawTopBorder = Value then Exit;
  FDrawTopBorder := Value;
  PropsChanged(Self);
end;

procedure TJppColorSwatchAppearance.SetTopColorValue(const Value: TJppColorSwatchColorValue);
begin
  FTopColorValue := Value;
  PropsChanged(Self);
end;

procedure TJppColorSwatchAppearance.SetBottomColorValue(const Value: TJppColorSwatchColorValue);
begin
  FBottomColorValue := Value;
  PropsChanged(Self);
end;

procedure TJppColorSwatchAppearance.SetColorRect(const Value: TJppColorSwatchColorRect);
begin
  FColorRect := Value;
  PropsChanged(Self);
end;

{$endregion TJppColorSwatchAppearance}



{$region ' ---------------------- TJppColorSwatchColorValue ------------------------- '}

constructor TJppColorSwatchColorValue.Create(AOwner: TComponent);
begin
  inherited Create;
  FVisible := True;
  FColorType := ctRgb;
  FFont := TFont.Create;
  FFont.OnChange := PropsChanged;
  FPrefix := '';
  FTextPosYDelta := 0;
end;

destructor TJppColorSwatchColorValue.Destroy;
begin
  FFont.Free;
  inherited;
end;

procedure TJppColorSwatchColorValue.Assign(cscv: TJppColorSwatchColorValue);
begin
  FBackgroundColor := cscv.BackgroundColor;
  FColorType := cscv.ColorType;
  FFont.Assign(cscv.Font);
  FPrefix := cscv.Prefix;
  FSuffix := cscv.Suffix;
  FTextPosYDelta := cscv.TextPosYDelta;
  FVisible := cscv.Visible;
end;

procedure TJppColorSwatchColorValue.SetBackgroundColor(const Value: TColor);
begin
  if FBackgroundColor = Value then Exit;
  FBackgroundColor := Value;
  PropsChanged(Self);
end;

procedure TJppColorSwatchColorValue.SetColorType(const Value: TColorType);
begin
  if FColorType = Value then Exit;
  FColorType := Value;
  PropsChanged(Self);
end;

procedure TJppColorSwatchColorValue.SetFont(const Value: TFont);
begin
  FFont := Value;
  PropsChanged(Self);
end;

procedure TJppColorSwatchColorValue.SetPrefix(const Value: string);
begin
  if FPrefix = Value then Exit;
  FPrefix := Value;
  PropsChanged(Self);
end;

procedure TJppColorSwatchColorValue.SetSuffix(const Value: string);
begin
  if FSuffix = Value then Exit;
  FSuffix := Value;
  PropsChanged(Self);
end;

procedure TJppColorSwatchColorValue.SetTextPosYDelta(const Value: ShortInt);
begin
  if FTextPosYDelta = Value then Exit;
  FTextPosYDelta := Value;
  PropsChanged(Self);
end;

procedure TJppColorSwatchColorValue.SetVisible(const Value: Boolean);
begin
  if FVisible = Value then Exit;
  FVisible := Value;
  PropsChanged(Self);
end;

{$endregion TJppColorSwatchColorValue}



{$region ' --------------------- TJppColorSwatchColorRect -------------------------- '}

constructor TJppColorSwatchColorRect.Create(AOwner: TComponent);
begin
  inherited Create;
  FVisible := True;
  FWidth := 34;
  FMargins := TJppMargins.Create(AOwner);
  FMargins.OnChange := PropsChanged;
  //FMargins.SetMargins(0, 0, 0, 0);
  FBorderColor := clGray;
  FDrawBorder := True;
  FBorderColorMixed := True;
  FBorderStyle := psSolid;
end;

destructor TJppColorSwatchColorRect.Destroy;
begin
  FMargins.Free;
  inherited;
end;

procedure TJppColorSwatchColorRect.Assign(cscr: TJppColorSwatchColorRect);
begin
  FBorderColor := cscr.BorderColor;
  FBorderColorMixed := cscr.BorderColorMixed;
  FBorderStyle := cscr.BorderStyle;
  FDrawBorder := cscr.DrawBorder;
  FMargins.Assign(cscr.Margins);
  FVisible := cscr.Visible;
  FWidth := cscr.Width;
end;

procedure TJppColorSwatchColorRect.SetBorderColor(const Value: TColor);
begin
  if FBorderColor = Value then Exit;
  FBorderColor := Value;
  PropsChanged(Self);
end;

procedure TJppColorSwatchColorRect.SetBorderColorMixed(const Value: Boolean);
begin
  if FBorderColorMixed = Value then Exit;
  FBorderColorMixed := Value;
  PropsChanged(Self);
end;

procedure TJppColorSwatchColorRect.SetBorderStyle(const Value: TPenStyle);
begin
  if FBorderStyle = Value then Exit;
  FBorderStyle := Value;
  if FDrawBorder then PropsChanged(Self);
end;

procedure TJppColorSwatchColorRect.SetDrawBorder(const Value: Boolean);
begin
  if FDrawBorder = Value then Exit;
  FDrawBorder := Value;
  PropsChanged(Self);
end;

procedure TJppColorSwatchColorRect.SetMargins(const Value: TJppMargins);
begin
  FMargins := Value;
  //PropsChanged(Self);
end;

procedure TJppColorSwatchColorRect.SetVisible(const Value: Boolean);
begin
  if FVisible = Value then Exit;
  FVisible := Value;
  PropsChanged(Self);
end;

procedure TJppColorSwatchColorRect.SetWidth(const Value: integer);
begin
  if FWidth = Value then Exit;
  FWidth := Value;
  PropsChanged(Self);
end;

{$endregion TJppColorSwatchColorRect}



{$Region ' ----------------------------- TJppCustomColorSwatchEx -------------------------------- '}

constructor TJppCustomColorSwatchEx.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Parent := TWinControl(AOwner);

  FBoundLabelPosition := lpLeft;
  FBoundLabelSpacing := 4;
  SetupInternalLabel;

  FButtonsSpacing := 2;
  SetupButtonChangeColor;
  SetupButtonCopyColor;
  SetupButtonPasteColor;

  FButtonsAlignment := taAlignTop;

  {$IFDEF DCC}FColorDialogOptions := [cdFullOpen, cdAnyColor];{$ENDIF}
end;

destructor TJppCustomColorSwatchEx.Destroy;
begin
  inherited;
end;

procedure TJppCustomColorSwatchEx.SetParent(AParent: TWinControl);
begin
  inherited SetParent(AParent);

  if FBoundLabel <> nil then
  begin
    FBoundLabel.Parent := AParent;
    FBoundLabel.Visible := True;
  end;

  if FButtonChangeColor <> nil then
  begin
    FButtonChangeColor.Parent := AParent;
    FButtonChangeColor.Visible := True;
  end;

  if FButtonCopyColor <> nil then
  begin
    FButtonCopyColor.Parent := AParent;
    FButtonCopyColor.Visible := True;
  end;

  if FButtonPasteColor <> nil then
  begin
    FButtonPasteColor.Parent := AParent;
    FButtonPasteColor.Visible := True;
  end;
end;

  {$Region ' --------------- Color Dialog -------------- '}

procedure TJppCustomColorSwatchEx.ShowColorDlg;
var
  dlgColor: TColorDialog;
begin
  dlgColor := TColorDialog.Create(Self);
  try
    {$IFDEF DCC}dlgColor.Options := FColorDialogOptions;{$ENDIF} // dlgColor.Options + [cdFullOpen, cdAnyColor];
    dlgColor.Color := FSelectedColor;
    if not dlgColor.Execute then
    begin
      SetSelectedColor(FSelectedColor);
      Exit;
    end;
    SetSelectedColor(dlgColor.Color);
  finally
    dlgColor.Free;
  end;
end;


{$IFDEF DCC}
procedure TJppCustomColorSwatchEx.SetColorDialogOptions(const Value: TColorDialogOptions);
begin
  FColorDialogOptions := Value;
end;
{$ENDIF}
  {$endregion Color Dialog}


  {$region ' --------- internal controls ------------- '}

procedure TJppCustomColorSwatchEx.SetupButtonChangeColor;
begin
  if Assigned(FButtonChangeColor) then Exit;
  FButtonChangeColor := TJppColorSwatchButton.Create(Self);
  FButtonChangeColor.Name := 'BtnChangeColor';
  FButtonChangeColor.FreeNotification(Self);
  FButtonChangeColor.OnClick := ButtonChangeColorClick;
  FButtonChangeColor.AutoWidth := False;
  FButtonChangeColor.Height := 25; //Height;
  FButtonChangeColor.Width := FButtonChangeColor.Height;
  FButtonChangeColor.Caption := '...';
  FButtonChangeColor.Appearance.Normal.BorderColor := GetSimilarColor(clBtnFace, 15, False);
  FButtonChangeColor.Hint := 'Change color...';
  FButtonChangeColor.OnVisibleChanged := ButtonPositionChanged;
end;

procedure TJppCustomColorSwatchEx.SetupButtonCopyColor;
begin
  if Assigned(FButtonCopyColor) then Exit;
  FButtonCopyColor := TJppColorSwatchButton.Create(Self);
  FButtonCopyColor.Name := 'BtnCopyColor';
  FButtonCopyColor.FreeNotification(Self);
  FButtonCopyColor.OnClick := ButtonCopyColorClick;
  FButtonCopyColor.AutoWidth := False;
  FButtonCopyColor.Height := 25; //Height;
  FButtonCopyColor.Width := FButtonCopyColor.Height;
  FButtonCopyColor.Caption := 'C';
  FButtonCopyColor.Appearance.Normal.BorderColor := GetSimilarColor(clBtnFace, 15, False);
  FButtonCopyColor.Hint := 'Copy color';
  FButtonCopyColor.OnVisibleChanged := ButtonPositionChanged;
end;

procedure TJppCustomColorSwatchEx.SetupButtonPasteColor;
begin
  if Assigned(FButtonPasteColor) then Exit;
  FButtonPasteColor := TJppColorSwatchButton.Create(Self);
  FButtonPasteColor.Name := 'BtnPasteColor';
  FButtonPasteColor.FreeNotification(Self);
  FButtonPasteColor.OnClick := ButtonPasteColorClick;
  FButtonPasteColor.AutoWidth := False;
  FButtonPasteColor.Height := 25; // Height;
  FButtonPasteColor.Width := FButtonPasteColor.Height;
  FButtonPasteColor.Caption := 'P';
  FButtonPasteColor.Appearance.Normal.BorderColor := GetSimilarColor(clBtnFace, 15, False);
  FButtonPasteColor.Hint := 'Paste color';
  FButtonPasteColor.OnVisibleChanged := ButtonPositionChanged;
end;

function TJppCustomColorSwatchEx.GetButtonTopPosition(Button: TJppColorSwatchButton): integer;
begin
  case FButtonsAlignment of
    taVerticalCenter: Result := Top + (Height div 2) - (Button.Height div 2);
    taAlignBottom: Result := (Top + Height) - Button.Height;
  else
    // taAlignTop
    Result := Top;
  end;
end;

procedure TJppCustomColorSwatchEx.SetButtonChangeColorPosition;
begin
  if not Assigned(FButtonChangeColor) then Exit;
  FButtonChangeColor.Left := Left + Width + FButtonsSpacing;
  //FButtonChangeColor.Top := Top;
  FButtonChangeColor.Top := GetButtonTopPosition(FButtonChangeColor);
end;

procedure TJppCustomColorSwatchEx.SetButtonCopyColorPosition;
var
  x: integer;
begin
  if not Assigned(FButtonCopyColor) then Exit;
  if (Assigned(FButtonChangeColor)) and (FButtonChangeColor.Visible) then x := FButtonChangeColor.Left + FButtonChangeColor.Width
  else x := Left + Width;
  x := x + FButtonsSpacing;
  FButtonCopyColor.Left := x;
  //FButtonCopyColor.Top := Top;
  FButtonCopyColor.Top := GetButtonTopPosition(FButtonCopyColor);
end;

procedure TJppCustomColorSwatchEx.SetButtonPasteColorPosition;
var
  x: integer;
begin
  if not Assigned(FButtonPasteColor) then Exit;
  if (Assigned(FButtonCopyColor)) and (FButtonCopyColor.Visible) then x := FButtonCopyColor.Left + FButtonCopyColor.Width
  else if (Assigned(FButtonChangeColor)) and (FButtonChangeColor.Visible) then x := FButtonChangeColor.Left + FButtonChangeColor.Width
  else x := Left + Width;
  x := x + FButtonsSpacing;
  FButtonPasteColor.Left := x;
  //FButtonPasteColor.Top := Top;
  FButtonPasteColor.Top := GetButtonTopPosition(FButtonPasteColor);
end;

procedure TJppCustomColorSwatchEx.SetButtonsAlignment(const Value: TVerticalAlignment);
begin
  if FButtonsAlignment = Value then Exit;
  FButtonsAlignment := Value;
  SetButtonsPosition;
end;

procedure TJppCustomColorSwatchEx.SetButtonsPosition;
begin
  SetButtonChangeColorPosition;
  SetButtonCopyColorPosition;
  SetButtonPasteColorPosition;
end;

procedure TJppCustomColorSwatchEx.SetButtonsSpacing(const Value: integer);
begin
  FButtonsSpacing := Value;
  SetButtonsPosition;
end;

procedure TJppCustomColorSwatchEx.ButtonChangeColorClick(Sender: TObject);
begin
  ShowColorDlg;
  //if Assigned(OnChange) then OnChange(Self);
end;

procedure TJppCustomColorSwatchEx.ButtonCopyColorClick(Sender: TObject);
begin
  //Clipboard.AsText := IntToStr(ColorToRGB(SelectedColor));
  Clipboard.AsText := ColorToDelphiHex(SelectedColor);
end;

procedure TJppCustomColorSwatchEx.ButtonPasteColorClick(Sender: TObject);
var
  //x: integer;
  cl: TColor;
  s: string;
begin
  s := Clipboard.AsText;
  if TryDelphiIntStrToColor(s, cl) then SelectedColor := cl
  else if JPL.Colors.TryGetColor(s, cl) then SelectedColor := cl;
//  if TryStrToInt(Clipboard.AsText, x) then
//  begin
//    SelectedColor := TColor(x);
//    //if Assigned(OnChange) then OnChange(Self);
//  end;
end;

procedure TJppCustomColorSwatchEx.ButtonPositionChanged(Sender: TObject);
begin
  SetButtonsPosition;
end;

procedure TJppCustomColorSwatchEx.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  SetBoundLabelPosition(FBoundLabelPosition);
  SetButtonsPosition;
end;

procedure TJppCustomColorSwatchEx.SetupInternalLabel;
begin
  if Assigned(FBoundLabel) then Exit;
  FBoundLabel := TJppControlBoundLabel.Create(Self);
  FBoundLabel.FreeNotification(Self);
  FBoundLabel.OnAdjustBounds := AdjustLabelBounds;
  FBoundLabel.FocusControl := Self;
  {$IFDEF FPC}FBoundLabel.OnChangeBounds := AdjustLabelBounds;{$ENDIF}
end;

procedure TJppCustomColorSwatchEx.AdjustLabelBounds(Sender: TObject);
begin
  SetBoundLabelPosition(FBoundLabelPosition);
end;

procedure TJppCustomColorSwatchEx.SetBoundLabelPosition(const Value: TLabelPosition);
var
  P: TPoint;
begin
  if FBoundLabel = nil then Exit;
  FBoundLabelPosition := Value;
  case Value of
    lpAbove: P := Point(Left, Top - FBoundLabel.Height - FBoundLabelSpacing);
    lpBelow: P := Point(Left, Top + Height + FBoundLabelSpacing);
    lpLeft : P := Point(Left - FBoundLabel.Width - FBoundLabelSpacing, Top + ((Height - FBoundLabel.Height) div 2));
    lpRight: P := Point(Left + Width + FBoundLabelSpacing, Top + ((Height - FBoundLabel.Height) div 2));
  end;

  FBoundLabel.SetBounds({%H-}P.x, {%H-}P.y, FBoundLabel.Width, FBoundLabel.Height);
end;

procedure TJppCustomColorSwatchEx.SetBoundLabelSpacing(const Value: Integer);
begin
  FBoundLabelSpacing := Value;
  SetBoundLabelPosition(FBoundLabelPosition);
end;

procedure TJppCustomColorSwatchEx.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = FBoundLabel then FBoundLabel := nil
    else if AComponent = FButtonChangeColor then FButtonChangeColor := nil
    else if AComponent = FButtonCopyColor then FButtonCopyColor := nil
    else if AComponent = FButtonPasteColor then FButtonPasteColor := nil;
  end;
end;

procedure TJppCustomColorSwatchEx.CMVisibleChanged(var Message: TMessage);
begin
  inherited;
  if FBoundLabel <> nil then FBoundLabel.Visible := Visible;
  if FButtonChangeColor <> nil then FButtonChangeColor.Visible := Visible;
  if FButtonCopyColor <> nil then FButtonCopyColor.Visible := Visible;
  if FButtonPasteColor <> nil then FButtonPasteColor.Visible := Visible;
end;

procedure TJppCustomColorSwatchEx.CMBiDiModeChanged(var Message: TMessage);
begin
  inherited;
  if FBoundLabel <> nil then FBoundLabel.BiDiMode := BiDiMode;
  if FButtonChangeColor <> nil then FButtonChangeColor.BiDiMode := BiDiMode;
  if FButtonCopyColor <> nil then FButtonCopyColor.BiDiMode := BiDiMode;
  if FButtonPasteColor <> nil then FButtonPasteColor.BiDiMode := BiDiMode;
end;

procedure TJppCustomColorSwatchEx.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if FBoundLabel <> nil then FBoundLabel.Enabled := Enabled;
  if FButtonChangeColor <> nil then FButtonChangeColor.Enabled := Enabled;
  if FButtonCopyColor <> nil then FButtonCopyColor.Enabled := Enabled;
  if FButtonPasteColor <> nil then FButtonPasteColor.Enabled := Enabled;
end;

procedure TJppCustomColorSwatchEx.ShowLabel;
begin
  if not Assigned(FBoundLabel) then Exit;
  FBoundLabel.Visible := True;
end;

procedure TJppCustomColorSwatchEx.HideLabel;
begin
  if not Assigned(FBoundLabel) then Exit;
  FBoundLabel.Visible := False;
end;

procedure TJppCustomColorSwatchEx.ShowButtonChangeColor;
begin
  if not Assigned(FButtonChangeColor) then Exit;
  FButtonChangeColor.Visible := True;
end;

procedure TJppCustomColorSwatchEx.HideButtonChangeColor;
begin
  if not Assigned(FButtonChangeColor) then Exit;
  FButtonChangeColor.Visible := False;
end;

procedure TJppCustomColorSwatchEx.ShowButtonCopyColor;
begin
  if not Assigned(FButtonCopyColor) then Exit;
  FButtonCopyColor.Visible := True;
end;

procedure TJppCustomColorSwatchEx.HideButtonCopyColor;
begin
  if not Assigned(FButtonCopyColor) then Exit;
  FButtonCopyColor.Visible := False;
end;

procedure TJppCustomColorSwatchEx.ShowButtonPasteColor;
begin
  if not Assigned(FButtonPasteColor) then Exit;
  FButtonPasteColor.Visible := True;
end;

procedure TJppCustomColorSwatchEx.HideButtonPasteColor;
begin
  if not Assigned(FButtonPasteColor) then Exit;
  FButtonPasteColor.Visible := False;
end;


  {$endregion internal controls}

{$endregion TJppCustomColorSwatchEx}


{$region ' ------------------------ TJppColorSwatchButton ----------------------------- '}
constructor TJppColorSwatchButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Name := 'ColorSwatchButton' + IntToStr(Random(10000)) + IntToStr(Random(10000));
  SetSubComponent(True);
end;


procedure TJppColorSwatchButton.SetOnVisibleChanged(const Value: TNotifyEvent);
begin
  FOnVisibleChanged := Value;
end;

procedure TJppColorSwatchButton.SetVisible(const Value: Boolean);
begin
  inherited {$IFDEF FPC}SetVisible(Value){$ENDIF};
  FVisible := Value;
  if Value then Show else Hide;
  if Assigned(OnVisibleChanged) then OnVisibleChanged(Self);
end;

{$endregion TJppColorSwatchButton}



end.
