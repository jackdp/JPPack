unit JPP.BasicPanel;

{
  Jacek Pazera
  http://www.pazera-software.com
  https://github.com/jackdp
}

{$I jpp.inc}
{$IFDEF FPC} {$mode objfpc}{$H+} {$ENDIF}

interface

uses
  {$IFDEF MSWINDOWS}Windows,{$ENDIF}
  Messages, SysUtils, Classes, Types, {$IFDEF DCC}{$IFDEF HAS_SYSTEM_UITYPES}System.UITypes,{$ENDIF}{$ENDIF}
  Controls, Forms, Menus, Graphics, StdCtrls, GraphUtil, Themes, ExtCtrls, Dialogs,
  {$IFDEF FPC}LMessages, LCLType, LCLIntf,{$ENDIF}
  JPL.Colors, JPL.Rects,
  JPP.Types, JPP.Graphics, JPP.Common, JPP.Common.Procs, JPP.AnchoredControls;


type


  {$region ' ------------ TJppBasicPanelBorder -------------- '}
  TJppBasicPanelBorder = class(TPersistent)
  private
    FPen: TPen;
    FVisible: Boolean;
    FOnChange: TNotifyEvent;
    FBorder3D: Boolean;
    procedure SetPen(const Value: TPen);
    procedure SetVisible(const Value: Boolean);
    procedure SetOnChange(const Value: TNotifyEvent);
    procedure SetBorder3D(const Value: Boolean);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(Border: TJppBasicPanelBorder); reintroduce;
  protected
    procedure PropsChanged(Sender: TObject);
  published
    property Pen: TPen read FPen write SetPen;
    property Visible: Boolean read FVisible write SetVisible default True;
    property Border3D: Boolean read FBorder3D write SetBorder3D default True;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
  end;
  {$endregion}


  {$region ' ------------ TJppBasicPanelBorders -------------- '}
  TJppBasicPanelBorders = class(TPersistent)
  private
    FLeft: TJppBasicPanelBorder;
    FRight: TJppBasicPanelBorder;
    FTop: TJppBasicPanelBorder;
    FBottom: TJppBasicPanelBorder;
    FOnChange: TNotifyEvent;
    procedure SetLeft(const Value: TJppBasicPanelBorder);
    procedure SetRight(const Value: TJppBasicPanelBorder);
    procedure SetTop(const Value: TJppBasicPanelBorder);
    procedure SetBottom(const Value: TJppBasicPanelBorder);
    procedure SetOnChange(const Value: TNotifyEvent);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(Borders: TJppBasicPanelBorders); reintroduce;
  protected
    procedure PropsChanged(Sender: TObject);
  published
    property Left: TJppBasicPanelBorder read FLeft write SetLeft;
    property Right: TJppBasicPanelBorder read FRight write SetRight;
    property Top: TJppBasicPanelBorder read FTop write SetTop;
    property Bottom: TJppBasicPanelBorder read FBottom write SetBottom;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
  end;
  {$endregion}


  {$region ' ----------- TJppBasicPanelAppearance ---------- '}
  TJppBasicPanelAppearance = class(TPersistent)
  private
    FOwner: TComponent;
    FUpperGradient: TJppGradientEx;
    FBottomGradient: TJppGradientEx;
    FUpperGradientPercent: Byte;
    FBorders: TJppBasicPanelBorders;
    FOnChange: TNotifyEvent;
    FBorderToGradientMargin: integer;
    FBackgroundColor: TColor;
    FDrawGradient: Boolean;
    FDrawBorder: Boolean;
    procedure SetUpperGradient(const Value: TJppGradientEx);
    procedure SetBottomGradient(const Value: TJppGradientEx);
    procedure SetUpperGradientPercent(const Value: Byte);
    procedure SetBorders(const Value: TJppBasicPanelBorders);
    procedure SetOnChange(const Value: TNotifyEvent);
    procedure SetBorderToGradientMargin(const Value: integer);
    procedure SetBackgroundColor(const Value: TColor);
    procedure SetDrawGradient(const Value: Boolean);
    procedure SetDrawBorder(const Value: Boolean);
  protected
    procedure PropsChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(Source: TJppBasicPanelAppearance); reintroduce;
  published
    property UpperGradient: TJppGradientEx read FUpperGradient write SetUpperGradient;
    property BottomGradient: TJppGradientEx read FBottomGradient write SetBottomGradient;
    property UpperGradientPercent: Byte read FUpperGradientPercent write SetUpperGradientPercent default 50;
    property Borders: TJppBasicPanelBorders read FBorders write SetBorders;
    property BorderToGradientMargin: integer read FBorderToGradientMargin write SetBorderToGradientMargin default 0;
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor default clBtnFace;
    property DrawGradient: Boolean read FDrawGradient write SetDrawGradient default True;
    property DrawBorder: Boolean read FDrawBorder write SetDrawBorder default True;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
  end;
  {$endregion}


  {$region ' --------------------------- TJppCustomBasePanel ---------------------------------- '}
  {$IFDEF MSWINDOWS}TProcOnOnDragDropFiles = procedure (Sender: TObject; var msg: TWMDropFiles) of object;{$ENDIF}

  TJppCustomBasePanel = class(TCustomPanel)
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnPaint: TNotifyEvent;
    FOnVisibleChanging: TNotifyEvent;
    FLayout: TTextLayout;
    FWordWrap: Boolean;
    FOnAfterDrawBackground: TNotifyEvent;
    {$IFDEF MSWINDOWS}FOnDragDropFiles: TProcOnOnDragDropFiles;{$ENDIF}
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    {$IFDEF MSWINDOWS}procedure CMDropFiles(var msg: TWMDropFiles); message WM_DROPFILES;{$ENDIF}
    procedure PropsChanged(Sender: TObject);
    procedure SetLayout(const Value: TTextLayout);
    procedure SetWordWrap(const Value: Boolean);
  protected
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure AdjustClientRect(var Rect: TRect); override;
    procedure Paint; override;
    procedure Loaded; override;
    procedure VisibleChanging; override;
    procedure DrawBackground(ARect: TRect); virtual;
    procedure DrawCaption(ARect: TRect); virtual;
    procedure MouseEnter(Sender: TObject); virtual;
    procedure MouseLeave(Sender: TObject); virtual;
    property Layout: TTextLayout read FLayout write SetLayout default tlCenter;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default False;
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


  {$region ' --------------------------------- TJppCustomBasicPanel ---------------------------------------- '}
  TJppCustomBasicPanel = class(TJppCustomBasePanel)
  private
    FOnGradientChange: TNotifyEvent;
    FAppearance: TJppBasicPanelAppearance;
    FTagExt: TJppTagExt;
    FAnchoredControls: TJppAnchoredControls;
    procedure SetOnGradientChange(const Value: TNotifyEvent);
    procedure SetAppearance(const Value: TJppBasicPanelAppearance);
    procedure SetTagExt(const Value: TJppTagExt);
    procedure SetAnchoredControls(const Value: TJppAnchoredControls);
  protected
    procedure DrawBackground(ARect: TRect); override;
    procedure DrawBorders(ARect: TRect);
    procedure PropsChanged(Sender: TObject);
    procedure GradientChanged(Sender: TObject);
    property OnGradientChange: TNotifyEvent read FOnGradientChange write SetOnGradientChange;
    property Appearance: TJppBasicPanelAppearance read FAppearance write SetAppearance;
    procedure Paint; override;
    procedure AdjustClientRect(var Rect: TRect); override;
    procedure GetHDeltas(var dxLeft, dxRight: integer);
    procedure GetVDeltas(var dxTop, dxBottom: integer);
    procedure GetDeltas(var dxLeft, dxRight, dxTop, dxBottom: integer);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;

    property Canvas;
    property DockManager;
    property TagExt: TJppTagExt read FTagExt write SetTagExt;
    property AnchoredControls: TJppAnchoredControls read FAnchoredControls write SetAnchoredControls;
  published
  end;
  {$endregion}


  {$region ' ------------------------ TJppBasicPanel ------------------------------- '}
  TJppBasicPanel = class(TJppCustomBasicPanel)
//{$IF RTLVersion >= 24.0 }
//  strict private
//    class constructor Create;
//    class destructor Destroy;
//{$IFEND}
  published
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    //property BevelInner;
    //property BevelOuter;
    //property BevelWidth;
    {$IFDEF DCC} property BevelKind; {$ENDIF}
    property BiDiMode;
    property BorderWidth;
    property BorderStyle;
//    property Caption;
//    property Color;
    property Constraints;
    {$IFDEF DCC} property Ctl3D; {$ENDIF}
    property UseDockManager default True;
    property DockSite;
    //{$IFDEF DELPHI2009_OR_ABOVE} property DoubleBuffered; {$ENDIF}
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FullRepaint;
    property Font;
    {$IFDEF DCC} property Locked; {$ENDIF}
    {$IFDEF DELPHI2009_OR_ABOVE} property Padding; {$ENDIF}
    property ParentBiDiMode;
    {$IFDEF DCC}property ParentBackground default false;{$ENDIF}
    {$IFDEF FPC}{$IFDEF HAS_PANEL_WITH_PARENTBACKGROUND}property ParentBackground default false;{$ENDIF}{$ENDIF}
//    property ParentColor;
    {$IFDEF DCC} property ParentCtl3D; {$ENDIF}
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    {$IFDEF DELPHI2009_OR_ABOVE} property ShowCaption; {$ENDIF}
    property ShowHint;
    property TabOrder;
    property TabStop;
    //property VerticalAlignment;  -> replaced by Layout property ...
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
    property Layout;
//    property WordWrap;
    property OnVisibleChanging;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnAfterDrawBackground;
    property OnPaint;
    {$IFDEF MSWINDOWS}property OnDragDropFiles;{$ENDIF}
    property Appearance;
    property TagExt;
    {$IFDEF HAS_STYLE_ELEMENTS} property StyleElements; {$ENDIF}
    {$IFDEF DELPHI2010_OR_ABOVE} property Touch; {$ENDIF}
    property DoubleBuffered;
    {$IFDEF DCC}property ParentDoubleBuffered;{$ENDIF}
    {$IFDEF FPC}
    {$IFDEF HAS_WINCONTROL_WITH_PARENTDOUBLEBUFFERED}property ParentDoubleBuffered;{$ENDIF}
    property BorderSpacing;
    //property BevelColor;
    property ChildSizing;
    property OnGetDockCaption;
    {$ENDIF}
    property AnchoredControls;
  end;
  {$endregion}


{$IFDEF DCC}
{$IF RTLVersion >= 24.0 }
  TJppBasicPanelStyleHook = class(TStyleHook)
  strict protected
    //procedure DrawPanel(ACanvas: TCanvas; AMouseInControl: Boolean); override;
  end;
{$IFEND}
{$ENDIF}



implementation




{$region ' ---------------------------------------- TJppCustomBasePanel ----------------------------------------------------- '}
constructor TJppCustomBasePanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //BevelInner := bvNone;
  BevelOuter := bvNone;
  FLayout := tlCenter;
  FWordWrap := False;
end;

destructor TJppCustomBasePanel.Destroy;
begin
  inherited Destroy;
end;

procedure TJppCustomBasePanel.Loaded;
begin
  Inherited;
end;

procedure TJppCustomBasePanel.PropsChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TJppCustomBasePanel.AdjustClientRect(var Rect: TRect);
begin
  inherited AdjustClientRect(Rect);
end;


  {$region ' --------------------------- PAINT & DRAW ------------------------------------ '}
procedure TJppCustomBasePanel.Paint;
var
  Rect, TextRect: TRect;
begin
  //inherited;
  Rect := GetClientRect;
  DrawBackground(Rect);

  if Assigned(FOnAfterDrawBackground) then FOnAfterDrawBackground(Self);

  //InflateRect(Rect, -BorderWidth, -BorderWidth);

  if (Caption <> '') {$IFDEF DELPHI2009_OR_ABOVE} and (ShowCaption) {$ENDIF}
  then begin
    TextRect := Rect;
    //InflateRect(TextRect, -FBevels.BevelsWidth, -FBevels.BevelsWidth);
    DrawCaption(TextRect);
  end;

  if Assigned(FOnPaint) then FOnPaint(Self);
end;

procedure TJppCustomBasePanel.DrawBackground(ARect: TRect);
begin
  {$IFDEF HAS_VCL_STYLES}
  if (not StyleServices.Enabled) or not ParentBackground
  then
  begin
    //Canvas.Brush.Color := Color;
    //Canvas.FillRect(ARect);
  end;
  {$ENDIF}
end;

procedure TJppCustomBasePanel.DrawCaption(aRect: TRect);
begin
//
end;

  {$endregion}

  {$region ' ------------------- reszta ------------------------------- '}

procedure TJppCustomBasePanel.SetLayout(const Value: TTextLayout);
begin
  FLayout := Value;
  Invalidate;
end;


procedure TJppCustomBasePanel.SetWordWrap(const Value: Boolean);
begin
  FWordWrap := Value;
  Invalidate;
end;

procedure TJppCustomBasePanel.VisibleChanging;
begin
  inherited;
  if Assigned(FOnVisibleChanging) then FOnVisibleChanging(Self);
end;

{$IFDEF MSWINDOWS}
procedure TJppCustomBasePanel.CMDropFiles(var msg: TWMDropFiles);
begin
  if Assigned(FOnDragDropFiles) then FOnDragDropFiles(Self, msg);
end;
{$ENDIF}

procedure TJppCustomBasePanel.CMMouseEnter(var Msg: TMessage);
begin
  inherited;
  MouseEnter(Self);
end;

procedure TJppCustomBasePanel.CMMouseLeave(var Msg: TMessage);
begin
  inherited;
  MouseLeave(Self);
end;

procedure TJppCustomBasePanel.MouseEnter(Sender: TObject);
begin
  if Assigned(FOnMouseEnter) then FOnMouseEnter(Self);
end;

procedure TJppCustomBasePanel.MouseLeave(Sender: TObject);
begin
  if Assigned(FonMouseLeave) then FOnMouseLeave(Self);
end;

procedure TJppCustomBasePanel.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
end;

procedure TJppCustomBasePanel.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
end;

procedure TJppCustomBasePanel.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
end;
  {$endregion}


{$endregion TJppCustomBasePanel}

{$region ' -------------------------------------------------------- TJppCustomBasicPanel ------------------------------------------------------------ '}
constructor TJppCustomBasicPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {$IFDEF DCC}ParentBackground := False;{$ENDIF}  // TJvGifAnimator needs this!
  {$IFDEF FPC}{$IFDEF HAS_PANEL_WITH_PARENTBACKGROUND}ParentBackground := False;{$ENDIF}{$ENDIF}

  FTagExt := TJppTagExt.Create(Self);
  FAppearance := TJppBasicPanelAppearance.Create(Self);
  FAppearance.OnChange := {$IFDEF FPC} @ {$ENDIF}PropsChanged;

  FAppearance.UpperGradient.OnChange := {$IFDEF FPC} @ {$ENDIF}GradientChanged;
  FAppearance.BottomGradient.OnChange := {$IFDEF FPC} @ {$ENDIF}GradientChanged;

  FAnchoredControls := TJppAnchoredControls.Create(Self);
end;

destructor TJppCustomBasicPanel.Destroy;
begin
  FTagExt.Free;
  FAppearance.Free;
  FAnchoredControls.Free;
  inherited Destroy;
end;

procedure TJppCustomBasicPanel.PropsChanged(Sender: TObject);
begin
  if csLoading in ComponentState then Exit;
  Invalidate;
end;

procedure TJppCustomBasicPanel.GetDeltas(var dxLeft, dxRight, dxTop, dxBottom: integer);
begin
  GetHDeltas(dxLeft, dxRight);
  GetVDeltas(dxTop, dxBottom);
end;

procedure TJppCustomBasicPanel.GetHDeltas(var dxLeft, dxRight: integer);
begin
  if Appearance.Borders.Left.Visible and (Appearance.Borders.Left.Pen.Width > 0) then dxLeft := Appearance.Borders.Left.Pen.Width else dxLeft := 0;
  if Appearance.Borders.Right.Visible and (Appearance.Borders.Right.Pen.Width > 0) then dxRight := Appearance.Borders.Right.Pen.Width else dxRight := 0;
end;

procedure TJppCustomBasicPanel.GetVDeltas(var dxTop, dxBottom: integer);
begin
  if Appearance.Borders.Top.Visible and (Appearance.Borders.Top.Pen.Width > 0) then dxTop := Appearance.Borders.Top.Pen.Width else dxTop := 0;
  if Appearance.Borders.Bottom.Visible and (Appearance.Borders.Bottom.Pen.Width > 0) then dxBottom := Appearance.Borders.Bottom.Pen.Width else dxBottom := 0;
end;

procedure TJppCustomBasicPanel.GradientChanged(Sender: TObject);
begin
  {$IFDEF DCC}if (csDesigning in ComponentState) and (not (csLoading in ComponentState)) and ParentBackground then ParentBackground := False;{$ENDIF}
  {$IFDEF FPC}{$IFDEF HAS_PANEL_WITH_PARENTBACKGROUND}
  if (csDesigning in ComponentState) and (not (csLoading in ComponentState)) and ParentBackground then ParentBackground := False;
  {$ENDIF}{$ENDIF}
  Invalidate;
end;

procedure TJppCustomBasicPanel.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  if not (csDestroying in ComponentState) then
    if Assigned(FAnchoredControls) then FAnchoredControls.UpdateAllControlsPos;
end;

procedure TJppCustomBasicPanel.Notification(AComponent: TComponent; Operation: TOperation);
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

procedure TJppCustomBasicPanel.SetAnchoredControls(const Value: TJppAnchoredControls);
begin
  FAnchoredControls := Value;
end;

procedure TJppCustomBasicPanel.SetAppearance(const Value: TJppBasicPanelAppearance);
begin
  FAppearance := Value;
  Invalidate;
end;

procedure TJppCustomBasicPanel.SetOnGradientChange(const Value: TNotifyEvent);
begin
  FOnGradientChange := Value;
end;


procedure TJppCustomBasicPanel.SetTagExt(const Value: TJppTagExt);
begin
  FTagExt := Value;
end;

procedure TJppCustomBasicPanel.AdjustClientRect(var Rect: TRect);
begin
  inherited AdjustClientRect(Rect);
end;

  {$region ' ---------------- TJppCustomBasicPanel.Paint --------------------------- '}
procedure TJppCustomBasicPanel.Paint;
begin
  inherited;
end;
  {$endregion}

  {$region ' ---------------- TJppCustomBasicPanel.DrawBackground ---------------------- '}
procedure TJppCustomBasicPanel.DrawBackground(ARect: TRect);
var
  R: TRect;
  xBottomGradientTop: integer;
  {$IFDEF HAS_VCL_STYLES} bVclStyle: Boolean; {$ENDIF}
  Border: TJppBasicPanelBorder;
  xBottom: integer;

  {$IFDEF HAS_VCL_STYLES}
  Rect: TRect;
  LColor: TColor;
  LStyle: TCustomStyleServices;
  LDetails: TThemedElementDetails;
  TopColor, BottomColor: TColor;
  BaseColor, BaseTopColor, BaseBottomColor: TColor;

  procedure AdjustColors(Bevel: TPanelBevel);
  begin
    TopColor := BaseTopColor;
    if Bevel = bvLowered then TopColor := BaseBottomColor;
    BottomColor := BaseBottomColor;
    if Bevel = bvLowered then BottomColor := BaseTopColor;
  end;
  {$ENDIF}

begin

  {$IFDEF DCC}
  if ParentBackground then
  begin
    DrawBorders(ARect);
    Exit;
  end;
  {$ENDIF}

  {$IFDEF FPC}{$IFDEF HAS_PANEL_WITH_PARENTBACKGROUND}
  if ParentBackground then
  begin
    DrawBorders(ARect);
    Exit;
  end;
  {$ENDIF}{$ENDIF}

  {$IFDEF HAS_VCL_STYLES}
  bVclStyle := Assigned(TStyleManager.ActiveStyle) and (TStyleManager.ActiveStyle.Name <> 'Windows');
  {$ENDIF}

  //if (not bVclStyle) or (bVclStyle and (not (seClient in StyleElements))) then
  {$IFDEF HAS_VCL_STYLES}if (not bVclStyle) {$IFDEF HAS_STYLE_ELEMENTS} or (bVclStyle and (not (seClient in StyleElements))) {$ENDIF} then{$ENDIF}
  begin

    Canvas.Brush.Style := bsClear;
    R := ARect;
    Canvas.Brush.Color := Appearance.BackgroundColor;
    if not Appearance.DrawGradient then Canvas.FillRect(R); // last mod: 07.09.2016 - dodano IFa

    if Appearance.DrawGradient then
    begin

      if Appearance.BorderToGradientMargin > 0 then
      begin
        Border := Appearance.Borders.Top;
        if Border.Visible then Inc(R.Top, Border.Pen.Width);
        Inc(R.Top, Appearance.BorderToGradientMargin);

        Border := Appearance.Borders.Bottom;
        if Border.Visible then Dec(R.Bottom, Border.Pen.Width);
        Dec(R.Bottom, Appearance.BorderToGradientMargin);

        Border := Appearance.Borders.Left;
        if Border.Visible then Inc(R.Left, Border.Pen.Width);
        Inc(R.Left, Appearance.BorderToGradientMargin);

        Border := Appearance.Borders.Right;
        if Border.Visible then Dec(R.Right, Border.Pen.Width);
        Dec(R.Right, Appearance.BorderToGradientMargin);
      end;

      xBottom := R.Bottom;
      R.Bottom := R.Top + Round((Appearance.UpperGradientPercent * RectHeight(R)) / 100);
      xBottomGradientTop := R.Bottom;
      Appearance.UpperGradient.Draw(Canvas, R);

      R.Top := xBottomGradientTop;
      R.Bottom := xBottom;
      Appearance.BottomGradient.Draw(Canvas, R);

    end;

  end
  {$IFNDEF HAS_VCL_STYLES};{$ENDIF}

  {$IFDEF HAS_VCL_STYLES}
  else

  // VCL Style
  begin

    LStyle := StyleServices;

    Rect := GetClientRect;

    BaseColor := Color;
    BaseTopColor := clBtnHighlight;
    BaseBottomColor := clBtnShadow;

    if LStyle.Enabled {$IFDEF HAS_STYLE_ELEMENTS} and (seClient in StyleElements) {$ENDIF} then
    begin

      LDetails := LStyle.GetElementDetails(tpPanelBackground);
      if LStyle.GetElementColor(LDetails, ecFillColor, LColor) and (LColor <> clNone) then BaseColor := LColor;

      LDetails := LStyle.GetElementDetails(tpPanelBevel);
      if LStyle.GetElementColor(LDetails, ecEdgeHighLightColor, LColor) and (LColor <> clNone) then BaseTopColor := LColor;
      if LStyle.GetElementColor(LDetails, ecEdgeShadowColor, LColor) and (LColor <> clNone) then BaseBottomColor := LColor;

    end;


    if BevelOuter <> bvNone then
    begin
      AdjustColors(BevelOuter);
      Frame3D(Canvas, Rect, TopColor, BottomColor, BevelWidth);
    end;


    if not (LStyle.Enabled and (csParentBackground in ControlStyle)) then Frame3D(Canvas, Rect, BaseColor, BaseColor, BorderWidth)
    else InflateRect(Rect, -Integer(BorderWidth), -Integer(BorderWidth));


    if BevelInner <> bvNone then
    begin
      AdjustColors(BevelInner);
      Frame3D(Canvas, Rect, TopColor, BottomColor, BevelWidth);
    end;

    with Canvas do
    begin
      if not LStyle.Enabled or not ParentBackground {$IF RTLVersion > 23} or not (seClient in StyleElements) {$IFEND} then
      begin
        Brush.Color := BaseColor;
        FillRect(Rect);
      end;
    end;

  end; // VCL Style

  {$ENDIF} // DCC


  DrawBorders(ARect);

end;
  {$endregion DrawBackground}

  {$region ' ---------------- TJppCustomBasicPanel.DrawBorders ---------------- '}
procedure TJppCustomBasicPanel.DrawBorders(ARect: TRect);
begin

  if not Appearance.DrawBorder then Exit;
  Canvas.Brush.Style := bsClear;

  if Appearance.Borders.Left.Visible then DrawLeftBorder(Canvas, ARect, Appearance.Borders.Left.Pen, Appearance.Borders.Left.Border3D);
  if Appearance.Borders.Right.Visible then DrawRightBorder(Canvas, ARect, Appearance.Borders.Right.Pen, Appearance.Borders.Right.Border3D);

  if Appearance.Borders.Top.Visible then DrawTopBorder(Canvas, ARect, Appearance.Borders.Top.Pen, Appearance.Borders.Top.Border3D);
  if Appearance.Borders.Bottom.Visible then DrawBottomBorder(Canvas, ARect, Appearance.Borders.Bottom.Pen, Appearance.Borders.Bottom.Border3D);
end;
  {$endregion DrawBorders}


{$endregion TJppCustomBasicPanel}

{$region ' ------------------------------------- TJppBasicPanelAppearance ------------------------------------------- '}

constructor TJppBasicPanelAppearance.Create(AOwner: TComponent);
begin
  inherited Create;
  FOwner := AOwner;

  FUpperGradient := TJppGradientEx.Create(AOwner);
  FBottomGradient := TJppGradientEx.Create(AOwner);
  FUpperGradientPercent := 50;

  FUpperGradient.ColorFrom := GetSimilarColor(clBtnFace, 5, True);
  FUpperGradient.ColorTo := GetSimilarColor(clBtnFace, 5, False);
  FBottomGradient.ColorFrom := FUpperGradient.ColorTo;
  FBottomGradient.ColorTo := GetSimilarColor(clBtnFace, 10, False);

  FBorders := TJppBasicPanelBorders.Create(AOwner);
  FBorders.OnChange := {$IFDEF FPC} @ {$ENDIF}PropsChanged;

  FBorderToGradientMargin := 0;
  FDrawGradient := True;
  FBackgroundColor := clBtnFace;
  FDrawBorder := True;
end;

destructor TJppBasicPanelAppearance.Destroy;
begin
  FUpperGradient.Free;
  FBottomGradient.Free;
  FBorders.Free;
  inherited;
end;

procedure TJppBasicPanelAppearance.Assign(Source: TJppBasicPanelAppearance);
begin
  FBorders.Assign(Source.Borders);
  FUpperGradient.Assign(Source.UpperGradient);
  FBottomGradient.Assign(Source.BottomGradient);
  FUpperGradientPercent := Source.UpperGradientPercent;
  FBorderToGradientMargin := Source.BorderToGradientMargin;
  FBackgroundColor := Source.BackgroundColor;
  FDrawGradient := Source.DrawGradient;
  FDrawBorder := Source.DrawBorder;
  PropsChanged(Self);
end;

procedure TJppBasicPanelAppearance.PropsChanged(Sender: TObject);
begin
  if Assigned(OnChange) then OnChange(Self);
end;

procedure TJppBasicPanelAppearance.SetBackgroundColor(const Value: TColor);
begin
  FBackgroundColor := Value;
  PropsChanged(Self);
end;

procedure TJppBasicPanelAppearance.SetBorders(const Value: TJppBasicPanelBorders);
begin
  FBorders := Value;
  PropsChanged(Self);
end;

procedure TJppBasicPanelAppearance.SetBorderToGradientMargin(const Value: integer);
begin
  FBorderToGradientMargin := Value;
  PropsChanged(Self);
end;

procedure TJppBasicPanelAppearance.SetBottomGradient(const Value: TJppGradientEx);
begin
  FBottomGradient := Value;
  PropsChanged(Self);
end;

procedure TJppBasicPanelAppearance.SetDrawBorder(const Value: Boolean);
begin
  FDrawBorder := Value;
  PropsChanged(Self);
end;

procedure TJppBasicPanelAppearance.SetDrawGradient(const Value: Boolean);
begin
  FDrawGradient := Value;
  PropsChanged(Self);
end;

procedure TJppBasicPanelAppearance.SetOnChange(const Value: TNotifyEvent);
begin
  FOnChange := Value;
end;

procedure TJppBasicPanelAppearance.SetUpperGradient(const Value: TJppGradientEx);
begin
  FUpperGradient := Value;
  PropsChanged(Self);
end;

procedure TJppBasicPanelAppearance.SetUpperGradientPercent(const Value: Byte);
begin
  FUpperGradientPercent := Value;
  FBottomGradient.OnChange(Self);
  FUpperGradient.OnChange(Self);
  PropsChanged(Self);
end;

{$endregion TJppBasicPanelAppearance}

{$region ' ------------------------------------- TJppBasicPanelBorder ------------------------------------ '}

constructor TJppBasicPanelBorder.Create(AOwner: TComponent);
begin
  inherited Create;
  FPen := TPen.Create;
  FVisible := True;
  FPen.OnChange := {$IFDEF FPC} @ {$ENDIF}PropsChanged;
  FBorder3D := True;
end;

destructor TJppBasicPanelBorder.Destroy;
begin
  FPen.Free;
  inherited;
end;

procedure TJppBasicPanelBorder.Assign(Border: TJppBasicPanelBorder);
begin
  FPen.Assign(Border.Pen);
  FPen.OnChange := {$IFDEF FPC} @ {$ENDIF}PropsChanged;
  FVisible := Border.Visible;
  FBorder3D := Border.Border3D;
end;

procedure TJppBasicPanelBorder.PropsChanged(Sender: TObject);
begin
  if Assigned(OnChange) then OnChange(Self);
end;

procedure TJppBasicPanelBorder.SetBorder3D(const Value: Boolean);
begin
  FBorder3D := Value;
  PropsChanged(Self);
end;

procedure TJppBasicPanelBorder.SetOnChange(const Value: TNotifyEvent);
begin
  FOnChange := Value;
end;

procedure TJppBasicPanelBorder.SetPen(const Value: TPen);
begin
  FPen := Value;
  PropsChanged(Self);
end;

procedure TJppBasicPanelBorder.SetVisible(const Value: Boolean);
begin
  FVisible := Value;
  PropsChanged(Self);
end;

{$endregion}

{$region ' ----------------------------------------- TJppBasicPanelBorders ------------------------------------------ '}
procedure TJppBasicPanelBorders.Assign(Borders: TJppBasicPanelBorders);
begin
  FLeft.Assign(Borders.Left);
  FRight.Assign(Borders.Right);
  FTop.Assign(Borders.Top);
  FBottom.Assign(Borders.Bottom);
end;

constructor TJppBasicPanelBorders.Create(AOwner: TComponent);
begin
  inherited Create;
  FLeft := TJppBasicPanelBorder.Create(AOwner);
  FRight := TJppBasicPanelBorder.Create(AOwner);
  FTop := TJppBasicPanelBorder.Create(AOwner);
  FBottom := TJppBasicPanelBorder.Create(AOwner);

  FLeft.OnChange := {$IFDEF FPC} @ {$ENDIF}PropsChanged;
  FRight.OnChange := {$IFDEF FPC} @ {$ENDIF}PropsChanged;
  FTop.OnChange := {$IFDEF FPC} @ {$ENDIF}PropsChanged;
  FBottom.OnChange := {$IFDEF FPC} @ {$ENDIF}PropsChanged;

  FLeft.Pen.Color := clBtnHighlight;
  FRight.Pen.Color := clBtnShadow;
  FTop.Pen.Color := clBtnHighlight;
  FBottom.Pen.Color := clBtnShadow;
end;

destructor TJppBasicPanelBorders.Destroy;
begin
  FLeft.Free;
  FRight.Free;
  FTop.Free;
  FBottom.Free;
  inherited;
end;

procedure TJppBasicPanelBorders.PropsChanged(Sender: TObject);
begin
  if Assigned(OnChange) then OnChange(Self);
end;

procedure TJppBasicPanelBorders.SetBottom(const Value: TJppBasicPanelBorder);
begin
  FBottom := Value;
  PropsChanged(Self);
end;

procedure TJppBasicPanelBorders.SetLeft(const Value: TJppBasicPanelBorder);
begin
  FLeft := Value;
  PropsChanged(Self);
end;

procedure TJppBasicPanelBorders.SetOnChange(const Value: TNotifyEvent);
begin
  FOnChange := Value;
end;

procedure TJppBasicPanelBorders.SetRight(const Value: TJppBasicPanelBorder);
begin
  FRight := Value;
  PropsChanged(Self);
end;

procedure TJppBasicPanelBorders.SetTop(const Value: TJppBasicPanelBorder);
begin
  FTop := Value;
  PropsChanged(Self);
end;

{$endregion}




{$region ' ---------------- Themes ------------------------- '}
{$IFDEF DCC}

{$IF RTLVersion < 23.0 }
//type
//  TThemeServicesHelper = class helper for TThemeServices
//  private
//    function GetEnabled: Boolean;
//  public
//    function GetElementContentRect(DC: HDC; Details: TThemedElementDetails; const BoundingRect: TRect; out ContentRect: TRect): Boolean; overload;
//    property Enabled: Boolean read GetEnabled;
//  end;
//
//function TThemeServicesHelper.GetElementContentRect(DC: HDC; Details: TThemedElementDetails; const BoundingRect: TRect; out ContentRect: TRect): Boolean;
//begin
//  ContentRect := Self.ContentRect(DC, Details, BoundingRect);
//  Result := true;
//end;
//
//function TThemeServicesHelper.GetEnabled: Boolean;
//begin
//  Result := ThemesEnabled;
//end;
//
//function StyleServices: TThemeServices;
//begin
//  Result := ThemeServices;
//end;
{$IFEND}



{$IF RTLVersion >= 24.0 }

//class constructor TJppBasicPanel.Create;
//begin
//  TCustomStyleEngine.RegisterStyleHook(TJppBasicPanel, TJppBasicPanelStyleHook);
//end;
//
//class destructor TJppBasicPanel.Destroy;
//begin
//  TCustomStyleEngine.UnRegisterStyleHook(TJppBasicPanel, TJppBasicPanelStyleHook);
//end;
{$IFEND}

{$ENDIF} // DCC
{$endregion Themes}

end.
