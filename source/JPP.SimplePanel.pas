unit JPP.SimplePanel;

{
  Jacek Pazera
  https://www.pazera-software.com
  https://github.com/jackdp
}


{$I jpp.inc}
{$IFDEF FPC} {$mode objfpc}{$H+} {$ENDIF}


interface

uses
  {$IFDEF MSWINDOWS}Windows,{$ENDIF}
  Messages,
  SysUtils, Classes, Types, {$IFDEF HAS_SYSTEM_UITYPES}System.UITypes,{$ENDIF}
  Controls, Forms, Menus, Graphics, StdCtrls, ExtCtrls, Dialogs,
  {$IFDEF FPC}LMessages, LCLType, LCLIntf,{$ENDIF}
  JPL.Colors, JPL.Rects,
  JPP.Types, JPP.Graphics, JPP.Gradient, JPP.AnchoredControls, JPP.Common, JPP.Common.Procs;


type


  {$region ' ----------- TJppSimplePanelAppearance ---------- '}
  TJppSimplePanelAppearance = class(TPersistent)
  private
    FOwner: TComponent;
    FOnChange: TNotifyEvent;
    FBackgroundColor: TColor;
    FBorderColor: TColor;
    FDrawTopBorder: Boolean;
    FDrawBottomBorder: Boolean;
    FDrawLeftBorder: Boolean;
    FDrawRightBorder: Boolean;
    FBorderStyle: TPenStyle;
    FBackroundColorTo: TColor;
    procedure SetOnChange(const Value: TNotifyEvent);
    procedure SetBackgroundColor(const Value: TColor);
    procedure SetBorderColor(const Value: TColor);
    procedure SetDrawTopBorder(const Value: Boolean);
    procedure SetDrawBottomBorder(const Value: Boolean);
    procedure SetDrawLeftBorder(const Value: Boolean);
    procedure SetDrawRightBorder(const Value: Boolean);
    procedure SetBorderStyle(const Value: TPenStyle);
    procedure SetBackroundColorTo(const Value: TColor);
  protected
    procedure PropsChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(Source: TJppSimplePanelAppearance); reintroduce;
  published
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor default clBtnFace;
    property BackroundColorTo: TColor read FBackroundColorTo write SetBackroundColorTo default clNone;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clGray;
    property DrawTopBorder: Boolean read FDrawTopBorder write SetDrawTopBorder default True;
    property DrawBottomBorder: Boolean read FDrawBottomBorder write SetDrawBottomBorder default True;
    property DrawLeftBorder: Boolean read FDrawLeftBorder write SetDrawLeftBorder default True;
    property DrawRightBorder: Boolean read FDrawRightBorder write SetDrawRightBorder default True;
    property BorderStyle: TPenStyle read FBorderStyle write SetBorderStyle default psSolid;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
  end;
  {$endregion}


  {$region ' --------------------------- TJppCustomBaseSimplePanel ---------------------------------- '}

  {$IFDEF MSWINDOWS}TProcOnOnDragDropFiles = procedure (Sender: TObject; var msg: TWMDropFiles) of object;{$ENDIF}

  TJppCustomBaseSimplePanel = class(TCustomPanel)
  private
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnPaint: TNotifyEvent;
    FOnVisibleChanging: TNotifyEvent;
    FOnAfterDrawBackground: TNotifyEvent;
    {$IFDEF MSWINDOWS}FOnDragDropFiles: TProcOnOnDragDropFiles;{$ENDIF}
    FCaptionMargin: integer;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    {$IFDEF MSWINDOWS}procedure CMDropFiles(var msg: TWMDropFiles); message WM_DROPFILES;{$ENDIF}
    procedure PropsChanged(Sender: TObject);
    procedure SetCaptionMargin(const Value: integer);
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
    property OnAfterDrawBackground: TNotifyEvent read FOnAfterDrawBackground write FOnAfterDrawBackground;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnPaint: TNotifyEvent read FOnPaint write FOnPaint;
    property OnVisibleChanging: TNotifyEvent read FOnVisibleChanging write FOnVisibleChanging;
    {$IFDEF MSWINDOWS}property OnDragDropFiles: TProcOnOnDragDropFiles read FOnDragDropFiles write FOnDragDropFiles;{$ENDIF}
    property CaptionMargin: integer read FCaptionMargin write SetCaptionMargin default 6;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
  end;
  {$endregion}


  {$region ' --------------------------------- TJppCustomSimplePanel ---------------------------------------- '}
  TJppCustomSimplePanel = class(TJppCustomBaseSimplePanel)
  private
    FAppearance: TJppSimplePanelAppearance;
    FTagExt: TJppTagExt;
    FAnchoredControls: TJppAnchoredControls;
    procedure SetAppearance(const Value: TJppSimplePanelAppearance);
    procedure SetTagExt(const Value: TJppTagExt);
    procedure SetAnchoredControls(const Value: TJppAnchoredControls);
  protected
    procedure DrawBackground(ARect: TRect); override;
    procedure DrawBorders(ARect: TRect);
    procedure PropsChanged(Sender: TObject);
    property Appearance: TJppSimplePanelAppearance read FAppearance write SetAppearance;
    procedure Paint; override;
    procedure AdjustClientRect(var Rect: TRect); override;
    property TagExt: TJppTagExt read FTagExt write SetTagExt;
    property AnchoredControls: TJppAnchoredControls read FAnchoredControls write SetAnchoredControls;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;

    property Canvas;
    property DockManager;
  published
  end;
  {$endregion}


  {$region ' ------------------------ TJppSimplePanel ------------------------------- '}
  TJppSimplePanel = class(TJppCustomSimplePanel)
  published
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BiDiMode;
//    property Color; use Appearance.BackgroundColor
    property Constraints;
    property Caption;
    property CaptionMargin;
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
    {$IFDEF DCC}property ParentBackground default false;{$ENDIF}
    {$IFDEF FPC}{$IFDEF HAS_PANEL_WITH_PARENTBACKGROUND}
    property ParentBackground default false;
    {$ENDIF}{$ENDIF}
    {$IFDEF DCC} property ParentCtl3D; {$ENDIF}
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    {$IFDEF DELPHI2009_OR_ABOVE} property ShowCaption; {$ENDIF}
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
    {$IFDEF HAS_STYLE_ELEMENTS} property StyleElements; {$ENDIF}
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
    property AnchoredControls;
  end;
  {$endregion}


implementation


{$region ' ---------------------------------------- TJppCustomBaseSimplePanel ----------------------------------------------------- '}
constructor TJppCustomBaseSimplePanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BevelOuter := bvNone;
  FCaptionMargin := 6;
end;

destructor TJppCustomBaseSimplePanel.Destroy;
begin
  inherited Destroy;
end;

procedure TJppCustomBaseSimplePanel.Loaded;
begin
  Inherited;
end;

procedure TJppCustomBaseSimplePanel.PropsChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TJppCustomBaseSimplePanel.SetCaptionMargin(const Value: integer);
begin
  if FCaptionMargin = Value then Exit;
  FCaptionMargin := Value;
  Invalidate;
end;

procedure TJppCustomBaseSimplePanel.AdjustClientRect(var Rect: TRect);
begin
  inherited AdjustClientRect(Rect);
end;


  {$region ' --------------------------- PAINT & DRAW ------------------------------------ '}
procedure TJppCustomBaseSimplePanel.Paint;
var
  Rect, TextRect: TRect;
begin
  Rect := GetClientRect;
  DrawBackground(Rect);

  if Assigned(FOnAfterDrawBackground) then FOnAfterDrawBackground(Self);

  if (Caption <> '') {$IFDEF DELPHI2009_OR_ABOVE} and (ShowCaption) {$ENDIF}
  then
  begin
    TextRect := Rect;
    DrawCaption(TextRect);
  end;

  if Assigned(FOnPaint) then FOnPaint(Self);
end;

procedure TJppCustomBaseSimplePanel.DrawBackground(ARect: TRect);
begin
  //
end;

procedure TJppCustomBaseSimplePanel.DrawCaption(aRect: TRect);
var
  x, y, tw, th: integer;
begin
  {$IFDEF DCC}if not ShowCaption then Exit;{$ENDIF}
  if Caption = '' then Exit;

  with Canvas do
  begin
    Font.Assign(Self.Font);
    tw := TextWidth(Caption);
    th := TextHeight(Caption);
    case Alignment of
      taLeftJustify: x := 0;
      taCenter: x := (aRect.Width div 2) - tw div 2;
      taRightJustify: x := aRect.Width - tw;
    else
      x := 0;
    end;

    if (Alignment = taLeftJustify) and (FCaptionMargin <> 0) then x := x + FCaptionMargin
    else if (Alignment = taRightJustify) and (FCaptionMargin <> 0) then x := x - FCaptionMargin;

    y := (aRect.Height div 2) - (th div 2);
    TextOut(x, y, Caption);
  end;
end;

  {$endregion}

  {$region ' ------------------- reszta ------------------------------- '}

procedure TJppCustomBaseSimplePanel.VisibleChanging;
begin
  inherited;
  if Assigned(FOnVisibleChanging) then FOnVisibleChanging(Self);
end;

{$IFDEF MSWINDOWS}
procedure TJppCustomBaseSimplePanel.CMDropFiles(var msg: TWMDropFiles);
begin
  if Assigned(FOnDragDropFiles) then FOnDragDropFiles(Self, msg);
end;
{$ENDIF}

procedure TJppCustomBaseSimplePanel.CMMouseEnter(var Msg: TMessage);
begin
  inherited;
  MouseEnter(Self);
end;

procedure TJppCustomBaseSimplePanel.CMMouseLeave(var Msg: TMessage);
begin
  inherited;
  MouseLeave(Self);
end;

procedure TJppCustomBaseSimplePanel.MouseEnter(Sender: TObject);
begin
  if Assigned(FOnMouseEnter) then FOnMouseEnter(Self);
end;

procedure TJppCustomBaseSimplePanel.MouseLeave(Sender: TObject);
begin
  if Assigned(FonMouseLeave) then FOnMouseLeave(Self);
end;

procedure TJppCustomBaseSimplePanel.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
end;

procedure TJppCustomBaseSimplePanel.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
end;

procedure TJppCustomBaseSimplePanel.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
end;
  {$endregion}


{$endregion TJppCustomBaseSimplePanel}


{$region ' -------------------------------------------------------- TJppCustomSimplePanel ------------------------------------------------------------ '}
constructor TJppCustomSimplePanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  {$IFDEF DCC}ParentBackground := False;{$ENDIF}
  {$IFDEF FPC}{$IFDEF HAS_PANEL_WITH_PARENTBACKGROUND}
  ParentBackground := False;
  {$ENDIF}{$ENDIF}

  FTagExt := TJppTagExt.Create(Self);
  FAppearance := TJppSimplePanelAppearance.Create(Self);
  FAppearance.OnChange := {$IFDEF FPC} @ {$ENDIF}PropsChanged;
  FAnchoredControls := TJppAnchoredControls.Create(Self);
end;

destructor TJppCustomSimplePanel.Destroy;
begin
  FTagExt.Free;
  FAppearance.Free;
  FAnchoredControls.Free;
  inherited Destroy;
end;

procedure TJppCustomSimplePanel.PropsChanged(Sender: TObject);
begin
  if csLoading in ComponentState then Exit;
  Invalidate;
  Self.Color := FAppearance.BackgroundColor;
end;

procedure TJppCustomSimplePanel.SetAnchoredControls(const Value: TJppAnchoredControls);
begin
  FAnchoredControls := Value;
end;

procedure TJppCustomSimplePanel.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  if not (csDestroying in ComponentState) then
    if Assigned(FAnchoredControls) then FAnchoredControls.UpdateAllControlsPos;
end;

procedure TJppCustomSimplePanel.Notification(AComponent: TComponent; Operation: TOperation);
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

procedure TJppCustomSimplePanel.SetAppearance(const Value: TJppSimplePanelAppearance);
begin
  FAppearance := Value;
  Invalidate;
end;

procedure TJppCustomSimplePanel.SetTagExt(const Value: TJppTagExt);
begin
  FTagExt := Value;
end;

procedure TJppCustomSimplePanel.AdjustClientRect(var Rect: TRect);
begin
  inherited AdjustClientRect(Rect);
end;

procedure TJppCustomSimplePanel.Paint;
begin
  inherited;
end;

  {$region ' ---------------- TJppCustomSimplePanel.DrawBackground ---------------------- '}
procedure TJppCustomSimplePanel.DrawBackground(ARect: TRect);
var
  R: TRect;
begin
  // Fix controls color, when DoubleBuffered = True
  // Gdy na panelu z włączonym DoubleBuffered umieszczone były kontrolki, to
  // "pobierały" one kolor z właściwości Color panelu.
  // To rozwiązuje problem koloru tła kontrolek, ale tylko gdy nie jest używany gradient.
  Self.Color := FAppearance.BackgroundColor;

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

  R := ARect;

  if Appearance.BackroundColorTo <> clNone then
  begin
    JppGradientFill(Canvas, R, Appearance.BackgroundColor, Appearance.BackroundColorTo, gtVertical);
  end
  else
  begin
    Canvas.Brush.Style := bsClear;
    Canvas.Brush.Color := Appearance.BackgroundColor;
    Canvas.FillRect(R);
  end;

  DrawBorders(ARect);

end;
  {$endregion DrawBackground}

  {$region ' ---------------- TJppCustomSimplePanel.DrawBorders ---------------- '}
procedure TJppCustomSimplePanel.DrawBorders(ARect: TRect);
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


{$endregion TJppCustomSimplePanel}


{$region ' ------------------------------------- TJppSimplePanelAppearance ------------------------------------------- '}

constructor TJppSimplePanelAppearance.Create(AOwner: TComponent);
begin
  inherited Create;
  FOwner := AOwner;

  FBorderColor := clGray;
  FDrawTopBorder := True;
  FDrawBottomBorder := True;
  FDrawLeftBorder := True;
  FDrawRightBorder := True;
  FBorderStyle := psSolid;

  FBackgroundColor := clBtnFace;
  FBackroundColorTo := clNone;
end;

destructor TJppSimplePanelAppearance.Destroy;
begin
  inherited;
end;

procedure TJppSimplePanelAppearance.Assign(Source: TJppSimplePanelAppearance);
begin
  FBackgroundColor := Source.BackgroundColor;
  FBackroundColorTo := Source.BackroundColorTo;
  FBorderColor := Source.BorderColor;
  FDrawTopBorder := Source.DrawTopBorder;
  FDrawBottomBorder := Source.DrawBottomBorder;
  FDrawLeftBorder := Source.DrawLeftBorder;
  FDrawRightBorder := Source.DrawRightBorder;
  FBorderStyle := Source.BorderStyle;
  PropsChanged(Self);
end;

procedure TJppSimplePanelAppearance.PropsChanged(Sender: TObject);
begin
  if Assigned(OnChange) then OnChange(Self);
end;

procedure TJppSimplePanelAppearance.SetBackgroundColor(const Value: TColor);
begin
  FBackgroundColor := Value;
  PropsChanged(Self);
end;

procedure TJppSimplePanelAppearance.SetBackroundColorTo(const Value: TColor);
begin
  if FBackroundColorTo = Value then Exit;
  FBackroundColorTo := Value;
  PropsChanged(Self);
end;

procedure TJppSimplePanelAppearance.SetBorderColor(const Value: TColor);
begin
  if FBorderColor = Value then Exit;
  FBorderColor := Value;
  PropsChanged(Self);
end;

procedure TJppSimplePanelAppearance.SetBorderStyle(const Value: TPenStyle);
begin
  if FBorderStyle = Value then Exit;
  FBorderStyle := Value;
  PropsChanged(Self);
end;

procedure TJppSimplePanelAppearance.SetDrawBottomBorder(const Value: Boolean);
begin
  if FDrawBottomBorder = Value then Exit;
  FDrawBottomBorder := Value;
  PropsChanged(Self);
end;

procedure TJppSimplePanelAppearance.SetDrawLeftBorder(const Value: Boolean);
begin
  if FDrawLeftBorder = Value then Exit;
  FDrawLeftBorder := Value;
  PropsChanged(Self);
end;

procedure TJppSimplePanelAppearance.SetDrawRightBorder(const Value: Boolean);
begin
  if FDrawRightBorder = Value then Exit;
  FDrawRightBorder := Value;
  PropsChanged(Self);
end;

procedure TJppSimplePanelAppearance.SetDrawTopBorder(const Value: Boolean);
begin
  if FDrawTopBorder = Value then Exit;
  FDrawTopBorder := Value;
  PropsChanged(Self);
end;

procedure TJppSimplePanelAppearance.SetOnChange(const Value: TNotifyEvent);
begin
  FOnChange := Value;
end;

{$endregion TJppSimplePanelAppearance}



end.
