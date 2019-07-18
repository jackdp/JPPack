unit JPP.SimplePanel;

{$IFDEF FPC} {$mode objfpc}{$H+} {$ENDIF}

interface

uses
  {$IFDEF DCC}
  Winapi.Messages, Winapi.Windows,
  System.SysUtils, System.Classes, System.Types, System.UITypes,
  Vcl.Controls, Vcl.Forms, Vcl.Menus, Vcl.Graphics, Vcl.StdCtrls, Vcl.ExtCtrls, Vcl.Dialogs,
  {$ELSE}
  SysUtils, Messages, LMessages, LCLType, LCLIntf, Classes, Graphics, Controls, StdCtrls, ExtCtrls,
  {$ENDIF}
  JPL.Colors,
  JPP.Types, JPP.Graphics, JPP.Common, JPP.Common.Procs;


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
    procedure SetOnChange(const Value: TNotifyEvent);
    procedure SetBackgroundColor(const Value: TColor);
    procedure SetBorderColor(const Value: TColor);
    procedure SetDrawTopBorder(const Value: Boolean);
    procedure SetDrawBottomBorder(const Value: Boolean);
    procedure SetDrawLeftBorder(const Value: Boolean);
    procedure SetDrawRightBorder(const Value: Boolean);
    procedure SetBorderStyle(const Value: TPenStyle);
  protected
    procedure PropsChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(Source: TJppSimplePanelAppearance); reintroduce;
  published
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor default clBtnFace;
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
    procedure SetAppearance(const Value: TJppSimplePanelAppearance);
    procedure SetTagExt(const Value: TJppTagExt);
  protected
    procedure DrawBackground(ARect: TRect); override;
    procedure DrawBorders(ARect: TRect);
    procedure PropsChanged(Sender: TObject);
    property Appearance: TJppSimplePanelAppearance read FAppearance write SetAppearance;
    procedure Paint; override;
    procedure AdjustClientRect(var Rect: TRect); override;
    property TagExt: TJppTagExt read FTagExt write SetTagExt;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
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
//    property Color;
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
    property ParentBackground default false;
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
    {$IFDEF DCC}{$IF RTLVersion > 23} property StyleElements; {$IFEND}{$ENDIF}
    {$IFDEF DCC} property Touch; {$ENDIF}
    property DoubleBuffered;
    property ParentDoubleBuffered;
    {$IFDEF FPC}
    property BorderSpacing;
    property ChildSizing;
    property OnGetDockCaption;
    {$ENDIF}
  end;
  {$endregion}


implementation


{$region ' ---------------------------------------- TJppCustomBaseSimplePanel ----------------------------------------------------- '}
constructor TJppCustomBaseSimplePanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  BevelOuter := bvNone;
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
  then begin
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
begin
  //
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
  ParentBackground := False;

  FTagExt := TJppTagExt.Create(Self);
  FAppearance := TJppSimplePanelAppearance.Create(Self);
  FAppearance.OnChange := {$IFDEF FPC} @ {$ENDIF}PropsChanged;
end;

destructor TJppCustomSimplePanel.Destroy;
begin
  FTagExt.Free;
  FAppearance.Free;
  inherited Destroy;
end;

procedure TJppCustomSimplePanel.PropsChanged(Sender: TObject);
begin
  if csLoading in ComponentState then Exit;
  Invalidate;
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

  if ParentBackground then
  begin
    DrawBorders(ARect);
    Exit;
  end;

  Canvas.Brush.Style := bsClear;
  R := ARect;
  Canvas.Brush.Color := Appearance.BackgroundColor;
  Canvas.FillRect(R);

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
end;

destructor TJppSimplePanelAppearance.Destroy;
begin
  inherited;
end;

procedure TJppSimplePanelAppearance.Assign(Source: TJppSimplePanelAppearance);
begin
  FBackgroundColor := Source.BackgroundColor;
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
