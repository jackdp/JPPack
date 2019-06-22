unit JPP.Panel;

{$IFDEF FPC} {$mode objfpc}{$H+} {$ENDIF}

{$IFDEF VER200}
  {$DEFINE DELPHI2009_OR_ABOVE}
{$ENDIF}

interface

uses
  {$IFDEF DCC}
  Winapi.Messages, Winapi.Windows,
  System.SysUtils, System.Classes, System.Types, System.UITypes,
  Vcl.Controls, Vcl.Forms, Vcl.Menus, Vcl.Graphics, Vcl.StdCtrls, Vcl.GraphUtil, Vcl.Themes, Vcl.ExtCtrls, Vcl.Dialogs,
  {$ELSE}
  SysUtils, Messages, LMessages, LCLType, LCLIntf, Classes, Graphics, Controls, StdCtrls, ExtCtrls, Forms,
  {$ENDIF}
  JPL.Colors, JPL.Math,
  JPP.Types, JPP.Graphics, JPP.Common, JPP.Common.Procs;


type

  //TJppPanelTagExt = class(TJppTagExt);

  //{$IFDEF FPC}TVerticalAlignment = (taAlignTop, taAlignBottom, taVerticalCenter);{$ENDIF}

  TJppVerticalLinePosition = (vlpFromLeft, vlpCenter, vlpFromRight);
  TJppHorizontalLinePosition = (hlpFromTop, hlpCenter, hlpFromBottom);

  TJppTextHorizontalPosition = (thpFromLeft, thpCenter, thpFromRight);
  TJppTextVerticalPosition = (tvpFromTop, tvpCenter, tvpFromBottom);

  TJppHorizontalBarPosition = (hbpFromTop, hbpCenter, hbpFromBottom, hbpPercentFromTop, hbpPercentFromBottom);
  TJppVerticalBarPosition = (vbpFromLeft, vbpCenter, vbpFromRight, vbpPercentFromLeft, vbpPercentFromRight);


  {$region ' ------------- TJppPanelHorizontalBar - collection item -------------- '}
  TJppPanelHorizontalBar = class(TCollectionItem)
  private
    FOnChange: TNotifyEvent;
    FBorder: TPen;
    FHeight: integer;
    FPosY: integer;
    FBarPosition: TJppHorizontalBarPosition;
    FBackgroundColor: TColor;
    FGradient: TJppGradientEx;
    FDrawGradient: Boolean;
    FLeftMargin: integer;
    FRightMargin: integer;
    FVisible: Boolean;
    procedure SetOnChange(const Value: TNotifyEvent);
    procedure SetBorder(const Value: TPen);
    procedure SetHeight(const Value: integer);
    procedure SetPosY(const Value: integer);
    procedure SetBarPosition(const Value: TJppHorizontalBarPosition);
    procedure SetBackgroundColor(const Value: TColor);
    procedure SetGradient(const Value: TJppGradientEx);
    procedure SetDrawGradient(const Value: Boolean);
    procedure SetLeftMargin(const Value: integer);
    procedure SetRightMargin(const Value: integer);
    procedure SetVisible(const Value: Boolean);
  protected
    procedure PropsChanged(Sender: TObject);
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
  published
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
    property Border: TPen read FBorder write SetBorder;
    property Height: integer read FHeight write SetHeight default 20;
    property PosY: integer read FPosY write SetPosY default 20;
    property BarPosition: TJppHorizontalBarPosition read FBarPosition write SetBarPosition default hbpFromTop;
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor default clBtnFace;
    property Gradient: TJppGradientEx read FGradient write SetGradient;
    property DrawGradient: Boolean read FDrawGradient write SetDrawGradient default True;
    property LeftMargin: integer read FLeftMargin write SetLeftMargin default 0;
    property RightMargin: integer read FRightMargin write SetRightMargin default 0;
    property Visible: Boolean read FVisible write SetVisible default True;
  end;
  {$endregion}

  {$region ' --------------- TJppPanelHorizontalBars - collection ---------------- '}
  TJppPanelHorizontalBars = class(TCollection)
  private
    FOwner: TComponent;
    FOnChange: TNotifyEvent;
    procedure SetOnChange(const Value: TNotifyEvent);
    function GetItem(Index: Integer): TJppPanelHorizontalBar;
    procedure SetItem(Index: Integer; const Value: TJppPanelHorizontalBar);
  protected
    function GetOwner: TPersistent; override;
    procedure PropsChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent);
    function Add: TJppPanelHorizontalBar;
    procedure Delete(Index: Integer);
    function Insert(Index: Integer): TJppPanelHorizontalBar;
    property Items[Index: Integer]: TJppPanelHorizontalBar read GetItem write SetItem; default;
  published
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
  end;
  {$endregion}

  {$region ' ------------- TJppPanelCaption - collection item -------------- '}
  TJppPanelCaption = class(TCollectionItem)
  private
    FOnChange: TNotifyEvent;
    FText: string;
    FFont: TFont;
    FPosX: integer;
    FPosY: integer;
    FMarginX: integer;
    FMarginY: integer;
    FVerticalPosition: TJppTextVerticalPosition;
    FHorizontalPosition: TJppTextHorizontalPosition;
    FVisible: Boolean;
    procedure SetOnChange(const Value: TNotifyEvent);
    procedure SetText(const Value: string);
    procedure SetFont(const Value: TFont);
    procedure SetPosX(const Value: integer);
    procedure SetPosY(const Value: integer);
    procedure SetMarginX(const Value: integer);
    procedure SetMarginY(const Value: integer);
    procedure SetVerticalPosition(const Value: TJppTextVerticalPosition);
    procedure SetHorizontalPosition(const Value: TJppTextHorizontalPosition);
    procedure SetVisible(const Value: Boolean);
  protected
    procedure PropsChanged(Sender: TObject);
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
    property Text: string read FText write SetText;
    property Font: TFont read FFont write SetFont;
    property PosX: integer read FPosX write SetPosX default 10;
    property PosY: integer read FPosY write SetPosY default 10;
    property MarginX: integer read FMarginX write SetMarginX default 0;
    property MarginY: integer read FMarginY write SetMarginY default 0;
    property HorizontalPosition: TJppTextHorizontalPosition read FHorizontalPosition write SetHorizontalPosition default thpFromLeft;
    property VerticalPosition: TJppTextVerticalPosition read FVerticalPosition write SetVerticalPosition default tvpFromTop;
    property Visible: Boolean read FVisible write SetVisible default True;
  end;
  {$endregion}

  {$region ' --------------- TJppPanelCaptions - collection ---------------- '}
  TJppPanelCaptions = class(TCollection)
  private
    FOwner: TComponent;
    FOnChange: TNotifyEvent;
    procedure SetOnChange(const Value: TNotifyEvent);
    function GetItem(Index: Integer): TJppPanelCaption;
    procedure SetItem(Index: Integer; const Value: TJppPanelCaption);
  protected
    function GetOwner: TPersistent; override;
    procedure PropsChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent);
    function Add: TJppPanelCaption;
    procedure Delete(Index: Integer);
    function Insert(Index: Integer): TJppPanelCaption;
    property Items[Index: Integer]: TJppPanelCaption read GetItem write SetItem; default;
  published
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
  end;
  {$endregion}

  {$region ' ------------- TJppPanelHorizontalLine - collection item -------------- '}
  TJppPanelHorizontalLine = class(TCollectionItem)
  private
    FOnChange: TNotifyEvent;
    FPen: TPen;
    FLeftMargin: integer;
    FRightMargin: integer;
    FVisible: Boolean;
    FPosY: integer;
    FLinePosition: TJppHorizontalLinePosition;
    procedure SetOnChange(const Value: TNotifyEvent);
    procedure SetPen(const Value: TPen);
    procedure SetLeftMargin(const Value: integer);
    procedure SetRightMargin(const Value: integer);
    procedure SetVisible(const Value: Boolean);
    procedure SetPosY(const Value: integer);
    procedure SetLinePosition(const Value: TJppHorizontalLinePosition);
  protected
    procedure PropsChanged(Sender: TObject);
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Pen: TPen read FPen write SetPen;
    property LeftMargin: integer read FLeftMargin write SetLeftMargin default 0;
    property RightMargin: integer read FRightMargin write SetRightMargin default 0;
    property Visible: Boolean read FVisible write SetVisible default True;
    property PosY: integer read FPosY write SetPosY default 10;
    property LinePosition: TJppHorizontalLinePosition read FLinePosition write SetLinePosition default hlpFromTop;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
  end;
  {$endregion}

  {$region ' --------------- TJppPanelHorizontalLines - collection ---------------- '}
  TJppPanelHorizontalLines = class(TCollection)
  private
    FOwner: TComponent;
    FOnChange: TNotifyEvent;
    procedure SetOnChange(const Value: TNotifyEvent);
    function GetItem(Index: Integer): TJppPanelHorizontalLine;
    procedure SetItem(Index: Integer; const Value: TJppPanelHorizontalLine);
  protected
    function GetOwner: TPersistent; override;
    procedure PropsChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent);
    function Add: TJppPanelHorizontalLine;
    procedure Delete(Index: Integer);
    function Insert(Index: Integer): TJppPanelHorizontalLine;
    property Items[Index: Integer]: TJppPanelHorizontalLine read GetItem write SetItem; default;
  published
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
  end;
  {$endregion}

  {$region ' ------------- TJppPanelVerticalLine - collection item -------------- '}
  TJppPanelVerticalLine = class(TCollectionItem)
  private
    FPen: TPen;
    FOnChange: TNotifyEvent;
    FTopMargin: integer;
    FBottomMargin: integer;
    FVisible: Boolean;
    FPosX: integer;
    FLinePosition: TJppVerticalLinePosition;
    procedure SetPen(const Value: TPen);
    procedure SetOnChange(const Value: TNotifyEvent);
    procedure SetTopMargin(const Value: integer);
    procedure SetBottomMargin(const Value: integer);
    procedure SetVisible(const Value: Boolean);
    procedure SetPosX(const Value: integer);
    procedure SetLinePosition(const Value: TJppVerticalLinePosition);
  protected
    procedure PropsChanged(Sender: TObject);
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Pen: TPen read FPen write SetPen;
    property TopMargin: integer read FTopMargin write SetTopMargin default 0;
    property BottomMargin: integer read FBottomMargin write SetBottomMargin default 0;
    property Visible: Boolean read FVisible write SetVisible default True;
    property PosX: integer read FPosX write SetPosX default 10;
    property LinePosition: TJppVerticalLinePosition read FLinePosition write SetLinePosition default vlpFromLeft;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
  end;
  {$endregion}

  {$region ' --------------- TJppPanelVerticalLines - collection ---------------- '}
  TJppPanelVerticalLines = class(TCollection)
  private
    FOwner: TComponent;
    FOnChange: TNotifyEvent;
    procedure SetOnChange(const Value: TNotifyEvent);
    function GetItem(Index: Integer): TJppPanelVerticalLine;
    procedure SetItem(Index: Integer; const Value: TJppPanelVerticalLine);
  protected
    function GetOwner: TPersistent; override;
    //procedure Update(Item: TJppPanelVerticalLine);
    procedure PropsChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent);
    function Add: TJppPanelVerticalLine;
    procedure Delete(Index: Integer);
    function Insert(Index: Integer): TJppPanelVerticalLine;
    property Items[Index: Integer]: TJppPanelVerticalLine read GetItem write SetItem; default;
  published
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
  end;
  {$endregion}

  {$region ' ------------ TJppPanelBorder -------------- '}
  TJppPanelBorder = class(TPersistent)
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
    procedure Assign(Border: TJppPanelBorder); reintroduce;
  protected
    procedure PropsChanged(Sender: TObject);
  published
    property Pen: TPen read FPen write SetPen;
    property Visible: Boolean read FVisible write SetVisible default True;
    property Border3D: Boolean read FBorder3D write SetBorder3D default True;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
  end;
  {$endregion}

  {$region ' ------------ TJppPanelBorders -------------- '}
  TJppPanelBorders = class(TPersistent)
  private
    FLeft: TJppPanelBorder;
    FRight: TJppPanelBorder;
    FTop: TJppPanelBorder;
    FBottom: TJppPanelBorder;
    FOnChange: TNotifyEvent;
    procedure SetLeft(const Value: TJppPanelBorder);
    procedure SetRight(const Value: TJppPanelBorder);
    procedure SetTop(const Value: TJppPanelBorder);
    procedure SetBottom(const Value: TJppPanelBorder);
    procedure SetOnChange(const Value: TNotifyEvent);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(Borders: TJppPanelBorders); reintroduce;
  protected
    procedure PropsChanged(Sender: TObject);
  published
    property Left: TJppPanelBorder read FLeft write SetLeft;
    property Right: TJppPanelBorder read FRight write SetRight;
    property Top: TJppPanelBorder read FTop write SetTop;
    property Bottom: TJppPanelBorder read FBottom write SetBottom;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
  end;
  {$endregion}

  {$region ' ----------- TJppPanelAppearance ---------- '}
  TJppPanelAppearance = class(TPersistent)
  private
    FOwner: TComponent;
    FUpperGradient: TJppGradientEx;
    FBottomGradient: TJppGradientEx;
    FUpperGradientPercent: Byte;
    FBorders: TJppPanelBorders;
    FOnChange: TNotifyEvent;
    FBorderToGradientMargin: integer;
    FBackgroundColor: TColor;
    FDrawGradient: Boolean;
    FDrawBorder: Boolean;
//    FVerticalLines: TJppPanelVerticalLines;
    procedure SetUpperGradient(const Value: TJppGradientEx);
    procedure SetBottomGradient(const Value: TJppGradientEx);
    procedure SetUpperGradientPercent(const Value: Byte);
    procedure SetBorders(const Value: TJppPanelBorders);
    procedure SetOnChange(const Value: TNotifyEvent);
    procedure SetBorderToGradientMargin(const Value: integer);
    procedure SetBackgroundColor(const Value: TColor);
    procedure SetDrawGradient(const Value: Boolean);
    procedure SetDrawBorder(const Value: Boolean);
//    procedure SetVerticalLines(const Value: TJppPanelVerticalLines);
  protected
    procedure PropsChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(Source: TJppPanelAppearance; bApplyGradientParams: Boolean = True; bApplyBordersParams: Boolean = True); reintroduce;
  published
    property UpperGradient: TJppGradientEx read FUpperGradient write SetUpperGradient;
    property BottomGradient: TJppGradientEx read FBottomGradient write SetBottomGradient;
    property UpperGradientPercent: Byte read FUpperGradientPercent write SetUpperGradientPercent default 50;
    property Borders: TJppPanelBorders read FBorders write SetBorders;
    property BorderToGradientMargin: integer read FBorderToGradientMargin write SetBorderToGradientMargin default 0;
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor default clBtnFace;
    property DrawGradient: Boolean read FDrawGradient write SetDrawGradient default True;
    property DrawBorder: Boolean read FDrawBorder write SetDrawBorder default True;
//    property VerticalLines: TJppPanelVerticalLines read FVerticalLines write SetVerticalLines;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
  end;
  {$endregion}

  {$region ' --------------------------- TJppBasePanel ---------------------------------- '}
  {$IFDEF MSWINDOWS}TProcOnOnDragDropFiles = procedure (Sender: TObject; var msg: TWMDropFiles) of object;{$ENDIF}

  TJppBasePanel = class(TCustomPanel)
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

  {$region ' --------------------------------- TJppCustomPanel ---------------------------------------- '}
  TJppCustomPanel = class(TJppBasePanel)
  private
    FOnGradientChange: TNotifyEvent;
    FAppearance: TJppPanelAppearance;
    FTagExt: TJppTagExt;
    FVerticalLines: TJppPanelVerticalLines;
    FHorizontalLines: TJppPanelHorizontalLines;
    FCaptions: TJppPanelCaptions;
    FHorizontalBars: TJppPanelHorizontalBars;
    procedure SetOnGradientChange(const Value: TNotifyEvent);
    procedure SetAppearance(const Value: TJppPanelAppearance);
    procedure SetTagExt(const Value: TJppTagExt);
    procedure SetVerticalLines(const Value: TJppPanelVerticalLines);
    procedure SetHorizontalLines(const Value: TJppPanelHorizontalLines);
    procedure SetCaptions(const Value: TJppPanelCaptions);
    procedure SetHorizontalBars(const Value: TJppPanelHorizontalBars);
    {$IFDEF DCC}procedure WMMouseWheel(var Message: TWMMouseWheel); message WM_MOUSEWHEEL;{$ENDIF}
  protected
    procedure DrawBackground(ARect: TRect); override;
    procedure DrawBorders(ARect: TRect);
    procedure DrawVerticalLines(ARect: TRect);
    procedure DrawHorizontalLines(ARect: TRect);
    procedure DrawCaptions(ARect: TRect);
    procedure DrawHorizontalBars(ARect: TRect);
    procedure PropsChanged(Sender: TObject);
    procedure GradientChanged(Sender: TObject);
    property OnGradientChange: TNotifyEvent read FOnGradientChange write SetOnGradientChange;
    property Appearance: TJppPanelAppearance read FAppearance write SetAppearance;
    procedure Paint; override;
    procedure AdjustClientRect(var Rect: TRect); override;
    procedure GetHDeltas(var dxLeft, dxRight: integer);
    procedure GetVDeltas(var dxTop, dxBottom: integer);
    procedure GetDeltas(var dxLeft, dxRight, dxTop, dxBottom: integer);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Canvas;
    property DockManager;
    property TagExt: TJppTagExt read FTagExt write SetTagExt;

  published
    property VerticalLines: TJppPanelVerticalLines read FVerticalLines write SetVerticalLines;
    property HorizontalLines: TJppPanelHorizontalLines read FHorizontalLines write SetHorizontalLines;
    property Captions: TJppPanelCaptions read FCaptions write SetCaptions;
    property HorizontalBars: TJppPanelHorizontalBars read FHorizontalBars write SetHorizontalBars;
  end;
  {$endregion}

  {$region ' ------------------------ TJppPanel ------------------------------- '}
  TJppPanel = class(TJppCustomPanel)
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
    {$IFDEF DCC}property BevelKind;{$ENDIF}
    property BiDiMode;
    property BorderWidth;
    property BorderStyle;
//    property Caption;
//    property Color;
    property Constraints;
    {$IFDEF DCC}property Ctl3D;{$ENDIF}
    property UseDockManager default True;
    property DockSite;
    {$IFDEF DELPHI2009_OR_ABOVE} property DoubleBuffered; {$ENDIF}
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FullRepaint;
    property Font;
    {$IFDEF DCC}property Locked;{$ENDIF}
    {$IFDEF DELPHI2009_OR_ABOVE} property Padding; {$ENDIF}
    property ParentBiDiMode;
    property ParentBackground default false;
//    property ParentColor;
    {$IFDEF DCC}property ParentCtl3D;{$ENDIF}
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    {$IFDEF DELPHI2009_OR_ABOVE} property ShowCaption; {$ENDIF}
    property ShowHint;
    property TabOrder;
    property TabStop;
    //property VerticalAlignment;  -> replaced by Layout property ...
    property Visible;
    {$IFDEF DCC}property OnCanResize;{$ENDIF}
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
    {$IFDEF DCC}
    {$IF RTLVersion > 23}
    property StyleElements;
    {$IFEND}
    property Touch;
    {$ENDIF}
    property DoubleBuffered;
    {$IFDEF FPC}
    property BorderSpacing;
    property ChildSizing;
    property OnGetDockCaption;
    {$ENDIF}
  end;
  {$endregion}


{$IFDEF DCC}
{$IF RTLVersion >= 24.0 }
  TJppPanelStyleHook = class(TStyleHook)
  strict protected
    //procedure DrawPanel(ACanvas: TCanvas; AMouseInControl: Boolean); override;
  end;
{$IFEND}
{$ENDIF}




implementation





{$region ' ---------------------------------------- TJppBasePanel ----------------------------------------------------- '}
constructor TJppBasePanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  //BevelInner := bvNone;
  BevelOuter := bvNone;
  FLayout := tlCenter;
  FWordWrap := False;
end;

destructor TJppBasePanel.Destroy;
begin
  inherited Destroy;
end;

procedure TJppBasePanel.Loaded;
begin
  Inherited;
end;

procedure TJppBasePanel.PropsChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TJppBasePanel.AdjustClientRect(var Rect: TRect);
begin
  inherited AdjustClientRect(Rect);
end;


  {$region ' --------------------------- PAINT & DRAW ------------------------------------ '}
procedure TJppBasePanel.Paint;
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

procedure TJppBasePanel.DrawBackground(ARect: TRect);
begin
  {$IFDEF DCC}
  if (not StyleServices.Enabled) or not ParentBackground
  then
  begin
    //Canvas.Brush.Color := Color;
    //Canvas.FillRect(ARect);
  end;
  {$ENDIF}
end;

procedure TJppBasePanel.DrawCaption(aRect: TRect);
begin
//
end;

  {$endregion}

  {$region ' ------------------- reszta ------------------------------- '}

procedure TJppBasePanel.SetLayout(const Value: TTextLayout);
begin
  FLayout := Value;
  Invalidate;
end;


procedure TJppBasePanel.SetWordWrap(const Value: Boolean);
begin
  FWordWrap := Value;
  Invalidate;
end;

procedure TJppBasePanel.VisibleChanging;
begin
  inherited;
  if Assigned(FOnVisibleChanging) then FOnVisibleChanging(Self);
end;

{$IFDEF MSWINDOWS}
procedure TJppBasePanel.CMDropFiles(var msg: TWMDropFiles);
begin
  if Assigned(FOnDragDropFiles) then FOnDragDropFiles(Self, msg);
end;
{$ENDIF}

procedure TJppBasePanel.CMMouseEnter(var Msg: TMessage);
begin
  inherited;
  MouseEnter(Self);
end;

procedure TJppBasePanel.CMMouseLeave(var Msg: TMessage);
begin
  inherited;
  MouseLeave(Self);
end;

procedure TJppBasePanel.MouseEnter(Sender: TObject);
begin
  if Assigned(FOnMouseEnter) then FOnMouseEnter(Self);
end;

procedure TJppBasePanel.MouseLeave(Sender: TObject);
begin
  if Assigned(FonMouseLeave) then FOnMouseLeave(Self);
end;

procedure TJppBasePanel.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
end;

procedure TJppBasePanel.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
end;

procedure TJppBasePanel.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited;
end;
  {$endregion}


{$endregion TJppBasePanel}

{$region ' -------------------------------------------------------- TJppCustomPanel ------------------------------------------------------------ '}
constructor TJppCustomPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ParentBackground := False;  // TJvGifAnimator needs this!

  FTagExt := TJppTagExt.Create(Self);
  FAppearance := TJppPanelAppearance.Create(Self);
  FAppearance.OnChange := {$IFDEF FPC} @ {$ENDIF}PropsChanged;

  FAppearance.UpperGradient.OnChange := {$IFDEF FPC} @ {$ENDIF}GradientChanged;
  FAppearance.BottomGradient.OnChange := {$IFDEF FPC} @ {$ENDIF}GradientChanged;

  FVerticalLines := TJppPanelVerticalLines.Create(Self);
  FVerticalLines.OnChange := {$IFDEF FPC} @ {$ENDIF}PropsChanged;

  FHorizontalLines := TJppPanelHorizontalLines.Create(Self);
  FHorizontalLines.OnChange := {$IFDEF FPC} @ {$ENDIF}PropsChanged;

  FCaptions := TJppPanelCaptions.Create(Self);
  FCaptions.OnChange := {$IFDEF FPC} @ {$ENDIF}PropsChanged;

  FHorizontalBars := TJppPanelHorizontalBars.Create(Self);
  FHorizontalBars.OnChange := {$IFDEF FPC} @ {$ENDIF}PropsChanged;
end;

destructor TJppCustomPanel.Destroy;
begin
  FTagExt.Free;
  FAppearance.Free;
  FVerticalLines.Free;
  FHorizontalLines.Free;
  FCaptions.Free;
  FHorizontalBars.Free;
  inherited Destroy;
end;

procedure TJppCustomPanel.PropsChanged(Sender: TObject);
begin
  if csLoading in ComponentState then Exit;
  Invalidate;
end;

procedure TJppCustomPanel.GetDeltas(var dxLeft, dxRight, dxTop, dxBottom: integer);
begin
  GetHDeltas(dxLeft, dxRight);
  GetVDeltas(dxTop, dxBottom);
end;

procedure TJppCustomPanel.GetHDeltas(var dxLeft, dxRight: integer);
begin
  if Appearance.Borders.Left.Visible and (Appearance.Borders.Left.Pen.Width > 0) then dxLeft := Appearance.Borders.Left.Pen.Width else dxLeft := 0;
  if Appearance.Borders.Right.Visible and (Appearance.Borders.Right.Pen.Width > 0) then dxRight := Appearance.Borders.Right.Pen.Width else dxRight := 0;
end;

procedure TJppCustomPanel.GetVDeltas(var dxTop, dxBottom: integer);
begin
  if Appearance.Borders.Top.Visible and (Appearance.Borders.Top.Pen.Width > 0) then dxTop := Appearance.Borders.Top.Pen.Width else dxTop := 0;
  if Appearance.Borders.Bottom.Visible and (Appearance.Borders.Bottom.Pen.Width > 0) then dxBottom := Appearance.Borders.Bottom.Pen.Width else dxBottom := 0;
end;

procedure TJppCustomPanel.GradientChanged(Sender: TObject);
begin
  if (csDesigning in ComponentState) and (not (csLoading in ComponentState)) and ParentBackground then ParentBackground := False;
  Invalidate;
end;

procedure TJppCustomPanel.SetAppearance(const Value: TJppPanelAppearance);
begin
  FAppearance := Value;
  Invalidate;
end;

procedure TJppCustomPanel.SetCaptions(const Value: TJppPanelCaptions);
begin
  FCaptions := Value;
  PropsChanged(Self);
end;

procedure TJppCustomPanel.SetHorizontalBars(const Value: TJppPanelHorizontalBars);
begin
  FHorizontalBars := Value;
  PropsChanged(Self);
end;

procedure TJppCustomPanel.SetHorizontalLines(const Value: TJppPanelHorizontalLines);
begin
  FHorizontalLines := Value;
  Invalidate;
end;

procedure TJppCustomPanel.SetOnGradientChange(const Value: TNotifyEvent);
begin
  FOnGradientChange := Value;
end;


procedure TJppCustomPanel.SetTagExt(const Value: TJppTagExt);
begin
  FTagExt := Value;
end;

procedure TJppCustomPanel.SetVerticalLines(const Value: TJppPanelVerticalLines);
begin
  FVerticalLines := Value;
  Invalidate;
end;

{$IFDEF DCC}
procedure TJppCustomPanel.WMMouseWheel(var Message: TWMMouseWheel);
var
  b: Boolean;
  p: TPoint;
begin
  inherited;
  p.X := Message.XPos;
  p.Y := Message.YPos;
  if Assigned(OnMouseWheel) then OnMouseWheel(Self, KeysToShiftState(Message.Keys), Message.WheelDelta, p, b);
end;
{$ENDIF}

procedure TJppCustomPanel.AdjustClientRect(var Rect: TRect);
begin
  inherited AdjustClientRect(Rect);
end;

  {$region ' ---------------- TJppCustomPanel.Paint --------------------------- '}
procedure TJppCustomPanel.Paint;
begin
  inherited;

   exit;

end;
  {$endregion}

  {$region ' ---------------- TJppCustomPanel.DrawBackground ---------------------- '}
procedure TJppCustomPanel.DrawBackground(ARect: TRect);
var
  R: TRect;
  xBottomGradientTop: integer;
  {$IFDEF DCC}bVclStyle: Boolean;{$ENDIF}
  Border: TJppPanelBorder;
  xBottom: integer;

  {$IFDEF DCC}
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

  if ParentBackground then
  begin
    DrawHorizontalBars(ARect);
    DrawVerticalLines(ARect);
    DrawHorizontalLines(ARect);
    DrawCaptions(ARect);
    DrawBorders(ARect);
    Exit;
  end;

  {$IFDEF DCC}bVclStyle := Assigned(TStyleManager.ActiveStyle) and (TStyleManager.ActiveStyle.Name <> 'Windows');{$ENDIF}

  {$IFDEF DCC}if (not bVclStyle) {$IF RTLVersion > 23} or (bVclStyle and (not (seClient in StyleElements))) {$IFEND} then{$ENDIF}
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
      R.Bottom := R.Top + Round((Appearance.UpperGradientPercent * R.Height) / 100);
      xBottomGradientTop := R.Bottom;
      Appearance.UpperGradient.Draw(Canvas, R);

      R.Top := xBottomGradientTop;
      R.Bottom := xBottom;
      Appearance.BottomGradient.Draw(Canvas, R);

    end;


//    DrawHorizontalBars(ARect);
//    DrawVerticalLines(ARect);
//    DrawHorizontalLines(ARect);
//    DrawCaptions(ARect);
//    DrawBorders(ARect);

  end{$IFDEF FPC};{$ENDIF}

  {$IFDEF DCC}
  else

  // VCL Style
  begin

    LStyle := StyleServices;

    Rect := GetClientRect;

    BaseColor := Color;
    BaseTopColor := clBtnHighlight;
    BaseBottomColor := clBtnShadow;

    if LStyle.Enabled {$IF RTLVersion > 23} and (seClient in StyleElements) {$IFEND} then
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


//    DrawHorizontalBars(ARect);
//    DrawVerticalLines(ARect);
//    DrawHorizontalLines(ARect);
//    DrawCaptions(ARect);
//    DrawBorders(ARect);
  end; // VCL Style

  {$ENDIF}


  DrawHorizontalBars(ARect);
  DrawVerticalLines(ARect);
  DrawHorizontalLines(ARect);
  DrawCaptions(ARect);
  DrawBorders(ARect);

end;
  {$endregion DrawBackground}

  {$region ' ---------------- TJppCustomPanel.DrawBorders ---------------- '}
procedure TJppCustomPanel.DrawBorders(ARect: TRect);
begin

  if not Appearance.DrawBorder then Exit;
  Canvas.Brush.Style := bsClear;

  if Appearance.Borders.Left.Visible then DrawLeftBorder(Canvas, ARect, Appearance.Borders.Left.Pen, Appearance.Borders.Left.Border3D);
  if Appearance.Borders.Right.Visible then DrawRightBorder(Canvas, ARect, Appearance.Borders.Right.Pen, Appearance.Borders.Right.Border3D);

  if Appearance.Borders.Top.Visible then DrawTopBorder(Canvas, ARect, Appearance.Borders.Top.Pen, Appearance.Borders.Top.Border3D);
  if Appearance.Borders.Bottom.Visible then DrawBottomBorder(Canvas, ARect, Appearance.Borders.Bottom.Pen, Appearance.Borders.Bottom.Border3D);
end;
  {$endregion DrawBorders}

  {$region ' ---------------- TJppCustomPanel.DrawCaptions ------------------- '}
procedure TJppCustomPanel.DrawCaptions(ARect: TRect);
var
  i, dxLeft, dxRight, dxTop, dxBottom: integer;
  ACaption: TJppPanelCaption;
  xTop, xLeft: integer;
  tw, th: integer;
begin

  GetDeltas(dxLeft, dxRight, dxTop, dxBottom);

  with Canvas do
  begin

    Brush.Style := bsClear;

    for i := 0 to Captions.Count - 1 do
    begin

      ACaption := Captions[i];
      if not ACaption.Visible then Continue;
      if Trim(ACaption.Text) = '' then Continue;

      Font.Assign(ACaption.Font);
      tw := TextWidth(ACaption.Text);
      th := TextHeight(ACaption.Text);

      case ACaption.HorizontalPosition of
        thpFromLeft: xLeft := ARect.Left + ACaption.PosX + dxLeft + ACaption.MarginX;
        thpFromRight: xLeft := ARect.Right - ACaption.PosX - dxRight - ACaption.MarginX - tw;
      else
        xLeft := ((ARect.Right - ARect.Left) div 2) - (tw div 2) + dxLeft - dxRight;
      end;


      case ACaption.VerticalPosition of
        tvpFromTop: xTop := ARect.Top + ACaption.PosY + dxTop + ACaption.MarginY;
        tvpFromBottom: xTop := ARect.Bottom - ACaption.PosY - dxBottom - ACaption.MarginY - th;
      else
        xTop := ((ARect.Bottom - ARect.Top) div 2) - (th div 2) + dxTop - dxBottom;
      end;

      TextOut(xLeft, xTop, ACaption.Text);

    end;

  end;
end;
  {$endregion DrawCaptions}

  {$region ' ---------------- TJppCustomPanel.DrawHorizontalBars -------------------- '}
procedure TJppCustomPanel.DrawHorizontalBars(ARect: TRect);
var
  i, dxLeft, dxRight, dxTop, dxBottom: integer;
  Bar: TJppPanelHorizontalBar;
  xLeft, xTop, xRight, xBottom: integer;
  R: TRect;
begin
  GetDeltas(dxLeft, dxRight, dxTop, dxBottom);

  with Canvas do
  begin

    for i := 0 to HorizontalBars.Count - 1 do
    begin
      Bar := HorizontalBars[i];
      if (not Bar.Visible) or (Bar.Height <= 0) then Continue;


      xLeft := ARect.Left + dxLeft + Bar.LeftMargin;
      xRight := ARect.Right - dxRight - Bar.RightMargin;

      case Bar.BarPosition of
        hbpCenter: xTop := ((ARect.Bottom - ARect.Top) div 2) - (Bar.Height div 2) + dxTop - dxBottom;
        hbpFromBottom: xTop := ARect.Bottom - dxBottom - Bar.PosY - Bar.Height;
        hbpPercentFromTop: xTop := ARect.Top + dxTop + PercentOf(Bar.PosY, ARect.Height - dxTop - dxBottom);
        hbpPercentFromBottom: xTop := ARect.Bottom - dxBottom - PercentOf(Bar.PosY, ARect.Height - dxTop - dxBottom) - Bar.Height;
      else xTop := ARect.Top + dxTop + Bar.PosY; // hbpFromTop
      end;

      xBottom := xTop + Bar.Height;


      R := Rect(xLeft, xTop, xRight, xBottom);

      Brush.Style := bsSolid;
      Brush.Color := Bar.BackgroundColor;
      FillRect(R);
      if Bar.DrawGradient then Bar.Gradient.Draw(Canvas, R);

      if Bar.Border.Width > 0 then
      begin
        Brush.Style := bsClear;
        Pen.Assign(Bar.Border);
        JppFrame3D(Canvas, R, Bar.Border.Color, Bar.Border.Width);
      end;



    end;

  end;
end;
  {$endregion DrawHorizontalBars}

  {$region ' ---------------- TJppCustomPanel.DrawHorizontalLines -------------------- '}
procedure TJppCustomPanel.DrawHorizontalLines(ARect: TRect);
var
  i, dxLeft, dxRight, dxTop, dxBottom: integer;
  ALine: TJppPanelHorizontalLine;
  xTop, xLeft, xRight: integer;
begin
  GetDeltas(dxLeft, dxRight, dxTop, dxBottom);

  with Canvas do
  begin

    for i := 0 to HorizontalLines.Count - 1 do
    begin
      ALine := HorizontalLines[i];
      if (not ALine.Visible) or (ALine.Pen.Style = psClear) or (ALine.Pen.Width <= 0) then Continue;

      Pen.Assign(ALine.Pen);

      xLeft := ARect.Left + dxLeft + ALine.LeftMargin;
      xRight := ARect.Right - dxRight - ALine.RightMargin;

      case ALine.LinePosition of
        hlpFromTop: xTop := ALine.PosY + dxTop;
        hlpFromBottom: xTop := ARect.Bottom - ALine.PosY - dxBottom;
      else
        xTop := (ARect.Height div 2) + dxTop - dxBottom;
      end;

      MoveTo(xLeft, xTop);
      LineTo(xRight, xTop);

    end;

  end;
end;
  {$endregion DrawHorizontalLines}

  {$region ' ---------------- TJppCustomPanel.DrawVerticalLines ---------------- '}
procedure TJppCustomPanel.DrawVerticalLines(ARect: TRect);
var
  i, dxLeft, dxRight, dxTop, dxBottom: integer;
  ALine: TJppPanelVerticalLine;
  xTop, xLeft, xBottom: integer;
begin
  GetDeltas(dxLeft, dxRight, dxTop, dxBottom);

  with Canvas do
  begin

    for i := 0 to VerticalLines.Count - 1 do
    begin
      ALine := VerticalLines[i];
      if (not ALine.Visible) or (ALine.Pen.Style = psClear) or (ALine.Pen.Width <= 0) then Continue;

      Pen.Assign(ALine.Pen);
      xTop := ARect.Top + dxTop + ALine.TopMargin;
      xBottom := ARect.Bottom - dxBottom - ALine.BottomMargin;

      case ALine.LinePosition of
        vlpFromLeft: xLeft := ALine.PosX + dxLeft;
        vlpFromRight: xLeft := ARect.Right - ALine.PosX - dxRight;
      else
        xLeft := (ARect.Width div 2) + dxLeft - dxRight;
      end;

      MoveTo(xLeft, xTop);
      LineTo(xLeft, xBottom);

    end;

  end;
end;
  {$endregion DrawVerticalLines}

{$endregion TJppCustomPanel}

{$region ' ------------------------------------- TJppPanelAppearance ------------------------------------------- '}
constructor TJppPanelAppearance.Create(AOwner: TComponent);
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

  FBorders := TJppPanelBorders.Create(AOwner);
  FBorders.OnChange := {$IFDEF FPC} @ {$ENDIF}PropsChanged;

  FBorderToGradientMargin := 0;
  FDrawGradient := True;
  FBackgroundColor := clBtnFace;
  FDrawBorder := True;

//  FVerticalLines := TJppPanelVerticalLines.Create(FOwner);
//  FVerticalLines.OnChange := PropsChanged;
end;

destructor TJppPanelAppearance.Destroy;
begin
  FUpperGradient.Free;
  FBottomGradient.Free;
  FBorders.Free;
//  FVerticalLines.Free;
  inherited;
end;


procedure TJppPanelAppearance.Assign(Source: TJppPanelAppearance; bApplyGradientParams: Boolean = True; bApplyBordersParams: Boolean = True);
begin
  if bApplyGradientParams then
  begin
    FDrawGradient := Source.DrawGradient;
    FUpperGradient.Assign(Source.UpperGradient);
    FBottomGradient.Assign(Source.BottomGradient);
    FUpperGradientPercent := Source.UpperGradientPercent;
  end;

  FBorderToGradientMargin := Source.BorderToGradientMargin;
  FBackgroundColor := Source.BackgroundColor;

  if bApplyBordersParams then
  begin
    FBorders.Assign(Source.Borders);
    FDrawBorder := Source.DrawBorder;
  end;

  PropsChanged(Self);
end;

procedure TJppPanelAppearance.PropsChanged(Sender: TObject);
begin
  if Assigned(OnChange) then OnChange(Self);
end;

procedure TJppPanelAppearance.SetBackgroundColor(const Value: TColor);
begin
  FBackgroundColor := Value;
  PropsChanged(Self);
end;

procedure TJppPanelAppearance.SetBorders(const Value: TJppPanelBorders);
begin
  FBorders := Value;
  PropsChanged(Self);
end;

procedure TJppPanelAppearance.SetBorderToGradientMargin(const Value: integer);
begin
  FBorderToGradientMargin := Value;
  PropsChanged(Self);
end;

procedure TJppPanelAppearance.SetBottomGradient(const Value: TJppGradientEx);
begin
  FBottomGradient := Value;
  PropsChanged(Self);
end;

procedure TJppPanelAppearance.SetDrawBorder(const Value: Boolean);
begin
  FDrawBorder := Value;
  PropsChanged(Self);
end;

procedure TJppPanelAppearance.SetDrawGradient(const Value: Boolean);
begin
  FDrawGradient := Value;
  PropsChanged(Self);
end;

procedure TJppPanelAppearance.SetOnChange(const Value: TNotifyEvent);
begin
  FOnChange := Value;
end;

procedure TJppPanelAppearance.SetUpperGradient(const Value: TJppGradientEx);
begin
  FUpperGradient := Value;
  PropsChanged(Self);
end;

procedure TJppPanelAppearance.SetUpperGradientPercent(const Value: Byte);
begin
  FUpperGradientPercent := Value;
  FBottomGradient.OnChange(Self);
  FUpperGradient.OnChange(Self);
  PropsChanged(Self);
end;

{$endregion TJppPanelAppearance}

{$region ' ------------------------------------- TJppPanelBorder ------------------------------------ '}
constructor TJppPanelBorder.Create(AOwner: TComponent);
begin
  inherited Create;
  FPen := TPen.Create;
  FVisible := True;
  FPen.OnChange := {$IFDEF FPC} @ {$ENDIF}PropsChanged;
  FBorder3D := True;
end;

destructor TJppPanelBorder.Destroy;
begin
  FPen.Free;
  inherited;
end;

procedure TJppPanelBorder.Assign(Border: TJppPanelBorder);
begin
  FPen.Assign(Border.Pen);
  FPen.OnChange := {$IFDEF FPC} @ {$ENDIF}PropsChanged;
  FVisible := Border.Visible;
  FBorder3D := Border.Border3D;
end;

procedure TJppPanelBorder.PropsChanged(Sender: TObject);
begin
  if Assigned(OnChange) then OnChange(Self);
end;

procedure TJppPanelBorder.SetBorder3D(const Value: Boolean);
begin
  FBorder3D := Value;
  PropsChanged(Self);
end;

procedure TJppPanelBorder.SetOnChange(const Value: TNotifyEvent);
begin
  FOnChange := Value;
end;

procedure TJppPanelBorder.SetPen(const Value: TPen);
begin
  FPen := Value;
  PropsChanged(Self);
end;

procedure TJppPanelBorder.SetVisible(const Value: Boolean);
begin
  FVisible := Value;
  PropsChanged(Self);
end;

{$endregion}

{$region ' ----------------------------------------- TJppPanelBorders ------------------------------------------ '}
constructor TJppPanelBorders.Create(AOwner: TComponent);
begin
  inherited Create;
  FLeft := TJppPanelBorder.Create(AOwner);
  FRight := TJppPanelBorder.Create(AOwner);
  FTop := TJppPanelBorder.Create(AOwner);
  FBottom := TJppPanelBorder.Create(AOwner);

  FLeft.OnChange := {$IFDEF FPC} @ {$ENDIF}PropsChanged;
  FRight.OnChange := {$IFDEF FPC} @ {$ENDIF}PropsChanged;
  FTop.OnChange := {$IFDEF FPC} @ {$ENDIF}PropsChanged;
  FBottom.OnChange := {$IFDEF FPC} @ {$ENDIF}PropsChanged;

  FLeft.Pen.Color := clBtnHighlight;
  FRight.Pen.Color := clBtnShadow;
  FTop.Pen.Color := clBtnHighlight;
  FBottom.Pen.Color := clBtnShadow;
end;

destructor TJppPanelBorders.Destroy;
begin
  FLeft.Free;
  FRight.Free;
  FTop.Free;
  FBottom.Free;
  inherited;
end;

procedure TJppPanelBorders.Assign(Borders: TJppPanelBorders);
begin
  FLeft.Assign(Borders.Left);
  FRight.Assign(Borders.Right);
  FTop.Assign(Borders.Top);
  FBottom.Assign(Borders.Bottom);
end;

procedure TJppPanelBorders.PropsChanged(Sender: TObject);
begin
  if Assigned(OnChange) then OnChange(Self);
end;

procedure TJppPanelBorders.SetBottom(const Value: TJppPanelBorder);
begin
  FBottom := Value;
  PropsChanged(Self);
end;

procedure TJppPanelBorders.SetLeft(const Value: TJppPanelBorder);
begin
  FLeft := Value;
  PropsChanged(Self);
end;

procedure TJppPanelBorders.SetOnChange(const Value: TNotifyEvent);
begin
  FOnChange := Value;
end;

procedure TJppPanelBorders.SetRight(const Value: TJppPanelBorder);
begin
  FRight := Value;
  PropsChanged(Self);
end;

procedure TJppPanelBorders.SetTop(const Value: TJppPanelBorder);
begin
  FTop := Value;
  PropsChanged(Self);
end;

{$endregion}

{$region ' -------------------------------- TJppPanelVerticalLine - collection item ----------------------------------- '}
constructor TJppPanelVerticalLine.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FPen := TPen.Create;
  FPen.OnChange := {$IFDEF FPC} @ {$ENDIF}PropsChanged;
  FTopMargin := 0;
  FBottomMargin := 0;
  FVisible := True;
  FPosX := 10;
  FLinePosition := vlpFromLeft;
end;

destructor TJppPanelVerticalLine.Destroy;
begin
  FPen.Free;
  inherited;
end;

procedure TJppPanelVerticalLine.Assign(Source: TPersistent);
begin
  if Source is TJppPanelVerticalLine then
  begin
    FPen := TJppPanelVerticalLine(Source).Pen;
    FTopMargin := TJppPanelVerticalLine(Source).TopMargin;
    FBottomMargin := TJppPanelVerticalLine(Source).BottomMargin;
    FVisible := TJppPanelVerticalLine(Source).Visible;
    FOnChange := TJppPanelVerticalLine(Source).OnChange;
    FPosX := TJppPanelVerticalLine(Source).PosX;
    FLinePosition := TJppPanelVerticalLine(Source).LinePosition;
    PropsChanged(Self);
  end
  else inherited;
end;

procedure TJppPanelVerticalLine.PropsChanged(Sender: TObject);
begin
  if Assigned(OnChange) then OnChange(Self);
end;

procedure TJppPanelVerticalLine.SetBottomMargin(const Value: integer);
begin
  FBottomMargin := Value;
  PropsChanged(Self);
end;

procedure TJppPanelVerticalLine.SetLinePosition(const Value: TJppVerticalLinePosition);
begin
  FLinePosition := Value;
  PropsChanged(Self);
end;

procedure TJppPanelVerticalLine.SetOnChange(const Value: TNotifyEvent);
begin
  FOnChange := Value;
end;

procedure TJppPanelVerticalLine.SetPen(const Value: TPen);
begin
  FPen := Value;
  PropsChanged(Self);
end;

procedure TJppPanelVerticalLine.SetPosX(const Value: integer);
begin
  FPosX := Value;
  PropsChanged(Self);
end;

procedure TJppPanelVerticalLine.SetTopMargin(const Value: integer);
begin
  FTopMargin := Value;
  PropsChanged(Self);
end;

procedure TJppPanelVerticalLine.SetVisible(const Value: Boolean);
begin
  FVisible := Value;
  PropsChanged(Self);
end;

{$endregion TJppPanelVerticalLine}

{$region ' -------------------------------- TJppPanelVerticalLines - collection --------------------------------------- '}
function TJppPanelVerticalLines.Add: TJppPanelVerticalLine;
begin
  Result := TJppPanelVerticalLine(inherited Add);
  Result.OnChange := {$IFDEF FPC} @ {$ENDIF}PropsChanged;
  PropsChanged(Self);
end;

function TJppPanelVerticalLines.Insert(Index: Integer): TJppPanelVerticalLine;
begin
  Result := TJppPanelVerticalLine(inherited Insert(Index));
  Result.OnChange := {$IFDEF FPC} @ {$ENDIF}PropsChanged;
  PropsChanged(Self);
end;

constructor TJppPanelVerticalLines.Create(AOwner: TComponent);
begin
  inherited Create(TJppPanelVerticalLine);
  FOwner := AOwner;
end;


procedure TJppPanelVerticalLines.Delete(Index: Integer);
begin
  inherited Delete(Index);
  PropsChanged(Self);
end;

function TJppPanelVerticalLines.GetItem(Index: Integer): TJppPanelVerticalLine;
begin
  Result := TJppPanelVerticalLine(inherited GetItem(Index));
end;

function TJppPanelVerticalLines.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TJppPanelVerticalLines.PropsChanged(Sender: TObject);
begin
  if Assigned(OnChange) then OnChange(Self);
end;

procedure TJppPanelVerticalLines.SetItem(Index: Integer; const Value: TJppPanelVerticalLine);
begin
  inherited SetItem(Index, Value);
  PropsChanged(Self);
end;

procedure TJppPanelVerticalLines.SetOnChange(const Value: TNotifyEvent);
begin
  FOnChange := Value;
end;
{$endregion TJppPanelVerticalLines}

{$region ' ----------------------------------- TJppPanelHorizontalLine - collection item ------------------------------------ '}
constructor TJppPanelHorizontalLine.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FPen := TPen.Create;
  FPen.OnChange := {$IFDEF FPC} @ {$ENDIF}PropsChanged;
  FLeftMargin := 0;
  FRightMargin := 0;
  FVisible := True;
  FPosY := 10;
  FLinePosition := hlpFromTop;
end;

destructor TJppPanelHorizontalLine.Destroy;
begin
  FPen.Free;
  inherited;
end;

procedure TJppPanelHorizontalLine.Assign(Source: TPersistent);
begin
  if Source is TJppPanelHorizontalLine then
  begin
    FPen := TJppPanelHorizontalLine(Source).Pen;
    FLeftMargin := TJppPanelHorizontalLine(Source).LeftMargin;
    FRightMargin := TJppPanelHorizontalLine(Source).RightMargin;
    FVisible := TJppPanelHorizontalLine(Source).Visible;
    FOnChange := TJppPanelHorizontalLine(Source).OnChange;
    FPosY := TJppPanelHorizontalLine(Source).PosY;
    FLinePosition := TJppPanelHorizontalLine(Source).LinePosition;
    PropsChanged(Self);
  end
  else inherited;
end;

procedure TJppPanelHorizontalLine.PropsChanged(Sender: TObject);
begin
  if Assigned(OnChange) then OnChange(Self);
end;

procedure TJppPanelHorizontalLine.SetLeftMargin(const Value: integer);
begin
  FLeftMargin := Value;
  PropsChanged(Self);
end;

procedure TJppPanelHorizontalLine.SetLinePosition(const Value: TJppHorizontalLinePosition);
begin
  FLinePosition := Value;
  PropsChanged(Self);
end;

procedure TJppPanelHorizontalLine.SetOnChange(const Value: TNotifyEvent);
begin
  FOnChange := Value;
end;

procedure TJppPanelHorizontalLine.SetPen(const Value: TPen);
begin
  FPen := Value;
  PropsChanged(Self);
end;

procedure TJppPanelHorizontalLine.SetPosY(const Value: integer);
begin
  FPosY := Value;
  PropsChanged(Self);
end;

procedure TJppPanelHorizontalLine.SetRightMargin(const Value: integer);
begin
  FRightMargin := Value;
  PropsChanged(Self);
end;

procedure TJppPanelHorizontalLine.SetVisible(const Value: Boolean);
begin
  FVisible := Value;
  PropsChanged(Self);
end;

{$endregion}

{$region ' ---------------------------------- TJppPanelHorizontalLines - collection ----------------------------------------- '}
function TJppPanelHorizontalLines.Add: TJppPanelHorizontalLine;
begin
  Result := TJppPanelHorizontalLine(inherited Add);
  Result.OnChange := {$IFDEF FPC} @ {$ENDIF}PropsChanged;
  PropsChanged(Self);
end;

function TJppPanelHorizontalLines.Insert(Index: Integer): TJppPanelHorizontalLine;
begin
  Result := TJppPanelHorizontalLine(inherited Insert(Index));
  Result.OnChange := {$IFDEF FPC} @ {$ENDIF}PropsChanged;
  PropsChanged(Self);
end;

constructor TJppPanelHorizontalLines.Create(AOwner: TComponent);
begin
  inherited Create(TJppPanelHorizontalLine);
  FOwner := AOwner;
end;


procedure TJppPanelHorizontalLines.Delete(Index: Integer);
begin
  inherited Delete(Index);
  PropsChanged(Self);
end;

function TJppPanelHorizontalLines.GetItem(Index: Integer): TJppPanelHorizontalLine;
begin
  Result := TJppPanelHorizontalLine(inherited GetItem(Index));
end;

function TJppPanelHorizontalLines.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TJppPanelHorizontalLines.PropsChanged(Sender: TObject);
begin
  if Assigned(OnChange) then OnChange(Self);
end;

procedure TJppPanelHorizontalLines.SetItem(Index: Integer; const Value: TJppPanelHorizontalLine);
begin
  inherited SetItem(Index, Value);
  PropsChanged(Self);
end;

procedure TJppPanelHorizontalLines.SetOnChange(const Value: TNotifyEvent);
begin
  FOnChange := Value;
end;
{$endregion}

{$region ' ------------------------------------ TJppPanelCaption - collection item --------------------------------- '}
constructor TJppPanelCaption.Create(ACollection: TCollection);
begin
  inherited;
  FFont := TFont.Create;
  FFont.OnChange := {$IFDEF FPC} @ {$ENDIF}PropsChanged;
  FPosX := 10;
  FPosY := 10;
  FMarginX := 0;
  FMarginY := 0;
  FVerticalPosition := tvpFromTop;
  FHorizontalPosition := thpFromLeft;
  FVisible := True;
end;

destructor TJppPanelCaption.Destroy;
begin
  FFont.Free;
  inherited;
end;

procedure TJppPanelCaption.Assign(Source: TPersistent);
begin
  if Source is TJppPanelCaption then
  begin
    FFont.Assign(TJppPanelCaption(Source).Font);
    FFont.OnChange := {$IFDEF FPC} @ {$ENDIF}PropsChanged;
    FPosX := TJppPanelCaption(Source).PosX;
    FPosY := TJppPanelCaption(Source).PosY;
    FMarginX := TJppPanelCaption(Source).MarginX;
    FMarginY := TJppPanelCaption(Source).MarginY;
    FVerticalPosition := TJppPanelCaption(Source).VerticalPosition;
    FHorizontalPosition := TJppPanelCaption(Source).HorizontalPosition;
  end
  else inherited;
end;

procedure TJppPanelCaption.PropsChanged(Sender: TObject);
begin
  if Assigned(OnChange) then OnChange(Self);
end;

procedure TJppPanelCaption.SetFont(const Value: TFont);
begin
  FFont := Value;
  PropsChanged(Self);
end;

procedure TJppPanelCaption.SetHorizontalPosition(const Value: TJppTextHorizontalPosition);
begin
  FHorizontalPosition := Value;
  PropsChanged(Self);
end;

procedure TJppPanelCaption.SetMarginX(const Value: integer);
begin
  FMarginX := Value;
  PropsChanged(Self);
end;

procedure TJppPanelCaption.SetMarginY(const Value: integer);
begin
  FMarginY := Value;
  PropsChanged(Self);
end;

procedure TJppPanelCaption.SetOnChange(const Value: TNotifyEvent);
begin
  FOnChange := Value;
end;

procedure TJppPanelCaption.SetPosX(const Value: integer);
begin
  FPosX := Value;
  PropsChanged(Self);
end;

procedure TJppPanelCaption.SetPosY(const Value: integer);
begin
  FPosY := Value;
  PropsChanged(Self);
end;

procedure TJppPanelCaption.SetText(const Value: string);
begin
  FText := Value;
  PropsChanged(Self);
end;

procedure TJppPanelCaption.SetVerticalPosition(const Value: TJppTextVerticalPosition);
begin
  FVerticalPosition := Value;
  PropsChanged(Self);
end;

procedure TJppPanelCaption.SetVisible(const Value: Boolean);
begin
  FVisible := Value;
  PropsChanged(Self);
end;

{$endregion}

{$region ' ---------------------------------- TJppPanelCaptions - collection ----------------------------------------- '}
function TJppPanelCaptions.Add: TJppPanelCaption;
begin
  Result := TJppPanelCaption(inherited Add);
  Result.OnChange := {$IFDEF FPC} @ {$ENDIF}PropsChanged;
  PropsChanged(Self);
end;

function TJppPanelCaptions.Insert(Index: Integer): TJppPanelCaption;
begin
  Result := TJppPanelCaption(inherited Insert(Index));
  Result.OnChange := {$IFDEF FPC} @ {$ENDIF}PropsChanged;
  PropsChanged(Self);
end;

constructor TJppPanelCaptions.Create(AOwner: TComponent);
begin
  inherited Create(TJppPanelCaption);
  FOwner := AOwner;
end;


procedure TJppPanelCaptions.Delete(Index: Integer);
begin
  inherited Delete(Index);
  PropsChanged(Self);
end;

function TJppPanelCaptions.GetItem(Index: Integer): TJppPanelCaption;
begin
  Result := TJppPanelCaption(inherited GetItem(Index));
end;

function TJppPanelCaptions.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TJppPanelCaptions.PropsChanged(Sender: TObject);
begin
  if Assigned(OnChange) then OnChange(Self);
end;

procedure TJppPanelCaptions.SetItem(Index: Integer; const Value: TJppPanelCaption);
begin
  inherited SetItem(Index, Value);
  PropsChanged(Self);
end;

procedure TJppPanelCaptions.SetOnChange(const Value: TNotifyEvent);
begin
  FOnChange := Value;
end;
{$endregion}


{$region ' -------------------------- TJppPanelHorizontalBar - collection item ------------------------------- '}
constructor TJppPanelHorizontalBar.Create(ACollection: TCollection);
begin
  inherited;
  FBorder := TPen.Create;
  FBorder.OnChange := {$IFDEF FPC} @ {$ENDIF}PropsChanged;
  FHeight := 20;
  FPosY := 20;
  FBarPosition := hbpFromTop;
  FBackgroundColor := clBtnFace;
  FGradient := TJppGradientEx.Create(nil);
  FGradient.OnChange := {$IFDEF FPC} @ {$ENDIF}PropsChanged;
  FDrawGradient := True;
  FLeftMargin := 0;
  FRightMargin := 0;
  FVisible := True;
end;

destructor TJppPanelHorizontalBar.Destroy;
begin
  FBorder.Free;
  FGradient.Free;
  inherited;
end;

procedure TJppPanelHorizontalBar.PropsChanged(Sender: TObject);
begin
  if Assigned(OnChange) then OnChange(Self);
end;

procedure TJppPanelHorizontalBar.SetBackgroundColor(const Value: TColor);
begin
  FBackgroundColor := Value;
  PropsChanged(Self);
end;

procedure TJppPanelHorizontalBar.SetBarPosition(const Value: TJppHorizontalBarPosition);
begin
  FBarPosition := Value;
  PropsChanged(Self);
end;

procedure TJppPanelHorizontalBar.SetBorder(const Value: TPen);
begin
  FBorder := Value;
  PropsChanged(Self);
end;

procedure TJppPanelHorizontalBar.SetDrawGradient(const Value: Boolean);
begin
  FDrawGradient := Value;
  PropsChanged(Self);
end;

procedure TJppPanelHorizontalBar.SetGradient(const Value: TJppGradientEx);
begin
  FGradient := Value;
  PropsChanged(Self);
end;

procedure TJppPanelHorizontalBar.SetHeight(const Value: integer);
begin
  FHeight := Value;
  PropsChanged(Self);
end;

procedure TJppPanelHorizontalBar.SetLeftMargin(const Value: integer);
begin
  FLeftMargin := Value;
  PropsChanged(Self);
end;

procedure TJppPanelHorizontalBar.SetOnChange(const Value: TNotifyEvent);
begin
  FOnChange := Value;
end;

procedure TJppPanelHorizontalBar.SetPosY(const Value: integer);
begin
  FPosY := Value;
  PropsChanged(Self);
end;

procedure TJppPanelHorizontalBar.SetRightMargin(const Value: integer);
begin
  FRightMargin := Value;
  PropsChanged(Self);
end;

procedure TJppPanelHorizontalBar.SetVisible(const Value: Boolean);
begin
  FVisible := Value;
  PropsChanged(Self);
end;

{$endregion TJppPanelHorizontalBar}


{$region ' ---------------------------------- TJppPanelHorizontalBars - collection ----------------------------------------- '}
function TJppPanelHorizontalBars.Add: TJppPanelHorizontalBar;
begin
  Result := TJppPanelHorizontalBar(inherited Add);
  Result.OnChange := {$IFDEF FPC} @ {$ENDIF}PropsChanged;
  PropsChanged(Self);
end;

function TJppPanelHorizontalBars.Insert(Index: Integer): TJppPanelHorizontalBar;
begin
  Result := TJppPanelHorizontalBar(inherited Insert(Index));
  Result.OnChange := {$IFDEF FPC} @ {$ENDIF}PropsChanged;
  PropsChanged(Self);
end;

constructor TJppPanelHorizontalBars.Create(AOwner: TComponent);
begin
  inherited Create(TJppPanelHorizontalBar);
  FOwner := AOwner;
end;


procedure TJppPanelHorizontalBars.Delete(Index: Integer);
begin
  inherited Delete(Index);
  PropsChanged(Self);
end;

function TJppPanelHorizontalBars.GetItem(Index: Integer): TJppPanelHorizontalBar;
begin
  Result := TJppPanelHorizontalBar(inherited GetItem(Index));
end;

function TJppPanelHorizontalBars.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TJppPanelHorizontalBars.PropsChanged(Sender: TObject);
begin
  if Assigned(OnChange) then OnChange(Self);
end;

procedure TJppPanelHorizontalBars.SetItem(Index: Integer; const Value: TJppPanelHorizontalBar);
begin
  inherited SetItem(Index, Value);
  PropsChanged(Self);
end;

procedure TJppPanelHorizontalBars.SetOnChange(const Value: TNotifyEvent);
begin
  FOnChange := Value;
end;
{$endregion TJppPanelHorizontalBars}





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
{$ENDIF}


{$IFDEF DCC}
{$IF RTLVersion >= 24.0 }

//class constructor TJppPanel.Create;
//begin
//  TCustomStyleEngine.RegisterStyleHook(TJppPanel, TJppPanelStyleHook);
//end;
//
//class destructor TJppPanel.Destroy;
//begin
//  TCustomStyleEngine.UnRegisterStyleHook(TJppPanel, TJppPanelStyleHook);
//end;
{$IFEND}
{$endregion Themes}
{$ENDIF}

end.
