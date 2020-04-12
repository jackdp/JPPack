unit JPP.StdPanel;

{
  Jacek Pazera
  https://github.com/jackdp
  http://www.pazera-software.com
}

{$I jpp.inc}
{$IFDEF FPC} {$mode delphi} {$ENDIF}

interface

uses
  {$IFDEF MSWINDOWS}Windows,{$ENDIF}
  Messages, Classes, Graphics, Controls, StdCtrls, ExtCtrls,
  JPP.AnchoredControls
  {$IFDEF FPC}, LCLType, LCLIntf, LMessages, Types{$ENDIF}
  ;

type


  TJppCustomStdPanel = class(TCustomPanel)
  private
    FAnchoredControls: TJppAnchoredControls;
    procedure SetAnchoredControls(const Value: TJppAnchoredControls);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  protected
    property AnchoredControls: TJppAnchoredControls read FAnchoredControls write SetAnchoredControls;
  end;


  {$region ' --- TJppStdPanel --- '}
  TJppStdPanel = class(TJppCustomStdPanel)
  public
    property DockManager;
  published
    property Align;
    property Alignment;
    property Anchors;
    property AutoSize;
    property BevelEdges;
    property BevelInner;
    property BevelKind;
    property BevelOuter;
    property BevelWidth;
    property BiDiMode;
    property BorderWidth;
    property BorderStyle;
    property Caption;
    property Color;
    property Constraints;
    property Ctl3D;
    property UseDockManager default True;
    property DockSite;
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FullRepaint;
    property Font;
    property Locked;
    property Padding;
    property ParentBiDiMode;
    property ParentBackground;
    property ParentColor;
    property ParentCtl3D;
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowCaption;
    property ShowHint;
    property TabOrder;
    property TabStop;
    {$IFDEF DELPHI2010_OR_ABOVE}property Touch;{$ENDIF}
    property VerticalAlignment;
    property Visible;
    {$IFDEF DCC}{$IF RTLVersion > 23}property StyleElements;{$IFEND}{$ENDIF}
    property OnAlignInsertBefore;
    property OnAlignPosition;
    property OnCanResize;
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
    {$IFDEF DELPHI2010_OR_ABOVE}property OnGesture;{$ENDIF}
    property OnGetSiteInfo;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
    // ----------------------
    property AnchoredControls;
  end;
  {$endregion TJppStdPanel}


implementation



constructor TJppCustomStdPanel.Create(AOwner: TComponent);
begin
  inherited;
  FAnchoredControls := TJppAnchoredControls.Create(Self);
end;

destructor TJppCustomStdPanel.Destroy;
begin
  FAnchoredControls.Free;
  inherited;
end;

procedure TJppCustomStdPanel.SetAnchoredControls(const Value: TJppAnchoredControls);
begin
  FAnchoredControls := Value;
end;

procedure TJppCustomStdPanel.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  if not (csDestroying in ComponentState) then
    if Assigned(FAnchoredControls) then FAnchoredControls.UpdateAllControlsPos;
end;

procedure TJppCustomStdPanel.Notification(AComponent: TComponent; Operation: TOperation);
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



end.
