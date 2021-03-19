unit JPP.CheckBox;

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
  JPP.Common, JPP.Common.Procs, JPP.AnchoredControls;


type

  {$region '   TJppCustomCheckBox   '}
  TJppCustomCheckBox = class(TCustomCheckBox)
  private
    FTagExt: TJppTagExt;
    FAnchoredControls: TJppAnchoredControls;
    procedure SetTagExt(const Value: TJppTagExt);
    procedure SetAnchoredControls(const Value: TJppAnchoredControls);
  protected
    procedure PropsChanged(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  protected
    property TagExt: TJppTagExt read FTagExt write SetTagExt;
    property AnchoredControls: TJppAnchoredControls read FAnchoredControls write SetAnchoredControls;
  end;
  {$endregion TJppCustomCheckBox}


  {$region '   TJppCheckBox   '}
  TJppCheckBox = class(TJppCustomCheckBox)
  published
    property Action;
    property Align;
    property Alignment;
    property AllowGrayed;
    property Anchors;
    {$IFDEF FPC}property AutoSize default True;{$ENDIF}
    property BiDiMode;
    {$IFDEF FPC}property BorderSpacing;{$ENDIF}
    property Caption;
    property Checked;
    property Color nodefault;
    property Constraints;
    {$IFDEF DCC}property Ctl3D;{$ENDIF}
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property Hint;
    property ParentBiDiMode;
    property ParentColor;
    {$IFDEF DCC}property ParentCtl3D;{$ENDIF}
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property State;
    property TabOrder;
    property TabStop;
    property Visible;
    {$IFDEF DCC}property WordWrap;{$ENDIF}
    {$IFDEF HAS_STYLE_ELEMENTS}property StyleElements;{$ENDIF}
    {$IFDEF FPC}
    property OnChange;
    property OnChangeBounds;
    {$ENDIF}
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    {$IFDEF FPC}property OnEditingDone; {$ENDIF}
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    {$IFDEF DCC}property OnMouseActivate;{$ENDIF}
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    {$IFDEF FPC}
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    {$ENDIF}
    property OnStartDock;
    property OnStartDrag;
    {$IFDEF FPC}property OnUTF8KeyPress;{$ENDIF}

    property TagExt;
    property AnchoredControls;
  end;
  {$endregion TJppCheckBox}


implementation




{$region '                       TJppCustomCheckBox                            '}

constructor TJppCustomCheckBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTagExt := TJppTagExt.Create(Self);
  FAnchoredControls := TJppAnchoredControls.Create(Self);
end;

destructor TJppCustomCheckBox.Destroy;
begin;
  FTagExt.Free;
  FAnchoredControls.Free;
  inherited;
end;

procedure TJppCustomCheckBox.PropsChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TJppCustomCheckBox.SetAnchoredControls(const Value: TJppAnchoredControls);
begin
  FAnchoredControls := Value;
end;

procedure TJppCustomCheckBox.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  if not (csDestroying in ComponentState) then
    if Assigned(FAnchoredControls) then FAnchoredControls.UpdateAllControlsPos;
end;

procedure TJppCustomCheckBox.Notification(AComponent: TComponent; Operation: TOperation);
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

procedure TJppCustomCheckBox.SetTagExt(const Value: TJppTagExt);
begin
  FTagExt := Value;
end;


{$endregion TJppCustomCheckBox}




end.
