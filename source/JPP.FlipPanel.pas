unit JPP.FlipPanel;

{
  Jacek Pazera
  https://www.pazera-software.com
  https://github.com/jackdp

  TJppFlipPanel - collapsible panel based on the TJvRollOut panel from the JVCL package
  https://github.com/project-jedi/jvcl/blob/master/jvcl/run/JvRollOut.pas

  Tested on:
    Delphi 2009, 2010, XE, XE2, XE3, XE4, XE5, XE6, XE7, Rio
    Lazarus 2.0.8 + FPC 3.0.4, Lazarus 2.0.10 + FPC 3.2.0


  ---------
  Original description

  The contents of this file are subject to the Mozilla Public License
  Version 1.1 (the "License"); you may not use this file except in compliance
  with the License. You may obtain a copy of the License at
  http://www.mozilla.org/MPL/MPL-1.1.html

  Software distributed under the License is distributed on an "AS IS" basis,
  WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
  the specific language governing rights and limitations under the License.

  The Original Code is: JvRollOut.PAS, released on 2002-05-26.
  The Initial Developer of the Original Code is Peter Thörnqvist [peter3 at sourceforge dot net]
  Portions created by Peter Thörnqvist are Copyright (C) 2002 Peter Thörnqvist.
  All Rights Reserved.

  Contributor(s):
  You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
  located at http://jvcl.delphi-jedi.org

  Description:
    TJvRollOut is an autoexpanding / collapsing panel.

  Known Issues:
    Doesn't draw an underline for speed-keys (the '&' character ) if
    Placement = plLeft. Something with DrawText ?
}


{$I jpp.inc}
{$IFDEF FPC} {$mode delphi} {$ENDIF}


interface

uses
  {$IFDEF FPC}LCLType, LCLIntf, LMessages, LCLVersion,{$ENDIF}
  {$IFDEF DCC}Windows, Messages,{$ENDIF}
  SysUtils, Classes, Controls, Graphics, ImgList, ExtCtrls, ActnList, Forms, StdCtrls, Types,
                                              JPL.Rects,
  {$IFDEF DCC}{$IFDEF HAS_SYSTEM_UITYPES}UITypes,{$ENDIF}{$ENDIF}
  JPP.Common, JPP.Common.Procs, JPP.Gradient, JPP.SimplePanel;


{$IFDEF FPC}
  {$I jpp_lcl_ver.inc} // must be after 'uses LCLVersion'
{$ENDIF}


const
  CM_EXPANDED = WM_USER + 155;


type

  {$Region '   TJppFlipPanelHeader   '}
  TJppFlipPanelHeader = class(TJppPersistent)
  private
    FOwner: TComponent;
    FOnChange: TNotifyEvent;
    FBackgroundColor: TColor;
    FBackgroundColorTo: TColor;
    FHotBackgroundColor: TColor;
    FHotBackgroundColorTo: TColor;
    FFont: TFont;
    FHotCaptionColor: TColor;
    FBorderColor: TColor;
    FHotBorderColor: TColor;
    FCaption: string;
    FCaptionCollapsed: string;
    FCaptionAlignment: TAlignment;
    FCaptionPosDeltaX: ShortInt;
    FCaptionPosDeltaY: ShortInt;
    FHeight: SmallInt;
    FOnHeightChanged: TNotifyEvent;
    FImages: TCustomImageList;
    FChangeLink: TChangeLink;
    FImageIndexExpanded: integer;
    FImageIndexCollapsed: integer;
    FImageMargin: ShortInt;
    FCaptionShadow: TJppTextShadowParams;
    FCaptionHotShadow: TJppTextShadowParams;
    FHideBottomBorderWhenCollapsed: Boolean;
    FImagePosDeltaYExpanded: ShortInt;
    FImagePosDeltaYCollapsed: ShortInt;
    FRightCaption: string;
    FRightCaptionVisible: Boolean;
    FRightCaptionFont: TFont;
    FRightCaptionEllipsis: TEllipsisPosition;
    procedure SetOnChange(const Value: TNotifyEvent);
    procedure SetBackgroundColor(const Value: TColor);
    procedure SetBackgroundColorTo(const Value: TColor);
    procedure SetHotBackgroundColor(const Value: TColor);
    procedure SetHotBackgroundColorTo(const Value: TColor);
    procedure SetFont(const Value: TFont);
    procedure SetHotCaptionColor(const Value: TColor);
    procedure SetBorderColor(const Value: TColor);
    procedure SetHotBorderColor(const Value: TColor);
    procedure SetCaption(const Value: string);
    procedure SetCaptionCollapsed(const Value: string);
    procedure SetCaptionAlignment(const Value: TAlignment);
    procedure SetCaptionPosDeltaX(const Value: ShortInt);
    procedure SetCaptionPosDeltaY(const Value: ShortInt);
    procedure SetHeight(const Value: SmallInt);
    procedure SetOnHeightChanged(const Value: TNotifyEvent);
    procedure SetImages(const Value: TCustomImageList);
    procedure SetImageIndexExpanded(const Value: integer);
    procedure SetImageIndexCollapsed(const Value: integer);
    procedure SetImageMargin(const Value: ShortInt);
    procedure SetCaptionShadow(const Value: TJppTextShadowParams);
    procedure SetCaptionHotShadow(const Value: TJppTextShadowParams);
    procedure SetHideBottomBorderWhenCollapsed(const Value: Boolean);
    procedure SetImagePosDeltaYExpanded(const Value: ShortInt);
    procedure SetImagePosDeltaYCollapsed(const Value: ShortInt);
    procedure SetRightCaption(const Value: string);
    procedure SetRightCaptionVisible(const Value: Boolean);
    procedure SetRightCaptionFont(const Value: TFont);
    procedure SetRightCaptionEllipsis(const Value: TEllipsisPosition);
  protected
    procedure PropsChanged(Sender: TObject);
    procedure DoChangeLink(Sender: TObject);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(const Source: TJppFlipPanelHeader; bCopyCaption: Boolean = False; bCopyRightCaption: Boolean = False); reintroduce;
  published
    property Height: SmallInt read FHeight write SetHeight default 22;
    property OnHeightChanged: TNotifyEvent read FOnHeightChanged write SetOnHeightChanged;

    property Caption: string read FCaption write SetCaption;
    property CaptionCollapsed: string read FCaptionCollapsed write SetCaptionCollapsed;
    property CaptionAlignment: TAlignment read FCaptionAlignment write SetCaptionAlignment default taLeftJustify;
    property CaptionPosDeltaX: ShortInt read FCaptionPosDeltaX write SetCaptionPosDeltaX default 5;
    property CaptionPosDeltaY: ShortInt read FCaptionPosDeltaY write SetCaptionPosDeltaY default 0;
    property CaptionShadow: TJppTextShadowParams read FCaptionShadow write SetCaptionShadow;
    property CaptionHotShadow: TJppTextShadowParams read FCaptionHotShadow write SetCaptionHotShadow;

    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor default $00DFDFDF;
    property BackgroundColorTo: TColor read FBackgroundColorTo write SetBackgroundColorTo default clNone;
    property HotBackgroundColor: TColor read FHotBackgroundColor write SetHotBackgroundColor default $00D4D4D4;
    property HotBackgroundColorTo: TColor read FHotBackgroundColorTo write SetHotBackgroundColorTo default clNone;
    property HotCaptionColor: TColor read FHotCaptionColor write SetHotCaptionColor default clNone;

    property BorderColor: TColor read FBorderColor write SetBorderColor default clGray;
    property HotBorderColor: TColor read FHotBorderColor write SetHotBorderColor default clNone;
    property HideBottomBorderWhenCollapsed: Boolean read FHideBottomBorderWhenCollapsed write SetHideBottomBorderWhenCollapsed default False;

    property Font: TFont read FFont write SetFont;
    property OnChange: TNotifyEvent read FOnChange write SetOnChange;
    property Images: TCustomImageList read FImages write SetImages;
    property ImageIndexExpanded: integer read FImageIndexExpanded write SetImageIndexExpanded default 0;
    property ImageIndexCollapsed: integer read FImageIndexCollapsed write SetImageIndexCollapsed default 1;
    property ImageMargin: ShortInt read FImageMargin write SetImageMargin default 5;
    property ImagePosDeltaYExpanded: ShortInt read FImagePosDeltaYExpanded write SetImagePosDeltaYExpanded default 0;
    property ImagePosDeltaYCollapsed: ShortInt read FImagePosDeltaYCollapsed write SetImagePosDeltaYCollapsed default 0;

    property RightCaption: string read FRightCaption write SetRightCaption;
    property RightCaptionVisible: Boolean read FRightCaptionVisible write SetRightCaptionVisible default True;
    property RightCaptionFont: TFont read FRightCaptionFont write SetRightCaptionFont;
    property RightCaptionEllipsis: TEllipsisPosition read FRightCaptionEllipsis write SetRightCaptionEllipsis default epEndEllipsis;
  end;
  {$endregion TJppFlipPanelHeader}


  {$Region '   TJppCustomFlipPanel   '}
  TJppCustomFlipPanel = class(TJppCustomSimplePanel)
  private
    FGroupIndex: Integer;
    FHeaderRect: TRect;
    FCollapsed: Boolean;
    FMouseDown: Boolean;
    FMouseInHeader: Boolean;
    FCWidth: Integer;
    FCHeight: Integer;
    FAWidth: Integer;
    FAHeight: Integer;
    FChildOffset: Integer;
    FOnExpand: TNotifyEvent;
    FOnCollapse: TNotifyEvent;
    FShowFocus: Boolean;
    FChildControlVisibility: TStringList;

    FCollapseControlsOnHeader: Boolean;
    FHeader: TJppFlipPanelHeader;

    procedure SetGroupIndex(Value: Integer);

    procedure WriteAWidth(Writer: TWriter);
    procedure WriteAHeight(Writer: TWriter);
    procedure WriteCWidth(Writer: TWriter);
    procedure WriteCHeight(Writer: TWriter);
    procedure ReadAWidth(Reader: TReader);
    procedure ReadAHeight(Reader: TReader);
    procedure ReadCWidth(Reader: TReader);
    procedure ReadCHeight(Reader: TReader);

    procedure SetCollapsed(Value: Boolean);
//    procedure SetButtonHeight(Value: Integer);
    procedure SetChildOffset(Value: Integer);
    procedure RedrawControl(DrawAll: Boolean);
    procedure DrawHeader;
    procedure UpdateGroup;
    procedure SetExpandedSize(const Value: Integer);
    procedure CMExpanded(var Msg: {$IFDEF FPC}TLMessage{$ELSE}TMessage{$ENDIF}); message CM_EXPANDED;
    procedure ChangeHeight(NewHeight: Integer);
    procedure SetShowFocus(const Value: Boolean);
    procedure SetHeader(const Value: TJppFlipPanelHeader);
    procedure SetExpanded(const Value: Boolean);
    function GetExpanded: Boolean;
  protected
    procedure CheckChildVisibility;
    procedure AlignControls(AControl: TControl; var Rect: TRect); override;
    procedure Click; override;
    procedure CMParentColorChanged(var Message: {$IFDEF FPC}TLMessage{$ELSE}TMessage{$ENDIF}); message CM_PARENTCOLORCHANGED;
    procedure CreateWnd; override;
    procedure DefineProperties(Filer: TFiler); override;
    {$IFDEF FPC}function DialogChar(var Message: TLMKey): boolean; override;{$ENDIF}
    procedure DoCollapse; dynamic;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure DoExpand; dynamic;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseEnter(Sender: TObject); override;
    procedure MouseLeave(Sender: TObject); override;

    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Paint; override;
    procedure OnHeaderHeightChanged(Sender: TObject);
    procedure DrawCaption({%H-}aRect: TRect); override;

    property ChildOffset: Integer read FChildOffset write SetChildOffset default 0;
    property CollapseControlsOnHeader: Boolean read FCollapseControlsOnHeader write FCollapseControlsOnHeader default False;
    property Collapsed: Boolean read FCollapsed write SetCollapsed default False;
    property Expanded: Boolean read GetExpanded write SetExpanded default True;
    property GroupIndex: Integer read FGroupIndex write SetGroupIndex default 0;
    property ShowFocus: Boolean read FShowFocus write SetShowFocus default False;
    property OnCollapse: TNotifyEvent read FOnCollapse write FOnCollapse;
    property OnExpand: TNotifyEvent read FOnExpand write FOnExpand;
    property Header: TJppFlipPanelHeader read FHeader write SetHeader;

  {$IFDEF FPC}
    { LCL scaling }
  protected
    procedure DoAutoAdjustLayout(const AMode: TLayoutAdjustmentPolicy; const AXProportion, AYProportion: Double); override;
  public
    {$IFDEF LCLVER_2010000_OR_ABOVE}
    procedure FixDesignFontsPPI(const ADesignTimePPI: Integer); override;
    {$ENDIF}
    procedure ScaleFontsPPI({$IFDEF LCLVER_1080100_OR_ABOVE}const AToPPI: Integer;{$ENDIF} const AProportion: Double); override;
  {$ENDIF}

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function MouseOverHeader: Boolean;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure Collapse; virtual;
    procedure Expand; virtual;

    property ExpandedSize: Integer write SetExpandedSize stored False;
  end;
  {$endregion TJppCustomFlipPanel}


  {$Region '   TJppFlipPanel   '}
  TJppFlipPanel = class(TJppCustomFlipPanel)
  published
    property Align;
    //property BevelWidth;
    //property BorderWidth;
    property ChildOffset;
    property CollapseControlsOnHeader;
    property Collapsed;
    property DragCursor;
    property DragMode;
    property Enabled;
    property Font;
    property GroupIndex;
//    property ImageOptions;
    //{$IFDEF LCLVER_2000000_OR_ABOVE}
    //property ParentBackground default True;
    //{$ENDIF}
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowFocus;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
    property OnExpand;
    property OnCollapse;

    // jacek
    {$IFDEF FPC}property BorderSpacing;{$ENDIF}
    property AnchoredControls;
    property Appearance;
    property Header;
    property Expanded;


    property Anchors;
    property BiDiMode;
    property Constraints;
    {$IFDEF DCC} property Ctl3D; {$ENDIF}
    property UseDockManager default True;
    property DockSite;
    property DragKind;
    property FullRepaint;
    {$IFDEF DCC} property Locked; {$ENDIF}
    {$IFDEF DELPHI2009_OR_ABOVE} property Padding; {$ENDIF}
    property ParentBiDiMode;
    {$IFDEF DCC}property ParentBackground default false;{$ENDIF}
    {$IFDEF FPC}{$IFDEF HAS_PANEL_WITH_PARENTBACKGROUND}
    property ParentBackground default false;
    {$ENDIF}{$ENDIF}
    {$IFDEF DCC} property ParentCtl3D; {$ENDIF}
    {$IFDEF DCC} property OnCanResize; {$ENDIF}
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnEndDock;
    property OnGetSiteInfo;
    property OnMouseWheel;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnResize;
    property OnStartDock;
    property OnUnDock;
    property OnVisibleChanging;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnAfterDrawBackground;
    property OnPaint;
    {$IFDEF MSWINDOWS}property OnDragDropFiles;{$ENDIF}
    property TagExt;
    {$IFDEF HAS_STYLE_ELEMENTS} property StyleElements; {$ENDIF}
    {$IFDEF DELPHI2010_OR_ABOVE} property Touch; {$ENDIF}
    property DoubleBuffered;
    {$IFDEF DCC}property ParentDoubleBuffered;{$ENDIF}
    {$IFDEF FPC}{$IFDEF HAS_WINCONTROL_WITH_PARENTDOUBLEBUFFERED}
    property ParentDoubleBuffered;
    {$ENDIF}{$ENDIF}
    {$IFDEF FPC}
    property ChildSizing;
    property OnGetDockCaption;
    {$ENDIF}
  end;
  {$endregion TJppFlipPanel}


implementation


{$Region '   JVCL routines   '}
// from JvJVCLUtils.pas (JVCL)
function ReplaceImageListReference(This: TComponent; NewReference: TCustomImageList; var VarReference: TCustomImageList; ChangeLink: TChangeLink): Boolean;
begin
  Result := (VarReference <> NewReference) and Assigned(This);
  if Result then
  begin
    if Assigned(VarReference) then
    begin
      VarReference.RemoveFreeNotification(This);
      VarReference.UnRegisterChanges(ChangeLink);
    end;
    VarReference := NewReference;
    if Assigned(VarReference) then
    begin
      VarReference.RegisterChanges(ChangeLink);
      VarReference.FreeNotification(This);
    end;
  end;
end;
{$endregion JVCL routines}



{$Region '                               TJppCustomFlipPanel                                '}

{$Region '               Create & Destroy               '}
constructor TJppCustomFlipPanel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  {$IFDEF DCC}ShowCaption := False;{$ELSE}Caption := '';{$ENDIF}

  FCollapseControlsOnHeader := False;
  SetBounds(0, 0, 145, 170);
  FAWidth := 145;
  FAHeight := 170;
  FCWidth := 22;
  FCHeight := 24;
  FShowFocus := False;

  ControlStyle := ControlStyle - [csDoubleClicks];    // Doubleclicks are converted into single clicks

  FHeader := TJppFlipPanelHeader.Create(Self);
  FHeader.Caption := Self.Name;
  FHeader.OnChange := PropsChanged;
  FHeader.OnHeightChanged := OnHeaderHeightChanged;

end;

destructor TJppCustomFlipPanel.Destroy;
begin
  FreeAndNil(FChildControlVisibility);
  FHeader.Free;
  inherited Destroy;
end;

procedure TJppCustomFlipPanel.CreateWnd;
begin
  inherited CreateWnd;
  if not Collapsed then UpdateGroup;
end;
{$endregion Create & Destroy}

procedure TJppCustomFlipPanel.Click;
begin
  //if MouseOverHeader then Collapsed := not FCollapsed;
  if FMouseInHeader then Collapsed := not FCollapsed;
  inherited Click;
  RedrawControl(False);
end;

procedure TJppCustomFlipPanel.AlignControls(AControl: TControl; var Rect: TRect);
begin
  if Assigned(FHeader) then
  begin
    Rect.Left := Rect.Left + ChildOffset;
    Rect.Top := Rect.Top + FHeader.Height;
  end;
  inherited AlignControls(AControl, Rect);
end;

procedure TJppCustomFlipPanel.RedrawControl(DrawAll: Boolean);
begin
  if DrawAll then Invalidate
  else DrawHeader;
end;

procedure TJppCustomFlipPanel.SetGroupIndex(Value: Integer);
begin
  if FGroupIndex = Value then Exit;
  FGroupIndex := Value;
  if not Collapsed then UpdateGroup;
end;

procedure TJppCustomFlipPanel.SetHeader(const Value: TJppFlipPanelHeader);
begin
  FHeader := Value;
  RedrawControl(False);
end;

function TJppCustomFlipPanel.GetExpanded: Boolean;
begin
  Result := not Collapsed;
end;

procedure TJppCustomFlipPanel.SetExpanded(const Value: Boolean);
begin
  SetCollapsed(not Value);
end;

procedure TJppCustomFlipPanel.SetCollapsed(Value: Boolean);
begin
  if FCollapsed = Value then Exit;

  FCollapsed := Value;
  if FCollapsed then
  begin
    ChangeHeight(FCHeight);
    DoCollapse;
  end
  else
  begin
    ChangeHeight(FAHeight);
    DoExpand;
    UpdateGroup;
  end;
  CheckChildVisibility;
end;

procedure TJppCustomFlipPanel.ChangeHeight(NewHeight: Integer);
var
  OldHeight: Integer;
begin
  OldHeight := Height;
  Parent.DisableAlign;
  DisableAlign;
  try
    Height := NewHeight;
    if Align = alBottom then Top := Top + (OldHeight - NewHeight);
  finally
    EnableAlign;
    Parent.EnableAlign;
  end;
end;

procedure TJppCustomFlipPanel.DoExpand;
begin
  if Assigned(FOnExpand) then FOnExpand(Self);
end;

procedure TJppCustomFlipPanel.DoCollapse;
begin
  if Assigned(FOnCollapse) then FOnCollapse(Self);
end;

procedure TJppCustomFlipPanel.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  if FCollapsed then FCHeight := AHeight
  else FAHeight := AHeight;

  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  if not Collapsed then UpdateGroup;
end;

procedure TJppCustomFlipPanel.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('FAWidth', ReadAWidth, WriteAWidth, True);
  Filer.DefineProperty('FAHeight', ReadAHeight, WriteAHeight, True);
  Filer.DefineProperty('FCWidth', ReadCWidth, WriteCWidth, True);
  Filer.DefineProperty('FCHeight', ReadCHeight, WriteCHeight, True);
end;

procedure TJppCustomFlipPanel.WriteAWidth(Writer: TWriter);
begin
  Writer.WriteInteger(FAWidth);
end;

procedure TJppCustomFlipPanel.WriteAHeight(Writer: TWriter);
begin
  Writer.WriteInteger(FAHeight);
end;

procedure TJppCustomFlipPanel.WriteCWidth(Writer: TWriter);
begin
  Writer.WriteInteger(FCWidth);
end;

procedure TJppCustomFlipPanel.WriteCHeight(Writer: TWriter);
begin
  Writer.WriteInteger(FCHeight);
end;

procedure TJppCustomFlipPanel.ReadAWidth(Reader: TReader);
begin
  FAWidth := Reader.ReadInteger;
//  if not Collapsed and (Placement = plLeft) then
//    SetBounds(Left, Top, FAWidth, Height);
end;

procedure TJppCustomFlipPanel.ReadAHeight(Reader: TReader);
begin
  FAHeight := Reader.ReadInteger;
  if not Collapsed {and (Placement = plTop)} then SetBounds(Left, Top, Width, FAHeight);
end;

procedure TJppCustomFlipPanel.ReadCWidth(Reader: TReader);
begin
  FCWidth := Reader.ReadInteger;
//  if Collapsed and (Placement = plLeft) then
//    SetBounds(Left, Top, FCWidth, Height);
end;

procedure TJppCustomFlipPanel.ReadCHeight(Reader: TReader);
begin
  FCHeight := Reader.ReadInteger;
  if Collapsed {and (Placement = plTop)} then SetBounds(Left, Top, Width, FCHeight);
end;

procedure TJppCustomFlipPanel.DrawCaption(aRect: TRect);
begin
  //inherited DrawCaption(aRect);
  // Use Header.Caption and Header.CaptionCollapsed
end;

procedure TJppCustomFlipPanel.OnHeaderHeightChanged(Sender: TObject);
var
  bCollapsed: Boolean;
begin
  if not Assigned(FHeader) then Exit;
  bCollapsed := Collapsed;
  try
    Expand;
    FCHeight := FHeader.Height + 2;
    FHeaderRect := Rect(BevelWidth, BevelWidth, Width - BevelWidth, FHeader.Height + BevelWidth);
    Realign;
    RedrawControl(True);
  finally
    Collapsed := bCollapsed;
  end;
end;

procedure TJppCustomFlipPanel.SetChildOffset(Value: Integer);
begin
  if FChildOffset <> Value then
  begin
    FChildOffset := Value;
    Realign;
  end;
end;

// To make Setting of expanded size possible, even if panel is collapsed
procedure TJppCustomFlipPanel.SetExpandedSize(const Value: Integer);
begin
  if FAHeight = Value then Exit;
  FAHeight := Value;
  if not FCollapsed then ChangeHeight(FAHeight);
end;

{$Region '                Mouse: Down, Up, Move, Enter, Leave                 '}
procedure TJppCustomFlipPanel.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  if not FMouseDown then
  begin
    FMouseDown := True;
    RedrawControl(False);
    if CanFocus {and not (csDesigning in ComponentState)} then SetFocus;
  end;
end;

procedure TJppCustomFlipPanel.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if FMouseDown then
  begin
    FMouseDown := False;
    RedrawControl(False);
  end;
end;

procedure TJppCustomFlipPanel.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  B: Boolean;
begin
  B := FMouseInHeader;
  inherited MouseMove(Shift, X, Y);

  FMouseInHeader := PtInRect(FHeaderRect, Point(X, Y));
  if FMouseInHeader <> B then RedrawControl(False);
end;

procedure TJppCustomFlipPanel.MouseEnter(Sender: TObject);
begin
  inherited;
end;

procedure TJppCustomFlipPanel.MouseLeave(Sender: TObject);
begin
  inherited;
  if csDesigning in ComponentState then Exit;
  if FMouseInHeader then
  begin
    FMouseInHeader := False;
    FMouseDown := False;
    RedrawControl(False);
  end;
end;
{$endregion Mouse: Down, Up, Move, Enter, Leave}

procedure TJppCustomFlipPanel.Paint;
begin
  inherited;
  DrawHeader;
end;

{$Region '                                DrawHeader                                 '}
procedure TJppCustomFlipPanel.DrawHeader;
var
  R: TRect;
  FIndex, dy: integer;
  sCaption, sRightCaption: string;
  Flags: Cardinal;
  cl: TColor;
begin
  if not Assigned(FHeader) then Exit;
  //FHeaderRect := Rect(BevelWidth, BevelWidth, Width - BevelWidth, FHeader.Height + BevelWidth);
  FHeaderRect := Rect(0, 0, Width, FHeader.Height+2);
  //FHeaderRect.Height := FHeaderRect.Height + 2 * BevelWidth;

  R := FHeaderRect;

  with Canvas do
  begin


    // ------------------------- Background ---------------------------

    cl := clNone;
    if FMouseInHeader then
    begin
      if FHeader.HotBackgroundColorTo <> clNone then JppGradientFill(Canvas, R, FHeader.HotBackgroundColor, FHeader.HotBackgroundColorTo, gtVertical, 128)
      else
      begin
        cl := FHeader.HotBackgroundColor;
        //if cl = FHeader.BackgroundColor then cl := clNone;
      end;

    end
    else
    begin
      if FHeader.BackgroundColorTo <> clNone then JppGradientFill(Canvas, R, FHeader.BackgroundColor, FHeader.BackgroundColorTo, gtVertical, 128)
      else cl := FHeader.BackgroundColor;
    end;

    if cl <> clNone then
    begin
      Brush.Color := cl;
      FillRect(R);
    end;



    // -------------------------- Border -----------------------------
    if FMouseInHeader then
    begin
      cl := FHeader.HotBorderColor;
      if cl = clNone then cl := FHeader.BorderColor;
    end
    else cl := FHeader.BorderColor;
    if cl <> clNone then
    begin
      Pen.Color := cl;
      //Brush.Style := bsClear;
      //Rectangle(R);
      if Collapsed and FHeader.HideBottomBorderWhenCollapsed then DrawRectEx(Canvas, R, True, True, True, False)
      else DrawRectEx(Canvas, R, True, True, True, True);
      //Brush.Style := bsSolid;
    end;


    // ------------------------------ Image --------------------------------

    if Collapsed then FIndex := FHeader.ImageIndexCollapsed
    else FIndex := FHeader.ImageIndexExpanded;

    R := FHeaderRect;

    if Assigned(FHeader.Images) then
    begin
      if Collapsed then dy := FHeader.ImagePosDeltaYCollapsed else dy := FHeader.ImagePosDeltaYExpanded;
      if Collapsed and FHeader.HideBottomBorderWhenCollapsed then Inc(dy);

      FHeader.Images.Draw(
        Canvas,
        FHeader.ImageMargin + BevelWidth,
        BevelWidth + ((FHeader.Height - FHeader.Images.Height) div 2) + dy,
        FIndex
      );
      R.Left := FHeader.Images.Width + FHeader.ImageMargin {* 2} + BevelWidth;
    end
    else
      R.Left := {FHeader.ImageMargin * 2 +} BevelWidth;



    if FCollapsed then
    begin
      sCaption := FHeader.CaptionCollapsed;
      if sCaption = '' then sCaption := FHeader.Caption;
    end
    else sCaption := FHeader.Caption;

    Canvas.Font.Assign(FHeader.Font);
    R.Top := R.Top - (TextHeight(sCaption) - (FHeaderRect.Bottom - FHeaderRect.Top)) div 2; // + BevelWidth div 2;


    // ------------------------------ Caption -------------------------------

    if Length(sCaption) > 0 then
    begin
      Canvas.Font.Assign(FHeader.Font);
      if FMouseInHeader and (FHeader.HotCaptionColor <> clNone) then Canvas.Font.Color := FHeader.HotCaptionColor;

      SetBkMode(Canvas.Handle, Transparent);
      Flags := DT_NOCLIP;// or DT_VCENTER;// or DT_SINGLELINE;

      case FHeader.CaptionAlignment of
        taLeftJustify: Flags := Flags or DT_LEFT;
        taCenter: Flags := Flags or DT_CENTER;
        taRightJustify: Flags := Flags or DT_RIGHT;
      end;

      if FHeader.CaptionPosDeltaX <> 0 then
        if FHeader.CaptionAlignment = taLeftJustify then R.Left := R.Left + FHeader.CaptionPosDeltaX
        else if FHeader.CaptionAlignment = taRightJustify then R.Right := R.Right - FHeader.CaptionPosDeltaX;
        // if taCenter we dont modify the X position

      dy := FHeader.CaptionPosDeltaY;
      if Collapsed and FHeader.HideBottomBorderWhenCollapsed then Inc(dy);
      R.Top := R.Top + dy;

      if FMouseInHeader then
      begin
        if FHeader.CaptionHotShadow.Enabled and (FHeader.HotCaptionColor <> clNone) and (FHeader.CaptionHotShadow.Color <> clNone) then
          DrawShadowText(Canvas, sCaption, R, Flags, Canvas.Font.Color, FHeader.CaptionHotShadow.Color, FHeader.CaptionHotShadow.ShiftX, FHeader.CaptionHotShadow.ShiftY)
        else
          DrawText(Canvas.Handle, PChar(sCaption), Length(sCaption), R, Flags);
      end
      else
      begin
        if FHeader.CaptionShadow.Enabled and (FHeader.CaptionShadow.Color <> clNone) then
          DrawShadowText(Canvas, sCaption, R, Flags, Canvas.Font.Color, FHeader.CaptionShadow.Color, FHeader.CaptionShadow.ShiftX, FHeader.CaptionShadow.ShiftY)
        else
          DrawText(Canvas.Handle, PChar(sCaption), Length(sCaption), R, Flags);
      end;

      //R.Top := R.Top - dy;

    end;



    // ------------------ Right Caption -----------------------
    // RightCaption is displayed properly only when HeaderCaption.CaptionAlignment = taLeftJustify !!!
    sRightCaption := FHeader.RightCaption;
    if (FHeader.RightCaptionVisible) and (sRightCaption <> '') then
    begin
      R := FHeaderRect;

      if sCaption <> '' then
      begin
        Font.Assign(FHeader.Font);
        R.Left := R.Left + Canvas.TextWidth(sCaption);
        R.Left := R.Left + FHeader.CaptionPosDeltaX + 5; // 5 - space between Caption and RightCaption
      end;

      if Assigned(FHeader.Images) then R.Left := R.Left + FHeader.Images.Width + FHeader.ImageMargin;

      R.Right := R.Right - 4; // 4 - right margin

      Font.Assign(FHeader.RightCaptionFont);
      Flags := 0;
      case FHeader.RightCaptionEllipsis of
        epPathEllipsis: Flags := DT_PATH_ELLIPSIS;
        epEndEllipsis: Flags := DT_END_ELLIPSIS;
        epWordEllipsis: Flags := DT_WORD_ELLIPSIS;
      end;

      Brush.Style := bsClear;
      DrawText(Canvas.Handle, PChar(sRightCaption), Length(sRightCaption), R, Flags or DT_RIGHT or DT_SINGLELINE or DT_VCENTER);
    end;


    // ---------------- Focus rect ----------------------

    if ShowFocus and Focused then
    begin
      R := FHeaderRect;
      if Collapsed and FHeader.HideBottomBorderWhenCollapsed then R.Bottom := R.Bottom + 1;
      InflateRect(R, -2, -2);
      DrawFocusRect(R);
    end;

  end; // with Canvas
end;
{$endregion DrawHeader}

procedure TJppCustomFlipPanel.Collapse;
begin
  SetCollapsed(True);
end;

procedure TJppCustomFlipPanel.Expand;
begin
  SetCollapsed(False);
end;

procedure TJppCustomFlipPanel.UpdateGroup;
var
  Msg: {$IFDEF FPC}TLMessage{$ELSE}TMessage{$ENDIF};
begin
  if (FGroupIndex <> 0) and (Parent <> nil) then
  begin
    Msg.Msg := CM_EXPANDED;
    Msg.WParam := FGroupIndex;
    Msg.LParam := LPARAM(Self);
    Msg.Result := 0;
    Parent.Broadcast(Msg);
  end;
end;

procedure TJppCustomFlipPanel.CMExpanded(var Msg: {$IFDEF FPC}TLMessage{$ELSE}TMessage{$ENDIF});
var
  Sender: TJppCustomFlipPanel;
begin
  if Msg.WParam = WPARAM(FGroupIndex) then
  begin
    Sender := TJppCustomFlipPanel(Msg.LParam);
    if (Sender <> Self) then
    begin
      SetCollapsed(True);
      CheckChildVisibility;
      Invalidate;
    end;
  end;
end;

procedure TJppCustomFlipPanel.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (FHeader <> nil) and (AComponent = FHeader.Images) then FHeader.Images := nil;
end;


procedure TJppCustomFlipPanel.CMParentColorChanged(var Message: {$IFDEF FPC}TLMessage{$ELSE}TMessage{$ENDIF});
begin
  if csLoading in ComponentState then Exit;
  inherited;
  if ParentColor then
  begin
    Appearance.BackgroundColor := Color;
    Appearance.BackroundColorTo := clNone;
  end;
end;

function TJppCustomFlipPanel.MouseOverHeader: Boolean;
var
  P: TPoint{$IFDEF FPC} = (x:0; y:0){$ENDIF};
  R: TRect;
begin
  {$IFDEF DCC}
  P.X := 0;
  P.Y := 0;
  {$ENDIF}
  GetCursorPos(P);
  P := ScreenToClient(P);
  R := FHeaderRect;
  // (p3) include edges in hit test
  InflateRect(R, 1, 1);
  Result := PtInRect(R, P);
end;

{$IFDEF FPC}
function TJppCustomFlipPanel.DialogChar(var Message: TLMKey): boolean;
var
  key: Word;
  shift: TShiftState;
begin
  key := Message.CharCode;
  shift := KeyDataToShiftState(Message.KeyData);
  Result := Enabled and (IsAccel(Key, Caption) and (ssAlt in Shift)) or ((Key = VK_SPACE) and Focused);
  if Result then
  begin
    SetCollapsed(not FCollapsed);
    if CanFocus then SetFocus;
  end
  else
    Result := inherited;
end;
{$ENDIF}

procedure TJppCustomFlipPanel.DoEnter;
begin
  CheckChildVisibility;
  inherited;
  Invalidate;
end;

procedure TJppCustomFlipPanel.DoExit;
begin
  CheckChildVisibility;
  inherited;
  Invalidate;
end;

procedure TJppCustomFlipPanel.SetShowFocus(const Value: Boolean);
begin
  if FShowFocus <> Value then
  begin
    FShowFocus := Value;
    if Focused then Invalidate;
  end;
end;

{ When the panel is collaped all contained controls are hidden
  to avoid tabbing into the child when the child is not visible or the
  rollout-caption-button being hidden by a contained control that is
  aligned to the bottom.
  The original visiblility of each control is restored then the rollout
  is expanded again. }
procedure TJppCustomFlipPanel.CheckChildVisibility;

  procedure GetChildVisibility;
  var
    I: Integer;
  begin
    if FChildControlVisibility = nil then
    begin
      FChildControlVisibility := TStringList.Create;
      FChildControlVisibility.Sorted := True;
    end;

    for I := 0 to ControlCount - 1 do
      if (Controls[I] is TWinControl) and (TWinControl(Controls[I]).Visible) then
      begin
        FChildControlVisibility.AddObject(Controls[I].Name, Controls[I]);
        if Assigned(FHeader) then
          if CollapseControlsOnHeader or (TWinControl(Controls[I]).Top > FHeader.Height) then
            TWinControl(Controls[I]).Visible := False;
      end;
  end;

  procedure SetChildVisibility;
  var
    I: Integer;
  begin
    if FChildControlVisibility <> nil then
    begin
      for I := 0 to FChildControlVisibility.Count - 1 do
        if FindChildControl(FChildControlVisibility[I]) <> nil then
          TWinControl(FChildControlVisibility.Objects[I]).Visible := True;
      FreeAndNil(FChildControlVisibility);
    end;
  end;

begin
  if csDesigning in ComponentState then Exit;

  if Collapsed then GetChildVisibility
  else SetChildVisibility;
end;


{$IFDEF FPC}
{ LCL scaling }
procedure TJppCustomFlipPanel.DoAutoAdjustLayout(const AMode: TLayoutAdjustmentPolicy; const AXProportion, AYProportion: Double);
begin
  inherited;
  if AMode in [lapAutoAdjustWithoutHorizontalScrolling, lapAutoAdjustForDPI] then
  begin
    //FButtonHeight := round(FButtonHeight * AYProportion);
    if Assigned(FHeader) then FHeader.Height := round(FHeader.Height * AYProportion);
    FChildOffset := round(FChildOffset * AXProportion);
    FCWidth := round(FCWidth * AXProportion);
    FCHeight := round(FCHeight * AYProportion);
    FAWidth := round(FAWidth * AXProportion);
    FAHeight := round(FAHeight * AYProportion);
  end;
end;

{$IFDEF LCLVER_2010000_OR_ABOVE}
procedure TJppCustomFlipPanel.FixDesignFontsPPI(const ADesignTimePPI: Integer);
begin
  inherited;
  //DoFixDesignFontPPI(FButtonFont, ADesignTimePPI);
  if Assigned(FHeader) then
    DoFixDesignFontPPI(FHeader.Font, ADesignTimePPI);
end;
{$ENDIF}

procedure TJppCustomFlipPanel.ScaleFontsPPI(
  {$IFDEF LCLVER_1080100_OR_ABOVE}const AToPPI: Integer;{$ENDIF}
  const AProportion: Double);
begin
  inherited;
  //DoScaleFontPPI(FButtonFont, AToPPI, AProportion);
  if Assigned(FHeader) then
    DoScaleFontPPI(FHeader.Font, AToPPI, AProportion);
end;
{$ENDIF}

{$endregion TJppCustomFlipPanel}



{$Region '                      TJppFlipPanelHeader                             '}
constructor TJppFlipPanelHeader.Create(AOwner: TComponent);
begin
  inherited Create;
  FOwner := AOwner;

  FChangeLink := TChangeLink.Create;
  FChangeLink.OnChange := DoChangeLink;
  FImageIndexExpanded := 0;
  FImageIndexCollapsed := 1;
  FImageMargin := 5;
  FImagePosDeltaYExpanded := 0;
  FImagePosDeltaYCollapsed := 0;

  FHeight := 22;
  FOnHeightChanged := nil;
  if Assigned(AOwner) then FCaption := AOwner.Name;
  FCaptionCollapsed := '';
  FCaptionAlignment := taLeftJustify;
  FCaptionPosDeltaX := 5;
  FCaptionPosDeltaY := 0;
  FBackgroundColor := $00DFDFDF;
  FBackgroundColorTo := clNone;
  FHotBackgroundColor := $00D4D4D4;
  FHotBackgroundColorTo := clNone;
  FHotCaptionColor := clNone;
  FBorderColor := clGray;
  FHotBorderColor := clNone;
  FHideBottomBorderWhenCollapsed := False;

  FFont := TFont.Create;
  FFont.OnChange := PropsChanged;

  FCaptionShadow := TJppTextShadowParams.Create(AOwner);
  FCaptionShadow.OnChange := PropsChanged;
  FCaptionHotShadow := TJppTextShadowParams.Create(AOwner);
  FCaptionHotShadow.OnChange := PropsChanged;

  FRightCaption := '';
  FRightCaptionVisible := True;
  FRightCaptionFont := TFont.Create;
  FRightCaptionFont.OnChange := PropsChanged;
  FRightCaptionEllipsis := epEndEllipsis;
end;

destructor TJppFlipPanelHeader.Destroy;
begin
  FFont.Free;
  FChangeLink.Free;
  FCaptionShadow.Free;
  FCaptionHotShadow.Free;
  FRightCaptionFont.Free;
  inherited;
end;

procedure TJppFlipPanelHeader.DoChangeLink(Sender: TObject);
begin
  PropsChanged(Self);
end;

procedure TJppFlipPanelHeader.Assign(const Source: TJppFlipPanelHeader; bCopyCaption: Boolean = False; bCopyRightCaption: Boolean = False);
begin
  FHeight := Source.Height;

  if bCopyCaption then
  begin
    FCaption := Source.Caption;
    FCaptionCollapsed := Source.CaptionCollapsed;
  end;
  FCaptionAlignment := Source.CaptionAlignment;
  FCaptionPosDeltaX := Source.CaptionPosDeltaX;
  FCaptionPosDeltaY := Source.CaptionPosDeltaY;

  FBackgroundColor := Source.BackgroundColor;
  FBackgroundColorTo := Source.BackgroundColorTo;
  FHotBackgroundColor := Source.HotBackgroundColor;
  FHotBackgroundColorTo := Source.HotBackgroundColorTo;

  FHotCaptionColor := Source.HotCaptionColor;

  FBorderColor := Source.BorderColor;
  FHotBorderColor := Source.HotBorderColor;
  FHideBottomBorderWhenCollapsed := Source.HideBottomBorderWhenCollapsed;

  FFont.Assign(Source.Font);

  FImages := Source.Images;
  FImageIndexExpanded := Source.ImageIndexExpanded;
  FImageIndexCollapsed := Source.ImageIndexCollapsed;
  FImageMargin := Source.ImageMargin;
  FImagePosDeltaYExpanded := Source.ImagePosDeltaYExpanded;
  FImagePosDeltaYCollapsed := Source.ImagePosDeltaYCollapsed;

  FCaptionShadow.Assign(Source.CaptionShadow);
  FCaptionHotShadow.Assign(Source.CaptionHotShadow);

  if bCopyRightCaption then FRightCaption := Source.RightCaption;
  FRightCaptionVisible := Source.RightCaptionVisible;
  FRightCaptionFont.Assign(Source.RightCaptionFont);
  FRightCaptionEllipsis := Source.RightCaptionEllipsis;
end;

procedure TJppFlipPanelHeader.PropsChanged(Sender: TObject);
begin
  if Assigned(OnChange) then OnChange(Self);
end;

procedure TJppFlipPanelHeader.SetBackgroundColor(const Value: TColor);
begin
  if FBackgroundColor = Value then Exit;
  FBackgroundColor := Value;
  PropsChanged(Self);
end;

procedure TJppFlipPanelHeader.SetBackgroundColorTo(const Value: TColor);
begin
  if FBackgroundColorTo = Value then Exit;
  FBackgroundColorTo := Value;
  PropsChanged(Self);
end;

procedure TJppFlipPanelHeader.SetBorderColor(const Value: TColor);
begin
  if FBorderColor = Value then Exit;
  FBorderColor := Value;
  PropsChanged(Self);
end;

procedure TJppFlipPanelHeader.SetCaption(const Value: string);
begin
  if FCaption = Value then Exit;
  FCaption := Value;
  PropsChanged(Self);
end;

procedure TJppFlipPanelHeader.SetCaptionAlignment(const Value: TAlignment);
begin
  if FCaptionAlignment = Value then Exit;
  FCaptionAlignment := Value;
  PropsChanged(Self);
end;

procedure TJppFlipPanelHeader.SetCaptionCollapsed(const Value: string);
begin
  if FCaptionCollapsed = Value then Exit;
  FCaptionCollapsed := Value;
  PropsChanged(Self);
end;

procedure TJppFlipPanelHeader.SetCaptionPosDeltaX(const Value: ShortInt);
begin
  if FCaptionPosDeltaX = Value then Exit;
  FCaptionPosDeltaX := Value;
  PropsChanged(Self);
end;

procedure TJppFlipPanelHeader.SetCaptionPosDeltaY(const Value: ShortInt);
begin
  if FCaptionPosDeltaY = Value then Exit;
  FCaptionPosDeltaY := Value;
  PropsChanged(Self);
end;

procedure TJppFlipPanelHeader.SetCaptionShadow(const Value: TJppTextShadowParams);
begin
  FCaptionShadow := Value;
  PropsChanged(Self);
end;

procedure TJppFlipPanelHeader.SetCaptionHotShadow(const Value: TJppTextShadowParams);
begin
  FCaptionHotShadow := Value;
  PropsChanged(Self);
end;

procedure TJppFlipPanelHeader.SetFont(const Value: TFont);
begin
  if Assigned(FFont) then FFont.Assign(Value);
  PropsChanged(Self);
end;

procedure TJppFlipPanelHeader.SetHeight(const Value: SmallInt);
begin
  if FHeight = Value then Exit;
  FHeight := Value;
  if Assigned(FOnHeightChanged) then FOnHeightChanged(Self);
end;

procedure TJppFlipPanelHeader.SetHideBottomBorderWhenCollapsed(const Value: Boolean);
begin
  if FHideBottomBorderWhenCollapsed = Value then Exit;
  FHideBottomBorderWhenCollapsed := Value;
  PropsChanged(Self);
end;

procedure TJppFlipPanelHeader.SetHotBackgroundColor(const Value: TColor);
begin
  if FHotBackgroundColor = Value then Exit;
  FHotBackgroundColor := Value;
  PropsChanged(Self);
end;

procedure TJppFlipPanelHeader.SetHotBackgroundColorTo(const Value: TColor);
begin
  if FHotBackgroundColorTo = Value then Exit;
  FHotBackgroundColorTo := Value;
  PropsChanged(Self);
end;

procedure TJppFlipPanelHeader.SetHotBorderColor(const Value: TColor);
begin
  if FHotBorderColor = Value then Exit;
  FHotBorderColor := Value;
  PropsChanged(Self);
end;

procedure TJppFlipPanelHeader.SetHotCaptionColor(const Value: TColor);
begin
  if FHotCaptionColor = Value then Exit;
  FHotCaptionColor := Value;
  PropsChanged(Self);
end;

procedure TJppFlipPanelHeader.SetImageIndexCollapsed(const Value: integer);
begin
  if FImageIndexCollapsed = Value then Exit;
  FImageIndexCollapsed := Value;
  PropsChanged(Self);
end;

procedure TJppFlipPanelHeader.SetImageIndexExpanded(const Value: integer);
begin
  if FImageIndexExpanded = Value then Exit;
  FImageIndexExpanded := Value;
  PropsChanged(Self);
end;

procedure TJppFlipPanelHeader.SetImageMargin(const Value: ShortInt);
begin
  if FImageMargin = Value then Exit;
  FImageMargin := Value;
  PropsChanged(Self);
end;

procedure TJppFlipPanelHeader.SetImagePosDeltaYCollapsed(const Value: ShortInt);
begin
  if FImagePosDeltaYCollapsed = Value then Exit;
  FImagePosDeltaYCollapsed := Value;
  PropsChanged(Self);
end;

procedure TJppFlipPanelHeader.SetImagePosDeltaYExpanded(const Value: ShortInt);
begin
  if FImagePosDeltaYExpanded = Value then Exit;
  FImagePosDeltaYExpanded := Value;
  PropsChanged(Self);
end;

procedure TJppFlipPanelHeader.SetImages(const Value: TCustomImageList);
begin
  ReplaceImageListReference(FOwner, Value, FImages, FChangeLink);
  PropsChanged(Self);
end;

procedure TJppFlipPanelHeader.SetOnChange(const Value: TNotifyEvent);
begin
  FOnChange := Value;
end;

procedure TJppFlipPanelHeader.SetOnHeightChanged(const Value: TNotifyEvent);
begin
  FOnHeightChanged := Value;
end;

procedure TJppFlipPanelHeader.SetRightCaption(const Value: string);
begin
  if FRightCaption = Value then Exit;
  FRightCaption := Value;
  PropsChanged(Self);
end;

procedure TJppFlipPanelHeader.SetRightCaptionEllipsis(const Value: TEllipsisPosition);
begin
  if FRightCaptionEllipsis = Value then Exit;
  FRightCaptionEllipsis := Value;
  PropsChanged(Self);
end;

procedure TJppFlipPanelHeader.SetRightCaptionFont(const Value: TFont);
begin
  if Assigned(FRightCaptionFont) and Assigned(Value) then
  begin
    FRightCaptionFont.Assign(Value);
    PropsChanged(Self);
  end;
end;

procedure TJppFlipPanelHeader.SetRightCaptionVisible(const Value: Boolean);
begin
  if FRightCaptionVisible = Value then Exit;
  FRightCaptionVisible := Value;
  PropsChanged(Self);
end;

{$endregion TJppFlipPanelHeader}


end.
