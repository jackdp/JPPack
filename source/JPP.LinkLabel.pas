unit JPP.LinkLabel;

{
  Jacek Pazera
  http://www.pazera-software.com
  https://github.com/jackdp
}


{$I jpp.inc}
{$IFDEF FPC} {$mode objfpc}{$H+} {$ENDIF}

interface

uses
  {$IFDEF MSWINDOWS}Windows, ShellAPI,{$ENDIF}
  Messages, SysUtils, Classes, {$IFDEF DCC}{$IFDEF HAS_SYSTEM_UITYPES}System.UITypes,{$ENDIF}{$ENDIF}
  Controls, Graphics, StdCtrls, {Buttons, GraphUtil, Dialogs,} ActnList, Themes,
  {$IFDEF FPC}LCLIntf, LCLType,{$ENDIF}
  JPP.Common, JPP.AnchoredControls;

type

  //TJppLinkLabelTagExt = class(TJppTagExt);

  TJppLinkLabelClickActionType = (catGoToURL, catExecuteAction);
  TJppOnLinkLabelClickEx = procedure(const ACaption, AURL: string; var Handled: Boolean) of object;


  {$region ' ---------- TJppCustomLinkLabel ---------- '}
  TJppCustomLinkLabel = class(TCustomLabel)
  private
    FTagExt: TJppTagExt;
    FFontHot: TFont;
    FFontNormal: TFont;
    FFontDisabled: TFont;

    FURL: string;
    FAction: TAction;
    FClickActionType: TJppLinkLabelClickActionType;
    FEnabled: Boolean;
    FCursorDisabled: TCursor;
    FCursorHot: TCursor;
    FFontVisitedNormal: TFont;
    FVisited: Boolean;
    FFontVisitedHot: TFont;
    FAnchoredControls: TJppAnchoredControls;
    FOnClickEx: TJppOnLinkLabelClickEx;

    //class constructor Create;
    procedure SetTagExt(const Value: TJppTagExt);
    procedure SetFontHot(const Value: TFont);
    procedure SetURL(const Value: string);
    procedure SetAction(const Value: TAction);
    procedure SetClickActionType(const Value: TJppLinkLabelClickActionType);
    procedure SetFontDisabled(const Value: TFont);
    procedure SetFontNormal(const Value: TFont);
    procedure SetCursorDisabled(const Value: TCursor);
    procedure SetCursorHot(const Value: TCursor);
    procedure SetFontVisitedNormal(const Value: TFont);
    procedure SetVisited(const Value: Boolean);
    procedure SetFontVisitedHot(const Value: TFont);
    procedure SetAnchoredControls(const Value: TJppAnchoredControls);

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure AssignFontParams(Src: TJppCustomLinkLabel);
  protected
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure Click; override;
    procedure SetEnabled(Value: Boolean); override;
    property Font;
    procedure FontNormalChange(Sender: TObject);
    procedure FontVisitedChange(Sender: TObject);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  published
    property TagExt: TJppTagExt read FTagExt write SetTagExt;
    property FontNormal: TFont read FFontNormal write SetFontNormal;
    property FontHot: TFont read FFontHot write SetFontHot;
    property FontDisabled: TFont read FFontDisabled write SetFontDisabled;
    property FontVisitedNormal: TFont read FFontVisitedNormal write SetFontVisitedNormal;
    property FontVisitedHot: TFont read FFontVisitedHot write SetFontVisitedHot;
    property URL: string read FURL write SetURL;
    property Action: TAction read FAction write SetAction;
    property ClickActionType: TJppLinkLabelClickActionType read FClickActionType write SetClickActionType default catGoToURL;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property CursorHot: TCursor read FCursorHot write SetCursorHot default crHandPoint;
    property CursorDisabled: TCursor read FCursorDisabled write SetCursorDisabled default crDefault;
    property Visited: Boolean read FVisited write SetVisited default False;
    property AnchoredControls: TJppAnchoredControls read FAnchoredControls write SetAnchoredControls;
    property OnClickEx: TJppOnLinkLabelClickEx read FOnClickEx write FOnClickEx;
  end;
  {$endregion TJppCustomLinkLabel}


  {$region ' ------------- TJppLinkLabel --------------- '}
  TJppLinkLabel = class(TJppCustomLinkLabel)
  published

    property TagExt;
    property FontNormal;
    property FontHot;
    property FontDisabled;
    property FontVisitedNormal;
    property FontVisitedHot;
    property URL;
    property Action;
    property ClickActionType;
    property Enabled;
    property CursorHot;
    property CursorDisabled;
    property Visited;

    {$IFDEF HAS_STYLE_ELEMENTS} property StyleElements; {$ENDIF}
    property Cursor default crHandPoint;

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
    {$IFDEF DCC} property EllipsisPosition; {$ENDIF}

    property FocusControl;
    //property Font;
    {$IFDEF DCC} property GlowSize; {$ENDIF} // Windows Vista only
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont default False;
    property ParentShowHint;
    property PopupMenu;
    property ShowAccelChar;
    property ShowHint;
    {$IFDEF DELPHI2010_OR_ABOVE} property Touch; {$ENDIF}
    property Transparent;
    property Layout;
    property Visible;
    property WordWrap;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    {$IFDEF DELPHI2010_OR_ABOVE} property OnGesture; {$ENDIF}
    {$IFDEF DCC} property OnMouseActivate; {$ENDIF}
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnStartDock;
    property OnStartDrag;
  end;
  {$endregion TJppLinkLabel}


//  TJppLinkLabelStyleHook = class(TStyleHook)
//  strict protected
//    procedure WndProc(var Message: TMessage); override;
//  public
//    constructor Create(AControl: TWinControl); override;
//  end;


procedure SetJppLinkLabelFonts(lbl: TJppCustomLinkLabel; FontName: string = 'Segoe UI'; FontSize: integer = 9);
procedure SetJppLinkLabelColors(lbl: TJppCustomLinkLabel; clNormal, clHot, clDisabled, clVisitedNormal, clVisitedHot: TColor); overload;
procedure SetJppLinkLabelColors(lbl: TJppCustomLinkLabel; Color: TColor); overload;



implementation



{$region ' ---------- helpers ---------- '}
procedure SetJppLinkLabelColors(lbl: TJppCustomLinkLabel; clNormal, clHot, clDisabled, clVisitedNormal, clVisitedHot: TColor);
begin
  lbl.FontNormal.Color := clNormal;
  lbl.FontHot.Color := clHot;
  lbl.FontDisabled.Color := clDisabled;
  lbl.FontVisitedNormal.Color := clVisitedNormal;
  lbl.FontVisitedHot.Color := clVisitedHot;
end;

procedure SetJppLinkLabelColors(lbl: TJppCustomLinkLabel; Color: TColor); overload;
begin
  SetJppLinkLabelColors(lbl, Color, Color, Color, Color, Color);
end;

procedure SetJppLinkLabelFonts(lbl: TJppCustomLinkLabel; FontName: string = 'Segoe UI'; FontSize: integer = 9);
begin
  lbl.FontNormal.Name := FontName;
  lbl.FontNormal.Size := FontSize;
  lbl.FontHot.Name := FontName;
  lbl.FontHot.Size := FontSize;
  lbl.FontDisabled.Name := FontName;
  lbl.FontDisabled.Size := FontSize;
  lbl.FontVisitedNormal.Name := FontName;
  lbl.FontVisitedNormal.Size := FontSize;
  lbl.FontVisitedHot.Name := FontName;
  lbl.FontVisitedHot.Size := FontSize;
end;
{$endregion helpers}




//class constructor TJppLinkLabel.Create;
//begin
//  TCustomStyleEngine.RegisterStyleHook(TJppLinkLabel, TJppLinkLabelStyleHook);
//end;


{$region ' --------------------------------------- TJppCustomLinkLabel --------------------------------------------------- '}

  {$region ' -------------------- Create & Destroy ---------------------- '}
constructor TJppCustomLinkLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEnabled := True;
  FVisited := False;
  FTagExt := TJppTagExt.Create(Self);

  FAnchoredControls := TJppAnchoredControls.Create(Self);

  FFontNormal := TFont.Create;
  FFontHot := TFont.Create;
  FFontDisabled := TFont.Create;
  FFontVisitedNormal := TFont.Create;
  FFontVisitedHot := TFont.Create;

  FFontNormal.Color := clBlue;
  FFontNormal.Style := [];
  Font.Assign(FFontNormal);
  FFontNormal.OnChange := {$IFDEF FPC}@{$ENDIF}FontNormalChange;

  FFontHot.Color := clBlue;
  FFontHot.Style := [fsUnderline];

  FFontDisabled.Color := clBtnShadow; // clNavy;
  FFontDisabled.Style := [fsUnderline];

  FFontVisitedNormal.Color := clPurple;
  FFontVisitedNormal.Style := FFontNormal.Style;
  FFontVisitedNormal.OnChange := {$IFDEF FPC}@{$ENDIF}FontVisitedChange;

  FFontVisitedHot.Color := FFontVisitedNormal.Color;
  FFontVisitedHot.Style := FFontHot.Style;

  FCursorHot := crHandPoint;
  Cursor := FCursorHot;
  FCursorDisabled := crDefault;
end;

destructor TJppCustomLinkLabel.Destroy;
begin
  FTagExt.Free;
  FFontHot.Free;
  FFontNormal.Free;
  FFontDisabled.Free;
  FFontVisitedNormal.Free;
  FFontVisitedHot.Free;
  FAnchoredControls.Free;
  inherited;
end;
  {$endregion Create & Destroy}


  {$region ' ------------- Mouse Enter & Leave -------------- '}
procedure TJppCustomLinkLabel.CMMouseEnter(var Msg: TMessage);
begin
  inherited;

  if FEnabled then
  begin
    if FVisited then Font.Assign(FFontVisitedHot) else Font.Assign(FFontHot);
    Cursor := FCursorHot;
  end
  else
  begin
    Font.Assign(FFontDisabled);
    Cursor := FCursorDisabled;
  end;
end;

procedure TJppCustomLinkLabel.CMMouseLeave(var Msg: TMessage);
begin
  inherited;

  if FEnabled then
  begin
    if FVisited then Font.Assign(FFontVisitedNormal) else Font.Assign(FFontNormal);
    Cursor := CursorHot;
  end
  else
  begin
    Font.Assign(FFontDisabled);
    Cursor := CursorDisabled;
  end;
end;
  {$endregion Mouse Enter & Leave}


  {$region ' --------------- Click ------------------- '}

procedure TJppCustomLinkLabel.Click;
var
  Handled: Boolean;
begin
  inherited;

  if csDesigning in ComponentState then Exit;
  if not FEnabled then Exit;

  if Assigned(FOnClickEx) then
  begin
    Handled := False;
    FOnClickEx(Caption, URL, Handled);
    if Handled then Exit;
  end;

  if (FClickActionType = catGoToURL) and (FURL <> '') then
  begin
    FVisited := True;
    Font.Assign(FFontVisitedHot);
    {$IFDEF MSWINDOWS}
    ShellExecute(0, 'open', PChar(FURL), '', '', SW_SHOW);
    {$ELSE}
      {$IFDEF FPC}
      if (Copy(FURL, 1, 4) = 'http') or (Copy(FURL, 1, 4) = 'www.') then OpenURL(FURL)
      else OpenDocument(FURL);
      {$ENDIF}
    {$ENDIF}
  end
  else if FClickActionType = catExecuteAction then
    if Assigned(FAction) then
    begin
      FVisited := True;
      Font.Assign(FFontVisitedHot);
      FAction.Execute;
    end;

end;
  {$endregion Click}


procedure TJppCustomLinkLabel.AssignFontParams(Src: TJppCustomLinkLabel);
begin
  Font.Assign(Src.Font);
  FontDisabled.Assign(Src.FontDisabled);
  FontHot.Assign(Src.FontHot);
  FontNormal.Assign(Src.FontNormal);
  FontVisitedHot.Assign(Src.FontVisitedHot);
  FontVisitedNormal.Assign(Src.FontVisitedNormal);
end;

procedure TJppCustomLinkLabel.SetAnchoredControls(const Value: TJppAnchoredControls);
begin
  FAnchoredControls := Value;
end;

procedure TJppCustomLinkLabel.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  if not (csDestroying in ComponentState) then
    if Assigned(FAnchoredControls) then FAnchoredControls.UpdateAllControlsPos;
end;

procedure TJppCustomLinkLabel.Notification(AComponent: TComponent; Operation: TOperation);
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

procedure TJppCustomLinkLabel.SetEnabled(Value: Boolean);
begin
  //inherited SetEnabled(Value);
  FEnabled := Value;

  if not Value then
  begin
    Font.Assign(FFontDisabled);
    Cursor := FCursorDisabled;
  end
  else
  begin
    if FVisited then Font.Assign(FFontVisitedNormal) else Font.Assign(FFontNormal);
    Cursor := FCursorHot;
  end;
end;

procedure TJppCustomLinkLabel.FontNormalChange(Sender: TObject);
begin
  inherited;
  if FEnabled then
    if Visited then Font.Assign(FFontVisitedNormal) else Font.Assign(FFontNormal);
end;

procedure TJppCustomLinkLabel.FontVisitedChange(Sender: TObject);
begin
  inherited;
  if FEnabled and FVisited then Font.Assign(FFontVisitedNormal);
end;

procedure TJppCustomLinkLabel.SetAction(const Value: TAction);
begin
  FAction := Value;
end;

procedure TJppCustomLinkLabel.SetClickActionType(const Value: TJppLinkLabelClickActionType);
begin
  FClickActionType := Value;
end;

procedure TJppCustomLinkLabel.SetCursorDisabled(const Value: TCursor);
begin
  FCursorDisabled := Value;
end;

procedure TJppCustomLinkLabel.SetCursorHot(const Value: TCursor);
begin
  FCursorHot := Value;
end;

procedure TJppCustomLinkLabel.SetFontVisitedHot(const Value: TFont);
begin
  if Assigned(FFontVisitedHot) and Assigned(Value) then FFontVisitedHot.Assign(Value);
end;

procedure TJppCustomLinkLabel.SetFontDisabled(const Value: TFont);
begin
  if Assigned(FFontDisabled) and Assigned(Value) then FFontDisabled.Assign(Value);
end;

procedure TJppCustomLinkLabel.SetFontHot(const Value: TFont);
begin
  if Assigned(FFontHot) and Assigned(Value) then FFontHot.Assign(Value);
end;

procedure TJppCustomLinkLabel.SetFontNormal(const Value: TFont);
begin
  if Assigned(FFontNormal) and Assigned(Value) then FFontNormal.Assign(Value);
end;

procedure TJppCustomLinkLabel.SetFontVisitedNormal(const Value: TFont);
begin
  if Assigned(FFontVisitedNormal) and Assigned(Value) then FFontVisitedNormal.Assign(Value);
end;

procedure TJppCustomLinkLabel.SetTagExt(const Value: TJppTagExt);
begin
  FTagExt := Value;
end;

procedure TJppCustomLinkLabel.SetURL(const Value: string);
begin
  FURL := Value;
end;

procedure TJppCustomLinkLabel.SetVisited(const Value: Boolean);
begin
  FVisited := Value;
  if FEnabled then
    if FVisited then Font.Assign(FFontVisitedNormal)
    else Font.Assign(FFontNormal);
end;


{$endregion TJppCustomLinkLabel}


{ TJppLinkLabelStyleHook }

//constructor TJppLinkLabelStyleHook.Create(AControl: TWinControl);
//begin
//  inherited;
//  OverrideEraseBkgnd := True;
//  Brush.Color := StyleServices.GetStyleColor(scWindow);
//end;
//
//procedure TJppLinkLabelStyleHook.WndProc(var Message: TMessage);
//begin
//  inherited;
//end;

end.
