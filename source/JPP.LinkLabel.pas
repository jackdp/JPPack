unit JPP.LinkLabel;


interface

uses
  Winapi.Windows, System.SysUtils, System.Classes, Vcl.Controls, Vcl.Graphics, Winapi.Messages, Vcl.StdCtrls, Vcl.Buttons, Vcl.GraphUtil, Vcl.Dialogs,
  JPP.Types, JPP.Graphics, JPP.Common, ActnList, ShellApi, Themes, System.UITypes
  ;

type

//  TtLabel = class(TCustomLabel)
//  published
//    property Align;
//    property Alignment;
//    property Anchors;
//    property AutoSize;
//    property BiDiMode;
//    property Caption;
//    property Color nodefault;
//    property Constraints;
//    property DragCursor;
//    property DragKind;
//    property DragMode;
//    property EllipsisPosition;
//    property Enabled;
//    property FocusControl;
//    property Font;
//    property GlowSize; // Windows Vista only
//    property ParentBiDiMode;
//    property ParentColor;
//    property ParentFont;
//    property ParentShowHint;
//    property PopupMenu;
//    property ShowAccelChar;
//    property ShowHint;
//    property Touch;
//    property Transparent;
//    property Layout;
//    property Visible;
//    property WordWrap;
//    property OnClick;
//    property OnContextPopup;
//    property OnDblClick;
//    property OnDragDrop;
//    property OnDragOver;
//    property OnEndDock;
//    property OnEndDrag;
//    property OnGesture;
//    property OnMouseActivate;
//    property OnMouseDown;
//    property OnMouseMove;
//    property OnMouseUp;
//    property OnMouseEnter;
//    property OnMouseLeave;
//    property OnStartDock;
//    property OnStartDrag;
//  end;

  TJppLinkLabelTagExt = class(TJppTagExt);

  TJppLinkLabelClickActionType = (catGoToURL, catExecuteAction);


  TJppLinkLabel = class(TCustomLabel)

  private
    FTagExt: TJppLinkLabelTagExt;
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

    //class constructor Create;
    procedure SetTagExt(const Value: TJppLinkLabelTagExt);
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

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  protected
    procedure CmMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CmMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure Click; override;
    procedure SetEnabled(Value: Boolean); override;
    property Font;


    procedure FontNormalChange(Sender: TObject);
    procedure FontVisitedChange(Sender: TObject);
  published
    property TagExt: TJppLinkLabelTagExt read FTagExt write SetTagExt;
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
    property EllipsisPosition;

    property FocusControl;
    //property Font;
    property GlowSize; // Windows Vista only
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont default False;
    property ParentShowHint;
    property PopupMenu;
    property ShowAccelChar;
    property ShowHint;
    property Touch;
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
    property OnGesture;
    property OnMouseActivate;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnStartDock;
    property OnStartDrag;

    {$IF RTLVersion > 23} property StyleElements; {$IFEND}

  end;

//  TJppLinkLabelStyleHook = class(TStyleHook)
//  strict protected
//    procedure WndProc(var Message: TMessage); override;
//  public
//    constructor Create(AControl: TWinControl); override;
//  end;


procedure SetJppLinkLabelFonts(lbl: TJppLinkLabel; FontName: string = 'Segoe UI'; FontSize: integer = 8);
procedure SetJppLinkLabelColors(lbl: TJppLinkLabel; clNormal, clHot, clDisabled, clVisitedNormal, clVisitedHot: TColor); overload;
procedure SetJppLinkLabelColors(lbl: TJppLinkLabel; Color: TColor); overload;
procedure Register;


implementation


procedure Register;
begin
  RegisterComponents(JPPackPageName, [TJppLinkLabel]);
end;

procedure SetJppLinkLabelColors(lbl: TJppLinkLabel; clNormal, clHot, clDisabled, clVisitedNormal, clVisitedHot: TColor);
begin
  lbl.FontNormal.Color := clNormal;
  lbl.FontHot.Color := clHot;
  lbl.FontDisabled.Color := clDisabled;
  lbl.FontVisitedNormal.Color := clVisitedNormal;
  lbl.FontVisitedHot.Color := clVisitedHot;
end;

procedure SetJppLinkLabelColors(lbl: TJppLinkLabel; Color: TColor); overload;
begin
  SetJppLinkLabelColors(lbl, Color, Color, Color, Color, Color);
end;

procedure SetJppLinkLabelFonts(lbl: TJppLinkLabel; FontName: string = 'Segoe UI'; FontSize: integer = 8);
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



{ TJppLinkLabel }

//class constructor TJppLinkLabel.Create;
//begin
//  TCustomStyleEngine.RegisterStyleHook(TJppLinkLabel, TJppLinkLabelStyleHook);
//end;


constructor TJppLinkLabel.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEnabled := True;
  FVisited := False;
  FTagExt := TJppLinkLabelTagExt.Create(Self);

  FFontNormal := TFont.Create;
  FFontHot := TFont.Create;
  FFontDisabled := TFont.Create;
  FFontVisitedNormal := TFont.Create;
  FFontVisitedHot := TFont.Create;

  FFontNormal.Color := clBlue;
  FFontNormal.Style := [];
  Font.Assign(FFontNormal);
  FFontNormal.OnChange := FontNormalChange;


  FFontHot.Color := clBlue;
  FFontHot.Style := [fsUnderline];

  FFontDisabled.Color := clBtnShadow; // clNavy;
  FFontDisabled.Style := [fsUnderline];

  FFontVisitedNormal.Color := clPurple;
  FFontVisitedNormal.Style := FFontNormal.Style;
  FFontVisitedNormal.OnChange := FontVisitedChange;

  FFontVisitedHot.Color := FFontVisitedNormal.Color;
  FFontVisitedHot.Style := FFontHot.Style;



  FCursorHot := crHandPoint;
  Cursor := FCursorHot;
  FCursorDisabled := crDefault;
end;



destructor TJppLinkLabel.Destroy;
begin
  FTagExt.Free;
  FFontHot.Free;
  FFontNormal.Free;
  FFontDisabled.Free;
  FFontVisitedNormal.Free;
  FFontVisitedHot.Free;
  inherited;
end;


procedure TJppLinkLabel.SetEnabled(Value: Boolean);
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


procedure TJppLinkLabel.FontNormalChange(Sender: TObject);
begin
  inherited;
  if FEnabled then
    if Visited then Font.Assign(FFontVisitedNormal) else Font.Assign(FFontNormal);
end;

procedure TJppLinkLabel.FontVisitedChange(Sender: TObject);
begin
  inherited;
  if FEnabled and FVisited then Font.Assign(FFontVisitedNormal);
end;

procedure TJppLinkLabel.Click;
begin
  inherited;


  if csDesigning in ComponentState then Exit;
  if not FEnabled then Exit;

  if (FClickActionType = catGoToURL) and (FURL <> '') then
  begin
    FVisited := True;
    Font.Assign(FFontVisitedHot);
    ShellExecute(0, 'open', PChar(FURL), '', '', SW_SHOW);
  end
  else if FClickActionType = catExecuteAction then
    if Assigned(FAction) then
    begin
      FVisited := True;
      Font.Assign(FFontVisitedHot);
      FAction.Execute;
    end;

end;


procedure TJppLinkLabel.CmMouseEnter(var Msg: TMessage);
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

procedure TJppLinkLabel.CmMouseLeave(var Msg: TMessage);
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



procedure TJppLinkLabel.SetAction(const Value: TAction);
begin
  FAction := Value;
end;

procedure TJppLinkLabel.SetClickActionType(const Value: TJppLinkLabelClickActionType);
begin
  FClickActionType := Value;
end;


procedure TJppLinkLabel.SetCursorDisabled(const Value: TCursor);
begin
  FCursorDisabled := Value;
end;

procedure TJppLinkLabel.SetCursorHot(const Value: TCursor);
begin
  FCursorHot := Value;
end;

procedure TJppLinkLabel.SetFontVisitedHot(const Value: TFont);
begin
  FFontVisitedHot := Value;
end;

procedure TJppLinkLabel.SetFontDisabled(const Value: TFont);
begin
  FFontDisabled := Value;
end;

procedure TJppLinkLabel.SetFontHot(const Value: TFont);
begin
  FFontHot := Value;
end;


procedure TJppLinkLabel.SetFontNormal(const Value: TFont);
begin
  FFontNormal := Value;
end;


procedure TJppLinkLabel.SetFontVisitedNormal(const Value: TFont);
begin
  FFontVisitedNormal := Value;
end;

procedure TJppLinkLabel.SetTagExt(const Value: TJppLinkLabelTagExt);
begin
  FTagExt := Value;
end;

procedure TJppLinkLabel.SetURL(const Value: string);
begin
  FURL := Value;
end;



procedure TJppLinkLabel.SetVisited(const Value: Boolean);
begin
  FVisited := Value;
  if FEnabled then
    if FVisited then Font.Assign(FFontVisitedNormal)
    else Font.Assign(FFontNormal);
end;

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
