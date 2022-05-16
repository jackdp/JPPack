unit JPP.ColorComboBox;

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
  Messages, SysUtils, Classes, Types, {$IFDEF DCC}{$IFDEF HAS_SYSTEM_UITYPES}System.UITypes,{$ENDIF}{$ENDIF}
  Controls, StdCtrls, Graphics, Dialogs, Buttons, Clipbrd, ExtCtrls,
  {$IFDEF FPC}LCLType, LCLIntf, LMessages,{$ENDIF}
  JPL.Strings, JPL.Conversion, JPL.Colors, JPL.ColorArrays, JPL.Rects,
  JPP.Types, JPP.Common, JPP.Common.Procs, JPP.AnchoredControls, JPP.ColorControls.Common, JPP.BasicSpeedButton, JPP.Graphics, JPP.Gradient
  ;


type

  TJppColorComboBoxItemType = (ccbitNone, ccbitSeparator, ccbitColor, ccbitChangeColor);

  TJppColorComboBoxItemData = record
    ItemType: TJppColorComboBoxItemType;
    Name: string;
    Color: TColor;
  end;

  TJppColorComboBoxAssignParams = record
    Bevel: Boolean;
    LabelPos: Boolean;
    ButtonsAppearance, ButtonsSize, ButtonsSpacing, ButtonsImages: Boolean;
    BackgroundColor: Boolean;
    Font: Boolean;
    ItemHeight: Boolean;
    Options: Boolean;
    DropDownCount: Boolean;
    Items: Boolean;
    SelectedColor: Boolean;
  end;

  TJppColorComboBoxOption = (ccboAddOnSelectIfNotExists, ccboAddAtTop);
  TJppColorComboBoxOptions = set of TJppColorComboBoxOption;

  TJppColorComboBoxBeforePaintItem = procedure(const Index: Integer; const Rect: TRect; const State: TOwnerDrawState; const ItemData: TJppColorComboBoxItemData;
    var PaintHandled: Boolean) of object;
  TJppColorComboBoxAfterPaintItem = procedure(const Index: Integer; const Rect: TRect; const State: TOwnerDrawState; const ItemData: TJppColorComboBoxItemData) of object;

  TJppColorComboBoxGetItemBackgroundColor = procedure(const Index: integer; const State: TOwnerDrawState; const ItemData: TJppColorComboBoxItemData; var BgColor: TColor) of object;
  TJppColorComboBoxGetItemGradientColors = procedure(const Index: integer; const State: TOwnerDrawState; const ItemData: TJppColorComboBoxItemData;
    var ColorFrom, ColorTo: TColor) of object;
  TJppColorComboBoxGetItemTextColor = procedure(const Index: integer; State: TOwnerDrawState; const ItemData: TJppColorComboBoxItemData; var TextColor: TColor) of object;
  TJppColorComboBoxGetNumericItemTextColor = procedure(const Index: integer; const State: TOwnerDrawState; const ItemData: TJppColorComboBoxItemData; var TextColor: TColor) of object;


  {$region ' ------------ TJppComboButton ------------ '}
  TJppComboButton = class(TJppBasicSpeedButton)
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


  {$Region ' ----------- TJppColorComboBoxAppearance ---------- '}
  TJppColorComboBoxAppearance = class(TJppColorControlAppearance)
  private
    FOwner: TComponent;
    FChangeColorItem: TJppColorControlGradientCaptionItem;
    FOnGetColorStrValue: TJppColorControlGetColorStrValue;
    procedure SetChangeColorItem(const Value: TJppColorControlGradientCaptionItem);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(ccba: TJppColorComboBoxAppearance); reintroduce;
  published
    property ChangeColorItem: TJppColorControlGradientCaptionItem read FChangeColorItem write SetChangeColorItem;
  end;
  {$endregion TJppColorComboBoxAppearance}


  {$region ' ----------- TJppCustomColorComboBox ---------- '}
  TJppCustomColorComboBox = class(TCustomComboBox)
  private
    FUpdateCounter: integer;
    FBoundLabel: TJppControlBoundLabel;
    FBoundLabelPosition: TLabelPosition;
    FBoundLabelSpacing: Integer;
    FButtonCopyColor: TJppComboButton;
    FButtonChangeColor: TJppComboButton;
    FButtonsSpacing: integer;
    FButtonPasteColor: TJppComboButton;
    FTagExt: TJppTagExt;
    FNoneColor: TColor;
    FColorListSet: TColorListSet;
    FOptions: TJppColorComboBoxOptions;
    FAppearance: TJppColorComboBoxAppearance;
    FSelected: TColor;
    {$IFDEF DCC}FColorDialogOptions: TColorDialogOptions;{$ENDIF}
    FOnSelectSeparatorItem: TJppColorControlSelectSeparator;
    FOnColorChanged: TNotifyEvent;
    FOnSelectChangeColorItem: TJppColorControlSelectChangeColor;
    FOnGetColorStrValue: TJppColorControlGetColorStrValue;
    FOnBeforePaintItem: TJppColorComboBoxBeforePaintItem;
    FOnAfterPaintItem: TJppColorComboBoxAfterPaintItem;
    FOnGetItemBackgroundColor: TJppColorComboBoxGetItemBackgroundColor;
    FOnGetItemGradientColors: TJppColorComboBoxGetItemGradientColors;
    FOnGetItemTextColor: TJppColorComboBoxGetItemTextColor;
    FOnGetNumericItemTextColor: TJppColorComboBoxGetNumericItemTextColor;
    FButtonsAlignment: TVerticalAlignment;
    FAnchoredControls: TJppAnchoredControls;
    procedure ButtonCopyColorClick(Sender: TObject);
    procedure ButtonPasteColorClick(Sender: TObject);
    procedure ButtonChangeColorClick(Sender: TObject);
    procedure SetBoundLabelPosition(const Value: TLabelPosition);
    procedure SetBoundLabelSpacing(const Value: Integer);
    procedure SetButtonCopyColorPosition;
    procedure SetButtonChangeColorPosition;
    procedure SetButtonPasteColorPosition;
    procedure SetButtonsSpacing(const Value: integer);
    procedure ButtonPositionChanged(Sender: TObject);
    procedure AdjustLabelBounds(Sender: TObject);
    procedure SetTagExt(const Value: TJppTagExt);
    procedure SetNoneColor(const Value: TColor);
    procedure SetColorListSet(const Value: TColorListSet);
    procedure SetOptions(const Value: TJppColorComboBoxOptions);
    procedure SetAppearance(const Value: TJppColorComboBoxAppearance);
    {$IFDEF DCC}procedure SetColorDialogOptions(const Value: TColorDialogOptions);{$ENDIF}
    procedure SetOnSelectSeparatorItem(const Value: TJppColorControlSelectSeparator);
    procedure SetOnColorChanged(const Value: TNotifyEvent);
    procedure SetOnSelectChangeColorItem(const Value: TJppColorControlSelectChangeColor);
    procedure SetOnGetColorStrValue(const Value: TJppColorControlGetColorStrValue);
    procedure SetOnBeforePaintItem(const Value: TJppColorComboBoxBeforePaintItem);
    procedure SetOnAfterPaintItem(const Value: TJppColorComboBoxAfterPaintItem);
    procedure SetOnGetItemBackgroundColor(const Value: TJppColorComboBoxGetItemBackgroundColor);
    procedure SetOnGetItemGradientColors(const Value: TJppColorComboBoxGetItemGradientColors);
    procedure SetOnGetItemTextColor(const Value: TJppColorComboBoxGetItemTextColor);
    procedure SetOnGetNumericItemTextColor(const Value: TJppColorComboBoxGetNumericItemTextColor);
    procedure SetButtonsAlignment(const Value: TVerticalAlignment);
    function GetButtonTopPosition(Button: TJppComboButton): integer;
    procedure SetAnchoredControls(const Value: TJppAnchoredControls);
  protected
    procedure SetParent(AParent: TWinControl); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMBiDiModeChanged(var Message: TMessage); message CM_BIDIMODECHANGED;
    procedure PropsChanged(Sender: TObject);
    procedure RecreateItems;
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateWnd; override;

    procedure AssignParams(
      ccb: TJppCustomColorComboBox; bBevel: Boolean = True; bLabelPos: Boolean = True;
      bButtonsAppearance: Boolean = True; bButtonsSize: Boolean = True; bButtonsSpacing: Boolean = True;
      bButtonsImages: Boolean = True; bBackgroundColor: Boolean = True; bFontParams: Boolean = True; bItemHeight: Boolean = True; bOptions: Boolean = True;
      bDropDownCount: Boolean = True; bItems: Boolean = True; bSelectedColor: Boolean = True
    ); overload;
    procedure AssignParams(ccb: TJppCustomColorComboBox; Params: TJppColorComboBoxAssignParams); overload;

    procedure BeginUpdate;
    procedure EndUpdate(bCallOnChangeAfterUpdate: Boolean = True; bResetUpdatingState: Boolean = False);
    function UpdatingControl: Boolean;

    ////////////////////////////////////////////////////////////////////////////////////////
    procedure DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
    ////////////////////////////////////////////////////////////////////////////////////////

    //procedure MeasureItem(Index: Integer; var Height: Integer); override;
    procedure GetColorFromStr(sVal: string; var ColorName: string; var Color: TColor);
    function GetSelectedColor: TColor;
    procedure SetSelectedColor(const Value: TColor);

    procedure Change; override;
    procedure ShowColorDlg;
    procedure SetupInternalLabel;
    procedure SetupButtonCopyColor;
    procedure SetupButtonChangeColor;
    procedure SetupButtonPasteColor;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer; AHeight: Integer); override;
    procedure SetButtonsPosition;

    function IsSeparatorItem(const Index: integer): Boolean;
    function IsChangeColorItem(const Index: integer): Boolean;
    function IsColorItem(const Index: integer): Boolean;

    function ColorExists(const Color: TColor): Boolean;

    function AddColor(const Color: TColor; const ColorName: string): integer; overload;
    function AddColor(const ColorName: string; const Color: TColor): integer; overload; // for compatibility with older version
    procedure InsertColor(const Index: integer; const Color: TColor; const ColorName: string); overload;
    procedure InsertColor(const Index: integer; const ColorName: string; const Color: TColor); overload;

    function AddChangeColorItem(bUseDefaultCaption: Boolean = True; ItemCaption: string = 'Change color...'): integer;
    procedure InsertChangeColorItem(const Index: integer; bUseDefaultCaption: Boolean = True; ItemCaption: string = 'Change color...');
    function AddSeparatorItem(const Caption: string): integer;
    procedure InsertSeparatorItem(const Index: integer; const Caption: string);

    procedure AddColorsFromArray(const Arr: array of TColorArrayItem; GroupName: string = ''; bAddSeparator: Boolean = False);

    function GetItemType(const Index: integer): TJppColorComboBoxItemType;
    procedure GetItemInfo(const Index: integer; out ItemData: TJppColorComboBoxItemData);
    function GetItemColor(const Index: integer): TColor;
    function GetItemName(const Index: integer): string;
    function GetColorIndex(const Color: TColor; bFirst: Boolean = True): integer;
    function AsColorArray: TArray<TColorArrayItem>;

    procedure UpdateColorObject(const Index: integer; bForce: Boolean = False);
    procedure UpdateAllColorObjects;

  //published
    property SelectedColor: TColor read GetSelectedColor write SetSelectedColor;

    property BoundLabel: TJppControlBoundLabel read FBoundLabel;
    property BoundLabelPosition: TLabelPosition read FBoundLabelPosition write SetBoundLabelPosition default lpLeft;
    property BoundLabelSpacing: Integer read FBoundLabelSpacing write SetBoundLabelSpacing default 4;

    property ButtonsSpacing: integer read FButtonsSpacing write SetButtonsSpacing;
    property ButtonChangeColor: TJppComboButton read FButtonChangeColor;
    property ButtonCopyColor: TJppComboButton read FButtonCopyColor;
    property ButtonPasteColor: TJppComboButton read FButtonPasteColor;
    property ButtonsAlignment: TVerticalAlignment read FButtonsAlignment write SetButtonsAlignment default taAlignTop;

    property Appearance: TJppColorComboBoxAppearance read FAppearance write SetAppearance;
    property TagExt: TJppTagExt read FTagExt write SetTagExt;
    property NoneColor: TColor read FNoneColor write SetNoneColor default clNone;
    property ColorListSet: TColorListSet read FColorListSet write SetColorListSet default [cltBasic];
    property Options: TJppColorComboBoxOptions read FOptions write SetOptions default [ccboAddOnSelectIfNotExists, ccboAddAtTop];
    {$IFDEF DCC}property ColorDialogOptions: TColorDialogOptions read FColorDialogOptions write SetColorDialogOptions default [cdFullOpen, cdAnyColor];{$ENDIF}

    property OnColorChanged: TNotifyEvent read FOnColorChanged write SetOnColorChanged;
    property OnSelectSeparatorItem: TJppColorControlSelectSeparator read FOnSelectSeparatorItem write SetOnSelectSeparatorItem;
    property OnSelectChangeColorItem: TJppColorControlSelectChangeColor read FOnSelectChangeColorItem write SetOnSelectChangeColorItem;
    property OnGetColorStrValue: TJppColorControlGetColorStrValue read FOnGetColorStrValue write SetOnGetColorStrValue;
    property OnBeforePaintItem: TJppColorComboBoxBeforePaintItem read FOnBeforePaintItem write SetOnBeforePaintItem;
    property OnAfterPaintItem: TJppColorComboBoxAfterPaintItem read FOnAfterPaintItem write SetOnAfterPaintItem;

    property OnGetItemBackgroundColor: TJppColorComboBoxGetItemBackgroundColor read FOnGetItemBackgroundColor write SetOnGetItemBackgroundColor;
    property OnGetItemGradientColors: TJppColorComboBoxGetItemGradientColors read FOnGetItemGradientColors write SetOnGetItemGradientColors;
    property OnGetItemTextColor: TJppColorComboBoxGetItemTextColor read FOnGetItemTextColor write SetOnGetItemTextColor;
    property OnGetNumericItemTextColor: TJppColorComboBoxGetNumericItemTextColor read FOnGetNumericItemTextColor write SetOnGetNumericItemTextColor;

    property AnchoredControls: TJppAnchoredControls read FAnchoredControls write SetAnchoredControls;

  end;
  {$endregion TJppCustomColorComboBox}


  {$Region ' ---------- TJppColorComboBox ---------- '}
  TJppColorComboBox = class(TJppCustomColorComboBox)
    property Align;
    property AutoComplete default True;
    {$IFDEF DCC}property AutoCompleteDelay default 500;{$ENDIF}
    property AutoDropDown default False;
    {$IFDEF DCC}
    property AutoCloseUp default False;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    {$ENDIF}
    property Style; // Must be published before Items
    property Anchors;
    property BiDiMode;
    property CharCase;
    property Color;
    property Constraints;
    {$IFDEF DCC}property Ctl3D;{$ENDIF}
    property DoubleBuffered;
    property DragCursor;
    property DragKind;
    property DragMode;
    property DropDownCount;
    property Enabled;
    property Font;
    {$IFDEF DCC}property ImeMode;{$ENDIF}
    {$IFDEF DCC}property ImeName;{$ENDIF}
    property ItemHeight;
    property ItemIndex default -1;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    {$IFDEF DCC}property ParentCtl3D;{$ENDIF}
    {$IFDEF DCC}property ParentDoubleBuffered;{$ENDIF}
    {$IFDEF FPC}{$IFDEF HAS_WINCONTROL_WITH_PARENTDOUBLEBUFFERED}
    property ParentDoubleBuffered;
    {$ENDIF}{$ENDIF}
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    //property Text;
    {$IFDEF DCC}property TextHint;{$ENDIF}
    {$IFDEF DELPHI2010_OR_ABOVE}property Touch;{$ENDIF}
    property Visible;
    {$IFDEF DCC}{$IF RTLVersion > 23} property StyleElements; {$IFEND}{$ENDIF}
    property OnChange;
    property OnClick;
    property OnCloseUp;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    {$IFDEF DELPHI2010_OR_ABOVE}property OnGesture;{$ENDIF}
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnSelect;
    property OnStartDock;
    property OnStartDrag;
    property Items; // Must be published after OnMeasureItem

    property SelectedColor;

    property BoundLabel;
    property BoundLabelPosition;
    property BoundLabelSpacing;

    property ButtonsSpacing;
    property ButtonChangeColor;
    property ButtonCopyColor;
    property ButtonPasteColor;
    property ButtonsAlignment;

    property Appearance;
    property TagExt;
    property NoneColor;
    property ColorListSet;
    property Options;
    {$IFDEF DCC}property ColorDialogOptions;{$ENDIF}

    property OnSelectSeparatorItem;
    property OnSelectChangeColorItem;
    property OnColorChanged;
    property OnGetColorStrValue;
    property OnBeforePaintItem;
    property OnAfterPaintItem;
    property OnGetItemBackgroundColor;
    property OnGetItemGradientColors;
    property OnGetItemTextColor;
    property OnGetNumericItemTextColor;
    {$IFDEF FPC}
    property BorderSpacing;
    {$ENDIF}
    property AnchoredControls;
  end;
  {$endregion TJppColorComboBox}




procedure ResetColorComboBoxAssignParams(var ccbap: TJppColorComboBoxAssignParams; FieldsValue: Boolean = True);



implementation




procedure ResetColorComboBoxAssignParams(var ccbap: TJppColorComboBoxAssignParams; FieldsValue: Boolean = True);
begin
  ccbap.Bevel := FieldsValue;
  ccbap.LabelPos := FieldsValue;
  ccbap.ButtonsAppearance := FieldsValue;
  ccbap.ButtonsSize := FieldsValue;
  ccbap.ButtonsSpacing := FieldsValue;
  ccbap.ButtonsImages := FieldsValue;
  ccbap.BackgroundColor := FieldsValue;
  ccbap.Font := FieldsValue;
  ccbap.ItemHeight := FieldsValue;
  ccbap.Options := FieldsValue;
  ccbap.DropDownCount := FieldsValue;
  ccbap.Items := FieldsValue;
  ccbap.SelectedColor := FieldsValue;
end;



{$region ' ------------------------------------------------- TJppColorComboBox ------------------------------------------------ '}

  {$region ' ------------------- TJppColorComboBox Create / Destroy / AssignParams ----------------- '}
constructor TJppCustomColorComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Parent := TWinControl(AOwner);

  {$IFDEF FPC}Color := clWindow;{$ENDIF}

  FUpdateCounter := 0;

  FAppearance := TJppColorComboBoxAppearance.Create(Self);
  FAppearance.OnChange := PropsChanged;

  FAnchoredControls := TJppAnchoredControls.Create(Self);

  FOnSelectSeparatorItem := nil;
  FOnColorChanged := nil;
  FOnSelectChangeColorItem := nil;
  FOnGetColorStrValue := nil;
  FOnBeforePaintItem := nil;
  FOnAfterPaintItem := nil;
  FOnGetItemBackgroundColor := nil;
  FOnGetItemGradientColors := nil;
  FOnGetItemTextColor := nil;
  FOnGetNumericItemTextColor := nil;

  FTagExt := TJppTagExt.Create(Self);

  Style := csOwnerDrawVariable;
  DropDownCount := 16;

  ItemIndex := 0;

  FNoneColor := clNone;

  FBoundLabelPosition := lpLeft;
  FBoundLabelSpacing := 4;
  SetupInternalLabel;

  FButtonsSpacing := 2;
  SetupButtonChangeColor;
  SetupButtonCopyColor;
  SetupButtonPasteColor;
  FButtonsAlignment := taAlignTop;

  FOptions := [ccboAddOnSelectIfNotExists, ccboAddAtTop];
  {$IFDEF DCC}FColorDialogOptions := [cdFullOpen, cdAnyColor];{$ENDIF}

  FColorListSet := [cltBasic];
  {
  [
    cltStandard16, cltWebGrayBlack, cltWebWhite, cltWebPink, cltWebRed, cltWebOrange, cltWebYellow, cltWebBrown, cltWebGreen,
    cltWebCyan, cltWebBlue, cltWebPurpleVioletMagenta
  ];
  }

  if csDesigning in ComponentState then RecreateItems;
end;

procedure TJppCustomColorComboBox.CreateWnd;
begin
  inherited CreateWnd;
end;

destructor TJppCustomColorComboBox.Destroy;
begin
  FAppearance.Free;
  FTagExt.Free;
  FAnchoredControls.Free;
  inherited;
end;


procedure TJppCustomColorComboBox.AssignParams(ccb: TJppCustomColorComboBox; Params: TJppColorComboBoxAssignParams);
begin
  AssignParams(
    ccb,
    Params.Bevel, Params.LabelPos,
    Params.ButtonsAppearance, Params.ButtonsSize, Params.ButtonsSpacing, Params.ButtonsImages,
    Params.BackgroundColor, Params.Font, Params.ItemHeight, Params.Options, Params.DropDownCount, Params.Items, Params.SelectedColor
  );
end;

procedure TJppCustomColorComboBox.AssignParams(
  ccb: TJppCustomColorComboBox; bBevel: Boolean = True; bLabelPos: Boolean = True;
  bButtonsAppearance: Boolean = True; bButtonsSize: Boolean = True; bButtonsSpacing: Boolean = True; bButtonsImages: Boolean = True;
  bBackgroundColor: Boolean = True; bFontParams: Boolean = True; bItemHeight: Boolean = True; bOptions: Boolean = True;
  bDropDownCount: Boolean = True; bItems: Boolean = True; bSelectedColor: Boolean = True
);
begin
  BeginUpdate;
  try

    FAppearance.Assign(ccb.Appearance);

    {$IFDEF DCC}
    if bBevel then
    begin
      BevelEdges := ccb.BevelEdges;
      BevelInner := ccb.BevelInner;
      BevelKind := ccb.BevelKind;
      BevelOuter := ccb.BevelOuter;
    end;
    {$ENDIF}

    if bLabelPos then
    begin
      BoundLabelPosition := ccb.BoundLabelPosition;
      BoundLabelSpacing := ccb.BoundLabelSpacing;
    end;

    if bButtonsAppearance then
    begin
      ButtonChangeColor.Appearance.Assign(ccb.ButtonChangeColor.Appearance);
      ButtonCopyColor.Appearance.Assign(ccb.ButtonCopyColor.Appearance);
      ButtonPasteColor.Appearance.Assign(ccb.ButtonPasteColor.Appearance);
    end;

    if bButtonsSize then
    begin
      ButtonChangeColor.AutoWidth := ccb.ButtonChangeColor.AutoWidth;
      ButtonChangeColor.Width := ccb.ButtonChangeColor.Width;
      ButtonChangeColor.Height := ccb.ButtonChangeColor.Height;
      ButtonCopyColor.AutoWidth := ccb.ButtonCopyColor.AutoWidth;
      ButtonCopyColor.Width := ccb.ButtonCopyColor.Width;
      ButtonCopyColor.Height := ccb.ButtonCopyColor.Height;
      ButtonPasteColor.AutoWidth := ccb.ButtonPasteColor.AutoWidth;
      ButtonPasteColor.Width := ccb.ButtonPasteColor.Width;
      ButtonPasteColor.Height := ccb.ButtonPasteColor.Height;
    end;

    if bButtonsSpacing then ButtonsSpacing := ccb.ButtonsSpacing;

    if bButtonsImages then
    begin
      ButtonChangeColor.PngImage.Assign(ccb.ButtonChangeColor.PngImage);
      ButtonCopyColor.PngImage.Assign(ccb.ButtonCopyColor.PngImage);
      ButtonPasteColor.PngImage.Assign(ccb.ButtonPasteColor.PngImage);
    end;

    if bBackgroundColor then Self.Color := ccb.Color;
    if bFontParams then Self.Font.Assign(ccb.Font);
    if bItemHeight then Self.ItemHeight := ccb.ItemHeight;
    if bOptions then Self.Options := ccb.Options;
    if bDropDownCount then Self.DropDownCount := ccb.DropDownCount;
    if bItems then Self.Items.Assign(ccb.Items);
    if bSelectedColor then Self.SelectedColor := ccb.SelectedColor;

    SetButtonsPosition;

  finally
    EndUpdate;
  end;
end;

  {$endregion Create / Destroy / AssignParams}


procedure TJppCustomColorComboBox.SetAnchoredControls(const Value: TJppAnchoredControls);
begin
  FAnchoredControls := Value;
end;

procedure TJppCustomColorComboBox.Loaded;
begin
  inherited Loaded;
end;


procedure TJppCustomColorComboBox.SetParent(AParent: TWinControl);
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


procedure TJppCustomColorComboBox.BeginUpdate;
begin
  Inc(FUpdateCounter);
end;

procedure TJppCustomColorComboBox.EndUpdate(bCallOnChangeAfterUpdate, bResetUpdatingState: Boolean);
begin
  if bResetUpdatingState then FUpdateCounter := 0
  else
  begin
    Dec(FUpdateCounter);
    if FUpdateCounter < 0 then FUpdateCounter := 0;
  end;

  if (FUpdateCounter = 0) and bCallOnChangeAfterUpdate then PropsChanged(Self);
end;

function TJppCustomColorComboBox.UpdatingControl: Boolean;
begin
  Result := FUpdateCounter > 0;
end;



{$Region ' --------------------- Add & Insert ---------------------- '}
function TJppCustomColorComboBox.AddColor(const Color: TColor; const ColorName: string): integer;
var
  x: integer;
begin
  x := Items.Add(ColorName + '=' + ColorToRgbIntStr(ColorToRGB(Color), 3, '0', ','));
  Items.Objects[x] := TObject(Color);
  Result := x;
end;

function TJppCustomColorComboBox.AddColor(const ColorName: string; const Color: TColor): integer;
begin
  Result := AddColor(Color, ColorName);
end;

procedure TJppCustomColorComboBox.InsertColor(const Index: integer; const Color: TColor; const ColorName: string);
begin
  Items.Insert(Index, ColorName + '=' + ColorToRgbIntStr(Color, 3, '0', ','));
  Items.Objects[Index] := TObject(Color);
end;

procedure TJppCustomColorComboBox.InsertColor(const Index: integer; const ColorName: string; const Color: TColor);
begin
  InsertColor(Index, Color, ColorName);
end;

function TJppCustomColorComboBox.AddChangeColorItem(bUseDefaultCaption: Boolean = True; ItemCaption: string = 'Change color...'): integer;
begin
  if bUseDefaultCaption then ItemCaption := FAppearance.ChangeColorItem.Caption;
  Result := Items.Add('@=' + ItemCaption);
end;

procedure TJppCustomColorComboBox.InsertChangeColorItem(const Index: integer; bUseDefaultCaption: Boolean = True; ItemCaption: string = 'Change color...');
begin
  if bUseDefaultCaption then ItemCaption := FAppearance.ChangeColorItem.Caption;
  Items.Insert(Index, '@=' + ItemCaption);
end;

function TJppCustomColorComboBox.AddSeparatorItem(const Caption: string): integer;
begin
  Result := Items.Add('-=' + Caption);
end;

procedure TJppCustomColorComboBox.InsertSeparatorItem(const Index: integer; const Caption: string);
begin
  Items.Insert(Index, '-=' + Caption);
end;


procedure TJppCustomColorComboBox.AddColorsFromArray(const Arr: array of TColorArrayItem; GroupName: string; bAddSeparator: Boolean);
var
  i, x: integer;
  cai: TColorArrayItem;
begin
  Items.BeginUpdate;
  BeginUpdate;
  try

    if bAddSeparator then Items.Add('-=' + GroupName);

    for i := 0 to High(Arr) do
    begin
      cai := Arr[i];
      cai.Color := ColorToRGB(cai.Color);
      x := Items.Add(cai.Name + '=' + ColorToRgbIntStr(cai.Color, 3, '0', ','));
      Items.Objects[x] := TObject(cai.Color);
    end;

  finally
    EndUpdate;
    Items.EndUpdate;
  end;
end;
  {$endregion Add & Insert}


  {$Region ' ---------------- Change, Get & Set Selected ------------------- '}

procedure TJppCustomColorComboBox.Change;
var
  cl: TColor;
  ItemData: TJppColorComboBoxItemData;
  bHandled: Boolean;
begin
  GetItemInfo(ItemIndex, ItemData);

  case ItemData.ItemType of

    ccbitChangeColor:
      begin
        bHandled := False;
        if Assigned(FOnSelectChangeColorItem) then
        begin
          cl := FSelected;
          FOnSelectChangeColorItem(FSelected, cl, bHandled);
          if bHandled and (cl <> FNoneColor) and (cl <> FSelected) then SetSelectedColor(cl);
        end;
        if not bHandled then ShowColorDlg;
      end;

    ccbitSeparator: if Assigned(FOnSelectSeparatorItem) then FOnSelectSeparatorItem(ItemIndex, ItemData.Name);

    ccbitColor:
      begin
        cl := ItemData.Color;
        if cl <> FNoneColor then SetSelectedColor(cl);
      end;

  end; // case

  inherited Change;

end;

function TJppCustomColorComboBox.GetSelectedColor: TColor;
begin
  Result := FNoneColor;
  {$IFDEF DCC}
  if HandleAllocated then
  {$ENDIF}
    if ItemIndex <> -1 then Result := GetItemColor(ItemIndex);
end;

procedure TJppCustomColorComboBox.SetSelectedColor(const Value: TColor);
var
  x: integer;
begin
  //if Value = FSelected then Exit;

  FSelected := Value;
  x := GetColorIndex(Value);

  if x >= 0 then
  begin
    ItemIndex := x;
  end

  else if ccboAddOnSelectIfNotExists in FOptions then
  begin
    if ccboAddAtTop in FOptions then
    begin
      x := 0;
      InsertColor(x, Value, '');
    end
    else x := AddColor(Value, '');

    ItemIndex := x;
  end;

  if Assigned(FOnColorChanged) then FOnColorChanged(Self);
end;
  {$endregion Change, Get & Set Selected}


  {$Region ' --------------- Color Dialog -------------- '}
procedure TJppCustomColorComboBox.ShowColorDlg;
var
  dlgColor: TColorDialog;
begin
  dlgColor := TColorDialog.Create(Self);
  try
    {$IFDEF DCC}dlgColor.Options := FColorDialogOptions;{$ENDIF} // dlgColor.Options + [cdFullOpen, cdAnyColor];
    dlgColor.Color := FSelected;
    if not dlgColor.Execute then
    begin
      SetSelectedColor(FSelected);
      Exit;
    end;
    SetSelectedColor(dlgColor.Color);
  finally
    dlgColor.Free;
  end;
end;

{$IFDEF DCC}
procedure TJppCustomColorComboBox.SetColorDialogOptions(const Value: TColorDialogOptions);
begin
  FColorDialogOptions := Value;
end;
{$ENDIF}
  {$endregion Color Dialog}


procedure TJppCustomColorComboBox.RecreateItems;
begin
  BeginUpdate;
  try
    //ShowMessage('ColorComboBox RecreateItems');
    Items.Clear;
    AddChangeColorItem;
    if cltBasic in FColorListSet then AddColorsFromArray(Colors_Basic, 'Basic colors', True);
    if cltSystem in FColorListSet then AddColorsFromArray(Colors_System, 'System colors', True);
    if cltStandard16 in FColorListSet then AddColorsFromArray(Colors_Standard16, 'Standard 16 colors', True);
    if cltStandard48 in FColorListSet then AddColorsFromArray(Colors_Standard48, 'Standard 48 colors', True);
    if cltWin10Theme in FColorListSet then AddColorsFromArray(Colors_Win10Theme, 'Windows 10 theme colors', True);
    if cltWebGrayBlack in FColorListSet then AddColorsFromArray(Colors_Web_GrayBlack, 'Web gray and black colors', True);
    if cltWebWhite in FColorListSet then AddColorsFromArray(Colors_Web_White, 'Web white colors', True);
    if cltWebPink in FColorListSet then AddColorsFromArray(Colors_Web_Pink, 'Web pink colors', True);
    if cltWebRed in FColorListSet then AddColorsFromArray(Colors_Web_Red, 'Web red colors', True);
    if cltWebOrange in FColorListSet then AddColorsFromArray(Colors_Web_Orange, 'Web orange colors', True);
    if cltWebYellow in FColorListSet then AddColorsFromArray(Colors_Web_Yellow, 'Web yellow colors', True);
    if cltWebBrown in FColorListSet then AddColorsFromArray(Colors_Web_Brown, 'Web brown colors', True);
    if cltWebGreen in FColorListSet then AddColorsFromArray(Colors_Web_Green, 'Web green colors', True);
    if cltWebCyan in FColorListSet then AddColorsFromArray(Colors_Web_Cyan, 'Web cyan colors', True);
    if cltWebBlue in FColorListSet then AddColorsFromArray(Colors_Web_Blue, 'Web blue colors', True);
    if cltWebPurpleVioletMagenta in FColorListSet then AddColorsFromArray(Colors_Web_PurpleVioletMagenta, 'Web purple, violet, magenta colors', True);
    if cltWeb216Safe in FColorListSet then AddColorsFromArray(Colors_Web_216_Safe, 'Web 216 safe colors', True);
  finally
    EndUpdate;
  end;
end;


function TJppCustomColorComboBox.GetColorIndex(const Color: TColor; bFirst: Boolean): integer;
var
  i: integer;
  cl: TColor;
begin
  Result := -1;
  for i := 0 to Items.Count - 1 do
  begin
    if IsSeparatorItem(i) then Continue;
    if Items.Objects[i] = nil then UpdateColorObject(i);
    cl := TColor(Items.Objects[i]);
    if cl = Color then
    begin
      Result := i;
      if bFirst then Break;
    end;
  end;
end;

function TJppCustomColorComboBox.GetItemColor(const Index: integer): TColor;
var
  id: TJppColorComboBoxItemData;
begin
  GetItemInfo(Index, id);
  Result := id.Color;
end;

procedure TJppCustomColorComboBox.GetItemInfo(const Index: integer; out ItemData: TJppColorComboBoxItemData);
var
  s, sName, s1: string;
  Color: TColor;
  xp: integer;
begin
  ItemData.Color := FNoneColor;
  ItemData.Name := '';
  ItemData.ItemType := ccbitNone;
  if (Index < 0) or (Index > Items.Count - 1) then Exit;

  s := Trim(Items[Index]);
  s1 := Copy(s, 1, 1);
  if s1 = '-' then ItemData.ItemType := ccbitSeparator
  else if s1 = '@' then ItemData.ItemType := ccbitChangeColor
  else ItemData.ItemType := ccbitColor;

  xp := Pos('=', s);
  if xp <= 0 then Exit;

  if (ItemData.ItemType = ccbitSeparator) or (ItemData.ItemType = ccbitChangeColor) then
  begin
    ItemData.Name := Copy(s, xp + 1, Length(s));
    Exit;
  end;

  GetColorFromStr(s, sName, Color);
  ItemData.Name := sName;
  ItemData.Color := Color;
end;

function TJppCustomColorComboBox.GetItemName(const Index: integer): string;
var
  id: TJppColorComboBoxItemData;
begin
  GetItemInfo(Index, id);
  Result := id.Name;
end;

function TJppCustomColorComboBox.GetItemType(const Index: integer): TJppColorComboBoxItemType;
var
  Znak: Char;
  s: string;
begin
  s := Items[Index];
  if s = '' then Exit(ccbitNone);
  Znak := s[1];

  case Znak of
    '-':  Result := ccbitSeparator;
    '@': Result := ccbitChangeColor;
  else
    Result := ccbitColor;
  end;
end;


function TJppCustomColorComboBox.IsChangeColorItem(const Index: integer): Boolean;
begin
  Result := GetItemType(Index) = ccbitChangeColor;
end;

function TJppCustomColorComboBox.IsColorItem(const Index: integer): Boolean;
begin
  Result := GetItemType(Index) = ccbitColor;
end;

function TJppCustomColorComboBox.IsSeparatorItem(const Index: integer): Boolean;
begin
  Result := GetItemType(Index) = ccbitSeparator;
end;

function TJppCustomColorComboBox.ColorExists(const Color: TColor): Boolean;
begin
  Result := GetColorIndex(Color) >= 0;
end;

procedure TJppCustomColorComboBox.UpdateAllColorObjects;
var
  Color: TColor;
  i: integer;
begin
  for i := 0 to Items.Count - 1 do
  begin
    if IsSeparatorItem(i) then Exit;
    Color := GetItemColor(i);
    Items.Objects[i] := TObject(Color);
  end;
end;

procedure TJppCustomColorComboBox.UpdateColorObject(const Index: integer; bForce: Boolean);
var
  Color: TColor;
begin
  if IsSeparatorItem(Index) then Exit;

  if bForce or (Items.Objects[Index] = nil) then
  begin
    Color := GetItemColor(Index);
    Items.Objects[Index] := TObject(Color);
  end;
end;


procedure TJppCustomColorComboBox.PropsChanged(Sender: TObject);
begin
  if csLoading in ComponentState then Exit;
  Invalidate;
end;


  {$region ' -------------- misc --------------- '}

function TJppCustomColorComboBox.AsColorArray: TArray<TColorArrayItem>;
var
  i: integer;
  Color: TColor;
  ColorName: string;
begin
  SetLength(Result{%H-}, 0);
  for i := 0 to Items.Count - 1 do
  begin
    if IsSeparatorItem(i) then Continue;
    if Items.Objects[i] = nil then UpdateColorObject(i);
    Color := TColor(Items.Objects[i]);
    ColorName := GetItemName(i);
    SetLength(Result, Length(Result) + 1);
    Result[High(Result)].Color := Color;
    Result[High(Result)].Name := ColorName;
  end;
end;

procedure TJppCustomColorComboBox.SetAppearance(const Value: TJppColorComboBoxAppearance);
begin
  FAppearance := Value;
  PropsChanged(Self);
end;

procedure TJppCustomColorComboBox.SetColorListSet(const Value: TColorListSet);
begin
  FColorListSet := Value;
  if not (csLoading in ComponentState) then RecreateItems;
  PropsChanged(Self);
end;

procedure TJppCustomColorComboBox.SetNoneColor(const Value: TColor);
begin
  if FNoneColor = Value then Exit;
  FNoneColor := Value;
end;

procedure TJppCustomColorComboBox.SetOnAfterPaintItem(const Value: TJppColorComboBoxAfterPaintItem);
begin
  FOnAfterPaintItem := Value;
  PropsChanged(Self);
end;

procedure TJppCustomColorComboBox.SetOnBeforePaintItem(const Value: TJppColorComboBoxBeforePaintItem);
begin
  FOnBeforePaintItem := Value;
  PropsChanged(Self);
end;

procedure TJppCustomColorComboBox.SetOnColorChanged(const Value: TNotifyEvent);
begin
  FOnColorChanged := Value;
end;

procedure TJppCustomColorComboBox.SetOnGetColorStrValue(const Value: TJppColorControlGetColorStrValue);
begin
  FOnGetColorStrValue := Value;
  PropsChanged(Self);
end;

procedure TJppCustomColorComboBox.SetOnGetItemBackgroundColor(const Value: TJppColorComboBoxGetItemBackgroundColor);
begin
  FOnGetItemBackgroundColor := Value;
  PropsChanged(Self);
end;

procedure TJppCustomColorComboBox.SetOnGetItemGradientColors(const Value: TJppColorComboBoxGetItemGradientColors);
begin
  FOnGetItemGradientColors := Value;
  PropsChanged(Self);
end;

procedure TJppCustomColorComboBox.SetOnGetItemTextColor(const Value: TJppColorComboBoxGetItemTextColor);
begin
  FOnGetItemTextColor := Value;
  PropsChanged(Self);
end;

procedure TJppCustomColorComboBox.SetOnGetNumericItemTextColor(const Value: TJppColorComboBoxGetNumericItemTextColor);
begin
  FOnGetNumericItemTextColor := Value;
  PropsChanged(Self);
end;

procedure TJppCustomColorComboBox.SetOnSelectChangeColorItem(const Value: TJppColorControlSelectChangeColor);
begin
  FOnSelectChangeColorItem := Value;
end;

procedure TJppCustomColorComboBox.SetOnSelectSeparatorItem(const Value: TJppColorControlSelectSeparator);
begin
  FOnSelectSeparatorItem := Value;
end;

procedure TJppCustomColorComboBox.SetOptions(const Value: TJppColorComboBoxOptions);
begin
  FOptions := Value;
  PropsChanged(Self);
end;

procedure TJppCustomColorComboBox.SetTagExt(const Value: TJppTagExt);
begin
  FTagExt := Value;
end;


  {$endregion TJppColorComboBoxEx misc}


  {$region ' ------------- TJppColorComboBoxEx.GetColorFromStr --------------- '}

procedure TJppCustomColorComboBox.GetColorFromStr(sVal: string; var ColorName: string; var Color: TColor);
var
  s: string;
  xp: integer;
begin
  ColorName := '';
  Color := FNoneColor;

  s := Trim(sVal);
  xp := Pos('@@', s);
  if xp > 0 then s := Copy(s, 1, xp - 1);

  xp := Pos('=', s);
  if xp <= 0 then Exit;
  ColorName := Copy(s, 1, xp - 1);
  s := Copy(s, xp + 1, Length(s));

  if Length(s) < 3 then Exit;

  if s[1] = '$' then
  begin
    if not TryStrToInt(s, xp) then Exit;
    Color := TColor(xp);
    Exit;
  end;

  if not TryRgbStrToColor(s, Color) then
    if not TryHtmlStrToColor(s, Color) then Color := FNoneColor;

end;


  {$endregion GetColorFromStr}


  {$region ' ------------------------------ TJppColorComboBoxEx.DrawItem ---------------------------------------------- '}

procedure SplitStrToColors(s: string; out clFont, clBg, clBgTo: TColor);
var
  {$IFDEF DELPHI2009_OR_BELOW}
  Arr: TStringDynArray;
  {$ELSE}
  Arr: TArray<string>;
  {$ENDIF}
  i, xp: integer;
  sInd, sValue: string;
  cl: TColor;
begin
  SplitStrToArray(s, Arr, '@');
  clFont := clNone;
  clBg := clNone;
  clBgTo := clNone;
  for i := 0 to High(Arr) do
  begin
    sInd := UpperCase(Arr[i]);
    xp := Pos('=', sInd);
    if xp = 0 then Continue;
    sValue := Copy(sInd, xp + 1, Length(sInd));
    sInd := Copy(sInd, 1, xp - 1);
    if not TryGetColor(sValue, cl) then Continue;
    if sInd = 'FONTCOLOR' then clFont := cl
    else if sInd = 'BGCOLOR' then clBg := cl
    else if sInd = 'BGCOLORTO' then clBgTo := cl;
    //else if sInd = 'BORDERCOLOR' then
  end;
end;

procedure TJppCustomColorComboBox.DrawItem(Index: Integer; Rect: TRect; State: TOwnerDrawState);
{
  TOwnerDrawState = set of (odSelected, odGrayed, odDisabled, odChecked,
    odFocused, odDefault, odHotLight, odInactive, odNoAccel, odNoFocusRect,
    odReserved1, odReserved2, odComboBoxEdit);
}

var
  //s,
  sName, ItemStr, s2: string;
  sOut, sRgbInt, sRgbHex, sBgrHex, sUserColorStr: string;
  y, xTextStart, xTextLeft, xTextWidth, xp: integer;
  ColorRect, OutTextRect: TRect;
  clBgFrom, clBgTo, clBackground, clText, clRectangle, clBorder: TColor;
  bPrevItemIsSeparator, bSeparatorItem, bChangeColorItem, bCanDrawDataSep: Boolean;
  //bFocused,
  bSelected, bPaintHandled: Boolean;
  grd: TJppGradientEx;
  Borders: TJppBorders;
  ItemData: TJppColorComboBoxItemData;
  bCustomFontColor, bCustomBgColor, bCustomGradient: Boolean;
  CustomFontColor, CustomBgColor, CustomBgColorTo: TColor;
begin

  //////////////////////////////////////////////////////
  if UpdatingControl then Exit;
  //////////////////////////////////////////////////////

  GetItemInfo(Index, ItemData);

  bPaintHandled := False;
  if Assigned(FOnBeforePaintItem) then FOnBeforePaintItem(Index, Rect, State, ItemData, bPaintHandled);
  if bPaintHandled then Exit;

  if Assigned(OnDrawItem) then
  begin
    inherited OnDrawItem(Self, Index, Rect, State);
    Exit;
  end;

  bCustomFontColor := False;
  bCustomBgColor := False;
  bCustomGradient := False;


  //s := ItemData.Name;
  bSeparatorItem := ItemData.ItemType = ccbitSeparator; // IsSeparatorItem(Index); // Copy(s, 1, 1) = '-';
  bChangeColorItem := ItemData.ItemType = ccbitChangeColor; // IsChangeColorItem(Index);
  //bFocused := odFocused in State;
  bSelected := odSelected in State;
  sName := ItemData.Name;


  ItemStr := Items[Index];
  xp := Pos('@@', ItemStr);
  if xp > 0 then
  begin
    s2 := Copy(ItemStr, xp + 2, Length(ItemStr));
    ItemStr := Copy(ItemStr, 1, xp - 1);
    SplitStrToColors(s2, CustomFontColor, CustomBgColor, CustomBgColorTo);
    bCustomFontColor := CustomFontColor <> clNone;
    bCustomBgColor := CustomBgColor <> clNone;
    if bCustomBgColor then bCustomGradient := CustomBgColorTo <> clNone;
  end;

  xp := Pos('@@', sName);
  if xp > 0 then sName := Copy(sName, 1, xp - 1);


  bPrevItemIsSeparator := False;
  if Index > 0 then bPrevItemIsSeparator := Self.IsSeparatorItem(Index - 1);


  with Canvas do
  begin

    // clear background
    if Enabled then Brush.Color := Self.Color else Brush.Color := FAppearance.DisabledBackgroundColor;
    Pen.Color := Self.Color;
    Brush.Style := bsSolid;
    Pen.Style := psSolid;
    Rectangle(Rect);


    {$region ' ------------ DRAW: Separator Item ---------------- '}
    if bSeparatorItem then
    begin

      Pen.Style := psClear;

      // --------- Background color / gradient -------------
      if FAppearance.SeparatorItem.Background.DrawGradient or bCustomGradient then
      begin
        grd := FAppearance.SeparatorItem.Background.Gradient;
        if bCustomGradient then
        begin
          clBgFrom := CustomBgColor;
          clBgTo := CustomBgColorTo;
        end
        else
        begin
          clBgFrom := grd.ColorFrom;
          clBgTo := grd.ColorTo;
        end;
        if Assigned(FOnGetItemGradientColors) then FOnGetItemGradientColors(Index, State, ItemData, clBgFrom, clBgTo);
        cyGradientFill(Self.Canvas, Rect, clBgFrom, clBgTo, grd.Orientation, grd.Balance, grd.AngleDegree, grd.BalanceMode, grd.MaxDegrade, grd.SpeedPercent);
      end
      else
      begin
        if bCustomBgColor then clBackground := CustomBgColor else clBackground := FAppearance.SeparatorItem.Background.Color;
        if Assigned(FOnGetItemBackgroundColor) then FOnGetItemBackgroundColor(Index, State, ItemData, clBackground);
        Brush.Color := clBackground;
        Rectangle(Rect);
      end;

      // ------------- Frame ----------------
      if FAppearance.SeparatorItem.Background.DrawBorders then
      begin
        Borders := FAppearance.SeparatorItem.Background.Borders;
        if Borders.Left.Visible then DrawRectLeftBorder(Self.Canvas, Rect, Borders.Left.Color, Borders.Left.Width, Borders.Left.Style);
        if Borders.Right.Visible then DrawRectRightBorder(Self.Canvas, Rect, Borders.Right.Color, Borders.Right.Width, Borders.Right.Style, 1);
        if Borders.Top.Visible then DrawRectTopBorder(Self.Canvas, Rect, Borders.Top.Color, Borders.Top.Width, Borders.Top.Style);
        if Borders.Bottom.Visible then DrawRectBottomBorder(Self.Canvas, Rect, Borders.Bottom.Color, Borders.Bottom.Width, Borders.Bottom.Style, 1);
      end;

      // ---------- Caption --------------
      Brush.Style := bsClear;

      if sName <> '' then
      begin
        Font.Assign(FAppearance.SeparatorItem.Font);
        if bCustomFontColor then clText := CustomFontColor else clText := Font.Color;
        if not Enabled then clText := FAppearance.SeparatorItem.DisabledFontColor;
        y := GetTextMiddlePosY(Rect, TextHeight(sName)) + FAppearance.SeparatorItem.TextPosDeltaY;
        if Assigned(FOnGetItemTextColor) then FOnGetItemTextColor(Index, State, ItemData, clText);
        Font.Color := clText;

        xTextWidth := TextWidth(sName);

        case FAppearance.SeparatorItem.Alignment of
          taLeftJustify: xTextLeft := FAppearance.SeparatorItem.CaptionMargin;
          taRightJustify: xTextLeft := Rect.Width - xTextWidth - FAppearance.SeparatorItem.CaptionMargin;
          taCenter: xTextLeft := (Rect.Width div 2) - (xTextWidth div 2);
          else xTextLeft := 10;
        end;

        TextOut(xTextLeft, y, sName);
      end;

      Exit;
    end;
    {$endregion DRAW: Separator Item}



    {$region ' ------------ DRAW: ChangeColor Item ---------------- '}
    if bChangeColorItem then
    begin

      Pen.Style := psClear;

      if bSelected then
      begin
        // --------- Background color / gradient -------------
        if FAppearance.SelectedItem.Background.DrawGradient then
        begin
          grd := FAppearance.SelectedItem.Background.Gradient;
          clBgFrom := grd.ColorFrom;
          clBgTo := grd.ColorTo;
          if Assigned(FOnGetItemGradientColors) then FOnGetItemGradientColors(Index, State, ItemData, clBgFrom, clBgTo);
          cyGradientFill(Self.Canvas, Rect, clBgFrom, clBgTo, grd.Orientation, grd.Balance, grd.AngleDegree, grd.BalanceMode, grd.MaxDegrade, grd.SpeedPercent);
        end
        else
        begin
          clBackground := FAppearance.SelectedItem.Background.Color;
          if Assigned(FOnGetItemBackgroundColor) then FOnGetItemBackgroundColor(Index, State, ItemData, clBackground);
          Brush.Color := clBackground;
          Rectangle(Rect);
        end;

        // ------------- Frame ----------------
        if FAppearance.SelectedItem.Background.DrawBorders then
        begin
          Borders := FAppearance.SelectedItem.Background.Borders;
          if Borders.Left.Visible then DrawRectLeftBorder(Self.Canvas, Rect, Borders.Left.Color, Borders.Left.Width, Borders.Left.Style);
          if Borders.Right.Visible then DrawRectRightBorder(Self.Canvas, Rect, Borders.Right.Color, Borders.Right.Width, Borders.Right.Style, 1);
          if Borders.Top.Visible then DrawRectTopBorder(Self.Canvas, Rect, Borders.Top.Color, Borders.Top.Width, Borders.Top.Style);
          if Borders.Bottom.Visible then DrawRectBottomBorder(Self.Canvas, Rect, Borders.Bottom.Color, Borders.Bottom.Width, Borders.Bottom.Style, 1);
        end;
      end

      else

      // not selected
      begin

        // --------- Background color / gradient -------------
        if FAppearance.ChangeColorItem.Background.DrawGradient then
        begin
          grd := FAppearance.ChangeColorItem.Background.Gradient;
          clBgFrom := grd.ColorFrom;
          clBgTo := grd.ColorTo;
          if Assigned(FOnGetItemGradientColors) then FOnGetItemGradientColors(Index, State, ItemData, clBgFrom, clBgTo);
          cyGradientFill(Self.Canvas, Rect, clBgFrom, clBgTo, grd.Orientation, grd.Balance, grd.AngleDegree, grd.BalanceMode, grd.MaxDegrade, grd.SpeedPercent);
        end
        else
        begin
          clBackground := FAppearance.ChangeColorItem.Background.Color;
          if Assigned(FOnGetItemBackgroundColor) then FOnGetItemBackgroundColor(Index, State, ItemData, clBackground);
          Brush.Color := clBackground;
          Rectangle(Rect);
        end;

        // ------------- Frame ----------------
        if FAppearance.ChangeColorItem.Background.DrawBorders then
        begin
          Borders := FAppearance.ChangeColorItem.Background.Borders;
          if Borders.Left.Visible then DrawRectLeftBorder(Self.Canvas, Rect, Borders.Left.Color, Borders.Left.Width, Borders.Left.Style);
          if Borders.Right.Visible then DrawRectRightBorder(Self.Canvas, Rect, Borders.Right.Color, Borders.Right.Width, Borders.Right.Style, 1);
          if Borders.Top.Visible then DrawRectTopBorder(Self.Canvas, Rect, Borders.Top.Color, Borders.Top.Width, Borders.Top.Style);
          if Borders.Bottom.Visible then DrawRectBottomBorder(Self.Canvas, Rect, Borders.Bottom.Color, Borders.Bottom.Width, Borders.Bottom.Style, 1);
        end;

      end;


      // ---------- Caption --------------
      Brush.Style := bsClear;

      if sName <> '' then
      begin
        Font.Assign(FAppearance.ChangeColorItem.Font);
        clText := Font.Color;

        if bSelected then
        begin
          clText := FAppearance.SelectedItem.FontColor;
          if not Enabled then clText := FAppearance.SelectedItem.DisabledFontColor;
        end
        else
          if not Enabled then clText := FAppearance.ChangeColorItem.DisabledFontColor;
        if Assigned(FOnGetItemTextColor) then FOnGetItemTextColor(Index, State, ItemData, clText);
        Font.Color := clText;

        y := GetTextMiddlePosY(Rect, TextHeight(sName)) + FAppearance.ChangeColorItem.TextPosDeltaY;

        xTextWidth := TextWidth(sName);

        case FAppearance.ChangeColorItem.Alignment of
          taLeftJustify: xTextLeft := FAppearance.ChangeColorItem.CaptionMargin;
          taRightJustify: xTextLeft := Rect.Width - xTextWidth - FAppearance.ChangeColorItem.CaptionMargin;
          taCenter: xTextLeft := (Rect.Width div 2) - (xTextWidth div 2);
        else
          xTextLeft := 10;
        end;

        TextOut(xTextLeft, y, sName);
      end;

      Exit;
    end;
    {$endregion DRAW: ChangeColor Item}



    Font.Assign(Self.Font);
    Brush.Color := Self.Color;
    Brush.Style := bsSolid;
    Pen.Style := psSolid;


    {$Region ' ----------- DRAW: Background ----------------- '}
    if bSelected then
    begin
      Pen.Style := psClear;

      // --------- Background color / gradient -------------
      if FAppearance.SelectedItem.Background.DrawGradient or bCustomGradient then
      begin
        grd := FAppearance.SelectedItem.Background.Gradient;
        clBgFrom := grd.ColorFrom;
        clBgTo := grd.ColorTo;
        if Assigned(FOnGetItemGradientColors) then FOnGetItemGradientColors(Index, State, ItemData, clBgFrom, clBgTo);
        cyGradientFill(Self.Canvas, Rect, clBgFrom, clBgTo, grd.Orientation, grd.Balance, grd.AngleDegree, grd.BalanceMode, grd.MaxDegrade, grd.SpeedPercent);
      end
      else
      begin
        clBackground := FAppearance.SelectedItem.Background.Color;
        if Assigned(FOnGetItemBackgroundColor) then FOnGetItemBackgroundColor(Index, State, ItemData, clBackground);
        Brush.Color := clBackground;
        Rectangle(Rect);
      end;

      // ------------- Frame ----------------
      if FAppearance.SelectedItem.Background.DrawBorders then
      begin
        Borders := FAppearance.SelectedItem.Background.Borders;
        if Borders.Left.Visible then DrawRectLeftBorder(Self.Canvas, Rect, Borders.Left.Color, Borders.Left.Width, Borders.Left.Style);
        if Borders.Right.Visible then DrawRectRightBorder(Self.Canvas, Rect, Borders.Right.Color, Borders.Right.Width, Borders.Right.Style, 1);
        if Borders.Top.Visible then DrawRectTopBorder(Self.Canvas, Rect, Borders.Top.Color, Borders.Top.Width, Borders.Top.Style);
        if Borders.Bottom.Visible then DrawRectBottomBorder(Self.Canvas, Rect, Borders.Bottom.Color, Borders.Bottom.Width, Borders.Bottom.Style, 1);
      end;

    end

    else

    begin
      if not Enabled then Brush.Color := FAppearance.DisabledBackgroundColor;
      if Enabled and bCustomBgColor then Brush.Color := CustomBgColor;
      clBackground := Brush.Color;
      if Assigned(FOnGetItemBackgroundColor) then FOnGetItemBackgroundColor(Index, State, ItemData, clBackground);
      Brush.Color := clBackground;
      Pen.Color := Brush.Color;
      Rectangle(Rect);
    end;

    {$endregion}

    Pen.Style := psSolid;

    //GetColorFromStr(s, sName, clRectangle);
    clRectangle := ItemData.Color;

    //if odFocused in State then Font.Color := clHighlightText;



    {$Region ' --------------- DRAW: Color rectangle ------------- '}
    if FAppearance.ColorRectangle.Visible and (FAppearance.ColorRectangle.Width > 0) then
    begin
      ColorRect := Rect;
      ColorRect.Left := FAppearance.LeftMargin + FAppearance.ColorRectangle.LeftMargin;
      ColorRect.Right := ColorRect.Left + FAppearance.ColorRectangle.Width;
      Pen.Width := FAppearance.ColorRectangle.BorderWidth;
      Brush.Color := clRectangle;

      ColorRect.Top := ColorRect.Top + FAppearance.ColorRectangle.PaddingTop;
      ColorRect.Bottom := ColorRect.Bottom - FAppearance.ColorRectangle.PaddingBottom;

      Pen.Width := 1;
      Pen.Color := Brush.Color;
      Rectangle(ColorRect);

      if (FAppearance.ColorRectangle.BorderWidth > 0) and (FAppearance.ColorRectangle.BorderMode <> bmNoBorder) then
      begin

        case FAppearance.ColorRectangle.BorderMode of
          bmAverageColor: clBorder := AvgColor(clRectangle, FAppearance.ColorRectangle.BorderColor);
          bmSimpleColor: clBorder := FAppearance.ColorRectangle.BorderColor;
          else clBorder := clGray;
        end;

        if (not FAppearance.ColorRectangle.HideTopBorder) or (bPrevItemIsSeparator) then
            DrawRectTopBorder(Self.Canvas, ColorRect, clBorder, FAppearance.ColorRectangle.BorderWidth, psSolid);
        DrawRectBottomBorder(Self.Canvas, ColorRect, clBorder, FAppearance.ColorRectangle.BorderWidth, psSolid, 1);
        DrawRectLeftBorder(Self.Canvas, ColorRect, clBorder, FAppearance.ColorRectangle.BorderWidth, psSolid);
        DrawRectRightBorder(Self.Canvas, ColorRect, clBorder, FAppearance.ColorRectangle.BorderWidth, psSolid);

      end;

    end;
    {$endregion DRAW: Color rectangle}



    xTextStart := FAppearance.LeftMargin + FAppearance.TextMargin;
    if FAppearance.ColorRectangle.Visible then xTextStart := xTextStart + FAppearance.ColorRectangle.LeftMargin + FAppearance.ColorRectangle.Width;


    {$Region ' ----------- DRAW: Text ---------- '}

    sUserColorStr := '';
    if Assigned(FOnGetColorStrValue) then FOnGetColorStrValue(Index, clRectangle, sUserColorStr);

    if FAppearance.ShowRgbInt then
    begin
      sRgbInt := ColorToRgbIntStr(clRectangle, FAppearance.RgbIntParams.PaddingLen, FAppearance.RgbIntParams.PaddingChar, FAppearance.RgbIntParams.RgbSeparator);
      sRgbInt := FAppearance.RgbIntParams.Prefix + sRgbInt + FAppearance.RgbIntParams.Suffix;
    end;

    if FAppearance.ShowRgbHex then
    begin
      sRgbHex := ColorToHtmlColorStr(clRectangle, '');
      sRgbHex := InsertNumSep(sRgbHex, FAppearance.RgbHexParams.RgbSeparator, 2);
      sRgbHex := FAppearance.RgbHexParams.Prefix + sRgbHex + FAppearance.RgbHexParams.Suffix;
    end;

    if FAppearance.ShowBgrHex then
    begin
      sBgrHex := ColorToBgrHexStr(clRectangle);
      sBgrHex := InsertNumSep(sBgrHex, FAppearance.BgrHexParams.BgrSeparator, 2);
      sBgrHex := FAppearance.BgrHexParams.Prefix + sBgrHex + FAppearance.BgrHexParams.Suffix;
    end;

    Brush.Style := bsClear;
    Pen.Style := psClear;
    OutTextRect := Rect;

    if FAppearance.UseCustomNumericFont then
    begin

      xTextLeft := xTextStart;
      Font.Assign(FAppearance.NumericFont);
      if bCustomFontColor then clText := CustomFontColor else clText := Font.Color;
      if bSelected then clText := FAppearance.NumericFontSelectedColor;
      if not Enabled then clText := FAppearance.DisabledFontColor;
      if bSelected and (not Enabled) then clText := FAppearance.SelectedItem.DisabledFontColor;
      if Assigned(FOnGetNumericItemTextColor) then FOnGetNumericItemTextColor(Index, State, ItemData, clText);
      Font.Color := clText;

      // User color value (FOnGetColorStrValue)
      if sUserColorStr <> '' then
      begin
        sOut := sUserColorStr;
        y := GetTextMiddlePosY(OutTextRect, TextHeight(sOut)) + FAppearance.NumericTextPosDeltaY;
        TextOut(xTextLeft, y, sOut);
        xTextLeft := xTextLeft + TextWidth(sOut);
        bCanDrawDataSep := FAppearance.ShowRgbInt or FAppearance.ShowRgbHex or FAppearance.ShowBgrHex or (FAppearance.ShowColorName and (sName <> ''));
        if bCanDrawDataSep then
        begin
          TextOut(xTextLeft, y, FAppearance.DataSeparator);
          xTextLeft := xTextLeft + TextWidth(FAppearance.DataSeparator);
        end;
      end;

      // RGB Int
      if FAppearance.ShowRgbInt then
      begin
        sOut := {%H-}sRgbInt;
        y := GetTextMiddlePosY(OutTextRect, TextHeight(sOut)) + FAppearance.NumericTextPosDeltaY;
        TextOut(xTextLeft, y, sOut);
        xTextLeft := xTextLeft + TextWidth(sOut);
        bCanDrawDataSep := FAppearance.ShowRgbHex or FAppearance.ShowBgrHex or (FAppearance.ShowColorName and (sName <> ''));
        if bCanDrawDataSep then
        begin
          TextOut(xTextLeft, y, FAppearance.DataSeparator);
          xTextLeft := xTextLeft + TextWidth(FAppearance.DataSeparator);
        end;
      end;

      // RGB Hex
      if FAppearance.ShowRgbHex then
      begin
        sOut := {%H-}sRgbHex;
        y := GetTextMiddlePosY(OutTextRect, TextHeight(sOut)) + FAppearance.NumericTextPosDeltaY;
        TextOut(xTextLeft, y, sOut);
        xTextLeft := xTextLeft + TextWidth(sOut);
        bCanDrawDataSep := FAppearance.ShowBgrHex or (FAppearance.ShowColorName and (sName <> ''));
        if bCanDrawDataSep then
        begin
          TextOut(xTextLeft, y, FAppearance.DataSeparator);
          xTextLeft := xTextLeft + TextWidth(FAppearance.DataSeparator);
        end;
      end;

      // BGR Hex
      if FAppearance.ShowBgrHex then
      begin
        sOut := {%H-}sBgrHex;
        y := GetTextMiddlePosY(OutTextRect, TextHeight(sOut)) + FAppearance.NumericTextPosDeltaY;
        TextOut(xTextLeft, y, sOut);
        xTextLeft := xTextLeft + TextWidth(sOut);
        bCanDrawDataSep := FAppearance.ShowColorName and (sName <> '');
        if bCanDrawDataSep then
        begin
          TextOut(xTextLeft, y, FAppearance.DataSeparator);
          xTextLeft := xTextLeft + TextWidth(FAppearance.DataSeparator);
        end;
      end;

      Font.Assign(Self.Font);
      if bCustomFontColor then clText := CustomFontColor else clText := Font.Color;
      if bSelected then clText := FAppearance.SelectedItem.FontColor;
      if not Enabled then clText := FAppearance.DisabledFontColor;
      if bSelected and (not Enabled) then clText := FAppearance.SelectedItem.DisabledFontColor;
      if Assigned(FOnGetItemTextColor) then FOnGetItemTextColor(Index, State, ItemData, clText);
      Font.Color := clText;

      // Color name
      if FAppearance.ShowColorName then
      begin
        sOut := sName;
        y := GetTextMiddlePosY(OutTextRect, TextHeight(sOut)) + FAppearance.TextPosDeltaY;
        TextOut(xTextLeft, y, sOut);
      end;

    end

    else

    begin
      sOut := '';
      if sUserColorStr <> '' then sOut := sUserColorStr;
      if FAppearance.ShowRgbInt then sOut := sOut + FAppearance.DataSeparator + sRgbInt;
      if FAppearance.ShowRgbHex then sOut := sOut + FAppearance.DataSeparator + sRgbHex;
      if FAppearance.ShowColorName then sOut := sOut + FAppearance.DataSeparator + sName;
      sOut := TrimFromStart(sOut, FAppearance.DataSeparator);

      y := GetTextMiddlePosY(OutTextRect, TextHeight(sOut)) + FAppearance.TextPosDeltaY;

      if bCustomFontColor then clText := CustomFontColor else clText := Font.Color;
      if bSelected then clText := FAppearance.SelectedItem.FontColor;
      if not Enabled then clText := FAppearance.DisabledFontColor;
      if bSelected and (not Enabled) then clText := FAppearance.SelectedItem.DisabledFontColor;
      if Assigned(FOnGetItemTextColor) then FOnGetItemTextColor(Index, State, ItemData, clText);
      Font.Color := clText;

      TextOut(xTextStart, y, sOut);
    end;

    {$endregion DRAW: Text}


  end;


  if Assigned(FOnAfterPaintItem) then FOnAfterPaintItem(Index, Rect, State, ItemData);

end;



{$endregion DrawItem}


  {$region ' --------- internal controls ------------- '}


procedure TJppCustomColorComboBox.SetupButtonChangeColor;
begin
  if Assigned(FButtonChangeColor) then Exit;
  FButtonChangeColor := TJppComboButton.Create(Self);
  FButtonChangeColor.Name := 'BtnChangeColor';
  FButtonChangeColor.FreeNotification(Self);
  FButtonChangeColor.OnClick := ButtonChangeColorClick;
  FButtonChangeColor.Height := Height;
  //FButtonChangeColor.AutoWidth := False;
  //FButtonChangeColor.Width := FButtonChangeColor.Height;
  FButtonChangeColor.Caption := '...';
  FButtonChangeColor.Appearance.Normal.BorderColor := GetSimilarColor(clBtnFace, 15, False);
  FButtonChangeColor.Hint := 'Change color...';
  FButtonChangeColor.OnVisibleChanged := ButtonPositionChanged;
end;

procedure TJppCustomColorComboBox.SetupButtonCopyColor;
begin
  if Assigned(FButtonCopyColor) then Exit;
  FButtonCopyColor := TJppComboButton.Create(Self);
  FButtonCopyColor.Name := 'BtnCopyColor';
  FButtonCopyColor.FreeNotification(Self);
  FButtonCopyColor.OnClick := ButtonCopyColorClick;
  //FButtonCopyColor.AutoWidth := False;
  FButtonCopyColor.Height := Height;
  //FButtonCopyColor.Width := FButtonCopyColor.Height;
  FButtonCopyColor.Caption := 'C';
  FButtonCopyColor.Appearance.Normal.BorderColor := GetSimilarColor(clBtnFace, 15, False);
  FButtonCopyColor.Hint := 'Copy color';
  FButtonCopyColor.OnVisibleChanged := ButtonPositionChanged;
end;

procedure TJppCustomColorComboBox.SetupButtonPasteColor;
begin
  if Assigned(FButtonPasteColor) then Exit;
  FButtonPasteColor := TJppComboButton.Create(Self);
  FButtonPasteColor.Name := 'BtnPasteColor';
  FButtonPasteColor.FreeNotification(Self);
  FButtonPasteColor.OnClick := ButtonPasteColorClick;
  //FButtonPasteColor.AutoWidth := False;
  FButtonPasteColor.Height := Height;
  //FButtonPasteColor.Width := FButtonPasteColor.Height;
  FButtonPasteColor.Caption := 'P';
  FButtonPasteColor.Appearance.Normal.BorderColor := GetSimilarColor(clBtnFace, 15, False);
  FButtonPasteColor.Hint := 'Paste color';
  FButtonPasteColor.OnVisibleChanged := ButtonPositionChanged;
end;

function TJppCustomColorComboBox.GetButtonTopPosition(Button: TJppComboButton): integer;
begin
  case FButtonsAlignment of
    taVerticalCenter: Result := Top + (Height div 2) - (Button.Height div 2);
    taAlignBottom: Result := (Top + Height) - Button.Height;
  else
    // taAlignTop
    Result := Top;
  end;
end;

procedure TJppCustomColorComboBox.SetButtonChangeColorPosition;
begin
  if not Assigned(FButtonChangeColor) then Exit;
  FButtonChangeColor.Left := Left + Width + FButtonsSpacing;
  //FButtonChangeColor.Top := Top;
  FButtonChangeColor.Top := GetButtonTopPosition(FButtonChangeColor);
end;

procedure TJppCustomColorComboBox.SetButtonCopyColorPosition;
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

procedure TJppCustomColorComboBox.SetButtonPasteColorPosition;
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

procedure TJppCustomColorComboBox.SetButtonsAlignment(const Value: TVerticalAlignment);
begin
  if FButtonsAlignment = Value then Exit;
  FButtonsAlignment := Value;
  SetButtonsPosition;
end;

procedure TJppCustomColorComboBox.SetButtonsPosition;
begin
  SetButtonChangeColorPosition;
  SetButtonCopyColorPosition;
  SetButtonPasteColorPosition;
end;

procedure TJppCustomColorComboBox.SetButtonsSpacing(const Value: integer);
begin
  FButtonsSpacing := Value;
  SetButtonsPosition;
end;

procedure TJppCustomColorComboBox.ButtonChangeColorClick(Sender: TObject);
begin
  ShowColorDlg;
  if Assigned(OnChange) then OnChange(Self);
end;

procedure TJppCustomColorComboBox.ButtonCopyColorClick(Sender: TObject);
begin
  Clipboard.AsText := IntToStr(ColorToRGB(SelectedColor));
end;

procedure TJppCustomColorComboBox.ButtonPasteColorClick(Sender: TObject);
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
//    if Assigned(OnChange) then OnChange(Self);
//  end;
end;

procedure TJppCustomColorComboBox.ButtonPositionChanged(Sender: TObject);
begin
  SetButtonsPosition;
end;

procedure TJppCustomColorComboBox.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  SetBoundLabelPosition(FBoundLabelPosition);
  SetButtonsPosition;
  if not (csDestroying in ComponentState) then
    if Assigned(FAnchoredControls) then FAnchoredControls.UpdateAllControlsPos;
end;


procedure TJppCustomColorComboBox.SetupInternalLabel;
begin
  if Assigned(FBoundLabel) then Exit;
  FBoundLabel := TJppControlBoundLabel.Create(Self);
  FBoundLabel.FreeNotification(Self);
  FBoundLabel.OnAdjustBounds := AdjustLabelBounds;
  FBoundLabel.FocusControl := Self;
  //FBoundLabel.Parent := Self;//.Parent;
end;


procedure TJppCustomColorComboBox.AdjustLabelBounds(Sender: TObject);
begin
  SetBoundLabelPosition(FBoundLabelPosition);
end;


procedure TJppCustomColorComboBox.SetBoundLabelPosition(const Value: TLabelPosition);
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
  FBoundLabel.Visible := Visible;
end;

procedure TJppCustomColorComboBox.SetBoundLabelSpacing(const Value: Integer);
begin
  FBoundLabelSpacing := Value;
  SetBoundLabelPosition(FBoundLabelPosition);
end;


procedure TJppCustomColorComboBox.CMBiDiModeChanged(var Message: TMessage);
begin
  inherited;
  if FBoundLabel <> nil then FBoundLabel.BiDiMode := BiDiMode;
  if FButtonChangeColor <> nil then FButtonChangeColor.BiDiMode := BiDiMode;
  if FButtonCopyColor <> nil then FButtonCopyColor.BiDiMode := BiDiMode;
  if FButtonPasteColor <> nil then FButtonPasteColor.BiDiMode := BiDiMode;
end;

procedure TJppCustomColorComboBox.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  if FBoundLabel <> nil then FBoundLabel.Enabled := Enabled;
  if FButtonChangeColor <> nil then FButtonChangeColor.Enabled := Enabled;
  if FButtonCopyColor <> nil then FButtonCopyColor.Enabled := Enabled;
  if FButtonPasteColor <> nil then FButtonPasteColor.Enabled := Enabled;
end;

procedure TJppCustomColorComboBox.CMVisibleChanged(var Message: TMessage);
begin
  inherited;
  if FBoundLabel <> nil then FBoundLabel.Visible := Visible;
  if FButtonChangeColor <> nil then FButtonChangeColor.Visible := Visible;
  if FButtonCopyColor <> nil then FButtonCopyColor.Visible := Visible;
  if FButtonPasteColor <> nil then FButtonPasteColor.Visible := Visible;
end;

procedure TJppCustomColorComboBox.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = FBoundLabel then FBoundLabel := nil
    else if AComponent = FButtonChangeColor then FButtonChangeColor := nil
    else if AComponent = FButtonCopyColor then FButtonCopyColor := nil
    else if AComponent = FButtonPasteColor then FButtonPasteColor := nil;

    if not (csDestroying in ComponentState) then
      if Assigned(FAnchoredControls) then
      begin
        if AComponent = FAnchoredControls.Top.Control then FAnchoredControls.Top.Control := nil
        else if AComponent = FAnchoredControls.Bottom.Control then FAnchoredControls.Bottom.Control := nil
        else if AComponent = FAnchoredControls.Left.Control then FAnchoredControls.Left.Control := nil
        else if AComponent = FAnchoredControls.Right.Control then FAnchoredControls.Right.Control := nil;
      end;
  end;
end;



  {$endregion internal controls}



{$endregion TJppColorComboBox}





{$region ' ------------------------ TJppComboButton ----------------------------- '}
constructor TJppComboButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Name := 'ComboButton' + IntToStr(Random(10000)) + IntToStr(Random(10000));
  //AutoWidth := False;
  SetSubComponent(True);
end;


procedure TJppComboButton.SetOnVisibleChanged(const Value: TNotifyEvent);
begin
  FOnVisibleChanged := Value;
end;

procedure TJppComboButton.SetVisible(const Value: Boolean);
begin
  inherited {$IFDEF FPC}SetVisible(Value){$ENDIF};
  FVisible := Value;
  if Value then Show else Hide;
  if Assigned(OnVisibleChanged) then OnVisibleChanged(Self);
end;

{$endregion TJppComboButton}


{$Region ' -----------------------------  TJppColorComboBoxAppearance ---------------------- '}

//{
//  TComponentState = set of (csLoading, csReading, csWriting, csDestroying,
//    csDesigning, csAncestor, csUpdating, csFixups, csFreeNotification,
//    csInline, csDesignInstance);
//}

constructor TJppColorComboBoxAppearance.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOwner := AOwner;
  FChangeColorItem := TJppColorControlGradientCaptionItem.Create(AOwner);

  FChangeColorItem.OnChange := PropsChanged;

  if (csLoading in AOwner.ComponentState) then
  begin
    ChangeColorItem.Background.DrawGradient := False;
    ChangeColorItem.Background.Color := clWindow;
    ChangeColorItem.Background.DrawBorders := False;
  end;

  FChangeColorItem.Caption := 'Change color...';
  FChangeColorItem.CaptionMargin := 6;

//  if (csLoading in AOwner.ComponentState) then
//  begin
//    FChangeColorItem.Background.DrawGradient := False;
//    FChangeColorItem.Background.Color := clWindow;
//    FChangeColorItem.Background.DrawBorders := False;
//  end;

  FChangeColorItem.Alignment := taLeftJustify;
  FOnGetColorStrValue := nil;

end;

destructor TJppColorComboBoxAppearance.Destroy;
begin
  FChangeColorItem.Free;
  inherited;
end;

procedure TJppColorComboBoxAppearance.Assign(ccba: TJppColorComboBoxAppearance);
begin
  BeginUpdate;
  try
    FChangeColorItem.Assign(ccba.ChangeColorItem);
    inherited Assign(TJppColorControlAppearance(ccba));
  finally
    EndUpdate;
  end;
end;

procedure TJppColorComboBoxAppearance.SetChangeColorItem(const Value: TJppColorControlGradientCaptionItem);
begin
  FChangeColorItem := Value;
  PropsChanged(Self);
end;


{$endregion TJppColorComboBoxAppearance}



end.

