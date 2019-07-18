unit JPP.ColorListBox;

{$IFDEF FPC} {$mode delphi} {$ENDIF}
//{$IFDEF FPC} {$mode objfpc}{$H+} {$ENDIF}

{
  Last mod: 12.07.2019
    [+] SetItemColor, SetItemName
}

interface

uses
  {$IFDEF MSWINDOWS}Windows, Messages,{$ENDIF}
  {$IFDEF DCC}
  System.SysUtils, System.Classes, System.Types, System.UITypes,
  Vcl.GraphUtil, Vcl.Controls, Vcl.ExtCtrls, Vcl.Graphics, Vcl.Dialogs, Vcl.StdCtrls,
  {$ELSE}
  SysUtils, Classes, Types, Controls, ExtCtrls, Graphics, StdCtrls, Dialogs, LCLType, LCLIntf,
  {$ENDIF}

  JPL.Conversion, JPL.Strings, JPL.Colors, JPL.ColorArrays,
  JPP.Types, JPP.Common, JPP.Common.Procs, JPP.ColorControls.Common, JPP.Graphics, JPP.Gradient;

type

  TJppColorArray = array of TColorArrayItem;

  TJppColorListBoxItemType = (clbitNone, clbitSeparator, clbitColor, clbitChangeColor);

  TJppColorListBoxItemData = record
    ItemType: TJppColorListBoxItemType;
    Name: string;
    Color: TColor;
  end;

  TJppColorListBoxAssignParams = record
    Bevel: Boolean;
    BorderStyle: Boolean;
    BackgroundColor: Boolean;
    Font: Boolean;
    ItemHeight: Boolean;
    Options: Boolean;
    Items: Boolean;
    SelectedColor: Boolean;
  end;

  TJppColorListBoxOption = (clboAddOnSelectIfNotExists, clboAddAtTop);
  TJppColorListBoxOptions = set of TJppColorListBoxOption;

  TJppColorListBoxBeforePaintItem = procedure(const Index: Integer; const Rect: TRect; const State: TOwnerDrawState; const ItemData: TJppColorListBoxItemData;
    var PaintHandled: Boolean) of object;
  TJppColorListBoxAfterPaintItem = procedure(const Index: Integer; const Rect: TRect; const State: TOwnerDrawState; const ItemData: TJppColorListBoxItemData) of object;

  TJppColorListBoxGetItemBackgroundColor = procedure(const Index: integer; State: TOwnerDrawState; const ItemData: TJppColorListBoxItemData; var BgColor: TColor) of object;
  TJppColorListBoxGetItemGradientColors = procedure(const Index: integer; State: TOwnerDrawState; const ItemData: TJppColorListBoxItemData;
    var ColorFrom, ColorTo: TColor) of object;
  TJppColorListBoxGetItemTextColor = procedure(const Index: integer; State: TOwnerDrawState; const ItemData: TJppColorListBoxItemData; var TextColor: TColor) of object;
  TJppColorListBoxGetNumericItemTextColor = procedure(const Index: integer; State: TOwnerDrawState; const ItemData: TJppColorListBoxItemData; var TextColor: TColor) of object;


  {$Region ' -------- TJppColorListBoxAppearance -------- '}
  TJppColorListBoxAppearance = class(TJppColorControlAppearance)
  private
    FOwner: TComponent;
    FChangeColorItem: TJppColorControlGradientCaptionItem;
    FOnGetColorStrValue: TJppColorControlGetColorStrValue;
    procedure SetChangeColorItem(const Value: TJppColorControlGradientCaptionItem);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(clba: TJppColorListBoxAppearance); reintroduce;
  published
    property ChangeColorItem: TJppColorControlGradientCaptionItem read FChangeColorItem write SetChangeColorItem;
  end;
  {$endregion TJppColorListBoxAppearance}


  {$REGION ' ------ TJppCustomColorListBox ------ '}

  TJppCustomColorListBox = class(TCustomListBox)
  private
    FUpdateCounter: integer;
    FSelectedColor: TColor;
    FTagExt: TJppTagExt;
    FNoneColor: TColor;
    FAppearance: TJppColorListBoxAppearance;
    FColorListSet: TColorListSet;
    FOptions: TJppColorListBoxOptions;
    FOnBeforePaintItem: TJppColorListBoxBeforePaintItem;
    FOnAfterPaintItem: TJppColorListBoxAfterPaintItem;
    {$IFDEF DCC}FColorDialogOptions: TColorDialogOptions;{$ENDIF}
    FOnGetColorStrValue: TJppColorControlGetColorStrValue;
    FOnColorChanged: TNotifyEvent;
    FOnSelectChangeColorItem: TJppColorControlSelectChangeColor;
    FOnSelectSeparatorItem: TJppColorControlSelectSeparator;
    FOnGetItemBackgroundColor: TJppColorListBoxGetItemBackgroundColor;
    FOnGetItemGradientColors: TJppColorListBoxGetItemGradientColors;
    FOnGetItemTextColor: TJppColorListBoxGetItemTextColor;
    FOnGetNumericItemTextColor: TJppColorListBoxGetNumericItemTextColor;
    //FOnMeasureItem: TMeasureItemEvent;
    procedure SetTagExt(const Value: TJppTagExt);
    procedure SetNoneColor(const Value: TColor);
    procedure SetSelectedColor(const Value: TColor);
    function GetSelectedColor: TColor;
    procedure SetAppearance(const Value: TJppColorListBoxAppearance);
    procedure SetColorListSet(const Value: TColorListSet);
    procedure SetOptions(const Value: TJppColorListBoxOptions);
    procedure SetOnBeforePaintItem(const Value: TJppColorListBoxBeforePaintItem);
    procedure SetOnAfterPaintItem(const Value: TJppColorListBoxAfterPaintItem);
    {$IFDEF DCC}procedure SetColorDialogOptions(const Value: TColorDialogOptions);{$ENDIF}
    procedure SetOnColorChanged(const Value: TNotifyEvent);
    procedure SetOnGetColorStrValue(const Value: TJppColorControlGetColorStrValue);
    procedure SetOnSelectChangeColorItem(const Value: TJppColorControlSelectChangeColor);
    procedure SetOnSelectSeparatorItem(const Value: TJppColorControlSelectSeparator);
    procedure SetOnGetItemBackgroundColor(const Value: TJppColorListBoxGetItemBackgroundColor);
    procedure SetOnGetItemGradientColors(const Value: TJppColorListBoxGetItemGradientColors);
    procedure SetOnGetItemTextColor(const Value: TJppColorListBoxGetItemTextColor);
    procedure SetOnGetNumericItemTextColor(const Value: TJppColorListBoxGetNumericItemTextColor);
  protected
    procedure PropsChanged(Sender: TObject);
    procedure CreateWnd; override;
    procedure KeyPress(var Key: Char); override;
    procedure Loaded; override;
    ///////////////////////////////////////////////////////////////////////////////////////////
    procedure DrawItem(Index: Integer; ARect: TRect; State: TOwnerDrawState); override;
    ///////////////////////////////////////////////////////////////////////////////////////////
    procedure GetColorFromStr(sVal: string; var ColorName: string; var AColor: TColor);
    procedure RecreateItems;
    procedure Click; override;

  public

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure AssignParams(
      clb: TJppCustomColorListBox; bBevel: Boolean = True; bBorderStyle: Boolean = True;
      bBackgroundColor: Boolean = True; bFontParams: Boolean = True; bItemHeight: Boolean = True; bOptions: Boolean = True;
      bItems: Boolean = True; bSelectedColor: Boolean = True
    ); overload;
    procedure AssignParams(clb: TJppCustomColorListBox; Params: TJppColorListBoxAssignParams); overload;

    procedure BeginUpdate;
    procedure EndUpdate(bCallOnChangeAfterUpdate: Boolean = True; bResetUpdatingState: Boolean = False);
    function UpdatingControl: Boolean;

    procedure UnselectAll;
    procedure InvertSelection;
    procedure RemoveSelectedItems;

    procedure ShowColorDialog;
    procedure GetItemInfo(const Index: Integer; out ItemData: TJppColorListBoxItemData);
    function GetItemType(const Index: Integer): TJppColorListBoxItemType;
    function GetItemColor(const Index: Integer): TColor;
    function GetItemName(const Index: Integer): string;
    function GetColorIndex(const AColor: TColor; bFirst: Boolean = True): Integer;

    function SetItemColor(const Index: integer; const cl: TColor): Boolean;
    function SetItemName(const Index: integer; const NewColorName: string): Boolean;

    function IsSeparatorItem(const Index: Integer): Boolean;
    function IsColorItem(const Index: integer): Boolean;
    function IsChangeColorItem(const Index: integer): Boolean;

    function ColorExists(const AColor: TColor): Boolean;

    procedure AddColorsFromArray(const Arr: array of TColorArrayItem; GroupName: string = ''; bAddSeparator: Boolean = False);

    function AddColor(const AColor: TColor; const ColorName: string): Integer;
    procedure InsertColor(const Index: Integer; const AColor: TColor; const ColorName: string); overload;
    procedure InsertColor(const Index: integer; const ColorName: string; const AColor: TColor); overload;

    function AddChangeColorItem(bUseDefaultCaption: Boolean = True; ItemCaption: string = 'Select color...'): integer;
    procedure InsertChangeColorItem(const Index: integer; bUseDefaultCaption: Boolean = True; ItemCaption: string = 'Select color...');
    function AddSeparatorItem(const ACaption: string): integer;
    procedure InsertSeparatorItem(const Index: integer; const ACaption: string);

    function AsColorArray: TJppColorArray; // {$IFDEF FPC}specialize{$ENDIF} TArray<TColorArrayItem>;
    procedure UpdateColorObject(const Index: Integer; bForce: Boolean = False);
    procedure UpdateAllColorObjects;

    property SelectedColor: TColor read GetSelectedColor write SetSelectedColor;


    //property OnMeasureItem: TMeasureItemEvent read FOnMeasureItem write FOnMeasureItem;
    property Appearance: TJppColorListBoxAppearance read FAppearance write SetAppearance;
    property TagExt: TJppTagExt read FTagExt write SetTagExt;
    property NoneColor: TColor read FNoneColor write SetNoneColor default clNone;

    property ColorListSet: TColorListSet read FColorListSet write SetColorListSet;
    property Options: TJppColorListBoxOptions read FOptions write SetOptions;
    {$IFDEF DCC}
    property ColorDialogOptions: TColorDialogOptions read FColorDialogOptions write SetColorDialogOptions default [cdFullOpen, cdAnyColor];
    {$ENDIF}

    property OnColorChanged: TNotifyEvent read FOnColorChanged write SetOnColorChanged;
    property OnSelectSeparatorItem: TJppColorControlSelectSeparator read FOnSelectSeparatorItem write SetOnSelectSeparatorItem;
    property OnSelectChangeColorItem: TJppColorControlSelectChangeColor read FOnSelectChangeColorItem write SetOnSelectChangeColorItem;
    property OnGetColorStrValue: TJppColorControlGetColorStrValue read FOnGetColorStrValue write SetOnGetColorStrValue;
    property OnBeforePaintItem: TJppColorListBoxBeforePaintItem read FOnBeforePaintItem write SetOnBeforePaintItem;
    property OnAfterPaintItem: TJppColorListBoxAfterPaintItem read FOnAfterPaintItem write SetOnAfterPaintItem;

    property OnGetItemBackgroundColor: TJppColorListBoxGetItemBackgroundColor read FOnGetItemBackgroundColor write SetOnGetItemBackgroundColor;
    property OnGetItemGradientColors: TJppColorListBoxGetItemGradientColors read FOnGetItemGradientColors write SetOnGetItemGradientColors;
    property OnGetItemTextColor: TJppColorListBoxGetItemTextColor read FOnGetItemTextColor write SetOnGetItemTextColor;
    property OnGetNumericItemTextColor: TJppColorListBoxGetNumericItemTextColor read FOnGetNumericItemTextColor write SetOnGetNumericItemTextColor;
  end;
  {$ENDREGION TJppCustomColorListBox}


  {$REGION ' ------ TJppColorListBox ------ '}

  TJppColorListBox = class(TJppCustomColorListBox)
  published
    property Align;
    {$IFDEF DCC}property AutoComplete;{$ENDIF}

    //property Style;

    property Anchors;
    {$IFDEF DCC}
    property BevelEdges;
    property BevelInner;
    property BevelKind;
    property BevelOuter;
    {$ENDIF}
    property BiDiMode;
    property Color;
    property Constraints;
    {$IFDEF DCC}property Ctl3D;{$ENDIF}
    property DoubleBuffered;
    property Enabled;
    property Font;
    property ItemHeight;
    property ParentBiDiMode;
    property ParentColor;
    {$IFDEF DCC}property ParentCtl3D;{$ENDIF}
    property ParentDoubleBuffered;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    {$IFDEF DCC}property Touch;{$ENDIF}
    property Visible;
    {$IFDEF DCC}{$IF RTLVersion > 23} property StyleElements; {$IFEND}{$ENDIF}
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    {$IFDEF DCC}property OnGesture;{$ENDIF}
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    {$IFDEF DCC}property OnMouseActivate;{$ENDIF}
    property OnMouseDown;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;

    property Multiselect;
    property OnMeasureItem;
    property BorderStyle;
    property Items;
    property TagExt;
    property NoneColor;
    property Appearance;
    property ColorListSet;
    property Options;

    property SelectedColor;
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
  end;
  {$ENDREGION TJppColorListBox}



procedure ResetColorComboBoxAssignParams(var clbap: TJppColorListBoxAssignParams; FieldsValue: Boolean = True);



implementation





procedure ResetColorComboBoxAssignParams(var clbap: TJppColorListBoxAssignParams; FieldsValue: Boolean = True);
begin
  clbap.Bevel := FieldsValue;
  clbap.BorderStyle := FieldsValue;
  clbap.BackgroundColor := FieldsValue;
  clbap.Font := FieldsValue;
  clbap.ItemHeight := FieldsValue;
  clbap.Options := FieldsValue;
  clbap.Items := FieldsValue;
  clbap.SelectedColor := FieldsValue;
end;


{$REGION '                         TJppCustomColorListBox                                     '}


  {$Region ' -------------------------- Create / Destroy / AssignParams -------------------------- '}
constructor TJppCustomColorListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  inherited Style := lbOwnerDrawVariable;
  Parent := TWinControl(AOwner);

  {$IFDEF FPC}Color := clWindow;{$ENDIF}
  FUpdateCounter := 0;

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

  FAppearance := TJppColorListBoxAppearance.Create(Self);
  FAppearance.OnChange := (*{$IFDEF FPC} @ {$ENDIF}*)PropsChanged;

  FTagExt := TJppTagExt.Create(Self);

  FNoneColor := clNone;
  {$IFDEF DCC}FColorDialogOptions := [cdFullOpen, cdAnyColor];{$ENDIF}

  FColorListSet := [cltBasic];
  {
  [
    cltStandard16, cltWebGrayBlack, cltWebWhite, cltWebPink, cltWebRed, cltWebOrange, cltWebYellow, cltWebBrown, cltWebGreen,
    cltWebCyan, cltWebBlue, cltWebPurpleVioletMagenta];
  }

  if csDesigning in ComponentState then RecreateItems;
end;

destructor TJppCustomColorListBox.Destroy;
begin
  FAppearance.Free;
  FTagExt.Free;
  inherited;
end;

procedure TJppCustomColorListBox.Loaded;
begin
  inherited Loaded;
  //FSelected := GetSelectedColor;
  //RecreateItems;
end;

procedure TJppCustomColorListBox.AssignParams(clb: TJppCustomColorListBox; Params: TJppColorListBoxAssignParams);
begin
  AssignParams(clb, Params.Bevel, Params.BorderStyle, Params.BackgroundColor, Params.Font, Params.ItemHeight,
    Params.Options, Params.Items, Params.SelectedColor);
end;

procedure TJppCustomColorListBox.AssignParams(clb: TJppCustomColorListBox; bBevel, bBorderStyle, bBackgroundColor, bFontParams, bItemHeight, bOptions, bItems,
  bSelectedColor: Boolean);
begin
  BeginUpdate;
  try

    FAppearance.Assign(clb.Appearance);

    {$IFDEF DCC}
    if bBevel then
    begin
      BevelEdges := clb.BevelEdges;
      BevelInner := clb.BevelInner;
      BevelKind := clb.BevelKind;
      BevelOuter := clb.BevelOuter;
    end;
    {$ENDIF}

    if bBorderStyle then Self.BorderStyle := clb.BorderStyle;
    if bBackgroundColor then Self.Color := clb.Color;
    if bFontParams then Self.Font.Assign(clb.Font);
    if bItemHeight then Self.ItemHeight := clb.ItemHeight;
    if bOptions then Self.Options := clb.Options;
    if bItems then Self.Items.Assign(clb.Items);
    if bSelectedColor then Self.SelectedColor := clb.SelectedColor;

  finally
    EndUpdate;
  end;

end;

  {$endregion Create / Destroy / AssignParams}



procedure TJppCustomColorListBox.BeginUpdate;
begin
  Inc(FUpdateCounter);
end;

procedure TJppCustomColorListBox.EndUpdate(bCallOnChangeAfterUpdate, bResetUpdatingState: Boolean);
begin
  if bResetUpdatingState then FUpdateCounter := 0
  else
  begin
    Dec(FUpdateCounter);
    if FUpdateCounter < 0 then FUpdateCounter := 0;
  end;

  if (FUpdateCounter = 0) and bCallOnChangeAfterUpdate then PropsChanged(Self);
end;

function TJppCustomColorListBox.UpdatingControl: Boolean;
begin
  Result := FUpdateCounter > 0;
end;


function TJppCustomColorListBox.SetItemColor(const Index: integer; const cl: TColor): Boolean;
var
  ItemData: TJppColorListBoxItemData;
begin
  Result := False;
  GetItemInfo(Index, ItemData);
  if ItemData.ItemType <> clbitColor then Exit;

  Items[Index] := ItemData.Name + '=' + ColorToRgbIntStr(ColorToRGB(cl), 3, '0', ',');
  UpdateColorObject(Index, True);

  Result := True;
end;

function TJppCustomColorListBox.SetItemName(const Index: integer; const NewColorName: string): Boolean;
var
  ItemData: TJppColorListBoxItemData;
begin
  Result := False;
  GetItemInfo(Index, ItemData);
  if ItemData.ItemType <> clbitColor then Exit;

  Items[Index] := NewColorName + '=' + ColorToRgbIntStr(ColorToRGB(ItemData.Color), 3, '0', ',');
  //UpdateColorObject(Index, True);

  Result := True;
end;

{$Region ' --------------------- Add & Insert ---------------------- '}
function TJppCustomColorListBox.AddChangeColorItem(bUseDefaultCaption: Boolean = True; ItemCaption: string = 'Select color...'): integer;
begin
  if bUseDefaultCaption then ItemCaption := FAppearance.ChangeColorItem.Caption;
  Result := Items.Add('@=' + ItemCaption);
end;

procedure TJppCustomColorListBox.InsertChangeColorItem(const Index: integer; bUseDefaultCaption: Boolean = True; ItemCaption: string = 'Select color...');
begin
  if bUseDefaultCaption then ItemCaption := FAppearance.ChangeColorItem.Caption;
  Items.Insert(Index, '@=' + ItemCaption);
end;

function TJppCustomColorListBox.AddColor(const AColor: TColor; const ColorName: string): Integer;
var
  x: Integer;
begin
  x := Items.Add(ColorName + '=' + ColorToRgbIntStr(ColorToRGB(AColor), 3, '0', ','));
  //Items.InsertObject(x, ColorName, TObject(Color));
  Items.Objects[x] := TObject(AColor);
  Result := x;
end;

procedure TJppCustomColorListBox.InsertColor(const Index: Integer; const AColor: TColor; const ColorName: string);
begin
  Items.Insert(Index, ColorName + '=' + ColorToRgbIntStr(AColor, 3, '0', ','));
  Items.Objects[Index] := TObject(AColor);
end;

procedure TJppCustomColorListBox.InsertColor(const Index: integer; const ColorName: string; const AColor: TColor);
begin
  InsertColor(Index, AColor, ColorName);
end;

function TJppCustomColorListBox.AddSeparatorItem(const ACaption: string): integer;
begin
  Result := Items.Add('-=' + ACaption);
end;

procedure TJppCustomColorListBox.InsertSeparatorItem(const Index: integer; const ACaption: string);
begin
  Items.Insert(Index, '-=' + ACaption);
end;



procedure TJppCustomColorListBox.AddColorsFromArray(const Arr: array of TColorArrayItem; GroupName: string = ''; bAddSeparator: Boolean = False);
var
  i, x: Integer;
  cai: TColorArrayItem;
begin
  BeginUpdate;
  Items.BeginUpdate;
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


  {$Region ' ------------------- Get & Set Selected --------------------- '}

function TJppCustomColorListBox.GetSelectedColor: TColor;
begin
  Result := FNoneColor;
  if HandleAllocated then
    if ItemIndex <> -1 then Result := GetItemColor(ItemIndex);
end;

procedure TJppCustomColorListBox.SetSelectedColor(const Value: TColor);
var
  x: Integer;
begin
  //if Value = FSelected then Exit;

  x := GetColorIndex(Value);

  if x >= 0 then
  begin
    inherited Selected[x] := True;
    ItemIndex := x;
    Self.TopIndex := x;
  end

  else if clboAddOnSelectIfNotExists in FOptions then
  begin
    if clboAddAtTop in FOptions then
    begin
      x := 0;
      InsertColor(x, Value, '');
    end
    else x := AddColor(Value, '');

    inherited Selected[x] := True;
    ItemIndex := x;
    Self.TopIndex := x;
  end;

  if Assigned(FOnColorChanged) then FOnColorChanged(Self);
end;


  {$endregion Get & Set Selected}


procedure TJppCustomColorListBox.Click;
var
  id: TJppColorListBoxItemData;
begin
  inherited Click;

  //ShowMessage(ItemIndex.ToString);
  GetItemInfo(ItemIndex, id);

  case id.ItemType of

    clbitChangeColor: ShowColorDialog;

    clbitColor:
      begin
        FSelectedColor := GetSelectedColor;
        if Assigned(FOnColorChanged) then FOnColorChanged(Self);
      end;

    clbitSeparator:
      begin
        if Assigned(FOnSelectSeparatorItem) then FOnSelectSeparatorItem(ItemIndex, id.Name);
      end;

  end;
end;


procedure TJppCustomColorListBox.ShowColorDialog;
var
  dlgColor: TColorDialog;
begin
  dlgColor := TColorDialog.Create(Self);
  try
    {$IFDEF DCC}dlgColor.Options := FColorDialogOptions;{$ENDIF}
    dlgColor.Color := FSelectedColor;
    if not dlgColor.Execute then
    begin
      //SetSelectedColor(FSelected);
      Exit;
    end;
    SetSelectedColor(dlgColor.Color);
  finally
    dlgColor.Free;
  end;
end;

procedure TJppCustomColorListBox.KeyPress(var Key: Char);
var
  AColor: TColor;
begin
  inherited KeyPress(Key);

  if IsColorItem(ItemIndex) then
  begin
    AColor := GetSelectedColor;
    if AColor <> FNoneColor then FSelectedColor := AColor;
  end;

  if (Key = #13) and (IsChangeColorItem(ItemIndex)) then
  begin
    Key := #0;
    ShowColorDialog;
  end;
end;


  {$REGION '                      GetColorFromStr                          '}

procedure TJppCustomColorListBox.GetColorFromStr(sVal: string; var ColorName: string; var AColor: TColor);
var
  s: string;
  xp: Integer;
begin
  ColorName := '';
  AColor := FNoneColor;

  s := Trim(sVal);

  xp := Pos('=', s);
  if xp <= 0 then Exit;
  ColorName := Copy(s, 1, xp - 1);
  s := Copy(s, xp + 1, Length(s));

  if Length(s) < 3 then Exit;

  if s[1] = '$' then
  begin
    if not TryStrToInt(s, xp) then Exit;
    AColor := TColor(xp);
    Exit;
  end;

  if not TryRgbStrToColor(s, AColor) then
    if not TryHtmlStrToColor(s, AColor) then AColor := FNoneColor;

end;

  {$ENDREGION GetColorFromStr}



function TJppCustomColorListBox.GetColorIndex(const AColor: TColor; bFirst: Boolean = True): Integer;
var
  i: Integer;
  cl: TColor;
begin
  Result := -1;
  for i := 0 to Items.Count - 1 do
  begin
    if IsSeparatorItem(i) then Continue;
    if Items.Objects[i] = nil then UpdateColorObject(i);
    cl := TColor(Items.Objects[i]);
    if cl = AColor then
    begin
      Result := i;
      if bFirst then Break;
    end;
  end;
end;



function TJppCustomColorListBox.ColorExists(const AColor: TColor): Boolean;
begin
  Result := GetColorIndex(AColor) >= 0;
end;

function TJppCustomColorListBox.GetItemColor(const Index: Integer): TColor;
var
  id: TJppColorListBoxItemData;
begin
  GetItemInfo(Index, id);
  Result := id.Color;
end;

procedure TJppCustomColorListBox.GetItemInfo(const Index: Integer; out ItemData: TJppColorListBoxItemData);
var
  s, sName, s1: string;
  AColor: TColor;
  xp: integer;
begin
  ItemData.Color := FNoneColor;
  ItemData.Name := '';
  ItemData.ItemType := clbitNone;
  if (Index < 0) or (Index > Items.Count - 1) then Exit;

  s := Trim(Items[Index]);
  s1 := Copy(s, 1, 1);
  if s1 = '-' then ItemData.ItemType := clbitSeparator
  else if s1 = '@' then ItemData.ItemType := clbitChangeColor
  else ItemData.ItemType := clbitColor;

  xp := Pos('=', s);
  if xp <= 0 then Exit;

  if (ItemData.ItemType = clbitSeparator) or (ItemData.ItemType = clbitChangeColor) then
  begin
    ItemData.Name := Copy(s, xp + 1, Length(s));
    Exit;
  end;

  GetColorFromStr(s, sName, AColor);
  ItemData.Name := sName;
  ItemData.Color := AColor;
end;

function TJppCustomColorListBox.GetItemName(const Index: Integer): string;
var
  id: TJppColorListBoxItemData;
begin
  GetItemInfo(Index, id);
  Result := id.Name;
end;

function TJppCustomColorListBox.GetItemType(const Index: Integer): TJppColorListBoxItemType;
var
  Znak: Char;
  s: string;
begin
  s := Items[Index];
  if s = '' then Exit(clbitNone);
  Znak := s[1];

  case Znak of
    '-': Result := clbitSeparator;
    '@': Result := clbitChangeColor;
    else Result := clbitColor;
  end;
end;



function TJppCustomColorListBox.IsChangeColorItem(const Index: integer): Boolean;
begin
  Result := GetItemType(Index) = clbitChangeColor;
end;

function TJppCustomColorListBox.IsColorItem(const Index: integer): Boolean;
begin
  Result := GetItemType(Index) = clbitColor;
end;

function TJppCustomColorListBox.IsSeparatorItem(const Index: Integer): Boolean;
begin
  Result := GetItemType(Index) = clbitSeparator;
end;

procedure TJppCustomColorListBox.PropsChanged(Sender: TObject);
begin
  if csLoading in ComponentState then Exit;
  Invalidate;
end;

procedure TJppCustomColorListBox.SetAppearance(const Value: TJppColorListBoxAppearance);
begin
  FAppearance := Value;
end;

{$IFDEF DCC}
procedure TJppCustomColorListBox.SetColorDialogOptions(const Value: TColorDialogOptions);
begin
  if FColorDialogOptions = Value then Exit;
  FColorDialogOptions := Value;
end;
{$ENDIF}

procedure TJppCustomColorListBox.SetColorListSet(const Value: TColorListSet);
begin
  FColorListSet := Value;
  if not (csLoading in ComponentState) then RecreateItems;
  PropsChanged(Self);
end;



procedure TJppCustomColorListBox.SetNoneColor(const Value: TColor);
begin
  if FNoneColor = Value then Exit;
  FNoneColor := Value;
end;

procedure TJppCustomColorListBox.SetOnAfterPaintItem(const Value: TJppColorListBoxAfterPaintItem);
begin
  FOnAfterPaintItem := Value;
  PropsChanged(Self);
end;

procedure TJppCustomColorListBox.SetOnBeforePaintItem(const Value: TJppColorListBoxBeforePaintItem);
begin
  FOnBeforePaintItem := Value;
  PropsChanged(Self);
end;

procedure TJppCustomColorListBox.SetOnColorChanged(const Value: TNotifyEvent);
begin
  FOnColorChanged := Value;
end;

procedure TJppCustomColorListBox.SetOnGetColorStrValue(const Value: TJppColorControlGetColorStrValue);
begin
  FOnGetColorStrValue := Value;
  PropsChanged(Self);
end;

procedure TJppCustomColorListBox.SetOnGetItemBackgroundColor(const Value: TJppColorListBoxGetItemBackgroundColor);
begin
  FOnGetItemBackgroundColor := Value;
  PropsChanged(Self);
end;

procedure TJppCustomColorListBox.SetOnGetItemGradientColors(const Value: TJppColorListBoxGetItemGradientColors);
begin
  FOnGetItemGradientColors := Value;
  PropsChanged(Self);
end;

procedure TJppCustomColorListBox.SetOnGetItemTextColor(const Value: TJppColorListBoxGetItemTextColor);
begin
  FOnGetItemTextColor := Value;
  PropsChanged(Self);
end;

procedure TJppCustomColorListBox.SetOnGetNumericItemTextColor(const Value: TJppColorListBoxGetNumericItemTextColor);
begin
  FOnGetNumericItemTextColor := Value;
  PropsChanged(Self);
end;

procedure TJppCustomColorListBox.SetOnSelectChangeColorItem(const Value: TJppColorControlSelectChangeColor);
begin
  FOnSelectChangeColorItem := Value;
end;

procedure TJppCustomColorListBox.SetOnSelectSeparatorItem(const Value: TJppColorControlSelectSeparator);
begin
  FOnSelectSeparatorItem := Value;
end;

procedure TJppCustomColorListBox.SetOptions(const Value: TJppColorListBoxOptions);
begin
  FOptions := Value;
  PropsChanged(Self);
end;

procedure TJppCustomColorListBox.SetTagExt(const Value: TJppTagExt);
begin
  FTagExt := Value;
end;



procedure TJppCustomColorListBox.CreateWnd;
begin
  inherited CreateWnd;
end;

procedure TJppCustomColorListBox.RecreateItems;
//var
//  Color: TColor;
begin
  //Color := GetSelectedColor;
  BeginUpdate;
  try
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
    //if Color <> FNoneColor then SetSelectedColor(Color);
    //ShowMessage(ColorToRgbIntStr(Color));
  finally
    EndUpdate;
  end;
end;

procedure TJppCustomColorListBox.RemoveSelectedItems;
var
  i: integer;
begin
  BeginUpdate;
  try
    for i := Items.Count - 1 downto 0 do
      if Selected[i] then Items.Delete(i);
  finally
    EndUpdate;
  end;
end;

procedure TJppCustomColorListBox.UnselectAll;
var
  i: integer;
begin
  BeginUpdate;
  try

    if MultiSelect then
      for i := 0 to Items.Count - 1 do
        Self.Selected[i] := False
    else
      if ItemIndex >= 0 then Selected[ItemIndex] := False;

  finally
    EndUpdate;
  end;
end;

procedure TJppCustomColorListBox.InvertSelection;
var
  i: integer;
begin
  BeginUpdate;
  try

    if MultiSelect then
      for i := 0 to Items.Count - 1 do
        Selected[i] := not Selected[i]
    else
      if ItemIndex >= 0 then Selected[ItemIndex] := not Selected[ItemIndex];

  finally
    EndUpdate;
  end;
end;

procedure TJppCustomColorListBox.UpdateAllColorObjects;
var
  AColor: TColor;
  i: Integer;
begin
  for i := 0 to Items.Count - 1 do
  begin
    if IsSeparatorItem(i) then Exit;
    AColor := GetItemColor(i);
    Items.Objects[i] := TObject(AColor);
  end;
end;

procedure TJppCustomColorListBox.UpdateColorObject(const Index: Integer; bForce: Boolean = False);
var
  AColor: TColor;
begin
  if IsSeparatorItem(Index) then Exit;

  if bForce or (Items.Objects[Index] = nil) then
  begin
    AColor := GetItemColor(Index);
    Items.Objects[Index] := TObject(AColor);
  end;
end;

function TJppCustomColorListBox.AsColorArray: TJppColorArray; // {$IFDEF FPC}specialize{$ENDIF} TArray<TColorArrayItem>;
var
  i: Integer;
  AColor: TColor;
  ColorName: string;
begin
  SetLength(Result{%H-}, 0);
  for i := 0 to Items.Count - 1 do
  begin
    if IsSeparatorItem(i) then Continue;
    //if Items.Objects[i] = nil then UpdateColorObject(i);
    AColor := TColor(Items.Objects[i]);
    ColorName := GetItemName(i);
    SetLength(Result, Length(Result) + 1);
    Result[High(Result)].Color := AColor;
    Result[High(Result)].Name := ColorName;
  end;
end;



{$REGION '                                        DrawItem                                              '}

procedure TJppCustomColorListBox.DrawItem(Index: Integer; ARect: TRect; State: TOwnerDrawState);
{
  TOwnerDrawState = set of (odSelected, odGrayed, odDisabled, odChecked,
    odFocused, odDefault, odHotLight, odInactive, odNoAccel, odNoFocusRect,
    odReserved1, odReserved2, odComboBoxEdit);
}

var
  //s,
  sName: string;
  sOut, sRgbInt, sRgbHex, sBgrHex, sUserColorStr: string;
  y, xTextStart, xTextLeft, xTextWidth: Integer;
  ColorRect, OutTextRect: TRect;
  clBgFrom, clBgTo, clBackground, clText, clRectangle, clBorder: TColor;
  bPrevItemIsSeparator, bSeparatorItem, bChangeColorItem, bCanDrawDataSep: Boolean;
  //bFocused,
  bSelected, bPaintHandled: Boolean;
  grd: TJppGradientEx;
  Borders: TJppBorders;
  ItemData: TJppColorListBoxItemData;

begin

  //////////////////////////////////////////////////////
  if UpdatingControl then Exit;
  //////////////////////////////////////////////////////

  GetItemInfo(Index, ItemData);

  bPaintHandled := False;
  if Assigned(FOnBeforePaintItem) then FOnBeforePaintItem(Index, ARect, State, ItemData, bPaintHandled);
  if bPaintHandled then Exit;

  if Assigned(OnDrawItem) then
  begin
    inherited OnDrawItem(Self, Index, ARect, State);
    Exit;
  end;


  //s := ItemData.Name;
  bSeparatorItem := ItemData.ItemType = clbitSeparator;
  bChangeColorItem := ItemData.ItemType = clbitChangeColor;
  bSelected := odSelected in State;
  sName := ItemData.Name;

  bPrevItemIsSeparator := False;
  if Index > 0 then bPrevItemIsSeparator := Self.IsSeparatorItem(Index - 1);

  with Canvas do
  begin

    // clear background
    if Enabled then Brush.Color := Self.Color
    else Brush.Color := FAppearance.DisabledBackgroundColor;
    Pen.Color := Self.Color;
    Brush.Style := bsSolid;
    Pen.Style := psSolid;
    //InflateRect(ARect, 4, 4);
    Rectangle(ARect);
    //InflateRect(ARect, -2, -2);

    {$REGION ' ------------ DRAW: Separator Item ---------------- '}
    if bSeparatorItem then
    begin

      Pen.Style := psClear;

      // --------- Background color / gradient -------------
      if FAppearance.SeparatorItem.Background.DrawGradient then
      begin
        grd := FAppearance.SeparatorItem.Background.Gradient;
        clBgFrom := grd.ColorFrom;
        clBgTo := grd.ColorTo;
        if Assigned(FOnGetItemGradientColors) then FOnGetItemGradientColors(Index, State, ItemData, clBgFrom, clBgTo);
        cyGradientFill(Self.Canvas, ARect, clBgFrom, clBgTo, grd.Orientation, grd.Balance, grd.AngleDegree, grd.BalanceMode, grd.MaxDegrade, grd.SpeedPercent);
      end
      else
      begin
        clBackground := FAppearance.SeparatorItem.Background.Color;
        if Assigned(FOnGetItemBackgroundColor) then FOnGetItemBackgroundColor(Index, State, ItemData, clBackground);
        Brush.Color := clBackground;
        Rectangle(ARect);
      end;

      // ------------- Frame ----------------
      if FAppearance.SeparatorItem.Background.DrawBorders then
      begin
        Borders := FAppearance.SeparatorItem.Background.Borders;
        if Borders.Left.Visible then DrawRectLeftBorder(Self.Canvas, ARect, Borders.Left.Color, Borders.Left.Width, Borders.Left.Style);
        if Borders.Right.Visible then DrawRectRightBorder(Self.Canvas, ARect, Borders.Right.Color, Borders.Right.Width, Borders.Right.Style, 1);
        if Borders.Top.Visible then DrawRectTopBorder(Self.Canvas, ARect, Borders.Top.Color, Borders.Top.Width, Borders.Top.Style);
        if Borders.Bottom.Visible then DrawRectBottomBorder(Self.Canvas, ARect, Borders.Bottom.Color, Borders.Bottom.Width, Borders.Bottom.Style, 1);
      end;

      // ---------- Caption --------------
      Brush.Style := bsClear;

      if sName <> '' then
      begin
        Font.Assign(FAppearance.SeparatorItem.Font);
        clText := Font.Color;
        if not Enabled then clText := FAppearance.SeparatorItem.DisabledFontColor;
        y := GetMiddlePosY(ARect, TextHeight(sName)) + FAppearance.SeparatorItem.TextPosDeltaY;
        if Assigned(FOnGetItemTextColor) then FOnGetItemTextColor(Index, State, ItemData, clText);
        Font.Color := clText;

        xTextWidth := TextWidth(sName);

        case FAppearance.SeparatorItem.Alignment of
          taLeftJustify: xTextLeft := FAppearance.SeparatorItem.CaptionMargin;
          taRightJustify: xTextLeft := ARect.Width - xTextWidth - FAppearance.SeparatorItem.CaptionMargin;
          taCenter: xTextLeft := (ARect.Width div 2) - (xTextWidth div 2);
          else xTextLeft := 10;
        end;

        TextOut(xTextLeft, y, sName);
      end;

      Exit;
    end;
    {$ENDREGION DRAW: Separator Item}


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
          cyGradientFill(Self.Canvas, ARect, clBgFrom, clBgTo, grd.Orientation, grd.Balance, grd.AngleDegree, grd.BalanceMode, grd.MaxDegrade, grd.SpeedPercent);
        end
        else
        begin
          clBackground := FAppearance.SelectedItem.Background.Color;
          if Assigned(FOnGetItemBackgroundColor) then FOnGetItemBackgroundColor(Index, State, ItemData, clBackground);
          Brush.Color := clBackground;
          Rectangle(ARect);
        end;

        // ------------- Frame ----------------
        if FAppearance.SelectedItem.Background.DrawBorders then
        begin
          Borders := FAppearance.SelectedItem.Background.Borders;
          if Borders.Left.Visible then DrawRectLeftBorder(Self.Canvas, ARect, Borders.Left.Color, Borders.Left.Width, Borders.Left.Style);
          if Borders.Right.Visible then DrawRectRightBorder(Self.Canvas, ARect, Borders.Right.Color, Borders.Right.Width, Borders.Right.Style, 1);
          if Borders.Top.Visible then DrawRectTopBorder(Self.Canvas, ARect, Borders.Top.Color, Borders.Top.Width, Borders.Top.Style);
          if Borders.Bottom.Visible then DrawRectBottomBorder(Self.Canvas, ARect, Borders.Bottom.Color, Borders.Bottom.Width, Borders.Bottom.Style, 1);
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
          cyGradientFill(Self.Canvas, ARect, clBgFrom, clBgTo, grd.Orientation, grd.Balance, grd.AngleDegree, grd.BalanceMode, grd.MaxDegrade, grd.SpeedPercent);
        end
        else
        begin
          clBackground := FAppearance.ChangeColorItem.Background.Color;
          if Assigned(FOnGetItemBackgroundColor) then FOnGetItemBackgroundColor(Index, State, ItemData, clBackground);
          Brush.Color := clBackground;
          Rectangle(ARect);
        end;

        // ------------- Frame ----------------
        if FAppearance.ChangeColorItem.Background.DrawBorders then
        begin
          Borders := FAppearance.ChangeColorItem.Background.Borders;
          if Borders.Left.Visible then DrawRectLeftBorder(Self.Canvas, ARect, Borders.Left.Color, Borders.Left.Width, Borders.Left.Style);
          if Borders.Right.Visible then DrawRectRightBorder(Self.Canvas, ARect, Borders.Right.Color, Borders.Right.Width, Borders.Right.Style, 1);
          if Borders.Top.Visible then DrawRectTopBorder(Self.Canvas, ARect, Borders.Top.Color, Borders.Top.Width, Borders.Top.Style);
          if Borders.Bottom.Visible then DrawRectBottomBorder(Self.Canvas, ARect, Borders.Bottom.Color, Borders.Bottom.Width, Borders.Bottom.Style, 1);
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

        y := GetMiddlePosY(ARect, TextHeight(sName)) + FAppearance.ChangeColorItem.TextPosDeltaY;

        xTextWidth := TextWidth(sName);

        case FAppearance.ChangeColorItem.Alignment of
          taLeftJustify: xTextLeft := FAppearance.ChangeColorItem.CaptionMargin;
          taRightJustify: xTextLeft := ARect.Width - xTextWidth - FAppearance.ChangeColorItem.CaptionMargin;
          taCenter: xTextLeft := (ARect.Width div 2) - (xTextWidth div 2);
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

    {$REGION ' ----------- DRAW: Background ----------------- '}
    if bSelected then
    begin
      Pen.Style := psClear;

      // --------- Background color / gradient -------------
      if FAppearance.SelectedItem.Background.DrawGradient then
      begin
        grd := FAppearance.SelectedItem.Background.Gradient;
        clBgFrom := grd.ColorFrom;
        clBgTo := grd.ColorTo;
        if Assigned(FOnGetItemGradientColors) then FOnGetItemGradientColors(Index, State, ItemData, clBgFrom, clBgTo);
        cyGradientFill(Self.Canvas, ARect, clBgFrom, clBgTo, grd.Orientation, grd.Balance, grd.AngleDegree, grd.BalanceMode, grd.MaxDegrade, grd.SpeedPercent);
      end
      else
      begin
        clBackground := FAppearance.SelectedItem.Background.Color;
        if Assigned(FOnGetItemBackgroundColor) then FOnGetItemBackgroundColor(Index, State, ItemData, clBackground);
        Brush.Color := clBackground;
        Rectangle(ARect);
      end;

      // ------------- Frame ----------------
      if FAppearance.SelectedItem.Background.DrawBorders then
      begin
        Borders := FAppearance.SelectedItem.Background.Borders;
        if Borders.Left.Visible then DrawRectLeftBorder(Self.Canvas, ARect, Borders.Left.Color, Borders.Left.Width, Borders.Left.Style);
        if Borders.Right.Visible then DrawRectRightBorder(Self.Canvas, ARect, Borders.Right.Color, Borders.Right.Width, Borders.Right.Style, 1);
        if Borders.Top.Visible then DrawRectTopBorder(Self.Canvas, ARect, Borders.Top.Color, Borders.Top.Width, Borders.Top.Style);
        if Borders.Bottom.Visible then DrawRectBottomBorder(Self.Canvas, ARect, Borders.Bottom.Color, Borders.Bottom.Width, Borders.Bottom.Style, 1);
      end;

    end

    else

    begin
      if not Enabled then Brush.Color := FAppearance.DisabledBackgroundColor;
      clBackground := Brush.Color;
      if Assigned(FOnGetItemBackgroundColor) then FOnGetItemBackgroundColor(Index, State, ItemData, clBackground);
      Brush.Color := clBackground;
      Pen.Color := Brush.Color;
      Rectangle(ARect);
    end;

    {$ENDREGION}

    Pen.Style := psSolid;

    //GetColorFromStr(s, sName, clRectangle);
    clRectangle := ItemData.Color;


    //if odFocused in State then Font.Color := clHighlightText;

    {$REGION ' --------------- DRAW: Color rectangle ------------- '}
    if FAppearance.ColorRectangle.Visible and (FAppearance.ColorRectangle.Width > 0) then
    begin
      ColorRect := ARect;
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
    {$ENDREGION DRAW: Color rectangle}


    xTextStart := FAppearance.LeftMargin + FAppearance.TextMargin;
    if FAppearance.ColorRectangle.Visible then xTextStart := xTextStart + FAppearance.ColorRectangle.LeftMargin + FAppearance.ColorRectangle.Width;

    {$REGION ' ----------- DRAW: Text ---------- '}
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
    OutTextRect := ARect;

    if FAppearance.UseCustomNumericFont then
    begin

      xTextLeft := xTextStart;
      Font.Assign(FAppearance.NumericFont);
      clText := Font.Color;
      if bSelected then clText := FAppearance.NumericFontSelectedColor;
      if not Enabled then clText := FAppearance.DisabledFontColor;
      if bSelected and (not Enabled) then clText := FAppearance.SelectedItem.DisabledFontColor;
      if Assigned(FOnGetNumericItemTextColor) then FOnGetNumericItemTextColor(Index, State, ItemData, clText);
      Font.Color := clText;

      // User color value (FOnGetColorStrValue)
      if sUserColorStr <> '' then
      begin
        sOut := sUserColorStr;
        y := GetMiddlePosY(OutTextRect, TextHeight(sOut)) + FAppearance.NumericTextPosDeltaY;
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
        y := GetMiddlePosY(OutTextRect, TextHeight(sOut)) + FAppearance.NumericTextPosDeltaY;
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
        y := GetMiddlePosY(OutTextRect, TextHeight(sOut)) + FAppearance.NumericTextPosDeltaY;
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
        y := GetMiddlePosY(OutTextRect, TextHeight(sOut)) + FAppearance.NumericTextPosDeltaY;
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
      clText := Font.Color;
      if bSelected then clText := FAppearance.SelectedItem.FontColor;
      if not Enabled then clText := FAppearance.DisabledFontColor;
      if bSelected and (not Enabled) then clText := FAppearance.SelectedItem.DisabledFontColor;
      if Assigned(FOnGetItemTextColor) then FOnGetItemTextColor(Index, State, ItemData, clText);
      Font.Color := clText;

      // Color name
      if FAppearance.ShowColorName then
      begin
        sOut := sName;
        y := GetMiddlePosY(OutTextRect, TextHeight(sOut)) + FAppearance.TextPosDeltaY;
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

      y := GetMiddlePosY(OutTextRect, TextHeight(sOut)) + FAppearance.TextPosDeltaY;

      clText := Font.Color;
      if bSelected then clText := FAppearance.SelectedItem.FontColor;
      if not Enabled then clText := FAppearance.DisabledFontColor;
      if bSelected and (not Enabled) then clText := FAppearance.SelectedItem.DisabledFontColor;
      if Assigned(FOnGetItemTextColor) then FOnGetItemTextColor(Index, State, ItemData, clText);
      Font.Color := clText;

      TextOut(xTextStart, y, sOut);
    end;

    {$ENDREGION DRAW: Text}
  end;

  if Assigned(FOnAfterPaintItem) then FOnAfterPaintItem(Index, ARect, State, ItemData);

end;


{$ENDREGION DrawItem}




{$ENDREGION TJppCustomColorListBox}



{$Region ' -----------------------------  TJppColorListBoxAppearance ---------------------- '}

constructor TJppColorListBoxAppearance.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FOwner := AOwner;
  FChangeColorItem := TJppColorControlGradientCaptionItem.Create(AOwner);

  FChangeColorItem.OnChange := (*{$IFDEF FPC} @ {$ENDIF}*)PropsChanged;

  if (csLoading in AOwner.ComponentState) then
  begin
    ChangeColorItem.Background.DrawGradient := False;
    ChangeColorItem.Background.Color := clWindow;
    ChangeColorItem.Background.DrawBorders := False;
  end;

  FChangeColorItem.Caption := 'Select color...';
  FChangeColorItem.CaptionMargin := 6;

  FChangeColorItem.Alignment := taLeftJustify;
  FOnGetColorStrValue := nil;

end;

destructor TJppColorListBoxAppearance.Destroy;
begin
  FChangeColorItem.Free;
  inherited;
end;

procedure TJppColorListBoxAppearance.Assign(clba: TJppColorListBoxAppearance);
begin
  BeginUpdate;
  try
    FChangeColorItem.Assign(clba.ChangeColorItem);
    inherited Assign(TJppColorControlAppearance(clba));
  finally
    EndUpdate;
  end;
end;

procedure TJppColorListBoxAppearance.SetChangeColorItem(const Value: TJppColorControlGradientCaptionItem);
begin
  FChangeColorItem := Value;
  PropsChanged(Self);
end;


{$endregion TJPColorComboBoxAppearance}

initialization

  {$IFDEF FPC}
  RegisterClass(TJppColorListBox);
  {$ENDIF}

end.
