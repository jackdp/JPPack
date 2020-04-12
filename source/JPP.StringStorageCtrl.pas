unit JPP.StringStorageCtrl;

{
  Jacek Pazera
  http://www.pazera-software.com
  https://github.com/jackdp
}


{$I jpp.inc}
{$IFDEF FPC} {$mode objfpc}{$H+} {$ENDIF}


interface

uses
  {$IFDEF MSWINDOWS}Windows,{$ENDIF}
  SysUtils, Classes, Graphics, Types, {$IFDEF HAS_SYSTEM_UITYPES}System.UITypes,{$ENDIF}
  {$IFDEF FPC}LCLType,{$ENDIF}
  JPP.Types, JPP.Common;


type

  TJppStringStorageFileContent = (ssfcNames, ssfcValues, ssfcNamesAndValues);

  {$region ' --------- TJppStringStorageCtrlItem - collection item ----------- '}
  TJppStringStorageCtrlItem = class(TCollectionItem)
  private
    FValue: string;
    FItemName: string;
    FTag: Integer;
    FEnabled: Boolean;
    procedure SetValue(const Value: string);
    procedure SetItemName(const Value: string);
    procedure SetTag(const Value: Integer);
    procedure SetEnabled(const Value: Boolean);
  protected
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
  published
    property ItemName: string read FItemName write SetItemName;
    property Enabled: Boolean read FEnabled write SetEnabled;
    property Tag: Integer read FTag write SetTag default 0;
    property Value: string read FValue write SetValue;
  end;
  {$endregion}


  {$region ' -------- TJppStringStorageCtrlCollection - collection -------- '}
  TJppStringStorageCtrlCollection = class(TCollection)
  private
    FOwner: TComponent;
    function GetItem(Index: Integer): TJppStringStorageCtrlItem;
    procedure SetItem(Index: Integer; const Value: TJppStringStorageCtrlItem);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TComponent);
    function Add: TJppStringStorageCtrlItem;
    procedure Delete(Index: Integer);
    function Insert(Index: Integer): TJppStringStorageCtrlItem;
    property Items[Index: Integer]: TJppStringStorageCtrlItem read GetItem write SetItem; default;
  published
  end;
  {$endregion}


  {$region ' --------- TJppCustomStringStorageCtrl ----------- '}
  TJppCustomStringStorageCtrl = class(TComponent)
  private
    FTagExt: TJppTagExt;
    FStorageCollection: TJppStringStorageCtrlCollection;
    procedure SetStorageCollection(const Value: TJppStringStorageCtrlCollection);
    procedure SetTagExt(const Value: TJppTagExt);
    function GetItem(Index: Integer): TJppStringStorageCtrlItem;
    procedure SetItem(Index: Integer; const Value: TJppStringStorageCtrlItem);
    procedure EnableOrDisableAll(const Enable: Boolean);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AsText: string;
    procedure Clear;

    procedure GetValues(sl: TStrings);
    procedure GetNames(sl: TStrings);
    procedure GetNameValuePairs(sl: TStrings);

    procedure AddValues(sl: TStrings);
    procedure AddNames(sl: TStrings);
    procedure AddNameValuePairs(sl: TStrings; UnknownAsValues: Boolean = True);

    procedure AssignNames(sl: TStrings);
    procedure AssignValues(sl: TStrings);
    procedure AssignNameValuePairs(sl: TStrings; UnknownAsValues: Boolean = True);

    procedure EnableAll;
    procedure DisableAll;

    function IsValidIndex(const Index: integer): Boolean;
    function ValueIndex(const Value: string; IgnoreCase: Boolean = False): integer;
    function NameIndex(const ItemName: string; IgnoreCase: Boolean = False): integer;
    function TryGetValueByName(const ItemName: string; out Value: string; IgnoreCase: Boolean = False): Boolean;
    function TryGetNameByValue(const Value: string; out ItemName: string; IgnoreCase: Boolean = False): Boolean;

    function AddItem: TJppStringStorageCtrlItem;
    function Count: integer;

    procedure SaveToFile(const FileName: string; Encoding: TEncoding; FileContent: TJppStringStorageFileContent = ssfcNamesAndValues);

    property Items[Index: Integer]: TJppStringStorageCtrlItem read GetItem write SetItem; default;
    property TagExt: TJppTagExt read FTagExt write SetTagExt;
    property StorageCollection: TJppStringStorageCtrlCollection read FStorageCollection write SetStorageCollection;
  end;
  {$endregion}


  {$region ' ----------- TJppStringStorageCtrl ----------- '}
  TJppStringStorageCtrl = class(TJppCustomStringStorageCtrl)
  published
    property TagExt;
    property StorageCollection;
  end;
  {$endregion}





implementation





{$region ' ------------------- TJppCustomStringStorageCtrl ---------------------------- '}

constructor TJppCustomStringStorageCtrl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTagExt := TJppTagExt.Create(Self);
  FStorageCollection := TJppStringStorageCtrlCollection.Create(Self);
end;

destructor TJppCustomStringStorageCtrl.Destroy;
begin
  FTagExt.Free;
  FStorageCollection.Free;
  inherited Destroy;
end;

procedure TJppCustomStringStorageCtrl.DisableAll;
begin
  EnableOrDisableAll(False);
end;

procedure TJppCustomStringStorageCtrl.EnableAll;
begin
  EnableOrDisableAll(True);
end;

procedure TJppCustomStringStorageCtrl.EnableOrDisableAll(const Enable: Boolean);
var
  i: integer;
begin
  for i := 0 to StorageCollection.Count - 1 do
    StorageCollection[i].Enabled := Enable;
end;

procedure TJppCustomStringStorageCtrl.Clear;
begin
  StorageCollection.Clear;
end;

function TJppCustomStringStorageCtrl.Count: integer;
begin
  Result := StorageCollection.Count;
end;

function TJppCustomStringStorageCtrl.AddItem: TJppStringStorageCtrlItem;
begin
  Result := StorageCollection.Add;
  //Result.Enabled := True;
end;

function TJppCustomStringStorageCtrl.GetItem(Index: Integer): TJppStringStorageCtrlItem;
begin
  Result := StorageCollection.Items[Index];
end;

procedure TJppCustomStringStorageCtrl.GetNames(sl: TStrings);
var
  i: integer;
  Item: TJppStringStorageCtrlItem;
begin
  for i := 0 to StorageCollection.Count - 1 do
  begin
    Item := StorageCollection[i];
    sl.Add(Item.ItemName);
  end;
end;

procedure TJppCustomStringStorageCtrl.GetNameValuePairs(sl: TStrings);
var
  i: integer;
  Item: TJppStringStorageCtrlItem;
begin
  for i := 0 to StorageCollection.Count - 1 do
  begin
    Item := StorageCollection[i];
    sl.Add(Item.ItemName + '=' + Item.Value);
  end;
end;

procedure TJppCustomStringStorageCtrl.GetValues(sl: TStrings);
var
  i: integer;
  Item: TJppStringStorageCtrlItem;
begin
  for i := 0 to StorageCollection.Count - 1 do
  begin
    Item := StorageCollection[i];
    sl.Add(Item.Value);
  end;
end;

function TJppCustomStringStorageCtrl.IsValidIndex(const Index: integer): Boolean;
begin
  if Self.Count = 0 then Exit(False);
  Result := (Index = 0) or (Index < Count);
end;


procedure TJppCustomStringStorageCtrl.SaveToFile(const FileName: string; Encoding: TEncoding; FileContent: TJppStringStorageFileContent = ssfcNamesAndValues);
var
  sl: TStringList;
begin
  sl := TStringList.Create;
  try
    case FileContent of
      ssfcNames: GetNames(sl);
      ssfcValues: GetValues(sl);
      ssfcNamesAndValues: GetNameValuePairs(sl);
    else
      GetNameValuePairs(sl); // tak na wszelki wypadek
    end;

    {$IFDEF DCC}
    {$IFDEF HAS_TSTRINGS_WRITEBOM}sl.WriteBOM := True;{$ENDIF}
    sl.SaveToFile(FileName, Encoding);
    {$ENDIF}

    {$IFDEF FPC}
      {$IFDEF HAS_TSTRINGS_WRITEBOM}sl.WriteBOM := True;{$ENDIF}
      {$IFDEF HAS_SAVE_WITH_ENCODING}sl.SaveToFile(FileName, Encoding);{$ELSE}sl.SaveToFile(FileName);{$ENDIF}
    {$ENDIF}



  finally
    sl.Free;
  end;
end;

procedure TJppCustomStringStorageCtrl.SetStorageCollection(const Value: TJppStringStorageCtrlCollection);
begin
  FStorageCollection := Value;
end;

procedure TJppCustomStringStorageCtrl.SetItem(Index: Integer; const Value: TJppStringStorageCtrlItem);
begin
  StorageCollection.Items[Index] := Value;
end;

procedure TJppCustomStringStorageCtrl.SetTagExt(const Value: TJppTagExt);
begin
  FTagExt := Value;
end;

function TJppCustomStringStorageCtrl.TryGetNameByValue(const Value: string; out ItemName: string; IgnoreCase: Boolean): Boolean;
var
  xInd: integer;
begin
  xInd := ValueIndex(Value, IgnoreCase);
  if xInd < 0 then Exit(False);
  ItemName := Items[xInd].ItemName;
  Result := True;
end;

function TJppCustomStringStorageCtrl.TryGetValueByName(const ItemName: string; out Value: string; IgnoreCase: Boolean = False): Boolean;
var
  xInd: integer;
begin
  xInd := NameIndex(ItemName, IgnoreCase);
  if xInd < 0 then Exit(False);
  Value := Items[xInd].Value;
  Result := True;
end;

function TJppCustomStringStorageCtrl.ValueIndex(const Value: string; IgnoreCase: Boolean): integer;
var
  i: integer;
  s: string;
  b: Boolean;
begin
  Result := -1;
  if Count = 0 then Exit;
  s := Value;
  if IgnoreCase then s := AnsiUpperCase(s);

  for i := 0 to Count - 1 do
  begin
    if IgnoreCase then b := s = AnsiUpperCase(Items[i].Value)
    else b := s = Items[i].Value;
    if b then
    begin
      Result := i;
      Break;
    end;
  end;
end;

function TJppCustomStringStorageCtrl.NameIndex(const ItemName: string; IgnoreCase: Boolean): integer;
var
  i: integer;
  s: string;
  b: Boolean;
begin
  Result := -1;
  if Count = 0 then Exit;
  s := ItemName;
  if IgnoreCase then s := AnsiUpperCase(s);

  for i := 0 to Count - 1 do
  begin
    if IgnoreCase then b := s = AnsiUpperCase(Items[i].ItemName)
    else b := s = Items[i].ItemName;
    if b then
    begin
      Result := i;
      Break;
    end;
  end;
end;

procedure TJppCustomStringStorageCtrl.AddNames(sl: TStrings);
var
  i: integer;
  Item: TJppStringStorageCtrlItem;
begin
  for i := 0 to sl.Count - 1 do
  begin
    Item := StorageCollection.Add;
    Item.ItemName := sl[i];
  end;
end;

procedure TJppCustomStringStorageCtrl.AddNameValuePairs(sl: TStrings; UnknownAsValues: Boolean = True);
var
  i, xp: integer;
  Item: TJppStringStorageCtrlItem;
  s: string;
begin
  for i := 0 to sl.Count - 1 do
  begin
    Item := StorageCollection.Add;
    s := sl[i];
    xp := Pos('=', s);
    if xp > 0 then
    begin
      Item.ItemName := Copy(s, 1, xp - 1);
      Item.Value := Copy(s, xp + 1, Length(s));
    end
    else
      if UnknownAsValues then Item.Value := s
      else Item.ItemName := s;
  end;
end;

procedure TJppCustomStringStorageCtrl.AddValues(sl: TStrings);
var
  i: integer;
  Item: TJppStringStorageCtrlItem;
begin
  for i := 0 to sl.Count - 1 do
  begin
    Item := StorageCollection.Add;
    Item.Value := sl[i];
  end;
end;

procedure TJppCustomStringStorageCtrl.AssignNames(sl: TStrings);
begin
  StorageCollection.Clear;
  AddNames(sl);
end;

procedure TJppCustomStringStorageCtrl.AssignNameValuePairs(sl: TStrings; UnknownAsValues: Boolean = True);
begin
  StorageCollection.Clear;
  AddNameValuePairs(sl, UnknownAsValues);
end;

procedure TJppCustomStringStorageCtrl.AssignValues(sl: TStrings);
begin
  StorageCollection.Clear;
  AddValues(sl);
end;

function TJppCustomStringStorageCtrl.AsText: string;
var
  i: integer;
  Item: TJppStringStorageCtrlItem;
  s, Sep: string;

  function btos(b: Boolean): string;
  begin
    if b then Result := 'True' else Result := 'False';
  end;

begin
  s := '';
  Sep := '  ';
  for i := 0 to StorageCollection.Count - 1 do
  begin
    Item := StorageCollection[i];
    s := s + 'Item ' + IntToStr(i) + ENDL;
    s := s + Sep + 'Enabled: ' + btos(Item.Enabled) + ENDL;
    s := s + Sep + 'Name: ' + Item.ItemName + ENDL;
    s := s + Sep + 'Tag: ' + IntToStr(Item.Tag) + ENDL;
    s := s + Sep + 'StrValue1: ' + Item.Value + ENDL;
  end;
  Result := s;
end;

{$endregion TJppCustomStringStorageCtrl}


{$region ' ------------------ TJppStringStorageCtrlItem - collection item ------------------- '}
constructor TJppStringStorageCtrlItem.Create(ACollection: TCollection);
begin
  inherited;

  Tag := 0;
  FValue := '';
  FItemName := '';
  FEnabled := True;
end;

destructor TJppStringStorageCtrlItem.Destroy;
begin
  inherited;
end;



procedure TJppStringStorageCtrlItem.SetEnabled(const Value: Boolean);
begin
  FEnabled := Value;
end;

procedure TJppStringStorageCtrlItem.SetItemName(const Value: string);
begin
  FItemName := Value;
end;

procedure TJppStringStorageCtrlItem.SetValue(const Value: string);
begin
  FValue := Value;
end;


procedure TJppStringStorageCtrlItem.SetTag(const Value: Integer);
begin
  FTag := Value;
end;

{$endregion TJppStringStorageCtrlItem}


{$region ' ---------------------------------- TJppStringStorageCtrlCollection - collection ----------------------------------------- '}
function TJppStringStorageCtrlCollection.Add: TJppStringStorageCtrlItem;
begin
  Result := TJppStringStorageCtrlItem(inherited Add);
end;

function TJppStringStorageCtrlCollection.Insert(Index: Integer): TJppStringStorageCtrlItem;
begin
  Result := TJppStringStorageCtrlItem(inherited Insert(Index));
end;

constructor TJppStringStorageCtrlCollection.Create(AOwner: TComponent);
begin
  inherited Create(TJppStringStorageCtrlItem);
  FOwner := AOwner;
end;

procedure TJppStringStorageCtrlCollection.Delete(Index: Integer);
begin
  inherited Delete(Index);
end;

function TJppStringStorageCtrlCollection.GetItem(Index: Integer): TJppStringStorageCtrlItem;
begin
  Result := TJppStringStorageCtrlItem(inherited GetItem(Index));
end;

function TJppStringStorageCtrlCollection.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TJppStringStorageCtrlCollection.SetItem(Index: Integer; const Value: TJppStringStorageCtrlItem);
begin
  inherited SetItem(Index, Value);
end;

{$endregion TJppStringStorageCtrlCollection}

end.
