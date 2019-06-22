unit JPP.StorageCtrl;

{
  Jacek Pazera
  http://www.pazera-software.com
  https://github.com/jackdp
  Last mod: 2019.05.25
}

{$IFDEF FPC} {$mode objfpc}{$H+} {$ENDIF}

interface

uses
  {$IFDEF DCC}
  Winapi.Messages, Winapi.Windows,
  System.SysUtils, System.Classes, System.Types, System.UITypes, Vcl.Graphics,
  {$ELSE}
  SysUtils, Classes, Graphics, LCLType, Types,
  {$ENDIF}

  JPP.Types, JPP.Common;


type


  {$region ' --------- TJppStorageCtrlItem - collection item ----------- '}
  TJppStorageCtrlItem = class(TCollectionItem)
  private
    FStrValue1: string;
    FStrValue2: string;
    FStrValue3: string;
    FStrValue4: string;
    FIntValue1: Integer;
    FIntValue2: Integer;
    FInt64Value1: Int64;
    FInt64Value2: Int64;
    FFloatValue1: Double;
    FFloatValue2: Double;
    FBoolValue1: Boolean;
    FBoolValue2: Boolean;
    FColorValue1: TColor;
    FColorValue2: TColor;
    FItemName: string;
    FTag: Integer;
    FPointerValue1: Pointer;
    FPointerValue2: Pointer;
    FByteValue1: Byte;
    FByteValue2: Byte;
    procedure SetStrValue1(const Value: string);
    procedure SetStrValue2(const Value: string);
    procedure SetStrValue3(const Value: string);
    procedure SetStrValue4(const Value: string);
    procedure SetIntValue1(const Value: Integer);
    procedure SetIntValue2(const Value: Integer);
    procedure SetInt64Value1(const Value: Int64);
    procedure SetInt64Value2(const Value: Int64);
    procedure SetFloatValue1(const Value: Double);
    procedure SetFloatValue2(const Value: Double);
    procedure SetBoolValue1(const Value: Boolean);
    procedure SetBoolValue2(const Value: Boolean);
    procedure SetColorValue1(const Value: TColor);
    procedure SetColorValue2(const Value: TColor);
    procedure SetItemName(const Value: string);
    procedure SetTag(const Value: Integer);
    procedure SetPointerValue1(const Value: Pointer);
    procedure SetPointerValue2(const Value: Pointer);
    procedure SetByteValue1(const Value: Byte);
    procedure SetByteValue2(const Value: Byte);
  protected
  public
    constructor Create(ACollection: TCollection); override;
    destructor Destroy; override;
    property PointerValue1: Pointer read FPointerValue1 write SetPointerValue1 default nil;
    property PointerValue2: Pointer read FPointerValue2 write SetPointerValue2;
  published
    property ItemName: string read FItemName write SetItemName;
    property Tag: Integer read FTag write SetTag default 0;
    property StrValue1: string read FStrValue1 write SetStrValue1;
    property StrValue2: string read FStrValue2 write SetStrValue2;
    property StrValue3: string read FStrValue3 write SetStrValue3;
    property StrValue4: string read FStrValue4 write SetStrValue4;
    property IntValue1: Integer read FIntValue1 write SetIntValue1 default 0;
    property IntValue2: Integer read FIntValue2 write SetIntValue2 default 0;
    property Int64Value1: Int64 read FInt64Value1 write SetInt64Value1 default 0;
    property Int64Value2: Int64 read FInt64Value2 write SetInt64Value2 default 0;
    property FloatValue1: Double read FFloatValue1 write SetFloatValue1;
    property FloatValue2: Double read FFloatValue2 write SetFloatValue2;
    property BoolValue1: Boolean read FBoolValue1 write SetBoolValue1 default False;
    property BoolValue2: Boolean read FBoolValue2 write SetBoolValue2 default False;
    property ColorValue1: TColor read FColorValue1 write SetColorValue1 default clNone;
    property ColorValue2: TColor read FColorValue2 write SetColorValue2 default clNone;
    property ByteValue1: Byte read FByteValue1 write SetByteValue1 default 0;
    property ByteValue2: Byte read FByteValue2 write SetByteValue2 default 0;
  end;
  {$endregion}


  {$region ' -------- TJppStorageCtrlCollection - collection -------- '}
  TJppStorageCtrlCollection = class(TCollection)
  private
    FOwner: TComponent;
    function GetItem(Index: Integer): TJppStorageCtrlItem;
    procedure SetItem(Index: Integer; const Value: TJppStorageCtrlItem);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AOwner: TComponent);
    function Add: TJppStorageCtrlItem;
    procedure Delete(Index: Integer);
    function Insert(Index: Integer): TJppStorageCtrlItem;
    property Items[Index: Integer]: TJppStorageCtrlItem read GetItem write SetItem; default;
  published
  end;
  {$endregion}


  {$region ' --------- TJppCustomStorageCtrl ----------- '}
  TJppCustomStorageCtrl = class(TComponent)
  private
    FTagExt: TJppTagExt;
    FStorageCollection: TJppStorageCtrlCollection;
    procedure SetHorizontalBars(const Value: TJppStorageCtrlCollection);
    procedure SetTagExt(const Value: TJppTagExt);
    function GetItem(Index: Integer): TJppStorageCtrlItem;
    procedure SetItem(Index: Integer; const Value: TJppStorageCtrlItem);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AsText: string;
    procedure Clear;
    function AddItem: TJppStorageCtrlItem;
    function Count: integer;

    property Items[Index: Integer]: TJppStorageCtrlItem read GetItem write SetItem; default;
    property TagExt: TJppTagExt read FTagExt write SetTagExt;
    property StorageCollection: TJppStorageCtrlCollection read FStorageCollection write SetHorizontalBars;
  end;
  {$endregion}


  {$region ' ----------- TJppStorageCtrl ----------- '}
  TJppStorageCtrl = class(TJppCustomStorageCtrl)
  published
    property TagExt;
    property StorageCollection;
  end;
  {$endregion}





implementation





{$region ' ------------------- TJppCustomStorageCtrl ---------------------------- '}

constructor TJppCustomStorageCtrl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTagExt := TJppTagExt.Create(Self);
  FStorageCollection := TJppStorageCtrlCollection.Create(Self);
end;

destructor TJppCustomStorageCtrl.Destroy;
begin
  FTagExt.Free;
  FStorageCollection.Free;
  inherited Destroy;
end;

procedure TJppCustomStorageCtrl.Clear;
begin
  StorageCollection.Clear;
end;

function TJppCustomStorageCtrl.Count: integer;
begin
  Result := StorageCollection.Count;
end;

function TJppCustomStorageCtrl.AddItem: TJppStorageCtrlItem;
begin
  Result := StorageCollection.Add;
end;

function TJppCustomStorageCtrl.GetItem(Index: Integer): TJppStorageCtrlItem;
begin
  Result := StorageCollection.Items[Index];
end;

procedure TJppCustomStorageCtrl.SetHorizontalBars(const Value: TJppStorageCtrlCollection);
begin
  FStorageCollection := Value;
end;

procedure TJppCustomStorageCtrl.SetItem(Index: Integer; const Value: TJppStorageCtrlItem);
begin
  StorageCollection.Items[Index] := Value;
end;

procedure TJppCustomStorageCtrl.SetTagExt(const Value: TJppTagExt);
begin
  FTagExt := Value;
end;

function TJppCustomStorageCtrl.AsText: string;
var
  i: integer;
  si: TJppStorageCtrlItem;
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
    si := StorageCollection[i];
    s := s + 'Item ' + IntToStr(i) + ENDL;
    s := s + Sep + 'Name: ' + si.ItemName + ENDL;
    s := s + Sep + 'Tag: ' + IntToStr(si.Tag) + ENDL;
    s := s + Sep + 'StrValue1: ' + si.StrValue1 + ENDL;
    s := s + Sep + 'StrValue2: ' + si.StrValue2 + ENDL;
    s := s + Sep + 'StrValue3: ' + si.StrValue3 + ENDL;
    s := s + Sep + 'StrValue4: ' + si.StrValue4 + ENDL;
    s := s + Sep + 'IntValue1: ' + IntToStr(si.IntValue1) + ENDL;
    s := s + Sep + 'IntValue2: ' + IntToStr(si.IntValue2) + ENDL;
    s := s + Sep + 'Int64Value1: ' + IntToStr(si.Int64Value1) + ENDL;
    s := s + Sep + 'Int64Value2: ' + IntToStr(si.Int64Value2) + ENDL;
    s := s + Sep + 'FloatValue1: ' + FormatFloat('0.00', si.FloatValue1) + ENDL;
    s := s + Sep + 'FloatValue2: ' + FormatFloat('0.00', si.FloatValue2) + ENDL;
    s := s + Sep + 'BoolValue1: ' + btos(si.BoolValue1) + ENDL;
    s := s + Sep + 'BoolValue2: ' + btos(si.BoolValue2) + ENDL;
    s := s + Sep + 'ColorValue1: ' + ColorToString(si.ColorValue1) + ENDL;
    s := s + Sep + 'ColorValue2: ' + ColorToString(si.ColorValue2) + ENDL;
    s := s + Sep + 'ByteValue1: ' + IntToStr(si.ByteValue1) + ENDL;
    s := s + Sep + 'ByteValue2: ' + IntToStr(si.ByteValue2) + ENDL;
    s := s + Sep + 'PointerValue1: ' + IntToStr({%H-}integer(si.PointerValue1)) + ENDL;
    s := s + Sep + 'PointerValue2: ' + IntToStr({%H-}integer(si.PointerValue2)) + ENDL;
  end;
  Result := s;
end;

{$endregion TJppCustomStorageCtrl}


{$region ' ------------------ TJppStorageCtrlItem - collection item ------------------- '}
constructor TJppStorageCtrlItem.Create(ACollection: TCollection);
begin
  inherited;

  Tag := 0;
  StrValue1 := '';
  StrValue2 := '';
  StrValue3 := '';
  StrValue4 := '';
  IntValue1 := 0;
  IntValue2 := 0;
  Int64Value1 := 0;
  Int64Value2 := 0;
  FloatValue1 := 0;
  FloatValue2 := 0;
  BoolValue1 := False;
  BoolValue2 := False;
  ColorValue1 := clNone;
  ColorValue2 := clNone;
  PointerValue1 := nil;
  PointerValue2 := nil;
  ByteValue1 := 0;
  ByteValue2 := 0;
end;

destructor TJppStorageCtrlItem.Destroy;
begin
  inherited;
end;

procedure TJppStorageCtrlItem.SetBoolValue1(const Value: Boolean);
begin
  FBoolValue1 := Value;
end;

procedure TJppStorageCtrlItem.SetBoolValue2(const Value: Boolean);
begin
  FBoolValue2 := Value;
end;

procedure TJppStorageCtrlItem.SetByteValue1(const Value: Byte);
begin
  FByteValue1 := Value;
end;

procedure TJppStorageCtrlItem.SetByteValue2(const Value: Byte);
begin
  FByteValue2 := Value;
end;

procedure TJppStorageCtrlItem.SetColorValue1(const Value: TColor);
begin
  FColorValue1 := Value;
end;

procedure TJppStorageCtrlItem.SetColorValue2(const Value: TColor);
begin
  FColorValue2 := Value;
end;

procedure TJppStorageCtrlItem.SetFloatValue1(const Value: Double);
begin
  FFloatValue1 := Value;
end;

procedure TJppStorageCtrlItem.SetFloatValue2(const Value: Double);
begin
  FFloatValue2 := Value;
end;

procedure TJppStorageCtrlItem.SetInt64Value1(const Value: Int64);
begin
  FInt64Value1 := Value;
end;

procedure TJppStorageCtrlItem.SetInt64Value2(const Value: Int64);
begin
  FInt64Value2 := Value;
end;

procedure TJppStorageCtrlItem.SetIntValue1(const Value: Integer);
begin
  FIntValue1 := Value;
end;

procedure TJppStorageCtrlItem.SetIntValue2(const Value: Integer);
begin
  FIntValue2 := Value;
end;

procedure TJppStorageCtrlItem.SetItemName(const Value: string);
begin
  FItemName := Value;
end;

procedure TJppStorageCtrlItem.SetPointerValue2(const Value: Pointer);
begin
  FPointerValue2 := Value;
end;

procedure TJppStorageCtrlItem.SetPointerValue1(const Value: Pointer);
begin
  FPointerValue1 := Value;
end;

procedure TJppStorageCtrlItem.SetStrValue1(const Value: string);
begin
  FStrValue1 := Value;
end;

procedure TJppStorageCtrlItem.SetStrValue2(const Value: string);
begin
  FStrValue2 := Value;
end;

procedure TJppStorageCtrlItem.SetStrValue3(const Value: string);
begin
  FStrValue3 := Value;
end;

procedure TJppStorageCtrlItem.SetStrValue4(const Value: string);
begin
  FStrValue4 := Value;
end;

procedure TJppStorageCtrlItem.SetTag(const Value: Integer);
begin
  FTag := Value;
end;

{$endregion TJppStorageCtrlItem}


{$region ' ---------------------------------- TJppStorageCtrlCollection - collection ----------------------------------------- '}
function TJppStorageCtrlCollection.Add: TJppStorageCtrlItem;
begin
  Result := TJppStorageCtrlItem(inherited Add);
end;

function TJppStorageCtrlCollection.Insert(Index: Integer): TJppStorageCtrlItem;
begin
  Result := TJppStorageCtrlItem(inherited Insert(Index));
end;

constructor TJppStorageCtrlCollection.Create(AOwner: TComponent);
begin
  inherited Create(TJppStorageCtrlItem);
  FOwner := AOwner;
end;

procedure TJppStorageCtrlCollection.Delete(Index: Integer);
begin
  inherited Delete(Index);
end;

function TJppStorageCtrlCollection.GetItem(Index: Integer): TJppStorageCtrlItem;
begin
  Result := TJppStorageCtrlItem(inherited GetItem(Index));
end;

function TJppStorageCtrlCollection.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TJppStorageCtrlCollection.SetItem(Index: Integer; const Value: TJppStorageCtrlItem);
begin
  inherited SetItem(Index, Value);
end;

{$endregion TJppStorageCtrlCollection}

end.
