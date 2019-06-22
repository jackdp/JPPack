unit JPP.PngCollection;

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
  System.SysUtils, System.Classes, System.Types, System.UITypes,
  Vcl.Graphics, Vcl.Imaging.PngImage,
  {$ELSE}
  SysUtils, Classes, Graphics,
  {$ENDIF}
  JPP.Common, JPL.Strings, JPL.Conversion, JPL.Colors;


type

  {$IFDEF FPC}
  TPngImage = TPortableNetworkGraphic;
  {$ENDIF}

  TJppPngCollectionItem = class;
  TJppPngCollectionItems = class;

  {$region ' --- TJppPngCollection --- '}
  TJppPngCollection = class(TComponent)
  private
    FItems: TJppPngCollectionItems;
    FTagExt: TJppTagExt;
    procedure EnableOrDisableAll(const Enable: Boolean);
    procedure SetTagExt(const Value: TJppTagExt);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    procedure EnableAll;
    procedure DisableAll;
    function PngIndex(const PngName: string; IgnoreCase: Boolean = True): integer;
    function Count: integer;
    function IsValidIndex(const Index: integer): Boolean;
    function AddPngImageFromFile(const FileName: string): integer; // Returns the index of the added item
    function AddPngImage(Png: TPngImage; ImageName: string = ''; Description: string = ''; Enabled: Boolean = True; ATag: integer = 0): integer; // Returns the index of the added item
    function GetPngImage(const Index: integer): TPngImage;
    function GetPngImageByName(const PngName: string; IgnoreCase: Boolean = True): TPngImage; // Returns the first PngImage with the given name
    function ReportStr: string; // for debug
  published
    property Items: TJppPngCollectionItems read FItems write FItems;
    property TagExt: TJppTagExt read FTagExt write SetTagExt;
  end;
  {$endregion TJppPngCollection}


  {$region ' --- TJppPngCollectionItems --- '}
  TJppPngCollectionItems = class(TCollection)
  private
    FOwner: TPersistent;
    function GetItem(Index: Integer): TJppPngCollectionItem;
    procedure SetItem(Index: Integer; const Value: TJppPngCollectionItem);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
    function IsValidIndex(const Index: integer): Boolean;
  public
    constructor Create(AOwner: TPersistent);
    function Add: TJppPngCollectionItem; //reintroduce;
    procedure Assign(Source: TPersistent); override;
    function Insert(Index: Integer): TJppPngCollectionItem;
    property Items[index: Integer]: TJppPngCollectionItem read GetItem write SetItem; default;
  end;
  {$endregion TJppPngCollectionItems}


  {$region ' --- TJppPngCollectionItem --- '}
  TJppPngCollectionItem = class(TCollectionItem)
  private
    FName: string;
    FDescription: string;
    FPngImage: TPngImage;
    FTag: integer;
    FEnabled: Boolean;
    procedure SetPngImage(const Value: TPngImage);
    function GetWidth: integer;
    function GetHeight: integer;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetDisplayName: string; override;
  public
    constructor Create(ACollection: TCollection); overload; override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Name: string read FName write FName;
    property Description: string read FDescription write FDescription;
    property PngImage: TPngImage read FPngImage write SetPngImage;
    property Width: integer read GetWidth;
    property Height: integer read GetHeight;
    property Tag: integer read FTag write FTag default 0;
    property Enabled: Boolean read FEnabled write FEnabled default True;
  end;
  {$endregion TJppPngCollectionItem}

implementation





{$region ' --------------------- TJppPngCollection ----------------------- '}
constructor TJppPngCollection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FItems := TJppPngCollectionItems.Create(Self);
  FTagExt := TJppTagExt.Create(Self);
end;

destructor TJppPngCollection.Destroy;
begin
  FItems.Free;
  FTagExt.Free;
  inherited Destroy;
end;

function TJppPngCollection.AddPngImage(Png: TPngImage; ImageName, Description: string; Enabled: Boolean; ATag: integer): integer;
var
  pci: TJppPngCollectionItem;
begin
  pci := Items.Add;
  try
    pci.PngImage.Assign(Png);
    Result := pci.Index;
    if ImageName <> '' then pci.Name := ImageName;
    pci.Description := Description;
    pci.Enabled := Enabled;
    pci.Tag := ATag;
  except
    Result := -1;
  end;
end;

function TJppPngCollection.AddPngImageFromFile(const FileName: string): integer;
var
  Png: TPngImage;
begin
  if not FileExists(FileName) then Exit(-1);
  Png := TPngImage.Create;
  try

    try
      Png.LoadFromFile(FileName);
      Result := AddPngImage(Png);
    except
      Result := -1;
    end;

  finally
    Png.Free;
  end;
end;

procedure TJppPngCollection.Clear;
begin
  FItems.Clear;
end;

function TJppPngCollection.Count: integer;
begin
  Result := FItems.Count;
end;

procedure TJppPngCollection.DisableAll;
begin
  EnableOrDisableAll(False);
end;

procedure TJppPngCollection.EnableAll;
begin
  EnableOrDisableAll(True);
end;

procedure TJppPngCollection.EnableOrDisableAll(const Enable: Boolean);
var
  i: integer;
begin
  for i := 0 to FItems.Count - 1 do
    FItems[i].Enabled := Enable;
end;

function TJppPngCollection.GetPngImage(const Index: integer): TPngImage;
begin
  Result := Items[Index].PngImage;
end;

function TJppPngCollection.GetPngImageByName(const PngName: string; IgnoreCase: Boolean): TPngImage;
var
  x: integer;
begin
  x := PngIndex(PngName, IgnoreCase);
  if x < 0 then Exit(nil);
  Result := Items[x].PngImage;
end;

function TJppPngCollection.IsValidIndex(const Index: integer): Boolean;
begin
  Result := Items.IsValidIndex(Index);
end;

function TJppPngCollection.PngIndex(const PngName: string; IgnoreCase: Boolean): integer;
var
  i: integer;
  s: string;
  b: Boolean;
begin
  Result := -1;
  if FItems.Count = 0 then Exit;
  s := PngName;
  if IgnoreCase then s := AnsiUpperCase(s);

  for i := 0 to FItems.Count - 1 do
  begin
    if IgnoreCase then b := s = AnsiUpperCase(FItems[i].Name)
    else b := s = FItems[i].Name;
    if b then
    begin
      Result := i;
      Break;
    end;
  end;
end;

function TJppPngCollection.ReportStr: string;
var
  i: integer;
  s, Sep: string;
  pci: TJppPngCollectionItem;
  b: Boolean;
begin
  s :=
    'PngCollection: ' + Self.Name + ENDL +
    'Items: ' + IntToStr(Items.Count) + ENDL;

  Sep := '  ';
  for i := 0 to Items.Count - 1 do
  begin
    pci := Items[i];
    s := s + 'Item ' + IntToStr(i + 1) + ENDL;
    s := s + Sep + 'Index: ' + IntToStr(pci.Index) + ENDL;
    s := s + Sep + 'Name: ' + pci.Name + ENDL;
    s := s + Sep + 'Description: ' + pci.Description + ENDL;
    s := s + Sep + 'Enabled: ' + BoolToStrYN(pci.Enabled) + ENDL;
    s := s + Sep + 'Tag: ' + IntToStr(pci.Tag) + ENDL;
    b := pci.PngImage.Empty;
    s := s + Sep + 'Empy image: ' + BoolToStrYN(b) + ENDL;
    if b then Continue;
    s := s + Sep + 'Width: ' + IntToStrEx(pci.PngImage.Width) + ENDL;
    s := s + Sep + 'Height: ' + IntToStrEx(pci.PngImage.Height) + ENDL;
    {$IFDEF DCC}
    s := s + Sep + 'Bit depth: ' + IntToStr(pci.PngImage.Header.BitDepth) + ENDL;
    s := s + Sep + 'Compression level: ' + IntToStr(pci.PngImage.CompressionLevel) + ENDL;
    s := s + Sep + 'Compression method: ' + IntToStr(pci.PngImage.Header.CompressionMethod) + ENDL;
    {$ENDIF}
    s := s + Sep + 'Transparent color: ' + ColorToRgbIntStr(pci.PngImage.TransparentColor) + ENDL;
  end;

  Result := s;
end;

procedure TJppPngCollection.SetTagExt(const Value: TJppTagExt);
begin
  FTagExt := Value;
end;

{$endregion TJppPngCollection}


{$region ' ------------------------ TJppPngCollectionItems ------------------------------ '}
constructor TJppPngCollectionItems.Create(AOwner: TPersistent);
begin
  inherited Create(TJppPngCollectionItem);
  FOwner := AOwner;
end;

function TJppPngCollectionItems.Add: TJppPngCollectionItem;
begin
  Result := TJppPngCollectionItem(inherited Add);
end;

procedure TJppPngCollectionItems.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  Update(nil);
end;

function TJppPngCollectionItems.IsValidIndex(const Index: integer): Boolean;
begin
  Result := (Index >= 0) and (Index < Count);
end;

function TJppPngCollectionItems.GetItem(Index: Integer): TJppPngCollectionItem;
begin
  if IsValidIndex(Index) then Result := TJppPngCollectionItem(inherited Items[Index])
  else Result := nil;
end;

procedure TJppPngCollectionItems.SetItem(Index: Integer; const Value: TJppPngCollectionItem);
begin
  if IsValidIndex(Index) then inherited Items[Index] := Value;
end;

procedure TJppPngCollectionItems.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
end;

function TJppPngCollectionItems.Insert(Index: Integer): TJppPngCollectionItem;
begin
  Result := TJppPngCollectionItem(inherited Insert(Index));
end;

function TJppPngCollectionItems.GetOwner: TPersistent;
begin
  Result := FOwner;
end;
{$endregion TJppPngCollectionItems}


{$region ' ---------------------- TJppPngCollectionItem --------------------------- '}
constructor TJppPngCollectionItem.Create(ACollection: TCollection);
begin
  inherited Create(ACollection);
  FPngImage := TPngImage.Create;
  FName := 'Png_' + IntToStr(Index);
  FTag := 0;
  FEnabled := True;
end;

destructor TJppPngCollectionItem.Destroy;
begin
  FPngImage.Free;
  inherited Destroy;
end;

procedure TJppPngCollectionItem.Assign(Source: TPersistent);
begin
  if Source is TJppPngCollectionItem then
  begin
    FPngImage.Assign(TJppPngCollectionItem(Source).PngImage);
    FName := TJppPngCollectionItem(Source).Name;
    FTag := TJppPngCollectionItem(Source).Tag;
    FEnabled := TJppPngCollectionItem(Source).Enabled;
  end
  else
    inherited Assign(Source);
end;

procedure TJppPngCollectionItem.AssignTo(Dest: TPersistent);
begin
  inherited AssignTo(Dest);
  if (Dest is TJppPngCollectionItem) then TJppPngCollectionItem(Dest).PngImage := FPngImage;
end;

function TJppPngCollectionItem.GetDisplayName: string;
begin
  if Length(FName) = 0 then Result := inherited GetDisplayName
  else Result := FName;
end;

function TJppPngCollectionItem.GetHeight: integer;
begin
  if PngImage.Empty then Result := 0
  else Result := PngImage.Height;
end;

function TJppPngCollectionItem.GetWidth: integer;
begin
  if PngImage.Empty then Result := 0
  else Result := PngImage.Width;
end;

procedure TJppPngCollectionItem.SetPngImage(const Value: TPngImage);
begin
  if FPngImage = nil then FPngImage := TPngImage.Create;
  FPngImage.Assign(Value);
  FTag := 0;
  Changed(False);
end;


{$endregion TJppPngCollectionItem}




end.
