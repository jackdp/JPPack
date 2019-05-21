unit JPP.PngCollection;

interface

uses
  Winapi.Messages, Winapi.Windows,
  System.SysUtils, System.Classes, System.Types, System.UITypes,
  Vcl.Graphics,
  Vcl.Imaging.PngImage;
  //JPP.Types, JPP.Common;


type

  TJppPngCollectionItem = class;
  TJppPngCollectionItems = class;

  {$region ' --- TJppPngCollection --- '}
  TJppPngCollection = class(TComponent)
  private
    FItems: TJppPngCollectionItems;
    procedure EnableOrDisableAll(const Enable: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    procedure EnableAll;
    procedure DisableAll;
    function PngIndex(const PngName: string; IgnoreCase: Boolean = False): integer;
    function Count: integer;
    function IsValidIndex(const Index: integer): Boolean;
  published
    property Items: TJppPngCollectionItems read FItems write FItems;
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
    constructor Create(Collection: TCollection); overload; override;
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
end;

destructor TJppPngCollection.Destroy;
begin
  FItems.Free;
  inherited Destroy;
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
constructor TJppPngCollectionItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FPngImage := TPngImage.Create;
  FName := 'Png_' + Index.toString;
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
