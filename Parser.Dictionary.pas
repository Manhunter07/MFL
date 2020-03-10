unit Parser.Dictionary;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Collections, System.Generics.Defaults,
  Parser.Exception, Parser.Language;

type
  TParserDictionary = class(TPersistent)
  private
    FObjects: TObjectDictionary<String, TParserObject>;
    FAliases: TDictionary<String, String>;
    function GetObjects(const AName: String): TParserObject;
    function GetCount: Integer;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    property Objects[const AName: String]: TParserObject read GetObjects; default;
    property Count: Integer read GetCount;
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure Add(const AObject: TParserObject);
    procedure AddAlias(const AAlias, AName: String);
    function Dealias(var AName: String): Boolean;
    procedure Remove(const AName: String);
    function Contains(const AName: String): Boolean;
    procedure Prettify(var AName: String);
  end;

implementation

{ TParserDictionary }

procedure TParserDictionary.Add(const AObject: TParserObject);
begin
  if Contains(AObject.Name) then
  begin
    if not (Supports(AObject, IParserWritableObject) and Supports(Self[AObject.Name], IParserWritableObject)) then
    begin
      raise EParserDictionaryDuplicateError.CreateFmt('Redeclared identifier: %s', [AObject.Name.QuotedString]);
    end;
    FObjects.Remove(AObject.Name);
  end;
  FObjects.Add(AObject.Name, AObject);
end;

procedure TParserDictionary.AddAlias(const AAlias, AName: String);
begin
  if not TParserObject.ValidName(AAlias) then
  begin
    raise EParserObjectNameError.CreateFmt('Invalid name: %s', [AAlias.QuotedString]);
  end;
  if Contains(AAlias) then
  begin
    raise EParserDictionaryDuplicateError.CreateFmt('Redeclared identifier: %s', [AAlias.QuotedString]);
  end;
  if SameText(AAlias, AName) then
  begin
    raise EParserDictionaryAliasError.CreateFmt('Identical alias name and meaning: %s', [AAlias.QuotedString]);
  end;
  FAliases.Add(AAlias, AName);
end;

procedure TParserDictionary.AssignTo(Dest: TPersistent);
var
  LObject: TParserObject;
  LAlias: TPair<String, String>;
begin
  if Dest is TParserDictionary then
  begin
    (Dest as TParserDictionary).Clear;
    for LObject in FObjects.Values do
    begin
      (Dest as TParserDictionary).Add(LObject);
    end;
    for LAlias in FAliases do
    begin
      (Dest as TParserDictionary).AddAlias(LAlias.Key, LAlias.Value);
    end;
  end else
  begin
    inherited;
  end;
end;

procedure TParserDictionary.Clear;
begin
  FObjects.Clear;
end;

function TParserDictionary.Contains(const AName: String): Boolean;
begin
  Result := FObjects.ContainsKey(AName) or FAliases.ContainsKey(AName);
end;

constructor TParserDictionary.Create;
begin
  inherited;
  FObjects := TObjectDictionary<String, TParserObject>.Create([doOwnsValues], TIStringComparer.Ordinal);
  FAliases := TDictionary<String, String>.Create(TIStringComparer.Ordinal);
end;

function TParserDictionary.Dealias(var AName: String): Boolean;
begin
  Result := FAliases.ContainsKey(AName);
  if Result then
  begin
    AName := FAliases[AName];
  end;
end;

destructor TParserDictionary.Destroy;
begin
  FAliases.Free;
  FObjects.Free;
  inherited;
end;

function TParserDictionary.GetCount: Integer;
begin
  Result := FObjects.Count;
end;

function TParserDictionary.GetObjects(const AName: String): TParserObject;
begin
  if not FObjects.TryGetValue(AName, Result) then
  begin
    raise EParserDictionaryUnknownError.CreateFmt('Undeclared identifier: %s', [AName.QuotedString]);
  end;
end;

procedure TParserDictionary.Prettify(var AName: String);
var
  LObject: TParserObject;
begin
  if not FObjects.TryGetValue(AName, LObject) then
  begin
    raise EParserDictionaryUnknownError.CreateFmt('Undeclared identifier: %s', [AName.QuotedString]);
  end;
  AName := LObject.Name;
end;

procedure TParserDictionary.Remove(const AName: String);
begin
  if not Contains(AName) then
  begin
    raise EParserDictionaryUnknownError.CreateFmt('Undeclared identifier: %s', [AName.QuotedString]);
  end;
  FObjects.Remove(AName);
end;

end.
