////////////////////////////////////////////////////////////////////////////////
/// MFL Parser library for Delphi                                            ///
/// ------------------------------------------------------------------------ ///
/// Written by: Dennis Göhlert                                               ///
/// Official repository: https://github.com/Manhunter07/MFL                  ///
///                                                                          ///
/// PROJECT DESCRIPTION:                                                     ///
/// MFL is a functional scripting language written in Delphi.                ///
/// It comes with a console expression parser, an editor and a FireMonkey    ///
/// expression evaluator with GUI, for both desktop and mobile platforms.    ///
/// The compiler itself runs on all platforms and does not use pointer       ///
/// types.                                                                   ///
///                                                                          ///
/// LICENSE DISCLAIMER:                                                      ///
/// This project is copyrighted with all rights reserved. It is freely       ///
/// available to the public, for both noncommercial and commercial use.      ///
/// You may edit and/or redistribute it as a whole.                          ///
/// This header must not be removed, moved or changed.                       ///
/// The terms of use may be changed by the project owner at any time and     ///
/// changes affect any commits dated at or after the time the updated terms  ///
/// have been released. Previews released are unaffected.                    ///
///                                                                          ///
/// Last updated: 2021-02-22                                                 ///
////////////////////////////////////////////////////////////////////////////////

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
    function GetEnumerator: TEnumerator<TParserObject>;
    procedure Clear;
    procedure Add(const AObject: TParserObject); overload;
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
  if AObject.Name.IsEmpty then
  begin
    // No anonymous types/functions
    raise EParserObjectNameError.CreateFmt('Invalid name: %s', [AObject.Name.QuotedString]);
  end;
  if Contains(AObject.Name) then
  begin
    if not (AObject.Writable and Self[AObject.Name].Writable) then
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

function TParserDictionary.GetEnumerator: TEnumerator<TParserObject>;
begin
  Result := FObjects.Values.GetEnumerator;
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
