unit Parser.Package;

interface

uses
  System.SysUtils, System.Types, System.Math, System.Generics.Defaults, System.Generics.Collections,
  Parser.Dictionary, Parser.Exception, Parser.Language, Parser.Syntax;

type
  TParserPackage = class(TSingletonImplementation{, IParserValueSupplier})
  private class var
    FDefaultPackage: TParserPackage;
  private
    FName: String;
    FExplicit: Boolean;
    FDictionary: TParserDictionary;
  public
    class property DefaultPackage: TParserPackage read FDefaultPackage;
    property Name: String read FName;
    property Explicit: Boolean read FExplicit default False;
    property Dictionary: TParserDictionary read FDictionary;
    class constructor Create;
    class destructor Destroy;
    constructor Create(const AName: String; const AExplicit: Boolean = False);
    destructor Destroy; override;
  end;

  TParserMemoryPackage = class(TParserPackage)
  public const
    Offset = 1;
  private
    FMemory: TDictionary<Integer, TArray<Double>>;
    function GetSize: Integer;
    function GetMemory(const AAddress: Integer): TArray<Double>;
    procedure SetMemory(const AAddress: Integer; const AValue: TArray<Double>);
  protected
    function NextAddress: Integer;
    procedure ValidateAddress(const AAddress: Integer);
    procedure ValidateAddressAndIndex(const AAddress, AIndex: Integer);
  public
    property Memory[const AAddress: Integer]: TArray<Double> read GetMemory write SetMemory;
    property Size: Integer read GetSize;
    constructor Create(const AName: String; const AExplicit: Boolean = False; const ASize: Integer = 0);
    destructor Destroy; override;
  end;

  TParserStandardPackages = class
  private
    class function GetConvert: TParserPackage; static;
    class function GetRandom: TParserPackage; static;
    class function GetDateTime: TParserPackage; static;
  public
    class property Convert: TParserPackage read GetConvert;
    class property Random: TParserPackage read GetRandom;
    class property DateTime: TParserPackage read GetDateTime;
  end;

implementation

{ TParserPackage }

constructor TParserPackage.Create(const AName: String; const AExplicit: Boolean = False);
begin
  inherited Create;
  if not IsValidIdent(AName) then
  begin
    raise EParserPackageNameError.CreateFmt('Invalid package name: %s', [AName.QuotedString]);
  end;
  FName := AName;
  FExplicit := AExplicit;
  FDictionary := TParserDictionary.Create;
end;

class constructor TParserPackage.Create;
begin
  FDefaultPackage := TParserPackage.Create('System');
  FDefaultPackage.Dictionary.Add(TParserConstant.Create('Version', 0.1));
  FDefaultPackage.Dictionary.Add(TParserConstant.Create('Pi', Pi));
  FDefaultPackage.Dictionary.Add(TParserConstant.Create('E', Exp(1)));
  FDefaultPackage.Dictionary.Add(TParserConstant.Create('Inf', Double.PositiveInfinity));
  FDefaultPackage.Dictionary.Add(TParserConstant.Create('NaN', Double.NaN));
  FDefaultPackage.Dictionary.Add(TParserConstant.Create('MinValue', Double.MinValue));
  FDefaultPackage.Dictionary.Add(TParserConstant.Create('MaxValue', Double.MaxValue));
  FDefaultPackage.Dictionary.Add(TParserStandardAnonReferenceFunction.Create('Sin', ['X'],
    function (Params: TArray<Double>): Double
    begin
      Result := Sin(Params[0]);
    end));
  FDefaultPackage.Dictionary.Add(TParserStandardAnonReferenceFunction.Create('Cos', ['X'],
    function (Params: TArray<Double>): Double
    begin
      Result := Cos(Params[0]);
    end));
  FDefaultPackage.Dictionary.Add(TParserStandardAnonReferenceFunction.Create('Tan', ['X'],
    function (Params: TArray<Double>): Double
    begin
      Result := Tan(Params[0]);
    end));
  FDefaultPackage.Dictionary.Add(TParserStandardAnonReferenceFunction.Create('Sign', ['X'],
    function (Params: TArray<Double>): Double
    begin
      Result := Sign(Params[0]);
    end));
  FDefaultPackage.Dictionary.Add(TParserStandardAnonReferenceFunction.Create('Log', ['Base', 'X'],
    function (Params: TArray<Double>): Double
    begin
      Result := LogN(Params[0], Params[1]);
    end));
  FDefaultPackage.Dictionary.Add(TParserStandardAnonReferenceFunction.Create('LN', ['X'],
    function (Params: TArray<Double>): Double
    begin
      Result := Ln(Params[0]);
    end));
  FDefaultPackage.Dictionary.Add(TParserStandardAnonReferenceFunction.Create('Abs', ['X'],
    function (Params: TArray<Double>): Double
    begin
      Result := Abs(Params[0]);
    end));
  FDefaultPackage.Dictionary.Add(TParserStandardAnonReferenceFunction.Create('Round', ['X', 'Digit'],
    function (Params: TArray<Double>): Double
    var
      LThisFunction: TParserStandardAnonReferenceFunction;
    begin
      LThisFunction := (FDefaultPackage.Dictionary['Round'] as TParserStandardAnonReferenceFunction);
      LThisFunction.AssertInteger(Params, 1);
      LThisFunction.AssertRange(Params, 1, Low(TRoundToEXRangeExtended), High(TRoundToEXRangeExtended));
      Result := RoundTo(Params[0], Trunc(Params[1]));
    end));
  FDefaultPackage.Dictionary.Add(TParserStandardAnonReferenceFunction.Create('Ceil', ['X'],
    function (Params: TArray<Double>): Double
    begin
      Result := Ceil(Params[0]);
    end));
  FDefaultPackage.Dictionary.Add(TParserStandardAnonReferenceFunction.Create('Floor', ['X'],
    function (Params: TArray<Double>): Double
    begin
      Result := Floor(Params[0]);
    end));
  FDefaultPackage.Dictionary.Add(TParserStandardAnonReferenceFunction.Create('Trunc', ['X'],
    function (Params: TArray<Double>): Double
    begin
      Result := Trunc(Params[0]);
    end));
  FDefaultPackage.Dictionary.Add(TParserStandardAnonReferenceFunction.Create('Frac', ['X'],
    function (Params: TArray<Double>): Double
    begin
      Result := Frac(Params[0]);
    end));
  FDefaultPackage.Dictionary.Add(TParserStandardAnonReferenceFunction.Create('Min', ['First', 'Second'],
    function (Params: TArray<Double>): Double
    begin
      Result := MinValue(Params);
    end, True));
  FDefaultPackage.Dictionary.Add(TParserStandardAnonReferenceFunction.Create('Max', ['First', 'Second'],
    function (Params: TArray<Double>): Double
    begin
      Result := MaxValue(Params);
    end, True));
  FDefaultPackage.Dictionary.Add(TParserStandardAnonReferenceFunction.Create('Avg', ['First', 'Second'],
    function (Params: TArray<Double>): Double
    begin
      Result := Mean(Params);
    end, True));
  FDefaultPackage.Dictionary.Add(TParserStandardAnonReferenceFunction.Create('Compare', ['First', 'Second'],
    function (Params: TArray<Double>): Double
    begin
      Result := CompareValue(Params[0], Params[1]);
    end));
  FDefaultPackage.Dictionary.Add(TParserStandardAnonReferenceFunction.Create('Mask', ['Output', 'Dummy'],
    function (Params: TArray<Double>): Double
    begin
      Result := Params[0];
    end, True));
  FDefaultPackage.Dictionary.Add(TParserStandardAnonReferenceFunction.Create('Error', ['Code'],
    function (Params: TArray<Double>): Double
    var
      LThisFunction: TParserStandardAnonReferenceFunction;
    begin
      LThisFunction := (FDefaultPackage.Dictionary['Error'] as TParserStandardAnonReferenceFunction);
      LThisFunction.AssertInteger(Params, 0);
      LThisFunction.AssertMin(Params, 0, 0);
      if not IsZero(Params[0]) then
      begin
        raise EParserUserError.CreateFmt('Error code: %d', [Trunc(Params[0])]);
      end;
      Result := Default(Double);
    end));
end;

destructor TParserPackage.Destroy;
begin
  Dictionary.Free;
  inherited;
end;

class destructor TParserPackage.Destroy;
begin
  DefaultPackage.Free;
end;

{ TParserMemoryPackage }

constructor TParserMemoryPackage.Create(const AName: String; const AExplicit: Boolean = False; const ASize: Integer = 0);
begin
  inherited Create(AName, AExplicit);
  FMemory := TDictionary<Integer, TArray<Double>>.Create(ASize);
  Dictionary.Add(TParserConstant.Create('Nil', 0));
  Dictionary.Add(TParserStandardAnonReferenceFunction.Create('Clear', [],
    function (Params: TArray<Double>): Double
    begin
      Result := Size;
      FMemory.Clear;
    end));
  Dictionary.Add(TParserStandardAnonReferenceFunction.Create('Alloc', ['Size'],
    function (Params: TArray<Double>): Double
    var
      LThisFunction: TParserStandardAnonReferenceFunction;
      LValues: TArray<Double>;
    begin
      LThisFunction := (Dictionary['Alloc'] as TParserStandardAnonReferenceFunction);
      LThisFunction.AssertInteger(Params, 0);
      LThisFunction.AssertMin(Params, 0, 1);
      SetLength(LValues, Trunc(Params[0]));
      Result := NextAddress;
      FMemory.Add(Trunc(Result), LValues);
    end));
  Dictionary.Add(TParserStandardAnonReferenceFunction.Create('Realloc', ['Address', 'Size'],
    function (Params: TArray<Double>): Double
    var
      LThisFunction: TParserStandardAnonReferenceFunction;
      LValues: TArray<Double>;
    begin
      LThisFunction := (Dictionary['Realloc'] as TParserStandardAnonReferenceFunction);
      LThisFunction.AssertInteger(Params, 0);
      LThisFunction.AssertInteger(Params, 1);
      LThisFunction.AssertMin(Params, 1, 1);
      if CompareValue(Params[0], Offset) = LessThanValue then
      begin
        raise EParserMemoryPackageAddressError.Create('Invalid memory address');
      end;
      ValidateAddress(Trunc(Params[0]));
      SetLength(LValues, Trunc(Params[1]));
      Result := 0;
      FMemory.AddOrSetValue(Trunc(Params[0]), LValues);
    end));
  Dictionary.Add(TParserStandardAnonReferenceFunction.Create('Free', ['Address'],
    function (Params: TArray<Double>): Double
    var
      LThisFunction: TParserStandardAnonReferenceFunction;
    begin
      LThisFunction := (Dictionary['Free'] as TParserStandardAnonReferenceFunction);
      LThisFunction.AssertInteger(Params, 0);
      ValidateAddress(Trunc(Params[0]));
      Result := Length(Memory[Trunc(Params[0])]);
      FMemory.Remove(Trunc(Params[0]));
    end));
  Dictionary.Add(TParserStandardAnonReferenceFunction.Create('Read', ['Address', 'Index'],
    function (Params: TArray<Double>): Double
    var
      LThisFunction: TParserStandardAnonReferenceFunction;
    begin
      LThisFunction := (Dictionary['Read'] as TParserStandardAnonReferenceFunction);
      LThisFunction.AssertInteger(Params, 0);
      LThisFunction.AssertInteger(Params, 1);
      ValidateAddressAndIndex(Trunc(Params[0]), Trunc(Params[1]));
      Result := Memory[Trunc(Params[0])][Trunc(Params[1])];
    end));
  Dictionary.Add(TParserStandardAnonReferenceFunction.Create('Write', ['Address', 'Index', 'Value'],
    function (Params: TArray<Double>): Double
    var
      LThisFunction: TParserStandardAnonReferenceFunction;
    begin
      LThisFunction := (Dictionary['Write'] as TParserStandardAnonReferenceFunction);
      LThisFunction.AssertInteger(Params, 0);
      LThisFunction.AssertInteger(Params, 1);
      LThisFunction.AssertMin(Params, 1, 0);
      ValidateAddressAndIndex(Trunc(Params[0]), Trunc(Params[1]));
      Memory[Trunc(Params[0])][Trunc(Params[1])] := Params[2];
      Result := Default(Double);
    end));
  Dictionary.Add(TParserStandardAnonReferenceFunction.Create('GetSize', ['Address'],
    function (Params: TArray<Double>): Double
    var
      LThisFunction: TParserStandardAnonReferenceFunction;
    begin
      LThisFunction := (Dictionary['GetSize'] as TParserStandardAnonReferenceFunction);
      LThisFunction.AssertInteger(Params, 0);
      ValidateAddress(Trunc(Params[0]));
      Result := Length(Memory[Trunc(Params[0])]);
    end));
  Dictionary.Add(TParserStandardAnonReferenceFunction.Create('SetSize', ['Address', 'Size'],
    function (Params: TArray<Double>): Double
    var
      LThisFunction: TParserStandardAnonReferenceFunction;
      LValues: TArray<Double>;
    begin
      LThisFunction := (Dictionary['SetSize'] as TParserStandardAnonReferenceFunction);
      LThisFunction.AssertInteger(Params, 0);
      LThisFunction.AssertInteger(Params, 1);
      LThisFunction.AssertMin(Params, 1, 1);
      ValidateAddress(Trunc(Params[0]));
      LValues := Memory[Trunc(Params[0])];
      SetLength(LValues, Trunc(Params[1]));
      Memory[Trunc(Params[0])] := LValues;
      Result := Default(Double);
    end));
  Dictionary.AddAlias('Null', 'Nil');
end;

destructor TParserMemoryPackage.Destroy;
begin
  FMemory.Free;
  inherited;
end;

function TParserMemoryPackage.GetMemory(const AAddress: Integer): TArray<Double>;
begin
  ValidateAddress(AAddress);
  Result := FMemory[AAddress];
end;

function TParserMemoryPackage.GetSize: Integer;
begin
  Result := FMemory.Count;
end;

function TParserMemoryPackage.NextAddress: Integer;
begin
  Result := Offset;
  while FMemory.ContainsKey(Result) do
  begin
    Inc(Result);
  end;
end;

procedure TParserMemoryPackage.SetMemory(const AAddress: Integer; const AValue: TArray<Double>);
begin
  ValidateAddress(AAddress);
  FMemory[AAddress] := Copy(AValue);
end;

procedure TParserMemoryPackage.ValidateAddress(const AAddress: Integer);
begin
  if not FMemory.ContainsKey(AAddress) then
  begin
    raise EParserMemoryPackageAddressError.CreateFmt('Memory at address %d not allocated', [AAddress]);
  end;
end;

procedure TParserMemoryPackage.ValidateAddressAndIndex(const AAddress, AIndex: Integer);
begin
  ValidateAddress(AAddress);
  if not InRange(AIndex, Low(FMemory[AAddress]), High(FMemory[AAddress])) then
  begin
    raise EParserMemoryPackageIndexError.CreateFmt('Index %d at address %d out of bounds', [AIndex, AAddress]);
  end;
end;

{ TParserStandardPackages }

class function TParserStandardPackages.GetConvert: TParserPackage;
begin

end;

class function TParserStandardPackages.GetDateTime: TParserPackage;
begin

end;

class function TParserStandardPackages.GetRandom: TParserPackage;
begin

end;

end.
