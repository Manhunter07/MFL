unit Parser.Language;

interface

uses
  System.SysUtils, System.StrUtils, System.Math, System.Types, System.Generics.Collections, System.Generics.Defaults, System.Classes, System.TypInfo, System.Rtti,
  Parser.Exception, Parser.Syntax;

type
  TParserObject = class abstract(TPersistent)
  private
    FName: String;
  protected
    function GetMinArgCount: Integer; virtual; abstract;
    function GetMaxArgCount: Integer; virtual; abstract;
  public
    class function ValidName(const AName: String): Boolean; // Must NOT be virtual, due to generic aliasing
    property Name: String read FName;
    property MinArgCount: Integer read GetMinArgCount;
    property MaxArgCount: Integer read GetMaxArgCount;
    procedure AssignTo(Dest: TPersistent); override;
    constructor Create(const AName: String);
  end;

  TParserValueObject = class abstract(TParserObject)
  protected
    function GetValue(const AArgs: TArray<Double>): Double; virtual;
  public
    property Value[const AArgs: TArray<Double>]: Double read GetValue;
  end;

  IParserWritableObject = interface
    ['{EF23603E-7E4C-4F2F-841C-4901F622E773}']
    procedure SetValue(const AValue: Double);
    property Value: Double write SetValue;
  end;

  TParserConstant = class(TParserValueObject)
  private
    FValue: Double;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetValue(const AArgs: TArray<Double>): Double; override;
    function GetMinArgCount: Integer; override;
    function GetMaxArgCount: Integer; override;
  public
    constructor Create(const AName: String; const AValue: Double);
  end;

  TParserVariable = class(TParserConstant, IParserWritableObject)
  private
    FInterfaceImplementor: TSingletonImplementation;
  public
    property InterfaceImplementor: TSingletonImplementation read FInterfaceImplementor implements IParserWritableObject;
    constructor Create(const AName: String; const AValue: Double = Default(Double));
    destructor Destroy; override;
    procedure SetValue(const AValue: Double); virtual;
  end;

  TParserFunction = class abstract(TParserValueObject)
  private
    FParams: TArray<String>;
    FVarArg: Boolean;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetValue(const AArgs: TArray<Double>): Double; override;
    function GetMinArgCount: Integer; override;
    function GetMaxArgCount: Integer; override;
  public
    property Params: TArray<String> read FParams;
    property VarArg: Boolean read FVarArg;
    constructor Create(const AName: String; const AParams: TArray<String>; const AVarArg: Boolean = False);
    procedure AssertRange(const AParams: TArray<Double>; const AParamIndex: Integer; const ARangeStart: Double; const ARangeEnd: Double);
    procedure AssertMin(const AParams: TArray<Double>; const AParamIndex: Integer; const AValue: Double);
    procedure AssertMax(const AParams: TArray<Double>; const AParamIndex: Integer; const AValue: Double);
    procedure AssertInteger(const AParams: TArray<Double>; const AParamIndex: Integer);
  end;

  TParserStandardEventFunction = class;

  TParserFunctionEvent = function (const AFunction: TParserStandardEventFunction; const AArgs: TArray<Double>): Double of object;

  TParserStandardEventFunction = class(TParserFunction)
  private
    FOnInvoke: TParserFunctionEvent;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetValue(const AArgs: TArray<Double>): Double; override;
  public
    property OnInvoke: TParserFunctionEvent read FOnInvoke;
    constructor Create(const AName: String; const AParams: TArray<String>; const AOnInvoke: TParserFunctionEvent; const AVarArg: Boolean = False);
  end;

  TParserFunctionKind = (fkNone, fkStatic, fkConstructor);

  TParserStandardAddrReferenceFunction = class(TParserFunction)
  private
    FFunction: Pointer;
    FKind: TParserFunctionKind;
    FCallConv: TCallConv;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetValue(const AArgs: TArray<Double>): Double; override;
  public
    property &Function: Pointer read FFunction;
    property Kind: TParserFunctionKind read FKind default fkNone;
    property CallConv: TCallConv read FCallConv default ccReg;
    constructor Create(const AName: String; const AParams: TArray<String>; const AFunction: Pointer; const AKind: TParserFunctionKind = fkNone; const ACallConv: TCallConv = ccReg);
  end;

  TParserStandardAnonReferenceFunction = class(TParserFunction)
  private
    FFunction: TFunc<TArray<Double>, Double>;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetValue(const AArgs: TArray<Double>): Double; override;
  public
    property &Function: TFunc<TArray<Double>, Double> read FFunction;
    constructor Create(const AName: String; const AParams: TArray<String>; const AFunction: TFunc<TArray<Double>, Double>; const AVarArg: Boolean = False);
  end;

  TParserCustomFunction = class(TParserFunction, IParserValueSupplier)
  private
    FInterfaceImplementor: TSingletonImplementation;
    FSyntaxTree: TParserTree;
    FParamValues: TDictionary<String, Double>;
    function GetValues(const AName: String; const AArgs: TArray<Double>): Double;
    function GetMinArgCount(const AName: String): Integer; reintroduce; overload;
    function GetMaxArgCount(const AName: String): Integer; reintroduce; overload;
  protected
    function ContainsValue(const AName: String): Boolean;
    property Values[const AName: String; const AArgs: TArray<Double>]: Double read GetValues;
    procedure AssignTo(Dest: TPersistent); override;
    function GetValue(const AArgs: TArray<Double>): Double; override;
    property MinArgCount[const AName: String]: Integer read GetMinArgCount;
    property MaxArgCount[const AName: String]: Integer read GetMaxArgCount;
  public
    property InterfaceImplementor: TSingletonImplementation read FInterfaceImplementor implements IParserValueSupplier;
    property SyntaxTree: TParserTree read FSyntaxTree write FSyntaxTree;
    constructor Create(const AName: String; const AParams: TArray<String>; const ASyntaxTree: TParserTree = Default(TParserTree));
    destructor Destroy; override;
  end;

implementation

{ TParserObject }

procedure TParserObject.AssignTo(Dest: TPersistent);
begin
  if Dest is TParserObject then
  begin
    (Dest as TParserObject).FName := Name;
  end else
  begin
    inherited;
  end;
end;

constructor TParserObject.Create(const AName: String);
begin
  inherited Create;
  if not ValidName(AName) then
  begin
    raise EParserObjectNameError.CreateFmt('Invalid name: %s', [AName.QuotedString]);
  end;
  FName := AName;
end;

class function TParserObject.ValidName(const AName: String): Boolean;
begin
  Result := IsValidIdent(AName) and (TParserKeyword.Create(AName) = kwNone);
end;

{ TParserValueObject }

function TParserValueObject.GetValue(const AArgs: TArray<Double>): Double;
begin
  if Length(AArgs) < MinArgCount then
  begin
    raise EParserObjectArgCountError.Create('Not enough parameters');
  end;
  if (MaxArgCount <> -1) and (Length(AArgs) > MaxArgCount) then
  begin                             
    if MaxArgCount = 0 then
    begin
      raise EParserObjectArgCountError.Create('No parameters expected');
    end;
    raise EParserObjectArgCountError.Create('Too many parameters');   
  end;
  Result := Default(Double);
end;

{ TParserConstant }

procedure TParserConstant.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TParserConstant then
  begin
    (Dest as TParserConstant).FValue := FValue;
  end;
end;

constructor TParserConstant.Create(const AName: String; const AValue: Double);
begin
  inherited Create(AName);
  FValue := AValue;
end;

function TParserConstant.GetMaxArgCount: Integer;
begin
  Result := 0;
end;

function TParserConstant.GetMinArgCount: Integer;
begin
  Result := 0;
end;

function TParserConstant.GetValue(const AArgs: TArray<Double>): Double;
begin
  inherited;
  Result := FValue;
end;

{ TParserVariable }

constructor TParserVariable.Create(const AName: String; const AValue: Double);
begin
  inherited Create(AName, AValue);
  FInterfaceImplementor := TSingletonImplementation.Create;
end;

destructor TParserVariable.Destroy;
begin
  InterfaceImplementor.Free;
  inherited;
end;

procedure TParserVariable.SetValue(const AValue: Double);
begin
  if not SameValue(AValue, FValue) then
  begin
    FValue := AValue;
  end;
end;

{ TParserFunction }

procedure TParserFunction.AssertInteger(const AParams: TArray<Double>; const AParamIndex: Integer);
begin
  if not IsZero(Frac(AParams[AParamIndex]))then
  begin
    raise EParserFunctionParamValueError.CreateFmt('Parameter %s must be an integer', [Params[AParamIndex].QuotedString]);
  end;
end;

procedure TParserFunction.AssertMax(const AParams: TArray<Double>; const AParamIndex: Integer; const AValue: Double);
begin
  if CompareValue(AParams[AParamIndex], AValue) = GreaterThanValue then
  begin
    raise EParserFunctionParamValueError.CreateFmt('Parameter %s must be %g or less', [Params[AParamIndex].QuotedString, AValue]);
  end;
end;

procedure TParserFunction.AssertMin(const AParams: TArray<Double>; const AParamIndex: Integer; const AValue: Double);
begin
  if CompareValue(AParams[AParamIndex], AValue) = LessThanValue then
  begin
    raise EParserFunctionParamValueError.CreateFmt('Parameter %s must be %g or greater', [Params[AParamIndex].QuotedString, AValue]);
  end;
end;

procedure TParserFunction.AssertRange(const AParams: TArray<Double>; const AParamIndex: Integer; const ARangeStart, ARangeEnd: Double);
begin
  if not InRange(AParams[AParamIndex], ARangeStart, ARangeEnd) then
  begin
    raise EParserFunctionParamValueError.CreateFmt('Parameter %s must be between %g and %g', [Params[AParamIndex].QuotedString, ARangeStart, ARangeEnd]);
  end;
end;

procedure TParserFunction.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TParserFunction then
  begin
    (Dest as TParserFunction).FParams := Params;
  end;
end;

constructor TParserFunction.Create(const AName: String; const AParams: TArray<String>; const AVarArg: Boolean = False);
var
  LParam: String;
  LParams: TStringList;
begin
  inherited Create(AName);
  LParams := TStringList.Create(dupError, False, False); // dupError has no effect here since Sorted is False
  try
    for LParam in AParams do
    begin
      if not IsValidIdent(LParam) then
      begin
        raise EParserFunctionParamNameError.CreateFmt('Invalid function parameter name: %s', [LParam.QuotedString]);
      end;
      if LParams.IndexOf(LParam) <> -1 then
      begin
        raise EParserFunctionParamDuplicateError.CreateFmt('Redeclared identifier: %s', [LParam.QuotedString]);
      end;
      LParams.Add(LParam);
    end;
    FParams := LParams.ToStringArray;
    FVarArg := AVarArg;
  finally
    LParams.Free;
  end;
end;

function TParserFunction.GetMaxArgCount: Integer;
begin
  if VarArg then
  begin
    Result := -1;
  end else
  begin
    Result := Length(FParams);
  end;
end;

function TParserFunction.GetMinArgCount: Integer;
begin
  Result := Length(FParams);
end;

function TParserFunction.GetValue(const AArgs: TArray<Double>): Double;
begin
  inherited;
  Result := Default(Double);
end;

{ TParserStandardEventFunction }

procedure TParserStandardEventFunction.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TParserStandardEventFunction then
  begin
    (Dest as TParserStandardEventFunction).FOnInvoke := OnInvoke;
  end;
end;

constructor TParserStandardEventFunction.Create(const AName: String; const AParams: TArray<String>; const AOnInvoke: TParserFunctionEvent; const AVarArg: Boolean = False);
begin
  inherited Create(AName, AParams, AVarArg);
  FOnInvoke := AOnInvoke;
end;

function TParserStandardEventFunction.GetValue(const AArgs: TArray<Double>): Double;
begin
  inherited;
  Result := OnInvoke(Self, AArgs);
end;

{ TParserStandardAddrReferenceFunction }

procedure TParserStandardAddrReferenceFunction.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TParserStandardAddrReferenceFunction then
  begin
    (Dest as TParserStandardAddrReferenceFunction).FFunction := &Function;
  end;
end;

constructor TParserStandardAddrReferenceFunction.Create(const AName: String; const AParams: TArray<String>; const AFunction: Pointer; const AKind: TParserFunctionKind = fkNone; const ACallConv: TCallConv = ccReg);
begin
  inherited Create(AName, AParams);
  FFunction := AFunction;
  FKind := AKind;
  FCallConv := ACallConv;
end;

function TParserStandardAddrReferenceFunction.GetValue(const AArgs: TArray<Double>): Double;
var
  LArgValues: TArray<TValue>;
  LIndex: Integer;
begin
  inherited;
  SetLength(LArgValues, Length(AArgs));
  for LIndex := Low(AArgs) to High(AArgs) do
  begin
    LArgValues[LIndex] := AArgs[LIndex];
  end;
  Result := Invoke(FFunction, LArgValues, CallConv, TypeInfo(Double), Kind = fkStatic, Kind = fkConstructor).AsType<Double>;
end;

{ TParserStandardAnonReferenceFunction }

procedure TParserStandardAnonReferenceFunction.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TParserStandardAnonReferenceFunction then
  begin
    (Dest as TParserStandardAnonReferenceFunction).FFunction := &Function;
  end;
end;

constructor TParserStandardAnonReferenceFunction.Create(const AName: String; const AParams: TArray<String>; const AFunction: TFunc<TArray<Double>, Double>; const AVarArg: Boolean = False);
begin
  inherited Create(AName, AParams, AVarArg);
  FFunction := AFunction;
end;

function TParserStandardAnonReferenceFunction.GetValue(const AArgs: TArray<Double>): Double;
begin
  inherited;
  Result := FFunction(AArgs);
end;

{ TParserCustomFunction }

procedure TParserCustomFunction.AssignTo(Dest: TPersistent);
begin
  inherited;
  if Dest is TParserCustomFunction then
  begin
    (Dest as TParserCustomFunction).FSyntaxTree := SyntaxTree;
  end;
end;

function TParserCustomFunction.ContainsValue(const AName: String): Boolean;
begin
  Result := FParamValues.ContainsKey(AName);
end;

constructor TParserCustomFunction.Create(const AName: String; const AParams: TArray<String>; const ASyntaxTree: TParserTree);
var
  LParam: String;
begin
  inherited Create(AName, AParams);
  FInterfaceImplementor := TSingletonImplementation.Create;
  FSyntaxTree := ASyntaxTree;
  FParamValues := TDictionary<String, Double>.Create(Length(AParams), TIStringComparer.Ordinal);
  for LParam in AParams do
  begin
    FParamValues.Add(LParam, Default(Double));
  end;
end;

destructor TParserCustomFunction.Destroy;
begin
  SyntaxTree.Free;
  InterfaceImplementor.Free;
  inherited;
end;

function TParserCustomFunction.GetMaxArgCount(const AName: String): Integer;
begin
  Result := 0;
end;

function TParserCustomFunction.GetMinArgCount(const AName: String): Integer;
begin
  Result := 0;
end;

function TParserCustomFunction.GetValue(const AArgs: TArray<Double>): Double;
begin
  inherited;
  Result := SyntaxTree.Calculate;
end;

function TParserCustomFunction.GetValues(const AName: String; const AArgs: TArray<Double>): Double;
begin
  if FParamValues.TryGetValue(AName, Result) then
  begin
    if Length(AArgs) <> 0 then
    begin
      raise EParserObjectArgCountError.Create('No parameters expected');
    end;
  end;
end;

end.
