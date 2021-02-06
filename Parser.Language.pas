unit Parser.Language;

interface

uses
  System.SysUtils, System.StrUtils, System.Math, System.Types, System.Generics.Collections, System.Generics.Defaults, System.Classes, System.TypInfo, System.Rtti,
  Parser.Exception, Parser.Syntax, Parser.Value;

type
  TParserAttrib = class;

  TParserObject = class abstract(TSingletonImplementation)
  private
    FName: String;
    function GetName: String; // Only to support IParserValueRefTarget
  protected
    function GetValue(const AArgs: TArray<TParserValue>): TParserValue; virtual;
    function GetMinArgCount: Integer; virtual; abstract;
    function GetMaxArgCount: Integer; virtual; abstract;
    function GetArgTypes(const AIndex: Integer): IParserValueConstraint; virtual; abstract;
  public
    class function ValidName(const AName: String; const AAllowEmpty: Boolean = False): Boolean; // Must NOT be virtual, due to generic aliasing
    property Name: String read GetName;
    property MinArgCount: Integer read GetMinArgCount;
    property MaxArgCount: Integer read GetMaxArgCount;
    property ArgTypes[const AIndex: Integer]: IParserValueConstraint read GetArgTypes;
    property Value[const AArgs: TArray<TParserValue>]: TParserValue read GetValue;
//    property Attribs[const AIndex: Integer]: TParserAttrib read GetAttribs;
//    property AttribCount: Integer read GetAttribCount;
    constructor Create(const AName: String);
  end;

  TParserObjectHelper = class helper for TParserObject
  private
    function GetWritable: Boolean;
    function GetAddressable: Boolean;
  public
    property Writable: Boolean read GetWritable;
    property Addressable: Boolean read GetAddressable;
  end;

  IParserWritableObject = interface
    ['{EF23603E-7E4C-4F2F-841C-4901F622E773}']
    procedure SetValue(const AValue: TParserValue);
    property Value: TParserValue write SetValue;
  end;

  TParserConstant = class(TParserObject)
  private
    FValue: TParserValue;
  protected
    function GetValue(const AArgs: TArray<TParserValue>): TParserValue; override;
    function GetMinArgCount: Integer; override;
    function GetMaxArgCount: Integer; override;
    function GetArgTypes(const AIndex: Integer): IParserValueConstraint; override;
  public
    constructor Create(const AName: String; const AValue: TParserValue);
  end;

  TParserVariable = class(TParserConstant, IParserWritableObject, IParserValueRefTarget)
  public
    procedure SetValue(const AValue: TParserValue); virtual;
  end;

  TParserParamOption = (poType, poDefault);

  TParserParamOptions = set of TParserParamOption;

  TParserParam = record
  private
    FName: String;
    FType: IParserValueConstraint;
    FDefault: TParserValue;
    FOptions: TParserParamOptions;
  public
    property Name: String read FName;
    property &Type: IParserValueConstraint read FType;
    property &Default: TParserValue read FDefault;
    property Options: TParserParamOptions read FOptions;
    constructor Create(const AName: String; const AType: IParserValueConstraint = nil); overload;
    constructor Create(const AName: String; const ADefault: TParserValue; const AType: IParserValueConstraint = nil); overload;
  end;

  TParserInline = class
  private
    FSyntaxTree: TParserTree;
  public
    property SyntaxTree: TParserTree read FSyntaxTree write FSyntaxTree;
    constructor Create(const AName: String; const ASyntaxTree: TParserTree = Default(TParserTree));
  end;

  TParserFunction = class abstract(TParserObject, IParserValueRefTarget)
  private
    FParams: TDictionary<String, TParserParam>;
    FOrderedParamNames: TStringList;
    FVarArg: Boolean;
    FMinArgCount: Integer;
    FArgs: TObjectStack<TList<TParserValue>>;
    function GetParams(const AName: String): TParserParam;
    function GetParamCount: Integer;
    function GetParamNames(const AIndex: Integer): String;
  protected
    function GetValue(const AArgs: TArray<TParserValue>): TParserValue; override;
    function GetMinArgCount: Integer; override;
    function GetMaxArgCount: Integer; override;
    function GetArgTypes(const AIndex: Integer): IParserValueConstraint; override;
    function ParamIndex(const AArgIndex: Integer): Integer;
    procedure PrepareArgs; virtual;
    procedure RestoreArgs;
  public
    property Params[const AName: String]: TParserParam read GetParams;
    property ParamCount: Integer read GetParamCount;
    property ParamNames[const AIndex: Integer]: String read GetParamNames;
    property VarArg: Boolean read FVarArg;
    constructor Create(const AName: String; const AParams: TArray<TParserParam>; const AVarArg: Boolean = False);
    destructor Destroy; override;
  end;

  TParserEventFunction = class;

  TParserFunctionEvent = function (const AFunction: TParserEventFunction; const AArgs: TArray<TParserValue>): TParserValue of object;

  TParserEventFunction = class(TParserFunction)
  private
    FOnInvoke: TParserFunctionEvent;
  protected
    function GetValue(const AArgs: TArray<TParserValue>): TParserValue; override;
  public
    property OnInvoke: TParserFunctionEvent read FOnInvoke;
    constructor Create(const AName: String; const AParams: TArray<TParserParam>; const AOnInvoke: TParserFunctionEvent; const AVarArg: Boolean = False);
  end;

  TParserReferenceFunction = class(TParserFunction)
  private
    FFunction: TFunc<TArray<TParserValue>, TParserValue>;
  protected
    function GetValue(const AArgs: TArray<TParserValue>): TParserValue; override;
  public
    property &Function: TFunc<TArray<TParserValue>, TParserValue> read FFunction;
    constructor Create(const AName: String; const AParams: TArray<TParserParam>; const AFunction: TFunc<TArray<TParserValue>, TParserValue>; const AVarArg: Boolean = False);
  end;

  TParserCustomFunction = class(TParserFunction, IParserValueSupplier)
  private
    FSyntaxTree: TParserTree;
    FHelpers: TObjectDictionary<String, TParserObject>;
    function GetValues(const AName: String; const AArgs: TArray<TParserValue>): TParserValue;
    function GetRefTargets(const AName: String): IParserValueRefTarget;
    function GetMinArgCount(const AName: String): Integer; reintroduce; overload;
    function GetMaxArgCount(const AName: String): Integer; reintroduce; overload;
  protected
    function ContainsValue(const AName: String): Boolean;
    function ContainsRefTarget(const AName: String): Boolean;
    property Values[const AName: String; const AArgs: TArray<TParserValue>]: TParserValue read GetValues;
    property RefTargets[const AName: String]: IParserValueRefTarget read GetRefTargets;
    function GetValue(const AArgs: TArray<TParserValue>): TParserValue; override;
    property MinArgCount[const AName: String]: Integer read GetMinArgCount;
    property MaxArgCount[const AName: String]: Integer read GetMaxArgCount;
  public
    property SyntaxTree: TParserTree read FSyntaxTree write FSyntaxTree;
    constructor Create(const AName: String; const AParams: TArray<TParserParam>; const ASyntaxTree: TParserTree = Default(TParserTree));
    destructor Destroy; override;
  end;

  TParserTypeClass = class of TParserType;

  TParserType = class abstract(TParserObject, IParserValueConstraint, IParserValueRefTarget)
  private
    FConstructor: TParserFunction;
    class function GetTypeClasses(const AKeyword: TParserKeyword): TParserTypeClass; static;
  protected
    function GetValue(const AArgs: TArray<TParserValue>): TParserValue; override;
    function GetMinArgCount: Integer; override;
    function GetMaxArgCount: Integer; override;
    function GetArgTypes(const AIndex: Integer): IParserValueConstraint; override;
    function GetSupported(const AValue: TParserValue): Boolean; virtual; abstract;
    function GetNew: TParserValue; virtual;
  public
    class property TypeClasses[const AKeyword: TParserKeyword]: TParserTypeClass read GetTypeClasses;
    property &Constructor: TParserFunction read FConstructor write FConstructor;
    property Supported[const AValue: TParserValue]: Boolean read GetSupported;
    property New: TParserValue read GetNew;
    destructor Destroy; override;
    procedure AssertValue(const AValue: TParserValue);
  end;

  TParserEventType = class;

  TParserTypeEvent = function (const AType: TParserEventType; const AValue: TParserValue): Boolean of object;

  TParserEventType = class(TParserType)
  private
    FOnCheck: TParserTypeEvent;
  protected
    function GetSupported(const AValue: TParserValue): Boolean; override;
  public
    property OnCheck: TParserTypeEvent read FOnCheck;
    constructor Create(const AName: String; const AOnCheck: TParserTypeEvent);
  end;

  TParserReferenceType = class(TParserType)
  private
    FChecker: TFunc<TParserValue, Boolean>;
  protected
    function GetSupported(const AValue: TParserValue): Boolean; override;
  public
    property Checker: TFunc<TParserValue, Boolean> read FChecker;
    constructor Create(const AName: String; const AChecker: TFunc<TParserValue, Boolean>);
  end;

  TParserAnyType = class(TParserType)
  private
    FTypes: TObjectList<TParserType>;
  protected
    function GetSupported(const AValue: TParserValue): Boolean; override;
  public
    constructor Create(const AName: String); overload;
    constructor Create(const AName: String; const ATypes: TArray<TParserType>); overload;
    function GetEnumerator: TEnumerator<TParserType>; overload;
  end;

  TParserRefType = class(TParserType)
  private
    FType: TParserType;
  protected
    function GetSupported(const AValue: TParserValue): Boolean; override;
  public
    property &Type: TParserType read FType;
    constructor Create(const AName: String); overload;
    constructor Create(const AName: String; const AType: TParserType); overload;
  end;

  TParserRangeType = class(TParserType)
  private
    FMinValue: TParserValue;
    FMaxValue: TParserValue;
  protected
    function GetSupported(const AValue: TParserValue): Boolean; override;
  public
    property MinValue: TParserValue read FMinValue;
    property MaxValue: TParserValue read FMaxValue;
    constructor Create(const AName: String; const AFirstConstraint, ASecondConstraint: TParserValue);
  end;

  TParserEnumType = class(TParserType)
  private
    FValues: TList<TParserValue>;
    function GetValueCount: Integer;
    function GetValues(const AIndex: Integer): TParserValue;
  protected
    function GetSupported(const AValue: TParserValue): Boolean; override;
  public
    property Values[const AIndex: Integer]: TParserValue read GetValues; default;
    property ValueCount: Integer read GetValueCount;
    constructor Create(const AName: String; const AValues: TArray<TParserValue>);
    destructor Destroy; override;
    function GetEnumerator: TEnumerator<TParserValue>;
  end;

  TParserNumberType = class(TParserType)
  protected
    function GetSupported(const AValue: TParserValue): Boolean; override;
    function GetNew: TParserValue; override;
  end;

  TParserIntegerType = class(TParserNumberType, IParserValueRefTarget)
  private
    FDivisors: TList<TParserValue>;
    function GetDivisors(const AIndex: Integer): TParserValue;
    function GetDivisorCount: Integer;
  protected
    function GetSupported(const AValue: TParserValue): Boolean; override;
  public
    property Divisors[const AIndex: Integer]: TParserValue read GetDivisors; default;
    property DivisorCount: Integer read GetDivisorCount;
    constructor Create(const AName: String); overload;
    constructor Create(const AName: String; const ADivisors: TArray<TParserValue>); overload;
    destructor Destroy; override;
    function GetEnumerator: TEnumerator<TParserValue>;
  end;

  TParserRangeIntegerType = class(TParserNumberType)
  private
    FRange: TParserRangeType;
    FInteger: TParserIntegerType;
  protected
    function GetSupported(const AValue: TParserValue): Boolean; override;
  public
    property Range: TParserRangeType read FRange;
    property Integer: TParserIntegerType read FInteger;
    constructor Create(const AName: String; const AMinValue, AMaxValue: TParserValue); overload;
    constructor Create(const AName: String; const AMinValue, AMaxValue: TParserValue; const ADivisors: TArray<TParserValue>); overload;
    destructor Destroy; override;
  end;

  TParserStringType = class(TParserType)
  private
    FLength: TParserValue;
  protected
    function GetSupported(const AValue: TParserValue): Boolean; override;
    function GetNew: TParserValue; override;
  public
    property Length: TParserValue read FLength;
    constructor Create(const AName: String); overload;
    constructor Create(const AName: String; const ALength: TParserValue); overload;
  end;

  TParserStructType = class(TParserType)
  protected
    function GetSupported(const AValue: TParserValue): Boolean; override;
  end;

  TParserArrayType = class(TParserType)
  protected
    function GetSupported(const AValue: TParserValue): Boolean; override;
    function GetNew: TParserValue; override;
  public
    constructor Create(const AName: String);
  end;

  TParserFieldType = class abstract(TParserType)
  protected
    function GetFields(const AIndex: Integer): TParserValue; virtual; abstract;
    function GetFieldCount: Integer; virtual; abstract;
  public
    property Fields[const AIndex: Integer]: TParserValue read GetFields; default;
    property FieldCount: Integer read GetFieldCount;
  end;

  TParserRecordType = class(TParserFieldType)
  private
    FFields: TList<TParserValue>;
  protected
    function GetSupported(const AValue: TParserValue): Boolean; override;
    function GetFields(const AIndex: Integer): TParserValue; override;
    function GetFieldCount: Integer; override;
    function GetNew: TParserValue; override;
  public
    constructor Create(const AName: String); overload;
    constructor Create(const AName: String; const AFields: TArray<TParserValue>); overload;
    destructor Destroy; override;
    function GetEnumerator: TEnumerator<TParserValue>;
  end;

  TParserObjectType = class(TParserFieldType)
  private
    FParents: TObjectList<TParserFieldType>;
    function GetParents(const AIndex: Integer): TParserFieldType;
    function GetParentCount: Integer;
  protected
    function GetSupported(const AValue: TParserValue): Boolean; override;
    function GetFields(const AIndex: Integer): TParserValue; override;
    function GetFieldCount: Integer; override;
  public
    property Parents[const AIndex: Integer]: TParserFieldType read GetParents; default;
    property ParentCount: Integer read GetParentCount;
    constructor Create(const AName: String; const AParents: TArray<TParserFieldType>);
    destructor Destroy; override;
    function GetEnumerator: TEnumerator<TParserFieldType>;
  end;

  TParserTempType = class(TInterfacedObject, IParserValueConstraint)
  private
    FType: TParserType;
    function GetSupported(const AValue: TParserValue): Boolean;
  public
    property Supported[const AValue: TParserValue]: Boolean read GetSupported;
    property &Type: TParserType read FType;
    constructor Create(const AType: TParserType);
    destructor Destroy; override;
    procedure AssertValue(const AValue: TParserValue);
  end;

  TParserAttrib = class(TParserObject)
  private

  public
//    constructor Create(const AName: String; const AParams: TArray<TParserParam>);
  end;

implementation

{ TParserObject }

constructor TParserObject.Create(const AName: String);
begin
  inherited Create;
  if not ValidName(AName, True) then
  begin
    raise EParserObjectNameError.CreateFmt('Invalid name: %s', [AName.QuotedString]);
  end;
  FName := AName;
end;

function TParserObject.GetName: String;
begin
  Result := FName;
end;

function TParserObject.GetValue(const AArgs: TArray<TParserValue>): TParserValue;
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
  Result := TParserValue.Empty[vkDouble];
end;

class function TParserObject.ValidName(const AName: String; const AAllowEmpty: Boolean = False): Boolean;
begin
  Result := (AName.IsEmpty and AAllowEmpty) or (IsValidIdent(AName) and (TParserKeyword.Create(AName) = kwNone));
end;

{ TParserObjectHelper }

function TParserObjectHelper.GetAddressable: Boolean;
begin
  Result := Supports(Self, IParserValueRefTarget);
end;

function TParserObjectHelper.GetWritable: Boolean;
begin
  Result := Supports(Self, IParserWritableObject);
end;

{ TParserConstant }

constructor TParserConstant.Create(const AName: String; const AValue: TParserValue);
begin
  inherited Create(AName);
  FValue := AValue;
end;

function TParserConstant.GetArgTypes(const AIndex: Integer): IParserValueConstraint;
begin
  Result := nil;
end;

function TParserConstant.GetMaxArgCount: Integer;
begin
  Result := 0;
end;

function TParserConstant.GetMinArgCount: Integer;
begin
  Result := 0;
end;

function TParserConstant.GetValue(const AArgs: TArray<TParserValue>): TParserValue;
begin
  inherited;
  Result := FValue;
end;

{ TParserVariable }

procedure TParserVariable.SetValue(const AValue: TParserValue);
begin
  FValue := AValue;
end;

{ TParserParam }

constructor TParserParam.Create(const AName: String; const ADefault: TParserValue; const AType: IParserValueConstraint = nil);
begin
  Create(AName, AType);
  FDefault := ADefault;
  Include(FOptions, poDefault);
end;

constructor TParserParam.Create(const AName: String; const AType: IParserValueConstraint = nil);
begin
  if not TParserObject.ValidName(AName) then
  begin
    raise EParserObjectNameError.CreateFmt('Invalid name: %s', [AName.QuotedString]);
  end;
  FName := AName;
  FType := AType;
  FOptions := System.Default(TParserParamOptions);
  if Assigned(AType) then
  begin
    Include(FOptions, poType);
  end;
end;

{ TParserInline }

constructor TParserInline.Create(const AName: String; const ASyntaxTree: TParserTree);
begin

end;

{ TParserFunction }

constructor TParserFunction.Create(const AName: String; const AParams: TArray<TParserParam>; const AVarArg: Boolean = False);
var
  LParam: TParserParam;
begin
  inherited Create(AName);
  FParams := TDictionary<String, TParserParam>.Create(Length(AParams), TIStringComparer.Ordinal);
  FOrderedParamNames := TStringList.Create(dupError, False, False);
  FOrderedParamNames.Capacity := Length(AParams);
  for LParam in AParams do
  begin
    if not IsValidIdent(LParam.Name) then
    begin
      raise EParserFunctionParamNameError.CreateFmt('Invalid function parameter name: %s', [LParam.Name.QuotedString]);
    end;
    if FParams.ContainsKey(LParam.Name) then
    begin
      raise EParserFunctionParamDuplicateError.CreateFmt('Redeclared identifier: %s', [LParam.Name.QuotedString]);
    end;
    if not (poDefault in LParam.Options) then
    begin
      if MinArgCount <> ParamCount then
      begin
        raise EParserFunctionParamDefaultError.CreateFmt('Function parameter %s must have a fallback value', [LParam.Name.QuotedString]);
      end;
      Inc(FMinArgCount);
    end;
    FParams.Add(LParam.Name, LParam);
    FOrderedParamNames.Add(LParam.Name);
  end;
  FVarArg := AVarArg;
  FArgs := TObjectStack<TList<TParserValue>>.Create;
end;

destructor TParserFunction.Destroy;
begin
  FArgs.Free;
  FOrderedParamNames.Free;
  FParams.Free;
  inherited;
end;

function TParserFunction.GetArgTypes(const AIndex: Integer): IParserValueConstraint;
begin
  Result := Params[ParamNames[ParamIndex(AIndex)]].&Type;
end;

function TParserFunction.GetMaxArgCount: Integer;
begin
  if VarArg then
  begin
    Result := -1;
  end else
  begin
    Result := ParamCount;
  end;
end;

function TParserFunction.GetMinArgCount: Integer;
begin
  Result := FMinArgCount;
end;

function TParserFunction.GetParamCount: Integer;
begin
  Result := FParams.Count;
end;

function TParserFunction.GetParamNames(const AIndex: Integer): String;
begin
  Result := FOrderedParamNames[AIndex];
end;

function TParserFunction.GetParams(const AName: String): TParserParam;
begin
  Result := FParams[AName];
end;

function TParserFunction.GetValue(const AArgs: TArray<TParserValue>): TParserValue;
var
  LIndex: Integer;
  LArgs: TList<TParserValue>;
begin
  inherited;
  LArgs := TList<TParserValue>.Create;
  LArgs.AddRange(AArgs);
  FArgs.Push(LArgs);
  PrepareArgs;
  for LIndex := 0 to Pred(LArgs.Count) do
  begin
    if Assigned(ArgTypes[LIndex]) then
    begin
      ArgTypes[LIndex].AssertValue(LArgs[LIndex]);
    end;
  end;
  Result := TParserValue.Empty[vkDouble];
end;

function TParserFunction.ParamIndex(const AArgIndex: Integer): Integer;
begin
  if (AArgIndex > Pred(ParamCount)) and VarArg then
  begin
    Result := Pred(ParamCount);
  end else
  begin
    Result := AArgIndex;
  end;
end;

procedure TParserFunction.PrepareArgs;
var
  LIndex: Integer;
begin
  for LIndex := FArgs.Peek.Count to Pred(ParamCount) do
  begin
    FArgs.Peek.Add(Params[ParamNames[ParamIndex(LIndex)]].Default);
  end;
end;

procedure TParserFunction.RestoreArgs;
begin
  FArgs.Pop;
end;

{ TParserEventFunction }

constructor TParserEventFunction.Create(const AName: String; const AParams: TArray<TParserParam>; const AOnInvoke: TParserFunctionEvent; const AVarArg: Boolean = False);
begin
  inherited Create(AName, AParams, AVarArg);
  FOnInvoke := AOnInvoke;
end;

function TParserEventFunction.GetValue(const AArgs: TArray<TParserValue>): TParserValue;
begin
  inherited;
  try
    Result := OnInvoke(Self, FArgs.Peek.ToArray);
  finally
    RestoreArgs;
  end;
end;

{ TParserReferenceFunction }

constructor TParserReferenceFunction.Create(const AName: String; const AParams: TArray<TParserParam>; const AFunction: TFunc<TArray<TParserValue>, TParserValue>; const AVarArg: Boolean = False);
begin
  inherited Create(AName, AParams, AVarArg);
  FFunction := AFunction;
end;

function TParserReferenceFunction.GetValue(const AArgs: TArray<TParserValue>): TParserValue;
begin
  inherited;
  try
    Result := FFunction(FArgs.Peek.ToArray);
  finally
    RestoreArgs;
  end;
end;

{ TParserCustomFunction }

function TParserCustomFunction.ContainsRefTarget(const AName: String): Boolean;
begin
  Result := FHelpers.ContainsKey(AName) and FHelpers[AName].Addressable;
end;

function TParserCustomFunction.ContainsValue(const AName: String): Boolean;
begin
  Result := FParams.ContainsKey(AName) or FHelpers.ContainsKey(AName);
end;

constructor TParserCustomFunction.Create(const AName: String; const AParams: TArray<TParserParam>; const ASyntaxTree: TParserTree);

  procedure AddHelpers;
  var
    LIndex: Integer;
    LParams: TArray<TParserValue>;
  begin
    FHelpers.Add('Self', TParserConstant.Create('Self', TParserValue.Create(Self)));
    SetLength(LParams, Length(AParams));
    for LIndex := Low(LParams) to High(LParams) do
    begin
      LParams[LIndex] := TParserValue.Create([TPair<String, TParserValue>.Create('Name', TParserValue.Create(AParams[LIndex].Name))]);
      if poType in AParams[LIndex].Options then
      begin
        LParams[LIndex] := TParserValue.Add(LParams[LIndex], TParserValue.Create([TPair<String, TParserValue>.Create('T', TParserValue.Create(AParams[LIndex].&Type as IParserValueRefTarget))]));
      end;
      if poDefault in AParams[LIndex].Options then
      begin
        LParams[LIndex] := TParserValue.Add(LParams[LIndex], TParserValue.Create([TPair<String, TParserValue>.Create('Default', AParams[LIndex].Default)]));
      end;
    end;
    FHelpers.Add('Params', TParserConstant.Create('Params', TParserValue.Create(LParams)));
    FHelpers.Add('Args', TParserReferenceFunction.Create('Args', [],
      function (Params: TArray<TParserValue>): TParserValue
      begin
        if FArgs.Count = 0 then
        begin
          Result := TParserValue.Empty[vkArray];
        end else
        begin
          Result := TParserValue.Create(FArgs.Peek.ToArray);
        end;
      end));
  end;

begin
  inherited Create(AName, AParams);
  FSyntaxTree := ASyntaxTree;
  FHelpers := TObjectDictionary<String, TParserObject>.Create([doOwnsValues], TIStringComparer.Ordinal);
  AddHelpers;
end;

destructor TParserCustomFunction.Destroy;
begin
  FHelpers.Free;
  SyntaxTree.Free;
  inherited;
end;

function TParserCustomFunction.GetMaxArgCount(const AName: String): Integer;
begin
  if FHelpers.ContainsKey(AName) then
  begin
    Result := FHelpers[AName].MaxArgCount;
  end else
  begin
    Result := 0;
  end;
end;

function TParserCustomFunction.GetMinArgCount(const AName: String): Integer;
begin
  if FHelpers.ContainsKey(AName) then
  begin
    Result := FHelpers[AName].MinArgCount;
  end else
  begin
    Result := 0;
  end;
end;

function TParserCustomFunction.GetRefTargets(const AName: String): IParserValueRefTarget;
var
  LObject: TParserObject;
begin
  if FHelpers.ContainsKey(AName) then
  begin
    LObject := FHelpers[AName];
  end;
  if (FOrderedParamNames.IndexOf(AName) = -1) and not Assigned(LObject) then
  begin
    raise EParserDictionaryUnknownError.CreateFmt('Undeclared identifier: %s', [AName.QuotedString]);
  end;
  if (FOrderedParamNames.IndexOf(AName) <> -1) or (Assigned(LObject) and not FHelpers[AName].Addressable) then
  begin
    raise EParserAddressError.CreateFmt('%s not addressable', [LObject.Name.QuotedString]);
  end;
  Result := LObject as IParserValueRefTarget;
end;

function TParserCustomFunction.GetValue(const AArgs: TArray<TParserValue>): TParserValue;
begin
  inherited;
  try
    Result := SyntaxTree.Calculate;
  finally
    RestoreArgs;
  end;
end;

function TParserCustomFunction.GetValues(const AName: String; const AArgs: TArray<TParserValue>): TParserValue;
var
  LIndex: Integer;
begin
  LIndex := FOrderedParamNames.IndexOf(AName);
  if LIndex <> -1 then
  begin
    Result := FArgs.Peek[LIndex];
  end else
  begin
    if not FHelpers.ContainsKey(AName) then
    begin
      raise EParserDictionaryUnknownError.CreateFmt('Undeclared identifier: %s', [AName.QuotedString]);
    end;
    Result := FHelpers[AName].Value[AArgs];
  end;
end;

{ TParserType }

procedure TParserType.AssertValue(const AValue: TParserValue);
begin
  if not Supported[AValue] then
  begin
    raise EParserTypeCompatibilityError.CreateFmt('%s not supported by %s', [AValue.ToString, Name.QuotedString]);
  end;
end;

destructor TParserType.Destroy;
begin
  &Constructor.Free;
  inherited;
end;

function TParserType.GetArgTypes(const AIndex: Integer): IParserValueConstraint;
begin
  if Assigned(&Constructor) then
  begin
    Result := &Constructor.ArgTypes[AIndex];
  end else
  begin
    Result := nil;
  end;
end;

function TParserType.GetMaxArgCount: Integer;
begin
  if Assigned(&Constructor) then
  begin
    Result := &Constructor.MaxArgCount;
  end else
  begin
    Result := -1;
  end;
end;

function TParserType.GetMinArgCount: Integer;
begin
  if Assigned(&Constructor) then
  begin
    Result := &Constructor.MinArgCount;
  end else
  begin
    Result := -1;
  end;
end;

function TParserType.GetNew: TParserValue;
begin
  raise EParserTypeNoDefault.CreateFmt('%s has no default value', [Name.QuotedString]);
end;

class function TParserType.GetTypeClasses(const AKeyword: TParserKeyword): TParserTypeClass;
begin
  case AKeyword of
    kwRange:
      begin
        Result := TParserRangeType;
      end;
    kwEnum:
      begin
        Result := TParserEnumType;
      end;
    kwInt:
      begin
        Result := TParserIntegerType;
      end;
    kwRangeInt:
      begin
        Result := TParserRangeIntegerType;
      end;
    else
      begin
        Result := Default(TParserTypeClass);
      end;
  end;
end;

function TParserType.GetValue(const AArgs: TArray<TParserValue>): TParserValue;
begin
  if not Assigned(&Constructor) then
  begin
    raise EParserTypeNoConstructor.CreateFmt('%s not evaluable', [Name.QuotedString]);
  end;
  Result := &Constructor.Value[AArgs];
  AssertValue(Result);
end;

{ TParserEventType }

constructor TParserEventType.Create(const AName: String; const AOnCheck: TParserTypeEvent);
begin
  inherited Create(AName);
  FOnCheck := AOnCheck;
end;

function TParserEventType.GetSupported(const AValue: TParserValue): Boolean;
begin
  Result := OnCheck(Self, AValue);
end;

{ TParserReferenceType }

constructor TParserReferenceType.Create(const AName: String; const AChecker: TFunc<TParserValue, Boolean>);
begin
  inherited Create(AName);
  FChecker := AChecker;
end;

function TParserReferenceType.GetSupported(const AValue: TParserValue): Boolean;
begin
  Result := Checker(AValue);
end;

{ TParserAnyType }

constructor TParserAnyType.Create(const AName: String; const ATypes: TArray<TParserType>);
begin
  inherited Create(AName);
  FTypes := TObjectList<TParserType>.Create(False);
  FTypes.AddRange(ATypes);
end;

constructor TParserAnyType.Create(const AName: String);
begin
  Create(AName, []);
end;

function TParserAnyType.GetEnumerator: TEnumerator<TParserType>;
begin
  Result := FTypes.GetEnumerator;
end;

function TParserAnyType.GetSupported(const AValue: TParserValue): Boolean;
var
  LType: TParserType;
begin
  for LType in Self do
  begin
    if LType.Supported[AValue] then
    begin
      Exit(True);
    end;
  end;
  Result := False;
end;

{ TParserRefType }

constructor TParserRefType.Create(const AName: String; const AType: TParserType);
begin
  inherited Create(AName);
  FType := AType;
end;

function TParserRefType.GetSupported(const AValue: TParserValue): Boolean;
begin
  Result := (AValue.Kind = vkReference) and (not Assigned(&Type) or &Type.Supported[AValue]);
end;

constructor TParserRefType.Create(const AName: String);
begin
  Create(AName, nil);
end;

{ TParserRangeType }

constructor TParserRangeType.Create(const AName: String; const AFirstConstraint, ASecondConstraint: TParserValue);
begin
  inherited Create(AName);
  if TParserValue.Compare(AFirstConstraint, ASecondConstraint) = GreaterThanValue then
  begin
    FMinValue := ASecondConstraint;
    FMaxValue := AFirstConstraint;
  end else
  begin
    FMinValue := AFirstConstraint;
    FMaxValue := ASecondConstraint;
  end;
end;

function TParserRangeType.GetSupported(const AValue: TParserValue): Boolean;
begin
  Result := (TParserValue.Compare(AValue, MinValue) <> LessThanValue) and (TParserValue.Compare(AValue, MaxValue) <> GreaterThanValue);
end;

{ TParserEnumType }

constructor TParserEnumType.Create(const AName: String; const AValues: TArray<TParserValue>);
begin
  inherited Create(AName);
  FValues := TList<TParserValue>.Create(TComparer<TParserValue>.Construct(
    function (const Left, Right: TParserValue): Integer
    begin
      Result := TParserValue.Compare(Left, Right);
    end
  ));
  FValues.AddRange(AValues);
end;

destructor TParserEnumType.Destroy;
begin
  FValues.Free;
  inherited;
end;

function TParserEnumType.GetEnumerator: TEnumerator<TParserValue>;
begin
  Result := FValues.GetEnumerator;
end;

function TParserEnumType.GetSupported(const AValue: TParserValue): Boolean;
begin
  Result := FValues.Contains(AValue);
end;

function TParserEnumType.GetValueCount: Integer;
begin
  Result := FValues.Count;
end;

function TParserEnumType.GetValues(const AIndex: Integer): TParserValue;
begin
  Result := FValues[AIndex];
end;

{ TParserNumberType }

function TParserNumberType.GetNew: TParserValue;
begin
  Result := TParserValue.Empty[vkDouble];
  if not Supported[Result] then
  begin
    inherited;
  end;
end;

function TParserNumberType.GetSupported(const AValue: TParserValue): Boolean;
begin
  Result := AValue.Kind = vkDouble;
end;

{ TParserIntegerType }

constructor TParserIntegerType.Create(const AName: String; const ADivisors: TArray<TParserValue>);
begin
  inherited Create(AName);
  FDivisors := TList<TParserValue>.Create;
  FDivisors.AddRange(ADivisors);
end;

destructor TParserIntegerType.Destroy;
begin
  FDivisors.Free;
  inherited;
end;

constructor TParserIntegerType.Create(const AName: String);
begin
  Create(AName, []);
end;

function TParserIntegerType.GetDivisorCount: Integer;
begin
  Result := FDivisors.Count;
end;

function TParserIntegerType.GetDivisors(const AIndex: Integer): TParserValue;
begin
  Result := FDivisors[AIndex];
end;

function TParserIntegerType.GetEnumerator: TEnumerator<TParserValue>;
begin
  Result := FDivisors.GetEnumerator;
end;

function TParserIntegerType.GetSupported(const AValue: TParserValue): Boolean;
var
  LDivisor: TParserValue;
begin
  if inherited and AValue.IsInteger then
  begin
    for LDivisor in Self do
    begin
      if not IsZero(FMod(AValue.AsDouble, LDivisor.AsDouble)) then
      begin
        Exit(False);
      end;
    end;
    Result := True;
  end else
  begin
    Result := False;
  end;
end;

{ TParserRangeIntegerType }

constructor TParserRangeIntegerType.Create(const AName: String; const AMinValue, AMaxValue: TParserValue; const ADivisors: TArray<TParserValue>);
begin
  inherited Create(AName);
  FRange := TParserRangeType.Create(AName, AMinValue, AMaxValue);
  FInteger := TParserIntegerType.Create(AName, ADivisors);
end;

constructor TParserRangeIntegerType.Create(const AName: String; const AMinValue, AMaxValue: TParserValue);
begin
  Create(AName, AMinValue, AMaxValue, []);
end;

destructor TParserRangeIntegerType.Destroy;
begin
  Integer.Free;
  Range.Free;
  inherited;
end;

function TParserRangeIntegerType.GetSupported(const AValue: TParserValue): Boolean;
begin
  Result := Integer.Supported[AValue] and Range.Supported[AValue];
end;

{ TParserStructType }

function TParserStructType.GetSupported(const AValue: TParserValue): Boolean;
begin
  Result := AValue.Kind in [vkString, vkArray, vkRecord];
end;

{ TParserStringType }

constructor TParserStringType.Create(const AName: String; const ALength: TParserValue);
begin
  inherited Create(AName);
  FLength := TParserValue.Create(Max(0, ALength.AsDouble));
end;

constructor TParserStringType.Create(const AName: String);
begin
  Create(AName, TParserValue.Create(0));
end;

function TParserStringType.GetNew: TParserValue;
begin
  Result := TParserValue.Empty[vkString];
end;

function TParserStringType.GetSupported(const AValue: TParserValue): Boolean;
begin
  Result := (AValue.Kind = vkString) and (Length.IsEmpty or Length.Equals(TParserValue.Create(AValue.Count)));
end;

{ TParserArrayType }

constructor TParserArrayType.Create(const AName: String);
begin
  inherited;
end;

function TParserArrayType.GetNew: TParserValue;
begin
  Result := TParserValue.Empty[vkArray];
end;

function TParserArrayType.GetSupported(const AValue: TParserValue): Boolean;
begin
  Result := AValue.Kind = vkArray;
end;

{ TParserRecordType }

constructor TParserRecordType.Create(const AName: String);
begin
  Create(AName, []);
end;

constructor TParserRecordType.Create(const AName: String; const AFields: TArray<TParserValue>);
begin
  inherited Create(AName);
  FFields := TList<TParserValue>.Create(TComparer<TParserValue>.Construct(
    function (const Left, Right: TParserValue): Integer
    begin
      Result := TParserValue.Compare(Left, Right);
    end
  ));
  FFields.AddRange(AFields);
end;

destructor TParserRecordType.Destroy;
begin
  FFields.Free;
  inherited;
end;

function TParserRecordType.GetEnumerator: TEnumerator<TParserValue>;
begin
  Result := FFields.GetEnumerator;
end;

function TParserRecordType.GetFieldCount: Integer;
begin
  Result := FFields.Count;
end;

function TParserRecordType.GetFields(const AIndex: Integer): TParserValue;
begin
  Result := FFields[AIndex];
end;

function TParserRecordType.GetNew: TParserValue;
begin
  Result := TParserValue.NewRecord[FFields.ToArray.AsStrings];
end;

function TParserRecordType.GetSupported(const AValue: TParserValue): Boolean;
var
  LValue: TParserValue;
begin
  if (AValue.Kind = vkRecord) and (AValue.Count = FieldCount) then
  begin
    for LValue in Self do
    begin
      if not AValue.HasMember(LValue) then
      begin
        Exit(False);
      end;
    end;
    Result := True;
  end else
  begin
    Result := False;
  end;
end;

{ TParserObjectType }

constructor TParserObjectType.Create(const AName: String; const AParents: TArray<TParserFieldType>);
begin
  inherited Create(AName);
  FParents := TObjectList<TParserFieldType>.Create(False);
  FParents.AddRange(AParents);
end;

destructor TParserObjectType.Destroy;
begin
  FParents.Free;
  inherited;
end;

function TParserObjectType.GetEnumerator: TEnumerator<TParserFieldType>;
begin
  Result := FParents.GetEnumerator;
end;

function TParserObjectType.GetFieldCount: Integer;
var
  LParent: TParserFieldType;
begin
  Result := 0;
  for LParent in Self do
  begin
    Inc(Result, LParent.FieldCount);
  end;
end;

function TParserObjectType.GetFields(const AIndex: Integer): TParserValue;
var
  LParent: TParserFieldType;
  LCount: Integer;
begin
  LCount := AIndex;
  for LParent in Self do
  begin
    if LCount < LParent.FieldCount then
    begin
      Exit(LParent[LCount]);
    end else
    begin
      Dec(LCount, LParent.FieldCount);
    end;
  end;
end;

function TParserObjectType.GetParentCount: Integer;
begin
  Result := FParents.Count;
end;

function TParserObjectType.GetParents(const AIndex: Integer): TParserFieldType;
begin
  Result := FParents[AIndex];
end;

function TParserObjectType.GetSupported(const AValue: TParserValue): Boolean;
var
  LParent: TParserFieldType;
begin
  for LParent in Self do
  begin
    if not LParent.Supported[AValue] then
    begin
      Exit(False);
    end;
  end;
  Result := True;
end;

{ TParserTempType }

procedure TParserTempType.AssertValue(const AValue: TParserValue);
begin
  &Type.AssertValue(AValue);
end;

constructor TParserTempType.Create(const AType: TParserType);
begin
  FType := AType;
end;

destructor TParserTempType.Destroy;
begin
  &Type.Free;
  inherited;
end;

function TParserTempType.GetSupported(const AValue: TParserValue): Boolean;
begin
  Result := &Type.Supported[AValue];
end;

end.