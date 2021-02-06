unit Parser.Value;

interface

uses
  System.SysUtils, System.Types, System.Math, System.Generics.Collections, System.Generics.Defaults, System.Rtti,
  Parser.Exception;

type
  TParserValueKind = (vkReference, vkDouble, vkString, vkArray, vkRecord{, vkReference});

  IParserValueRefTarget = interface;

  TParserValue = record
  private
    FValue: TValue;
    class function GetEmpty(const AKind: TParserValueKind): TParserValue; static;
    class function GetInf: TParserValue; static;
    class function GetNaN: TParserValue; static;
    class function GetMaxValue: TParserValue; static;
    class function GetMinValue: TParserValue; static;
    class function GetNewArray(const ALength: Integer): TParserValue; static;
    class function GetNewRecord(const AFields: TArray<String>): TParserValue; static;
    function GetKind: TParserValueKind;
    function GetMembers(const AKey: TParserValue): TParserValue;
    function GetAsReference: IParserValueRefTarget;
    procedure SetAsReference(const AValue: IParserValueRefTarget);
    function GetAsDouble: Double;
    procedure SetAsDouble(const AValue: Double);
    function GetAsString: String;
    procedure SetAsString(const AValue: String);
    function GetAsArray: TArray<TParserValue>;
    procedure SetAsArray(const AValue: TArray<TParserValue>);
    function GetAsRecord: TArray<TPair<String, TParserValue>>;
    procedure SetAsRecord(const AValue: TArray<TPair<String, TParserValue>>);
    function GetIsNaN: Boolean;
    function GetIsNegInf: Boolean;
    function GetIsPosInf: Boolean;
    function GetIsEmpty: Boolean;
    function GetCount: Integer;
  public
    class property Empty[const AKind: TParserValueKind]: TParserValue read GetEmpty;
    class property NaN: TParserValue read GetNaN;
    class property Inf: TParserValue read GetInf;
    class property MinValue: TParserValue read GetMinValue;
    class property MaxValue: TParserValue read GetMaxValue;
    class property NewArray[const ALength: Integer]: TParserValue read GetNewArray;
    class property NewRecord[const AFields: TArray<String>]: TParserValue read GetNewRecord;
    class function Compare(const AFirst, ASecond: TParserValue): TValueRelationship; static;
    class function Add(const AFirst, ASecond: TParserValue): TParserValue; static;
    class function Subtract(const AFirst, ASecond: TParserValue): TParserValue; static;
    class function Multiply(const AFirst, ASecond: TParserValue): TParserValue; static;
    class function Divide(const AFirst, ASecond: TParserValue): TParserValue; static;
    class function Exponentiate(const AFirst, ASecond: TParserValue): TParserValue; static;
    property Kind: TParserValueKind read GetKind;
    property Count: Integer read GetCount;
    property Members[const AKey: TParserValue]: TParserValue read GetMembers; default;
    property IsNaN: Boolean read GetIsNaN;
    property IsPosInf: Boolean read GetIsPosInf;
    property IsNegInf: Boolean read GetIsNegInf;
    property IsEmpty: Boolean read GetIsEmpty;
    property AsReference: IParserValueRefTarget read GetAsReference write SetAsReference;
    property AsDouble: Double read GetAsDouble write SetAsDouble;
    property AsString: String read GetAsString write SetAsString;
    property AsArray: TArray<TParserValue> read GetAsArray write SetAsArray;
    property AsRecord: TArray<TPair<String, TParserValue>> read GetAsRecord write SetAsRecord;
    constructor Create(const AReference: IParserValueRefTarget); overload;
    constructor Create(const ADouble: Double); overload;
    constructor Create(const AString: String); overload;
    constructor Create(const AArray: TArray<TParserValue>); overload;
    constructor Create(const ARecord: TArray<TPair<String, TParserValue>>); overload;
    function ToString: String;
    function HasMember(const AKey: TParserValue): Boolean;
    function Equals(const AValue: TParserValue): Boolean;
    function Negate: TParserValue;
  end;

  TParserValueHelper = record helper for TParserValue
  private
    function GetAsBoolean: Boolean;
    procedure SetAsBoolean(const AValue: Boolean);
    function GetAsInteger: Int64;
    procedure SetAsInteger(const AValue: Int64);
    function GetIsBoolean: Boolean;
    function GetIsInteger: Boolean;
  public
    property IsInteger: Boolean read GetIsInteger;
    property IsBoolean: Boolean read GetIsBoolean;
    property AsInteger: Int64 read GetAsInteger write SetAsInteger;
    property AsBoolean: Boolean read GetAsBoolean write SetAsBoolean;
    constructor Create(const AInteger: Int64); overload;
    constructor Create(const ABoolean: Boolean); overload;
  end;

  TParserValuesHelper = record helper for TArray<TParserValue>
  private
    function GetAsDoubles: TArray<Double>;
    function GetAsStrings: TArray<String>;
  public
    property AsDoubles: TArray<Double> read GetAsDoubles;
    property AsStrings: TArray<String> read GetAsStrings;
    class function Create(const AString: String): TArray<TParserValue>; static;
  end;

  IParserValueConstraint = interface
    ['{0E0905C6-427E-41FC-92EE-F31A0FC73AF4}']
    function GetSupported(const AValue: TParserValue): Boolean;
    property Supported[const AValue: TParserValue]: Boolean read GetSupported;
    procedure AssertValue(const AValue: TParserValue);
  end;

  IParserValueRefTarget = interface
    ['{0DF1D164-7078-4D25-B195-928D5496A0A3}']
    function GetName: String;
    function GetValue(const AArgs: TArray<TParserValue>): TParserValue;
    function GetMinArgCount: Integer;
    function GetMaxArgCount: Integer;
    function GetArgTypes(const AIndex: Integer): IParserValueConstraint;
    property Name: String read GetName;
    property MinArgCount: Integer read GetMinArgCount;
    property MaxArgCount: Integer read GetMaxArgCount;
    property ArgTypes[const AIndex: Integer]: IParserValueConstraint read GetArgTypes;
    property Value[const AArgs: TArray<TParserValue>]: TParserValue read GetValue;
  end;

  IParserValueSupplier = interface
    ['{4D63069F-53A6-4CEF-B69B-79BACE8D4698}']
    function ContainsValue(const AName: String): Boolean;
    function ContainsRefTarget(const AName: String): Boolean;
    function GetMinArgCount(const AName: String): Integer;
    function GetMaxArgCount(const AName: String): Integer;
    function GetValues(const AName: String; const AArgs: TArray<TParserValue>): TParserValue;
    function GetRefTargets(const AName: String): IParserValueRefTarget;
    property Values[const AName: String; const AArgs: TArray<TParserValue>]: TParserValue read GetValues;
    property RefTargets[const AName: String]: IParserValueRefTarget read GetRefTargets;
    property MinArgCount[const AName: String]: Integer read GetMinArgCount;
    property MaxArgCount[const AName: String]: Integer read GetMaxArgCount;
  end;

implementation

uses
  Parser.Exporter;

{ TParserValue }

constructor TParserValue.Create(const ADouble: Double);
begin
  AsDouble := ADouble;
end;

constructor TParserValue.Create(const AString: String);
begin
  AsString := AString;
end;

class function TParserValue.Add(const AFirst, ASecond: TParserValue): TParserValue;

  function AddRecords(const AFirst, ASecond: TArray<TPair<String, TParserValue>>): TArray<TPair<String, TParserValue>>;
  var
    LFields: TDictionary<String, TParserValue>;
    LField: TPair<String, TParserValue>;
  begin
    LFields := TDictionary<String, TParserValue>.Create(Length(AFirst) + Length(ASecond), TIStringComparer.Ordinal);
    try
      for LField in AFirst do
      begin
        LFields.Add(LField.Key, LField.Value);
      end;
      for LField in ASecond do
      begin
        if LFields.ContainsKey(LField.Key) then
        begin
          LFields[LField.Key] := TParserValue.Add(LFields[LField.Key], LField.Value);
        end else
        begin
          LFields.Add(LField.Key, LField.Value);
        end;
      end;
      Result := LFields.ToArray;
    finally
      LFields.Free;
    end;
  end;

begin
  case AFirst.Kind of
    vkDouble:
      begin
        Result := TParserValue.Create(AFirst.AsDouble + ASecond.AsDouble);
      end;
    vkString:
      begin
        Result := TParserValue.Create(Concat(AFirst.AsString, ASecond.AsString));
      end;
    vkArray:
      begin
        Result := TParserValue.Create(Concat(AFirst.AsArray, ASecond.AsArray));
      end;
    vkRecord:
      begin
        Result := TParserValue.Create(AddRecords(AFirst.AsRecord, ASecond.AsRecord));
      end;
  end;
end;

{$WARN NO_RETVAL OFF}
class function TParserValue.Compare(const AFirst, ASecond: TParserValue): TValueRelationship;

  function CompareArray(const AFirst, ASecond: TArray<TParserValue>): TValueRelationship;
  var
    LIndex: Integer;
  begin
    if Length(AFirst) = Length(ASecond) then
    begin
      for Lindex := Low(AFirst) to High(AFirst) do
      begin
        Result := Compare(AFirst[LIndex], ASecond[LIndex]);
        if Result <> EqualsValue then
        begin
          Exit;
        end;
      end;
    end else
    begin
      Result := CompareValue(Length(AFirst), Length(ASecond));
    end;
  end;

  function CompareRecord(const AFirst, ASecond: TArray<TPair<String, TParserValue>>): TValueRelationship;
  begin
    Result := EqualsValue;
  end;

begin
  if AFirst.Kind <> ASecond.Kind then
  begin
    raise EParserValueComparisonError.Create('Incomparable values');
  end;
  case AFirst.Kind of
    vkDouble:
      begin
        if (AFirst.IsNaN <> ASecond.IsNaN) then
        begin
          raise EParserValueComparisonError.CreateFmt('Incomparable values: %s and %s', [AFirst.ToString.QuotedString, ASecond.ToString.QuotedString]);
        end;
        Result := CompareValue(AFirst.AsDouble, ASecond.AsDouble);
      end;
    vkString:
      begin
        Result := String.Compare(AFirst.AsString, ASecond.AsString);
      end;
    vkArray:
      begin
        Result := CompareArray(AFirst.AsArray, ASecond.AsArray);
      end;
    vkRecord:
      begin
        Result := CompareRecord(AFirst.AsRecord, ASecond.AsRecord);
      end;
  end;
end;
{$WARN NO_RETVAL ON}

constructor TParserValue.Create(const AArray: TArray<TParserValue>);
begin
  AsArray := Copy(AArray);
end;

{$WARN NO_RETVAL OFF}
function TParserValue.Equals(const AValue: TParserValue): Boolean;

  function SameArray(const AFirst, ASecond: TArray<TParserValue>): Boolean;
  var
    LIndex: Integer;
  begin
    if Length(AFirst) = Length(ASecond) then
    begin
      for Lindex := Low(AFirst) to High(AFirst) do
      begin
        if not AFirst[LIndex].Equals(ASecond[LIndex]) then
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

  function SameRecord(const AFirst, ASecond: TArray<TPair<String, TParserValue>>): Boolean;
  var
    LFields: TDictionary<String, TParserValue>;
    LField: TPair<String, TParserValue>;
  begin
    if Length(AFirst) = Length(ASecond) then
    begin
      LFields := TDictionary<String, TParserValue>.Create(Length(AFirst), TIStringComparer.Ordinal);
      try
        for LField in AFirst do
        begin
          LFields.Add(LField.Key, LField.Value);
        end;
        for LField in ASecond do
        begin
          if not (LFields.ContainsKey(LField.Key) and LField.Value.Equals(LFields[LField.Key])) then
          begin
            Exit(False);
          end;
        end;
        Result := True;
      finally
        LFields.Free;
      end;
    end else
    begin
      Result := False;
    end;
  end;

begin
  if AValue.Kind = Kind then
  begin
    case Kind of
      vkReference:
        begin
          Result := AValue.AsReference = AsReference;
        end;
      vkDouble:
        begin
          Result := (AValue.IsNaN = IsNaN) and (AValue.IsPosInf = IsPosInf) and (AValue.IsNegInf = IsNegInf) and (AValue.IsEmpty = IsEmpty) and SameValue(AValue.AsDouble, AsDouble);
        end;
      vkString:
        begin
          Result := AValue.AsString.Equals(AsString);
        end;
      vkArray:
        begin
          Result := SameArray(AValue.AsArray, AsArray);
        end;
      vkRecord:
        begin
          Result := SameRecord(AValue.AsRecord, AsRecord);
        end;
    end;
  end else
  begin
    Result := False;
  end;
end;
{$WARN NO_RETVAL ON}

class function TParserValue.Exponentiate(const AFirst, ASecond: TParserValue): TParserValue;
begin

end;

function TParserValue.GetAsArray: TArray<TParserValue>;

  function StringToArray(const AString: String): TArray<TParserValue>;
  var
    LIndex: Integer;
  begin
    SetLength(Result, Length(AString));
    for LIndex := Low(Result) to High(Result) do
    begin
      Result[LIndex] := TParserValue.Create(AString.Chars[LIndex]);
    end;
  end;

begin
  case Kind of
    vkString:
      begin
        Result := StringToArray(FValue.AsString);
      end;
    vkArray:
      begin
        Result := FValue.AsType<TArray<TParserValue>>;
      end;
    else
      begin
        raise EParserValueKindError.CreateFmt('Unsupported value: %s', [ToString.QuotedString]);
      end;
  end;
end;

function TParserValue.GetAsDouble: Double;
begin
  if Kind <> vkDouble then
  begin
    raise EParserValueKindError.CreateFmt('Unsupported value: %s', [ToString.QuotedString]);
  end;
  Result := FValue.AsType<Double>;
end;

function TParserValue.GetAsRecord: TArray<TPair<String, TParserValue>>;
begin
  if Kind <> vkRecord then
  begin
    raise EParserValueKindError.CreateFmt('Unsupported value: %s', [ToString.QuotedString]);
  end;
  Result := FValue.AsType<TArray<TPair<String, TParserValue>>>;
end;

function TParserValue.GetAsReference: IParserValueRefTarget;
begin
  if Kind <> vkReference then
  begin
    raise EParserValueKindError.CreateFmt('Unsupported value: %s', [ToString.QuotedString]);
  end;
  try
    Result := FValue.AsType<IParserValueRefTarget>;
  except
    raise EParserValueRefError.Create('Invalid reference');
  end;
end;

function TParserValue.GetAsString: String;
begin
  if Kind <> vkString then
  begin
    raise EParserValueKindError.CreateFmt('Unsupported value: %s', [ToString.QuotedString]);
  end;
  Result := FValue.AsType<String>;
end;

function TParserValue.GetCount: Integer;
begin
  case Kind of
    vkString:
      begin
        Result := Length(AsString);
      end;
    vkArray:
      begin
        Result := Length(AsArray);
      end;
    vkRecord:
      begin
        Result := Length(AsRecord);
      end
    else
      begin
        raise EParserValueMemberError.Create('Value does not support members');
      end;
  end;
end;

class function TParserValue.GetEmpty(const AKind: TParserValueKind): TParserValue;
const
  LEmptyReference = nil;
  LEmptyDouble = 0;
  LEmptyString = String.Empty;
  LEmptyArray: TArray<TParserValue> = [];
  LEmptyRecord: TArray<TPair<String, TParserValue>> = [];
begin
  case AKind of
    vkReference:
      begin
        Result := TParserValue.Create(LEmptyReference);
      end;
    vkDouble:
      begin
        Result := TParserValue.Create(LEmptyDouble);
      end;
    vkString:
      begin
        Result := TParserValue.Create(LEmptyString);
      end;
    vkArray:
      begin
        Result := TParserValue.Create(LEmptyArray);
      end;
    vkRecord:
      begin
        Result := TParserValue.Create(LEmptyRecord);
      end;
  end;
end;

class function TParserValue.GetInf: TParserValue;
begin
  Result := TParserValue.Create(Double.PositiveInfinity);
end;

{$WARN NO_RETVAL OFF}
function TParserValue.GetIsEmpty: Boolean;
begin
  case Kind of
    vkDouble:
      begin
        Result := IsZero(AsDouble);
      end;
    vkString:
      begin
        Result := AsString.IsEmpty;
      end;
    vkArray, vkRecord:
      begin
        Result := Count = 0;
      end;
  end;
end;
{$WARN NO_RETVAL ON}

function TParserValue.GetIsNaN: Boolean;
begin
  Result := (Kind = vkDouble) and AsDouble.IsNan;
end;

function TParserValue.GetIsNegInf: Boolean;
begin
  Result := (Kind = vkDouble) and AsDouble.IsNegativeInfinity;
end;

function TParserValue.GetIsPosInf: Boolean;
begin
  Result := (Kind = vkDouble) and AsDouble.IsPositiveInfinity;
end;

function TParserValue.GetKind: TParserValueKind;
begin
  if FValue.IsType<IParserValueRefTarget>(False) then
  begin
    Result := vkReference;
  end else
  begin
    if FValue.IsType<Double>(False) then
    begin
      Result := vkDouble;
    end else
    begin
      if FValue.IsType<String>(False) then
      begin
        Result := vkString;
      end else
      begin
        if FValue.IsType<TArray<TParserValue>>(False) then
        begin
          Result := vkArray;
        end else
        begin
          if FValue.IsType<TArray<TPair<String, TParserValue>>>(False) then
          begin
            Result := vkRecord;
          end else
          begin
            raise EParserValueKindError.Create('Unknown value kind');
          end;
        end;
      end;
    end;
  end;
end;

class function TParserValue.GetMaxValue: TParserValue;
begin
  Result := TParserValue.Create(Double.MaxValue);
end;

{$WARN NO_RETVAL OFF}
function TParserValue.GetMembers(const AKey: TParserValue): TParserValue;
var
  LValue: TPair<String, TParserValue>;
begin
  if not HasMember(AKey) then
  begin
    raise EParserValueMemberError.CreateFmt('Member %s not found', [AKey.ToString.QuotedString]);
  end;
  case Kind of
    vkString:
      begin
        Result := TParserValue.Create(AsString.Chars[AKey.AsInteger]);
      end;
    vkArray:
      begin
        Result := AsArray[AKey.AsInteger];
      end;
    vkRecord:
      begin
        for LValue in AsRecord do
        begin
          if SameText(LValue.Key, AKey.AsString) then
          begin
            Exit(LValue.Value);
          end;
        end;
      end;
  end;
end;
{$WARN NO_RETVAL ON}

class function TParserValue.GetMinValue: TParserValue;
begin
  Result := TParserValue.Create(Double.MinValue);
end;

class function TParserValue.GetNaN: TParserValue;
begin
  Result := TParserValue.Create(Double.NaN);
end;

class function TParserValue.GetNewArray(const ALength: Integer): TParserValue;
var
  LValues: TArray<TParserValue>;
  LIndex: Integer;
begin
  SetLength(LValues, ALength);
  for LIndex := Low(LValues) to High(LValues) do
  begin
    LValues[LIndex] := Empty[vkDouble];
  end;
  Result := TParserValue.Create(LValues);
end;

class function TParserValue.GetNewRecord(const AFields: TArray<String>): TParserValue;
var
  LValues: TArray<TPair<String, TParserValue>>;
  LIndex: Integer;
begin
  SetLength(LValues, Length(AFields));
  for LIndex := Low(LValues) to High(LValues) do
  begin
    LValues[LIndex] := TPair<String, TParserValue>.Create(AFields[LIndex], Empty[vkDouble]);
  end;
  Result := TParserValue.Create(LValues);
end;

function TParserValue.HasMember(const AKey: TParserValue): Boolean;
var
  LValue: TPair<String, TParserValue>;
begin
  case Kind of
    vkString, vkArray:
      begin
        Result := AKey.IsInteger and InRange(AKey.AsDouble, 0, Pred(Count));
      end;
    vkRecord:
      begin
        for LValue in AsRecord do
        begin
          if SameText(LValue.Key, AKey.AsString) then
          begin
            Exit(True);
          end;
        end;
        Result := False;
      end
    else
      begin
        raise EParserValueMemberError.Create('Value does not support members');
      end;
  end;
end;

class function TParserValue.Multiply(const AFirst, ASecond: TParserValue): TParserValue;
begin

end;

function TParserValue.Negate: TParserValue;

  function NegateArray(const AElements: TArray<TParserValue>): TArray<TParserValue>;
  var
    LIndex: Integer;
  begin
    SetLength(Result, Length(AElements));
    for LIndex := Low(AElements) to High(AElements) do
    begin
      Result[LIndex] := AElements[LIndex].Negate;
    end;
  end;

  function NegateRecord(const AElements: TArray<TPair<String, TParserValue>>): TArray<TPair<String, TParserValue>>;
  var
    LIndex: Integer;
  begin
    SetLength(Result, Length(AElements));
    for LIndex := Low(AElements) to High(AElements) do
    begin
      Result[LIndex] := TPair<String, TParserValue>.Create(AElements[LIndex].Key, AElements[LIndex].Value.Negate);
    end;
  end;

begin
  case Kind of
    vkDouble:
      begin
        Result := TParserValue.Create(-AsDouble);
      end;
    vkArray:
      begin
        Result := TParserValue.Create(NegateArray(AsArray));
      end;
    vkRecord:
      begin
        Result := TParserValue.Create(NegateRecord(AsRecord));
      end;
    else
      begin
        raise EParserValueNegationError.CreateFmt('Negation not supported for: %s', [ToString.QuotedString]);
      end;
  end;
end;

procedure TParserValue.SetAsArray(const AValue: TArray<TParserValue>);
begin
  FValue := TValue.From<TArray<TParserValue>>(AValue);
end;

procedure TParserValue.SetAsDouble(const AValue: Double);
begin
  FValue := TValue.From<Double>(AValue);
end;

procedure TParserValue.SetAsRecord(const AValue: TArray<TPair<String, TParserValue>>);
var
  LFields: TDictionary<String, TParserValue>;
  LField: TPair<String, TParserValue>;
begin
  LFields := TDictionary<String, TParserValue>.Create(Length(AValue), TIStringComparer.Ordinal);
  try
    for LField in AValue do
    begin
      if LFields.ContainsKey(LField.Key) then
      begin
        raise EParserValueMemberError.CreateFmt('Duplicate record field: %s', [LField.Key.QuotedString]);
      end;
      LFields.Add(LField.Key, LField.Value);
    end;
  finally
    LFields.Free;
  end;
  FValue := TValue.From<TArray<TPair<String, TParserValue>>>(AValue);
end;

procedure TParserValue.SetAsReference(const AValue: IParserValueRefTarget);
begin
  FValue := TValue.From<IParserValueRefTarget>(AValue);
end;

procedure TParserValue.SetAsString(const AValue: String);
var
  LValue: String;
begin
  LValue := AValue;
  UniqueString(LValue);
  FValue := FValue.From<String>(LValue);
end;

class function TParserValue.Subtract(const AFirst, ASecond: TParserValue): TParserValue;

  function SubtractArrays(const AFirst, ASecond: TArray<TParserValue>): TArray<TParserValue>;
  begin

  end;

  function SubtractRecords(const AFirst, ASecond: TArray<TPair<String, TParserValue>>): TArray<TPair<String, TParserValue>>;
  begin

  end;

begin
  case AFirst.Kind of
    vkDouble:
      begin
        Result := TParserValue.Create(AFirst.AsDouble - ASecond.AsDouble);
      end;
    vkArray:
      begin
        Result := TParserValue.Create(SubtractArrays(AFirst.AsArray, ASecond.AsArray));
      end;
    vkRecord:
      begin
        Result := TParserValue.Create(SubtractRecords(AFirst.AsRecord, ASecond.AsRecord));
      end;
  end;
end;

function TParserValue.ToString: String;
begin
  Result := TParserCodeExporter.ValueToString(Self);
end;

constructor TParserValue.Create(const ARecord: TArray<TPair<String, TParserValue>>);
begin
  AsRecord := ARecord;
end;

constructor TParserValue.Create(const AReference: IParserValueRefTarget);
begin
  AsReference := AReference;
end;

class function TParserValue.Divide(const AFirst, ASecond: TParserValue): TParserValue;
begin

end;

{ TParserValueHelper }

constructor TParserValueHelper.Create(const AInteger: Int64);
begin
  AsInteger := AInteger;
end;

constructor TParserValueHelper.Create(const ABoolean: Boolean);
begin
  AsBoolean := ABoolean;
end;

function TParserValueHelper.GetAsBoolean: Boolean;
begin
  if not IsBoolean then
  begin
    raise EParserValueKindError.CreateFmt('Unsupported value: %s', [ToString.QuotedString]);
  end;
  Result := AsInteger.ToBoolean;
end;

function TParserValueHelper.GetAsInteger: Int64;
begin
  if not IsInteger then
  begin
    raise EParserValueKindError.CreateFmt('Unsupported value: %s', [ToString.QuotedString]);
  end;
  Result := Trunc(AsDouble);
end;

function TParserValueHelper.GetIsBoolean: Boolean;
begin
  Result := IsInteger and (AsInteger in [True.ToInteger, False.ToInteger]);
end;

function TParserValueHelper.GetIsInteger: Boolean;
begin
  Result := (Kind = vkDouble) and IsZero(Frac(AsDouble));
end;

procedure TParserValueHelper.SetAsBoolean(const AValue: Boolean);
begin
  AsDouble := AValue.ToInteger.ToDouble;
end;

procedure TParserValueHelper.SetAsInteger(const AValue: Int64);
begin
  AsDouble := AValue.ToDouble;
end;

{ TParserValuesHelper }

class function TParserValuesHelper.Create(const AString: String): TArray<TParserValue>;
var
  LIndex: Integer;
begin
  SetLength(Result, Length(AString));
  for LIndex := Low(AString) to High(AString) do
  begin
    Result[LIndex] := TParserValue.Create(AString[LIndex]);
  end;
end;

function TParserValuesHelper.GetAsDoubles: TArray<Double>;
var
  LIndex: Integer;
begin
  SetLength(Result, Length(Self));
  for LIndex := Low(Self) to High(Self) do
  begin
    Result[LIndex] := Self[LIndex].AsDouble;
  end;
end;

function TParserValuesHelper.GetAsStrings: TArray<String>;
var
  LIndex: Integer;
begin
  SetLength(Result, Length(Self));
  for LIndex := Low(Self) to High(Self) do
  begin
    Result[LIndex] := Self[LIndex].AsString;
  end;
end;

end.
