unit Parser.Exporter;

interface

uses
  System.SysUtils, System.Math, System.Classes, System.Generics.Collections,
  Parser.Syntax, Parser.Language, Parser.Dictionary, Parser.Exception, Parser.Value;

type
  TParserObjectKind = (okUnknown, okConstant, okVariable, okFunction, okType);

  TParserObjectKinds = set of TParserObjectKind;

  TParserObjectKindHelper = record helper for TParserObjectKind
  public
    constructor Create(const AObject: TParserObject);
  end;

  TParserFunctionKind = (fkFunction, fkConstructor);

  TParserTypeKind = (toUnknown, toAny, toRange, toEnum, toInt, toRangeInt, toStr, toRecord, toObject);

  TParserTypeKindHelper = record helper for TParserTypeKind
  public
    constructor Create(const AType: TParserType);
  end;

  TParserExporter = class abstract
  private
    FTarget: TStringList;
  protected
    class function ReferenceToString(const AReference: IParserValueRefTarget): String; virtual; abstract;
    class function DoubleToString(const ADouble: Double): String; virtual; abstract;
    class function StringToString(const AString: String): String; virtual; abstract;
    class function ArrayToString(const AArray: TArray<TParserValue>): String; virtual; abstract;
    class function RecordToString(const ARecord: TArray<TPair<String, TParserValue>>): String; virtual; abstract;
    procedure ExportAlias(const AAlias, AName: String); virtual; abstract;
    procedure ExportConstant(const AConstant: TParserConstant); virtual; abstract;
    procedure ExportVariable(const AVariable: TParserVariable); virtual; abstract;
    procedure ExportFunction(const AFunction: TParserFunction; const AFunctionKeyword: TParserFunctionKind = fkFunction); virtual; abstract;
    procedure ExportType(const AType: TParserType); virtual; abstract;
  public
    class function Title: String; virtual;
    class function CanExport(const AObject: TParserObject): Boolean; virtual;
    class function ValueToString(const AValue: TParserValue): String;
    property Target: TStringList read FTarget write FTarget;
    constructor Create(const ATarget: TStringList);
    procedure &Export(const AObject: TParserObject); overload;
    procedure &Export(const ADictionary: TParserDictionary; AName: String = String.Empty); overload;
  end;

  TParserCodeExporter = class(TParserExporter)
  private const
    FDeclPatternIntro = '%s %s';
    FDeclPatternValue = ' = %s';
    FDeclPatternType = '%s: %s';
    FDeclPatternParams = '(%s)';
  protected
    class function ReferenceToString(const AReference: IParserValueRefTarget): String; override;
    class function DoubleToString(const ADouble: Double): String; override;
    class function StringToString(const AString: String): String; override;
    class function ArrayToString(const AArray: TArray<TParserValue>): String; override;
    class function RecordToString(const ARecord: TArray<TPair<String, TParserValue>>): String; override;
    procedure ExportAlias(const AAlias, AName: String); override;
    procedure ExportConstant(const AConstant: TParserConstant); override;
    procedure ExportVariable(const AVariable: TParserVariable); override;
    procedure ExportFunction(const AFunction: TParserFunction; const AKind: TParserFunctionKind = fkFunction); override;
    procedure ExportType(const AType: TParserType); override;
  end;

implementation

{ TParserExportKindHelper }

constructor TParserObjectKindHelper.Create(const AObject: TParserObject);
begin
  if AObject is TParserFunction then
  begin
    Self := okFunction;
  end else
  begin
    if AObject is TParserVariable then
    begin
      Self := okVariable;
    end else
    begin
      if AObject is TParserConstant then
      begin
        Self := okConstant;
      end else
      begin
        if AObject is TParserType then
        begin
          Self := okType;
        end else
        begin
          Self := okUnknown;
        end;
      end;
    end;
  end;
end;

{ TParserTypeKindHelper }

constructor TParserTypeKindHelper.Create(const AType: TParserType);
begin
  if AType is TParserAnyType then
  begin
    Self := toAny;
  end else
  begin
    if AType is TParserRangeType then
    begin
      Self := toRange;
    end else
    begin
      if AType is TParserEnumType then
      begin
        Self := toEnum;
      end else
      begin
        if AType is TParserIntegerType then
        begin
          Self := toInt;
        end else
        begin
          if AType is TParserRangeIntegerType then
          begin
            Self := toRangeInt;
          end else
          begin
            if AType is TParserStringType then
            begin
              Self := toStr;
            end else
            begin
              if AType is TParserRecordType then
              begin
                Self := toRecord;
              end else
              begin
                if AType is TParserRecordType then
                begin
                  Self := toObject;
                end else
                begin
                  Self := toUnknown;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
  end;
end;

{ TParserExporter }

class function TParserExporter.CanExport(const AObject: TParserObject): Boolean;
begin
  Result := True;
end;

constructor TParserExporter.Create(const ATarget: TStringList);
begin
  inherited Create;
  Target := ATarget;
end;

procedure TParserExporter.Export(const ADictionary: TParserDictionary; AName: String);
var
  LObject: TParserObject;
begin
  if AName.IsEmpty then
  begin
    for LObject in ADictionary do
    begin
      &Export(LObject);
    end;
  end else
  begin
    &Export(ADictionary[AName]);
  end;
end;

class function TParserExporter.ValueToString(const AValue: TParserValue): String;
begin
  case AValue.Kind of
    vkReference:
      begin
        Result := ReferenceToString(AValue.AsReference);
      end;
    vkDouble:
      begin
        Result := DoubleToString(AValue.AsDouble);
      end;
    vkString:
      begin
        Result := StringToString(AValue.AsString);
      end;
    vkArray:
      begin
        Result := ArrayToString(AValue.AsArray);
      end;
    vkRecord:
      begin
        Result := RecordToString(AValue.AsRecord);
      end;
  end;
end;

class function TParserExporter.Title: String;
begin
  Result := String.Empty;
end;

procedure TParserExporter.Export(const AObject: TParserObject);
var
  LTitle: String;
begin
  if not CanExport(AObject) then
  begin
    raise EParserExportUnsupportedError.CreateFmt('%s not exportable', [AObject.Name.QuotedString]);
  end;
  case TParserObjectKind.Create(AObject) of
    okUnknown:
      begin
        LTitle := Title;
        if LTitle.IsEmpty then
        begin
          raise EParserExportUnsupportedError.CreateFmt('%s not exportable', [AObject.Name.QuotedString]);
        end;
        raise EParserExportUnsupportedError.CreateFmt('%s not exportable as %s', [AObject.Name.QuotedString, LTitle]);
      end;
    okConstant:
      begin
        ExportConstant(AObject as TParserConstant);
      end;
    okVariable:
      begin
        ExportVariable(AObject as TParserVariable);
      end;
    okFunction:
      begin
        ExportFunction(AObject as TParserFunction);
      end;
    okType:
      begin
        ExportType(AObject as TParserType);
      end;
  end;
end;

{ TParserCodeExporter }

class function TParserCodeExporter.ArrayToString(const AArray: TArray<TParserValue>): String;
const
  LSeparator = ', ';
  LBrackets = '[%s]';
var
  LValues: TStringList;
  LValue: TParserValue;
begin
  LValues := TStringList.Create;
  try
    for LValue in AArray do
    begin
      LValues.Add(LValue.ToString);
    end;
    Result := String.Format(LBrackets, [String.Join(LSeparator, LValues.ToStringArray)]);
  finally
    LValues.Free;
  end;
end;

class function TParserCodeExporter.DoubleToString(const ADouble: Double): String;
const
  LValueNaN = 'NaN';
  LValueInfPos = 'Inf';
  LValueInfNeg = Concat('-', LValueInfPos);
var
  LValue: TParserValue;
begin
  LValue := TParserValue.Create(ADouble);
  if LValue.IsNan then
  begin
    Result := LValueNaN;
  end else
  begin
    if LValue.IsNegInf then
    begin
      Result := LValueInfNeg;
    end else
    begin
      if LValue.IsPosInf then
      begin
        Result := LValueInfPos;
      end else
      begin
        Result := ADouble.ToString(TFormatSettings.Invariant);
      end;
    end;
  end;
end;

procedure TParserCodeExporter.ExportAlias(const AAlias, AName: String);
var
  LBuilder: TStringBuilder;
begin
  LBuilder := TStringBuilder.Create;
  try
    LBuilder.AppendFormat(FDeclPatternIntro, [kwAlias.ToString, AAlias]);
    LBuilder.AppendFormat(FDeclPatternValue, [AName]);
    Target.Add(LBuilder.ToString);
  finally
    LBuilder.Free;
  end;
end;

procedure TParserCodeExporter.ExportConstant(const AConstant: TParserConstant);
var
  LBuilder: TStringBuilder;
begin
  LBuilder := TStringBuilder.Create;
  try
    LBuilder.AppendFormat(FDeclPatternIntro, [kwConstant.ToString, AConstant.Name]);
    LBuilder.AppendFormat(FDeclPatternValue, [ValueToString(AConstant.Value[[]])]);
    Target.Add(LBuilder.ToString);
  finally
    LBuilder.Free;
  end;
end;

procedure TParserCodeExporter.ExportFunction(const AFunction: TParserFunction; const AKind: TParserFunctionKind = fkFunction);
const
  LKeywords: array [TParserFunctionKind] of String = ('function', 'constructor');
  LBuiltInBody = '<built-in>';
var
  LIndex: Integer;
  LParams: TStringList;
  LBuilder: TStringBuilder;
  LBody: String;
begin
  LParams := TStringList.Create;
  try
    for LIndex := 0 to Pred(AFunction.ParamCount) do
    begin
      if Assigned(AFunction.ArgTypes[LIndex]) then
      begin
        LParams.Add(String.Format(FDeclPatternType, [AFunction.ParamNames[LIndex], (AFunction.ArgTypes[LIndex] as TParserType).Name]));
      end else
      begin
        LParams.Add(AFunction.ParamNames[LIndex]);
      end;
    end;
    LBuilder := TStringBuilder.Create;
    try
      LBuilder.AppendFormat(FDeclPatternIntro, [LKeywords[AKind], AFunction.Name]);
      if LParams.Count <> 0 then
      begin
        LBuilder.AppendFormat(FDeclPatternParams, [String.Join(', ', LParams.ToStringArray)]);
      end;
      if AFunction is TParserCustomFunction then
      begin
        // ToDo: Export syntax tree
      end else
      begin
        LBody := LBuiltInBody;
      end;
      LBuilder.AppendFormat(FDeclPatternValue, [LBody]);
      Target.Add(LBuilder.ToString);
    finally
      LBuilder.Free;
    end;
  finally
    LParams.Free;
  end;
end;

procedure TParserCodeExporter.ExportType(const AType: TParserType);
const
  LTypeKindNames: array [TParserTypeKind] of String = (String.Empty, 'any', 'range', 'enum', 'int', 'rangeint', 'str', 'record', 'object');
var
  LKind: TParserTypeKind;
  LArgs: TStringList;
  LIndex: Integer;
  LBuilder: TStringBuilder;
begin
  LKind := TParserTypeKind.Create(AType);
  LArgs := TStringList.Create;
  try
    case LKind of
      toAny:
        begin

        end;
      toRange:
        begin
          LArgs.Add(ValueToString((AType as TParserRangeType).MinValue));
          LArgs.Add(ValueToString((AType as TParserRangeType).MaxValue));
        end;
      toEnum:
        begin
          for LIndex := 0 to Pred((AType as TParserEnumType).ValueCount) do
          begin
            LArgs.Add(ValueToString((AType as TParserEnumType)[LIndex]));
          end;
        end;
      toInt:
        begin
          for LIndex := 0 to Pred((AType as TParserIntegerType).DivisorCount) do
          begin
            LArgs.Add(ValueToString((AType as TParserIntegerType)[LIndex]));
          end;
        end;
      toRangeInt:
        begin
          LArgs.Add(ValueToString((AType as TParserRangeIntegerType).Range.MinValue));
          LArgs.Add(ValueToString((AType as TParserRangeIntegerType).Range.MaxValue));
          for LIndex := 0 to Pred((AType as TParserRangeIntegerType).Integer.DivisorCount) do
          begin
            LArgs.Add(ValueToString((AType as TParserRangeIntegerType).Integer[LIndex]));
          end;
        end;
      toStr:
        begin
//          LDivisor := (AType as TParserStringType).Length;
//          if not SameValue(LDivisor.AsDouble, 0) then
//          begin
//            LArgs.Add(ValueToString(LDivisor));
//          end;
        end;
      toRecord:
        begin
          for LIndex := 0 to Pred((AType as TParserRecordType).FieldCount) do
          begin
            LArgs.Add((AType as TParserRecordType).Fields[LIndex].AsString);
          end;
        end;
    end;
    LBuilder := TStringBuilder.Create;
    try
      LBuilder.AppendFormat(FDeclPatternIntro, [kwType.ToString, AType.Name]);
      LBuilder.AppendFormat(FDeclPatternValue, [LTypeKindNames[LKind]]);
      if LArgs.Count <> 0 then
      begin
        LBuilder.AppendFormat(FDeclPatternParams, [String.Join(', ', LArgs.ToStringArray)]);
      end;
      Target.Add(LBuilder.ToString);
      if Assigned(AType.&Constructor) then
      begin
        ExportFunction(AType.&Constructor, fkConstructor);
      end;
    finally
      LBuilder.Free;
    end;
  finally
    LArgs.Free;
  end;
end;


procedure TParserCodeExporter.ExportVariable(const AVariable: TParserVariable);
var
  LValue: TParserValue;
  LBuilder: TStringBuilder;
begin
  LValue :=  AVariable.Value[[]];
  LBuilder := TStringBuilder.Create;
  try
    LBuilder.AppendFormat(FDeclPatternIntro, [kwVariable.ToString, AVariable.Name]);
    if not LValue.IsEmpty then
    begin
      LBuilder.AppendFormat(FDeclPatternValue, [ValueToString(LValue)]);
    end;
    Target.Add(LBuilder.ToString);
  finally
    LBuilder.Free;
  end;
end;

class function TParserCodeExporter.RecordToString(const ARecord: TArray<TPair<String, TParserValue>>): String;
const
  LField = '%s = %s';
  LSeparator = ', ';
  LBraces = '{%s}';
var
  LValues: TStringList;
  LValue: TPair<String, TParserValue>;
begin
  LValues := TStringList.Create;
  try
    for LValue in ARecord do
    begin
      LValues.Add(String.Format(LField, [LValue.Key, LValue.Value.ToString]));
    end;
    Result := String.Format(LBraces, [String.Join(LSeparator, LValues.ToStringArray)]);
  finally
    LValues.Free;
  end;
end;

class function TParserCodeExporter.ReferenceToString(const AReference: IParserValueRefTarget): String;
begin
  if Assigned(AReference) then
  begin
    if AReference is TParserObject then
    begin
      Result := '@' + AReference.Name;
    end;
  end;
end;

class function TParserCodeExporter.StringToString(const AString: String): String;
const
  LQuotations = '"%s"';
begin
  Result := String.Format(LQuotations, [AString]);
end;

end.
