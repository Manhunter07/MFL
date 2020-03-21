unit Parser;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Defaults, System.Generics.Collections, System.Rtti,
  Parser.Dictionary, Parser.Exception, Parser.Language, Parser.Lexer, Parser.Package, Parser.Syntax, Parser.Exporter;

type
  TParserExpressionKind = (exEmpty, exTerm, exResolution, exDeclConstant, exDeclVariable, exDeclFunction, exDeclAlias, exShow, exDeletion);

  TParserExpressionKindHelper = record helper for TParserExpressionKind
  public
    constructor Create(const AFirstToken: TParserToken);
  end;

  TParserValueKind = (vkNone, vkDouble, vkString, vkStringArray);

  TParserValue = record
  private
    FValue: TValue;
    function GetKind: TParserValueKind;
    function GetAsDouble: Double;
    function GetAsString: String;
    function GetAsStringArray: TArray<String>;
  public
    property Kind: TParserValueKind read GetKind;
    property AsDouble: Double read GetAsDouble;
    property AsString: String read GetAsString;
    property AsStringArray: TArray<String> read GetAsStringArray;
    constructor Create(const AValue: Double); overload;
    constructor Create(const AValue: String); overload;
    constructor Create(const AValue: TArray<String>); overload;
    function ToString(const APretty: Boolean = False): String;
  end;

  TParserResponse = record
  private
    FWarnings: TArray<String>;
    FExpressionKind: TParserExpressionKind;
    FReturnValue: TParserValue;
  public
    property Warnings: TArray<String> read FWarnings;
    property ExpressionKind: TParserExpressionKind read FExpressionKind;
    property ReturnValue: TParserValue read FReturnValue;
  end;

  TParser = class;

  TParserNameData = record
  private
    FRelativeName: String;
    FPackageName: String;
    FDictionary: TParserDictionary;
    function GetAbsoluteName: String;
  public
    property RelativeName: String read FRelativeName;
    property PackageName: String read FPackageName;
    property Dictionary: TParserDictionary read FDictionary;
    property AbsoluteName: String read GetAbsoluteName;
    constructor Create(const AParser: TParser; const AName: String);
  end;

  TParser = class(TSingletonImplementation, IParserValueSupplier)
  private
    FDictionary: TParserDictionary;
    FPackages: TObjectDictionary<String, TParserPackage>;
    FOrderedPackageNames: TStringList;
    function GetValues(const AName: String; const AArgs: TArray<Double>): Double;
    function GetMinArgCount(const AName: String): Integer;
    function GetMaxArgCount(const AName: String): Integer;
    function GetPackages(const AName: String): TParserPackage;
    function GetPackageCount: Integer;
  protected
    property Values[const AName: String; const AArgs: TArray<Double>]: Double read GetValues;
    property MinArgCount[const AName: String]: Integer read GetMinArgCount;
    property MaxArgCount[const AName: String]: Integer read GetMaxArgCount;
    function ContainsValue(const AName: String): Boolean;
  public
    property Dictionary: TParserDictionary read FDictionary;
    property Packages[const AName: String]: TParserPackage read GetPackages;
    property PackageCount: Integer read GetPackageCount;
    constructor Create;
    destructor Destroy; override;
    function Evaluate(const AExpression: String): TParserResponse; virtual;
    procedure RegisterPackage(const APackage: TParserPackage);
    procedure UnregisterPackage(const AName: String);
    function HasPackage(const AName: String): Boolean;
  end;

implementation

{ TParserExpressionKindHelper }

constructor TParserExpressionKindHelper.Create(const AFirstToken: TParserToken);
begin
  case AFirstToken.Kind of
    tkEnd:
      begin
        Self := exEmpty;
      end;
    tkName:
      begin
        case TParserKeyword.Create(AFirstToken.Name) of
          kwResolve:
            begin
              Self := exResolution;
            end;
          kwConstant:
            begin
              Self := exDeclConstant;
            end;
          kwVariable:
            begin
              Self := exDeclVariable;
            end;
          kwFunction:
            begin
              Self := exDeclFunction;
            end;
          kwAlias:
            begin
              Self := exDeclAlias;
            end;
          kwShow:
            begin
              Self := exShow;
            end;
          kwDelete:
            begin
              Self := exDeletion;
            end
          else
            begin
              Self := exTerm;
            end;
        end;
      end;
    else
      begin
        Self := exTerm;
      end;
  end;
end;

{ TParserValue }

constructor TParserValue.Create(const AValue: TArray<String>);
begin
  FValue := TValue.From<TArray<String>>(AValue);
end;

constructor TParserValue.Create(const AValue: String);
begin
  FValue := TValue.From<String>(AValue);
end;

constructor TParserValue.Create(const AValue: Double);
begin
  FValue := TValue.From<Double>(AValue);
end;

function TParserValue.GetAsDouble: Double;
begin
  Result := FValue.AsType<Double>;
end;

function TParserValue.GetAsString: String;
begin
  Result := FValue.AsType<String>;
end;

function TParserValue.GetAsStringArray: TArray<String>;
begin
  Result := FValue.AsType<TArray<String>>;
end;

function TParserValue.GetKind: TParserValueKind;
begin
  if FValue.IsType<Double> then
  begin
    Result := vkDouble;
  end else
  begin
    if FValue.IsType<String> then
    begin
      Result := vkString;
    end else
    begin
      if FValue.IsType<TArray<String>> then
      begin
        Result := vkStringArray;
      end else
      begin
        Result := Default(TParserValueKind);
      end;
    end;
  end;
end;

function TParserValue.ToString(const APretty: Boolean): String;
begin
  case Kind of
    vkNone:
      begin
        Result := String.Empty;
      end;
    vkDouble:
      begin
        if AsDouble.IsNan then
        begin
          if APretty then
          begin
            Result := 'Invalid number';
          end else
          begin
            Result := 'NaN';
          end;
        end else
        begin
          if AsDouble.IsNegativeInfinity then
          begin
            if APretty then
            begin
              Result := 'Negative infinity';
            end else
            begin
              Result := '-Inf';
            end;
          end else
          begin
            if AsDouble.IsPositiveInfinity then
            begin
              if APretty then
              begin
                Result := 'Positive infinity';
              end else
              begin
                Result := 'Inf';
              end;
            end else
            begin
              Result := AsDouble.ToString(TFormatSettings.Invariant);
            end;
          end;
        end;
      end;
    vkString:
      begin
        Result := AsString;
      end;
    vkStringArray:
      begin
        Result := String.Join(sLineBreak, AsStringArray);
      end;
  end;
end;

{ TParserNameData }

constructor TParserNameData.Create(const AParser: TParser; const AName: String);
var
  LNamePath: TArray<String>;
  LPackage: TParserPackage;
  LPackageIndex: Integer;
begin
  LNamePath := AName.Split(['.']);
  FRelativeName := LNamePath[High(LNamePath)];
  if Length(LNamePath) > 1 then
  begin
    FPackageName := LNamePath[Low(LNamePath)];
    if AParser.FPackages.TryGetValue(PackageName, LPackage) then
    begin
      FPackageName := LPackage.Name;
      FDictionary := LPackage.Dictionary;
    end else
    begin
      FDictionary := Default(TParserDictionary);
    end;
    Dictionary.Dealias(FRelativeName);
    Dictionary.Prettify(FRelativeName);
  end else
  begin
    if not AParser.Dictionary.Contains(FRelativeName) then
    begin
      for LPackageIndex := Pred(AParser.PackageCount) downto 0 do
      begin
        LPackage := AParser.Packages[AParser.FOrderedPackageNames[LPackageIndex]];
        if not LPackage.Explicit and LPackage.Dictionary.Contains(RelativeName) then
        begin
          FPackageName := LPackage.Name;
          FDictionary := LPackage.Dictionary;
          Dictionary.Dealias(FRelativeName);
          Dictionary.Prettify(FRelativeName);
          Exit;
        end;
      end;
    end;
    FPackageName := String.Empty;
    FDictionary := AParser.Dictionary;
    if Dictionary.Dealias(FRelativeName) then
    begin
      Create(AParser, RelativeName);
    end;
    Dictionary.Prettify(FRelativeName);
  end;
end;

function TParserNameData.GetAbsoluteName: String;
begin
  Result := RelativeName;
  if not PackageName.IsEmpty then
  begin
    Result := String.Join('.', [PackageName, Result]);
  end;
end;

{ TParser }

function TParser.ContainsValue(const AName: String): Boolean;
var
  LNameData: TParserNameData;
begin
  LNameData := TParserNameData.Create(Self, AName);
  Result := Assigned(LNameData.Dictionary) and LNameData.Dictionary.Contains(LNameData.RelativeName);
end;

constructor TParser.Create;
begin
  inherited;
  FDictionary := TParserDictionary.Create;
  FPackages := TObjectDictionary<String, TParserPackage>.Create([], TIStringComparer.Ordinal);
  FOrderedPackageNames := TStringList.Create(dupError, False, False);
  RegisterPackage(TParserPackage.DefaultPackage);
end;

destructor TParser.Destroy;
begin
  UnregisterPackage(TParserPackage.DefaultPackage.Name);
  FOrderedPackageNames.Free;
  FPackages.Free;
  Dictionary.Free;
  inherited;
end;

function TParser.Evaluate(const AExpression: String): TParserResponse;
var
  LWarnings: TStringList;
  LLexer: TParserLexer;
  LToken: TParserToken;

  procedure ExpectToken(const AAllowedKinds: TParserTokenKinds = []; const APreview: Boolean = False);
  begin
    LToken := LLexer.NextToken[APreview];
    if (AAllowedKinds <> []) and not (LToken.Kind in AAllowedKinds) then
    begin
      raise EParserTokenUnexpectedError.CreateFmt('Unexpected token at %d: %s', [LToken.CharIndex, LToken.ToString.QuotedString]);
    end;
  end;

  procedure EvaluateExpression;
  var
    LSyntaxTree: TParserTree;
    LObjectName: String;
    LObjectNames: TStringList;
    LInitialValue: Double;
    LFunct: TParserCustomFunction;
    LFunctParams: TStringList;
    LExportTarget: TStringList;
    LExporter: TParserCodeExporter;
    LNameData: TParserNameData;

    procedure BuildTree;
    var
      LNode: TParserParentNode;

      procedure BuildParentNode(const AFunctParams: Boolean = False);

        procedure BuildSubNode;
        var
          LOperator: TParserOperator;
          LNewNode: TParserParentNode;
        begin
          if LToken.Kind = tkSymbolOp then
          begin
            LOperator := LToken.&Operator;
            ExpectToken([tkSymbolParOp, tkNumberDec, tkNumberHex, tkName]);
          end else
          begin
            LOperator := opAdd;
          end;
          case LToken.Kind of
            tkSymbolParOp:
              begin
                LNode := TParserParentNode.Create(LNode, LOperator);
                ExpectToken([tkEnd, tkSymbolOp, tkSymbolParOp, tkNumberDec, tkNumberHex, tkName]);
                BuildParentNode;
              end;
            tkNumberDec:
              begin
                TParserValueNode.Create(LNode, LOperator, LToken.NumberDec);
                ExpectToken([tkEnd, tkSymbolOp, tkSymbolComma, tkSymbolParCl]);
              end;
            tkNumberHex:
              begin
                TParserValueNode.Create(LNode, LOperator, LToken.NumberHex);
                ExpectToken([tkEnd, tkSymbolOp, tkSymbolComma, tkSymbolParCl]);
              end;
            tkName:
              begin
                LNewNode := TParserNamedNode.Create(LNode, LOperator, LToken.Name);
                ExpectToken([tkEnd, tkSymbolOp, tkSymbolParOp, tkSymbolComma, tkSymbolParCl]);
                if LToken.Kind = tkSymbolParOp then
                begin
                  LNode := LNewNode;
                  ExpectToken([tkSymbolOp, tkSymbolParOp, tkSymbolParCl, tkNumberDec, tkNumberHex, tkName]);
                  BuildParentNode(True);
                end;
              end;
          end;
        end;

      begin
        if (LToken.Kind = tkSymbolOp) and not (LToken.&Operator in TParserOperator.UnaryOperators) then
        begin
          raise EParserOperatorError.CreateFmt('Invalid prefix: %s', [String(LToken.&Operator.ToChar).QuotedString]);
        end;
        if AFunctParams then
        begin
          LNode := TParserArgNode.Create(LNode);
        end;
        repeat
          case LToken.Kind of
            tkSymbolParCl:
              begin
                if not Assigned(LNode.Parent) or (AFunctParams and not Assigned(LNode.Parent.Parent)) then
                begin
                  raise EParserParenthesisError.CreateFmt('Unexpected parenthesis at %d', [LToken.CharIndex]);
                end;
                LNode := LNode.Parent;
                if AFunctParams then
                begin
                  LNode := LNode.Parent;
                end;
                ExpectToken([tkEnd, tkSymbolOp, tkSymbolComma, tkSymbolParCl]);
                Exit;
              end;
            tkSymbolComma:
              begin
                if not AFunctParams then
                begin
                  raise EParserCommaError.CreateFmt('Unexpected parameter separator at %d', [LToken.CharIndex]);
                end;
                LNode := TParserArgNode.Create(LNode.Parent);
                ExpectToken([tkSymbolOp, tkSymbolParOp, tkNumberDec, tkNumberHex, tkName]);
                //Exit;
              end;
            else
              begin
                BuildSubNode;
              end;
          end;
        until LToken.Kind = tkEnd;
        if Assigned(LNode.Parent) then
        begin
          raise EParserParenthesisError.Create('Missing parenthesis');
        end;
      end;

    begin
      LNode := LSyntaxTree.Nodes;
      BuildParentNode;
    end;

    procedure ParseObjectNames(const AAllowedFollowTokens: TParserTokenKinds);
    var
      LDealiasedName: String;
    begin
      repeat
        ExpectToken([tkName]);
        LDealiasedName := LToken.Name;
        if Dictionary.Dealias(LDealiasedName) then
        begin
          LWarnings.Add(String.Format('%s resolved as an alias for %s', [String(LToken.Name).QuotedString, LDealiasedName.QuotedString]));
        end;
        LObjectNames.Add(LDealiasedName);
        ExpectToken(AAllowedFollowTokens + [tkSymbolComma]);
      until LToken.Kind <> tkSymbolComma;
    end;

  begin
    case Result.ExpressionKind of
      exTerm:
        begin
          LSyntaxTree := TParserTree.Create([Self]);
          try
            BuildTree;
            Result.FReturnValue := TParserValue.Create(LSyntaxTree.Calculate);
          finally
            LSyntaxTree.Free;
          end;
        end;
      exResolution:
        begin
          ExpectToken([tkName]);
          LObjectName := LToken.Name;
          Result.FReturnValue := TParserValue.Create(TParserNameData.Create(Self, LObjectName).AbsoluteName);
          ExpectToken([tkEnd]);
        end;
      exDeclConstant:
        begin
          LObjectNames := TStringList.Create;
          try
            ParseObjectNames([tkSymbolEq]);
            ExpectToken([tkSymbolOp, tkSymbolParOp, tkSymbolParCl, tkNumberDec, tkNumberHex, tkName]);
            LSyntaxTree := TParserTree.Create([Self]);
            try
              BuildTree;
              LInitialValue := LSyntaxTree.Calculate;
              for LObjectName in LObjectNames do
              begin
                Dictionary.Add(TParserConstant.Create(LObjectName, LInitialValue));
              end;
            finally
              LSyntaxTree.Free;
            end;
          finally
            LObjectNames.Free;
          end;
        end;
      exDeclVariable:
        begin
          LObjectNames := TStringList.Create;
          try
            ParseObjectNames([tkEnd, tkSymbolEq]);
            if LToken.Kind = tkSymbolEq then
            begin
              ExpectToken([tkSymbolOp, tkSymbolParOp, tkSymbolParCl, tkNumberDec, tkNumberHex, tkName]);
              LSyntaxTree := TParserTree.Create([Self]);
              try
                BuildTree;
                LInitialValue := LSyntaxTree.Calculate;
              finally
                LSyntaxTree.Free;
              end;
            end else
            begin
              LInitialValue := Default(Double);
            end;
            for LObjectName in LObjectNames do
            begin
              Dictionary.Add(TParserVariable.Create(LObjectName, LInitialValue));
            end;
          finally
            LObjectNames.Free;
          end;
        end;
      exDeclFunction:
        begin
          ExpectToken([tkName]);
          LObjectName := LToken.Name;
          ExpectToken([tkSymbolEq, tkSymbolParOp]);
          LFunctParams := TStringList.Create{(dupError, False, False)}; // Duplicate check is done at TParserFunction.Create(...)
          try
            if LToken.Kind = tkSymbolParOp then
            begin
              repeat
                ExpectToken([tkSymbolParCl, tkName]);
                if LToken.Kind = tkName then
                begin
                  LFunctParams.Add(LToken.Name);
                  ExpectToken([tkSymbolComma, tkSymbolParCl]);
                end;
              until LToken.Kind = tkSymbolParCl;
              ExpectToken([tkSymbolEq]);
            end;
            ExpectToken([tkSymbolOp, tkSymbolParOp, tkSymbolParCl, tkNumberDec, tkNumberHex, tkName]);
            LFunct := TParserCustomFunction.Create(LObjectName, LFunctParams.ToStringArray);
            LSyntaxTree := TParserTree.Create([LFunct, Self]);
            try
              BuildTree;
            finally
              try
                LFunct.SyntaxTree := LSyntaxTree;
                Dictionary.Add(LFunct);
              except
                LSyntaxTree.Free;
              end;
            end;
          finally
            LFunctParams.Free;
          end;
        end;
      exDeclAlias:
        begin
          LObjectNames := TStringList.Create;
          try
            ParseObjectNames([tkSymbolEq]);
            ExpectToken([tkName]);
            for LObjectName in LObjectNames do
            begin
              Dictionary.AddAlias(LObjectName, LToken.Name);
            end;
          finally
            LObjectNames.Free;
          end;
        end;
      exShow:
        begin
          ExpectToken([tkName]);
          LObjectName := LToken.Name;
          LExportTarget := TStringList.Create;
          LExporter := TParserCodeExporter.Create(LExportTarget);
          try
            LNameData := TParserNameData.Create(Self, LObjectName);
            LExporter.Export(LNameData.Dictionary, LNameData.RelativeName);
            Result.FReturnValue := TParserValue.Create(LExportTarget.ToStringArray);
          finally
            LExporter.Free;
            LExportTarget.Free;
          end;
          ExpectToken([tkEnd]);
        end;
      exDeletion:
        begin
          LObjectNames := TStringList.Create;
          try
            ParseObjectNames([tkEnd]);
            for LObjectName in LObjectNames do
            begin
              if not TParserObject.ValidName(LObjectName) then
              begin
                raise EParserObjectNameError.CreateFmt('Invalid name: %s', [LObjectName.QuotedString]);
              end;
              Dictionary.Remove(LObjectName);
            end;
          finally
            LObjectNames.Free;
          end;
        end;
    end;
  end;

begin
  LWarnings := TStringList.Create;
  LLexer := TParserLexer.Create(AExpression);
  try
    ExpectToken([tkEnd, tkSymbolOp, tkSymbolParOp, tkSymbolParCl, tkNumberDec, tkNumberHex, tkName]);
    Result.FExpressionKind := TParserExpressionKind.Create(LToken);
    EvaluateExpression;
    Result.FWarnings := LWarnings.ToStringArray;
  finally
    LLexer.Free;
    LWarnings.Free;
  end;
end;

function TParser.GetMaxArgCount(const AName: String): Integer;
var
  LNameData: TParserNameData;
begin
  LNameData := TParserNameData.Create(Self, AName);
  if not Assigned(LNameData.Dictionary) then
  begin
    raise EParserUnknownPackageError.CreateFmt('Unknown package: %s', [LNameData.PackageName.QuotedString]);
  end;
  Result := LNameData.Dictionary[LNameData.RelativeName].MaxArgCount;
end;

function TParser.GetMinArgCount(const AName: String): Integer;
var
  LNameData: TParserNameData;
begin
  LNameData := TParserNameData.Create(Self, AName);
  if not Assigned(LNameData.Dictionary) then
  begin
    raise EParserUnknownPackageError.CreateFmt('Unknown package: %s', [LNameData.PackageName.QuotedString]);
  end;
  Result := LNameData.Dictionary[LNameData.RelativeName].MinArgCount;
end;

function TParser.GetPackageCount: Integer;
begin
  Result := FPackages.Count;
end;

function TParser.GetPackages(const AName: String): TParserPackage;
begin
  if not HasPackage(AName) then
  begin
    raise EParserUnknownPackageError.CreateFmt('Unknown package: %s', [AName.QuotedString]);
  end;
  Result := FPackages[AName];
end;

function TParser.GetValues(const AName: String; const AArgs: TArray<Double>): Double;
var
  LNameData: TParserNameData;
begin
  LNameData := TParserNameData.Create(Self, AName);
  if not Assigned(LNameData.Dictionary) then
  begin
    raise EParserUnknownPackageError.CreateFmt('Unknown package: %s', [LNameData.PackageName.QuotedString]);
  end;
  Result := (LNameData.Dictionary[LNameData.RelativeName] as TParserValueObject).Value[AArgs];
end;

function TParser.HasPackage(const AName: String): Boolean;
begin
  Result := FPackages.ContainsKey(AName);
end;

procedure TParser.RegisterPackage(const APackage: TParserPackage);
begin
  FPackages.Add(APackage.Name, APackage);
  FOrderedPackageNames.Add(APackage.Name);
end;

procedure TParser.UnregisterPackage(const AName: String);
begin
  FOrderedPackageNames.Delete(FOrderedPackageNames.IndexOf(AName));
  FPackages.Remove(AName);
end;

end.
