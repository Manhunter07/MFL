unit Parser;

interface

uses
  System.SysUtils, System.Classes, System.Generics.Defaults, System.Generics.Collections,
  Parser.Dictionary, Parser.Exception, Parser.Language, Parser.Lexer, Parser.Package, Parser.Syntax;

type
  TParserExpressionKind = (exEmpty, exTerm, exResolution, exDeclConstant, exDeclVariable, exDeclFunction, exDeclAlias, exDeletion);

  TParserResponse = record
    Warnings: TArray<String>;
    case ExpressionKind: TParserExpressionKind of
      exTerm: (
        Value: Double;
      );
      exResolution: (
        Name: TParserNameChars;
      );
  end;

  TParserNameData = record
    RelativeName: String;
    PackageName: String;
    Dictionary: TParserDictionary;
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
    function NameData(const AName: String): TParserNameData; virtual;
    function AbsoluteName(const AName: String): String; virtual;
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

{ TParser }

function TParser.AbsoluteName(const AName: String): String;
var
  LNameData: TParserNameData;
begin
  LNameData := NameData(AName);
  if not LNameData.Dictionary.Contains(LNameData.RelativeName) then
  begin
    raise EParserUnknownError.CreateFmt('Undeclared identifier: %s', [AName.QuotedString]);
  end;
  Result := LNameData.RelativeName;
  if not LNameData.PackageName.IsEmpty then
  begin
    Result := String.Join('.', [LNameData.PackageName, Result]);
  end;
end;

function TParser.ContainsValue(const AName: String): Boolean;
var
  LNameData: TParserNameData;
begin
  LNameData := NameData(AName);
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

  procedure DetermineKind;
  begin
    case LToken.Kind of
      tkEnd:
        begin
          Result.ExpressionKind := exEmpty;
        end;
      tkName:
        begin
          case TParserKeyword.Create(LToken.Name) of
            kwResolve:
              begin
                Result.ExpressionKind := exResolution;
              end;
            kwConstant:
              begin
                Result.ExpressionKind := exDeclConstant;
              end;
            kwVariable:
              begin
                Result.ExpressionKind := exDeclVariable;
              end;
            kwFunction:
              begin
                Result.ExpressionKind := exDeclFunction;
              end;
            kwAlias:
              begin
                Result.ExpressionKind := exDeclAlias;
              end;
            kwDelete:
              begin
                Result.ExpressionKind := exDeletion;
              end
            else
              begin
                Result.ExpressionKind := exTerm;
              end;
          end;
        end;
      else
        begin
          Result.ExpressionKind := exTerm;
        end;
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

    function AbsoluteNameChars(const AName: String): TParserNameChars;
    var
      LAbsoluteName: String;
      LCharIndex: Integer;
      LChar: Char;
    begin
      LAbsoluteName := AbsoluteName(AName);
      LCharIndex := Low(TParserNameChars);
      for LChar in LAbsoluteName do
      begin
        if LCharIndex > High(TParserNameChars) then
        begin
          LWarnings.Add('Resolved name exceeds the maximum length for display')
        end;
        Result[LCharIndex] := LChar;
        Inc(LCharIndex);
      end;
      FillChar(Result[LCharIndex], Length(Result) - LCharIndex, #0);
    end;

  begin
    case Result.ExpressionKind of
      exTerm:
        begin
          LSyntaxTree := TParserTree.Create([Self]);
          try
            BuildTree;
            Result.Value := LSyntaxTree.Calculate;
          finally
            LSyntaxTree.Free;
          end;
        end;
      exResolution:
        begin
          ExpectToken([tkName]);
          LObjectName := LToken.Name;
          Result.Name := AbsoluteNameChars(LObjectName);
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
    DetermineKind;
    EvaluateExpression;
    Result.Warnings := LWarnings.ToStringArray;
  finally
    LLexer.Free;
    LWarnings.Free;
  end;
end;

function TParser.GetMaxArgCount(const AName: String): Integer;
var
  LNameData: TParserNameData;
begin
  LNameData := NameData(AName);
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
  LNameData := NameData(AName);
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
  LNameData := NameData(AName);
  if not Assigned(LNameData.Dictionary) then
  begin
    raise EParserUnknownPackageError.CreateFmt('Unknown package: %s', [LNameData.PackageName.QuotedString]);
  end;
  Result := LNameData.Dictionary[LNameData.RelativeName].Value[AArgs];
end;

function TParser.HasPackage(const AName: String): Boolean;
begin
  Result := FPackages.ContainsKey(AName);
end;

function TParser.NameData(const AName: String): TParserNameData;
var
  LNamePath: TArray<String>;
  LPackage: TParserPackage;
  LPackageIndex: Integer;
begin
  LNamePath := AName.Split(['.']);
  Result.RelativeName := LNamePath[High(LNamePath)];
  if Length(LNamePath) > 1 then
  begin
    Result.PackageName := LNamePath[Low(LNamePath)];
    if FPackages.TryGetValue(Result.PackageName, LPackage) then
    begin
      Result.PackageName := LPackage.Name;
      Result.Dictionary := LPackage.Dictionary;
    end else
    begin
      Result.Dictionary := Default(TParserDictionary);
    end;
    Result.Dictionary.Dealias(Result.RelativeName);
    Result.Dictionary.Prettify(Result.RelativeName);
  end else
  begin
    if not Dictionary.Contains(Result.RelativeName) then
    begin
      for LPackageIndex := Pred(PackageCount) downto 0 do
      begin
        LPackage := Packages[FOrderedPackageNames[LPackageIndex]];
        if not LPackage.Explicit and LPackage.Dictionary.Contains(Result.RelativeName) then
        begin
          Result.PackageName := LPackage.Name;
          Result.Dictionary := LPackage.Dictionary;
          Result.Dictionary.Dealias(Result.RelativeName);
          Result.Dictionary.Prettify(Result.RelativeName);
          Exit;
        end;
      end;
    end;
    Result.PackageName := String.Empty;
    Result.Dictionary := Dictionary;
    if Result.Dictionary.Dealias(Result.RelativeName) then
    begin
      Result := NameData(Result.RelativeName);
    end;
    Result.Dictionary.Prettify(Result.RelativeName);
  end;
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
