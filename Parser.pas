unit Parser;

interface

uses
  System.SysUtils, System.Math, System.StrUtils, System.Classes, System.Generics.Defaults, System.Generics.Collections, System.Rtti,
  Parser.Dictionary, Parser.Exception, Parser.Language, Parser.Lexer, Parser.Package, Parser.Syntax, Parser.Exporter, Parser.Value;

type
  TParserExpressionKind = (exEmpty, exTerm, exResolution, exDeclConstant, exDeclVariable, exDeclFunction, exDeclType, exDeclConstructor, exDeclInline, exDeclAlias, exShow, exDeletion, exAssertion, exLinkage);

  TParserExpressionKindHelper = record helper for TParserExpressionKind
  public
    constructor Create(const AFirstToken: TParserToken);
  end;

  TParserResponseValueKind = (rvNone, rvValue, rvString, rvStringArray);

  TParserResponseValue = record
  private
    FValue: TValue;
    function GetKind: TParserResponseValueKind;
    function GetAsValue: TParserValue;
    function GetAsString: String;
    function GetAsStringArray: TArray<String>;
  public
    property Kind: TParserResponseValueKind read GetKind;
    property AsValue: TParserValue read GetAsValue;
    property AsString: String read GetAsString;
    property AsStringArray: TArray<String> read GetAsStringArray;
    constructor Create(const AValue: TParserValue); overload;
    constructor Create(const AValue: String); overload;
    constructor Create(const AValue: TArray<String>); overload;
    function ToString(const APretty: Boolean = False): String;
  end;

  TParserResponse = record
  private
    FWarnings: TArray<String>;
    FExpressionKind: TParserExpressionKind;
    FReturnValue: TParserResponseValue;
  public
    property Warnings: TArray<String> read FWarnings;
    property ExpressionKind: TParserExpressionKind read FExpressionKind;
    property ReturnValue: TParserResponseValue read FReturnValue;
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

  TParserOption = (poWarnings, poOptimization);

  TParserOptions = set of TParserOption;

  TParserOptionHelper = record helper for TParserOption
  public
    constructor Create(const AName: String);
  end;

  TParserOptionsHelper = record helper for TParserOptions
  private
    class function GetDefaultOptions: TParserOptions; static;
  public
    class property DefaultOptions: TParserOptions read GetDefaultOptions;
  end;

  TParser = class(TSingletonImplementation, IParserValueSupplier, IParserPackageLinks)
  strict private
    procedure PackagePathsChange(Sender: TObject);
  private
    FDictionary: TParserDictionary;
    FPackages: TObjectDictionary<String, TParserPackage>;
    FOrderedPackageNames: TStringList;
    FPackagePaths: TStringList;
    FOptions: TParserOptions;
    function GetValues(const AName: String; const AArgs: TArray<TParserValue>): TParserValue;
    function GetRefTargets(const AName: String): IParserValueRefTarget;
    function GetMinArgCount(const AName: String): Integer;
    function GetMaxArgCount(const AName: String): Integer;
    function GetPackages(const AName: String): TParserPackage;
    function GetPackageNames(const AIndex: Integer): String;
    function GetPackageCount: Integer;
    function GetStandardPackages: TParserStandardPackages;
    procedure SetStandardPackages(const AValue: TParserStandardPackages);
    function GetObjects(const AName: String): TParserObject;
  protected
    property Values[const AName: String; const AArgs: TArray<TParserValue>]: TParserValue read GetValues;
    property RefTargets[const AName: String]: IParserValueRefTarget read GetRefTargets;
    property MinArgCount[const AName: String]: Integer read GetMinArgCount;
    property MaxArgCount[const AName: String]: Integer read GetMaxArgCount;
    function ContainsValue(const AName: String): Boolean;
    function ContainsRefTarget(const AName: String): Boolean;
  public
    property Dictionary: TParserDictionary read FDictionary;
    property Packages[const AName: String]: TParserPackage read GetPackages;
    property PackageNames[const AIndex: Integer]: String read GetPackageNames;
    property PackageCount: Integer read GetPackageCount;
    property StandardPackages: TParserStandardPackages read GetStandardPackages write SetStandardPackages;
    property PackagePaths: TStringList read FPackagePaths;
    property Objects[const AName: String]: TParserObject read GetObjects;
    property Options: TParserOptions read FOptions write FOptions;
    constructor Create;
    destructor Destroy; override;
    function HasPackage(const AName: String): Boolean;
    procedure RegisterPackage(const APackage: TParserPackage);
    procedure LoadPackage(const AName: String);
    procedure UnregisterPackage(const AName: String);
    procedure ClearPackages;
    function Evaluate(const AExpression: String): TParserResponse; virtual;
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
        case AFirstToken.Keyword of
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
          kwType:
            begin
              Self := exDeclType;
            end;
          kwConstructor:
            begin
              Self := exDeclConstructor;
            end;
          kwInline:
            begin
              Self := exDeclInline;
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
            end;
          kwAssert:
            begin
              Self := exAssertion;
            end;
          kwLink:
            begin
              Self := exLinkage;
            end;
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

{ TParserResponseValue }

constructor TParserResponseValue.Create(const AValue: TArray<String>);
begin
  FValue := TValue.From<TArray<String>>(AValue);
end;

constructor TParserResponseValue.Create(const AValue: String);
begin
  FValue := TValue.From<String>(AValue);
end;

constructor TParserResponseValue.Create(const AValue: TParserValue);
begin
  FValue := TValue.From<TParserValue>(AValue);
end;

function TParserResponseValue.GetAsValue: TParserValue;
begin
  Result := FValue.AsType<TParserValue>;
end;

function TParserResponseValue.GetAsString: String;
begin
  Result := FValue.AsType<String>;
end;

function TParserResponseValue.GetAsStringArray: TArray<String>;
begin
  Result := FValue.AsType<TArray<String>>;
end;

function TParserResponseValue.GetKind: TParserResponseValueKind;
begin
  if FValue.IsType<TParserValue>(False) then
  begin
    Result := rvValue;
  end else
  begin
    if FValue.IsType<String>(False) then
    begin
      Result := rvString;
    end else
    begin
      if FValue.IsType<TArray<String>>(False) then
      begin
        Result := rvStringArray;
      end else
      begin
        Result := Default(TParserResponseValueKind);
      end;
    end;
  end;
end;

function TParserResponseValue.ToString(const APretty: Boolean): String;
begin
  case Kind of
    rvNone:
      begin
        Result := String.Empty;
      end;
    rvValue:
      begin
        if APretty then
        begin
          if AsValue.IsNan then
          begin
            Result := 'Invalid number';
          end else
          begin
            if AsValue.IsNegInf then
            begin
              Result := 'Negative infinity';
            end else
            begin
              if AsValue.IsPosInf then
              begin
                Result := 'Positive infinity';
              end else
              begin
                Result := AsValue.ToString;
              end;
            end;
          end;
        end else
        begin
          Result := AsValue.ToString;
        end;
      end;
    rvString:
      begin
        Result := AsString;
      end;
    rvStringArray:
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
    if not AParser.FPackages.TryGetValue(PackageName, LPackage) then
    begin
      raise EParserUnknownPackageError.CreateFmt('Unknown package: %s', [PackageName.QuotedString]);
    end;
    FPackageName := LPackage.Name;
    FDictionary := LPackage.Dictionary;
    Dictionary.Dealias(FRelativeName);
    Dictionary.Prettify(FRelativeName);
  end else
  begin
    if not AParser.Dictionary.Contains(FRelativeName) then
    begin
      for LPackageIndex := Pred(AParser.PackageCount) downto 0 do
      begin
        LPackage := AParser.Packages[AParser.PackageNames[LPackageIndex]];
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

{ TParserOptionHelper }

constructor TParserOptionHelper.Create(const AName: String);
const
  LOptionNames: array [TParserOption] of String = ('Warnings', 'Optimization');
var
  LIndex: Integer;
begin
  LIndex := IndexText(AName, LOptionNames);
  if LIndex = -1 then
  begin
    raise EParserOptionError.CreateFmt('Option %s not not', [AName.QuotedString]);
  end;
  Self := TParserOption(LIndex);
end;

{ TParserOptionsHelper }

class function TParserOptionsHelper.GetDefaultOptions: TParserOptions;
begin
  Result := [Low(TParserOption) .. High(TParserOption)];
end;

{ TParser }

procedure TParser.ClearPackages;
begin
  FPackages.Clear;
  FOrderedPackageNames.Clear;
end;

function TParser.ContainsRefTarget(const AName: String): Boolean;
var
  LNameData: TParserNameData;
begin
  LNameData := TParserNameData.Create(Self, AName);
  Result := LNameData.Dictionary.Contains(LNameData.RelativeName) and LNameData.Dictionary[AName].Addressable;
end;

function TParser.ContainsValue(const AName: String): Boolean;
var
  LNameData: TParserNameData;
begin
  LNameData := TParserNameData.Create(Self, AName);
  Result := LNameData.Dictionary.Contains(LNameData.RelativeName);
end;

constructor TParser.Create;
begin
  inherited;
  FDictionary := TParserDictionary.Create;
  FPackages := TObjectDictionary<String, TParserPackage>.Create([], TIStringComparer.Ordinal);
  FOrderedPackageNames := TStringList.Create(dupError, False, False);
  RegisterPackage(TParserPackage.DefaultPackage);
  StandardPackages := TParserStandardPackages.DefaultPackages;
  FPackagePaths := TStringList.Create;
  PackagePaths.OnChange := PackagePathsChange;
  Options := TParserOptions.DefaultOptions;
end;

destructor TParser.Destroy;
begin
  PackagePaths.Free;
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

  procedure ExpectToken(const AAllowedKinds: TParserTokenKinds = []; const AAllowedKeywords: TParserKeywords = [kwNone]; const APreview: Boolean = False);
  begin
    LToken := LLexer.NextToken[APreview];
    if ((AAllowedKinds <> []) and not (LToken.Kind in AAllowedKinds) and ((LToken.Kind <> tkName) or (LToken.Keyword = kwNone))) or ((LToken.Kind = tkName) and not (LToken.Keyword in AAllowedKeywords)) then
    begin
      raise EParserTokenUnexpectedError.CreateFmt('Unexpected token at %d: %s', [LToken.CharIndex, LToken.ToString.QuotedString]);
    end;
  end;

  procedure EvaluateExpression;
  var
    LSyntaxTree: TParserTree;
    LObjectName: String;
    LObjectNames: TStringList;
    LValue: TParserValue;
    LFunct: TParserCustomFunction;
    LFunctParams: TList<TParserParam>;
    LFunctParamName: String;
    LTypeClass: TParserTypeClass;
    LType: IParserValueConstraint;
    LExportTarget: TStringList;
    LExporter: TParserCodeExporter;
    LNameData: TParserNameData;

    procedure BuildTree(const AEnding: Boolean = True);
    var
      LNode: TParserParentNode;

      procedure BuildParentNode(const AKind: TParserNodeKind = nkNone);
      var
        LDeRef: Boolean;

        procedure BuildSubNode;
        var
          LOperator: TParserOperator;
          LNewNode: TParserParentNode;
        begin
          if LToken.Kind = tkSymbolOp then
          begin
            if ((LNode.Count = 0) and not (LToken.&Operator in TParserOperator.UnaryOperators)) or ((LNode.Count <> 0) and not (LToken.&Operator in TParserOperator.BinaryOperators)) then
            begin
              raise EParserOperatorError.CreateFmt('Wrong operand count for %s', [String(LToken.&Operator.ToChar).QuotedString]);
            end;
            LOperator := LToken.&Operator;
            ExpectToken([tkSymbolParOp, tkSymbolBrackOp, tkSymbolBraceOp, tkSymbolAbs, tkNumberDec, tkNumberHex, tkText, tkName]);
          end else
          begin
            LOperator := opAdd;
          end;
          case LToken.Kind of
            tkSymbolRef:
              begin
                ExpectToken([tkName]);
                TParserRefNode.Create(LNode, LToken.Name);
                ExpectToken([tkEnd, tkSymbolOp, tkSymbolComma, tkSymbolParCl, tkSymbolBrackCl, tkSymbolBraceCl, tkSymbolAbs], TParserKeyword.Allowed[AKind]);
              end;
            tkSymbolHash:
              begin
                LDeRef := True;
                ExpectToken([tkName]);
                BuildSubNode;
              end;
            tkSymbolParOp:
              begin
                LNode := TParserParentNode.Create(LNode, LOperator);
                ExpectToken([tkEnd, tkSymbolOp, tkSymbolRef, tkSymbolHash, tkSymbolParOp, tkSymbolBrackOp, tkSymbolBraceOp, tkSymbolAbs, tkNumberDec, tkNumberHex, tkText, tkName]);
                BuildParentNode;
              end;
            tkSymbolBrackOp:
              begin
                LNode := TParserArrayNode.Create(LNode, LOperator);
                ExpectToken([tkEnd, tkSymbolOp, tkSymbolRef, tkSymbolHash, tkSymbolParOp, tkSymbolBrackOp, tkSymbolBraceOp, tkSymbolBrackCl, tkSymbolAbs, tkNumberDec, tkNumberHex, tkText, tkName]);
                BuildParentNode(nkElements);
              end;
            tkSymbolBraceOp:
              begin
                LNode := TParserRecordNode.Create(LNode, LOperator);
                ExpectToken([tkEnd, tkSymbolBraceCl, tkName]);
                BuildParentNode(nkFields);
              end;
            tkSymbolAbs:
              begin
                LNode := TParserAbsNode.Create(LNode, LOperator);
                ExpectToken([tkEnd, tkSymbolOp, tkSymbolParOp, tkSymbolBrackOp, tkSymbolBraceOp, tkSymbolAbs, tkNumberDec, tkNumberHex, tkText, tkName]);
                BuildParentNode(nkAbs);
              end;
            tkNumberDec:
              begin
                TParserValueNode.Create(LNode, LOperator, TParserValue.Create(LToken.NumberDec));
                ExpectToken([tkEnd, tkSymbolOp, tkSymbolComma, tkSymbolParCl, tkSymbolBrackCl, tkSymbolBraceCl, tkSymbolAbs], TParserKeyword.Allowed[AKind]);
              end;
            tkNumberHex:
              begin
                TParserValueNode.Create(LNode, LOperator, TParserValue.Create(LToken.NumberHex));
                ExpectToken([tkEnd, tkSymbolOp, tkSymbolComma, tkSymbolParCl, tkSymbolBrackCl, tkSymbolBraceCl, tkSymbolAbs], TParserKeyword.Allowed[AKind]);
              end;
            tkText:
              begin
                TParserValueNode.Create(LNode, LOperator, TParserValue.Create(LToken.Text));
                ExpectToken([tkEnd, tkSymbolOp, tkSymbolComma, tkSymbolParCl, tkSymbolBrackCl, tkSymbolBraceCl, tkSymbolAbs], TParserKeyword.Allowed[AKind]);
              end;
            tkName:
              begin
                case LToken.Keyword of
                  kwSpread:
                    begin
                      if LNode.Count <> 0 then
                      begin
                        raise EParserTokenUnexpectedError.CreateFmt('Unexpected token at %d: %s', [LToken.CharIndex, LToken.ToString.QuotedString]);
                      end;
                      (LNode as TParserArgNode).Spread := True;
                      ExpectToken([tkSymbolOp, tkSymbolRef, tkSymbolHash, tkSymbolParOp, tkSymbolBrackOp, tkSymbolBraceOp, tkSymbolBraceOp, tkSymbolAbs, tkNumberDec, tkNumberHex, tkText, tkName]);
                    end;
                  kwIf:
                    begin
                      LNode := TParserIfNode.Create(LNode, LOperator);
                      ExpectToken([tkSymbolOp, tkSymbolRef, tkSymbolHash, tkSymbolParOp, tkSymbolBrackOp, tkSymbolBraceOp, tkSymbolBraceOp, tkSymbolAbs, tkNumberDec, tkNumberHex, tkText, tkName]);
                      BuildParentNode(nkIf);
                      BuildParentNode(nkThen);
                      BuildParentNode(nkElse);
                    end;
                  kwTry:
                    begin
                      LNode := TParserTryNode.Create(LNode, LOperator);
                      ExpectToken([tkSymbolOp, tkSymbolRef, tkSymbolHash, tkSymbolParOp, tkSymbolBrackOp, tkSymbolBraceOp, tkSymbolBraceOp, tkSymbolAbs, tkNumberDec, tkNumberHex, tkText, tkName]);
                      BuildParentNode(nkTry);
                      BuildParentNode(nkExcept);
                    end;
                  else
                    begin
                      if LDeRef then
                      begin
                        LNewNode := TParserDeRefNode.Create(LNode, LToken.Name);
                        LDeRef := False;
                      end else
                      begin
                        LNewNode := TParserNamedNode.Create(LNode, LOperator, LToken.Name);
                      end;
                      ExpectToken([tkEnd, tkSymbolOp, tkSymbolParOp, tkSymbolParCl, tkSymbolBrackCl,  tkSymbolBraceCl, tkSymbolAbs, tkSymbolComma], TParserKeyword.Allowed[AKind]);
                      if LToken.Kind = tkSymbolParOp then
                      begin
                        LNode := LNewNode;
                        ExpectToken([tkSymbolOp, tkSymbolRef, tkSymbolHash, tkSymbolParOp, tkSymbolParCl, tkSymbolBrackOp, tkSymbolBraceOp, tkSymbolAbs, tkNumberDec, tkNumberHex, tkText, tkName], TParserKeyword.Allowed[nkArgs]);
                        BuildParentNode(nkArgs);
                      end;
                    end;
                end;
              end;
          end;
        end;

      begin
        LDeRef := False;
        case AKind of
          nkArgs, nkElements, nkIf, nkThen, nkElse, nkTry, nkExcept:
            begin
              LNode := TParserArgNode.Create(LNode);
            end;
          nkFields:
            begin
              if LToken.Kind = tkName then
              begin
                LNode := TParserNamedArgNode.Create(LNode, LToken.Name);
                ExpectToken([tkSymbolEq]);
                ExpectToken([tkSymbolOp, tkSymbolRef, tkSymbolHash, tkSymbolParOp, tkSymbolBrackOp, tkSymbolBraceOp, tkSymbolBraceCl, tkNumberDec, tkNumberHex, tkText, tkName]);
              end else
              begin
                LNode := TParserArgNode.Create(LNode);
              end;
            end;
        end;
        repeat
          case LToken.Kind of
            tkSymbolParCl:
              begin
                if not Assigned(LNode.Parent) or not (AKind in [nkNone, nkArgs]) or ((AKind = nkArgs) and not Assigned(LNode.Parent.Parent)) then
                begin
                  raise EParserParenthesisError.CreateFmt('Unexpected parenthesis at %d', [LToken.CharIndex]);
                end;
                if AKind = nkArgs then
                begin
                  if LNode.Count = 0 then
                  begin
                    LNode := LNode.Parent;
                    LNode.Clear;
                  end else
                  begin
                    LNode := LNode.Parent;
                  end;
                end;
                LNode := LNode.Parent;
                ExpectToken([tkEnd, tkSymbolOp, tkSymbolComma, tkSymbolParCl, tkSymbolBrackCl, tkSymbolAbs], TParserKeyword.Allowed[TParserNodeKind.Create(LNode)]);
                Exit;
              end;
            tkSymbolBrackCl:
              begin
                if not Assigned(LNode.Parent) or (AKind <> nkElements) or ((AKind = nkElements) and not Assigned(LNode.Parent.Parent)) then
                begin
                  raise EParserParenthesisError.CreateFmt('Unexpected parenthesis at %d', [LToken.CharIndex]);
                end;
                if AKind = nkElements then
                begin
                  if LNode.Count = 0 then
                  begin
                    LNode := LNode.Parent;
                    LNode.Clear;
                  end else
                  begin
                    LNode := LNode.Parent;
                  end;
                end;
                LNode := LNode.Parent;
                ExpectToken([tkEnd, tkSymbolOp, tkSymbolComma, tkSymbolParCl, tkSymbolBrackCl, tkSymbolBraceCl, tkSymbolAbs], TParserKeyword.Allowed[TParserNodeKind.Create(LNode)]);
                Exit;
              end;
            tkSymbolBraceCl:
              begin
                if not Assigned(LNode.Parent) or (AKind <> nkFields) or ((AKind = nkFields) and not Assigned(LNode.Parent.Parent)) then
                begin
                  raise EParserParenthesisError.CreateFmt('Unexpected parenthesis at %d', [LToken.CharIndex]);
                end;
                if AKind = nkFields then
                begin
                  if LNode.Count = 0 then
                  begin
                    LNode := LNode.Parent;
                    LNode.Clear;
                  end else
                  begin
                    LNode := LNode.Parent;
                  end;
                end;
                LNode := LNode.Parent;
                ExpectToken([tkEnd, tkSymbolOp, tkSymbolComma, tkSymbolParCl, tkSymbolBrackCl, tkSymbolBraceCl, tkSymbolAbs], TParserKeyword.Allowed[TParserNodeKind.Create(LNode)]);
                Exit;
              end;
            tkSymbolAbs:
              begin
                if AKind = nkAbs then
                begin
                  if not Assigned(LNode.Parent) then
                  begin
                    raise EParserParenthesisError.CreateFmt('Unexpected parenthesis at %d', [LToken.CharIndex]);
                  end;
                  if LNode.Count = 0 then
                  begin
                    raise EParserTokenUnexpectedError.CreateFmt('Unexpected token at %d: %s', [LToken.CharIndex, LToken.ToString.QuotedString]);
                  end;
                  LNode := LNode.Parent;
                  ExpectToken([tkEnd, tkSymbolOp, tkSymbolComma, tkSymbolParCl, tkSymbolBrackCl, tkSymbolAbs], TParserKeyword.Allowed[TParserNodeKind.Create(LNode)]);
                  Exit;
                end else
                begin
                  if LNode.Count <> 0 then
                  begin
                    raise EParserParenthesisError.CreateFmt('Unexpected parenthesis at %d', [LToken.CharIndex]);
                  end;
                  BuildSubNode;
                end;
              end;
            tkSymbolComma:
              begin
                case AKind of
                  nkArgs, nkElements:
                    begin
                      LNode := TParserArgNode.Create(LNode.Parent);
                    end;
                  nkFields:
                    begin
                      ExpectToken([tkName]);
                      LNode := TParserNamedArgNode.Create(LNode.Parent, LToken.Name);
                      ExpectToken([tkSymbolEq]);
                    end;
                  else
                    begin
                      raise EParserCommaError.CreateFmt('Unexpected separator at %d', [LToken.CharIndex]);
                    end;
                end;
                ExpectToken([tkSymbolOp, tkSymbolRef, tkSymbolHash, tkSymbolParOp, tkSymbolBrackOp, tkSymbolBraceOp, tkNumberDec, tkNumberHex, tkText, tkName], TParserKeyword.Allowed[AKind]);
                //Exit;
              end;
            tkName:
              begin
                if LToken.Keyword in [kwThen, kwElse, kwExcept] then
                begin
                  LNode := LNode.Parent;
                  ExpectToken([tkSymbolParOp, tkSymbolBrackOp, tkSymbolBraceOp, tkSymbolBraceOp, tkSymbolAbs, tkNumberDec, tkNumberHex, tkText, tkName]);
                  Exit;
                end else
                begin
                  BuildSubNode;
                end;
              end;
            else
              begin
                BuildSubNode;
              end;
          end;
        until LToken.Kind = tkEnd;
        if AKind in [nkElse, nkExcept] then
        begin
          LNode := LNode.Parent.Parent;
          Exit;
        end;
        if Assigned(LNode.Parent) then
        begin
          raise EParserParenthesisError.Create('Unterminated expression');
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

    procedure ParseType(const AAllowedFollowTokens: TParserTokenKinds; AName: String = String.Empty);
    begin
      ExpectToken(AAllowedFollowTokens + [tkSymbolParOp]);
      if LToken.Kind = tkSymbolParOp then
      begin
        repeat
          ExpectToken([tkSymbolParCl, tkName]);
          if LToken.Kind = tkName then
          begin
            ExpectToken([tkSymbolComma, tkSymbolParCl]);
          end;
        until LToken.Kind = tkSymbolParCl;
        ExpectToken(AAllowedFollowTokens);
      end;
      LType := LTypeClass.Create(AName);
    end;

  begin
    case Result.ExpressionKind of
      exTerm:
        begin
          LSyntaxTree := TParserTree.Create([Self]);
          try
            BuildTree;
            Result.FReturnValue := TParserResponseValue.Create(LSyntaxTree.Calculate);
          finally
            LSyntaxTree.Free;
          end;
        end;
      exResolution:
        begin
          ExpectToken([tkName]);
          LObjectName := LToken.Name;
          Result.FReturnValue := TParserResponseValue.Create(TParserNameData.Create(Self, LObjectName).AbsoluteName);
          ExpectToken([tkEnd]);
        end;
      exDeclConstant:
        begin
          LObjectNames := TStringList.Create;
          try
            ParseObjectNames([tkSymbolEq]);
            ExpectToken([tkSymbolOp, tkSymbolRef, tkSymbolHash, tkSymbolParOp, tkSymbolParCl, tkSymbolBrackOp, tkSymbolBrackCl, tkSymbolBraceOp, tkSymbolBraceCl, tkNumberDec, tkNumberHex, tkText, tkName], TParserKeyword.Allowed[nkNone]);
            LSyntaxTree := TParserTree.Create([Self]);
            try
              BuildTree;
              LValue := LSyntaxTree.Calculate;
              for LObjectName in LObjectNames do
              begin
                Dictionary.Add(TParserConstant.Create(LObjectName, LValue));
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
              ExpectToken([tkSymbolOp, tkSymbolRef, tkSymbolHash, tkSymbolParOp, tkSymbolParCl, tkSymbolBrackOp, tkSymbolBrackCl, tkNumberDec, tkNumberHex, tkText, tkName], TParserKeyword.Allowed[nkNone]);
              LSyntaxTree := TParserTree.Create([Self]);
              try
                BuildTree;
                LValue := LSyntaxTree.Calculate;
              finally
                LSyntaxTree.Free;
              end;
            end else
            begin
              LValue := TParserValue.Empty[vkDouble];
            end;
            for LObjectName in LObjectNames do
            begin
              Dictionary.Add(TParserVariable.Create(LObjectName, LValue));
            end;
          finally
            LObjectNames.Free;
          end;
        end;
      exDeclConstructor, exDeclFunction:
        begin
          ExpectToken([tkName]);
          LObjectName := LToken.Name;
          ExpectToken([tkSymbolEq, tkSymbolParOp]);
          LFunctParams := TList<TParserParam>.Create{(dupError, False, False)}; // Duplicate check is done at TParserFunction.Create(...)
          try
            if LToken.Kind = tkSymbolParOp then
            begin
              repeat
                ExpectToken([tkSymbolParCl, tkName]);
                if LToken.Kind = tkName then
                begin
                  LFunctParamName := LToken.Name;
                  ExpectToken([tkSymbolComma, tkSymbolEq, tkSymbolColon, tkSymbolParCl]);
                  if LToken.Kind = tkSymbolColon then
                  begin
                    ExpectToken([tkName], TParserKeyword.TypeConstructors + [kwNone]);
                    LTypeClass := TParserType.TypeClasses[LToken.Keyword];
                    if Assigned(LTypeClass) then
                    begin
                      ParseType([tkSymbolComma, tkSymbolEq, tkSymbolParCl]);
                      LType := TParserTempType.Create(LType as TParserType);
                    end else
                    begin
                      if not (Objects[LToken.Name] is TParserType) then
                      begin
                        raise EParserNoTypeError.CreateFmt('%s is not a type', [LObjectName]);
                      end;
                      LType := Objects[LToken.Name] as TParserType;
                      ExpectToken([tkSymbolComma, tkSymbolEq, tkSymbolParCl]);
                    end;
                  end;
                  if LToken.Kind = tkSymbolEq then
                  begin
                    ExpectToken([tkSymbolOp, tkSymbolRef, tkSymbolHash, tkSymbolParOp, tkSymbolParCl, tkSymbolBrackOp, tkSymbolBrackCl, tkNumberDec, tkNumberHex, tkText, tkName], TParserKeyword.Allowed[nkNone]);
                    LSyntaxTree := TParserTree.Create([Self]);
                    try
                      BuildTree;
                      LValue := LSyntaxTree.Calculate;
                    finally
                      LSyntaxTree.Free;
                    end;
                    LFunctParams.Add(TParserParam.Create(LFunctParamName, LValue, LType));
                  end else
                  begin
                    LFunctParams.Add(TParserParam.Create(LFunctParamName, LType));
                  end;
                  LType := Default(IParserValueConstraint);
                end;
              until LToken.Kind = tkSymbolParCl;
              ExpectToken([tkSymbolEq]);
            end;
            ExpectToken([tkSymbolOp, tkSymbolRef, tkSymbolHash, tkSymbolParOp, tkSymbolParCl, tkSymbolBrackOp, tkSymbolBrackCl, tkSymbolBraceOp, tkSymbolBraceCl, tkNumberDec, tkNumberHex, tkText, tkName], TParserKeyword.Allowed[nkNone]);
            LFunct := TParserCustomFunction.Create(LObjectName, LFunctParams.ToArray);
            try
              LSyntaxTree := TParserTree.Create([LFunct, Self]);
              try
                BuildTree;
              except
                LSyntaxTree.Free;
                raise;
              end;
              if poOptimization in Options then
              begin
                LSyntaxTree.Optimize;
              end;
              LFunct.SyntaxTree := LSyntaxTree;
              Dictionary.Add(LFunct);
            except
              LFunct.Free;
              raise;
            end;
          finally
            LFunctParams.Free;
          end;
        end;
      exDeclType:
        begin
          ExpectToken([tkName]);
          LObjectName := LToken.Name;
          ExpectToken([tkSymbolEq]);
          ExpectToken([tkName], TParserKeyword.TypeConstructors);
          LTypeClass := TParserType.TypeClasses[LToken.Keyword];
          if not Assigned(LTypeClass) then
          begin
            raise EParserTypeUnknownError.CreateFmt('Unknown type constructor: %s', [String(LToken.Name).QuotedString]);
          end;
          ParseType([tkEnd], LObjectName);
          Dictionary.Add(LType as TParserType);
        end;
      exDeclInline:
        begin
          ExpectToken([tkName]);
          LObjectName := LToken.Name;
          ExpectToken([tkSymbolEq]);
          ExpectToken([tkSymbolOp, tkSymbolRef, tkSymbolHash, tkSymbolParOp, tkSymbolParCl, tkSymbolBrackOp, tkSymbolBrackCl, tkSymbolBraceOp, tkSymbolBraceCl, tkNumberDec, tkNumberHex, tkText, tkName], TParserKeyword.Allowed[nkNone]);
          LSyntaxTree := TParserTree.Create([LFunct, Self]);
          try
            BuildTree;
          except
            LSyntaxTree.Free;
            raise;
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
            Result.FReturnValue := TParserResponseValue.Create(LExportTarget.ToStringArray);
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
      exAssertion:
        begin
          LObjectNames := TStringList.Create;
          try
            ParseObjectNames([tkSymbolEq]);
            ExpectToken([tkSymbolOp, tkSymbolRef, tkSymbolHash, tkSymbolParOp, tkSymbolParCl, tkSymbolBrackOp, tkSymbolBrackCl, tkSymbolBraceOp, tkSymbolBraceCl, tkNumberDec, tkNumberHex, tkText, tkName], TParserKeyword.Allowed[nkNone]);
            LSyntaxTree := TParserTree.Create([Self]);
            try
              BuildTree;
              LValue := LSyntaxTree.Calculate;
              for LObjectName in LObjectNames do
              begin
                if not TParserObject.ValidName(LObjectName) then
                begin
                  raise EParserObjectNameError.CreateFmt('Invalid name: %s', [LObjectName.QuotedString]);
                end;
                if not Values[LObjectName, []].Equals(LSyntaxTree.Calculate) then
                begin
                  raise EParserAssertionError.CreateFmt('Assertion for %s failed', [LObjectName.QuotedString]);
                end;
              end;
            finally
              LSyntaxTree.Free;
            end;
          finally
            LObjectNames.Free;
          end;
        end;
      exLinkage:
        begin
          LObjectNames := TStringList.Create;
          try
            ParseObjectNames([tkEnd]);
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
    ExpectToken([tkEnd, tkSymbolOp, tkSymbolRef, tkSymbolHash, tkSymbolParOp, tkSymbolParCl, tkSymbolBrackOp, tkSymbolBrackCl, tkSymbolBraceOp, tkSymbolBraceCl, tkSymbolAbs, tkNumberDec, tkNumberHex, tkText, tkName], TParserKeyword.ExpressionStarters + TParserKeyword.Allowed[nkNone]);
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
  Result := LNameData.Dictionary[LNameData.RelativeName].MaxArgCount;
end;

function TParser.GetMinArgCount(const AName: String): Integer;
var
  LNameData: TParserNameData;
begin
  LNameData := TParserNameData.Create(Self, AName);
  Result := LNameData.Dictionary[LNameData.RelativeName].MinArgCount;
end;

function TParser.GetObjects(const AName: String): TParserObject;
var
  LNameData: TParserNameData;
begin
  LNameData := TParserNameData.Create(Self, AName);
  if not Assigned(LNameData.Dictionary) then
  begin
    raise EParserUnknownPackageError.CreateFmt('Unknown package: %s', [LNameData.PackageName.QuotedString]);
  end;
  Result := LNameData.Dictionary[LNameData.RelativeName];
end;

function TParser.GetPackageCount: Integer;
begin
  Result := FPackages.Count;
end;

function TParser.GetPackageNames(const AIndex: Integer): String;
begin
  Result := FOrderedPackageNames[AIndex];
end;

function TParser.GetPackages(const AName: String): TParserPackage;
begin
  if not HasPackage(AName) then
  begin
    raise EParserUnknownPackageError.CreateFmt('Unknown package: %s', [AName.QuotedString]);
  end;
  Result := FPackages[AName];
end;

function TParser.GetRefTargets(const AName: String): IParserValueRefTarget;
var
  LObject: TParserObject;
begin
  LObject := Objects[AName];
  if not LObject.Addressable then
  begin
    raise EParserAddressError.CreateFmt('%s not addressable', [LObject.Name.QuotedString]);
  end;
  Result := LObject as IParserValueRefTarget;
end;

function TParser.GetStandardPackages: TParserStandardPackages;
var
  LPackage: TParserStandardPackage;
begin
  for LPackage := Low(TParserStandardPackage) to High(TParserStandardPackage) do
  begin
    if HasPackage(LPackage.Name) then
    begin
      Include(Result, LPackage);
    end;
  end;
end;

function TParser.GetValues(const AName: String; const AArgs: TArray<TParserValue>): TParserValue;
begin
  Result := Objects[AName].Value[AArgs];
end;

function TParser.HasPackage(const AName: String): Boolean;
begin
  Result := FPackages.ContainsKey(AName);
end;

procedure TParser.LoadPackage(const AName: String);
var
  LIndex: Integer;
  LFileName: String;
begin
  for LIndex := Pred(PackagePaths.Count) downto 0 do
  begin
    LFileName := ChangeFileExt(Concat(PackagePaths[LIndex], AName), TParserCustomPackage.FileExtension);
    if FileExists(LFileName) then
    begin
      RegisterPackage(TParserCustomPackage.Create(LFileName));
    end;
  end;
end;

procedure TParser.PackagePathsChange(Sender: TObject);
var
  LIndex: Integer;
begin
  for LIndex := 0 to Pred(PackagePaths.Count) do
  begin
    PackagePaths[LIndex] := IncludeTrailingPathDelimiter(PackagePaths[LIndex]);
  end;
end;

procedure TParser.RegisterPackage(const APackage: TParserPackage);
begin
  if HasPackage(APackage.Name) then
  begin
    raise EParserDuplicatePackageError.CreateFmt('Package %s already registered', [APackage.Name.QuotedString]);
  end;
  FPackages.Add(APackage.Name, APackage);
  FOrderedPackageNames.Add(APackage.Name);
end;

procedure TParser.SetStandardPackages(const AValue: TParserStandardPackages);
var
  LPackage: TParserStandardPackage;
begin
  for LPackage := Low(TParserStandardPackage) to High(TParserStandardPackage) do
  begin
    if LPackage in AValue then
    begin
      if not HasPackage(LPackage.Name) then
      begin
        RegisterPackage(TParserStandardPackageProvider[LPackage]);
      end;
    end else
    begin
      if HasPackage(LPackage.Name) then
      begin
        UnregisterPackage(LPackage.Name);
      end;
    end;
  end;
end;

procedure TParser.UnregisterPackage(const AName: String);
begin          
  if not HasPackage(AName) then
  begin
    raise EParserUnknownPackageError.CreateFmt('Unknown package: %s', [AName.QuotedString]);
  end;
  FOrderedPackageNames.Delete(FOrderedPackageNames.IndexOf(AName));
  FPackages.Remove(AName);
end;

end.
