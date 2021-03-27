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

unit Parser;

interface

uses
  System.SysUtils, System.Math, System.StrUtils, System.Classes, System.Generics.Defaults, System.Generics.Collections, System.Rtti,
  Parser.Dictionary, Parser.Exception, Parser.Language, Parser.Lexer, Parser.Package, Parser.Syntax, Parser.Exporter, Parser.Value;

type
  TParserExpressionKind = (exEmpty, exTerm, exResolution, exDeclConstant, exDeclVariable, exDeclFunction, exDeclType, exDeclConstructor, exDeclInline, exOption, exDeclAlias, exShow, exDeletion, exAssertion, exLinkage);

  TParserExpressionKinds = set of TParserExpressionKind;

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

  TParserOption = (poWarnings, poOptimization, poAllowed, poInitialValue, poPackagePaths);

  TParserOptionHelper = record helper for TParserOption
  public
    constructor Create(const AName: String);
    procedure Define(const AParser: TParser; const AValue: TParserValue);
  end;

  TParserOptions = class(TPersistent)
  strict private
    procedure PackagePathsChange(Sender: TObject);
  private
    FWarnings: Boolean;
    FOptimization: Boolean;
    FAllowed: TParserExpressionKinds;
    FInitialType: TParserValueKind;
    FPackagePaths: TStringList;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    property Warnings: Boolean read FWarnings write FWarnings;
    property Optimization: Boolean read FOptimization write FOptimization;
    property Allowed: TParserExpressionKinds read FAllowed write FAllowed;
    property InitialType: TParserValueKind read FInitialType write FInitialType;
    property PackagePaths: TStringList read FPackagePaths;
    constructor Create;
    destructor Destroy; override;
  end;

  TParser = class(TSingletonImplementation, IParserValueSupplier, IParserPackageLinks)
  private
    FDictionary: TParserDictionary;
    FPackages: TObjectDictionary<String, TParserPackage>;
    FOrderedPackageNames: TStringList;
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
    property Objects[const AName: String]: TParserObject read GetObjects;
    property Options: TParserOptions read FOptions;
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
          kwOpt:
            begin
              Self := exOption;
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
var
  LIndex: Integer;
begin
  LIndex := IndexText(AName, ['Warnings', 'Optimization', 'Allowed', 'InitialType', 'PackagePaths']);
  if not (LIndex in [Ord(Low(TParserOption)) .. Ord(High(TParserOption))]) then
  begin
    raise EParserOptionError.CreateFmt('Option %s not found', [AName.QuotedString]);
  end;
  Self := TParserOption(LIndex);
end;

procedure TParserOptionHelper.Define(const AParser: TParser; const AValue: TParserValue);

  function ExpressionKinds(const AValues: TArray<TParserValue>): TParserExpressionKinds;
  var
    LElement: TParserValue;
  begin
    Result := Default(TParserExpressionKinds);
    for LElement in AValues do
    begin
      Include(Result, TParserExpressionKind(LElement.AsInteger));
    end;
  end;

begin
  case Self of
    poWarnings:
      begin
        AParser.Options.Warnings := AValue.AsBoolean;
      end;
    poOptimization:
      begin
        AParser.Options.Optimization := AValue.AsBoolean;
      end;
    poAllowed:
      begin
        AParser.Options.Allowed := ExpressionKinds(AValue.AsArray);
      end;
    poInitialValue:
      begin
        if not InRange(AValue.AsInteger, Ord(Low(TParserValueKind)), Ord(High(TParserValueKind))) then
        begin
          raise EParserOptionError.Create('Invalid value kind');
        end;
        AParser.Options.InitialType := TParserValueKind(AValue.AsInteger);
      end;
    poPackagePaths:
      begin
        AParser.Options.PackagePaths.Clear;
        AParser.Options.PackagePaths.AddStrings(AValue.AsArray.AsStrings);
      end;
  end;
end;

{ TParserOptions }

procedure TParserOptions.AssignTo(Dest: TPersistent);
begin
  if Dest is TParserOptions then
  begin
    (Dest as TParserOptions).Warnings := Warnings;
    (Dest as TParserOptions).Optimization := Optimization;
    (Dest as TParserOptions).Allowed := Allowed;
    (Dest as TParserOptions).InitialType := InitialType;
  end else
  begin
    inherited;
  end;
end;

constructor TParserOptions.Create;
begin
  Warnings := True;
  Optimization := True;
  Allowed := [Low(TParserExpressionKind) .. High(TParserExpressionKind)];
  InitialType := vkDouble;
  FPackagePaths := TStringList.Create;
  PackagePaths.OnChange := PackagePathsChange;
end;

destructor TParserOptions.Destroy;
begin
  PackagePaths.Free;
  inherited;
end;

procedure TParserOptions.PackagePathsChange(Sender: TObject);
var
  LIndex: Integer;
begin
  PackagePaths.OnChange := Default(TNotifyEvent);
  PackagePaths.BeginUpdate;
  for LIndex := 0 to Pred(PackagePaths.Count) do
  begin
    PackagePaths[LIndex] := IncludeTrailingPathDelimiter(PackagePaths[LIndex]);
  end;
  PackagePaths.EndUpdate;
  PackagePaths.OnChange := PackagePathsChange;
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
  FOptions := TParserOptions.Create;
end;

destructor TParser.Destroy;
begin
  Options.Free;
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
    LTypeClass: TParserTypeClass;
    LExportTarget: TStringList;
    LExporter: TParserCodeExporter;
    LNameData: TParserNameData;

    procedure ParseType(const AAllowedFollowTokens: TParserTokenKinds; AName: String = String.Empty); forward;
    procedure BuildFunctParams(var AFunctParams: TList<TParserParam>; const ADelegate: Boolean = False); forward;

    procedure BuildTree(var ATree: TParserTree; const ASyntaxTreeKind: TParserSyntaxTreeKind = stNone);
    var
      LNode: TParserParentNode;

      procedure BuildParentNode(const ANodeKind: TParserNodeKind = nkNone);
      var
        LDeRef: Boolean;

        procedure BuildSubNode;
        var
          LOperator: TParserOperator;
          LNewNode: TParserParentNode;
          LDelegateFunct: TParserCustomFunction;
          LDelegateFunctParams: TList<TParserParam>;
          LDelegateSyntaxTree: TParserTree;
        begin
          if LToken.Kind = tkSymbolOp then
          begin
            if ((LNode.Count = 0) and not (LToken.&Operator in TParserOperator.UnaryOperators)) or ((LNode.Count <> 0) and not (LToken.&Operator in TParserOperator.BinaryOperators)) then
            begin
              raise EParserOperatorError.CreateFmt('Wrong operand count for %s', [String(LToken.&Operator.ToChar).QuotedString]);
            end;
            LOperator := LToken.&Operator;
            ExpectToken([tkSymbolParOp, tkSymbolBrackOp, tkSymbolBraceOp, tkSymbolAbs, tkNumberDec, tkNumberHex, tkText, tkName], TParserKeyword.BlockStarters + TParserKeyword.Allowed[ASyntaxTreeKind, ANodeKind]);
          end else
          begin
            LOperator := opAdd;
          end;
          case LToken.Kind of
            tkSymbolRef:
              begin
                ExpectToken([tkName]);
                TParserRefNode.Create(LNode, LToken.Name);
                ExpectToken([tkEnd, tkSymbolOp, tkSymbolComma, tkSymbolParCl, tkSymbolBrackCl, tkSymbolBraceCl, tkSymbolAbs], TParserKeyword.Allowed[ASyntaxTreeKind, ANodeKind]);
              end;
            tkSymbolHash:
              begin
                LDeRef := True;
                ExpectToken([tkSymbolParOp, tkName]);
                BuildSubNode;
              end;
            tkSymbolParOp:
              begin
                LNode := TParserParentNode.Create(LNode, LOperator);
                ExpectToken([tkEnd, tkSymbolOp, tkSymbolRef, tkSymbolHash, tkSymbolParOp, tkSymbolBrackOp, tkSymbolBraceOp, tkSymbolAbs, tkNumberDec, tkNumberHex, tkText, tkName], TParserKeyword.BlockStarters + TParserKeyword.Allowed[ASyntaxTreeKind, nkNone]);
                BuildParentNode;
              end;
            tkSymbolBrackOp:
              begin
                LNode := TParserArrayNode.Create(LNode, LOperator);
                ExpectToken([tkEnd, tkSymbolOp, tkSymbolRef, tkSymbolHash, tkSymbolParOp, tkSymbolBrackOp, tkSymbolBraceOp, tkSymbolBrackCl, tkSymbolAbs, tkNumberDec, tkNumberHex, tkText, tkName], TParserKeyword.BlockStarters + TParserKeyword.Allowed[ASyntaxTreeKind, nkElements]);
                BuildParentNode(nkElements);
              end;
            tkSymbolBraceOp:
              begin
                LNode := TParserRecordNode.Create(LNode, LOperator);
                ExpectToken([tkEnd, tkSymbolParOp, tkSymbolBraceCl, tkName]);
                BuildParentNode(nkFields);
              end;
            tkSymbolAbs:
              begin
                LNode := TParserAbsNode.Create(LNode, LOperator);
                ExpectToken([tkEnd, tkSymbolOp, tkSymbolParOp, tkSymbolBrackOp, tkSymbolBraceOp, tkSymbolAbs, tkNumberDec, tkNumberHex, tkText, tkName], TParserKeyword.BlockStarters + TParserKeyword.Allowed[ASyntaxTreeKind, nkAbs]);
                BuildParentNode(nkAbs);
              end;
            tkNumberDec:
              begin
                TParserValueNode.Create(LNode, LOperator, TParserValue.Create(LToken.NumberDec));
                ExpectToken([tkEnd, tkSymbolOp, tkSymbolComma, tkSymbolParCl, tkSymbolBrackCl, tkSymbolBraceCl, tkSymbolAbs], TParserKeyword.Allowed[ASyntaxTreeKind, ANodeKind]);
              end;
            tkNumberHex:
              begin
                TParserValueNode.Create(LNode, LOperator, TParserValue.Create(LToken.NumberHex));
                ExpectToken([tkEnd, tkSymbolOp, tkSymbolComma, tkSymbolParCl, tkSymbolBrackCl, tkSymbolBraceCl, tkSymbolAbs], TParserKeyword.Allowed[ASyntaxTreeKind, ANodeKind]);
              end;
            tkText:
              begin
                TParserValueNode.Create(LNode, LOperator, TParserValue.Create(LToken.Text));
                ExpectToken([tkEnd, tkSymbolOp, tkSymbolComma, tkSymbolParCl, tkSymbolBrackCl, tkSymbolBraceCl, tkSymbolAbs], TParserKeyword.Allowed[ASyntaxTreeKind, ANodeKind]);
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
                      ExpectToken([tkSymbolOp, tkSymbolRef, tkSymbolHash, tkSymbolParOp, tkSymbolBrackOp, tkSymbolBraceOp, tkSymbolBraceOp, tkSymbolAbs, tkNumberDec, tkNumberHex, tkText, tkName], TParserKeyword.BlockStarters + TParserKeyword.Allowed[ASyntaxTreeKind, ANodeKind]);
                    end;
                  kwIf:
                    begin
                      LNode := TParserIfNode.Create(LNode, LOperator);
                      ExpectToken([tkSymbolOp, tkSymbolRef, tkSymbolHash, tkSymbolParOp, tkSymbolBrackOp, tkSymbolBraceOp, tkSymbolBraceOp, tkSymbolAbs, tkNumberDec, tkNumberHex, tkText, tkName], TParserKeyword.BlockStarters + TParserKeyword.Allowed[ASyntaxTreeKind, ANodeKind]);
                      BuildParentNode(nkIf);
                      BuildParentNode(nkThen);
                      BuildParentNode(nkElse);
                    end;
                  kwTry:
                    begin
                      LNode := TParserTryNode.Create(LNode, LOperator);
                      ExpectToken([tkSymbolOp, tkSymbolRef, tkSymbolHash, tkSymbolParOp, tkSymbolBrackOp, tkSymbolBraceOp, tkSymbolBraceOp, tkSymbolAbs, tkNumberDec, tkNumberHex, tkText, tkName], TParserKeyword.BlockStarters + TParserKeyword.Allowed[ASyntaxTreeKind, ANodeKind]);
                      BuildParentNode(nkTry);
                      BuildParentNode(nkExcept);
                    end;
                  kwFunc:
                    begin
                      LDelegateFunctParams := TList<TParserParam>.Create;
                      try
                        BuildFunctParams(LDelegateFunctParams, True);
                        ExpectToken([tkSymbolOp, tkSymbolRef, tkSymbolHash, tkSymbolParOp, tkSymbolParCl, tkSymbolBrackOp, tkSymbolBrackCl, tkSymbolBraceOp, tkSymbolBraceCl, tkNumberDec, tkNumberHex, tkText, tkName], TParserKeyword.BlockStarters + TParserKeyword.Allowed[ASyntaxTreeKind, ANodeKind]);
                        LDelegateFunct := TParserCustomFunction.Create(LObjectName, LDelegateFunctParams.ToArray);
                        try
                          LDelegateSyntaxTree := TParserTree.Create([LDelegateFunct, Self]);
                          try
                            BuildTree(LDelegateSyntaxTree, stFuncBody);
                          except
                            LDelegateSyntaxTree.Free;
                            raise;
                          end;
                          if Options.Optimization then
                          begin
                            LSyntaxTree.Optimize;
                          end;
                          LDelegateFunct.SyntaxTree := LDelegateSyntaxTree;
                          TParserValueNode.Create(LNode, LOperator, TParserValue.Create(TParserFunctionDelegate.Create(LDelegateFunct)));
                        except
                          LDelegateFunct.Free;
                          raise;
                        end;
                      finally
                        LDelegateFunctParams.Free;
                      end;
                    end
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
                      ExpectToken([tkEnd, tkSymbolOp, tkSymbolParOp, tkSymbolParCl, tkSymbolBrackCl,  tkSymbolBraceCl, tkSymbolAbs, tkSymbolComma], TParserKeyword.Allowed[ASyntaxTreeKind, ANodeKind]);
                      if LToken.Kind = tkSymbolParOp then
                      begin
                        LNode := LNewNode;
                        ExpectToken([tkSymbolOp, tkSymbolRef, tkSymbolHash, tkSymbolParOp, tkSymbolParCl, tkSymbolBrackOp, tkSymbolBraceOp, tkSymbolAbs, tkNumberDec, tkNumberHex, tkText, tkName], TParserKeyword.BlockStarters + TParserKeyword.Allowed[ASyntaxTreeKind, nkArgs]);
                        BuildParentNode(nkArgs);
                      end;
                    end;
                end;
              end;
          end;
        end;

      begin
        LDeRef := False;
        case ANodeKind of
          nkArgs, nkElements, nkIf, nkThen, nkElse, nkTry, nkExcept:
            begin
              LNode := TParserArgNode.Create(LNode);
            end;
          nkFields:
            begin
              case LToken.Kind of
                tkSymbolParOp:
                  begin
                    LNode := TParserDynamicNamedArgNode.Create(LNode);
                    ExpectToken([tkEnd, tkSymbolOp, tkSymbolRef, tkSymbolHash, tkSymbolParOp, tkSymbolBrackOp, tkSymbolBraceOp, tkSymbolAbs, tkNumberDec, tkNumberHex, tkText, tkName], TParserKeyword.BlockStarters + TParserKeyword.Allowed[ASyntaxTreeKind, nkFieldName]);
                    BuildParentNode(nkFieldName);
                    ExpectToken([tkSymbolOp, tkSymbolRef, tkSymbolHash, tkSymbolParOp, tkSymbolBrackOp, tkSymbolBraceOp, tkSymbolBraceCl, tkSymbolAbs, tkNumberDec, tkNumberHex, tkText, tkName], TParserKeyword.BlockStarters + TParserKeyword.Allowed[ASyntaxTreeKind, nkFields]);
                    LNode := TParserArgNode.Create(LNode);
                  end;
                tkName:
                  begin
                    LNode := TParserStaticNamedArgNode.Create(LNode, LToken.Name);
                    ExpectToken([tkSymbolEq]);
                    ExpectToken([tkSymbolOp, tkSymbolRef, tkSymbolHash, tkSymbolParOp, tkSymbolBrackOp, tkSymbolBraceOp, tkSymbolBraceCl, tkSymbolAbs, tkNumberDec, tkNumberHex, tkText, tkName], TParserKeyword.BlockStarters + TParserKeyword.Allowed[ASyntaxTreeKind, nkFields]);
                  end;
                else
                  begin
                    LNode := TParserArgNode.Create(LNode);
                  end;
              end;
            end;
          nkFieldName:
            begin
              LNode := TParserArgNode.Create(LNode);
            end;
        end;
        repeat
          case LToken.Kind of
            tkSymbolParCl:
              begin
                if not Assigned(LNode.Parent) or not (ANodeKind in [nkNone, nkArgs, nkFieldName]) or ((ANodeKind = nkArgs) and not Assigned(LNode.Parent.Parent)) then
                begin
                  if ASyntaxTreeKind = stFuncBody then
                  begin
                    Exit;
                  end;
                  if (ASyntaxTreeKind <> stParam) and not (ANodeKind in [nkElse, nkExcept]) then
                  begin
                    raise EParserParenthesisError.CreateFmt('Unexpected parenthesis at %d', [LToken.CharIndex]);
                  end;
                  Break;
                end;
                if ANodeKind = nkArgs then
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
                ExpectToken(TParserTokenKind.AllowedAfter[ANodeKind], TParserKeyword.Allowed[ASyntaxTreeKind, TParserNodeKind.Create(LNode)]);
                Exit;
              end;
            tkSymbolBrackCl:
              begin
                if (not Assigned(LNode.Parent) or (ANodeKind <> nkElements) or ((ANodeKind = nkElements) and not Assigned(LNode.Parent.Parent))) and not (ANodeKind in [nkElse, nkExcept]) then
                begin
                  if ASyntaxTreeKind = stFuncBody then
                  begin
                    Exit;
                  end;
                  raise EParserParenthesisError.CreateFmt('Unexpected parenthesis at %d', [LToken.CharIndex]);
                end;
                if ANodeKind = nkElements then
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
                ExpectToken([tkEnd, tkSymbolOp, tkSymbolComma, tkSymbolParCl, tkSymbolBrackCl, tkSymbolBraceCl, tkSymbolAbs], TParserKeyword.Allowed[ASyntaxTreeKind, TParserNodeKind.Create(LNode)]);
                Exit;
              end;
            tkSymbolBraceCl:
              begin
                if (not Assigned(LNode.Parent) or (ANodeKind <> nkFields) or ((ANodeKind = nkFields) and not Assigned(LNode.Parent.Parent))) and not (ANodeKind in [nkElse, nkExcept]) then
                begin
                  if ASyntaxTreeKind = stFuncBody then
                  begin
                    Exit;
                  end;
                  raise EParserParenthesisError.CreateFmt('Unexpected parenthesis at %d', [LToken.CharIndex]);
                end;
                if ANodeKind = nkFields then
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
                if LNode is TParserDynamicNamedArgNode then
                begin
                  LNode := LNode.Parent;
                end;
                LNode := LNode.Parent;
                ExpectToken([tkEnd, tkSymbolOp, tkSymbolComma, tkSymbolParCl, tkSymbolBrackCl, tkSymbolBraceCl, tkSymbolAbs], TParserKeyword.Allowed[ASyntaxTreeKind, TParserNodeKind.Create(LNode)]);
                Exit;
              end;
            tkSymbolAbs:
              begin
                if ANodeKind = nkAbs then
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
                  ExpectToken([tkEnd, tkSymbolOp, tkSymbolComma, tkSymbolParCl, tkSymbolBrackCl, tkSymbolAbs], TParserKeyword.Allowed[ASyntaxTreeKind, TParserNodeKind.Create(LNode)]);
                  Exit;
                end else
                begin
                  if ASyntaxTreeKind = stFuncBody then
                  begin
                    Exit;
                  end;
                  if LNode.Count <> 0 then
                  begin
                    raise EParserParenthesisError.CreateFmt('Unexpected parenthesis at %d', [LToken.CharIndex]);
                  end;
                  BuildSubNode;
                end;
              end;
            tkSymbolComma:
              begin
                case ANodeKind of
                  nkArgs, nkElements:
                    begin
                      LNode := TParserArgNode.Create(LNode.Parent);
                    end;
                  nkFields:
                    begin
                      if LNode.Parent is TParserDynamicNamedArgNode then
                      begin
                        LNode := LNode.Parent;
                      end;
                      ExpectToken([tkName, tkSymbolParOp]);
                      case LToken.Kind of
                        tkSymbolParOp:
                          begin
                            LNode := TParserDynamicNamedArgNode.Create(LNode.Parent);
                            ExpectToken([tkEnd, tkSymbolOp, tkSymbolRef, tkSymbolHash, tkSymbolParOp, tkSymbolBrackOp, tkSymbolBraceOp, tkSymbolAbs, tkNumberDec, tkNumberHex, tkText, tkName], TParserKeyword.BlockStarters + TParserKeyword.Allowed[ASyntaxTreeKind, nkFieldName]);
                            BuildParentNode(nkFieldName);
                            LNode := TParserArgNode.Create(LNode);
                          end;
                        tkName:
                          begin
                            LNode := TParserStaticNamedArgNode.Create(LNode.Parent, LToken.Name);
                            ExpectToken([tkSymbolEq]);
                          end;
                      end;
                    end;
                  else
                    begin
                      case ASyntaxTreeKind of
                        stNone:
                          begin
                            raise EParserCommaError.CreateFmt('Unexpected separator at %d', [LToken.CharIndex]);
                          end;
                        stFuncBody:
                          begin
                            Exit;
                          end;
                        else
                          begin
                            Break;
                          end;
                      end;
                    end;
                end;
                ExpectToken([tkSymbolOp, tkSymbolRef, tkSymbolHash, tkSymbolParOp, tkSymbolBrackOp, tkSymbolBraceOp, tkNumberDec, tkNumberHex, tkText, tkName], TParserKeyword.BlockStarters + TParserKeyword.Allowed[ASyntaxTreeKind, ANodeKind]);
              end;
            tkName:
              begin
                case LToken.Keyword of
                  kwThen, kwElse, kwExcept:
                    begin
                      LNode := LNode.Parent;
                      ExpectToken([tkSymbolParOp, tkSymbolBrackOp, tkSymbolBraceOp, tkSymbolBraceOp, tkSymbolAbs, tkNumberDec, tkNumberHex, tkText, tkName], TParserKeyword.BlockStarters + TParserKeyword.Allowed[stNone, nkNone]);
                      Exit;
                    end;
                  kwRet:
                    begin
                      Exit;
                    end
                  else
                    begin
                      BuildSubNode;
                    end;
                end;
              end;
            else
              begin
                BuildSubNode;
              end;
          end;
        until LToken.Kind = tkEnd;
        if ANodeKind in [nkElse, nkExcept] then
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
      LNode := ATree.Nodes;
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

    procedure BuildFunctParams(var AFunctParams: TList<TParserParam>; const ADelegate: Boolean = False);
    const
      LTreeKinds: array [Boolean] of TParserSyntaxTreeKind = (
        stParam, stFuncParam
      );
      LEndingTokens: array [Boolean] of TParserTokenKind = (
        tkSymbolParCl, tkName
      );
    var
      LDefValSyntaxTree: TParserTree;
      LNames: TStringList;
      LName: String;
      LType: IParserValueConstraint;
    begin
      if not ADelegate then
      begin
        ExpectToken([tkSymbolEq, tkSymbolParOp]);
      end;
      if ADelegate or (LToken.Kind = tkSymbolParOp) then
      begin
        LNames := TStringList.Create;
        try
          repeat
            ExpectToken([LEndingTokens[ADelegate], tkName], TParserKeyword.Allowed[LTreeKinds[ADelegate], nkNone]);
            if (LToken.Kind = tkName) and (LToken.Keyword <> kwRet) then
            begin
              repeat
                LNames.Add(LToken.Name);
                ExpectToken([tkSymbolComma, tkSymbolEq, tkSymbolColon, tkSymbolAmp, LEndingTokens[ADelegate]], TParserKeyword.Allowed[LTreeKinds[ADelegate], nkNone]);
                if LToken.Kind = tkSymbolAmp then
                begin
                  ExpectToken([tkName]);
                end;
              until (LToken.Kind <> tkName) or (LToken.Keyword = kwRet);
              if LToken.Kind = tkSymbolColon then
              begin
                ExpectToken([tkName], TParserKeyword.TypeConstructors + [kwNone]);
                LTypeClass := TParserType.TypeClasses[LToken.Keyword];
                if Assigned(LTypeClass) then
                begin
                  ParseType([tkSymbolComma, tkSymbolEq, LEndingTokens[ADelegate]]);
//                      LType := TParserTempType.Create(LType as TParserType);
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
                ExpectToken([tkSymbolOp, tkSymbolRef, tkSymbolHash, tkSymbolParOp, tkSymbolParCl, tkSymbolBrackOp, tkSymbolBrackCl, tkNumberDec, tkNumberHex, tkText, tkName], TParserKeyword.BlockStarters + TParserKeyword.Allowed[LTreeKinds[ADelegate], nkNone]);
                LDefValSyntaxTree := TParserTree.Create([Self]);
                try
                  BuildTree(LDefValSyntaxTree, LTreeKinds[ADelegate]);
                  LValue := LDefValSyntaxTree.Calculate;
                finally
                  LDefValSyntaxTree.Free;
                end;
                for LName in LNames do
                begin
                  AFunctParams.Add(TParserParam.Create(LName, LValue, LType));
                end;
              end else
              begin
                for LName in LNames do
                begin
                  AFunctParams.Add(TParserParam.Create(LName, LType));
                end;
              end;
              LNames.Clear;
              LType := Default(IParserValueConstraint);
            end;
          until (LToken.Kind = LEndingTokens[ADelegate]) and (not ADelegate or (LToken.Keyword = kwRet));
        finally
          LNames.Free;
        end;
        if not ADelegate then
        begin
          ExpectToken([tkSymbolEq]);
        end;
      end;
    end;

    procedure ParseType(const AAllowedFollowTokens: TParserTokenKinds; AName: String = String.Empty);
    begin
//      ExpectToken(AAllowedFollowTokens + [tkSymbolParOp]);
//      if LToken.Kind = tkSymbolParOp then
//      begin
//        repeat
//          ExpectToken([tkSymbolParCl, tkName]);
//          if LToken.Kind = tkName then
//          begin
//            ExpectToken([tkSymbolComma, tkSymbolParCl]);
//          end;
//        until LToken.Kind = tkSymbolParCl;
//        ExpectToken(AAllowedFollowTokens);
//      end;
//      LType := LTypeClass.Create(AName);
    end;

  begin
    if not (Result.ExpressionKind in Options.Allowed) then
    begin
      if Result.ExpressionKind = exTerm then
      begin
        raise EParserCommandError.Create('Expression not allowed');
      end;
      raise EParserCommandError.CreateFmt('Command %s not allowed', [LToken.Name.QuotedString]);
    end;
    case Result.ExpressionKind of
      exTerm:
        begin
          LSyntaxTree := TParserTree.Create([Self]);
          try
            BuildTree(LSyntaxTree);
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
            ExpectToken([tkSymbolOp, tkSymbolRef, tkSymbolHash, tkSymbolParOp, tkSymbolParCl, tkSymbolBrackOp, tkSymbolBrackCl, tkSymbolBraceOp, tkSymbolBraceCl, tkNumberDec, tkNumberHex, tkText, tkName], TParserKeyword.BlockStarters + TParserKeyword.Allowed[stNone, nkNone]);
            LSyntaxTree := TParserTree.Create([Self]);
            try
              BuildTree(LSyntaxTree);
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
              ExpectToken([tkSymbolOp, tkSymbolRef, tkSymbolHash, tkSymbolParOp, tkSymbolParCl, tkSymbolBrackOp, tkSymbolBrackCl, tkNumberDec, tkNumberHex, tkText, tkName], TParserKeyword.BlockStarters + TParserKeyword.Allowed[stNone, nkNone]);
              LSyntaxTree := TParserTree.Create([Self]);
              try
                BuildTree(LSyntaxTree);
                LValue := LSyntaxTree.Calculate;
              finally
                LSyntaxTree.Free;
              end;
            end else
            begin
              LValue := TParserValue.Empty[Options.InitialType];
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
          LFunctParams := TList<TParserParam>.Create;
          try
            BuildFunctParams(LFunctParams);
            ExpectToken([tkSymbolOp, tkSymbolRef, tkSymbolHash, tkSymbolParOp, tkSymbolParCl, tkSymbolBrackOp, tkSymbolBrackCl, tkSymbolBraceOp, tkSymbolBraceCl, tkNumberDec, tkNumberHex, tkText, tkName], TParserKeyword.BlockStarters + TParserKeyword.Allowed[stNone, nkNone]);
            LFunct := TParserCustomFunction.Create(LObjectName, LFunctParams.ToArray);
            try
              LSyntaxTree := TParserTree.Create([LFunct, Self]);
              try
                BuildTree(LSyntaxTree);
              except
                LSyntaxTree.Free;
                raise;
              end;
              if Options.Optimization then
              begin
                LSyntaxTree.Optimize;
              end;
              LFunct.SyntaxTree := LSyntaxTree;
              case Result.ExpressionKind of
                exDeclFunction:
                  begin
                    Dictionary.Add(LFunct);
                  end;
                exDeclConstructor:
                  begin
                    if not (Dictionary[LFunct.Name] is TParserType) then
                    begin
                      raise EParserNoTypeError.CreateFmt('%s is not a type', [LFunct.Name]);
                    end;
                    (Dictionary[LFunct.Name] as TParserType).&Constructor := LFunct;
                  end;
              end;
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
//          Dictionary.Add(LType as TParserType);
        end;
      exDeclInline:
        begin
//          ExpectToken([tkName]);
//          LObjectName := LToken.Name;
//          ExpectToken([tkSymbolEq]);
//          ExpectToken([tkSymbolOp, tkSymbolRef, tkSymbolHash, tkSymbolParOp, tkSymbolParCl, tkSymbolBrackOp, tkSymbolBrackCl, tkSymbolBraceOp, tkSymbolBraceCl, tkNumberDec, tkNumberHex, tkText, tkName], TParserKeyword.Allowed[nkNone]);
//          LSyntaxTree := TParserTree.Create([LFunct, Self]);
//          try
//            BuildTree;
//          except
//            LSyntaxTree.Free;
//            raise;
//          end;
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
      exOption:
        begin
          LObjectNames := TStringList.Create;
          try
            ParseObjectNames([tkSymbolEq]);
            ExpectToken([tkSymbolOp, tkSymbolRef, tkSymbolHash, tkSymbolParOp, tkSymbolParCl, tkSymbolBrackOp, tkSymbolBrackCl, tkNumberDec, tkNumberHex, tkText, tkName], TParserKeyword.BlockStarters + TParserKeyword.Allowed[stNone, nkNone]);
            LSyntaxTree := TParserTree.Create([Self]);
            try
              BuildTree(LSyntaxTree);
              LValue := LSyntaxTree.Calculate;
            finally
              LSyntaxTree.Free;
            end;
            for LObjectName in LObjectNames do
            begin
              TParserOption.Create(LObjectName).Define(Self, LValue);
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
            ExpectToken([tkSymbolOp, tkSymbolRef, tkSymbolHash, tkSymbolParOp, tkSymbolParCl, tkSymbolBrackOp, tkSymbolBrackCl, tkSymbolBraceOp, tkSymbolBraceCl, tkNumberDec, tkNumberHex, tkText, tkName], TParserKeyword.BlockStarters + TParserKeyword.Allowed[stNone, nkNone]);
            LSyntaxTree := TParserTree.Create([Self]);
            try
              BuildTree(LSyntaxTree);
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
            for LObjectName in LObjectNames do
            begin
              LoadPackage(LObjectName);
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
    ExpectToken([tkEnd, tkSymbolOp, tkSymbolRef, tkSymbolHash, tkSymbolParOp, tkSymbolParCl, tkSymbolBrackOp, tkSymbolBrackCl, tkSymbolBraceOp, tkSymbolBraceCl, tkSymbolAbs, tkNumberDec, tkNumberHex, tkText, tkName], TParserKeyword.ExpressionStarters + TParserKeyword.BlockStarters + TParserKeyword.Allowed[stNone, nkNone]);
    Result.FExpressionKind := TParserExpressionKind.Create(LToken);
    EvaluateExpression;
    if Options.Warnings then
    begin
      Result.FWarnings := LWarnings.ToStringArray;
    end;
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
  for LIndex := Pred(Options.PackagePaths.Count) downto 0 do
  begin
    LFileName := ChangeFileExt(Concat(Options.PackagePaths[LIndex], AName), TParserCustomPackage.FileExtension);
    if FileExists(LFileName) then
    begin
      RegisterPackage(TParserCustomPackage.Create(LFileName));
    end;
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
