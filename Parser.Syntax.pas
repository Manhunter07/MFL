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

unit Parser.Syntax;

interface

uses
  System.SysUtils, System.StrUtils, System.Types, System.Math, System.Rtti, System.Generics.Collections, System.Generics.Defaults,
  Parser.Exception, Parser.Value;

type
  TParserNode = class;

  TParserNodeKind = (nkNone, nkAbs, nkArgs, nkElements, nkFields, nkFieldName, nkIf, nkThen, nkElse, nkTry, nkExcept);

  TParserNodeKindHelper = record helper for TParserNodeKind
  public
    constructor Create(const ANode: TParserNode); overload;
  end;

  TParserSyntaxTreeKind = (stNone, stParam, stFuncParam, stFuncBody);

  TParserKeyword = (kwNone, kw_, kwResolve, kwConstant, kwVariable, kwInline, kwFunction, kwType, kwConstructor, kwOpt, kwRange, kwEnum, kwInt, kwRangeInt, kwAttrib, kwAlias, kwShow, kwDelete, kwAssert, kwLink, kwSpread, kwIf, kwThen, kwElse, kwTry, kwExcept, kwFunc, kwRet);

  TParserKeywords = set of TParserKeyword;

  TParserKeywordHelper = record helper for TParserKeyword
  private const
    FKeywords: array [Succ(Low(TParserKeyword)) .. High(TParserKeyword)] of String = ('_', 'resolve', 'const', 'var', 'inline', 'function', 'type', 'constructor', 'opt', 'range', 'enum', 'int', 'rangeint', 'attrib', 'alias', 'show', 'delete', 'assert', 'link', 'spread', 'if', 'then', 'else', 'try', 'except', 'func', 'ret');
    class function GetTypeConstructors: TParserKeywords; static;
    class function GetExpressionStarters: TParserKeywords; static;
    class function GetBlockStarters: TParserKeywords; static;
    class function GetAllowed(const ATreeKind: TParserSyntaxTreeKind; const ANodeKind: TParserNodeKind): TParserKeywords; static;
  public
    constructor Create(const AName: String);
    class property Allowed[const ATreeKind: TParserSyntaxTreeKind; const ANodeKind: TParserNodeKind]: TParserKeywords read GetAllowed;
    class property ExpressionStarters: TParserKeywords read GetExpressionStarters;
    class property BlockStarters: TParserKeywords read GetBlockStarters;
    class property TypeConstructors: TParserKeywords read GetTypeConstructors;
    function ToString: String;
  end;

  TParserOperator = (opAdd, opSub, opMul, opDiv, opMod, opExp, opRnd, opCmp);

  TParserOperators = set of TParserOperator;

  TParserOperatorPrecedence = 0 .. 3;

  TParserOperatorHelper = record helper for TParserOperator
  private
    class function GetOperators(const APrecedence: TParserOperatorPrecedence): TParserOperators; static;
    class function GetPrecedences(const AOperator: TParserOperator): TParserOperatorPrecedence; static;
    class function GetUnaryOperators: TParserOperators; static;
    class function GetBinaryOperators: TParserOperators; static;
  public
    class property UnaryOperators: TParserOperators read GetUnaryOperators;
    class property BinaryOperators: TParserOperators read GetBinaryOperators;
    class property Operators[const APrecedence: TParserOperatorPrecedence]: TParserOperators read GetOperators;
    class property Precedences[const AOperator: TParserOperator]: TParserOperatorPrecedence read GetPrecedences;
    constructor Create(const ASymbol: Char);
    function ToChar: Char;
    function Invoke(const AFirst, ASecond: TParserValue): TParserValue; overload;
    function Invoke(const AValue: TParserValue): TParserValue; overload;
    function Supported(const AValue: TParserValue): Boolean; overload;
    function Supported(const AFirst, ASecond: TParserValue): Boolean; overload;
  end;

  TParserParentNode = class;

  TParserTree = class;

  TParserNode = class abstract
  private
    FSyntaxTree: TParserTree;
    FParent: TParserParentNode;
    FOperator: TParserOperator;
  protected
    function GetValue: TParserValue; virtual; abstract;
  public
    property SyntaxTree: TParserTree read FSyntaxTree;
    property Parent: TParserParentNode read FParent;
    property &Operator: TParserOperator read FOperator;
    property Value: TParserValue read GetValue;
    constructor Create(const AParent: TParserParentNode; const AOperator: TParserOperator); overload;
  end;

  TParserParentNode = class(TParserNode)
  private
    FNodes: TObjectList<TParserNode>;
    function GetNodes(const AIndex: Integer): TParserNode;
    function GetCount: Integer;
  protected
    function GetValue: TParserValue; override;
    procedure Add(const ANode: TParserNode); virtual;
    function ChildrenValues: TArray<TParserValue>;
  public
    property Nodes[const AIndex: Integer]: TParserNode read GetNodes; default;
    property Count: Integer read GetCount;
    constructor Create(const AParent: TParserParentNode; const AOperator: TParserOperator);
    destructor Destroy; override;
    function GetEnumerator: TEnumerator<TParserNode>; inline;
    procedure Clear;
  end;

  TParserTypeNode = class(TParserParentNode)
  private
    FKeyword: TParserKeyword;
  protected
    function GetValue: TParserValue; override;
  public
    property Keyword: TParserKeyword read FKeyword;
    constructor Create(const AParent: TParserParentNode; const AKeyword: TParserKeyword);
  end;

  TParserAbsNode = class(TParserParentNode)
  protected
    function GetValue: TParserValue; override;
  end;

  TParserRootNode = class(TParserParentNode)
  public
    constructor Create(const ASyntaxTree: TParserTree);
  end;

  TParserNamedNode = class(TParserParentNode)
  private
    FName: String;
  protected
    function GetValue: TParserValue; override;
  public
    property Name: String read FName;
    constructor Create(const AParent: TParserParentNode; const AOperator: TParserOperator; const AName: String);
  end;

  TParserArrayNode = class(TParserParentNode)
  protected
    function GetValue: TParserValue; override;
  end;

  TParserRecordNode = class(TParserParentNode)
  protected
    function GetValue: TParserValue; override;
    function ChildrenNames: TArray<String>;
  end;

  TParserArgNode = class(TParserParentNode)
  private
    FSpread: Boolean;
  public
    property Spread: Boolean read FSpread write FSpread;
    constructor Create(const AParent: TParserParentNode);
  end;

  TParserNamedArgNode = class abstract(TParserArgNode)
  protected
    function GetName: String; virtual; abstract;
  public
    property Name: String read GetName;
  end;

  TParserStaticNamedArgNode = class(TParserNamedArgNode)
  private
    FName: String;
  protected
    function GetName: String; override;
  public
    constructor Create(const AParent: TParserParentNode; const AName: String);
  end;

  TParserDynamicNamedArgNode = class(TParserNamedArgNode)
  protected
    function GetValue: TParserValue; override;
    function GetName: String; override;
  end;

  TParserValueNode = class(TParserNode)
  private
    FValue: TParserValue;
  protected
    function GetValue: TParserValue; override;
  public
    constructor Create(const AParent: TParserParentNode; const AOperator: TParserOperator; const AValue: TParserValue);
  end;

  TParserRefNode = class(TParserNode)
  private
    FName: String;
  protected
    function GetValue: TParserValue; override;
  public
    property Name: String read FName;
    constructor Create(const AParent: TParserParentNode; const AName: String);
  end;

  TParserDeRefNode = class(TParserNamedNode)
  protected
    function GetValue: TParserValue; override;
  public
    constructor Create(const AParent: TParserParentNode; const AName: String);
  end;

  TParserIfNode = class(TParserParentNode)
  protected
    function GetValue: TParserValue; override;
  end;

  TParserTryNode = class(TParserParentNode)
  protected
    function GetValue: TParserValue; override;
  end;

  TParserTree = class
  private
    FValueSuppliers: TArray<IParserValueSupplier>;
    FNodes: TParserParentNode;
    function GetValues(const AName: String; const AArgs: TArray<TParserValue>): TParserValue;
    function GetRefTargets(const AName: String): IParserValueRefTarget;
  protected
    property Values[const AName: String; const AArgs: TArray<TParserValue>]: TParserValue read GetValues;
    property RefTargets[const AName: String]: IParserValueRefTarget read GetRefTargets;
  public
    property Nodes: TParserParentNode read FNodes;
    constructor Create(const AValueSuppliers: TArray<IParserValueSupplier> = []);
    destructor Destroy; override;
    function GetEnumerator: TEnumerator<TParserNode>; inline;
    function Calculate: TParserValue;
    procedure Optimize;
  end;

implementation

uses
  Parser.Language;

{ TParserNodeKindHelper }

{$WARN NO_RETVAL OFF}
constructor TParserNodeKindHelper.Create(const ANode: TParserNode);
var
  LParent: TParserParentNode;
begin
  LParent := ANode.Parent;
  if LParent is TParserAbsNode then
  begin
    Self := nkAbs;
  end else
  begin
    if LParent is TParserNamedNode then
    begin
      Self := nkArgs;
    end else
    begin
      if LParent is TParserArrayNode then
      begin
        Self := nkElements;
      end else
      begin
        if LParent is TParserRecordNode then
        begin
           Self := nkFields;
        end else
        begin
          if LParent is TParserIfNode then
          begin
            case LParent.FNodes.IndexOf(ANode) of
              0:
                begin
                  Self := nkIf;
                end;
              1:
                begin
                  Self := nkThen;
                end;
              2:
                begin
                  Self := nkElse;
                end;
            end;
          end else
          begin
            if LParent is TParserTryNode then
            begin
              case LParent.FNodes.IndexOf(ANode) of
                0:
                  begin
                    Self := nkTry;
                  end;
                1:
                  begin
                    Self := nkExcept;
                  end;
              end;
            end else
            begin
              Self := nkNone;
            end;
          end;
        end;
      end;
    end;
  end;
end;
{$WARN NO_RETVAL ON}

{ TParserKeywordHelper }

constructor TParserKeywordHelper.Create(const AName: String);
begin
  Self := TParserKeyword(Succ(IndexText(AName, FKeywords)));
end;

class function TParserKeywordHelper.GetAllowed(const ATreeKind: TParserSyntaxTreeKind; const ANodeKind: TParserNodeKind): TParserKeywords;
const
  LAllowedTreeKeywords: array [TParserSyntaxTreeKind] of TParserKeywords = (
    [], [], [kwRet], []
  );
  LAllowedNodeKeywords: array [TParserNodeKind] of TParserKeywords = (
    [], [], [kwSpread], [], [], [], [kwThen], [kwElse], [], [kwExcept], []
  );
begin
  Result := LAllowedTreeKeywords[ATreeKind] + LAllowedNodeKeywords[ANodeKind];
  Include(Result, kwNone);
end;

class function TParserKeywordHelper.GetBlockStarters: TParserKeywords;
begin
  Result := [kwIf, kwTry, kwFunc];
end;

class function TParserKeywordHelper.GetExpressionStarters: TParserKeywords;
begin
  Result := [kwResolve, kwConstant, kwVariable, kwInline, kwFunction, kwType, kwConstructor, kwAlias, kwOpt, kwShow, kwDelete, kwAssert, kwLink];
end;

class function TParserKeywordHelper.GetTypeConstructors: TParserKeywords;
begin
  Result := [kwRange, kwEnum, kwInt, kwRangeInt];
end;

function TParserKeywordHelper.ToString: String;
begin
  Result := FKeywords[Self];
end;

{ TParserOperatorHelper }

{$WARN NO_RETVAL OFF}
constructor TParserOperatorHelper.Create(const ASymbol: Char);
begin
  case ASymbol of
    '+':
      begin
        Self := opAdd;
      end;
    '-':
      begin
        Self := opSub;
      end;
    '*':
      begin
        Self := opMul;
      end;
    '/':
      begin
        Self := opDiv;
      end;
    '%':
      begin
        Self := opMod;
      end;
    '^':
      begin
        Self := opExp;
      end;
    '~':
      begin
        Self := opRnd;
      end;
    '?':
      begin
        Self := opCmp;
      end;
  end;
end;
{$WARN NO_RETVAL ON}

class function TParserOperatorHelper.GetBinaryOperators: TParserOperators;
begin
  Result := [opAdd, opSub, opCmp, opMul, opDiv, opMod, opExp];
end;

class function TParserOperatorHelper.GetOperators(const APrecedence: TParserOperatorPrecedence): TParserOperators;
const
  LOperators: array [TParserOperatorPrecedence] of TParserOperators = ([opAdd, opSub], [opMul, opDiv, opMod], [opExp], [opRnd, opCmp]);
begin
  Result := LOperators[APrecedence];
end;

class function TParserOperatorHelper.GetPrecedences(const AOperator: TParserOperator): TParserOperatorPrecedence;
const
  LPrecedences: array [TParserOperator] of TParserOperatorPrecedence = (0, 0, 1, 1, 1, 2, 3, 3);
begin
  Result := LPrecedences[AOperator];
end;

class function TParserOperatorHelper.GetUnaryOperators: TParserOperators;
begin
  Result := [opAdd, opSub, opRnd, opCmp];
end;

{$WARN NO_RETVAL OFF}
function TParserOperatorHelper.Invoke(const AValue: TParserValue): TParserValue;
begin
  if not Supported(AValue) then
  begin
    raise EParserOperatorError.Create('Operand not supported');
  end;
  case Self of
    opAdd:
      begin
        Result := AValue;
      end;
    opSub:
      begin
        Result := AValue.Negate;
      end;
    opRnd:
      begin
        Result := TParserValue.Create(SimpleRoundTo(AValue.AsDouble, 0));
      end;
    opCmp:
      begin
        Result := TParserValue.Create(Sign(AValue.AsDouble));
      end;
  end;
end;
{$WARN NO_RETVAL ON}

{$WARN NO_RETVAL OFF}
function TParserOperatorHelper.Invoke(const AFirst, ASecond: TParserValue): TParserValue;
begin
  if not Supported(AFirst, ASecond) then
  begin
    raise EParserOperatorError.Create('Operand not supported');
  end;
  case Self of
    opAdd:
      begin
        Result := TParserValue.Add(AFirst, ASecond);
      end;
    opSub:
      begin
        Result := TParserValue.Subtract(AFirst, ASecond);
      end;
    opCmp:
      begin
        Result := TParserValue.Create(TParserValue.Compare(AFirst, ASecond));
      end;
    opMul:
      begin
        Result := TParserValue.Multiply(AFirst, ASecond);
      end;
    opDiv:
      begin
        Result := TParserValue.Divide(AFirst, ASecond);
      end;
    opMod:
      begin
        Result := TParserValue.Create(FMod(AFirst.AsDouble, ASecond.AsDouble));
      end;
    opExp:
      begin
        Result := TParserValue.Create(Power(AFirst.AsDouble, ASecond.AsDouble));
      end;
  end;
end;
{$WARN NO_RETVAL ON}

function TParserOperatorHelper.Supported(const AValue: TParserValue): Boolean;
const
  LSupportedUnaryOperators: array [TParserValueKind] of TParserOperators = (
    [opAdd],
    [opAdd, opSub, opRnd, opCmp],
    [opAdd, opSub],
    [opAdd, opSub],
    [opAdd, opSub]
  );
begin
  Result := Self in LSupportedUnaryOperators[AValue.Kind];
end;

function TParserOperatorHelper.Supported(const AFirst, ASecond: TParserValue): Boolean;
const
  LSupportedBinaryOperators: array [TParserValueKind] of TParserOperators = (
    [],
    [opAdd, opSub, opCmp, opMul, opDiv, opMod, opExp],
    [opAdd, opCmp],
    [opAdd, opCmp],
    [opAdd, opSub, opCmp]
  );
begin
  Result := (Self in LSupportedBinaryOperators[AFirst.Kind]) and (Self in LSupportedBinaryOperators[ASecond.Kind]);
end;

function TParserOperatorHelper.ToChar: Char;
const
  LOperators: array [TParserOperator] of Char = ('+', '-', '*', '/', '%', '^', '~', '?');
begin
  Result := LOperators[Self];
end;

{ TParserNode }

constructor TParserNode.Create(const AParent: TParserParentNode; const AOperator: TParserOperator);
begin
  inherited Create;
  if Assigned(AParent) then
  begin
    FParent := AParent;
    Parent.Add(Self);
    FSyntaxTree := Parent.SyntaxTree;
  end;
  FOperator := AOperator;
end;

{ TParserAbsNode }

function TParserAbsNode.GetValue: TParserValue;
begin
  Result := inherited GetValue.Absolute;
end;

{ TParserParentNode }

procedure TParserParentNode.Add(const ANode: TParserNode);
begin
  FNodes.Add(ANode);
end;

function TParserParentNode.ChildrenValues: TArray<TParserValue>;
var
  LValues: TList<TParserValue>;
  LNode: TParserNode;
begin
  LValues := TList<TParserValue>.Create;
  try
    for LNode in Self do
    begin
      if (LNode as TParserArgNode).Spread then
      begin
        LValues.AddRange(LNode.Value.AsArray);
      end else
      begin
        LValues.Add(LNode.Value);
      end;
    end;
    Result := LValues.ToArray;
  finally
    LValues.Free;
  end;
end;

procedure TParserParentNode.Clear;
begin
  FNodes.Clear;
end;

constructor TParserParentNode.Create(const AParent: TParserParentNode; const AOperator: TParserOperator);
begin
  inherited;
  FNodes := TObjectList<TParserNode>.Create;
end;

destructor TParserParentNode.Destroy;
begin
  FNodes.Free;
  inherited;
end;

function TParserParentNode.GetCount: Integer;
begin
  Result := FNodes.Count;
end;

function TParserParentNode.GetEnumerator: TEnumerator<TParserNode>;
begin
  Result := FNodes.GetEnumerator;
end;

function TParserParentNode.GetNodes(const AIndex: Integer): TParserNode;
begin
  Result := FNodes[AIndex];
end;

function TParserParentNode.GetValue: TParserValue;
var
  LIndex: Integer;
  LNode: TParserNode;

  function NodeValue(const ANode: TParserNode): TParserValue;
  var
    LNextNode: TParserNode;
  begin
//    Result := ANode.Value;
//    if LIndex <> Pred(Count) then
//    begin
//      LNextNode := Self[Succ(LIndex)];
//      if TParserOperator.Precedences[LNextNode.Operator] > TParserOperator.Precedences[ANode.Operator] then
//      begin
//        Inc(LIndex);
//        Result := LNextNode.Operator.Invoke(Result, NodeValue(LNextNode));
//      end;
//    end;
    Result := ANode.Value;
    while LIndex < Pred(Count) do
    begin
      LNextNode := Self[Succ(LIndex)];
      case CompareValue(TParserOperator.Precedences[LNextNode.Operator], TParserOperator.Precedences[ANode.Operator]) of
        LessThanValue:
          begin
            Exit;
          end;
        EqualsValue, GreaterThanValue:
          begin
            Inc(LIndex);
            Result := LNextNode.Operator.Invoke(Result, NodeValue(LNextNode));
          end;
      end;
    end;
  end;

begin
  inherited;
  if Count = 0 then
  begin
    Result := TParserValue.Empty[vkDouble];
  end else
  begin
    LIndex := 0;
    LNode := Nodes[0];
    Result := LNode.&Operator.Invoke(NodeValue(LNode));
//    if Count > 1 then
//    begin
//      Inc(LIndex);
//      while LIndex < Count do
//      begin
//        LNode := Nodes[LIndex];
//        Result := LNode.&Operator.Invoke(Result, NodeValue(LNode));
//        Inc(LIndex);
//      end;
//    end;
  end;
end;

{ TParserRootNode }

constructor TParserRootNode.Create(const ASyntaxTree: TParserTree);
begin
  inherited Create(Default(TParserTree), opAdd);
  FSyntaxTree := ASyntaxTree;
end;

{ TParserNamedNode }

constructor TParserNamedNode.Create(const AParent: TParserParentNode; const AOperator: TParserOperator; const AName: String);
begin
  inherited Create(AParent, AOperator);
  FName := AName;
end;

function TParserNamedNode.GetValue: TParserValue;
begin
  Result := SyntaxTree.Values[Name, ChildrenValues];
end;

{ TParserTypeNode }

constructor TParserTypeNode.Create(const AParent: TParserParentNode; const AKeyword: TParserKeyword);
begin
  inherited Create(AParent, opAdd);
  FKeyword := AKeyword;
end;

function TParserTypeNode.GetValue: TParserValue;
var
  LArgs: TArray<TParserValue>;

  procedure AssertArgCount(const AMin, AMax: Integer);
  begin
    if Length(LArgs) < AMin then
    begin
      raise EParserTypeConstructionError.Create('Not enough arguments');
    end;
    if (AMax <> -1) and (Length(LArgs) > AMax) then
    begin
      if AMax = 0 then
      begin
        raise EParserTypeConstructionError.Create('No arguments expected');
      end;
      raise EParserTypeConstructionError.Create('Too many arguments');
    end;
  end;

begin
  LArgs := ChildrenValues;
  case Keyword of
    kwRange:
      begin
        AssertArgCount(2, 2);
        Result := TParserValue.Create(TParserTypeDelegate.Create(TParserRangeType.Create(String.Empty, LArgs[0], LArgs[1])));
      end;
    kwEnum:
      begin
        AssertArgCount(1, -1);
        Result := TParserValue.Create(TParserTypeDelegate.Create(TParserEnumType.Create(String.Empty, LArgs)));
      end;
    kwInt:
      begin
        AssertArgCount(0, -1);
        Result := TParserValue.Create(TParserTypeDelegate.Create(TParserIntegerType.Create(String.Empty, LArgs)));
      end;
    kwRangeInt:
      begin
        AssertArgCount(2, -1);
        Result := TParserValue.Create(TParserTypeDelegate.Create(TParserRangeIntegerType.Create(String.Empty, LArgs[0], LArgs[1], Copy(LArgs, 2, Length(LArgs) - 2))));
      end;
  end;
end;

{ TParserArrayNode }

function TParserArrayNode.GetValue: TParserValue;
begin
  Result := TParserValue.Create(ChildrenValues);
end;

{ TParserRecordNode }

function TParserRecordNode.ChildrenNames: TArray<String>;
var
  LIndex: Integer;
begin
  SetLength(Result, Count);
  for LIndex := 0 to Pred(Count) do
  begin
    Result[LIndex] := (Nodes[LIndex] as TParserNamedArgNode).Name;
  end;
end;

function TParserRecordNode.GetValue: TParserValue;
var
  LResult: TArray<TPair<String, TParserValue>>;
  LNames: TArray<String>;
  LValues: TArray<TParserValue>;
  LIndex: Integer;
begin
  SetLength(LResult, Count);
  LValues := ChildrenValues;
  LNames := ChildrenNames;
  for LIndex := Low(LResult) to High(LResult) do
  begin
    LResult[LIndex] := TPair<String, TParserValue>.Create(LNames[LIndex], LValues[LIndex]);
  end;
  Result := TParserValue.Create(LResult);
end;

{ TParserArgNode }

constructor TParserArgNode.Create(const AParent: TParserParentNode);
begin
  inherited Create(AParent, opAdd);
end;

{ TParserStaticNamedArgNode }

constructor TParserStaticNamedArgNode.Create(const AParent: TParserParentNode; const AName: String);
begin
  inherited Create(AParent);
  FName := AName;
end;

function TParserStaticNamedArgNode.GetName: String;
begin
  Result := FName;
end;

{ TParserDynamicNamedArgNode }

function TParserDynamicNamedArgNode.GetName: String;
begin
  Result := Self[0].Value.AsString;
end;

function TParserDynamicNamedArgNode.GetValue: TParserValue;
begin
  Result := Self[1].Value;
end;

{ TParserValueNode }

constructor TParserValueNode.Create(const AParent: TParserParentNode; const AOperator: TParserOperator; const AValue: TParserValue);
begin
  inherited Create(AParent, AOperator);
  FValue := AValue;
end;

function TParserValueNode.GetValue: TParserValue;
begin
  inherited;
  Result := FValue;
end;

{ TParserRefNode }

constructor TParserRefNode.Create(const AParent: TParserParentNode; const AName: String);
begin
  inherited Create(AParent, opAdd);
  FName := AName;
end;

function TParserRefNode.GetValue: TParserValue;
begin
  Result := TParserValue.Create(SyntaxTree.RefTargets[Name]);
end;

{ TParserDeRefNode }

constructor TParserDeRefNode.Create(const AParent: TParserParentNode; const AName: String);
begin
  inherited Create(AParent, opAdd, AName);
end;

function TParserDeRefNode.GetValue: TParserValue;
begin
  Result := SyntaxTree.Values[Name, []].AsReference.Value[ChildrenValues];
end;

{ TParserIfNode }

function TParserIfNode.GetValue: TParserValue;
begin
  if not Nodes[0].Value.IsEmpty then
  begin
    Result := Nodes[1].Value;
  end else
  begin
    Result := Nodes[2].Value;
  end;
end;

{ TParserTryNode }

function TParserTryNode.GetValue: TParserValue;
begin
  try
    Result := Nodes[0].Value;
  except
    Result := Nodes[1].Value;
  end;
end;

{ TParserTree }

function TParserTree.Calculate: TParserValue;
begin
  Result := Nodes.Value;
end;

constructor TParserTree.Create(const AValueSuppliers: TArray<IParserValueSupplier> = []);
begin
  inherited Create;
  FValueSuppliers := AValueSuppliers;
  FNodes := TParserRootNode.Create(Self);
end;

destructor TParserTree.Destroy;
begin
  Nodes.Free;
  inherited;
end;

function TParserTree.GetEnumerator: TEnumerator<TParserNode>;
begin
  Result := FNodes.GetEnumerator;
end;

function TParserTree.GetRefTargets(const AName: String): IParserValueRefTarget;
var
  LSupplier: IParserValueSupplier;
begin
  for LSupplier in FValueSuppliers do
  begin
    if LSupplier.ContainsValue(AName) then
    begin
      Exit(LSupplier.RefTargets[AName]);
    end;
  end;
  raise EParserTreeUnknownError.CreateFmt('Undeclared identifier: %s', [AName.QuotedString]);
end;

function TParserTree.GetValues(const AName: String; const AArgs: TArray<TParserValue>): TParserValue;
var
  LSupplier: IParserValueSupplier;
begin
  for LSupplier in FValueSuppliers do
  begin
    if LSupplier.ContainsValue(AName) then
    begin
      Exit(LSupplier.Values[AName, AArgs]);
    end;
  end;
  raise EParserTreeUnknownError.CreateFmt('Undeclared identifier: %s', [AName.QuotedString]);
end;

procedure TParserTree.Optimize;
var
  LNode: TParserNode;

  procedure OptimizeNode(const ANode: TParserNode);
  var
    LNode: TParserNode;
  begin
    if ANode is TParserParentNode then
    begin
      for LNode in (ANode as TParserParentNode) do
      begin
        OptimizeNode(ANode);
      end;
      // Optimize tree node
    end;
  end;

begin
//  for LNode in Self do
//  begin
//    OptimizeNode(LNode);
//  end;
end;

end.
