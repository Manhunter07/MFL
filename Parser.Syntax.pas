unit Parser.Syntax;

interface

uses
  System.SysUtils, System.StrUtils, System.Math, System.Rtti, System.Generics.Collections, System.Generics.Defaults,
  Parser.Exception, Parser.Value;

type
  TParserNode = class;

  TParserNodeKind = (nkNone, nkAbs, nkArgs, nkElements, nkFields, nkIf, nkThen, nkElse, nkTry, nkExcept);

  TParserNodeKindHelper = record helper for TParserNodeKind
  public
    constructor Create(const ANode: TParserNode);
  end;

  TParserKeyword = (kwNone, kw_, kwResolve, kwConstant, kwVariable, kwInline, kwFunction, kwType, kwConstructor, kwRange, kwEnum, kwInt, kwRangeInt, kwAttrib, kwAlias, kwShow, kwDelete, kwAssert, kwLink, kwSpread, kwIf, kwThen, kwElse, kwTry, kwExcept, kwFunc, kwRet);

  TParserKeywords = set of TParserKeyword;

  TParserKeywordHelper = record helper for TParserKeyword
  private const
    FKeywords: array [Succ(Low(TParserKeyword)) .. High(TParserKeyword)] of String = ('_', 'resolve', 'const', 'var', 'inline', 'function', 'type', 'constructor', 'range', 'enum', 'int', 'rangeint', 'attrib', 'alias', 'show', 'delete', 'assert', 'link', 'spread', 'if', 'then', 'else', 'try', 'except', 'func', 'ret');
    class function GetTypeConstructors: TParserKeywords; static;
    class function GetExpressionStarters: TParserKeywords; static;
    class function GetAllowed(const ANodeKind: TParserNodeKind): TParserKeywords; static;
  public
    constructor Create(const AName: String);
    class property Allowed[const ANodeKind: TParserNodeKind]: TParserKeywords read GetAllowed;
    class property ExpressionStarters: TParserKeywords read GetExpressionStarters;
    class property TypeConstructors: TParserKeywords read GetTypeConstructors;
    function ToString: String;
  end;

  TParserTypeConstructor = (tcRange, tcEnum, tcInt, tcRangeInt, tcStr, tcArray, tcRecord);

  TParserOperator = (opAdd, opSub, opRnd, opCmp, opMul, opDiv, opMod, opExp);

  TParserOperators = set of TParserOperator;

  TParserOperatorPrecedence = 0 .. 2;

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
    procedure Add(const ANode: TParserNode);
    function ChildrenValues: TArray<TParserValue>;
  public
    property Nodes[const AIndex: Integer]: TParserNode read GetNodes; default;
    property Count: Integer read GetCount;
    constructor Create(const AParent: TParserParentNode; const AOperator: TParserOperator);
    destructor Destroy; override;
    function GetEnumerator: TEnumerator<TParserNode>; inline;
    procedure Clear;
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

  TParserNamedArgNode = class(TParserArgNode)
  private
    FName: String;
  public
    property Name: String read FName;
    constructor Create(const AParent: TParserParentNode; const AName: String);
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

  TParserFuncNode = class(TParserParentNode);

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

class function TParserKeywordHelper.GetAllowed(const ANodeKind: TParserNodeKind): TParserKeywords;
const
  LAllowedKeywords: array [TParserNodeKind] of TParserKeywords = (
    [kwIf, kwTry], [], [kwSpread], [], [], [kwThen], [kwElse], [], [kwExcept], []
  );
begin
  Result := LAllowedKeywords[ANodeKind];
  Include(Result, kwNone);
end;

class function TParserKeywordHelper.GetExpressionStarters: TParserKeywords;
begin
  Result := [kwResolve, kwConstant, kwVariable, kwInline, kwFunction, kwType, kwConstructor, kwAlias, kwShow, kwDelete, kwAssert, kwLink];
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
    '~':
      begin
        Self := opRnd;
      end;
    '?':
      begin
        Self := opCmp;
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
  end;
end;
{$WARN NO_RETVAL ON}

class function TParserOperatorHelper.GetBinaryOperators: TParserOperators;
begin
  Result := [opAdd, opSub, opCmp, opMul, opDiv, opExp];
end;

class function TParserOperatorHelper.GetOperators(const APrecedence: TParserOperatorPrecedence): TParserOperators;
const
  LOperators: array [TParserOperatorPrecedence] of TParserOperators = ([opAdd, opSub, opRnd, opCmp], [opMul, opDiv, opMod], [opExp]);
begin
  Result := LOperators[APrecedence];
end;

class function TParserOperatorHelper.GetPrecedences(const AOperator: TParserOperator): TParserOperatorPrecedence;
const
  LPrecedences: array [TParserOperator] of TParserOperatorPrecedence = (0, 0, 0, 0, 1, 1, 1, 2);
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

  function Subtract(const AFirst, ASecond: TArray<TParserValue>): TArray<TParserValue>;
  var
    LResult: TList<TParserValue>;
    LValue: TParserValue;
  begin
    LResult := TList<TParserValue>.Create;
    try
      LResult.AddRange(AFirst);
      for LValue in ASecond do
      begin
        LResult.Remove(LValue);
      end;
      Result := LResult.ToArray;
    finally
      LResult.Free;
    end;
  end;

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
        case AFirst.Kind of
          vkDouble:
            begin
              Result := TParserValue.Create(AFirst.AsDouble - ASecond.AsDouble);
            end;
          vkArray:
            begin
              Result := TParserValue.Create(Subtract(AFirst.AsArray, ASecond.AsArray));
            end;
        end;
      end;
    opCmp:
      begin
        Result := TParserValue.Create(TParserValue.Compare(AFirst, ASecond));
      end;
    opMul:
      begin
        Result := TParserValue.Create(AFirst.AsDouble * ASecond.AsDouble);
      end;
    opDiv:
      begin
        Result := TParserValue.Create(AFirst.AsDouble / ASecond.AsDouble);
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
    [opAdd],
    [opAdd, opSub],
    [opAdd]
  );
begin
  Result := Self in LSupportedUnaryOperators[AValue.Kind];
end;

function TParserOperatorHelper.Supported(const AFirst, ASecond: TParserValue): Boolean;
const
  LSupportedBinaryOperators: array [TParserValueKind] of TParserOperators = (
    [],
    [opAdd, opSub, opCmp, opMul, opDiv, opExp],
    [opAdd, opCmp],
    [opAdd, opSub, opCmp],
    [opAdd, opCmp]
  );
begin
  Result := (Self in LSupportedBinaryOperators[AFirst.Kind]) and (Self in LSupportedBinaryOperators[ASecond.Kind]);
end;

function TParserOperatorHelper.ToChar: Char;
const
  LOperators: array [TParserOperator] of Char = ('+', '-', '~', '?', '*', '/', '%', '^');
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
  Result := TParserValue.Create(Abs(inherited GetValue.AsDouble));
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
begin
  inherited;
  if Count = 0 then
  begin
    Result := TParserValue.Empty[vkDouble];
  end else
  begin
    LNode := Nodes[0];
    Result := LNode.&Operator.Invoke(LNode.Value);
  end;
  for LIndex := 1 to Pred(Count) do
  begin
    LNode := Nodes[LIndex];
    Result := LNode.&Operator.Invoke(Result, LNode.Value);
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

{ TParserNamedArgNode }

constructor TParserNamedArgNode.Create(const AParent: TParserParentNode; const AName: String);
begin
  inherited Create(AParent);
  FName := AName;
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
