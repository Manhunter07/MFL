unit Parser.Syntax;

interface

uses
  System.SysUtils, System.StrUtils, System.Math, System.Generics.Collections, System.Generics.Defaults,
  Parser.Exception;

type
  IParserValueSupplier = interface
    ['{4D63069F-53A6-4CEF-B69B-79BACE8D4698}']
    function ContainsValue(const AName: String): Boolean;
    function GetMinArgCount(const AName: String): Integer;
    function GetMaxArgCount(const AName: String): Integer;
    function GetValues(const AName: String; const AArgs: TArray<Double>): Double;
    property Values[const AName: String; const AArgs: TArray<Double>]: Double read GetValues;
    property MinArgCount[const AName: String]: Integer read GetMinArgCount;
    property MaxArgCount[const AName: String]: Integer read GetMaxArgCount;
  end;

  TParserKeyword = (kwNone = -1, kwResolve, kwConstant, kwVariable, kwFunction, kwAlias, kwDelete);

  TParserKeywordHelper = record helper for TParserKeyword
  public
    constructor Create(const AName: String);
  end;

  TParserOperator = (opAdd, opSub, opMul, opDiv, opMod, opExp);

  TParserOperators = set of TParserOperator;

  TParserOperatorPrecedence = 0 .. 2;

  TParserOperatorHelper = record helper for TParserOperator
  private
    class function GetOperators(const APrecedence: TParserOperatorPrecedence): TParserOperators; static;
    class function GetPrecedences(const AOperator: TParserOperator): TParserOperatorPrecedence; static;
    class function GetUnaryOperators: TParserOperators; static;
  public
    class property UnaryOperators: TParserOperators read GetUnaryOperators;
    class property Operators[const APrecedence: TParserOperatorPrecedence]: TParserOperators read GetOperators;
    class property Precedences[const AOperator: TParserOperator]: TParserOperatorPrecedence read GetPrecedences;
    constructor Create(const ASymbol: Char);
    function ToChar: Char;
    function Invoke(const AFirst, ASecond: Double): Double; overload;
    function Invoke(const AValue: Double): Double; overload;
  end;

  TParserParentNode = class;
  TParserTree = class;

  TParserNode = class abstract
  private
    FSyntaxTree: TParserTree;
    FParent: TParserParentNode;
    FOperator: TParserOperator;
  protected
    function GetValue: Double; virtual; abstract;
  public
    property SyntaxTree: TParserTree read FSyntaxTree;
    property Parent: TParserParentNode read FParent;
    property &Operator: TParserOperator read FOperator;
    property Value: Double read GetValue;
    constructor Create(const AParent: TParserParentNode; const AOperator: TParserOperator);
  end;

  TParserParentNode = class(TParserNode)
  private
    FNodes: TObjectList<TParserNode>;
    function GetNodes(const AIndex: Integer): TParserNode;
    function GetCount: Integer;
  protected
    function GetValue: Double; override;
    procedure Add(const ANode: TParserNode);
  public
    property Nodes[const AIndex: Integer]: TParserNode read GetNodes; default;
    property Count: Integer read GetCount;
    constructor Create(const AParent: TParserParentNode; const AOperator: TParserOperator);
    destructor Destroy; override;
    function GetEnumerator: TEnumerator<TParserNode>; inline;
  end;

  TParserRootNode = class(TParserParentNode)
  public
    constructor Create(const ASyntaxTree: TParserTree);
  end;

  TParserNamedNode = class(TParserParentNode)
  private
    FName: String;
  protected
    function GetValue: Double; override;
    function ChildrenValues: TArray<Double>;
  public
    property Name: String read FName;
    constructor Create(const AParent: TParserParentNode; const AOperator: TParserOperator; const AName: String);
  end;

  TParserArgNode = class(TParserParentNode)
  public
    constructor Create(const AParent: TParserParentNode);
  end;

  TParserValueNode = class(TParserNode)
  private
    FValue: Double;
  protected
    function GetValue: Double; override;
  public
    constructor Create(const AParent: TParserParentNode; const AOperator: TParserOperator; const AValue: Double);
  end;

  TParserTree = class
  private
    FValueSuppliers: TArray<IParserValueSupplier>;
    FNodes: TParserParentNode;
    function GetValues(const AName: String; const AArgs: TArray<Double>): Double;
  protected
    property Values[const AName: String; const AArgs: TArray<Double>]: Double read GetValues;
  public
    property Nodes: TParserParentNode read FNodes;
    constructor Create(const AValueSuppliers: TArray<IParserValueSupplier>);
    destructor Destroy; override;
    function Calculate: Double;
  end;

implementation

{ TParserKeywordHelper }

constructor TParserKeywordHelper.Create(const AName: String);
begin
  Self := TParserKeyword(IndexText(AName, ['RESOLVE', 'CONST', 'VAR', 'FUNCTION', 'ALIAS', 'DELETE']));
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
  end;
end;
{$WARN NO_RETVAL ON}

class function TParserOperatorHelper.GetOperators(const APrecedence: TParserOperatorPrecedence): TParserOperators;
const
  LOperators: array [TParserOperatorPrecedence] of TParserOperators = ([opAdd, opSub], [opMul, opDiv, opMod], [opExp]);
begin
  Result := LOperators[APrecedence];
end;

class function TParserOperatorHelper.GetPrecedences(const AOperator: TParserOperator): TParserOperatorPrecedence;
const
  LPrecedences: array [TParserOperator] of TParserOperatorPrecedence = (0, 0, 1, 1, 1, 2);
begin
  Result := LPrecedences[AOperator];
end;

class function TParserOperatorHelper.GetUnaryOperators: TParserOperators;
begin
  Result := [opAdd, opSub];
end;

{$WARN NO_RETVAL OFF}
function TParserOperatorHelper.Invoke(const AValue: Double): Double;
begin
  case Self of
    opAdd:
      begin
        Result := +Result;
      end;
    opSub:
      begin
        Result := -Result;
      end;
  end;
end;
{$WARN NO_RETVAL ON}

{$WARN NO_RETVAL OFF}
function TParserOperatorHelper.Invoke(const AFirst, ASecond: Double): Double;
begin
  case Self of
    opAdd:
      begin
        Result := AFirst + ASecond;
      end;
    opSub:
      begin
        Result := AFirst - ASecond;
      end;
    opMul:
      begin
        Result := AFirst * ASecond;
      end;
    opDiv:
      begin
        Result := AFirst / ASecond;
      end;
    opMod:
      begin
        Result := FMod(AFirst, ASecond);
      end;
    opExp:
      begin
        Result := Power(AFirst, ASecond);
      end;
  end;
end;
{$WARN NO_RETVAL ON}

function TParserOperatorHelper.ToChar: Char;
const
  LOperators: array [TParserOperator] of Char = ('+', '-', '*', '/', '%', '^');
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

{ TParserParentNode }

procedure TParserParentNode.Add(const ANode: TParserNode);
begin
  FNodes.Add(ANode);
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

function TParserParentNode.GetValue: Double;
var
  LNode: TParserNode;
begin
  inherited;
  Result := Default(Double);
  for LNode in Self do
  begin
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

function TParserNamedNode.ChildrenValues: TArray<Double>;
var
  LIndex: Integer;
begin
  SetLength(Result, Count);
  for LIndex := 0 to Pred(Count) do
  begin
    Result[LIndex] := Self[LIndex].Value;
  end;
end;

constructor TParserNamedNode.Create(const AParent: TParserParentNode; const AOperator: TParserOperator; const AName: String);
begin
  inherited Create(AParent, AOperator);
  FName := AName;
end;

function TParserNamedNode.GetValue: Double;
begin
  inherited;
  Result := SyntaxTree.Values[Name, ChildrenValues];
end;

{ TParserArgNode }

constructor TParserArgNode.Create(const AParent: TParserParentNode);
begin
  inherited Create(AParent, opAdd);
end;

{ TParserValueNode }

constructor TParserValueNode.Create(const AParent: TParserParentNode; const AOperator: TParserOperator; const AValue: Double);
begin
  inherited Create(AParent, AOperator);
  FValue := AValue;
end;

function TParserValueNode.GetValue: Double;
begin
  inherited;
  Result := FValue;
end;

{ TParserTree }

function TParserTree.Calculate: Double;
begin
  Result := Nodes.Value;
end;

constructor TParserTree.Create(const AValueSuppliers: TArray<IParserValueSupplier>);
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

function TParserTree.GetValues(const AName: String; const AArgs: TArray<Double>): Double;
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

end.
