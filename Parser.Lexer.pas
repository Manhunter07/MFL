unit Parser.Lexer;

interface

uses
  System.SysUtils, System.Rtti,
  Parser.Exception, Parser.Value, Parser.Syntax;

type
  TParserTokenKind = (tkUnknown, tkEnd, tkSymbolOp, tkSymbolRef, tkSymbolHash, tkSymbolEq, tkSymbolColon, tkSymbolComma, tkSymbolParOp, tkSymbolParCl, tkSymbolBrackOp, tkSymbolBrackCl, tkSymbolBraceOp, tkSymbolBraceCl, tkSymbolAbs, tkNumberDec, tkNumberHex, tkText{Str, tkTextChr}, tkName);

  TParserTokenKinds = set of TParserTokenKind;

  TParserTokenKindHelper = record helper for TParserTokenKind
  public
    constructor Create(const ABeginning: Char);
  end;

  TParserToken = record
  private
    FCharIndex: Integer;
    FKind: TParserTokenKind;
    FInfo: TArray<TValue>;
  public
    property CharIndex: Integer read FCharIndex;
    property Kind: TParserTokenKind read FKind;
    property Info: TArray<TValue> read FInfo write FInfo;
    constructor Create(const ACharIndex: Integer; const AKind: TParserTokenKind);
    function ToString: String;
  end;

  TParserTokenHelper = record helper for TParserToken
  private
    function GetOperator: TParserOperator;
    procedure SetOperator(const AValue: TParserOperator);
    function GetNumberDec: Double;
    procedure SetNumberDec(const AValue: Double);
    function GetNumberHex: Int64;
    procedure SetNumberHex(const AValue: Int64);
    function GetText: String;
    procedure SetText(const AValue: String);
    function GetName: String;
    procedure SetName(const AValue: String);
    function GetKeyword: TParserKeyword;
    procedure SetKeyword(const AValue: TParserKeyword);
  public
    property &Operator: TParserOperator read GetOperator write SetOperator;
    property NumberDec: Double read GetNumberDec write SetNumberDec;
    property NumberHex: Int64 read GetNumberHex write SetNumberHex;
    property Text: String read GetText write SetText;
    property Name: String read GetName write SetName;
    property Keyword: TParserKeyword read GetKeyword write SetKeyword;
  end;

  TParserLexer = class
  private
    FExpression: String;
    FCurrentIndex: Integer;
  protected
    property CurrentIndex: Integer read FCurrentIndex write FCurrentIndex;
    function GetNextToken(const APreview: Boolean): TParserToken; virtual;
  public
    property Expression: String read FExpression;
    property NextToken[const APreview: Boolean]: TParserToken read GetNextToken;
    constructor Create(const AExpression: String);
  end;

implementation

{ TParserTokenKindHelper }

constructor TParserTokenKindHelper.Create(const ABeginning: Char);
begin
  case ABeginning of
    '+', '-', '~', '?', '*', '/', '%', '^':
      begin
        Self := tkSymbolOp;
      end;
    '@':
      begin
        Self := tkSymbolRef;
      end;
    '#':
      begin
        Self := tkSymbolHash;
      end;
    '=':
      begin
        Self := tkSymbolEq;
      end;
    ',':
      begin
        Self := tkSymbolComma;
      end;
    ':':
      begin
        Self := tkSymbolColon;
      end;
    '(':
      begin
        Self := tkSymbolParOp;
      end;
    ')':
      begin
        Self := tkSymbolParCl;
      end;
    '[':
      begin
        Self := tkSymbolBrackOp;
      end;
    ']':
      begin
        Self := tkSymbolBrackCl;
      end;
    '{':
      begin
        Self := tkSymbolBraceOp;
      end;
    '}':
      begin
        Self := tkSymbolBraceCl;
      end;
    '|':
      begin
        Self := tkSymbolAbs;
      end;
    '0' .. '9':
      begin
        Self := tkNumberDec;
      end;
    '$':
      begin
        Self := tkNumberHex;
      end;
    '"':
      begin
        Self := tkText;
      end;
    'A' .. 'Z', 'a' .. 'z', '_':
      begin
        Self := tkName;
      end;
    else
      begin
        Self := tkUnknown;
      end;
  end;
end;

{ TParserToken }

constructor TParserToken.Create(const ACharIndex: Integer; const AKind: TParserTokenKind);
const
  LInfoLengths: array [TParserTokenKind] of Integer = (
    0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1, 1, 1, 2
  );
begin
  FCharIndex := ACharIndex;
  FKind := AKind;
  SetLength(FInfo, LInfoLengths[Kind]);
end;

function TParserToken.ToString: String;
begin
  case Kind of
    tkEnd:
      begin
        Result := 'End of expression';
      end;
    tkSymbolOp:
      begin
        Result := &Operator.ToChar;
      end;
    tkSymbolRef:
      begin
        Result := '@';
      end;
    tkSymbolHash:
      begin
        Result := '#';
      end;
    tkSymbolEq:
      begin
        Result := '=';
      end;
    tkSymbolComma:
      begin
        Result := ',';
      end;
    tkSymbolColon:
      begin
        Result := ':';
      end;
    tkSymbolParOp:
      begin
        Result := '(';
      end;
    tkSymbolParCl:
      begin
        Result := ')';
      end;
    tkSymbolBrackOp:
      begin
        Result := '[';
      end;
    tkSymbolBrackCl:
      begin
        Result := ']';
      end;
    tkSymbolBraceOp:
      begin
        Result := '{';
      end;
    tkSymbolBraceCl:
      begin
        Result := '}';
      end;
    tkSymbolAbs:
      begin
        Result := '|';
      end;
    tkNumberDec:
      begin
        Result := TParserValue.Create(NumberDec).ToString;
      end;
    tkNumberHex:
      begin
        Result := Concat('$', NumberHex.ToHexString);
      end;
    tkText:
      begin
        Result := TParserValue.Create(Text).ToString;
      end;
    tkName:
      begin
        Result := Name;
      end;
  end;
end;

{ TParserTokenHelper }

function TParserTokenHelper.GetKeyword: TParserKeyword;
begin
  Result := Info[1].AsType<TParserKeyword>;
end;

function TParserTokenHelper.GetName: String;
begin
  Result := Info[0].AsType<String>;
end;

function TParserTokenHelper.GetNumberDec: Double;
begin
  Result := Info[0].AsType<Double>;
end;

function TParserTokenHelper.GetNumberHex: Int64;
begin
  Result := Info[0].AsType<Int64>;
end;

function TParserTokenHelper.GetOperator: TParserOperator;
begin
  Result := Info[0].AsType<TParserOperator>;
end;

function TParserTokenHelper.GetText: String;
begin
  Result := Info[0].AsType<String>;
end;

procedure TParserTokenHelper.SetKeyword(const AValue: TParserKeyword);
begin
  Info[1] := TValue.From<TParserKeyword>(AValue);
end;

procedure TParserTokenHelper.SetName(const AValue: String);
begin
  Info[0] := TValue.From<String>(AValue);
end;

procedure TParserTokenHelper.SetNumberDec(const AValue: Double);
begin
  Info[0] := TValue.From<Double>(AValue);
end;

procedure TParserTokenHelper.SetNumberHex(const AValue: Int64);
begin
  Info[0] := TValue.From<Int64>(AValue);
end;

procedure TParserTokenHelper.SetOperator(const AValue: TParserOperator);
begin
  Info[0] := TValue.From<TParserOperator>(AValue);
end;

procedure TParserTokenHelper.SetText(const AValue: String);
begin
  Info[0] := TValue.From<String>(AValue);
end;

{ TParserLexer }

constructor TParserLexer.Create(const AExpression: String);
begin
  inherited Create;
  FExpression := AExpression;
  CurrentIndex := Low(Expression);
end;

function TParserLexer.GetNextToken(const APreview: Boolean): TParserToken;
var
  LCurrentIndex: Integer;

  procedure ParseWhitespaces;
  begin
    while (LCurrentIndex <= High(Expression)) and CharInSet(Expression[LCurrentIndex], [#9, #10, #13, #32]) do
    begin
      Inc(LCurrentIndex);
    end;
  end;

  function DetermineKind: TParserTokenKind;
  begin
    if LCurrentIndex > High(Expression) then
    begin
      Result := tkEnd;
    end else
    begin
      Result := TParserTokenKind.Create(Expression[LCurrentIndex]);
      if Result = tkUnknown then
      begin
        raise EParserLexerCharacterError.CreateFmt('Invalid character at %d: %s', [LCurrentIndex, String(Expression[LCurrentIndex]).QuotedString]);
      end;
    end;
  end;

  procedure ParseToken;

    procedure ParseSymbolOp;
    begin
      Result.&Operator := TParserOperator.Create(Expression[LCurrentIndex]);
      Inc(LCurrentIndex);
    end;

    procedure ParseSymbolOther;
    begin
      Inc(LCurrentIndex);
    end;

    procedure ParseNumberDec;
    var
      LStringBuilder: TStringBuilder;
    begin
      LStringBuilder := TStringBuilder.Create;
      try
        repeat
          LStringBuilder.Append(Expression[LCurrentIndex]);
          Inc(LCurrentIndex);
        until (LCurrentIndex > High(Expression)) or not CharInSet(Expression[LCurrentIndex], ['0' .. '9']);
        if Expression[LCurrentIndex] = FormatSettings.Invariant.DecimalSeparator then
        begin
          repeat
            LStringBuilder.Append(Expression[LCurrentIndex]);
            Inc(LCurrentIndex);
          until (LCurrentIndex > High(Expression)) or not CharInSet(Expression[LCurrentIndex], ['0' .. '9']);
        end;
        if CharInSet(Expression[LCurrentIndex], ['E', 'e']) then
        begin
          LStringBuilder.Append(Expression[LCurrentIndex]);
          Inc(LCurrentIndex);
          if CharInSet(Expression[LCurrentIndex], ['+', '-']) then
          begin
            LStringBuilder.Append(Expression[LCurrentIndex]);
            Inc(LCurrentIndex);
          end;
          if (LCurrentIndex > High(Expression)) or not CharInSet(Expression[LCurrentIndex], ['0' .. '9']) then
          begin
            raise EParserLexerNumberFormatError.Create('Invalid number format');
          end;
          repeat
            LStringBuilder.Append(Expression[LCurrentIndex]);
            Inc(LCurrentIndex);
          until (LCurrentIndex > High(Expression)) or not CharInSet(Expression[LCurrentIndex], ['0' .. '9']);
        end;
        Result.NumberDec := StrToFloat(LStringBuilder.ToString, TFormatSettings.Invariant);
      finally
        LStringBuilder.Free;
      end;
    end;

    procedure ParseNumberHex;
    var
      LStringBuilder: TStringBuilder;
      LNumber: Int64;
    begin
      LStringBuilder := TStringBuilder.Create;
      try
        repeat
          LStringBuilder.Append(Expression[LCurrentIndex]);
          Inc(LCurrentIndex);
        until (LCurrentIndex > High(Expression)) or not CharInSet(Expression[LCurrentIndex], ['0' .. '9', 'A' .. 'F', 'a' .. 'f']);
        if not TryStrToInt64(LStringBuilder.ToString, LNumber) then
        begin
          raise EParserLexerNumberFormatError.Create('Invalid number format');
        end;
        Result.NumberHex := LNumber;
      finally
        LStringBuilder.Free;
      end;
    end;

    procedure ParseText;
    var
      LStringBuilder: TStringBuilder;
    begin
      LStringBuilder := TStringBuilder.Create;
      try
        Inc(LCurrentIndex);
        while (LCurrentIndex <= High(Expression)) and (Expression[LCurrentIndex] <> '"') do
        begin
          LStringBuilder.Append(Expression[LCurrentIndex]);
          Inc(LCurrentIndex);
        end;
        if LCurrentIndex > High(Expression) then
        begin
          raise EParserLexerTokenIncomplete.CreateFmt('Unterminated string at %d', [Result.CharIndex]);
        end;
        Inc(LCurrentIndex);
        Result.Text := LStringBuilder.ToString;
      finally
        LStringBuilder.Free;
      end;
    end;

    procedure ParseName;
    var
      LStringBuilder: TStringBuilder;
    begin
      LStringBuilder := TStringBuilder.Create;
      try
        repeat
          LStringBuilder.Append(Expression[LCurrentIndex]);
          Inc(LCurrentIndex);
        until (LCurrentIndex > High(Expression)) or not CharInSet(Expression[LCurrentIndex], ['0' .. '9', 'A' .. 'Z', 'a' .. 'z', '_']);
        ParseWhitespaces;
        if Expression[LCurrentIndex] = '.' then
        begin
          LStringBuilder.Append(Expression[LCurrentIndex]);
          Inc(LCurrentIndex);
          ParseWhitespaces;
          if not CharInSet(Expression[LCurrentIndex], ['A' .. 'Z', 'a' .. 'z', '_']) then
          begin
            raise EParserLexerTokenIncomplete.CreateFmt('Incomplete identifier at %d', [Result.CharIndex]);
          end;
          repeat
            LStringBuilder.Append(Expression[LCurrentIndex]);
            Inc(LCurrentIndex);
          until (LCurrentIndex > High(Expression)) or not CharInSet(Expression[LCurrentIndex], ['0' .. '9', 'A' .. 'Z', 'a' .. 'z', '_']);
        end;
        Result.Name := LStringBuilder.ToString;
        Result.Keyword := TParserKeyword.Create(Result.Name);
      finally
        LStringBuilder.Free;
      end;
    end;

  begin
    case Result.Kind of
      tkSymbolOp:
        begin
          ParseSymbolOp;
        end;
      tkSymbolRef, tkSymbolHash, tkSymbolEq, tkSymbolComma, tkSymbolColon, tkSymbolParOp, tkSymbolParCl, tkSymbolBrackOp, tkSymbolBrackCl, tkSymbolBraceOp, tkSymbolBraceCl, tkSymbolAbs:
        begin
          ParseSymbolOther;
        end;
      tkNumberDec:
        begin
          ParseNumberDec;
        end;
      tkNumberHex:
        begin
          ParseNumberHex;
        end;
      tkText:
        begin
          ParseText;
        end;
      tkName:
        begin
          ParseName;
        end;
    end;
  end;

begin
  LCurrentIndex := CurrentIndex;
  try
    ParseWhitespaces;
    Result := TParserToken.Create(LCurrentIndex, DetermineKind);
    ParseToken;
  finally
    if not APreview then
    begin
      CurrentIndex := LCurrentIndex;
    end;
  end;
end;

end.
