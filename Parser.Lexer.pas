unit Parser.Lexer;

interface

uses
  System.SysUtils,
  Parser.Exception, Parser.Syntax;

type
  TParserTokenKind = (tkUnknown, tkEnd, tkSymbolOp, tkSymbolEq, tkSymbolComma, tkSymbolParOp, tkSymbolParCl, tkNumberDec, tkNumberHex, tkName);

  TParserTokenKinds = set of TParserTokenKind;

  TParserTokenKindHelper = record helper for TParserTokenKind
  public
    constructor Create(const ABeginning: Char);
  end;

  TParserNameChars = array [Byte] of Char;

  TParserToken = record
    CharIndex: Integer;
    case Kind: TParserTokenKind of
      tkSymbolOp: (
        &Operator: TParserOperator;
      );
      tkNumberDec: (
        NumberDec: Double;
      );
      tkNumberHex: (
        NumberHex: Int64;
      );
      tkName: (
        Name: TParserNameChars;
      );
  end;

  TParserTokenHelper = record helper for TParserToken
  public
    function ToString: String;
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
    '+', '-', '*', '/', '%', '^':
      begin
        Self := tkSymbolOp;
      end;
    '=':
      begin
        Self := tkSymbolEq;
      end;
    ',':
      begin
        Self := tkSymbolComma;
      end;
    '(':
      begin
        Self := tkSymbolParOp;
      end;
    ')':
      begin
        Self := tkSymbolParCl;
      end;
    '0' .. '9':
      begin
        Self := tkNumberDec;
      end;
    '$':
      begin
        Self := tkNumberHex;
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

{ TParserTokenHelper }

function TParserTokenHelper.ToString: String;
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
    tkSymbolEq:
      begin
        Result := '=';
      end;
    tkSymbolComma:
      begin
        Result := ',';
      end;
    tkSymbolParOp:
      begin
        Result := '(';
      end;
    tkSymbolParCl:
      begin
        Result := ')';
      end;
    tkNumberDec:
      begin
        Result := NumberDec.ToString;
      end;
    tkNumberHex:
      begin
        Result := NumberHex.ToHexString;
      end;
    tkName:
      begin
        Result := Name;
      end;
  end;
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

  procedure DetermineKind;
  begin
    if LCurrentIndex > High(Expression) then
    begin
      Result.Kind := tkEnd;
    end else
    begin
      Result.Kind := TParserTokenKind.Create(Expression[LCurrentIndex]);
      if Result.Kind = tkUnknown then
      begin
        raise EParserLexerCharacterError.CreateFmt('Invalid character at %d: %s', [LCurrentIndex, String(Expression[LCurrentIndex]).QuotedString]);
      end;
    end;
  end;

  procedure ParseToken;
  var
    LStringBuilder: TStringBuilder;
    LCharIndex: Integer;
  begin
    case Result.Kind of
      tkSymbolOp:
        begin
          Result.&Operator := TParserOperator.Create(Expression[LCurrentIndex]);
          Inc(LCurrentIndex);
        end;
      tkSymbolEq, tkSymbolComma, tkSymbolParOp, tkSymbolParCl:
        begin
          Inc(LCurrentIndex);
        end;
      tkNumberDec:
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
                raise EParserLexerTokenLengthError.Create('Invalid floating point format');
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
      tkNumberHex:
        begin
          LStringBuilder := TStringBuilder.Create;
          try
            repeat
              LStringBuilder.Append(Expression[LCurrentIndex]);
              Inc(LCurrentIndex);
            until (LCurrentIndex > High(Expression)) or not CharInSet(Expression[LCurrentIndex], ['0' .. '9', 'A' .. 'F', 'a' .. 'f']);
            Result.NumberHex := LStringBuilder.ToString.ToInt64;
          finally
            LStringBuilder.Free;
          end;
        end;
      tkName:
        begin
          Result.Name := Default(TParserNameChars);
          LCharIndex := Low(Result.Name);
          repeat
            Result.Name[LCharIndex] := Expression[LCurrentIndex];
            Inc(LCurrentIndex);
            Inc(LCharIndex);
            if LCharIndex > High(Result.Name) then
            begin
              raise EParserLexerTokenLengthError.Create('Identifiers must not be longer than 255 characters');
            end;
          until (LCurrentIndex > High(Expression)) or not CharInSet(Expression[LCurrentIndex], ['0' .. '9', 'A' .. 'Z', 'a' .. 'z', '_']);
          ParseWhitespaces;
          if Expression[LCurrentIndex] = '.' then
          begin
            Result.Name[LCharIndex] := Expression[LCurrentIndex];
            Inc(LCurrentIndex);
            Inc(LCharIndex);
            ParseWhitespaces;
            if not CharInSet(Expression[LCurrentIndex], ['A' .. 'Z', 'a' .. 'z', '_']) then
            begin
              raise EParserLexerTokenIncomplete.CreateFmt('Incomplete identifier at %d', [Result.CharIndex]);
            end;
            repeat
              if LCharIndex > High(Result.Name) then
              begin
                raise EParserLexerTokenLengthError.Create('Identifiers must not be longer than 255 characters');
              end;
              Result.Name[LCharIndex] := Expression[LCurrentIndex];
              Inc(LCurrentIndex);
              Inc(LCharIndex);
            until (LCurrentIndex > High(Expression)) or not CharInSet(Expression[LCurrentIndex], ['0' .. '9', 'A' .. 'Z', 'a' .. 'z', '_']);
          end;
        end;
    end;
  end;

begin
  LCurrentIndex := CurrentIndex;
  try
    ParseWhitespaces;
    Result.CharIndex := LCurrentIndex;
    DetermineKind;
    ParseToken;
  finally
    if not APreview then
    begin
      CurrentIndex := LCurrentIndex;
    end;
  end;
end;

end.
