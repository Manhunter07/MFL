program pConsoleExprParser;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  Parser in 'Parser.pas',
  Parser.Syntax in 'Parser.Syntax.pas',
  Parser.Dictionary in 'Parser.Dictionary.pas',
  Parser.Language in 'Parser.Language.pas',
  Parser.Exception in 'Parser.Exception.pas',
  Parser.Lexer in 'Parser.Lexer.pas',
  Parser.Package in 'Parser.Package.pas',
  Parser.Runtime in 'Parser.Runtime.pas';

function ValueToString(const AValue: Double): String;
begin
  if AValue.IsNan then
  begin
    Result := 'Invalid number';
  end else
  begin
    if AValue.IsNegativeInfinity then
    begin
      Result := 'Negative infinity';
    end else
    begin
      if AValue.IsPositiveInfinity then
      begin
        Result := 'Positive infinity';
      end else
      begin
        Result := AValue.ToString(TFormatSettings.Invariant);
      end;
    end;
  end;
end;

var
  Parser: TParser;
  MemoryPackage: TParserMemoryPackage;
  Expr: String;
  Response: TParserResponse;
  Warning: String;
label
  Query;
begin
  Parser := TParser.Create;
  MemoryPackage := TParserMemoryPackage.Create('Memory', True);
  try
    Parser.RegisterPackage(MemoryPackage);
    Query:
      try
        Readln(Expr);
        Response := Parser.Evaluate(Expr);
        case Response.ExpressionKind of
          exTerm:
            begin
              Writeln(' = ', ValueToString(Response.Value));
            end;
          exResolution:
            begin
              Writeln(Response.Name);
            end;
        end;
        for Warning in Response.Warnings do
        begin
          Writeln('Warning: ', Warning);
        end;
      except
        on LException: EParserError do
        begin
          Writeln('Error: ', LException.Message);
        end else
        begin
          Writeln('Internal error');
        end;
      end;
    goto Query;
  finally
    Parser.Free;
    MemoryPackage.Free;
  end;
end.
