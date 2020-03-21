program pConsoleExprParser;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils, System.Classes,
  Parser in 'Parser.pas',
  Parser.Syntax in 'Parser.Syntax.pas',
  Parser.Dictionary in 'Parser.Dictionary.pas',
  Parser.Language in 'Parser.Language.pas',
  Parser.Exception in 'Parser.Exception.pas',
  Parser.Lexer in 'Parser.Lexer.pas',
  Parser.Package in 'Parser.Package.pas',
  Parser.Exporter in 'Parser.Exporter.pas';

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
              Writeln(' = ', Response.ReturnValue.ToString(True));
            end;
          exResolution, exShow:
            begin
              Writeln(Response.ReturnValue.ToString);
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
          end;
        on EInvalidOp do
          begin
            Writeln('Error: Invalid operation');
          end;
        else
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
