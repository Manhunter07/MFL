program pEditor;

uses
  Vcl.Forms,
  uMain in 'uMain.pas' {fmMain},
  SynHighlighterMFL in 'SynHighlighterMFL.pas',
  uOptions in 'uOptions.pas' {fmOptions},
  Parser in '..\Parser.pas',
  Parser.Syntax in '..\Parser.Syntax.pas',
  Parser.Value in '..\Parser.Value.pas',
  Parser.Dictionary in '..\Parser.Dictionary.pas',
  Parser.Exception in '..\Parser.Exception.pas',
  Parser.Exporter in '..\Parser.Exporter.pas',
  Parser.Language in '..\Parser.Language.pas',
  Parser.Lexer in '..\Parser.Lexer.pas',
  Parser.Package in '..\Parser.Package.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.
