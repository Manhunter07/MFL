program pGUI_App;

uses
  System.StartUpCopy,
  FMX.Forms,
  uMain in 'uMain.pas' {fmMain},
  Parser in '..\Parser.pas',
  Parser.Dictionary in '..\Parser.Dictionary.pas',
  Parser.Exception in '..\Parser.Exception.pas',
  Parser.Exporter in '..\Parser.Exporter.pas',
  Parser.Language in '..\Parser.Language.pas',
  Parser.Lexer in '..\Parser.Lexer.pas',
  Parser.Package in '..\Parser.Package.pas',
  Parser.Syntax in '..\Parser.Syntax.pas',
  Parser.Value in '..\Parser.Value.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.FormFactor.Orientations := [TFormOrientation.Portrait, TFormOrientation.InvertedPortrait, TFormOrientation.Landscape, TFormOrientation.InvertedLandscape];
  Application.CreateForm(TfmMain, fmMain);
  Application.Run;
end.
