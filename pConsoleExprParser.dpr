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

program pConsoleExprParser;

{$APPTYPE CONSOLE}

{$R *.res}

uses
  System.SysUtils,
  System.Classes,
  Parser in 'Parser.pas',
  Parser.Syntax in 'Parser.Syntax.pas',
  Parser.Dictionary in 'Parser.Dictionary.pas',
  Parser.Language in 'Parser.Language.pas',
  Parser.Exception in 'Parser.Exception.pas',
  Parser.Lexer in 'Parser.Lexer.pas',
  Parser.Package in 'Parser.Package.pas',
  Parser.Exporter in 'Parser.Exporter.pas',
  Parser.Value in 'Parser.Value.pas';

var
  Parser: TParser;
//  MemoryPackage: TParserMemoryPackage;
  Expr: String;
  Response: TParserResponse;
  Warning: String;
label
  Query;
begin
  Parser := TParser.Create;
//  MemoryPackage := TParserMemoryPackage.Create('Memory', True);
  try
//    Parser.RegisterPackage(MemoryPackage);
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
//    MemoryPackage.Free;
  end;
end.
