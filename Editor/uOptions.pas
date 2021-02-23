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

unit uOptions;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.StdCtrls, Vcl.CheckLst,
  Vcl.ExtCtrls, Vcl.WinXCtrls, Vcl.ComCtrls, Parser, Parser.Value, Parser.Package;

type
  TfmOptions = class(TForm)
    pcOptions: TPageControl;
    tsCompiler: TTabSheet;
    Packages: TTabSheet;
    lbStandardPackages: TCheckListBox;
    laStandardPackages: TLabel;
    laOptionInitialType: TLabel;
    cbOptionInitialType: TComboBox;
    cbOptionWarnings: TCheckBox;
    cbOptionOptimization: TCheckBox;
    pnButtons: TFlowPanel;
    btOK: TButton;
    pnCompilerOptions: TGridPanel;
  private
    function GetOptions: TParserOptions;
    function GetStandardPackages: TParserStandardPackages;
  public
    property Options: TParserOptions read GetOptions;
    property StandardPackages: TParserStandardPackages read GetStandardPackages;
    constructor Create(const AOptions: TParserOptions; const AStandardPackages: TParserStandardPackages); reintroduce;
  end;

var
  fmOptions: TfmOptions;

implementation

{$R *.dfm}

constructor TfmOptions.Create(const AOptions: TParserOptions; const AStandardPackages: TParserStandardPackages);
var
  LStandardPackage: TParserStandardPackage;
begin
  inherited Create(nil);
  for LStandardPackage := Low(TParserStandardPackage) to High(TParserStandardPackage) do
  begin
    lbStandardPackages.Checked[lbStandardPackages.Items.Add(LStandardPackage.Name)] := LStandardPackage in AStandardPackages;
  end;
  cbOptionWarnings.Checked := AOptions.Warnings;
  cbOptionOptimization.Checked := AOptions.Optimization;
  cbOptionInitialType.ItemIndex := Ord(AOptions.InitialType);
end;

function TfmOptions.GetOptions: TParserOptions;
begin
  Result := TParserOptions.Create;
  Result.Warnings := cbOptionWarnings.Checked;
  Result.Optimization := cbOptionOptimization.Checked;
  Result.InitialType :=  TParserValueKind(cbOptionInitialType.ItemIndex);
end;

function TfmOptions.GetStandardPackages: TParserStandardPackages;
var
  LStandardPackage: TParserStandardPackage;
begin
  Result := Default(TParserStandardPackages);
  for LStandardPackage := Low(TParserStandardPackage) to High(TParserStandardPackage) do
  begin
    if lbStandardPackages.Checked[lbStandardPackages.Items.IndexOf(LStandardPackage.Name)] then
    begin
      Include(Result, LStandardPackage);
    end;
  end;
end;

end.
