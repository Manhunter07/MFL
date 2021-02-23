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
