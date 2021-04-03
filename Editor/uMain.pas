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

unit uMain;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.ComCtrls, Vcl.ToolWin, Vcl.ActnMan,
  Vcl.ActnCtrls, Vcl.ActnMenus, System.Actions, Vcl.ActnList, System.UITypes, Parser.Syntax,
  Vcl.PlatformDefaultStyleActnCtrls, SynEdit, SynHighlighterMFL, Vcl.Menus, SynEditHighlighter,
  Vcl.ActnPopup, Parser, Parser.Package, Parser.Language, Vcl.StdCtrls, Vcl.ExtCtrls,
  SynCompletionProposal, Parser.Exporter, Parser.Lexer, SynEditExport, SynExportTeX;

type
  TfmMain = class(TForm)
    edCode: TSynEdit;
    ActionManager: TActionManager;
    acFileNew: TAction;
    acFileOpen: TAction;
    acFileSave: TAction;
    acFileSaveAs: TAction;
    acEditUndo: TAction;
    acEditRedo: TAction;
    acEditCopy: TAction;
    acEditCut: TAction;
    acEditPaste: TAction;
    acEditDelete: TAction;
    acViewStatusBar: TAction;
    acProgramRun: TAction;
    acProgramOptions: TAction;
    acInfoAbout: TAction;
    ActionMainMenuBar: TActionMainMenuBar;
    StatusBar: TStatusBar;
    FileOpenDialog: TFileOpenDialog;
    FileSaveDialog: TFileSaveDialog;
    acSelectAll: TAction;
    acSelectNone: TAction;
    PopupActionBar: TPopupActionBar;
    miCopy: TMenuItem;
    miCut: TMenuItem;
    miPaste: TMenuItem;
    miDelete: TMenuItem;
    pnLog: TPanel;
    spLog: TSplitter;
    tcLog: TTabControl;
    lbLog: TListBox;
    pnLogButtons: TFlowPanel;
    btLogClose: TButton;
    SynCompletionProposal: TSynCompletionProposal;
    procedure acViewStatusBarExecute(Sender: TObject);
    procedure acFileNewExecute(Sender: TObject);
    procedure acFileOpenExecute(Sender: TObject);
    procedure acFileSaveExecute(Sender: TObject);
    procedure acFileSaveAsExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure acEditUndoExecute(Sender: TObject);
    procedure acEditRedoExecute(Sender: TObject);
    procedure acEditCopyExecute(Sender: TObject);
    procedure acEditCutExecute(Sender: TObject);
    procedure acEditPasteExecute(Sender: TObject);
    procedure acEditDeleteExecute(Sender: TObject);
    procedure acSelectAllExecute(Sender: TObject);
    procedure acSelectNoneExecute(Sender: TObject);
    procedure acProgramOptionsExecute(Sender: TObject);
    procedure acEditUndoUpdate(Sender: TObject);
    procedure acEditRedoUpdate(Sender: TObject);
    procedure acEditCopyUpdate(Sender: TObject);
    procedure acEditCutUpdate(Sender: TObject);
    procedure acEditPasteUpdate(Sender: TObject);
    procedure acEditDeleteUpdate(Sender: TObject);
    procedure acSelectAllUpdate(Sender: TObject);
    procedure acSelectNoneUpdate(Sender: TObject);
    procedure tcLogChange(Sender: TObject);
    procedure acProgramRunExecute(Sender: TObject);
    procedure btLogCloseClick(Sender: TObject);
    procedure tcLogChanging(Sender: TObject; var AllowChange: Boolean);
    procedure edCodeChange(Sender: TObject);
    procedure SynCompletionProposalExecute(Kind: SynCompletionType;
      Sender: TObject; var CurrentInput: string; var x, y: Integer;
      var CanExecute: Boolean);
  private
    FFileModified: Boolean;
    FCodeModified: Boolean;
    FFileName: String;
    FHighlighter: TSynMflSyn;
    FOptions: TParserOptions;
    FStandardPackages: TParserStandardPackages;
    FMessages: TStringList;
    FOutput: TStringList;
  protected
    function CloseFile: Boolean;
  public
    property FileModified: Boolean read FFileModified;
    property CodeModified: Boolean read FCodeModified;
    property FileName: String read FFileName;
    property Highlighter: TSynMflSyn read FHighlighter;
    property Options: TParserOptions read FOptions;
    property StandardPackages: TParserStandardPackages read FStandardPackages;
    property Messages: TStringList read FMessages;
    property Output: TStringList read FOutput;
  end;

var
  fmMain: TfmMain;

implementation

uses
  uOptions;

{$R *.dfm}

procedure TfmMain.acEditCopyExecute(Sender: TObject);
begin
  edCode.CopyToClipboard;
end;

procedure TfmMain.acEditCopyUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := edCode.SelAvail;
end;

procedure TfmMain.acEditCutExecute(Sender: TObject);
begin
  edCode.CutToClipboard;
end;

procedure TfmMain.acEditCutUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := edCode.SelAvail;
end;

procedure TfmMain.acEditDeleteExecute(Sender: TObject);
begin
  edCode.ClearSelection;
end;

procedure TfmMain.acEditDeleteUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := edCode.SelAvail;
end;

procedure TfmMain.acEditPasteExecute(Sender: TObject);
begin
  edCode.PasteFromClipboard;
end;

procedure TfmMain.acEditPasteUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := edCode.CanPaste;
end;

procedure TfmMain.acEditRedoExecute(Sender: TObject);
begin
  edCode.Redo;
end;

procedure TfmMain.acEditRedoUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := edCode.CanRedo;
end;

procedure TfmMain.acEditUndoExecute(Sender: TObject);
begin
  edCode.Undo;
end;

procedure TfmMain.acEditUndoUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := edCode.CanUndo;
end;

procedure TfmMain.acFileNewExecute(Sender: TObject);
begin
  if CloseFile then
  begin
    edCode.Clear;
    FFileModified := False;
    FCodeModified := True;
  end;
end;

procedure TfmMain.acFileOpenExecute(Sender: TObject);
begin
  if CloseFile then
  begin
    FileOpenDialog.FileName := FileName;
    if FileOpenDialog.Execute then
    begin
      edCode.Lines.LoadFromFile(FileOpenDialog.FileName);
      FFileModified := False;
      FCodeModified := True;
      FFileName := FileOpenDialog.FileName;
    end;
  end;
end;

procedure TfmMain.acFileSaveAsExecute(Sender: TObject);
begin
  FileOpenDialog.FileName := FileName;
  if FileSaveDialog.Execute then
  begin
    FFileName := FileOpenDialog.FileName;
  end;
  acFileSave.Execute;
end;

procedure TfmMain.acFileSaveExecute(Sender: TObject);
begin
  if FileName.IsEmpty then
  begin
    acFileSaveAs.Execute;
  end else
  begin
    edCode.Lines.SaveToFile(FileName);
    FFileModified := False;
    FCodeModified := True;
  end;
end;

procedure TfmMain.acProgramOptionsExecute(Sender: TObject);
var
  LfmOptions: TfmOptions;
begin
  LfmOptions := TfmOptions.Create(Options, StandardPackages);
  if LfmOptions.ShowModal = mrOk then
  begin
    FOptions.Free;
    FOptions := LfmOptions.Options;
    FStandardPackages := LfmOptions.StandardPackages;
    FFileModified := True;
    FCodeModified := True;
  end;
end;

procedure TfmMain.acProgramRunExecute(Sender: TObject);

  procedure PrintMessage(const AMessage: String; const ALine: Integer = -1);
  const
    LLineMessage = '[Line %d] %s';
  begin
    if ALine >= 0 then
    begin
      Messages.Add(String.Format(LLineMessage, [Succ(ALine), AMessage]));
    end else
    begin
      Messages.Add(AMessage);
    end;
    tcLog.TabIndex := 0;
    lbLog.Items.Assign(Messages);
  end;

const
  LMessageInfo = 'Info: %s';
  LMessageWarning = 'Warning: %s';
  LMessageError = 'Error: %s';
var
  LParser: TParser;
  LIndex: Integer;
  LResponse: TParserResponse;
  LWarning: String;
begin
  Messages.Clear;
  Output.Clear;
  lbLog.Clear;
  pnLog.Show;
  spLog.Show;
  LParser := TParser.Create;
  try
    PrintMessage('Defining options...');
    LParser.Options.Assign(Options);
    LParser.StandardPackages := StandardPackages;
    PrintMessage('Executing code...');
    try
      for LIndex := 0 to Pred(edCode.Lines.Count) do
      begin
        try
          LResponse := LParser.Evaluate(edCode.Lines[LIndex]);
          case LResponse.ExpressionKind of
            exTerm:
              begin
                Output.Add(LResponse.ReturnValue.ToString(True));
              end;
            exResolution, exShow:
              begin
                PrintMessage(String.Format(LMessageInfo, [LResponse.ReturnValue.ToString]), LIndex);
              end;
          end;
          for LWarning in LResponse.Warnings do
          begin
            PrintMessage(String.Format(LMessageWarning, [LWarning]), LIndex);
          end;
        except
          on LException: EParserError do
            begin
              PrintMessage(String.Format(LMessageError, [LException.Message]), LIndex);
              raise;
            end;
          on EInvalidOp do
            begin
              PrintMessage(String.Format(LMessageError, ['Invalid operation']), LIndex);
              raise;
            end;
          else
            begin
              PrintMessage(String.Format(LMessageError, ['Internal error']), LIndex);
              raise;
            end;
        end;
      end;
      PrintMessage('Successfully executed');
    except
      PrintMessage('Execution failed');
    end;
  finally
    LParser.Free;
  end;
  if Output.Count <> 0 then
  begin
    tcLog.TabIndex := 1;
    lbLog.Items.Assign(Output);
  end;
end;

procedure TfmMain.acSelectAllExecute(Sender: TObject);
begin
  edCode.SelectAll;
end;

procedure TfmMain.acSelectAllUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := edCode.SelLength <> edCode.Text.Length;
end;

procedure TfmMain.acSelectNoneExecute(Sender: TObject);
begin
  edCode.SelLength := 0;
end;

procedure TfmMain.acSelectNoneUpdate(Sender: TObject);
begin
  (Sender as TAction).Enabled := edCode.SelAvail;
end;

procedure TfmMain.acViewStatusBarExecute(Sender: TObject);
begin
  StatusBar.Visible := (Sender as TAction).Checked;
end;

procedure TfmMain.btLogCloseClick(Sender: TObject);
begin
  pnLog.Hide;
  spLog.Hide;
end;

function TfmMain.CloseFile: Boolean;
begin
  if FileModified then
  begin
    case MessageDlg('Save changes?', mtConfirmation, mbYesNoCancel, 0, mbCancel) of
      mrYes:
        begin
          acFileSave.Execute;
        end;
      mrCancel:
        begin
          Exit(False);
        end;
    end;
  end;
  Result := True;
end;

procedure TfmMain.edCodeChange(Sender: TObject);
begin
  FFileModified := True;
  FCodeModified := True;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  FHighlighter := TSynMflSyn.Create(nil);
  FOptions := TParserOptions.Create;
  FStandardPackages := TParserStandardPackages.DefaultPackages;
  FMessages := TStringList.Create;
  FOutput := TStringList.Create;
  edCode.Highlighter := Highlighter;
  acFileNew.Execute;
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  FOptions.Free;
  FHighlighter.Free;
  edCode.Highlighter := Default(TSynMflSyn);
end;

procedure TfmMain.SynCompletionProposalExecute(Kind: SynCompletionType;
  Sender: TObject; var CurrentInput: string; var x, y: Integer;
  var CanExecute: Boolean);

  function HighlightItem(const AItem: String): String;
  var
    LLexer: TParserLexer;
    LToken: TParserToken;
    LBuilder: TStringBuilder;

    procedure BuildAttributeString(const AAttribute: TSynHighlighterAttributes);
    const
      LColor = '\color{%s}';
      LStyle = '\style{%s}';
      LStyleOption: array [Boolean] of String = (
        '-%s', '+%s'
      );
    begin
      LBuilder.AppendFormat(LColor, [ColorToString(AAttribute.Foreground)]);
      LBuilder.AppendFormat(LStyle, [String.Format(LStyleOption[fsBold in AAttribute.Style], ['B'])]);
      LBuilder.AppendFormat(LStyle, [String.Format(LStyleOption[fsItalic in AAttribute.Style], ['I'])]);
      LBuilder.AppendFormat(LStyle, [String.Format(LStyleOption[fsUnderline in AAttribute.Style], ['U'])]);
      LBuilder.AppendFormat(LStyle, [String.Format(LStyleOption[fsStrikeOut in AAttribute.Style], ['S'])]);
    end;

  begin
    LBuilder := TStringBuilder.Create;
    try
      LLexer := TParserLexer.Create(AItem);
      try
        repeat
          LToken := LLexer.NextToken[False];
          if LToken.Kind <> tkEnd then
          begin
            case LToken.Kind of
              tkSymbolOp, tkSymbolRef, tkSymbolHash, tkSymbolEq, tkSymbolColon, tkSymbolDot, tkSymbolComma, tkSymbolAmp, tkSymbolParOp, tkSymbolParCl, tkSymbolBrackOp, tkSymbolBrackCl, tkSymbolBraceOp, tkSymbolBraceCl, tkSymbolAbs:
                begin
                  BuildAttributeString(Highlighter.SymbolAttri);
                end;
              tkNumberDec:
                begin
                  BuildAttributeString(Highlighter.NumberDecAttri);
                end;
              tkNumberHex:
                begin
                  BuildAttributeString(Highlighter.NumberHexAttri);
                end;
              tkText:
                begin
                  BuildAttributeString(Highlighter.StringAttribute);
                end;
              tkName:
                begin
                  if LToken.Keyword = kwNone then
                  begin
                    BuildAttributeString(Highlighter.IdentifierAttri);
                  end else
                  begin
                    BuildAttributeString(Highlighter.KeywordAttri);
                  end;
                end;
            end;
//            if ((LToken.Kind = tkSymbolOp) and (LLexer.NextToken[True].Kind in [tkNumberDec, tkName)) or ((LToken.Kind = tkSymbolOp) and ) then

            LBuilder.Append(LToken.ToString);
//            if LToken.Kind in [tkSymbolOp, tkSymbolEq, tkSymbolColon, tkSymbolComma, tkSymbolAmp, tkSymbolParCl, tkSymbolBrackCl, tkSymbolBraceCl, tkSymbolAbs, tkNumberDec, tkNumberHex, tkText, tkName] and not (LLexer.NextToken[True] in []) then
            begin
//              LBuilder.Append(#32);
            end;
          end;
        until LToken.Kind = tkEnd;
      finally
        LLexer.Free;
      end;
      Result := LBuilder.ToString.TrimRight;
    finally
      LBuilder.Free;
    end;
  end;

var
  LParser: TParser;
  LIndex: Integer;
  LExporter: TParserExporter;
  LExportTarget: TStringList;
  LObject: TParserObject;
  LStandardPackage: TParserStandardPackage;
begin
  if CodeModified then
  begin
    LParser := TParser.Create;
    try
      LParser.Options.Assign(Options);
      LParser.StandardPackages := StandardPackages;
      for LIndex := 0 to Pred(edCode.CaretY) do
      begin
        try
          LParser.Evaluate(edCode.Lines[LIndex]);
        except
          Continue;
        end;
      end;
      SynCompletionProposal.ClearList;
      LExportTarget := TStringList.Create;
      try
        LExporter := TParserCodeExporter.Create(LExportTarget);
        try
          LExporter.Export(LParser.Dictionary);
          for LObject in LParser.Dictionary do
          begin
            for LIndex := 0 to Pred(LExporter.Export(LObject)) do
            begin
              SynCompletionProposal.ItemList.Add(HighlightItem(LExportTarget[LIndex]));
              SynCompletionProposal.InsertList.Add(LObject.Name);
            end;
            LExportTarget.Clear;
          end;
          for LStandardPackage in StandardPackages do
          begin
            for LObject in TParserStandardPackageProvider[LStandardPackage].Dictionary do
            begin
              for LIndex := 0 to Pred(LExporter.Export(LObject)) do
              begin
                SynCompletionProposal.ItemList.Add(HighlightItem(LExportTarget[LIndex]));
                SynCompletionProposal.InsertList.Add(LObject.Name);
              end;
            end;
            LExportTarget.Clear;
          end;
        finally
          LExporter.Free;
        end;
      finally
        LExportTarget.Free;
      end;
      FCodeModified := False;
    finally
      LParser.Free;
    end;
  end;
end;

procedure TfmMain.tcLogChange(Sender: TObject);
begin
  case tcLog.TabIndex of
    0:
      begin
        lbLog.Items.Assign(Messages);
      end;
    1:
      begin
        lbLog.Items.Assign(Output);
      end;
  end;
end;

procedure TfmMain.tcLogChanging(Sender: TObject; var AllowChange: Boolean);
begin
  case tcLog.TabIndex of
    0:
      begin
        Messages.Assign(lbLog.Items);
      end;
    1:
      begin
        Output.Assign(lbLog.Items);
      end;
  end;
end;

end.
