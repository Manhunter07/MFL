unit uMain;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Edit, FMX.ListBox,
  FMX.ListView.Types, FMX.ListView.Appearances, FMX.ListView.Adapters.Base,
  FMX.ListView, Parser, Parser.Package, Parser.Language, Parser.Dictionary, Parser.Syntax;

type
  TLogCategory = (lcResult, lcError, lcWarning, lcInfo);

  TLogCategoryHelper = record helper for TLogCategory
  public
    function ToString: String;
  end;

  TfmMain = class(TForm)
    ScaledLayout: TScaledLayout;
    GridPanelLayoutCommand: TGridPanelLayout;
    btRun: TButton;
    GridPanelLayoutObjects: TGridPanelLayout;
    laConst: TLabel;
    laVar: TLabel;
    lbConst: TListBox;
    lbVar: TListBox;
    edCommand: TEdit;
    lwLog: TListView;
    Splitter1: TSplitter;
    pnSuggestBack: TPanel;
    laFunction: TLabel;
    laType: TLabel;
    lbFunction: TListBox;
    lbType: TListBox;
    Layout: TLayout;
    pnSuggest: TPanel;
    lbSuggest: TListBox;
    procedure btRunClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure lbObjectItemClick(const Sender: TCustomListBox;
      const Item: TListBoxItem);
    procedure edCommandResized(Sender: TObject);
    procedure pnSuggestBackClick(Sender: TObject);
    procedure edCommandTyping(Sender: TObject);
    procedure edCommandTap(Sender: TObject; const Point: TPointF);
    procedure ScaledLayoutResized(Sender: TObject);
    procedure lbSuggestItemClick(const Sender: TCustomListBox;
      const Item: TListBoxItem);
    procedure edCommandKeyDown(Sender: TObject; var Key: Word;
      var KeyChar: Char; Shift: TShiftState);
  private
    { Private-Deklarationen }
    Parser: TParser;
    MemoryPackage: TParserMemoryPackage;
  public
    { Public-Deklarationen }
    procedure WriteLog(const ACategory: TLogCategory; const AText: String);
    procedure RefreshData;
    procedure LoadDictionary(const ADictionary: TParserDictionary; const APackage: TParserPackage = nil);
    function GetSelWord(out AIndex: Integer): String;
    procedure ReplaceWord(var AText: String; const AIndex: Integer; const ANewWord: String);
    procedure SetInput(const AValue: String);
  end;

var
  fmMain: TfmMain;

implementation

{$R *.fmx}

{ TLogCategoryHelper }

function TLogCategoryHelper.ToString: String;
const
  LStrings: array [TLogCategory] of String = ('Result', 'Error', 'Warning', 'Info');
begin
  Result := LStrings[Self];
end;

procedure TfmMain.btRunClick(Sender: TObject);
var
  Response: TParserResponse;
  Warning: String;
begin
  lwLog.Items.Clear;
  try
    Response := Parser.Evaluate(edCommand.Text);
    case Response.ExpressionKind of
      exTerm:
        begin
          WriteLog(lcResult, Response.ReturnValue.ToString(True));
        end;
      exResolution, exShow:
        begin
          WriteLog(lcInfo, Response.ReturnValue.ToString);
        end;
    end;
    for Warning in Response.Warnings do
    begin
      WriteLog(lcWarning, Warning);
    end;
  except
    on LException: EParserError do
      begin
        WriteLog(lcError, LException.Message);
      end;
    on EInvalidOp do
      begin
        WriteLog(lcError, 'Invalid operation');
      end;
    else
      begin
        WriteLog(lcError, 'Internal error');
      end;
  end;
  RefreshData;
  edCommand.Text := String.Empty;
  edCommand.SetFocus;
end;

procedure TfmMain.edCommandKeyDown(Sender: TObject; var Key: Word;
  var KeyChar: Char; Shift: TShiftState);
begin
  if lbSuggest.Visible and (Key = vkDown) then
  begin
    lbSuggest.ItemIndex := 0;
    lbSuggest.SetFocus;
  end;
end;

procedure TfmMain.edCommandResized(Sender: TObject);
begin
  pnSuggest.Position.X := edCommand.Margins.Left + edCommand.Position.X;
  pnSuggest.Position.Y := edCommand.Margins.Top + edCommand.Position.Y + edCommand.Height;
  pnSuggest.Width := edCommand.Width;
end;

procedure TfmMain.edCommandTap(Sender: TObject; const Point: TPointF);
begin
  edCommandTyping(Sender);
end;

procedure TfmMain.edCommandTyping(Sender: TObject);
var
  LSuggestVisible: Boolean;
  LIndex: Integer;
  LSelWord: String;
  LSelWordIndex: Integer;
begin
  LSelWord := GetSelWord(LSelWordIndex).ToUpperInvariant;
  LSuggestVisible := False;
  for LIndex := 0 to Pred(lbSuggest.Count) do
  begin
    lbSuggest.ListItems[LIndex].Visible := lbSuggest.Items[LIndex].ToUpperInvariant.IndexOf(LSelWord) <> -1;
    if lbSuggest.ListItems[LIndex].Visible then
    begin
      LSuggestVisible := True;
    end;
  end;
  pnSuggestBack.Visible := LSuggestVisible;
  pnSuggest.Visible := LSuggestVisible;
end;

procedure TfmMain.FormCreate(Sender: TObject);
begin
  Parser := TParser.Create;
//  Parser.StandardPackages := [pkConvert, pkRandom, pkDateTime];
  MemoryPackage := TParserMemoryPackage.Create('Memory', True);
  Parser.RegisterPackage(MemoryPackage);
  RefreshData;
end;

procedure TfmMain.FormDestroy(Sender: TObject);
begin
  Parser.Free;
  MemoryPackage.Free;
end;

function TfmMain.GetSelWord(out AIndex: Integer): String;
var
  LIdentBuilder: TStringBuilder;
begin
  LIdentBuilder := TStringBuilder.Create;
  try
    AIndex := Pred(edCommand.CaretPosition);
    while (AIndex <> -1) and CharInSet(edCommand.Text.Chars[AIndex], ['A' .. 'Z', 'a' .. 'z', '0' .. '9', '_']) do
    begin
      LIdentBuilder.Insert(0, edCommand.Text.Chars[AIndex]);
      Dec(AIndex);
    end;
    Result := LIdentBuilder.ToString;
  finally
    LIdentBuilder.Free;
  end;
  Inc(AIndex);
end;

procedure TfmMain.lbObjectItemClick(const Sender: TCustomListBox;
  const Item: TListBoxItem);
var
  LText: String;
begin
  edCommand.DeleteSelection;
  LText := edCommand.Text;
  LText.Insert(edCommand.CaretPosition, Item.Text);
  SetInput(LText);
  pnSuggestBack.Visible := False;
  pnSuggest.Visible := False;
  lbSuggest.ClearSelection;
  lbConst.ClearSelection;
  lbVar.ClearSelection;
  lbFunction.ClearSelection;
  lbType.ClearSelection;
end;

procedure TfmMain.lbSuggestItemClick(const Sender: TCustomListBox;
  const Item: TListBoxItem);
var
  LSelWord: String;
  LSelWordIndex: Integer;
  LText: String;
begin
  LSelWord := GetSelWord(LSelWordIndex);
  LText := edCommand.Text;
  LText.Remove(LSelWordIndex, LSelWord.Length);
  LText.Insert(edCommand.CaretPosition, Item.Text);
  SetInput(LText);
  pnSuggestBack.Visible := False;
  pnSuggest.Visible := False;
  lbSuggest.ClearSelection;
end;

procedure TfmMain.LoadDictionary(const ADictionary: TParserDictionary; const APackage: TParserPackage = nil);
const
  LFontFamily = {$IFDEF MSWINDOWS}'Courier New'{$ELSE ANDROID}'monospace'{$ENDIF};
var
  LObject: TParserObject;
  LObjectName: String;
  LIndex: Integer;
begin
  lbSuggest.BeginUpdate;
  lbConst.BeginUpdate;
  lbVar.BeginUpdate;
  lbFunction.BeginUpdate;
  lbType.BeginUpdate;
  try
    for LObject in ADictionary do
    begin
      if Assigned(APackage) and APackage.Explicit then
      begin
        LObjectName := String.Format('%s.%s', [APackage.Name, LObject.Name]);
      end else
      begin
        LObjectName := LObject.Name;
      end;
      if LObject.ClassType = TParserConstant then
      begin
        lbConst.Items.AddObject(LObjectName, LObject);
      end else
      begin
        if LObject.ClassType = TParserVariable then
        begin
          lbVar.Items.AddObject(LObjectName, LObject);
        end else
        begin
          if LObject is TParserFunction then
          begin
            lbFunction.Items.AddObject(LObjectName, LObject);
          end else
          begin
            if LObject is TParserType then
            begin
              lbType.Items.AddObject(LObjectName, LObject);
            end;
          end;
        end;
      end;
      lbSuggest.Items.AddObject(LObjectName, LObject);
    end;
    for LIndex := 0 to Pred(lbSuggest.Count) do
    begin
      lbSuggest.ListItems[LIndex].TextSettings.Font.Family := LFontFamily;
      lbSuggest.ListItems[LIndex].StyledSettings := lbSuggest.ListItems[LIndex].StyledSettings - [TStyledSetting.Family];
    end;
    for LIndex := 0 to Pred(lbConst.Count) do
    begin
      lbConst.ListItems[LIndex].TextSettings.Font.Family := LFontFamily;
      lbConst.ListItems[LIndex].StyledSettings := lbConst.ListItems[LIndex].StyledSettings - [TStyledSetting.Family];
    end;
    for LIndex := 0 to Pred(lbVar.Count) do
    begin
      lbVar.ListItems[LIndex].TextSettings.Font.Family := LFontFamily;
      lbVar.ListItems[LIndex].StyledSettings := lbVar.ListItems[LIndex].StyledSettings - [TStyledSetting.Family];
    end;
    for LIndex := 0 to Pred(lbFunction.Count) do
    begin
      lbFunction.ListItems[LIndex].TextSettings.Font.Family := LFontFamily;
      lbFunction.ListItems[LIndex].StyledSettings := lbFunction.ListItems[LIndex].StyledSettings - [TStyledSetting.Family];
    end;
    for LIndex := 0 to Pred(lbType.Count) do
    begin
      lbType.ListItems[LIndex].TextSettings.Font.Family := LFontFamily;
      lbType.ListItems[LIndex].StyledSettings := lbType.ListItems[LIndex].StyledSettings - [TStyledSetting.Family];
    end;
  finally
    lbSuggest.EndUpdate;
    lbConst.EndUpdate;
    lbVar.EndUpdate;
    lbFunction.EndUpdate;
    lbType.EndUpdate;
  end;
end;

procedure TfmMain.pnSuggestBackClick(Sender: TObject);
begin
  pnSuggestBack.Visible := False;
  pnSuggest.Visible := False;
end;

procedure TfmMain.RefreshData;
var
  LKeyWord: TParserKeyword;
  LIndex: Integer;
begin
  pnSuggestBack.Visible := False;
  pnSuggest.Visible := False;
  lbSuggest.Clear;
  lbConst.Clear;
  lbVar.Clear;
  lbFunction.Clear;
  lbType.Clear;
  for LKeyword := Low(TParserKeyword) to High(TParserKeyword) do
  begin
    lbSuggest.Items.Add(LKeyword.ToString);
  end;
  LoadDictionary(Parser.Dictionary);
  for LIndex := 0 to Pred(Parser.PackageCount) do
  begin
    LoadDictionary(Parser.Packages[Parser.PackageNames[LIndex]].Dictionary, Parser.Packages[Parser.PackageNames[LIndex]]);
  end;
end;

procedure TfmMain.ReplaceWord(var AText: String; const AIndex: Integer; const ANewWord: String);
var
  LIndex: Integer;
begin
//  LIndex := AIndex;
//  while (LIndex <= High(AText)) and (LIndex - AIndex <= High(ANewWord.Last)) and CharInSet(AText.Chars[LIndex], ['A' .. 'Z', 'a' .. 'z', '0' .. '9', '_']) do
//  begin
//    AText[Succ(LIndex)] := ANewWord.Chars[LIndex - AIndex];
//    Inc(LIndex);
//  end;
end;

procedure TfmMain.ScaledLayoutResized(Sender: TObject);
begin
  pnSuggestBack.Position.X := 0;
  pnSuggestBack.Position.Y := edCommand.Margins.Top + edCommand.Position.Y + edCommand.Height;
  pnSuggestBack.Width := ScaledLayout.Width;
  pnSuggestBack.Height := ScaledLayout.Height - pnSuggestBack.Position.Y;
end;

procedure TfmMain.SetInput(const AValue: String);
var
  LNewCaretPosition: Integer;
begin
  LNewCaretPosition := edCommand.CaretPosition - edCommand.Text.Length + AValue.Length;
  edCommand.Text := AValue;
  edCommand.SetFocus;
  edCommand.CaretPosition := LNewCaretPosition;
  edCommand.ResetSelection;
end;

procedure TfmMain.WriteLog(const ACategory: TLogCategory; const AText: String);
var
  LItem: TListViewItem;
begin
  LItem := lwLog.Items.Add;
  LItem.Text := ACategory.ToString;
  LItem.Detail := AText;
//  lwLog.ItemAppearanceObjects.ItemObjects.Text.
  lwLog.ItemIndex := LItem.Index;
end;

end.
