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

unit SynHighlighterMFL;

interface

uses
  Graphics,
  SynEditTypes,
  SynEditHighlighter,
  SynUnicode,
  SysUtils,
  Classes;

type
  TtkTokenKind = (
    tkComment,
    tkIdentifier,
    tkKeyword,
    tkNull,
    tkNumberDec,
    tkNumberHex,
    tkSpace,
    tkSymbol,
    tkText,
    tkTypeConstructor,
    tkUnknown);

  TRangeState = (rsUnKnown, rsComment, rsText);

  TProcTableProc = procedure of object;

  PIdentFuncTableFunc = ^TIdentFuncTableFunc;
  TIdentFuncTableFunc = function (Index: Integer): TtkTokenKind of object;

type
  TSynMflSyn = class(TSynCustomHighlighter)
  private
    fRange: TRangeState;
    fTokenID: TtkTokenKind;
    fIdentFuncTable: array[0..52] of TIdentFuncTableFunc;
    fCommentAttri: TSynHighlighterAttributes;
    fIdentifierAttri: TSynHighlighterAttributes;
    fKeywordAttri: TSynHighlighterAttributes;
    fNumberDecAttri: TSynHighlighterAttributes;
    fNumberHexAttri: TSynHighlighterAttributes;
    fSpaceAttri: TSynHighlighterAttributes;
    fSymbolAttri: TSynHighlighterAttributes;
    fTextAttri: TSynHighlighterAttributes;
    fTypeConstructorAttri: TSynHighlighterAttributes;
    function HashKey(Str: PWideChar): Cardinal;
    function Func95(Index: Integer): TtkTokenKind;
    function FuncAlias(Index: Integer): TtkTokenKind;
    function FuncAny(Index: Integer): TtkTokenKind;
    function FuncArray(Index: Integer): TtkTokenKind;
    function FuncAssert(Index: Integer): TtkTokenKind;
    function FuncAttrib(Index: Integer): TtkTokenKind;
    function FuncConst(Index: Integer): TtkTokenKind;
    function FuncConstructor(Index: Integer): TtkTokenKind;
    function FuncCustom(Index: Integer): TtkTokenKind;
    function FuncDelete(Index: Integer): TtkTokenKind;
    function FuncElse(Index: Integer): TtkTokenKind;
    function FuncEnum(Index: Integer): TtkTokenKind;
    function FuncExcept(Index: Integer): TtkTokenKind;
    function FuncFunc(Index: Integer): TtkTokenKind;
    function FuncFunction(Index: Integer): TtkTokenKind;
    function FuncIf(Index: Integer): TtkTokenKind;
    function FuncInline(Index: Integer): TtkTokenKind;
    function FuncInt(Index: Integer): TtkTokenKind;
    function FuncLink(Index: Integer): TtkTokenKind;
    function FuncObject(Index: Integer): TtkTokenKind;
    function FuncOpt(Index: Integer): TtkTokenKind;
    function FuncRange(Index: Integer): TtkTokenKind;
    function FuncRangeint(Index: Integer): TtkTokenKind;
    function FuncRecord(Index: Integer): TtkTokenKind;
    function FuncRef(Index: Integer): TtkTokenKind;
    function FuncResolve(Index: Integer): TtkTokenKind;
    function FuncRet(Index: Integer): TtkTokenKind;
    function FuncShow(Index: Integer): TtkTokenKind;
    function FuncSpread(Index: Integer): TtkTokenKind;
    function FuncString(Index: Integer): TtkTokenKind;
    function FuncThen(Index: Integer): TtkTokenKind;
    function FuncTry(Index: Integer): TtkTokenKind;
    function FuncType(Index: Integer): TtkTokenKind;
    function FuncVar(Index: Integer): TtkTokenKind;
    procedure IdentProc;
    procedure NumberDecProc;
    procedure NumberHexProc;
    procedure SymbolProc;
    procedure UnknownProc;
    function AltFunc(Index: Integer): TtkTokenKind;
    procedure InitIdent;
    function IdentKind(MayBe: PWideChar): TtkTokenKind;
    procedure NullProc;
    procedure SpaceProc;
    procedure CRProc;
    procedure LFProc;
    procedure CommentOpenProc;
    procedure CommentProc;
    procedure TextOpenProc;
    procedure TextProc;
  protected
    function GetSampleSource: string; override;
    function IsFilterStored: Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    class function GetFriendlyLanguageName: string; override;
    class function GetLanguageName: string; override;
    function GetRange: Pointer; override;
    procedure ResetRange; override;
    procedure SetRange(Value: Pointer); override;
    function GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes; override;
    function GetEol: Boolean; override;
    function GetTokenID: TtkTokenKind;
    function GetTokenAttribute: TSynHighlighterAttributes; override;
    function GetTokenKind: Integer; override;
    function IsIdentChar(AChar: WideChar): Boolean; override;
    procedure Next; override;
  published
    property CommentAttri: TSynHighlighterAttributes read fCommentAttri write fCommentAttri;
    property IdentifierAttri: TSynHighlighterAttributes read fIdentifierAttri write fIdentifierAttri;
    property KeywordAttri: TSynHighlighterAttributes read fKeywordAttri write fKeywordAttri;
    property NumberDecAttri: TSynHighlighterAttributes read fNumberDecAttri write fNumberDecAttri;
    property NumberHexAttri: TSynHighlighterAttributes read fNumberHexAttri write fNumberHexAttri;
    property SpaceAttri: TSynHighlighterAttributes read fSpaceAttri write fSpaceAttri;
    property SymbolAttri: TSynHighlighterAttributes read fSymbolAttri write fSymbolAttri;
    property TextAttri: TSynHighlighterAttributes read fTextAttri write fTextAttri;
    property TypeConstructorAttri: TSynHighlighterAttributes read fTypeConstructorAttri write fTypeConstructorAttri;
  end;

implementation

uses
  SynEditStrConst;

resourcestring
  SYNS_FilterMFL = 'MFL files (*.mfl)|*.mfl';
  SYNS_LangMFL = 'MFL';
  SYNS_FriendlyLangMFL = 'MFL';
  SYNS_AttrKeyword = 'Keyword';
  SYNS_FriendlyAttrKeyword = 'Keyword';
  SYNS_AttrNumberDec = 'NumberDec';
  SYNS_FriendlyAttrNumberDec = 'NumberDec';
  SYNS_AttrNumberHex = 'NumberHex';
  SYNS_FriendlyAttrNumberHex = 'NumberHex';
  SYNS_AttrTypeConstructor = 'TypeConstructor';
  SYNS_FriendlyAttrTypeConstructor = 'TypeConstructor';

const
  // as this language is case-insensitive keywords *must* be in lowercase
  KeyWords: array[0..33] of string = (
    '_', 'alias', 'any', 'array', 'assert', 'attrib', 'const', 'constructor', 
    'custom', 'delete', 'else', 'enum', 'except', 'func', 'function', 'if', 
    'inline', 'int', 'link', 'object', 'opt', 'range', 'rangeint', 'record', 
    'ref', 'resolve', 'ret', 'show', 'spread', 'string', 'then', 'try', 'type', 
    'var' 
  );

  KeyIndices: array[0..52] of Integer = (
    33, 15, 31, -1, -1, 17, -1, -1, 26, 14, -1, 3, 9, 19, -1, 32, 18, 29, 4, 30, 
    8, -1, 22, -1, 2, 12, 1, -1, 27, 11, -1, 24, 16, 13, 6, 7, 5, 0, -1, 25, -1, 
    -1, -1, -1, 20, -1, -1, 28, -1, 23, 21, -1, 10 
  );

procedure TSynMflSyn.InitIdent;
var
  i: Integer;
begin
  for i := Low(fIdentFuncTable) to High(fIdentFuncTable) do
    if KeyIndices[i] = -1 then
      fIdentFuncTable[i] := AltFunc;

  fIdentFuncTable[37] := Func95;
  fIdentFuncTable[26] := FuncAlias;
  fIdentFuncTable[24] := FuncAny;
  fIdentFuncTable[11] := FuncArray;
  fIdentFuncTable[18] := FuncAssert;
  fIdentFuncTable[36] := FuncAttrib;
  fIdentFuncTable[34] := FuncConst;
  fIdentFuncTable[35] := FuncConstructor;
  fIdentFuncTable[20] := FuncCustom;
  fIdentFuncTable[12] := FuncDelete;
  fIdentFuncTable[52] := FuncElse;
  fIdentFuncTable[29] := FuncEnum;
  fIdentFuncTable[25] := FuncExcept;
  fIdentFuncTable[33] := FuncFunc;
  fIdentFuncTable[9] := FuncFunction;
  fIdentFuncTable[1] := FuncIf;
  fIdentFuncTable[32] := FuncInline;
  fIdentFuncTable[5] := FuncInt;
  fIdentFuncTable[16] := FuncLink;
  fIdentFuncTable[13] := FuncObject;
  fIdentFuncTable[44] := FuncOpt;
  fIdentFuncTable[50] := FuncRange;
  fIdentFuncTable[22] := FuncRangeint;
  fIdentFuncTable[49] := FuncRecord;
  fIdentFuncTable[31] := FuncRef;
  fIdentFuncTable[39] := FuncResolve;
  fIdentFuncTable[8] := FuncRet;
  fIdentFuncTable[28] := FuncShow;
  fIdentFuncTable[47] := FuncSpread;
  fIdentFuncTable[17] := FuncString;
  fIdentFuncTable[19] := FuncThen;
  fIdentFuncTable[2] := FuncTry;
  fIdentFuncTable[15] := FuncType;
  fIdentFuncTable[0] := FuncVar;
end;

{$Q-}
function TSynMflSyn.HashKey(Str: PWideChar): Cardinal;
begin
  Result := 0;
  while IsIdentChar(Str^) do
  begin
    Result := Result * 946 + Ord(Str^) * 40;
    inc(Str);
  end;
  Result := Result mod 53;
  fStringLen := Str - fToIdent;
end;
{$Q+}

function TSynMflSyn.Func95(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKeyword
  else
    Result := tkIdentifier;
end;

function TSynMflSyn.FuncAlias(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKeyword
  else
    Result := tkIdentifier;
end;

function TSynMflSyn.FuncAny(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkTypeConstructor
  else
    Result := tkIdentifier;
end;

function TSynMflSyn.FuncArray(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkTypeConstructor
  else
    Result := tkIdentifier;
end;

function TSynMflSyn.FuncAssert(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKeyword
  else
    Result := tkIdentifier;
end;

function TSynMflSyn.FuncAttrib(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKeyword
  else
    Result := tkIdentifier;
end;

function TSynMflSyn.FuncConst(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKeyword
  else
    Result := tkIdentifier;
end;

function TSynMflSyn.FuncConstructor(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKeyword
  else
    Result := tkIdentifier;
end;

function TSynMflSyn.FuncCustom(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkTypeConstructor
  else
    Result := tkIdentifier;
end;

function TSynMflSyn.FuncDelete(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKeyword
  else
    Result := tkIdentifier;
end;

function TSynMflSyn.FuncElse(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKeyword
  else
    Result := tkIdentifier;
end;

function TSynMflSyn.FuncEnum(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkTypeConstructor
  else
    Result := tkIdentifier;
end;

function TSynMflSyn.FuncExcept(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKeyword
  else
    Result := tkIdentifier;
end;

function TSynMflSyn.FuncFunc(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKeyword
  else
    Result := tkIdentifier;
end;

function TSynMflSyn.FuncFunction(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKeyword
  else
    Result := tkIdentifier;
end;

function TSynMflSyn.FuncIf(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKeyword
  else
    Result := tkIdentifier;
end;

function TSynMflSyn.FuncInline(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKeyword
  else
    Result := tkIdentifier;
end;

function TSynMflSyn.FuncInt(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkTypeConstructor
  else
    Result := tkIdentifier;
end;

function TSynMflSyn.FuncLink(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKeyword
  else
    Result := tkIdentifier;
end;

function TSynMflSyn.FuncObject(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkTypeConstructor
  else
    Result := tkIdentifier;
end;

function TSynMflSyn.FuncOpt(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKeyword
  else
    Result := tkIdentifier;
end;

function TSynMflSyn.FuncRange(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkTypeConstructor
  else
    Result := tkIdentifier;
end;

function TSynMflSyn.FuncRangeint(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkTypeConstructor
  else
    Result := tkIdentifier;
end;

function TSynMflSyn.FuncRecord(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkTypeConstructor
  else
    Result := tkIdentifier;
end;

function TSynMflSyn.FuncRef(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkTypeConstructor
  else
    Result := tkIdentifier;
end;

function TSynMflSyn.FuncResolve(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKeyword
  else
    Result := tkIdentifier;
end;

function TSynMflSyn.FuncRet(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKeyword
  else
    Result := tkIdentifier;
end;

function TSynMflSyn.FuncShow(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKeyword
  else
    Result := tkIdentifier;
end;

function TSynMflSyn.FuncSpread(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKeyword
  else
    Result := tkIdentifier;
end;

function TSynMflSyn.FuncString(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkTypeConstructor
  else
    Result := tkIdentifier;
end;

function TSynMflSyn.FuncThen(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKeyword
  else
    Result := tkIdentifier;
end;

function TSynMflSyn.FuncTry(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKeyword
  else
    Result := tkIdentifier;
end;

function TSynMflSyn.FuncType(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKeyword
  else
    Result := tkIdentifier;
end;

function TSynMflSyn.FuncVar(Index: Integer): TtkTokenKind;
begin
  if IsCurrentToken(KeyWords[Index]) then
    Result := tkKeyword
  else
    Result := tkIdentifier;
end;

function TSynMflSyn.AltFunc(Index: Integer): TtkTokenKind;
begin
  Result := tkIdentifier;
end;

function TSynMflSyn.IdentKind(MayBe: PWideChar): TtkTokenKind;
var
  Key: Cardinal;
begin
  fToIdent := MayBe;
  Key := HashKey(MayBe);
  if Key <= High(fIdentFuncTable) then
    Result := fIdentFuncTable[Key](KeyIndices[Key])
  else
    Result := tkIdentifier;
end;

procedure TSynMflSyn.SpaceProc;
begin
  inc(Run);
  fTokenID := tkSpace;
  while (FLine[Run] <= #32) and not IsLineEnd(Run) do inc(Run);
end;

procedure TSynMflSyn.NullProc;
begin
  fTokenID := tkNull;
  inc(Run);
end;

procedure TSynMflSyn.CRProc;
begin
  fTokenID := tkSpace;
  inc(Run);
  if fLine[Run] = #10 then
    inc(Run);
end;

procedure TSynMflSyn.LFProc;
begin
  fTokenID := tkSpace;
  inc(Run);
end;

procedure TSynMflSyn.CommentOpenProc;
begin
  Inc(Run);
  fRange := rsComment;
  CommentProc;
  fTokenID := tkComment;
end;

procedure TSynMflSyn.CommentProc;
begin
  fTokenID := tkComment;
  repeat
    if (fLine[Run] = '\') then
    begin
      Inc(Run, 1);
      fRange := rsUnKnown;
      Break;
    end;
    if not IsLineEnd(Run) then
      Inc(Run);
  until IsLineEnd(Run);
end;

procedure TSynMflSyn.TextOpenProc;
begin
  Inc(Run);
  fRange := rsText;
  TextProc;
  fTokenID := tkText;
end;

procedure TSynMflSyn.TextProc;
begin
  fTokenID := tkText;
  repeat
    if (fLine[Run] = '"') then
    begin
      Inc(Run, 1);
      fRange := rsUnKnown;
      Break;
    end;
    if not IsLineEnd(Run) then
      Inc(Run);
  until IsLineEnd(Run);
end;

constructor TSynMflSyn.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  fCaseSensitive := False;

  fCommentAttri := TSynHighLighterAttributes.Create(SYNS_AttrComment, SYNS_FriendlyAttrComment);
  fCommentAttri.Style := [fsItalic];
  fCommentAttri.Foreground := clGreen;
  AddAttribute(fCommentAttri);

  fIdentifierAttri := TSynHighLighterAttributes.Create(SYNS_AttrIdentifier, SYNS_FriendlyAttrIdentifier);
  AddAttribute(fIdentifierAttri);

  fKeywordAttri := TSynHighLighterAttributes.Create(SYNS_AttrKeyword, SYNS_FriendlyAttrKeyword);
  fKeywordAttri.Style := [fsBold];
  fKeywordAttri.Foreground := clNavy;
  AddAttribute(fKeywordAttri);

  fNumberDecAttri := TSynHighLighterAttributes.Create(SYNS_AttrNumberDec, SYNS_FriendlyAttrNumberDec);
  fNumberDecAttri.Foreground := clBlue;
  AddAttribute(fNumberDecAttri);

  fNumberHexAttri := TSynHighLighterAttributes.Create(SYNS_AttrNumberHex, SYNS_FriendlyAttrNumberHex);
  fNumberHexAttri.Foreground := clBlue;
  AddAttribute(fNumberHexAttri);

  fSpaceAttri := TSynHighLighterAttributes.Create(SYNS_AttrSpace, SYNS_FriendlyAttrSpace);
  AddAttribute(fSpaceAttri);

  fSymbolAttri := TSynHighLighterAttributes.Create(SYNS_AttrSymbol, SYNS_FriendlyAttrSymbol);
  fSymbolAttri.Foreground := clGray;
  AddAttribute(fSymbolAttri);

  fTextAttri := TSynHighLighterAttributes.Create(SYNS_AttrText, SYNS_FriendlyAttrText);
  fTextAttri.Foreground := clBlue;
  AddAttribute(fTextAttri);

  fTypeConstructorAttri := TSynHighLighterAttributes.Create(SYNS_AttrTypeConstructor, SYNS_FriendlyAttrTypeConstructor);
  fTypeConstructorAttri.Style := [fsBold];
  fTypeConstructorAttri.Foreground := clRed;
  AddAttribute(fTypeConstructorAttri);

  SetAttributesOnChange(DefHighlightChange);
  InitIdent;
  fDefaultFilter := SYNS_FilterMFL;
  fRange := rsUnknown;
end;

procedure TSynMflSyn.IdentProc;
begin
  fTokenID := IdentKind((fLine + Run));
  Inc(Run, fStringLen);
  while IsIdentChar(fLine[Run]) do
  begin
    Inc(Run);
  end;
end;

procedure TSynMflSyn.NumberDecProc;
begin
  fTokenID := tkNumberDec;
  repeat
    Inc(Run);
  until not CharInSet(fLine[Run], ['0'..'9']);
  if fLine[Run] = '.' then
  begin
    repeat
      Inc(Run);
    until not CharInSet(fLine[Run], ['0'..'9']);
  end;
  if fLine[Run] = 'e' then
  begin
    Inc(Run);
	if CharInSet(fLine[Run], ['+', '-']) then
	begin
	  Inc(Run);
	end;
    while CharInSet(fLine[Run], ['0'..'9']) do
	begin
      Inc(Run);
    end;
  end;
end;

procedure TSynMflSyn.NumberHexProc;
begin
  fTokenID := tkNumberHex;
  repeat
    Inc(Run);
  until not CharInSet(fLine[Run], ['0'..'9', 'A' .. 'F', 'a' .. 'f']);
end;

procedure TSynMflSyn.SymbolProc;
begin
  fTokenID := tkSymbol;
  Inc(Run);
end;

procedure TSynMflSyn.UnknownProc;
begin
  inc(Run);
  fTokenID := tkUnknown;
end;

procedure TSynMflSyn.Next;
begin
  fTokenPos := Run;
//  case fRange of
//  else
    case fLine[Run] of
      #0: NullProc;
      #10: LFProc;
      #13: CRProc;
      '\': CommentOpenProc;
      '"': TextOpenProc;
      #1..#9, #11, #12, #14..#32: SpaceProc;
      'A'..'Z', 'a'..'z', '_': IdentProc;
      '0' .. '9': NumberDecProc;
      '$': NumberHexProc;
      '=', '(', ')', '[', ']', '{', '}', ':', '.', ',', '&', '+', '-', '*', '/', '%', '^', '~', '?', '@', '#': SymbolProc;
    else
      UnknownProc;
    end;
//  end;
  inherited;
end;

function TSynMflSyn.GetDefaultAttribute(Index: Integer): TSynHighLighterAttributes;
begin
  case Index of
    SYN_ATTR_COMMENT: Result := fCommentAttri;
    SYN_ATTR_IDENTIFIER: Result := fIdentifierAttri;
    SYN_ATTR_WHITESPACE: Result := fSpaceAttri;
    SYN_ATTR_SYMBOL: Result := fSymbolAttri;
  else
    Result := nil;
  end;
end;

function TSynMflSyn.GetEol: Boolean;
begin
  Result := Run = fLineLen + 1;
end;

function TSynMflSyn.GetTokenID: TtkTokenKind;
begin
  Result := fTokenId;
end;

function TSynMflSyn.GetTokenAttribute: TSynHighLighterAttributes;
begin
  case GetTokenID of
    tkComment: Result := fCommentAttri;
    tkIdentifier: Result := fIdentifierAttri;
    tkKeyword: Result := fKeywordAttri;
    tkNumberDec: Result := fNumberDecAttri;
    tkNumberHex: Result := fNumberHexAttri;
    tkSpace: Result := fSpaceAttri;
    tkSymbol: Result := fSymbolAttri;
    tkText: Result := fTextAttri;
    tkTypeConstructor: Result := fTypeConstructorAttri;
    tkUnknown: Result := fIdentifierAttri;
  else
    Result := nil;
  end;
end;

function TSynMflSyn.GetTokenKind: Integer;
begin
  Result := Ord(fTokenId);
end;

function TSynMflSyn.IsIdentChar(AChar: WideChar): Boolean;
begin
  case AChar of
    '_', '0'..'9', 'a'..'z', 'A'..'Z':
      Result := True;
    else
      Result := False;
  end;
end;

function TSynMflSyn.GetSampleSource: string;
begin
  Result := 
    '\ Sample source \'#13#10 +
    'const S = "Hello World"'#13#10 +
    'var Len = Length(S)'#13#10 +
    'type T = string(Len)'#13#10 +
    'delete Len''function Words(Text: T = Split(Text, " ")';
end;

function TSynMflSyn.IsFilterStored: Boolean;
begin
  Result := fDefaultFilter <> SYNS_FilterMFL;
end;

class function TSynMflSyn.GetFriendlyLanguageName: string;
begin
  Result := SYNS_FriendlyLangMFL;
end;

class function TSynMflSyn.GetLanguageName: string;
begin
  Result := SYNS_LangMFL;
end;

procedure TSynMflSyn.ResetRange;
begin
  fRange := rsUnknown;
end;

procedure TSynMflSyn.SetRange(Value: Pointer);
begin
  fRange := TRangeState(Value);
end;

function TSynMflSyn.GetRange: Pointer;
begin
  Result := Pointer(fRange);
end;

initialization
{$IFNDEF SYN_CPPB_1}
  RegisterPlaceableHighlighter(TSynMflSyn);
{$ENDIF}
end.
