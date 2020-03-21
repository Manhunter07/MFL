unit Parser.Exporter;

interface

uses
  System.SysUtils, System.Math, System.Classes,
  Parser.Syntax, Parser.Language, Parser.Dictionary, Parser.Exception;

type
  TParserObjectKind = (okUnknown, okConstant, okVariable, okFunction);

  TParserObjectKinds = set of TParserObjectKind;

  TParserObjectKindHelper = record helper for TParserObjectKind
    constructor Create(const AObject: TParserObject);
  end;

  TParserExporter = class abstract
  private
    FTarget: TStringList;
  protected
    procedure ExportAlias(const AAlias, AName: String); virtual; abstract;
    procedure ExportConstant(const AConstant: TParserConstant); virtual; abstract;
    procedure ExportVariable(const AVariable: TParserVariable); virtual; abstract;
    procedure ExportFunction(const AFunction: TParserFunction); virtual; abstract;
  public
    class function CanExport(const AObject: TParserObject): Boolean; virtual;
    property Target: TStringList read FTarget write FTarget;
    constructor Create(const ATarget: TStringList);
    procedure &Export(const AObject: TParserObject); overload;
    procedure &Export(const ADictionary: TParserDictionary; AName: String = String.Empty); overload;
  end;

  TParserCodeExporter = class(TParserExporter)
  private const
    FDeclPatternNoValue = '%s %s';
    FDeclPattern = Concat(FDeclPatternNoValue, ' = %s');
  protected
    procedure ExportAlias(const AAlias, AName: String); override;
    procedure ExportConstant(const AConstant: TParserConstant); override;
    procedure ExportVariable(const AVariable: TParserVariable); override;
    procedure ExportFunction(const AFunction: TParserFunction); override;
  end;

implementation

{ TParserExportKindHelper }

constructor TParserObjectKindHelper.Create(const AObject: TParserObject);
begin
  if AObject is TParserFunction then
  begin
    Self := okFunction;
  end else
  begin
    if AObject is TParserVariable then
    begin
      Self := okVariable;
    end else
    begin
      if AObject is TParserConstant then
      begin
        Self := okConstant;
      end else
      begin
        Self := okUnknown;
      end;
    end;
  end;
end;

{ TParserExporter }

class function TParserExporter.CanExport(const AObject: TParserObject): Boolean;
begin
  Result := True;
end;

constructor TParserExporter.Create(const ATarget: TStringList);
begin
  inherited Create;
  Target := ATarget;
end;

procedure TParserExporter.Export(const ADictionary: TParserDictionary; AName: String);
var
  LObject: TParserObject;
begin
  if AName.IsEmpty then
  begin
    for LObject in ADictionary do
    begin
      &Export(LObject);
    end;
  end else
  begin
    &Export(ADictionary[AName]);
  end;
end;

procedure TParserExporter.Export(const AObject: TParserObject);
begin
  if not CanExport(AObject) then
  begin
    raise EParserUnsupportedError.Create('Object cannot be exported');
  end;
  case TParserObjectKind.Create(AObject) of
    okUnknown:
      begin
        raise EParserUnsupportedError.Create('Object cannot be exported');
      end;
    okConstant:
      begin
        ExportConstant(AObject as TParserConstant);
      end;
    okVariable:
      begin
        ExportVariable(AObject as TParserVariable);
      end;
    okFunction:
      begin
        ExportFunction(AObject as TParserFunction);
      end;
  end;
end;

{ TParserCodeExporter }

procedure TParserCodeExporter.ExportAlias(const AAlias, AName: String);
begin
  Target.Add(String.Format(FDeclPattern, [kwAlias.ToString, AAlias, AName]));
end;

procedure TParserCodeExporter.ExportConstant(const AConstant: TParserConstant);
begin
  Target.Add(String.Format(FDeclPattern, [kwConstant.ToString, AConstant.Name, AConstant.Value[[]].ToString]));
end;

procedure TParserCodeExporter.ExportFunction(const AFunction: TParserFunction);
begin
  Target.Add(String.Format(FDeclPattern, [kwFunction.ToString, AFunction.Name, '<function>']));
end;

procedure TParserCodeExporter.ExportVariable(const AVariable: TParserVariable);
var
  LValue: Double;
begin
  LValue :=  AVariable.Value[[]];
  if IsZero(LValue) then
  begin
    Target.Add(String.Format(FDeclPatternNoValue, [kwVariable.ToString, AVariable.Name]));
  end else
  begin
    Target.Add(String.Format(FDeclPattern, [kwVariable.ToString, AVariable.Name, LValue.ToString]));
  end;
end;

end.
