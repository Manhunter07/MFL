unit Parser.Package;

interface

uses
  System.SysUtils, System.Types, System.Classes, System.Math, System.Generics.Defaults, System.Generics.Collections, System.DateUtils, System.IOUtils,
  {$IFDEF MSWINDOWS}Winapi.Windows,{$ENDIF}
  Parser.Dictionary, Parser.Exception, Parser.Language, Parser.Syntax, Parser.Value;

type
  TParserPackage = class;

  IParserPackageLinks = interface
    ['{9265B6A2-0EFB-4A86-9959-637092032439}']
    function HasPackage(const AName: String): Boolean;
    function GetPackages(const AName: String): TParserPackage;
    property Packages[const AName: String]: TParserPackage read GetPackages;
  end;

  TParserStandardPackage = ({$IFDEF CONSOLE}pkConsole,{$ENDIF} {$IFDEF MSWINDOWS}pkLibrary,{$ENDIF} pkConvert, pkRandom, pkDateTime, pkThread, pkInfo, pkFile);

  TParserStandardPackages = set of TParserStandardPackage;

  TParserStandardPackageHelper = record helper for TParserStandardPackage
  private
    function GetName: String;
  public
    property Name: String read GetName;
    procedure CreatePackage(out APackage: TParserPackage);
  end;

  TParserStandardPackagesHelper = record helper for TParserStandardPackages
  private
    class function GetDefaultPackages: TParserStandardPackages; static;
  public
    class property DefaultPackages: TParserStandardPackages read GetDefaultPackages;
  end;

  TParserStandardPackageProvider = class
  private class var
    FPackages: array [TParserStandardPackage] of TParserPackage;
  private
    class function GetPackages(const APackage: TParserStandardPackage): TParserPackage; static;
  public
    class property Packages[const APackage: TParserStandardPackage]: TParserPackage read GetPackages; default;
    {$IFDEF CONSOLE}class property Console: TParserPackage index pkConsole read GetPackages;{$ENDIF}
    {$IFDEF MSWINDOWS}class property &Library: TParserPackage index pkLibrary read GetPackages;{$ENDIF}
    class property Convert: TParserPackage index pkConvert read GetPackages;
    class property Random: TParserPackage index pkRandom read GetPackages;
    class property DateTime: TParserPackage index pkDateTime read GetPackages;
    class property Thread: TParserPackage index pkThread read GetPackages;
    class property Info: TParserPackage index pkInfo read GetPackages;
    class constructor Create;
    class destructor Destroy;
  end;

  TParserPackage = class(TSingletonImplementation, IParserValueSupplier, IParserPackageLinks)
  private class var
    FDefaultPackage: TParserPackage;
  private
    FName: String;
    FExplicit: Boolean;
    FDictionary: TParserDictionary;
    FPackageLinks: TObjectDictionary<String, TParserPackage>;
    function GetValues(const AName: String; const AArgs: TArray<TParserValue>): TParserValue;
    function GetRefTargets(const AName: String): IParserValueRefTarget;
    function GetMinArgCount(const AName: String): Integer;
    function GetMaxArgCount(const AName: String): Integer;
    function GetPackageLinks(const AName: String): TParserPackage;
  protected
    // Value Supplier methods should return only local values and those from linked packages
    property Values[const AName: String; const AArgs: TArray<TParserValue>]: TParserValue read GetValues;
    property RefTargets[const AName: String]: IParserValueRefTarget read GetRefTargets;
    property MinArgCount[const AName: String]: Integer read GetMinArgCount;
    property MaxArgCount[const AName: String]: Integer read GetMaxArgCount;
    function ContainsValue(const AName: String): Boolean;
    function ContainsRefTarget(const AName: String): Boolean;
  public
    class property DefaultPackage: TParserPackage read FDefaultPackage;
    property Name: String read FName;
    property Explicit: Boolean read FExplicit default False;
    property Dictionary: TParserDictionary read FDictionary;
    property PackageLinks[const AName: String]: TParserPackage read GetPackageLinks;
    class constructor Create;
    class destructor Destroy;
    constructor Create(const AName: String; const AExplicit: Boolean = False);
    destructor Destroy; override;
    function HasPackageLink(const AName: String): Boolean;
    procedure AddPackageLink(const APackage: TParserPackage);
    procedure RemovePackageLink(const AName: String);
  private
    // In order to clearify that those are only linked and no sub packages
    function IParserPackageLinks.GetPackages = GetPackageLinks;
    function IParserPackageLinks.HasPackage = HasPackageLink;
  end;

  TParserCustomPackage = class(TParserPackage)
  private
    FFileName: String;
  protected
    procedure LoadFromFile; virtual;
  public
    class function FileExtension: String; virtual;
    property FileName: String read FFileName;
    constructor Create(const AFileName: String);
  end;

  TParserMemoryPackage = class(TParserPackage)
  public const
    Offset = 1;
  private
    FMemory: TDictionary<Integer, TArray<TParserValue>>;
    function GetSize: Integer;
    function GetMemory(const AAddress: Integer): TArray<TParserValue>;
    procedure SetMemory(const AAddress: Integer; const AValue: TArray<TParserValue>);
  protected
    function NextAddress: Integer;
    procedure ValidateAddress(const AAddress: Integer);
    procedure ValidateAddressAndIndex(const AAddress, AIndex: Integer);
  public
    property Memory[const AAddress: Integer]: TArray<TParserValue> read GetMemory write SetMemory;
    property Size: Integer read GetSize;
    constructor Create(const AName: String = 'Memory'; const AExplicit: Boolean = False; const ASize: Integer = 0);
    destructor Destroy; override;
  end deprecated 'Use structured types instead';

implementation

uses
  Parser;

{ TParserStandardPackageHelper }

procedure TParserStandardPackageHelper.CreatePackage(out APackage: TParserPackage);

  {$IFDEF CONSOLE}
  procedure CreateConsole;
  begin
    APackage.Dictionary.Add(TParserReferenceFunction.Create('Read', [TParserParam.Create('ValueType')],
      function (Params: TArray<TParserValue>): TParserValue
      var
        LInput: String;
      begin
        Readln(LInput);
        Result := TParserValue.Create(LInput);
      end));
    APackage.Dictionary.Add(TParserReferenceFunction.Create('Write', [TParserParam.Create('Value')],
      function (Params: TArray<TParserValue>): TParserValue

        procedure Write(const AValue: TParserValue);
        var
          LOutput: String;
          LValue: TParserValue;
        begin
          case Params[0].Kind of
            vkDouble:
              begin
                LOutput := AValue.ToString;
              end;
            vkString:
              begin
                LOutput := AValue.AsString;
              end;
            vkArray:
              begin
                for LValue in AValue.AsArray do
                begin
                  Write(LValue);
                end;
              end;
            else
              begin
                raise EParserConsoleError.CreateFmt('%s cannot be written to console', [Params[0].ToString.QuotedString]);
              end;
          end;
          Writeln(LOutput);
        end;

      begin
        Write(Params[0]);
        Result := Params[0];
      end));
    APackage.Dictionary.Add(TParserReferenceFunction.Create('Info', [],
      function (Params: TArray<TParserValue>): TParserValue
      const
        LInfoLines: TArray<String> = [
          'MFL Version 0.1 (alpha)',
          '(c) 2021 Dennis Göhlert',
          'See www.mflang.info for more information'
        ];
      begin
        WriteLn(String.Join(sLineBreak, LInfoLines));
        Result := TParserValue.Empty[vkDouble];
      end));
  end;
  {$ENDIF}

  {$IFDEF MSWINDOWS}
  procedure CreateLibrary;
  begin
    APackage.Dictionary.Add(TParserReferenceFunction.Create('Load', [TParserParam.Create('Name')],
      function (Params: TArray<TParserValue>): TParserValue
      begin
        Result := TParserValue.Create(LoadLibrary(PChar(Params[0].AsString)));
      end));
    APackage.Dictionary.Add(TParserReferenceFunction.Create('Free', [TParserParam.Create('Library')],
      function (Params: TArray<TParserValue>): TParserValue
      begin
        Result := TParserValue.Create(FreeLibrary(Params[0].AsInteger));
      end));
  end;
  {$ENDIF}

  procedure CreateConvert;
  begin
    APackage.Dictionary.Add(TParserReferenceFunction.Create('ToString', [TParserParam.Create('Value')],
      function (Params: TArray<TParserValue>): TParserValue
      begin
        Result := TParserValue.Create(Params[0].AsDouble.ToString(TFormatSettings.Invariant));
      end));
    APackage.Dictionary.Add(TParserReferenceFunction.Create('ToFloat', [TParserParam.Create('Value')],
      function (Params: TArray<TParserValue>): TParserValue
      begin
        Result := TParserValue.Create(Params[0].AsString.ToDouble);
      end));
    APackage.Dictionary.Add(TParserReferenceFunction.Create('ToArray', [TParserParam.Create('Value')],
      function (Params: TArray<TParserValue>): TParserValue
      begin
        Result := TParserValue.Create(Params[0].AsArray);
      end));
  end;

  procedure CreateRandom;
  begin
    APackage.Dictionary.Add(TParserReferenceFunction.Create('Randomize', [],
      function (Params: TArray<TParserValue>): TParserValue
      begin
        Randomize;
        Result := TParserValue.Create(RandSeed);
      end));
    APackage.Dictionary.Add(TParserReferenceFunction.Create('Get', [TParserParam.Create('RangeBegin'), TParserParam.Create('RangeEnd')],
      function (Params: TArray<TParserValue>): TParserValue
      begin
        case TParserValue.Compare(Params[0], Params[1]) of
          LessThanValue:
            begin
              Result := TParserValue.Create(Params[0].AsDouble + Random * (Params[1].AsDouble - Params[0].AsDouble));
            end;
          EqualsValue:
            begin
              Result := Params[0];
            end;
          GreaterThanValue:
            begin
              Result := TParserValue.Create(Params[1].AsDouble + Random * (Params[0].AsDouble - Params[1].AsDouble));
            end;
        end;
      end));
    APackage.Dictionary.Add(TParserReferenceFunction.Create('GetInt', [TParserParam.Create('RangeBegin'), TParserParam.Create('RangeEnd')],
      function (Params: TArray<TParserValue>): TParserValue
      begin
        Result := TParserValue.Create(RandomRange(Params[0].AsInteger, Params[1].AsInteger));
      end));
  end;

  procedure CreateDateTime;
  begin
    APackage.Dictionary.Add(TParserRangeType.Create('DateTime', TParserValue.Create(MinDateTime), TParserValue.Create(MaxDateTime)));
    APackage.Dictionary.Add(TParserReferenceFunction.Create('Make', [TParserParam.Create('Year'), TParserParam.Create('Month'), TParserParam.Create('Day'), TParserParam.Create('Hour'), TParserParam.Create('Minute'), TParserParam.Create('Second'), TParserParam.Create('Milisecond', TParserValue.Empty[vkDouble])],
      function (Params: TArray<TParserValue>): TParserValue
      begin
        Result := TParserValue.Create(EncodeDateTime(Params[0].AsInteger, Params[0].AsInteger, Params[0].AsInteger, Params[0].AsInteger, Params[0].AsInteger, Params[0].AsInteger, Params[0].AsInteger));
      end));
  end;

  procedure CreateThread;
  begin
    APackage.Dictionary.Add(TParserRangeIntegerType.Create('ID', TParserValue.Create(MinDateTime), TParserValue.Create(MaxDateTime)));
    APackage.Dictionary.Add(TParserReferenceFunction.Create('Current', [],
      function (Params: TArray<TParserValue>): TParserValue
      begin
        Result := TParserValue.Create(TThread.Current.ThreadID);
      end));
  end;

  procedure CreateInfo;
  begin
    APackage.Dictionary.Add(TParserRecordType.Create('ValueInfo', [TParserValue.Create('Kind')]));
    APackage.Dictionary.Add(TParserRecordType.Create('ConstInfo', [TParserValue.Create('Name'), TParserValue.Create('Value')]));
    APackage.Dictionary.Add(TParserRecordType.Create('VarInfo', [TParserValue.Create('Name'), TParserValue.Create('Value')]));
    APackage.Dictionary.Add(TParserRecordType.Create('FunctionInfo', [TParserValue.Create('Name'), TParserValue.Create('Args')]));
    APackage.Dictionary.Add(TParserRecordType.Create('TypeInfo', [TParserValue.Create('Name'), TParserValue.Create('Args')]));
    APackage.Dictionary.Add(TParserReferenceFunction.Create('Get', [TParserParam.Create('Value')],
      function (Params: TArray<TParserValue>): TParserValue
      begin
        Result := TParserValue.Create([TPair<String, TParserValue>.Create('Kind', TParserValue.Create(Ord(Params[0].Kind))), TPair<String, TParserValue>.Create('Value', Params[0])]);
      end));
  end;

  procedure CreateFile;
  begin
    APackage.Dictionary.Add(TParserReferenceFunction.Create('Exists', [TParserParam.Create('Name')],
      function (Params: TArray<TParserValue>): TParserValue
      begin
        Result := TParserValue.Create(TFile.Exists(Params[0].AsString));
      end));
    APackage.Dictionary.Add(TParserReferenceFunction.Create('Create', [TParserParam.Create('Name')],
      function (Params: TArray<TParserValue>): TParserValue
      begin
        Result := TParserValue.Create(NativeInt(TFile.Create(Params[0].AsString)));
      end));
    APackage.Dictionary.Add(TParserReferenceFunction.Create('Open', [TParserParam.Create('Name'), TParserParam.Create('ReadOnly', TParserValue.Create(False))],
      function (Params: TArray<TParserValue>): TParserValue
      var
        LAccess: TFileAccess;
      begin
        if Params[1].AsBoolean then
        begin
          LAccess := TFileAccess.faRead;
        end else
        begin
          LAccess := TFileAccess.faReadWrite;
        end;
        Result := TParserValue.Create(NativeInt(TFile.Open(Params[0].AsString, TFileMode.fmOpen, LAccess)));
      end));
//    APackage.Dictionary.Add(TParserReferenceFunction.Create('Close', [TParserParam.Create('Name')],
//      function (Params: TArray<TParserValue>): TParserValue
//      begin
//        Result := TParserValue.Create(NativeInt(TFile.Close (Params[0].AsString)));
//      end));
  end;

begin
  APackage := TParserPackage.Create(Name, True);
  case Self of
    {$IFDEF CONSOLE}
    pkConsole:
      begin
        CreateConsole;
      end;
    {$ENDIF}
    {$IFDEF MSWINDOWS}
    pkLibrary:
      begin
        CreateLibrary;
      end;
    {$ENDIF}
    pkConvert:
      begin
        CreateConvert;
      end;
    pkRandom:
      begin
        CreateRandom;
      end;
    pkDateTime:
      begin
        CreateDateTime;
      end;
    pkThread:
      begin
        CreateThread;
      end;
    pkInfo:
      begin
        CreateInfo;
      end;
    pkFile:
      begin
        CreateFile;
      end;
  end;
end;

function TParserStandardPackageHelper.GetName: String;
const
  LNames: array [TParserStandardPackage] of String = ({$IFDEF CONSOLE}'Console',{$ENDIF} {$IFDEF MSWINDOWS}'Library',{$ENDIF} 'Convert', 'Random', 'DateTime', 'Thread', 'Info', 'File');
begin
  Result := LNames[Self];
end;

{ TParserStandardPackagesHelper }

class function TParserStandardPackagesHelper.GetDefaultPackages: TParserStandardPackages;
begin
  Result := [Low(TParserStandardPackage) .. High(TParserStandardPackage)];
end;

{ TParserStandardPackageProvider }

class constructor TParserStandardPackageProvider.Create;
var
  LPackage: TParserStandardPackage;
begin
  for LPackage := Low(TParserStandardPackage) to High(TParserStandardPackage) do
  begin
    LPackage.CreatePackage(FPackages[LPackage]);
  end;
end;

class destructor TParserStandardPackageProvider.Destroy;
var
  LPackage: TParserPackage;
begin
  for LPackage in FPackages do
  begin
    LPackage.Free;
  end;
end;

class function TParserStandardPackageProvider.GetPackages(const APackage: TParserStandardPackage): TParserPackage;
begin
  Result := FPackages[APackage];
end;

{ TParserPackage }

procedure TParserPackage.AddPackageLink(const APackage: TParserPackage);
begin
  FPackageLinks.Add(APackage.Name, APackage);
end;

function TParserPackage.ContainsRefTarget(const AName: String): Boolean;
begin

end;

function TParserPackage.ContainsValue(const AName: String): Boolean;
begin
  Result := Dictionary.Contains(AName);
end;

constructor TParserPackage.Create(const AName: String; const AExplicit: Boolean = False);
begin
  inherited Create;
  if not IsValidIdent(AName) then
  begin
    raise EParserPackageNameError.CreateFmt('Invalid package name: %s', [AName.QuotedString]);
  end;
  FName := AName;
  FExplicit := AExplicit;
  FDictionary := TParserDictionary.Create;
  FPackageLinks := TObjectDictionary<String, TParserPackage>.Create([], TIStringComparer.Ordinal);
  if Assigned(TParserPackage.DefaultPackage) then
  begin
    AddPackageLink(TParserPackage.DefaultPackage);
  end;
end;

class constructor TParserPackage.Create;
var
  LConstants: record
    Version: TParserConstant;
    &Nil: TParserConstant;
    Pi: TParserConstant;
    E: TParserConstant;
    Inf: TParserConstant;
    NaN: TParserConstant;
    MinValue: TParserConstant;
    MaxValue: TParserConstant;
    True: TParserConstant;
    False: TParserConstant;
    ReferenceType: TParserConstant;
    NumberType: TParserConstant;
    StringType: TParserConstant;
    ArrayType: TParserConstant;
    RecordType: TParserConstant;
  end;
  LTypes: record
    Any: TParserAnyType;
    Value: TParserValueType;
    Number: TParserNumberType;
    Positive: TParserRangeType;
    Negative: TParserRangeType;
    Integer: TParserIntegerType;
    PosInt: TParserRangeIntegerType;
    NegInt: TParserRangeIntegerType;
    RoundRange: TParserRangeIntegerType;
    TypeKind: TParserEnumType;
    Ref: TParserReferenceType;
    Boolean: TParserEnumType;
    Struct: TParserStructType;
    &String: TParserStringType;
    &Array: TParserArrayType;
    &Record: TParserRecordType;
    Exception: TParserRecordType;
  end;
begin
  LConstants.Version := TParserConstant.Create('Version', TParserValue.Create(0.1));
  LConstants.&Nil := TParserConstant.Create('Nil', TParserValue.Create(nil));
  LConstants.Pi := TParserConstant.Create('Pi', TParserValue.Create(Pi));
  LConstants.E := TParserConstant.Create('E', TParserValue.Create(Exp(1)));
  LConstants.Inf := TParserConstant.Create('Inf', TParserValue.Inf);
  LConstants.NaN := TParserConstant.Create('NaN', TParserValue.NaN);
  LConstants.MinValue := TParserConstant.Create('MinValue', TParserValue.MinValue);
  LConstants.MaxValue := TParserConstant.Create('MaxValue', TParserValue.MaxValue);
  LConstants.&True := TParserConstant.Create('True', TParserValue.Create(True));
  LConstants.&False := TParserConstant.Create('False', TParserValue.Create(False));
  LConstants.ReferenceType := TParserConstant.Create('ReferenceType', TParserValue.Create(Ord(vkReference)));
  LConstants.NumberType := TParserConstant.Create('NumberType', TParserValue.Create(Ord(vkDouble)));
  LConstants.StringType := TParserConstant.Create('StringType', TParserValue.Create(Ord(vkString)));
  LConstants.ArrayType := TParserConstant.Create('ArrayType', TParserValue.Create(Ord(vkArray)));
  LConstants.RecordType := TParserConstant.Create('RecordType', TParserValue.Create(Ord(vkRecord)));
  LTypes.Any := TParserAnyType.Create('Any');
  LTypes.Value := TParserValueType.Create('Value');
  LTypes.Number := TParserNumberType.Create('Number');
  LTypes.Positive := TParserRangeType.Create('Positive', TParserValue.Empty[vkDouble], LConstants.Inf.Value[[]]);
  LTypes.Negative := TParserRangeType.Create('Negative', TParserValue.Create(-LConstants.Inf.Value[[]].AsDouble), TParserValue.Empty[vkDouble]);
  LTypes.Integer := TParserIntegerType.Create('Integer');
  LTypes.PosInt := TParserRangeIntegerType.Create('PosInt', LTypes.Positive.MinValue, LTypes.Positive.MaxValue);
  LTypes.NegInt := TParserRangeIntegerType.Create('NegInt', LTypes.Negative.MinValue, LTypes.Negative.MaxValue);
  LTypes.RoundRange := TParserRangeIntegerType.Create('RoundRange', TParserValue.Create(Low(TRoundToEXRangeExtended)), TParserValue.Create(High(TRoundToEXRangeExtended)));
  LTypes.TypeKind := TParserEnumType.Create('TypeKind', [LConstants.NumberType.Value[[]], LConstants.StringType.Value[[]],  LConstants.ArrayType.Value[[]], LConstants.RecordType.Value[[]]]);
  LTypes.Ref := TParserReferenceType.Create('Ref');
  LTypes.Boolean := TParserEnumType.Create('Boolean', [LConstants.False.Value[[]], LConstants.True.Value[[]]]);
  LTypes.Struct := TParserStructType.Create('Struct');
  LTypes.&String := TParserStringType.Create('String');
  LTypes.&Array := TParserArrayType.Create('Array');
  LTypes.Exception := TParserRecordType.Create('Exception', [TParserValue.Create('Message')]);
  LTypes.&Boolean.&Constructor := TParserReferenceFunction.Create('Boolean', [TParserParam.Create('Value', TParserValue.Create(0))],
    function (Params: TArray<TParserValue>): TParserValue
    begin
      Result := TParserValue.Create(not Params[0].IsEmpty);
    end);
  LTypes.&String.&Constructor := TParserReferenceFunction.Create('String', [TParserParam.Create('Length', TParserValue.Create(0), LTypes.PosInt)],
    function (Params: TArray<TParserValue>): TParserValue
    begin
      Result := TParserValue.Create(String.Create(' ', Params[0].AsInteger));
    end);
  FDefaultPackage := TParserPackage.Create('System');
  FDefaultPackage.Dictionary.Add(LConstants.Version);
  FDefaultPackage.Dictionary.Add(LConstants.&Nil);
  FDefaultPackage.Dictionary.Add(LConstants.Pi);
  FDefaultPackage.Dictionary.Add(LConstants.E);
  FDefaultPackage.Dictionary.Add(LConstants.Inf);
  FDefaultPackage.Dictionary.Add(LConstants.NaN);
  FDefaultPackage.Dictionary.Add(LConstants.MinValue);
  FDefaultPackage.Dictionary.Add(LConstants.MaxValue);
  FDefaultPackage.Dictionary.Add(LConstants.False);
  FDefaultPackage.Dictionary.Add(LConstants.True);
  FDefaultPackage.Dictionary.Add(LConstants.ReferenceType);
  FDefaultPackage.Dictionary.Add(LConstants.NumberType);
  FDefaultPackage.Dictionary.Add(LConstants.StringType);
  FDefaultPackage.Dictionary.Add(LConstants.ArrayType);
  FDefaultPackage.Dictionary.Add(LConstants.RecordType);
  FDefaultPackage.Dictionary.Add(LTypes.Any);
  FDefaultPackage.Dictionary.Add(LTypes.Value);
  FDefaultPackage.Dictionary.Add(LTypes.Number);
  FDefaultPackage.Dictionary.Add(LTypes.Positive);
  FDefaultPackage.Dictionary.Add(LTypes.Negative);
  FDefaultPackage.Dictionary.Add(LTypes.Integer);
  FDefaultPackage.Dictionary.Add(LTypes.PosInt);
  FDefaultPackage.Dictionary.Add(LTypes.NegInt);
  FDefaultPackage.Dictionary.Add(LTypes.RoundRange);
  FDefaultPackage.Dictionary.Add(LTypes.TypeKind);
  FDefaultPackage.Dictionary.Add(LTypes.Ref);
  FDefaultPackage.Dictionary.Add(LTypes.Boolean);
  FDefaultPackage.Dictionary.Add(LTypes.&String);
  FDefaultPackage.Dictionary.Add(LTypes.&Array);
  FDefaultPackage.Dictionary.Add(LTypes.Exception);
  FDefaultPackage.Dictionary.Add(TParserReferenceFunction.Create('New', [TParserParam.Create('T')],
    function (Params: TArray<TParserValue>): TParserValue
    begin
      Result := (Params[0].AsReference as IParserValueConstraint).New;
    end));
  FDefaultPackage.Dictionary.Add(TParserReferenceFunction.Create('Supports', [TParserParam.Create('T'), TParserParam.Create('Value')],
    function (Params: TArray<TParserValue>): TParserValue
    begin
      Result := TParserValue.Create((Params[0].AsReference as TParserType).Supported[Params[1]]);
    end));
  FDefaultPackage.Dictionary.Add(TParserReferenceFunction.Create('Includes', [TParserParam.Create('Parent'), TParserParam.Create('Child')],
    function (Params: TArray<TParserValue>): TParserValue
    begin
      Result := TParserValue.Create((Params[0].AsReference as TParserType).Included[Params[1].AsReference as TParserType]);
    end));
  FDefaultPackage.Dictionary.Add(TParserReferenceFunction.Create('TypeOf', [TParserParam.Create('Value')],
    function (Params: TArray<TParserValue>): TParserValue
    begin
      Result := TParserValue.Create(Ord(Params[0].Kind));
    end));
  FDefaultPackage.Dictionary.Add(TParserReferenceFunction.Create('ValueOf', [TParserParam.Create('Reference', LTypes.Ref), TParserParam.Create('Args')],
    function (Params: TArray<TParserValue>): TParserValue
    begin
      Result := Params[0].AsReference.Value[Copy(Params, 1, High(Params))];
    end, True));
  FDefaultPackage.Dictionary.Add(TParserReferenceFunction.Create('NameOf', [TParserParam.Create('Object', LTypes.Ref)],
    function (Params: TArray<TParserValue>): TParserValue
    begin
      Result := TParserValue.Create((Params[0].AsReference as TParserObject).Name);
    end));
  FDefaultPackage.Dictionary.Add(TParserReferenceFunction.Create('ConstructorOf', [TParserParam.Create('T', LTypes.Ref)],
    function (Params: TArray<TParserValue>): TParserValue
    begin
      Result := TParserValue.Create((Params[0].AsReference as TParserType).&Constructor);
    end));
  FDefaultPackage.Dictionary.Add(TParserReferenceFunction.Create('Tuple', [TParserParam.Create('Field', LTypes.&String)],
    function (Params: TArray<TParserValue>): TParserValue
    begin
      Result := TParserValue.NewRecord[Params.AsStrings];
    end, True));
  FDefaultPackage.Dictionary.Add(TParserReferenceFunction.Create('List', [TParserParam.Create('Length', LTypes.PosInt)],
    function (Params: TArray<TParserValue>): TParserValue
    begin
      Result := TParserValue.NewArray[Params[0].AsInteger];
    end));
  FDefaultPackage.Dictionary.Add(TParserReferenceFunction.Create('Fib', [TParserParam.Create('X', LTypes.PosInt)],
    function (Params: TArray<TParserValue>): TParserValue
    var
      LIndex: Integer;
      LSequence: TArray<Int64>;
    begin
      LSequence := [0, 1, 1];
      SetLength(LSequence, Succ(Params[0].AsInteger));
      for LIndex := 2 to High(LSequence) do
      begin
        LSequence[LIndex] := LSequence[Pred(LIndex)] + LSequence[Pred(Pred(LIndex))];
      end;
      Result := TParserValue.Create(LSequence[High(LSequence)]);
    end));
  FDefaultPackage.Dictionary.Add(TParserReferenceFunction.Create('Sin', [TParserParam.Create('X', LTypes.Number)],
    function (Params: TArray<TParserValue>): TParserValue
    begin
      Result := TParserValue.Create(Sin(Params[0].AsDouble));
    end));
  FDefaultPackage.Dictionary.Add(TParserReferenceFunction.Create('Cos', [TParserParam.Create('X', LTypes.Number)],
    function (Params: TArray<TParserValue>): TParserValue
    begin
      Result := TParserValue.Create(Cos(Params[0].AsDouble));
    end));
  FDefaultPackage.Dictionary.Add(TParserReferenceFunction.Create('Tan', [TParserParam.Create('X', LTypes.Number)],
    function (Params: TArray<TParserValue>): TParserValue
    begin
      Result := TParserValue.Create(Tan(Params[0].AsDouble));
    end));
  FDefaultPackage.Dictionary.Add(TParserReferenceFunction.Create('ArcSin', [TParserParam.Create('X', LTypes.Number)],
    function (Params: TArray<TParserValue>): TParserValue
    begin
      Result := TParserValue.Create(ArcSin(Params[0].AsDouble));
    end));
  FDefaultPackage.Dictionary.Add(TParserReferenceFunction.Create('ArcCos', [TParserParam.Create('X', LTypes.Number)],
    function (Params: TArray<TParserValue>): TParserValue
    begin
      Result := TParserValue.Create(ArcCos(Params[0].AsDouble));
    end));
  FDefaultPackage.Dictionary.Add(TParserReferenceFunction.Create('ArcTan', [TParserParam.Create('X', LTypes.Number)],
    function (Params: TArray<TParserValue>): TParserValue
    begin
      Result := TParserValue.Create(ArcTan(Params[0].AsDouble));
    end));
  FDefaultPackage.Dictionary.Add(TParserReferenceFunction.Create('Sign', [TParserParam.Create('X', LTypes.Number)],
    function (Params: TArray<TParserValue>): TParserValue
    begin
      Result := TParserValue.Create(Sign(Params[0].AsDouble));
    end));
  FDefaultPackage.Dictionary.Add(TParserReferenceFunction.Create('Log', [TParserParam.Create('Base', LTypes.Number), TParserParam.Create('X', LTypes.Number)],
    function (Params: TArray<TParserValue>): TParserValue
    begin
      Result := TParserValue.Create(LogN(Params[0].AsDouble, Params[1].AsDouble));
    end));
  FDefaultPackage.Dictionary.Add(TParserReferenceFunction.Create('LN', [TParserParam.Create('X', LTypes.Number)],
    function (Params: TArray<TParserValue>): TParserValue
    begin
      Result := TParserValue.Create(Ln(Params[0].AsDouble));
    end));
  FDefaultPackage.Dictionary.Add(TParserReferenceFunction.Create('Not', [TParserParam.Create('X', LTypes.Integer)],
    function (Params: TArray<TParserValue>): TParserValue
    begin
      Result := TParserValue.Create(not Params[0].AsInteger);
    end));
  FDefaultPackage.Dictionary.Add(TParserReferenceFunction.Create('Or', [TParserParam.Create('X', LTypes.Integer)],
    function (Params: TArray<TParserValue>): TParserValue
    var
      LIndex: Integer;
    begin
      Result := Params[0];
      for LIndex := Succ(Low(Params)) to High(Params) do
      begin
        Result := TParserValue.Create(Result.AsInteger or Params[LIndex].AsInteger);
      end;
    end, True));
  FDefaultPackage.Dictionary.Add(TParserReferenceFunction.Create('And', [TParserParam.Create('X', LTypes.Integer)],
    function (Params: TArray<TParserValue>): TParserValue
    var
      LIndex: Integer;
    begin
      Result := Params[0];
      for LIndex := Succ(Low(Params)) to High(Params) do
      begin
        Result := TParserValue.Create(Result.AsInteger and Params[LIndex].AsInteger);
      end;
    end, True));
  FDefaultPackage.Dictionary.Add(TParserReferenceFunction.Create('Xor', [TParserParam.Create('X', LTypes.Integer)],
    function (Params: TArray<TParserValue>): TParserValue
    var
      LIndex: Integer;
    begin
      Result := Params[0];
      for LIndex := Succ(Low(Params)) to High(Params) do
      begin
        Result := TParserValue.Create(Result.AsInteger xor Params[LIndex].AsInteger);
      end;
    end, True));
  FDefaultPackage.Dictionary.Add(TParserReferenceFunction.Create('Round', [TParserParam.Create('X', LTypes.Number), TParserParam.Create('Digit', TParserValue.Empty[vkDouble], LTypes.RoundRange)],
    function (Params: TArray<TParserValue>): TParserValue
    begin
      Result := TParserValue.Create(RoundTo(Params[0].AsDouble, Params[1].AsInteger));
    end));
  FDefaultPackage.Dictionary.Add(TParserReferenceFunction.Create('Ceil', [TParserParam.Create('X', LTypes.Number)],
    function (Params: TArray<TParserValue>): TParserValue
    begin
      Result := TParserValue.Create(Ceil(Params[0].AsDouble));
    end));
  FDefaultPackage.Dictionary.Add(TParserReferenceFunction.Create('Floor', [TParserParam.Create('X', LTypes.Number)],
    function (Params: TArray<TParserValue>): TParserValue
    begin
      Result := TParserValue.Create(Floor(Params[0].AsDouble));
    end));
  FDefaultPackage.Dictionary.Add(TParserReferenceFunction.Create('Trunc', [TParserParam.Create('X', LTypes.Number)],
    function (Params: TArray<TParserValue>): TParserValue
    begin
      Result := TParserValue.Create(Trunc(Params[0].AsDouble));
    end));
  FDefaultPackage.Dictionary.Add(TParserReferenceFunction.Create('Frac', [TParserParam.Create('X', LTypes.Number)],
    function (Params: TArray<TParserValue>): TParserValue
    begin
      Result := TParserValue.Create(Frac(Params[0].AsDouble));
    end));
  FDefaultPackage.Dictionary.Add(TParserReferenceFunction.Create('Min', [TParserParam.Create('Value')],
    function (Params: TArray<TParserValue>): TParserValue
    begin
      Result := TParserValue.Create(MinValue(Params.AsDoubles));
    end, True));
  FDefaultPackage.Dictionary.Add(TParserReferenceFunction.Create('Max', [TParserParam.Create('Value')],
    function (Params: TArray<TParserValue>): TParserValue
    begin
      Result := TParserValue.Create(MaxValue(Params.AsDoubles));
    end, True));
  FDefaultPackage.Dictionary.Add(TParserReferenceFunction.Create('Avg', [TParserParam.Create('Value')],
    function (Params: TArray<TParserValue>): TParserValue
    begin
      Result := TParserValue.Create(Mean(Params.AsDoubles));
    end, True));
  FDefaultPackage.Dictionary.Add(TParserReferenceFunction.Create('Mask', [TParserParam.Create('Output'), TParserParam.Create('Dummy')],
    function (Params: TArray<TParserValue>): TParserValue
    begin
      Result := Params[0];
    end, True));
  FDefaultPackage.Dictionary.Add(TParserReferenceFunction.Create('Count', [TParserParam.Create('X'), TParserParam.Create('Values', LTypes.&Array)],
    function (Params: TArray<TParserValue>): TParserValue
    var
      LValue: TParserValue;
      LResult: Integer;
    begin
      LResult := Default(Integer);
      for LValue in Params[1].AsArray do
      begin
        if LValue.Equals(Params[0]) then
        begin
          Inc(LResult);
        end;
      end;
      Result := TParserValue.Create(LResult);
    end, True));
  FDefaultPackage.Dictionary.Add(TParserReferenceFunction.Create('Index', [TParserParam.Create('X'), TParserParam.Create('Value')],
    function (Params: TArray<TParserValue>): TParserValue
    var
      LResult: Integer;
      LIndex: Integer;
    begin
      LResult := -1;
      for LIndex := Succ(Low(Params)) to High(Params) do
      begin
        if Params[LIndex].Equals(Params[0]) then
        begin
          if LResult <> -1 then
          begin
            raise EParserMultiResultError.Create('Return value not determinable');
          end;
          LResult := Pred(LIndex);
        end;
      end;
      Result := TParserValue.Create(LResult);
    end, True));
  FDefaultPackage.Dictionary.Add(TParserReferenceFunction.Create('Length', [TParserParam.Create('Value', LTypes.Struct)],
    function (Params: TArray<TParserValue>): TParserValue
    begin
      Result := TParserValue.Create(Params[0].Count);
    end));
  FDefaultPackage.Dictionary.Add(TParserReferenceFunction.Create('Sort', [TParserParam.Create('Value', LTypes.&Array)],
    function (Params: TArray<TParserValue>): TParserValue
    begin
      Result := TParserValue.Create(Params[0].AsArray);
      TArray.Sort<TParserValue>(Result.AsArray, TComparer<TParserValue>.Construct(
        function (const Left, Right: TParserValue): Integer
        begin
          Result := TParserValue.Compare(Left, Right);
        end));
    end));
  FDefaultPackage.Dictionary.Add(TParserReferenceFunction.Create('Keys', [TParserParam.Create('Value', LTypes.Struct)],
    function (Params: TArray<TParserValue>): TParserValue
    begin
      Result := TParserValue.Create(Params[0].Keys);
    end));
//  FDefaultPackage.Dictionary.Add(TParserReferenceFunction.Create('Values', [TParserParam.Create('Value', LTypes.Record), TParserParam.Create('From'), TParserParam.Create('To')],
//    function (Params: TArray<TParserValue>): TParserValue
//    begin
//      Result := Params[0].Copy(Params[1], Params[2]);
//    end));
  FDefaultPackage.Dictionary.Add(TParserReferenceFunction.Create('Copy', [TParserParam.Create('Value', LTypes.Struct), TParserParam.Create('From'), TParserParam.Create('To')],
    function (Params: TArray<TParserValue>): TParserValue
    begin
      Result := Params[0].Copy(Params[1], Params[2]);
    end));
  FDefaultPackage.Dictionary.Add(TParserReferenceFunction.Create('Map', [TParserParam.Create('Values', LTypes.Struct), TParserParam.Create('MapFunction', LTypes.Ref)],
    function (Params: TArray<TParserValue>): TParserValue
    var
      LValues: TArray<TParserValue>;
    begin

    end));
  FDefaultPackage.Dictionary.Add(TParserReferenceFunction.Create('Filter', [TParserParam.Create('Values', LTypes.Struct), TParserParam.Create('FilterRef', LTypes.Ref)],
    function (Params: TArray<TParserValue>): TParserValue
    begin

    end));
  FDefaultPackage.Dictionary.Add(TParserReferenceFunction.Create('Reduce', [TParserParam.Create('Values', LTypes.Struct), TParserParam.Create('ReduceFunction', LTypes.Ref)],
    function (Params: TArray<TParserValue>): TParserValue

      procedure ReduceArray;
      var
        LMember: TParserValue;
      begin
        for LMember in Params[0].AsArray do
        begin

        end;
      end;

      procedure ReduceRecord;
      var
        LField: TPair<String, TParserValue>;
      begin
        for LField in Params[0].AsRecord do
        begin

        end;
      end;

    begin
      case Params[0].Kind of
        vkString, vkArray:
          begin
            ReduceArray;
          end;
        vkRecord:
          begin
            ReduceRecord;
          end;
      end;
    end));
  FDefaultPackage.Dictionary.Add(TParserReferenceFunction.Create('Join', [TParserParam.Create('Values', LTypes.&Array), TParserParam.Create('Separator', TParserValue.Empty[vkString], LTypes.&String)],
    function (Params: TArray<TParserValue>): TParserValue
    var
      LIndex: Integer;
      LValues: TArray<TParserValue>;
    begin
      LValues := Params[0].AsArray;
      if Length(LValues) = 0 then
      begin
        Result := TParserValue.Empty[vkString];
      end else
      begin
        Result := LValues[0];
        for LIndex := 1 to High(LValues) do
        begin
          Result := opAdd.Invoke(Result, Params[1]);
          Result := opAdd.Invoke(Result, LValues[LIndex]);
        end;
      end;
    end));
  FDefaultPackage.Dictionary.Add(TParserReferenceFunction.Create('Format', [TParserParam.Create('Text', LTypes.&String), TParserParam.Create('Args', LTypes.&Array)],
    function (Params: TArray<TParserValue>): TParserValue
    begin
//      Result := TParserValue.Create(String.Format(Params[0].AsString, Params[1].AsArray.AsConsts));
    end));
  FDefaultPackage.Dictionary.Add(TParserReferenceFunction.Create('Replace', [TParserParam.Create('Text', LTypes.&String), TParserParam.Create('Old', LTypes.&String), TParserParam.Create('New', LTypes.&String)],
    function (Params: TArray<TParserValue>): TParserValue
    begin
      Result := TParserValue.Create(Params[0].AsString.Replace(Params[1].AsString, Params[2].AsString));
    end));
  FDefaultPackage.Dictionary.Add(TParserReferenceFunction.Create('Flatten', [TParserParam.Create('Value', LTypes.Struct)],
    function (Params: TArray<TParserValue>): TParserValue

      function Flatten(AValue: TParserValue): TParserValue;

        function FlattenArray(const AArray: TArray<TParserValue>): TArray<TParserValue>;
        begin

        end;

        function FlattenRecord(const ARecordy: TArray<TPair<String, TParserValue>>): TArray<TPair<String, TParserValue>>;
        begin

        end;

      begin
        case AValue.Kind of
          vkArray:
            begin
              Result := TParserValue.Create(FlattenArray(AValue.AsArray));
            end;
          vkRecord:
            begin
              Result := TParserValue.Create(FlattenRecord(AValue.AsRecord));
            end;
          else
            begin
              Result := AValue;
            end;
        end;
      end;

    begin
      Result := Flatten(Params[0]);
    end));
  FDefaultPackage.Dictionary.Add(TParserReferenceFunction.Create('Char', [TParserParam.Create('X', LTypes.PosInt)],
    function (Params: TArray<TParserValue>): TParserValue
    begin
      Result := TParserValue.Create(Chr(Params[0].AsInteger));
    end));
  FDefaultPackage.Dictionary.Add(TParserReferenceFunction.Create('Hex', [TParserParam.Create('X', LTypes.Integer)],
    function (Params: TArray<TParserValue>): TParserValue
    begin
      Result := TParserValue.Create(Params[0].AsInteger.ToHexString);
    end));
  FDefaultPackage.Dictionary.Add(TParserReferenceFunction.Create('Get', [TParserParam.Create('Value', LTypes.Struct), TParserParam.Create('Member')],
    function (Params: TArray<TParserValue>): TParserValue
    begin
      Result := Params[0][Params[1]];
    end));
  FDefaultPackage.Dictionary.Add(TParserReferenceFunction.Create('Has', [TParserParam.Create('Value', LTypes.Struct), TParserParam.Create('Member')],
    function (Params: TArray<TParserValue>): TParserValue
    begin
      Result := TParserValue.Create(Params[0].HasMember(Params[1]));
    end));
  FDefaultPackage.Dictionary.Add(TParserReferenceFunction.Create('Contains', [TParserParam.Create('Value', LTypes.Struct), TParserParam.Create('Member')],
    function (Params: TArray<TParserValue>): TParserValue
    var
      LKeyDummy: TParserValue;
    begin
      Result := TParserValue.Create(Params[0].Find(Params[1], LKeyDummy));
    end));
  FDefaultPackage.Dictionary.Add(TParserReferenceFunction.Create('Assigned', [TParserParam.Create('Value')],
    function (Params: TArray<TParserValue>): TParserValue
    begin
      Result := TParserValue.Create(not Params[0].IsEmpty);
    end));
  FDefaultPackage.Dictionary.Add(TParserReferenceFunction.Create('IfDecl', [TParserParam.Create('Identifier', LTypes.&String), TParserParam.Create('ThenValue'), TParserParam.Create('ElseValue')],
    function (Params: TArray<TParserValue>): TParserValue
    begin
      Result := TParserValue.Create(IfThen(FDefaultPackage.Dictionary.Contains(Params[0].AsString), Params[1].AsDouble, Params[2].AsDouble));
    end));
  FDefaultPackage.Dictionary.Add(TParserReferenceFunction.Create('IfEqual', [TParserParam.Create('First'), TParserParam.Create('Second'), TParserParam.Create('ThenValue'), TParserParam.Create('ElseValue')],
    function (Params: TArray<TParserValue>): TParserValue
    begin
      Result := TParserValue.Create(IfThen(Params[0].Equals(Params[1]), Params[2].AsDouble, Params[3].AsDouble));
    end));
  FDefaultPackage.Dictionary.Add(TParserReferenceFunction.Create('IfEmpty', [TParserParam.Create('Value', LTypes.Number), TParserParam.Create('ThenValue'), TParserParam.Create('ElseValue')],
    function (Params: TArray<TParserValue>): TParserValue
    begin
      Result := TParserValue.Create(IfThen(Params[0].IsEmpty, Params[1].AsDouble, Params[2].AsDouble));
    end));
  FDefaultPackage.Dictionary.Add(TParserReferenceFunction.Create('IfPos', [TParserParam.Create('Value', LTypes.Number), TParserParam.Create('ThenValue'), TParserParam.Create('ElseValue')],
    function (Params: TArray<TParserValue>): TParserValue
    begin
      Result := TParserValue.Create(IfThen(CompareValue(Params[0].AsDouble, 0) = GreaterThanValue, Params[1].AsDouble, Params[2].AsDouble));
    end));
  FDefaultPackage.Dictionary.Add(TParserReferenceFunction.Create('IfNeg', [TParserParam.Create('Value', LTypes.Number), TParserParam.Create('ThenValue'), TParserParam.Create('ElseValue')],
    function (Params: TArray<TParserValue>): TParserValue
    begin
      Result := TParserValue.Create(IfThen(CompareValue(Params[0].AsDouble, 0) = LessThanValue, Params[1].AsDouble, Params[2].AsDouble));
    end));
  FDefaultPackage.Dictionary.Add(TParserReferenceFunction.Create('IfNaN', [TParserParam.Create('Value', LTypes.Number), TParserParam.Create('ThenValue'), TParserParam.Create('ElseValue')],
    function (Params: TArray<TParserValue>): TParserValue
    begin
      Result := TParserValue.Create(IfThen(Params[0].IsNaN, Params[1].AsDouble, Params[2].AsDouble));
    end));
  FDefaultPackage.Dictionary.Add(TParserReferenceFunction.Create('IfInf', [TParserParam.Create('Value', LTypes.Number), TParserParam.Create('ThenValue'), TParserParam.Create('ElseValue')],
    function (Params: TArray<TParserValue>): TParserValue
    begin
      Result := TParserValue.Create(IfThen(IsInfinite(Params[0].AsDouble), Params[1].AsDouble, Params[2].AsDouble));
    end));
  FDefaultPackage.Dictionary.Add(TParserReferenceFunction.Create('Nearest', [TParserParam.Create('X', LTypes.Number), TParserParam.Create('Value', LTypes.Number)],
    function (Params: TArray<TParserValue>): TParserValue
    var
      LIndex: Integer;
    begin
      Result := Params[1];
      for LIndex := 2 to High(Params) do
      begin
        case CompareValue(Abs(Params[LIndex].AsDouble - Params[0].AsDouble), Abs(Result.AsDouble - Params[0].AsDouble)) of
          LessThanValue:
            begin
              Result := Params[LIndex];
            end;
          EqualsValue:
            begin
              raise EParserMultiResultError.Create('Return value not determinable');
            end;
        end;
      end;
    end, True));
  FDefaultPackage.Dictionary.Add(TParserReferenceFunction.Create('Furthest', [TParserParam.Create('X', LTypes.Number), TParserParam.Create('Value', LTypes.Number)],
    function (Params: TArray<TParserValue>): TParserValue
    var
      LIndex: Integer;
    begin
      Result := Params[1];
      for LIndex := 2 to High(Params) do
      begin
        case CompareValue(Abs(Params[LIndex].AsDouble - Params[0].AsDouble), Abs(Result.AsDouble - Params[0].AsDouble)) of
          EqualsValue:
            begin
              raise EParserMultiResultError.Create('Return value not determinable');
            end;
          GreaterThanValue:
            begin
              Result := Params[LIndex];
            end;
        end;
      end;
    end, True));
  FDefaultPackage.Dictionary.Add(TParserReferenceFunction.Create('Asc', [TParserParam.Create('First'), TParserParam.Create('Next')],
    function (Params: TArray<TParserValue>): TParserValue
    var
      LIndex: Integer;
    begin
      for LIndex := Succ(Low(Params)) to High(Params) do
      begin
        if TParserValue.Compare(Params[Pred(LIndex)], Params[LIndex]) <> LessThanValue then
        begin
          Exit(TParserValue.Create(False));
        end;
      end;
      Result := LConstants.True.Value[[]];
    end, True));
  FDefaultPackage.Dictionary.Add(TParserReferenceFunction.Create('Desc', [TParserParam.Create('First'), TParserParam.Create('Next')],
    function (Params: TArray<TParserValue>): TParserValue
    var
      LIndex: Integer;
    begin
      for LIndex := Succ(Low(Params)) to High(Params) do
      begin
        if TParserValue.Compare(Params[Pred(LIndex)], Params[LIndex]) <> GreaterThanValue then
        begin
          Exit(TParserValue.Create(False));
        end;
      end;
      Result := LConstants.True.Value[[]];
    end, True));
  FDefaultPackage.Dictionary.Add(TParserReferenceFunction.Create('Equal', [TParserParam.Create('First'), TParserParam.Create('Next')],
    function (Params: TArray<TParserValue>): TParserValue
    var
      LIndex: Integer;
    begin
      for LIndex := Succ(Low(Params)) to High(Params) do
      begin
        if not Params[Pred(LIndex)].Equals(Params[LIndex]) then
        begin
          Exit(TParserValue.Create(False));
        end;
      end;
      Result := LConstants.True.Value[[]];
    end, True));
  FDefaultPackage.Dictionary.Add(TParserReferenceFunction.Create('Error', [TParserParam.Create('Message', TParserValue.Empty[vkString])],
    function (Params: TArray<TParserValue>): TParserValue
    begin
      if Params[0].IsEmpty then
      begin
        raise EParserUserError.Create('Error');
      end;
      case Params[0].Kind of
        vkString:
          begin
            raise EParserUserError.CreateFmt('Error: %s', [Params[0].AsString]);
          end;
        vkRecord:
          begin
            LTypes.Exception.AssertValue(Params[0]);
            raise EParserUserError.CreateFmt('Error: %s', [Params[0][TParserValue.Create('Message')].AsString]);
          end;
      end;
      Result := TParserValue.Empty[vkDouble];
    end));
  // Testing only
  FDefaultPackage.Dictionary.Add(TParserConstant.Create('test', TParserValue.Create(FDefaultPackage.Dictionary['Sin'] as IParserValueRefTarget)));
end;

destructor TParserPackage.Destroy;
begin
  Dictionary.Free;
  inherited;
end;

function TParserPackage.GetMaxArgCount(const AName: String): Integer;
begin
  Result := Dictionary[AName].MaxArgCount;
end;

function TParserPackage.GetMinArgCount(const AName: String): Integer;
begin
  Result := Dictionary[AName].MinArgCount;
end;

function TParserPackage.GetPackageLinks(const AName: String): TParserPackage;
begin

end;

function TParserPackage.GetRefTargets(
  const AName: String): IParserValueRefTarget;
begin

end;

function TParserPackage.GetValues(const AName: String; const AArgs: TArray<TParserValue>): TParserValue;
begin
  Result := Dictionary[AName].Value[AArgs];
end;

function TParserPackage.HasPackageLink(const AName: String): Boolean;
begin
  Result := FPackageLinks.ContainsKey(AName);
end;

procedure TParserPackage.RemovePackageLink(const AName: String);
begin
  FPackageLinks.Remove(AName);
end;

class destructor TParserPackage.Destroy;
begin
  DefaultPackage.Free;
end;

{ TParserCustomPackage }

constructor TParserCustomPackage.Create(const AFileName: String);
begin
  inherited Create(String.Empty, True);
  FFileName := ChangeFileExt(AFileName, FileExtension);
end;

class function TParserCustomPackage.FileExtension: String;
begin
  Result := '.mfl';
end;

procedure TParserCustomPackage.LoadFromFile;
begin

end;

{ TParserMemoryPackage }

constructor TParserMemoryPackage.Create(const AName: String = 'Memory'; const AExplicit: Boolean = False; const ASize: Integer = 0);
var
  LConstants: record
    &Nil: TParserConstant;
  end;
  LTypes: record
    Address: TParserRangeIntegerType;
    Size: TParserRangeIntegerType;
  end;
begin
  inherited Create(AName, AExplicit);
  LConstants.&Nil := TParserConstant.Create('Nil', TParserValue.Empty[vkDouble]);
  LTypes.Address := TParserRangeIntegerType.Create('Address', LConstants.&Nil.Value[[]], TParserValue.Inf);
  LTypes.Size := TParserRangeIntegerType.Create('Size', LConstants.&Nil.Value[[]], TParserValue.Inf);
  FMemory := TDictionary<Integer, TArray<TParserValue>>.Create(ASize);
  Dictionary.Add(LConstants.&Nil);
  Dictionary.Add(LTypes.Address);
  Dictionary.Add(LTypes.Size);
  Dictionary.Add(TParserReferenceFunction.Create('Clear', [],
    function (Params: TArray<TParserValue>): TParserValue
    begin
      Result := TParserValue.Create(Size);
      FMemory.Clear;
    end));
  Dictionary.Add(TParserReferenceFunction.Create('Alloc', [TParserParam.Create('Size', TParserValue.Create(1), LTypes.Size)],
    function (Params: TArray<TParserValue>): TParserValue
    var
      LValues: TArray<TParserValue>;
    begin
      SetLength(LValues, Params[0].AsInteger);
      Result := TParserValue.Create(NextAddress);
      FMemory.Add(Result.AsInteger, LValues);
    end));
  Dictionary.Add(TParserReferenceFunction.Create('Realloc', [TParserParam.Create('Address', LTypes.Address), TParserParam.Create('Size', LTypes.Size)],
    function (Params: TArray<TParserValue>): TParserValue
    var
      LValues: TArray<TParserValue>;
    begin
      if CompareValue(Params[0].AsDouble, Offset) = LessThanValue then
      begin
        raise EParserMemoryPackageAddressError.Create('Invalid memory address');
      end;
      ValidateAddress(Params[0].AsInteger);
      SetLength(LValues, Params[1].AsInteger);
      Result := TParserValue.Empty[vkDouble];
      FMemory.AddOrSetValue(Params[0].AsInteger, LValues);
    end));
  Dictionary.Add(TParserReferenceFunction.Create('Free', [TParserParam.Create('Address', LTypes.Address)],
    function (Params: TArray<TParserValue>): TParserValue
    begin
      ValidateAddress(Params[0].AsInteger);
      Result := TParserValue.Create(Length(Memory[Params[0].AsInteger]));
      FMemory.Remove(Params[0].AsInteger);
    end));
  Dictionary.Add(TParserReferenceFunction.Create('Read', [TParserParam.Create('Address', LTypes.Address), TParserParam.Create('Index', LTypes.Address)],
    function (Params: TArray<TParserValue>): TParserValue
    begin
      ValidateAddressAndIndex(Params[0].AsInteger, Params[1].AsInteger);
      Result := Memory[Params[0].AsInteger][Params[1].AsInteger];
    end));
  Dictionary.Add(TParserReferenceFunction.Create('Write', [TParserParam.Create('Address', LTypes.Address), TParserParam.Create('Index', LTypes.Address), TParserParam.Create('Value')],
    function (Params: TArray<TParserValue>): TParserValue
    var
      LIndex: Integer;
    begin
      for LIndex := 2 to High(Params) do
      begin
        ValidateAddressAndIndex(Params[0].AsInteger, Params[1].AsInteger + LIndex - 2);
        Memory[Params[0].AsInteger][Params[1].AsInteger + LIndex - 2] := Params[LIndex];
      end;
      Result := TParserValue.Empty[vkDouble];
    end, True));
  Dictionary.Add(TParserReferenceFunction.Create('GetSize', [TParserParam.Create('Address', LTypes.Address)],
    function (Params: TArray<TParserValue>): TParserValue
    begin
      ValidateAddress(Params[0].AsInteger);
      Result := TParserValue.Create(Length(Memory[Params[0].AsInteger]));
    end));
  Dictionary.Add(TParserReferenceFunction.Create('SetSize', [TParserParam.Create('Address', LTypes.Address), TParserParam.Create('Size', LTypes.Size)],
    function (Params: TArray<TParserValue>): TParserValue
    var
      LValues: TArray<TParserValue>;
    begin
      ValidateAddress(Params[0].AsInteger);
      LValues := Memory[Params[0].AsInteger];
      SetLength(LValues, Params[1].AsInteger);
      Memory[Params[0].AsInteger] := LValues;
      Result := TParserValue.Empty[vkDouble];
    end));
  Dictionary.AddAlias('Null', 'Nil');
end;

destructor TParserMemoryPackage.Destroy;
begin
  FMemory.Free;
  inherited;
end;

function TParserMemoryPackage.GetMemory(const AAddress: Integer): TArray<TParserValue>;
begin
  ValidateAddress(AAddress);
  Result := FMemory[AAddress];
end;

function TParserMemoryPackage.GetSize: Integer;
begin
  Result := FMemory.Count;
end;

function TParserMemoryPackage.NextAddress: Integer;
begin
  Result := Offset;
  while FMemory.ContainsKey(Result) do
  begin
    Inc(Result);
  end;
end;

procedure TParserMemoryPackage.SetMemory(const AAddress: Integer; const AValue: TArray<TParserValue>);
begin
  ValidateAddress(AAddress);
  FMemory[AAddress] := Copy(AValue);
end;

procedure TParserMemoryPackage.ValidateAddress(const AAddress: Integer);
begin
  if not FMemory.ContainsKey(AAddress) then
  begin
    raise EParserMemoryPackageAddressError.CreateFmt('Memory at %d not allocated', [AAddress]);
  end;
end;

procedure TParserMemoryPackage.ValidateAddressAndIndex(const AAddress, AIndex: Integer);
begin
  ValidateAddress(AAddress);
  if not InRange(AIndex, Low(FMemory[AAddress]), High(FMemory[AAddress])) then
  begin
    raise EParserMemoryPackageIndexError.CreateFmt('Index %d at %d out of bounds', [AIndex, AAddress]);
  end;
end;

end.
