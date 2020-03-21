unit Parser.Exception;

interface

uses
  System.SysUtils;

type
  EParserError = class abstract(Exception);

    EParserUnknownError = class(EParserError);

    EParserUnknownPackageError = class(EParserError);

    EParserTokenUnexpectedError = class(EParserError);

    EParserParenthesisError = class(EParserError);

    EParserCommaError = class(EParserError);

    EParserOperatorError = class(EParserError);

    EParserUserError = class(EParserError);

    EParserLexerError = class abstract(EParserError);

      EParserLexerCharacterError = class(EParserLexerError);

      EParserLexerTokenLengthError = class(EParserLexerError);

      EParserLexerTokenIncomplete = class(EParserLexerError);

    EParserPackageError = class abstract(EParserError);

      EParserPackageNameError = class(EParserPackageError);

      EParserMemoryPackageError = class abstract(EParserPackageError);

        EParserMemoryPackageAddressError = class(EParserMemoryPackageError);

        EParserMemoryPackageIndexError = class(EParserMemoryPackageError);

    EParserDictionaryError = class abstract(EParserError);

      EParserDictionaryUnknownError = class(EParserDictionaryError);

      EParserDictionaryDuplicateError = class(EParserDictionaryError);

      EParserDictionaryAliasError = class(EParserDictionaryError);

    EParserObjectError = class abstract(EParserError);

      EParserObjectNameError = class(EParserObjectError);

      EParserObjectArgCountError = class(EParserObjectError);

      EParserFunctionParamNameError = class(EParserObjectError);

      EParserFunctionParamDuplicateError = class(EParserObjectError);

      EParserFunctionParamValueError = class(EParserObjectError);

    EParserTreeError = class abstract(EParserError);

      EParserTreeUnknownError = class(EParserTreeError);

    EParserExportError = class abstract(EParserError);

      EParserUnsupportedError = class(EParserError);

implementation

end.
