unit Parser.Exception;

interface

uses
  System.SysUtils;

type
  EParserError = class abstract(Exception);

    EParserCommandError = class(EParserError);

    EParserUnknownPackageError = class(EParserError);

    EParserDuplicatePackageError = class(EParserError);

    EParserNoTypeError = class(EParserError);

    EParserTokenUnexpectedError = class(EParserError);

    EParserParenthesisError = class(EParserError);

    EParserCommaError = class(EParserError);

    EParserEmptySectionError = class(EParserError);

    EParserAssertionError = class(EParserError);

    EParserOperatorError = class(EParserError);

    EParserOptionError = class(EParserError);

    EParserMultiResultError = class(EParserError);

    EParserAddressError = class abstract(EParserError);

    EParserUserError = class(EParserError);

    EParserLexerError = class abstract(EParserError);

      EParserLexerCharacterError = class(EParserLexerError);

      EParserLexerNumberFormatError = class(EParserLexerError);

      EParserLexerTokenIncomplete = class(EParserLexerError);

    EParserPackageError = class abstract(EParserError);

      EParserPackageNameError = class(EParserPackageError);

      EParserConsoleError = class(EParserPackageError);

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

      EParserFunctionUnknownError = class(EParserObjectError);

      EParserFunctionParamNameError = class(EParserObjectError);

      EParserFunctionParamDuplicateError = class(EParserObjectError);

      EParserFunctionParamValueError = class(EParserObjectError);

      EParserFunctionParamDefaultError = class(EParserObjectError);

      EParserTypeNoConstructor = class(EParserObjectError);

      EParserTypeNoDefault = class(EParserObjectError);

      EParserTypeCompatibilityError = class(EParserObjectError);

      EParserTypeUnknownError = class(EParserObjectError);

    EParserTreeError = class abstract(EParserError);

      EParserTreeUnknownError = class(EParserTreeError);

    EParserExportError = class abstract(EParserError);

      EParserExportUnsupportedError = class(EParserError);

    EParserValueError = class abstract(EParserError);

      EParserValueKindError = class(EParserValueError);

      EParserValueMemberError = class(EParserValueError);

      EParserValueComparisonError = class(EParserValueError);

      EParserValueNegationError = class(EParserValueError);

      EParserValueAbsolutionError = class(EParserValueError);

      EParserValueRefError = class(EParserValueError);

implementation

end.
