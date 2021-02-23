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

    EParserResultDeterminationError = class(EParserError);

    EParserAddressError = class abstract(EParserError);

    EParserUserError = class(EParserError);

    EParserTypeConstructionError = class(EParserError);

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

      EParserTypeNoConstructorError = class(EParserObjectError);

      EParserTypeNoDefaultError = class(EParserObjectError);

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
