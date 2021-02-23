object fmOptions: TfmOptions
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Options'
  ClientHeight = 299
  ClientWidth = 377
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  PixelsPerInch = 96
  TextHeight = 13
  object pcOptions: TPageControl
    AlignWithMargins = True
    Left = 3
    Top = 3
    Width = 371
    Height = 262
    ActivePage = tsCompiler
    Align = alClient
    TabOrder = 0
    ExplicitLeft = 0
    ExplicitTop = 0
    ExplicitWidth = 367
    ExplicitHeight = 249
    object tsCompiler: TTabSheet
      Caption = 'Compiler'
      ExplicitWidth = 359
      ExplicitHeight = 261
      object pnCompilerOptions: TGridPanel
        Left = 0
        Top = 0
        Width = 363
        Height = 234
        Align = alClient
        BevelOuter = bvNone
        ColumnCollection = <
          item
            Value = 49.637777305781310000
          end
          item
            Value = 50.413223140495870000
          end>
        ControlCollection = <
          item
            Column = 1
            Control = cbOptionInitialType
            Row = 2
          end
          item
            Column = 0
            ColumnSpan = 2
            Control = cbOptionOptimization
            Row = 1
          end
          item
            Column = 0
            ColumnSpan = 2
            Control = cbOptionWarnings
            Row = 0
          end
          item
            Column = 0
            Control = laOptionInitialType
            Row = 2
          end>
        RowCollection = <
          item
            SizeStyle = ssAuto
            Value = 50.000000000000000000
          end
          item
            SizeStyle = ssAuto
            Value = 100.000000000000000000
          end
          item
            SizeStyle = ssAuto
            Value = 100.000000000000000000
          end
          item
            SizeStyle = ssAuto
            Value = 100.000000000000000000
          end>
        TabOrder = 0
        ExplicitLeft = 24
        ExplicitTop = 120
        ExplicitWidth = 185
        ExplicitHeight = 41
        object cbOptionInitialType: TComboBox
          AlignWithMargins = True
          Left = 183
          Top = 49
          Width = 177
          Height = 21
          Align = alClient
          Style = csDropDownList
          TabOrder = 0
          Items.Strings = (
            'Reference'
            'Number (Default)'
            'String'
            'Array'
            'Record')
          ExplicitLeft = 28
          ExplicitTop = 132
          ExplicitWidth = 145
        end
        object cbOptionOptimization: TCheckBox
          AlignWithMargins = True
          Left = 3
          Top = 26
          Width = 357
          Height = 17
          Align = alClient
          Caption = 'Optimize code'
          TabOrder = 1
          ExplicitWidth = 341
        end
        object cbOptionWarnings: TCheckBox
          AlignWithMargins = True
          Left = 3
          Top = 3
          Width = 357
          Height = 17
          Align = alClient
          Caption = 'Show warnings'
          TabOrder = 2
          ExplicitLeft = 0
          ExplicitTop = 4
          ExplicitWidth = 341
        end
        object laOptionInitialType: TLabel
          AlignWithMargins = True
          Left = 3
          Top = 49
          Width = 174
          Height = 21
          Align = alClient
          Caption = 'Initial variable type:'
          ExplicitTop = 53
          ExplicitWidth = 190
          ExplicitHeight = 13
        end
      end
    end
    object Packages: TTabSheet
      Caption = 'Packages'
      ImageIndex = 1
      ExplicitWidth = 359
      ExplicitHeight = 261
      object laStandardPackages: TLabel
        AlignWithMargins = True
        Left = 3
        Top = 3
        Width = 357
        Height = 13
        Align = alTop
        Caption = 'Select implicit standard packages:'
        ExplicitWidth = 161
      end
      object lbStandardPackages: TCheckListBox
        AlignWithMargins = True
        Left = 3
        Top = 22
        Width = 357
        Height = 209
        Align = alClient
        ItemHeight = 13
        TabOrder = 0
        ExplicitLeft = 104
        ExplicitTop = 48
        ExplicitWidth = 121
        ExplicitHeight = 97
      end
    end
  end
  object pnButtons: TFlowPanel
    Left = 0
    Top = 268
    Width = 377
    Height = 31
    Align = alBottom
    AutoSize = True
    BevelOuter = bvNone
    FlowStyle = fsRightLeftBottomTop
    TabOrder = 1
    ExplicitTop = 264
    ExplicitWidth = 367
    object btOK: TButton
      AlignWithMargins = True
      Left = 289
      Top = 3
      Width = 75
      Height = 25
      Caption = 'OK'
      Default = True
      ModalResult = 1
      TabOrder = 0
    end
  end
end
