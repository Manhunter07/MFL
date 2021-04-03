object fmMain: TfmMain
  Left = 0
  Top = 0
  Caption = 'MFL Editor'
  ClientHeight = 400
  ClientWidth = 800
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object spLog: TSplitter
    Left = 0
    Top = 258
    Width = 800
    Height = 3
    Cursor = crVSplit
    Align = alBottom
    Visible = False
    ExplicitTop = 25
    ExplicitWidth = 183
  end
  object edCode: TSynEdit
    AlignWithMargins = True
    Left = 3
    Top = 28
    Width = 794
    Height = 227
    Align = alClient
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Courier New'
    Font.Style = []
    Font.Quality = fqClearTypeNatural
    PopupMenu = PopupActionBar
    TabOrder = 0
    UseCodeFolding = False
    Gutter.Font.Charset = DEFAULT_CHARSET
    Gutter.Font.Color = clWindowText
    Gutter.Font.Height = -11
    Gutter.Font.Name = 'Consolas'
    Gutter.Font.Style = []
    OnChange = edCodeChange
  end
  object ActionMainMenuBar: TActionMainMenuBar
    Left = 0
    Top = 0
    Width = 800
    Height = 25
    UseSystemFont = False
    ActionManager = ActionManager
    Color = clMenuBar
    ColorMap.DisabledFontColor = 7171437
    ColorMap.HighlightColor = clWhite
    ColorMap.BtnSelectedFont = clBlack
    ColorMap.UnusedColor = clWhite
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    Spacing = 0
  end
  object StatusBar: TStatusBar
    Left = 0
    Top = 381
    Width = 800
    Height = 19
    Panels = <>
  end
  object pnLog: TPanel
    Left = 0
    Top = 261
    Width = 800
    Height = 120
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 3
    Visible = False
    DesignSize = (
      800
      120)
    object tcLog: TTabControl
      AlignWithMargins = True
      Left = 3
      Top = 3
      Width = 794
      Height = 114
      Align = alClient
      TabOrder = 0
      Tabs.Strings = (
        'Messages'
        'Output')
      TabIndex = 0
      OnChange = tcLogChange
      OnChanging = tcLogChanging
      object lbLog: TListBox
        AlignWithMargins = True
        Left = 7
        Top = 27
        Width = 780
        Height = 80
        Align = alClient
        ItemHeight = 13
        TabOrder = 0
      end
    end
    object pnLogButtons: TFlowPanel
      Left = 673
      Top = 0
      Width = 117
      Height = 17
      Anchors = [akTop, akRight]
      BevelOuter = bvNone
      FlowStyle = fsRightLeftTopBottom
      TabOrder = 1
      object btLogClose: TButton
        Left = 99
        Top = 0
        Width = 18
        Height = 18
        Caption = #215
        TabOrder = 0
        OnClick = btLogCloseClick
      end
    end
  end
  object ActionManager: TActionManager
    ActionBars = <
      item
        Items = <
          item
            Items = <
              item
                Action = acFileNew
                Caption = '&New'
                ShortCut = 16462
              end
              item
                Caption = '-'
              end
              item
                Action = acFileOpen
                Caption = '&Open...'
                ShortCut = 16463
              end
              item
                Caption = '-'
              end
              item
                Action = acFileSave
                Caption = '&Save'
                ShortCut = 16467
              end
              item
                Action = acFileSaveAs
                Caption = 'S&ave as...'
              end>
            Caption = '&File'
          end
          item
            Items = <
              item
                Action = acEditUndo
                Caption = '&Undo'
                ShortCut = 16474
              end
              item
                Action = acEditRedo
                Caption = '&Redo'
                ShortCut = 16473
              end
              item
                Caption = '-'
              end
              item
                Action = acEditCopy
                Caption = '&Copy'
                ShortCut = 16451
              end
              item
                Action = acEditCut
                Caption = 'Cu&t'
                ShortCut = 16472
              end
              item
                Action = acEditPaste
                Caption = '&Paste'
                ShortCut = 16470
              end
              item
                Action = acEditDelete
                Caption = '&Delete'
                ShortCut = 46
              end
              item
                Caption = '-'
              end
              item
                Action = acSelectAll
                Caption = '&Select all'
                ShortCut = 16449
              end
              item
                Action = acSelectNone
                Caption = 'S&elect none'
                ShortCut = 27
              end>
            Caption = 'E&dit'
          end
          item
            Items = <
              item
                Action = acViewStatusBar
                Caption = '&Status bar'
              end>
            Caption = '&View'
          end
          item
            Items = <
              item
                Action = acProgramRun
                Caption = '&Run'
                ShortCut = 120
              end
              item
                Action = acProgramOptions
                Caption = '&Options...'
              end>
            Caption = '&Program'
          end
          item
            Items = <
              item
                Action = acInfoAbout
                Caption = '&About...'
              end>
            Caption = '&Info'
          end>
        ActionBar = ActionMainMenuBar
      end>
    Left = 720
    Top = 8
    StyleName = 'Platform Default'
    object acFileNew: TAction
      Category = 'File'
      Caption = 'New'
      ShortCut = 16462
      OnExecute = acFileNewExecute
    end
    object acFileOpen: TAction
      Category = 'File'
      Caption = 'Open...'
      ShortCut = 16463
      OnExecute = acFileOpenExecute
    end
    object acFileSave: TAction
      Category = 'File'
      Caption = 'Save'
      ShortCut = 16467
      OnExecute = acFileSaveExecute
    end
    object acFileSaveAs: TAction
      Category = 'File'
      Caption = 'Save as...'
      OnExecute = acFileSaveAsExecute
    end
    object acEditUndo: TAction
      Category = 'Edit'
      Caption = 'Undo'
      ShortCut = 16474
      OnExecute = acEditUndoExecute
      OnUpdate = acEditUndoUpdate
    end
    object acEditRedo: TAction
      Category = 'Edit'
      Caption = 'Redo'
      ShortCut = 16473
      OnExecute = acEditRedoExecute
      OnUpdate = acEditRedoUpdate
    end
    object acEditCopy: TAction
      Category = 'Edit'
      Caption = 'Copy'
      ShortCut = 16451
      OnExecute = acEditCopyExecute
      OnUpdate = acEditCopyUpdate
    end
    object acEditCut: TAction
      Category = 'Edit'
      Caption = 'Cut'
      ShortCut = 16472
      OnExecute = acEditCutExecute
      OnUpdate = acEditCutUpdate
    end
    object acEditPaste: TAction
      Category = 'Edit'
      Caption = 'Paste'
      ShortCut = 16470
      OnExecute = acEditPasteExecute
      OnUpdate = acEditPasteUpdate
    end
    object acEditDelete: TAction
      Category = 'Edit'
      Caption = 'Delete'
      ShortCut = 46
      OnExecute = acEditDeleteExecute
      OnUpdate = acEditDeleteUpdate
    end
    object acViewStatusBar: TAction
      Category = 'View'
      AutoCheck = True
      Caption = 'Status bar'
      Checked = True
      OnExecute = acViewStatusBarExecute
    end
    object acProgramRun: TAction
      Category = 'Program'
      Caption = 'Run'
      ShortCut = 120
      OnExecute = acProgramRunExecute
    end
    object acProgramOptions: TAction
      Category = 'Program'
      Caption = 'Options...'
      OnExecute = acProgramOptionsExecute
    end
    object acInfoAbout: TAction
      Category = 'Info'
      Caption = 'About...'
    end
    object acSelectAll: TAction
      Category = 'Edit'
      Caption = 'Select all'
      ShortCut = 16449
      OnExecute = acSelectAllExecute
      OnUpdate = acSelectAllUpdate
    end
    object acSelectNone: TAction
      Category = 'Edit'
      Caption = 'Select none'
      ShortCut = 27
      OnExecute = acSelectNoneExecute
      OnUpdate = acSelectNoneUpdate
    end
  end
  object FileOpenDialog: TFileOpenDialog
    DefaultExtension = '.mfl'
    FavoriteLinks = <>
    FileTypes = <
      item
        DisplayName = 'MFL files (*.mfl)'
        FileMask = '*.mfl'
      end
      item
        DisplayName = 'All files'
        FileMask = '*.*'
      end>
    Options = []
    Left = 720
    Top = 56
  end
  object FileSaveDialog: TFileSaveDialog
    DefaultExtension = '.mfl'
    FavoriteLinks = <>
    FileTypes = <
      item
        DisplayName = 'MFL Files (*.mfl)'
        FileMask = '*.mfl'
      end
      item
        DisplayName = 'All files'
        FileMask = '*.*'
      end>
    Options = []
    Left = 720
    Top = 104
  end
  object PopupActionBar: TPopupActionBar
    Left = 720
    Top = 152
    object miCopy: TMenuItem
      Action = acEditCopy
    end
    object miCut: TMenuItem
      Action = acEditCut
    end
    object miPaste: TMenuItem
      Action = acEditPaste
    end
    object miDelete: TMenuItem
      Action = acEditDelete
    end
  end
  object SynCompletionProposal: TSynCompletionProposal
    Options = [scoLimitToMatchedText, scoUseInsertList, scoUsePrettyText, scoUseBuiltInTimer, scoEndCharCompletion, scoCompleteWithTab, scoCompleteWithEnter]
    EndOfTokenChr = '()[]. '
    TriggerChars = '.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    TitleFont.Charset = DEFAULT_CHARSET
    TitleFont.Color = clBtnText
    TitleFont.Height = -11
    TitleFont.Name = 'MS Shell Dlg 2'
    TitleFont.Style = [fsBold]
    Columns = <
      item
        ColumnWidth = 50
        DefaultFontStyle = [fsBold]
      end
      item
        ColumnWidth = 50
      end
      item
        ColumnWidth = 100
      end>
    Resizeable = True
    OnExecute = SynCompletionProposalExecute
    ShortCut = 16416
    Editor = edCode
    Left = 720
    Top = 200
  end
end
