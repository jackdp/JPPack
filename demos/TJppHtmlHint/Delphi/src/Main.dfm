object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 494
  ClientWidth = 772
  Color = 13882323
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    772
    494)
  PixelsPerInch = 96
  TextHeight = 15
  object meHint: TJppMemo
    Left = 199
    Top = 28
    Width = 556
    Height = 430
    Anchors = [akLeft, akTop, akRight, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Consolas'
    Font.Style = []
    Lines.Strings = (
      'Test panel <c:mclLightBlue>HINT</c>')
    ParentFont = False
    ScrollBars = ssVertical
    TabOrder = 0
    Text = 'Test panel <c:mclLightBlue>HINT</c>'
    Appearance.NormalBgColor = 15531007
    Appearance.FocusedBgColor = 15531007
    Appearance.HotBgColor = 15531007
    BoundLabel.Width = 79
    BoundLabel.Height = 15
    BoundLabel.Caption = 'Test panel hint:'
    BoundLabel.Font.Charset = DEFAULT_CHARSET
    BoundLabel.Font.Color = clWindowText
    BoundLabel.Font.Height = -12
    BoundLabel.Font.Name = 'Segoe UI'
    BoundLabel.Font.Style = []
    BoundLabel.ParentFont = False
    BoundLabel.FocusControl = meHint
    BoundLabelPosition = lpAbove
    BoundLabelSpacing = 3
    AnchoredControls.Bottom.Control = btnUpdateHint
    AnchoredControls.Bottom.Spacing = 5
    AnchoredControls.Bottom.Position = bcpBottomRight
    AnchoredControls.Left.Control = btnLoad
    AnchoredControls.Left.Spacing = 5
    AnchoredControls.Left.ControlPositionMode = cpmCustom
    AnchoredControls.Left.CustomPosition = cpBottomLeft
  end
  object pnTest: TJppSimplePanel
    Left = 20
    Top = 36
    Width = 157
    Height = 46
    Caption = 'Test panel'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -15
    Font.Name = 'Segoe UI'
    Font.Style = [fsBold]
    ParentFont = False
    ParentShowHint = False
    ShowHint = True
    TabOrder = 1
    Appearance.BackgroundColor = 12578815
    Appearance.BorderColor = 45548
    Appearance.BorderStyle = psDash
  end
  object btnUpdateHint: TButton
    Left = 592
    Top = 463
    Width = 163
    Height = 25
    Action = actUpdateHint
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 2
  end
  object btnLoad: TButton
    Left = 199
    Top = 463
    Width = 137
    Height = 25
    Action = actLoadFile
    TabOrder = 3
  end
  object btnSave: TButton
    Left = 341
    Top = 463
    Width = 172
    Height = 25
    Action = actSaveFile
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 4
  end
  object btnSetLightMode: TButton
    Left = 15
    Top = 105
    Width = 81
    Height = 25
    Action = actSetLightMode
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 5
  end
  object btnSetDarkMode: TButton
    Left = 101
    Top = 105
    Width = 81
    Height = 25
    Action = actSetDarkMode
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 6
  end
  object ActionList1: TActionList
    Left = 168
    Top = 208
    object actUpdateHint: TAction
      Caption = 'Update and show hint (F5)'
      ShortCut = 116
      OnExecute = actUpdateHintExecute
    end
    object actEsc: TAction
      Caption = 'actEsc'
      ShortCut = 27
      OnExecute = actEscExecute
    end
    object actSetLightMode: TAction
      Caption = 'Light mode'
      OnExecute = actSetLightModeExecute
    end
    object actSetDarkMode: TAction
      Caption = 'Dark mode'
      OnExecute = actSetDarkModeExecute
    end
    object actLoadFile: TAction
      Caption = 'Load "hint.txt" file'
      OnExecute = actLoadFileExecute
    end
    object actSaveFile: TAction
      Caption = 'Save "hint.txt" file (Ctrl+S)'
      ShortCut = 16467
      OnExecute = actSaveFileExecute
    end
  end
  object tmMoveCursor: TJppTimer
    Interval = 400
    OnTimer = tmMoveCursorTimer
    Counter = 0
    RepeatCountLimit = 1
    Left = 400
    Top = 200
  end
  object htHint: TJppHtmlHint
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -13
    Font.Name = 'Segoe UI'
    Font.Style = []
    Appearance.LightModeColors.mclDarkGreen = 5807415
    Appearance.DarkModeColors.Background = 3881787
    Appearance.DarkModeColors.Text = 13882323
    Appearance.DarkModeColors.Border = 3223857
    Appearance.DarkModeColors.Arrow = 15658734
    Appearance.DarkModeColors.ArrowBackground = 1644825
    Appearance.DarkModeColors.ArrowRectBorder = clBlack
    Appearance.DarkModeColors.mclDarkGray = 7368816
    Appearance.DarkModeColors.mclLightGray = 12171705
    Appearance.DarkModeColors.mclDarkBlue = 15890688
    Appearance.DarkModeColors.mclLightBlue = 16756318
    Appearance.DarkModeColors.mclDarkGreen = 3779635
    Appearance.DarkModeColors.mclLightGreen = 5762651
    Appearance.DarkModeColors.mclDarkRed = 2368767
    Appearance.DarkModeColors.mclLightRed = 7303167
    Appearance.DarkModeColors.mclDarkCyan = 13553152
    Appearance.DarkModeColors.mclLightCyan = 16777041
    Appearance.DarkModeColors.mclDarkMagenta = 16719103
    Appearance.DarkModeColors.mclLightMagenta = 16745727
    Appearance.DarkModeColors.mclDarkYellow = 48857
    Appearance.DarkModeColors.mclLightYellow = 7335423
    Left = 112
    Top = 304
  end
end
