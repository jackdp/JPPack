object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'Form1'
  ClientHeight = 322
  ClientWidth = 597
  Color = 13948116
  DoubleBuffered = True
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 15
  object pb3: TJppProgressBar
    Left = 396
    Top = 120
    Width = 117
    Height = 17
    Caption = 'pb3'
    Max = 200
    Position = 50
    Appearance.Font.Charset = DEFAULT_CHARSET
    Appearance.Font.Color = clWindowText
    Appearance.Font.Height = -11
    Appearance.Font.Name = 'Tahoma'
    Appearance.Font.Style = []
    Appearance.DisabledFont.Charset = DEFAULT_CHARSET
    Appearance.DisabledFont.Color = clMedGray
    Appearance.DisabledFont.Height = -11
    Appearance.DisabledFont.Name = 'Tahoma'
    Appearance.DisabledFont.Style = []
    Appearance.TextPostfix = '%'
    BoundLabel.Width = 3
    BoundLabel.Height = 15
    BoundLabel.Font.Charset = DEFAULT_CHARSET
    BoundLabel.Font.Color = clWindowText
    BoundLabel.Font.Height = -12
    BoundLabel.Font.Name = 'Segoe UI'
    BoundLabel.Font.Style = []
    BoundLabel.ParentFont = False
  end
  object pb1: TJppProgressBar
    Left = 66
    Top = 37
    Width = 447
    Height = 30
    Hint = 'Progress'
    Caption = 'pb1'
    ShowHint = True
    Max = 200
    Position = 50
    AnchoredControls.Bottom.Control = lblProgress
    AnchoredControls.Bottom.Spacing = 5
    AnchoredControls.Left.Control = lblMin
    AnchoredControls.Left.Spacing = 10
    AnchoredControls.Left.Position = lcpLeftCenter
    AnchoredControls.Right.Control = lblMax
    AnchoredControls.Right.Spacing = 10
    AnchoredControls.Right.Position = rcpRightCenter
    Appearance.BackgroundColor = 9276813
    Appearance.BackgroundColorTo = 6250335
    Appearance.ProgressColor = 16490317
    Appearance.ProgressColorTo = 12015876
    Appearance.BorderColor = 1710618
    Appearance.BorderSize = 2
    Appearance.DisabledBackGroundColor = clSilver
    Appearance.DisabledBackgroundColorTo = clSilver
    Appearance.DisabledProgressColor = clMedGray
    Appearance.DisabledProgressColorTo = clMedGray
    Appearance.Font.Charset = EASTEUROPE_CHARSET
    Appearance.Font.Color = 7527422
    Appearance.Font.Height = -15
    Appearance.Font.Name = 'Consolas'
    Appearance.Font.Style = []
    Appearance.DisabledFont.Charset = EASTEUROPE_CHARSET
    Appearance.DisabledFont.Color = 3552822
    Appearance.DisabledFont.Height = -15
    Appearance.DisabledFont.Name = 'Consolas'
    Appearance.DisabledFont.Style = []
    Appearance.TextPrefix = 'Operation progress: '
    Appearance.TextPostfix = '%   Please wait...'
    Appearance.TextShadowSize = 1
    Appearance.TextShadowColor = clBlack
    Appearance.DisabledTextShadowColor = clGray
    Appearance.EndMarkerColor = 106710
    Appearance.DisabledEndMarkerColor = clGray
    Appearance.EndMarkerWidth = 3
    Appearance.MidStage.Paleness = -20
    Appearance.FinalStage.PercentThreshold = 80
    Appearance.FinalStage.Paleness = -40
    BoundLabel.Width = 255
    BoundLabel.Height = 15
    BoundLabel.Caption = 'TJppProgressBar.BoundLabel - Position: lpAbove'
    BoundLabel.Font.Charset = DEFAULT_CHARSET
    BoundLabel.Font.Color = clWindowText
    BoundLabel.Font.Height = -12
    BoundLabel.Font.Name = 'Segoe UI'
    BoundLabel.Font.Style = []
    BoundLabel.ParentFont = False
    BoundLabelSpacing = 5
    OnProgressChanged = pb1ProgressChanged
  end
  object pb2: TJppProgressBar
    Left = 66
    Top = 115
    Width = 300
    Height = 25
    Caption = 'pb2'
    Max = 200
    Position = 50
    Appearance.GradientType = pgtHorizontalBar
    Appearance.BackgroundColor = 12383486
    Appearance.BackgroundColorTo = 3136763
    Appearance.ProgressColor = 9364273
    Appearance.ProgressColorTo = 6728727
    Appearance.BorderColor = 6003477
    Appearance.Font.Charset = DEFAULT_CHARSET
    Appearance.Font.Color = clBlack
    Appearance.Font.Height = -13
    Appearance.Font.Name = 'Segoe UI'
    Appearance.Font.Style = []
    Appearance.DisabledFont.Charset = DEFAULT_CHARSET
    Appearance.DisabledFont.Color = clMedGray
    Appearance.DisabledFont.Height = -13
    Appearance.DisabledFont.Name = 'Segoe UI'
    Appearance.DisabledFont.Style = []
    Appearance.TextDisplayMode = ptdPixel
    Appearance.TextPostfix = ' pix / 300 pix'
    Appearance.TextShadowSize = 1
    Appearance.EndMarkerColor = 6003477
    BoundLabel.Width = 3
    BoundLabel.Height = 15
    BoundLabel.Font.Charset = DEFAULT_CHARSET
    BoundLabel.Font.Color = clWindowText
    BoundLabel.Font.Height = -12
    BoundLabel.Font.Name = 'Segoe UI'
    BoundLabel.Font.Style = []
    BoundLabel.ParentFont = False
  end
  object lblProgress: TLabel
    Left = 66
    Top = 72
    Width = 66
    Height = 17
    Caption = 'lblProgress'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
  end
  object lblMin: TLabel
    Left = 22
    Top = 45
    Width = 34
    Height = 15
    Caption = 'lblMin'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
  end
  object lblMax: TLabel
    Left = 523
    Top = 45
    Width = 36
    Height = 15
    Caption = 'lblMax'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
  end
  object pb4: TJppProgressBar
    Left = 66
    Top = 222
    Width = 50
    Height = 25
    Caption = 'pb4'
    Max = 200
    Position = 50
    AnchoredControls.Right.Control = lblTextOnly
    AnchoredControls.Right.Spacing = 5
    AnchoredControls.Right.Position = rcpRightCenter
    Appearance.Font.Charset = DEFAULT_CHARSET
    Appearance.Font.Color = clWindowText
    Appearance.Font.Height = -11
    Appearance.Font.Name = 'Tahoma'
    Appearance.Font.Style = []
    Appearance.DisabledFont.Charset = DEFAULT_CHARSET
    Appearance.DisabledFont.Color = clMedGray
    Appearance.DisabledFont.Height = -11
    Appearance.DisabledFont.Name = 'Tahoma'
    Appearance.DisabledFont.Style = []
    Appearance.TextPostfix = '%'
    Appearance.ProgressBarVisible = False
    BoundLabel.Width = 3
    BoundLabel.Height = 15
  end
  object lblTextOnly: TLabel
    Left = 121
    Top = 219
    Width = 133
    Height = 30
    Caption = 'Text only'#13#10'(no internal progress bar)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
  end
  object pb5: TJppProgressBar
    Left = 67
    Top = 179
    Width = 446
    Height = 19
    Caption = 'pb5'
    Max = 200
    Position = 50
    AnchoredControls.Top.Control = lblStage
    Appearance.DrawGradient = False
    Appearance.GradientType = pgtHorizontalBar
    Appearance.BackgroundColor = clBlack
    Appearance.ProgressColor = 6002719
    Appearance.BorderColor = clBlack
    Appearance.Font.Charset = DEFAULT_CHARSET
    Appearance.Font.Color = clWhite
    Appearance.Font.Height = -12
    Appearance.Font.Name = 'Segoe UI'
    Appearance.Font.Style = []
    Appearance.DisabledFont.Charset = DEFAULT_CHARSET
    Appearance.DisabledFont.Color = clMedGray
    Appearance.DisabledFont.Height = -12
    Appearance.DisabledFont.Name = 'Segoe UI'
    Appearance.DisabledFont.Style = []
    Appearance.TextPostfix = '%'
    Appearance.EndMarkerWidth = 0
    Appearance.MidStage.PercentThreshold = 40
    Appearance.MidStage.MixWithColor = clFuchsia
    Appearance.MidStage.Paleness = 0
    Appearance.FinalStage.PercentThreshold = 80
    Appearance.FinalStage.MixWithColor = clRed
    Appearance.FinalStage.Paleness = 0
    BoundLabel.Width = 3
    BoundLabel.Height = 15
    OnProgressChanged = pb5ProgressChanged
  end
  object lblStage: TLabel
    Left = 67
    Top = 161
    Width = 42
    Height = 15
    Caption = 'lblStage'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
  end
  object btnPauseResume: TButton
    Left = 66
    Top = 279
    Width = 153
    Height = 25
    Caption = 'Pause / Resume timer'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    OnClick = btnPauseResumeClick
  end
  object btnEnableDisable: TButton
    Left = 232
    Top = 279
    Width = 193
    Height = 25
    Caption = 'Enable / Disable progress bars'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
    OnClick = btnEnableDisableClick
  end
  object Actions: TActionList
    Left = 384
    object actEsc: TAction
      Caption = 'actEsc'
      ShortCut = 27
      OnExecute = actEscExecute
    end
  end
  object tmProgress: TJppTimer
    Interval = 50
    OnTimer = tmProgressTimer
    Counter = 0
    Left = 440
  end
end
