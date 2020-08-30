object Form1: TForm1
  Left = 432
  Height = 322
  Top = 97
  Width = 597
  Caption = 'Form1'
  ClientHeight = 322
  ClientWidth = 597
  Color = 13948116
  DoubleBuffered = True
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Segoe UI'
  OnCreate = FormCreate
  ParentDoubleBuffered = False
  Position = poDefaultPosOnly
  LCLVersion = '7.1'
  object pb3: TJppProgressBar
    Left = 396
    Height = 17
    Top = 120
    Width = 117
    Caption = 'pb3'
    Max = 200
    Position = 50
    Appearance.Font.Color = clWindowText
    Appearance.Font.Height = -11
    Appearance.Font.Name = 'Tahoma'
    Appearance.DisabledFont.Color = clMedGray
    Appearance.DisabledFont.Height = -11
    Appearance.DisabledFont.Name = 'Tahoma'
    Appearance.TextPostfix = '%'
    BoundLabel.Height = 1
    BoundLabel.Width = 1
    BoundLabel.ParentColor = False
  end
  object pb1: TJppProgressBar
    Left = 66
    Height = 30
    Hint = 'Progress'
    Top = 37
    Width = 447
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
    Appearance.Font.CharSet = EASTEUROPE_CHARSET
    Appearance.Font.Color = 7527422
    Appearance.Font.Height = -15
    Appearance.Font.Name = 'Consolas'
    Appearance.DisabledFont.CharSet = EASTEUROPE_CHARSET
    Appearance.DisabledFont.Color = 3552822
    Appearance.DisabledFont.Height = -15
    Appearance.DisabledFont.Name = 'Consolas'
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
    BoundLabel.Height = 1
    BoundLabel.Width = 1
    BoundLabel.ParentColor = False
    BoundLabelSpacing = 5
    OnProgressChanged = pb1ProgressChanged
  end
  object pb2: TJppProgressBar
    Left = 66
    Height = 25
    Top = 115
    Width = 300
    Caption = 'pb2'
    Max = 200
    Position = 50
    Appearance.GradientType = pgtHorizontalBar
    Appearance.BackgroundColor = 12383486
    Appearance.BackgroundColorTo = 3136763
    Appearance.ProgressColor = 9364273
    Appearance.ProgressColorTo = 6728727
    Appearance.BorderColor = 6003477
    Appearance.Font.Color = clBlack
    Appearance.Font.Height = -13
    Appearance.Font.Name = 'Segoe UI'
    Appearance.DisabledFont.Color = clMedGray
    Appearance.DisabledFont.Height = -13
    Appearance.DisabledFont.Name = 'Segoe UI'
    Appearance.TextDisplayMode = ptdPixel
    Appearance.TextPostfix = ' pix / 300 pix'
    Appearance.TextShadowSize = 1
    Appearance.EndMarkerColor = 6003477
    BoundLabel.Height = 1
    BoundLabel.Width = 1
    BoundLabel.ParentColor = False
  end
  object lblProgress: TLabel
    Left = 66
    Height = 17
    Top = 72
    Width = 66
    Caption = 'lblProgress'
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Segoe UI'
    ParentColor = False
    ParentFont = False
  end
  object lblMin: TLabel
    Left = 22
    Height = 15
    Top = 45
    Width = 34
    Caption = 'lblMin'
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    ParentColor = False
    ParentFont = False
  end
  object lblMax: TLabel
    Left = 523
    Height = 15
    Top = 45
    Width = 36
    Caption = 'lblMax'
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    ParentColor = False
    ParentFont = False
  end
  object pb4: TJppProgressBar
    Left = 66
    Height = 25
    Top = 222
    Width = 50
    Caption = 'pb4'
    Max = 200
    Position = 50
    AnchoredControls.Right.Control = lblTextOnly
    AnchoredControls.Right.Spacing = 5
    AnchoredControls.Right.Position = rcpRightCenter
    Appearance.Font.Color = clWindowText
    Appearance.Font.Height = -11
    Appearance.Font.Name = 'Tahoma'
    Appearance.DisabledFont.Color = clMedGray
    Appearance.DisabledFont.Height = -11
    Appearance.DisabledFont.Name = 'Tahoma'
    Appearance.TextPostfix = '%'
    Appearance.ProgressBarVisible = False
    BoundLabel.Height = 1
    BoundLabel.Width = 1
    BoundLabel.ParentColor = False
  end
  object lblTextOnly: TLabel
    Left = 121
    Height = 30
    Top = 219
    Width = 133
    Caption = 'Text only'#13#10'(no internal progress bar)'
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    ParentColor = False
    ParentFont = False
  end
  object pb5: TJppProgressBar
    Left = 67
    Height = 19
    Top = 179
    Width = 446
    Caption = 'pb5'
    Max = 200
    Position = 50
    AnchoredControls.Top.Control = lblStage
    Appearance.DrawGradient = False
    Appearance.GradientType = pgtHorizontalBar
    Appearance.BackgroundColor = clBlack
    Appearance.ProgressColor = 6002719
    Appearance.BorderColor = clBlack
    Appearance.Font.Color = clWhite
    Appearance.Font.Height = -12
    Appearance.Font.Name = 'Segoe UI'
    Appearance.DisabledFont.Color = clMedGray
    Appearance.DisabledFont.Height = -12
    Appearance.DisabledFont.Name = 'Segoe UI'
    Appearance.TextPostfix = '%'
    Appearance.EndMarkerWidth = 0
    Appearance.MidStage.PercentThreshold = 40
    Appearance.MidStage.MixWithColor = clFuchsia
    Appearance.MidStage.Paleness = 0
    Appearance.FinalStage.PercentThreshold = 80
    Appearance.FinalStage.MixWithColor = clRed
    Appearance.FinalStage.Paleness = 0
    BoundLabel.Height = 1
    BoundLabel.Width = 1
    BoundLabel.ParentColor = False
    OnProgressChanged = pb5ProgressChanged
  end
  object lblStage: TLabel
    Left = 67
    Height = 15
    Top = 161
    Width = 42
    Caption = 'lblStage'
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    ParentColor = False
    ParentFont = False
  end
  object btnPauseResume: TButton
    Left = 66
    Height = 25
    Top = 279
    Width = 153
    Caption = 'Pause / Resume timer'
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    OnClick = btnPauseResumeClick
    ParentFont = False
    TabOrder = 0
  end
  object btnEnableDisable: TButton
    Left = 232
    Height = 25
    Top = 279
    Width = 193
    Caption = 'Enable / Disable progress bars'
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Segoe UI'
    OnClick = btnEnableDisableClick
    ParentFont = False
    TabOrder = 1
  end
  object Button1: TButton
    Left = 445
    Height = 25
    Top = 231
    Width = 75
    Caption = 'Button1'
    TabOrder = 2
  end
  object Actions: TActionList
    Left = 384
    object actEsc: TAction
      Caption = 'actEsc'
      OnExecute = actEscExecute
      ShortCut = 27
    end
  end
  object tmProgress: TJppTimer
    Interval = 50
    OnTimer = tmProgressTimer
    Counter = 0
    Left = 440
  end
end
