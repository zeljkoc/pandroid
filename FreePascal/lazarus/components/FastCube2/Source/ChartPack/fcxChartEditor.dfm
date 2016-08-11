object fcxChartEditor: TfcxChartEditor
  Left = 246
  Top = 155
  BorderIcons = [biSystemMenu, biMaximize]
  BorderStyle = bsDialog
  BorderWidth = 7
  ClientHeight = 286
  ClientWidth = 506
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    506
    286)
  PixelsPerInch = 96
  TextHeight = 13
  object lbBaseAxisDataType: TLabel
    Left = 0
    Top = 235
    Width = 98
    Height = 13
    Caption = 'lbBaseAxisDataType'
  end
  object OkBtn: TButton
    Left = 349
    Top = 261
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object GroupBox2: TGroupBox
    Left = 0
    Top = 20
    Width = 506
    Height = 181
    Anchors = [akLeft, akTop, akRight, akBottom]
    Caption = 'DataSource'
    TabOrder = 1
    DesignSize = (
      506
      181)
    object lbSeriesFieldCount: TLabel
      Left = 8
      Top = 122
      Width = 92
      Height = 13
      Caption = 'lbSeriesFieldCount:'
    end
    object lbValuesFieldCount: TLabel
      Left = 255
      Top = 122
      Width = 94
      Height = 13
      Caption = 'lbValuesFieldCount:'
    end
    object lbMeasureFieldIndex: TLabel
      Left = 8
      Top = 158
      Width = 103
      Height = 13
      Caption = 'lbMeasureFieldIndex:'
    end
    object ChartDataType: TComboBox
      Left = 6
      Top = 16
      Width = 495
      Height = 21
      Style = csDropDownList
      Anchors = [akLeft, akTop, akRight]
      ItemHeight = 13
      TabOrder = 0
      OnChange = ChartDataTypeChange
    end
    object SeriesAxis: TRadioGroup
      Left = 6
      Top = 40
      Width = 244
      Height = 73
      Caption = 'SeriesAxis'
      TabOrder = 1
      OnClick = SeriesAxisClick
    end
    object ValuesAxis: TRadioGroup
      Left = 255
      Top = 40
      Width = 244
      Height = 73
      Caption = 'ValuesAxis'
      TabOrder = 2
      OnClick = ValuesAxisClick
    end
    object SeriesFieldCount: TSpinEdit
      Left = 191
      Top = 120
      Width = 56
      Height = 22
      MaxValue = 100
      MinValue = 0
      TabOrder = 3
      Value = 0
      OnChange = SeriesFieldCountChange
    end
    object MeasureFieldIndex: TSpinEdit
      Left = 191
      Top = 150
      Width = 56
      Height = 22
      MaxValue = 100
      MinValue = 0
      TabOrder = 4
      Value = 0
      OnChange = MeasureFieldIndexChange
    end
    object ValuesFieldCount: TSpinEdit
      Left = 443
      Top = 120
      Width = 56
      Height = 22
      MaxValue = 100
      MinValue = 0
      TabOrder = 5
      Value = 0
      OnChange = ValuesFieldCountChange
    end
  end
  object CancelBtn: TButton
    Left = 430
    Top = 261
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object RealTimeChange: TCheckBox
    Left = 0
    Top = 0
    Width = 193
    Height = 17
    Caption = 'RealTimeChange'
    Checked = True
    State = cbChecked
    TabOrder = 3
  end
  object SkipNullPoints: TCheckBox
    Left = 0
    Top = 208
    Width = 193
    Height = 17
    Caption = 'SkipNullPoints'
    TabOrder = 4
    OnClick = SkipNullPointsClick
  end
  object BaseAxisDataType: TComboBox
    Left = 159
    Top = 232
    Width = 160
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    TabOrder = 5
    OnChange = BaseAxisDataTypeChange
  end
end
