object fcxContinuousHighlightEditorFrame: TfcxContinuousHighlightEditorFrame
  Left = 0
  Top = 0
  Width = 544
  Height = 240
  TabOrder = 0
  object lblHighlightKind: TLabel
    Left = 6
    Top = 10
    Width = 72
    Height = 13
    Caption = 'lblHighlightKind'
  end
  object lblType: TLabel
    Left = 6
    Top = 54
    Width = 34
    Height = 13
    Caption = 'lblType'
  end
  object lblValue: TLabel
    Left = 6
    Top = 78
    Width = 37
    Height = 13
    Caption = 'lblValue'
  end
  object lblColor: TLabel
    Left = 6
    Top = 102
    Width = 34
    Height = 13
    Caption = 'lblColor'
  end
  object lblMinValue: TLabel
    Left = 90
    Top = 32
    Width = 54
    Height = 13
    Caption = 'lblMinValue'
  end
  object lblMidValue: TLabel
    Left = 242
    Top = 32
    Width = 54
    Height = 13
    Caption = 'lblMidValue'
  end
  object lblMaxValue: TLabel
    Left = 394
    Top = 32
    Width = 57
    Height = 13
    Caption = 'lblMaxValue'
  end
  object pbScale: TPaintBox
    Left = 90
    Top = 156
    Width = 145
    Height = 25
    Color = clBtnFace
    ParentColor = False
    OnPaint = pbScalePaint
  end
  object lblExample: TLabel
    Left = 6
    Top = 162
    Width = 50
    Height = 13
    Caption = 'lblExample'
  end
  object lblFrameColor: TLabel
    Left = 6
    Top = 126
    Width = 63
    Height = 13
    Caption = 'lblFrameColor'
  end
  object lblIconSet: TLabel
    Left = 6
    Top = 32
    Width = 47
    Height = 13
    Caption = 'lblIconSet'
  end
  object cbHighlightKind: TComboBox
    Left = 90
    Top = 6
    Width = 145
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 0
    OnChange = cbHighlightKindChange
  end
  object cbMinValueType: TComboBox
    Left = 90
    Top = 48
    Width = 145
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 1
    OnChange = cbMinValueTypeChange
  end
  object cbMidValueType: TComboBox
    Left = 242
    Top = 48
    Width = 145
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 2
    OnChange = cbMidValueTypeChange
  end
  object cbMaxValueType: TComboBox
    Left = 394
    Top = 48
    Width = 145
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 3
    OnChange = cbMaxValueTypeChange
  end
  object edMinValue: TEdit
    Left = 90
    Top = 72
    Width = 145
    Height = 21
    TabOrder = 4
    Text = '0'
    OnChange = edMinValueChange
  end
  object edMidValue: TEdit
    Left = 242
    Top = 72
    Width = 145
    Height = 21
    TabOrder = 5
    Text = '0'
    OnChange = edMidValueChange
  end
  object edMaxValue: TEdit
    Left = 394
    Top = 72
    Width = 145
    Height = 21
    TabOrder = 6
    Text = '0'
    OnChange = edMaxValueChange
  end
  object cbMinValueColor: TColorBox
    Left = 90
    Top = 96
    Width = 145
    Height = 22
    ItemHeight = 16
    TabOrder = 7
    OnChange = cbMinValueColorChange
  end
  object cbMidValueColor: TColorBox
    Left = 242
    Top = 96
    Width = 145
    Height = 22
    ItemHeight = 16
    TabOrder = 8
    OnChange = cbMidValueColorChange
  end
  object cbMaxValueColor: TColorBox
    Left = 394
    Top = 96
    Width = 145
    Height = 22
    ItemHeight = 16
    TabOrder = 9
    OnChange = cbMaxValueColorChange
  end
  object cbShowCellValue: TCheckBox
    Left = 242
    Top = 8
    Width = 145
    Height = 17
    Caption = 'cbShowCellValue'
    TabOrder = 10
    OnClick = cbShowCellValueClick
  end
  object cbGradientDraw: TCheckBox
    Left = 242
    Top = 99
    Width = 145
    Height = 17
    Caption = 'cbGradientDraw'
    TabOrder = 11
    OnClick = cbGradientDrawClick
  end
  object cbFrameColor: TColorBox
    Left = 90
    Top = 120
    Width = 145
    Height = 22
    ItemHeight = 16
    TabOrder = 12
    OnChange = cbFrameColorChange
  end
  object cbIconSet: TComboBox
    Left = 90
    Top = 30
    Width = 145
    Height = 22
    Style = csOwnerDrawFixed
    ItemHeight = 16
    TabOrder = 13
    OnChange = cbIconSetChange
    OnDrawItem = cbIconSetDrawItem
  end
  object btnReverseOrder: TButton
    Left = 242
    Top = 32
    Width = 145
    Height = 22
    Caption = 'btnReverseOrder'
    TabOrder = 14
    OnClick = btnReverseOrderClick
  end
end
