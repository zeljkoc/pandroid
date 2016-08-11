object fcxStyleEditorFrame: TfcxStyleEditorFrame
  Left = 0
  Top = 0
  Width = 370
  Height = 154
  AutoSize = True
  TabOrder = 0
  object lblFillStyle: TLabel
    Left = 0
    Top = 0
    Width = 41
    Height = 13
    Caption = 'Fill Style:'
  end
  object lblFillColor1: TLabel
    Left = 0
    Top = 40
    Width = 51
    Height = 13
    Caption = 'Fill Color 1:'
  end
  object lblFillColor2: TLabel
    Left = 0
    Top = 80
    Width = 51
    Height = 13
    Caption = 'Fill Color 2:'
  end
  object lblTextColor: TLabel
    Left = 0
    Top = 118
    Width = 51
    Height = 13
    Caption = 'Text Color:'
  end
  object PaintBox1: TPaintBox
    Left = 225
    Top = 96
    Width = 145
    Height = 58
    OnDblClick = PaintBox1DblClick
    OnPaint = PaintBox1Paint
  end
  object lblExample: TLabel
    Left = 224
    Top = 80
    Width = 43
    Height = 13
    Caption = 'Example:'
  end
  object lblTextStyle: TLabel
    Left = 224
    Top = 0
    Width = 50
    Height = 13
    Caption = 'Text Style:'
  end
  object cbFillStyle: TComboBox
    Left = 0
    Top = 16
    Width = 217
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 0
    OnChange = cbFillStyleChange
  end
  object cbFillColor1: TColorBox
    Left = 0
    Top = 56
    Width = 217
    Height = 22
    ItemHeight = 16
    TabOrder = 1
    OnChange = cbFillStyleChange
  end
  object cbFillColor2: TColorBox
    Left = 0
    Top = 94
    Width = 217
    Height = 22
    ItemHeight = 16
    TabOrder = 2
    OnChange = cbFillStyleChange
  end
  object cbTextColor: TColorBox
    Left = 0
    Top = 132
    Width = 217
    Height = 22
    ItemHeight = 16
    TabOrder = 3
    OnChange = cbFillStyleChange
  end
  object cbTextStyle: TCheckListBox
    Left = 225
    Top = 14
    Width = 145
    Height = 63
    OnClickCheck = cbFillStyleChange
    ItemHeight = 13
    TabOrder = 4
  end
end
