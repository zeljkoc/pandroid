object fcxFormatEditorFrame: TfcxFormatEditorFrame
  Left = 0
  Top = 0
  Width = 311
  Height = 233
  AutoSize = True
  TabOrder = 0
  DesignSize = (
    311
    233)
  object CategoryL: TGroupBox
    Left = 0
    Top = 0
    Width = 151
    Height = 137
    Anchors = [akLeft, akTop, akBottom]
    Caption = 'Category'
    TabOrder = 0
    DesignSize = (
      151
      137)
    object CategoryLB: TListBox
      Left = 8
      Top = 20
      Width = 133
      Height = 105
      Style = lbOwnerDrawFixed
      Anchors = [akLeft, akTop, akRight, akBottom]
      ItemHeight = 14
      TabOrder = 0
      OnClick = CategoryLBClick
    end
  end
  object FormatL: TGroupBox
    Left = 160
    Top = 0
    Width = 151
    Height = 137
    Anchors = [akLeft, akTop, akBottom]
    Caption = 'Format'
    TabOrder = 1
    DesignSize = (
      151
      137)
    object FormatLB: TListBox
      Left = 8
      Top = 20
      Width = 133
      Height = 105
      Style = lbOwnerDrawFixed
      Anchors = [akLeft, akTop, akRight, akBottom]
      ItemHeight = 14
      TabOrder = 0
      OnClick = FormatLBClick
      OnDrawItem = FormatLBDrawItem
    end
  end
  object GroupBox1: TGroupBox
    Left = 0
    Top = 146
    Width = 311
    Height = 87
    Anchors = [akRight, akBottom]
    TabOrder = 2
    DesignSize = (
      311
      87)
    object FormatStrL: TLabel
      Left = 11
      Top = 24
      Width = 63
      Height = 13
      Caption = 'Format string:'
    end
    object SeparatorL: TLabel
      Left = 11
      Top = 55
      Width = 88
      Height = 13
      Caption = 'Decimal separator:'
    end
    object FormatE: TEdit
      Left = 165
      Top = 24
      Width = 133
      Height = 21
      Anchors = [akLeft, akTop, akRight]
      TabOrder = 0
    end
    object SeparatorE: TEdit
      Left = 165
      Top = 55
      Width = 25
      Height = 21
      TabOrder = 1
      Text = ','
    end
  end
end
