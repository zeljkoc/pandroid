object fcxTopNDialogForm: TfcxTopNDialogForm
  Left = 566
  Top = 135
  BorderStyle = bsDialog
  BorderWidth = 6
  Caption = 'fcxTopNDialogForm'
  ClientHeight = 121
  ClientWidth = 272
  Color = clBtnFace
  Constraints.MinHeight = 164
  Constraints.MinWidth = 292
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  DesignSize = (
    272
    121)
  PixelsPerInch = 96
  TextHeight = 13
  object lblDimension: TLabel
    Left = 0
    Top = 0
    Width = 59
    Height = 13
    Caption = 'lblDimension'
  end
  object lblMeasure: TLabel
    Left = 0
    Top = 28
    Width = 51
    Height = 13
    Caption = 'lblMeasure'
  end
  object lblShow: TLabel
    Left = 0
    Top = 54
    Width = 37
    Height = 13
    Caption = 'lblShow'
  end
  object OkBtn: TButton
    Left = 117
    Top = 96
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object CancelBtn: TButton
    Left = 197
    Top = 96
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 0
  end
  object cbCreateOthers: TCheckBox
    Left = 0
    Top = 75
    Width = 129
    Height = 17
    Caption = 'cbCreateOthers'
    TabOrder = 2
  end
  object cbDimension: TComboBox
    Left = 80
    Top = 0
    Width = 192
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    TabOrder = 3
  end
  object cbMeasure: TComboBox
    Left = 80
    Top = 24
    Width = 192
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    TabOrder = 4
  end
  object SpinEdit1: TSpinEdit
    Left = 80
    Top = 48
    Width = 49
    Height = 22
    MaxValue = 99999
    MinValue = 1
    TabOrder = 5
    Value = 5
  end
  object cbTopType: TComboBox
    Left = 136
    Top = 48
    Width = 136
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    ItemHeight = 13
    TabOrder = 6
  end
end
