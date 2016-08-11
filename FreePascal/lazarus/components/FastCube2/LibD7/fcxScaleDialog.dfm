object fcxScaleDialogForm: TfcxScaleDialogForm
  Left = 215
  Top = 116
  BorderStyle = bsDialog
  BorderWidth = 6
  Caption = 'fcxScaleDialogForm'
  ClientHeight = 217
  ClientWidth = 177
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  DesignSize = (
    177
    217)
  PixelsPerInch = 96
  TextHeight = 13
  object rgScale: TRadioGroup
    Left = 0
    Top = 0
    Width = 177
    Height = 185
    Align = alTop
    Caption = 'rgScale'
    Items.Strings = (
      '300%'
      '200%'
      '100%'
      '75%'
      '50%'
      '25%'
      'sCustom')
    TabOrder = 0
    OnClick = rgScaleClick
  end
  object OkBtn: TButton
    Left = 22
    Top = 192
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OkBtn'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object CancelBtn: TButton
    Left = 102
    Top = 192
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'CancelBtn'
    ModalResult = 2
    TabOrder = 2
  end
end
