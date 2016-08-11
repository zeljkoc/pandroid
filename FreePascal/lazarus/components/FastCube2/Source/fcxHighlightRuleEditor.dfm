object fcxHighlightRuleEditorDialog: TfcxHighlightRuleEditorDialog
  Left = 228
  Top = 138
  BorderStyle = bsDialog
  BorderWidth = 6
  Caption = 'fcxHighlightRuleEditorDialog'
  ClientHeight = 415
  ClientWidth = 615
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
    615
    415)
  PixelsPerInch = 96
  TextHeight = 13
  object Pages: TPageControl
    Left = 0
    Top = 0
    Width = 465
    Height = 382
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
  end
  object OkBtn: TButton
    Left = 460
    Top = 388
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OkBtn'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object CancelBtn: TButton
    Left = 540
    Top = 388
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'CancelBtn'
    ModalResult = 2
    TabOrder = 2
  end
  object pApplyTo: TPanel
    Left = 472
    Top = 0
    Width = 142
    Height = 382
    Anchors = [akTop, akRight, akBottom]
    BevelOuter = bvNone
    Caption = 'pApplyTo'
    TabOrder = 3
    object clbApplyTo: TCheckListBox
      Left = 0
      Top = 17
      Width = 142
      Height = 365
      Align = alClient
      ItemHeight = 13
      TabOrder = 0
    end
    object hdApplyTo: THeaderControl
      Left = 0
      Top = 0
      Width = 142
      Height = 17
      Sections = <
        item
          AllowClick = False
          ImageIndex = -1
          Text = 'ApplyTo'
          Width = 142
        end>
    end
  end
end
