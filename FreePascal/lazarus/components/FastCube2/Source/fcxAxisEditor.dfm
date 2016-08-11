object fcxAxisEditorForm: TfcxAxisEditorForm
  Left = 214
  Top = 219
  BorderStyle = bsDialog
  BorderWidth = 6
  Caption = 'fcxAxisEditorForm'
  ClientHeight = 350
  ClientWidth = 394
  Color = clBtnFace
  Constraints.MinHeight = 389
  Constraints.MinWidth = 300
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  DesignSize = (
    394
    350)
  PixelsPerInch = 96
  TextHeight = 13
  object Pages: TPageControl
    Left = 0
    Top = 0
    Width = 394
    Height = 316
    ActivePage = PageGeneral
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object PageGeneral: TTabSheet
      BorderWidth = 6
      Caption = 'General'
      DesignSize = (
        374
        276)
      object lblAdditionalTotals: TLabel
        Left = 0
        Top = 146
        Width = 85
        Height = 13
        Caption = 'lblAdditionalTotals'
      end
      object lblTotalPosition: TLabel
        Left = 0
        Top = 86
        Width = 71
        Height = 13
        Caption = 'lblTotalPosition'
      end
      object lblAxisType: TLabel
        Left = 0
        Top = 4
        Width = 53
        Height = 13
        Caption = 'lblAxisType'
      end
      object lblSortType: TLabel
        Left = 0
        Top = 31
        Width = 53
        Height = 13
        Caption = 'lblSortType'
      end
      object lblFunctionTotal: TLabel
        Left = 0
        Top = 114
        Width = 44
        Height = 13
        Caption = 'Function:'
      end
      object clbAdditionalTotals: TCheckListBox
        Left = 0
        Top = 160
        Width = 374
        Height = 116
        Align = alBottom
        ItemHeight = 13
        TabOrder = 0
      end
      object cbTotalPosition: TComboBox
        Left = 142
        Top = 82
        Width = 196
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        TabOrder = 1
      end
      object cbAxisType: TComboBox
        Left = 142
        Top = 0
        Width = 196
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        TabOrder = 2
      end
      object cbSortType: TComboBox
        Left = 142
        Top = 27
        Width = 196
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        TabOrder = 3
      end
      object cbHideZeros: TCheckBox
        Left = 0
        Top = 56
        Width = 369
        Height = 17
        Caption = 'cbHideZeros'
        TabOrder = 4
      end
      object cbScriptFunctionTotal: TComboBox
        Left = 142
        Top = 111
        Width = 197
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        TabOrder = 5
        Text = 'cbScriptFunction'
        OnDblClick = cbScriptFunctionTotalDblClick
        OnDropDown = cbScriptFunctionTotalDropDown
      end
    end
  end
  object OkBtn: TButton
    Left = 160
    Top = 323
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object CancelBtn: TButton
    Left = 240
    Top = 323
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 2
  end
  object ApplyBtn: TButton
    Left = 320
    Top = 323
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Apply'
    TabOrder = 3
    OnClick = ApplyBtnClick
  end
end
