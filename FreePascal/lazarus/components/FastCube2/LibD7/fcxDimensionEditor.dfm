object fcxDimensionEditorForm: TfcxDimensionEditorForm
  Left = 571
  Top = 201
  BorderStyle = bsDialog
  BorderWidth = 6
  Caption = 'fcDimensionEditorForm'
  ClientHeight = 350
  ClientWidth = 395
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
    395
    350)
  PixelsPerInch = 96
  TextHeight = 13
  object Pages: TPageControl
    Left = 0
    Top = 0
    Width = 395
    Height = 316
    ActivePage = PageGeneral
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object PageGeneral: TTabSheet
      BorderWidth = 6
      Caption = 'General'
      DesignSize = (
        375
        276)
      object lblCaption: TLabel
        Left = 0
        Top = 6
        Width = 39
        Height = 13
        Caption = 'Caption:'
      end
      object lblTotalPosition: TLabel
        Left = 0
        Top = 58
        Width = 71
        Height = 13
        Caption = 'lblTotalPosition'
      end
      object lblSortDirection: TLabel
        Left = 0
        Top = 82
        Width = 71
        Height = 13
        Caption = 'lblSortDirection'
      end
      object lblAdditionalTotals: TLabel
        Left = 0
        Top = 186
        Width = 85
        Height = 13
        Caption = 'lblAdditionalTotals'
      end
      object lblFunctionTotal: TLabel
        Left = 0
        Top = 106
        Width = 44
        Height = 13
        Caption = 'Function:'
      end
      object edCaption: TEdit
        Left = 64
        Top = 0
        Width = 262
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
        Text = 'edCaption'
      end
      object cbTotalPosition: TComboBox
        Left = 142
        Top = 54
        Width = 197
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        TabOrder = 1
      end
      object cbUseTotalPositionFromMeasure: TCheckBox
        Left = 0
        Top = 32
        Width = 273
        Height = 17
        Caption = 'cbUseTotalPositionFromMeasure'
        TabOrder = 2
      end
      object cbSortDirection: TComboBox
        Left = 142
        Top = 78
        Width = 197
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        TabOrder = 3
      end
      object clbAdditionalTotals: TCheckListBox
        Left = 0
        Top = 200
        Width = 375
        Height = 76
        Align = alBottom
        ItemHeight = 13
        TabOrder = 4
      end
      object cbScriptFunctionTotal: TComboBox
        Left = 142
        Top = 103
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
    object PageFormat: TTabSheet
      Caption = 'Format'
      ImageIndex = 4
      inline FormatFrame: TfcxFormatEditorFrame
        Left = 13
        Top = 13
        Width = 361
        Height = 265
        AutoSize = True
        TabOrder = 0
        inherited CategoryL: TGroupBox
          Width = 177
          Height = 169
          DesignSize = (
            177
            169)
          inherited CategoryLB: TListBox
            Width = 160
            Height = 139
          end
        end
        inherited FormatL: TGroupBox
          Left = 190
          Width = 171
          Height = 169
          DesignSize = (
            171
            169)
          inherited FormatLB: TListBox
            Width = 155
            Height = 139
          end
        end
        inherited GroupBox1: TGroupBox
          Top = 178
          Width = 361
          inherited FormatE: TEdit
            Width = 183
          end
        end
      end
    end
  end
  object OkBtn: TButton
    Left = 161
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
    Left = 241
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
    Left = 321
    Top = 323
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Apply'
    TabOrder = 3
    OnClick = ApplyBtnClick
  end
end
