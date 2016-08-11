object fcxMeasureEditorForm: TfcxMeasureEditorForm
  Left = 964
  Top = 196
  BorderStyle = bsDialog
  BorderWidth = 6
  Caption = 'fcMeasureEditorForm'
  ClientHeight = 352
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
    352)
  PixelsPerInch = 96
  TextHeight = 13
  object Pages: TPageControl
    Left = 0
    Top = 0
    Width = 394
    Height = 318
    ActivePage = PageGeneral
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 0
    object PageGeneral: TTabSheet
      BorderWidth = 6
      Caption = 'General'
      DesignSize = (
        374
        278)
      object lblCaption: TLabel
        Left = 0
        Top = 31
        Width = 39
        Height = 13
        Caption = 'Caption:'
      end
      object lblAggregate: TLabel
        Left = 0
        Top = 57
        Width = 52
        Height = 13
        Caption = 'Aggregate:'
      end
      object lblBaseField: TLabel
        Left = 6
        Top = 84
        Width = 52
        Height = 13
        Caption = 'Base Field:'
      end
      object lblFunction: TLabel
        Left = 16
        Top = 138
        Width = 44
        Height = 13
        Caption = 'Function:'
      end
      object lblOrder: TLabel
        Left = 16
        Top = 165
        Width = 29
        Height = 13
        Caption = 'Order:'
      end
      object lblName: TLabel
        Left = 0
        Top = 4
        Width = 31
        Height = 13
        Caption = 'Name:'
      end
      object lbDistinctField: TLabel
        Left = 6
        Top = 224
        Width = 63
        Height = 13
        Caption = 'Distinct Field:'
      end
      object lblExtraField: TLabel
        Left = 6
        Top = 111
        Width = 52
        Height = 13
        Caption = 'Extra Field:'
      end
      object edCaption: TEdit
        Left = 64
        Top = 27
        Width = 148
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 1
        Text = 'edCaption'
      end
      object cbAggregate: TComboBox
        Left = 64
        Top = 54
        Width = 148
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        TabOrder = 2
        OnChange = cbAggregateChange
      end
      object cbBaseField: TComboBox
        Left = 80
        Top = 81
        Width = 132
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        TabOrder = 3
        OnSelect = cbBaseFieldSelect
      end
      object cbScriptFunction: TComboBox
        Left = 80
        Top = 135
        Width = 132
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        TabOrder = 4
        Text = 'cbScriptFunction'
        OnDblClick = cbScriptFunctionDblClick
        OnDropDown = cbScriptFunctionDropDown
      end
      object edOrder: TSpinEdit
        Left = 80
        Top = 162
        Width = 49
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 5
        Value = 0
      end
      object cbCalcAllCells: TCheckBox
        Left = 136
        Top = 165
        Width = 225
        Height = 17
        Caption = 'cbCalcAllCells'
        TabOrder = 6
      end
      object edName: TEdit
        Left = 64
        Top = 0
        Width = 148
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        TabOrder = 0
      end
      object cbDistinct: TCheckBox
        Left = 0
        Top = 197
        Width = 225
        Height = 17
        Caption = 'cbDistinct'
        TabOrder = 7
        OnClick = cbDistinctClick
      end
      object cbDistinctField: TComboBox
        Left = 80
        Top = 221
        Width = 132
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        TabOrder = 8
      end
      object cbExtraField: TComboBox
        Left = 80
        Top = 108
        Width = 132
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 13
        TabOrder = 9
      end
    end
    object PageTotals: TTabSheet
      BorderWidth = 6
      Caption = 'Totals'
      ImageIndex = 1
      DesignSize = (
        374
        278)
      object lblAggregateTotal: TLabel
        Left = 6
        Top = 22
        Width = 52
        Height = 13
        Caption = 'Aggregate:'
      end
      object lblDefaultTotalPosition: TLabel
        Left = 0
        Top = 227
        Width = 105
        Height = 13
        Caption = 'lblDefaultTotalPosition'
      end
      object lblFunctionTotal: TLabel
        Left = 16
        Top = 50
        Width = 44
        Height = 13
        Caption = 'Function:'
      end
      object lblOrderTotal: TLabel
        Left = 16
        Top = 77
        Width = 29
        Height = 13
        Caption = 'Order:'
      end
      object cbUseDifferentAggregateForTotals: TCheckBox
        Left = 0
        Top = 0
        Width = 257
        Height = 17
        Caption = 'cbUseDifferentAggregateForTotals'
        TabOrder = 0
        OnClick = cbUseDifferentAggregateForTotalsClick
      end
      object cbAggregateForTotals: TComboBox
        Left = 66
        Top = 19
        Width = 160
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 0
        TabOrder = 1
        OnChange = cbUseDifferentAggregateForTotalsClick
      end
      object cbCalcTotalsOnTotals: TCheckBox
        Left = 0
        Top = 107
        Width = 257
        Height = 17
        Caption = 'cbCalcTotalsOnTotals'
        TabOrder = 5
        OnClick = cbCalcTotalsOnTotalsClick
      end
      object cbUseXAxisTotalsAsBase: TCheckBox
        Left = 6
        Top = 129
        Width = 257
        Height = 17
        Caption = 'cbUseXAxisTotalsAsBase'
        TabOrder = 6
      end
      object rbTotalsConflictResolving: TRadioGroup
        Left = 0
        Top = 148
        Width = 369
        Height = 71
        Caption = 'rbTotalsConflictResolving'
        TabOrder = 7
      end
      object cbDefaultTotalPosition: TComboBox
        Left = 134
        Top = 223
        Width = 160
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 0
        TabOrder = 8
      end
      object cbScriptFunctionTotal: TComboBox
        Left = 82
        Top = 47
        Width = 108
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 0
        TabOrder = 2
        Text = 'cbScriptFunction'
        OnDblClick = cbScriptFunctionDblClick
        OnDropDown = cbScriptFunctionDropDown
      end
      object edOrderTotal: TSpinEdit
        Left = 82
        Top = 74
        Width = 49
        Height = 22
        MaxValue = 0
        MinValue = 0
        TabOrder = 3
        Value = 0
      end
      object cbCalcAllCellsTotal: TCheckBox
        Left = 138
        Top = 77
        Width = 223
        Height = 17
        Caption = 'cbCalcAllCells'
        TabOrder = 4
      end
    end
    object PageFiltering: TTabSheet
      BorderWidth = 6
      Caption = 'Filtering'
      ImageIndex = 2
      DesignSize = (
        374
        278)
      object lblFilterFunction: TLabel
        Left = 0
        Top = 4
        Width = 44
        Height = 13
        Caption = 'Function:'
      end
      object cbFilterFunction: TComboBox
        Left = 56
        Top = 0
        Width = 184
        Height = 21
        Anchors = [akLeft, akTop, akRight]
        ItemHeight = 0
        TabOrder = 0
        OnDblClick = cbFilterFunctionDblClick
        OnDropDown = cbFilterFunctionDropDown
      end
    end
    object PageStyle: TTabSheet
      Caption = 'Style'
      ImageIndex = 3
      object lbStyles: TListBox
        Left = 0
        Top = 42
        Width = 386
        Height = 248
        Style = lbOwnerDrawFixed
        Align = alClient
        ItemHeight = 30
        TabOrder = 0
        OnClick = lbStylesClick
        OnDblClick = lbStylesDblClick
        OnDrawItem = lbStylesDrawItem
      end
      object tbStyles: TToolBar
        Left = 0
        Top = 0
        Width = 386
        Height = 25
        ButtonHeight = 25
        ButtonWidth = 25
        Caption = 'tbStyles'
        EdgeBorders = [ebBottom]
        Flat = True
        TabOrder = 1
        Transparent = True
        object tbAdd: TToolButton
          Left = 0
          Top = 0
          Action = actAdd
          ParentShowHint = False
          ShowHint = True
        end
        object tbEdit: TToolButton
          Left = 25
          Top = 0
          Action = actEdit
          ParentShowHint = False
          ShowHint = True
        end
        object ToolButton3: TToolButton
          Left = 50
          Top = 0
          Width = 8
          Caption = 'ToolButton3'
          ImageIndex = 2
          Style = tbsSeparator
        end
        object tbDelete: TToolButton
          Left = 58
          Top = 0
          Action = actDelete
          ParentShowHint = False
          ShowHint = True
        end
        object tbUp: TToolButton
          Left = 83
          Top = 0
          Action = actMoveUp
          ParentShowHint = False
          ShowHint = True
        end
        object tbDown: TToolButton
          Left = 108
          Top = 0
          Action = actMoveDown
          ParentShowHint = False
          ShowHint = True
        end
      end
      object hdStyles: THeaderControl
        Left = 0
        Top = 25
        Width = 386
        Height = 17
        Sections = <
          item
            AllowClick = False
            ImageIndex = -1
            MaxWidth = 250
            MinWidth = 250
            Width = 250
          end
          item
            AllowClick = False
            ImageIndex = -1
            Width = 140
          end>
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
    Left = 160
    Top = 325
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
    Top = 325
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
    Top = 325
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = '&Apply'
    TabOrder = 3
    OnClick = ApplyBtnClick
  end
  object ActionList1: TActionList
    Left = 356
    object actAdd: TAction
      Caption = 'actAdd'
      ImageIndex = 0
      OnExecute = actAddExecute
    end
    object actEdit: TAction
      Caption = 'actEdit'
      ImageIndex = 4
      OnExecute = actEditExecute
      OnUpdate = actEditUpdate
    end
    object actDelete: TAction
      Caption = 'actDelete'
      ImageIndex = 1
      OnExecute = actDeleteExecute
      OnUpdate = actDeleteUpdate
    end
    object actMoveUp: TAction
      Caption = 'actMoveUp'
      ImageIndex = 2
      OnExecute = actMoveUpExecute
      OnUpdate = actMoveUpUpdate
    end
    object actMoveDown: TAction
      Caption = 'actMoveDown'
      ImageIndex = 3
      OnExecute = actMoveDownExecute
      OnUpdate = actMoveDownUpdate
    end
  end
end
