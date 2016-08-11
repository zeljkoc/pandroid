object fcxpCrossEditorForm: TfcxpCrossEditorForm
  Left = 136
  Top = 118
  Width = 778
  Height = 578
  ActiveControl = OkB
  Caption = 'Cross-tab Editor'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poScreenCenter
  ShowHint = True
  OnCreate = FormCreate
  OnHide = FormHide
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  DesignSize = (
    762
    540)
  PixelsPerInch = 96
  TextHeight = 13
  object CubeL: TLabel
    Left = 8
    Top = 515
    Width = 29
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Cube:'
  end
  object GridL: TLabel
    Left = 200
    Top = 515
    Width = 23
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Grid:'
  end
  object OkB: TButton
    Left = 601
    Top = 511
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object CancelB: TButton
    Left = 681
    Top = 511
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object CubeCB: TComboBox
    Left = 43
    Top = 512
    Width = 125
    Height = 22
    Style = csOwnerDrawFixed
    Anchors = [akLeft, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ItemHeight = 16
    ParentFont = False
    TabOrder = 2
    OnClick = CubeCBClick
    OnDrawItem = CubeCBDrawItem
  end
  object PageControl1: TPageControl
    Left = 0
    Top = 0
    Width = 762
    Height = 505
    ActivePage = StructureSheet
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 3
    object StructureSheet: TTabSheet
      Caption = 'StructureSheet'
      object fcxSliceGrid1: TfcxSliceGrid
        Left = 0
        Top = 22
        Width = 754
        Height = 455
        Version = '2.0.0'
        Align = alClient
        PaintStyle = psFlat
        Styles.CaptionArea.TextColor = clBlack
        Styles.CaptionArea.FillColor = 15466495
        Styles.CaptionArea.GradientColor = clWhite
        Styles.CaptionArea.GradientDirection = tgdHorizontal
        Styles.CaptionArea.Font.Charset = DEFAULT_CHARSET
        Styles.CaptionArea.Font.Color = clWindowText
        Styles.CaptionArea.Font.Height = -11
        Styles.CaptionArea.Font.Name = 'Tahoma'
        Styles.CaptionArea.Font.Style = []
        Styles.HeaderArea.TextColor = clBlack
        Styles.HeaderArea.FillColor = clBtnFace
        Styles.HeaderArea.Font.Charset = DEFAULT_CHARSET
        Styles.HeaderArea.Font.Color = clWindowText
        Styles.HeaderArea.Font.Height = -11
        Styles.HeaderArea.Font.Name = 'Tahoma'
        Styles.HeaderArea.Font.Style = []
        Styles.HeaderCells.TextColor = clBlack
        Styles.HeaderCells.FillColor = clBtnFace
        Styles.HeaderCells.Font.Charset = DEFAULT_CHARSET
        Styles.HeaderCells.Font.Color = clWindowText
        Styles.HeaderCells.Font.Height = -11
        Styles.HeaderCells.Font.Name = 'Tahoma'
        Styles.HeaderCells.Font.Style = []
        Styles.HeaderCellsSelected.TextColor = clBtnText
        Styles.HeaderCellsSelected.FillColor = clBtnShadow
        Styles.HeaderCellsSelected.Font.Charset = DEFAULT_CHARSET
        Styles.HeaderCellsSelected.Font.Color = clWindowText
        Styles.HeaderCellsSelected.Font.Height = -11
        Styles.HeaderCellsSelected.Font.Name = 'Tahoma'
        Styles.HeaderCellsSelected.Font.Style = []
        Styles.DataArea.TextColor = clGray
        Styles.DataArea.FillColor = clWhite
        Styles.DataArea.Font.Charset = DEFAULT_CHARSET
        Styles.DataArea.Font.Color = clWindowText
        Styles.DataArea.Font.Height = -11
        Styles.DataArea.Font.Name = 'Tahoma'
        Styles.DataArea.Font.Style = []
        Styles.DataCells.TextColor = clBlack
        Styles.DataCells.FillColor = clWhite
        Styles.DataCells.Font.Charset = DEFAULT_CHARSET
        Styles.DataCells.Font.Color = clWindowText
        Styles.DataCells.Font.Height = -11
        Styles.DataCells.Font.Name = 'Tahoma'
        Styles.DataCells.Font.Style = []
        Styles.DataCellsSelected.TextColor = clHighlightText
        Styles.DataCellsSelected.FillColor = clHighlight
        Styles.DataCellsSelected.Font.Charset = DEFAULT_CHARSET
        Styles.DataCellsSelected.Font.Color = clWindowText
        Styles.DataCellsSelected.Font.Height = -11
        Styles.DataCellsSelected.Font.Name = 'Tahoma'
        Styles.DataCellsSelected.Font.Style = []
        Styles.StatusArea.TextColor = clBlack
        Styles.StatusArea.FillColor = clBtnFace
        Styles.StatusArea.Font.Charset = DEFAULT_CHARSET
        Styles.StatusArea.Font.Color = clWindowText
        Styles.StatusArea.Font.Height = -11
        Styles.StatusArea.Font.Name = 'Tahoma'
        Styles.StatusArea.Font.Style = []
        Styles.ActiveDimension.TextColor = clCaptionText
        Styles.ActiveDimension.FillColor = clActiveCaption
        Styles.ActiveDimension.GradientColor = clGradientActiveCaption
        Styles.ActiveDimension.GradientDirection = tgdHorizontal
        Styles.ActiveDimension.Font.Charset = DEFAULT_CHARSET
        Styles.ActiveDimension.Font.Color = clWindowText
        Styles.ActiveDimension.Font.Height = -11
        Styles.ActiveDimension.Font.Name = 'Tahoma'
        Styles.ActiveDimension.Font.Style = []
        Styles.InactiveDimension.TextColor = clInactiveCaptionText
        Styles.InactiveDimension.FillColor = clInactiveCaption
        Styles.InactiveDimension.GradientColor = clGradientInactiveCaption
        Styles.InactiveDimension.GradientDirection = tgdHorizontal
        Styles.InactiveDimension.Font.Charset = DEFAULT_CHARSET
        Styles.InactiveDimension.Font.Color = clWindowText
        Styles.InactiveDimension.Font.Height = -11
        Styles.InactiveDimension.Font.Name = 'Tahoma'
        Styles.InactiveDimension.Font.Style = []
        Styles.Measure.TextColor = clCaptionText
        Styles.Measure.FillColor = clGreen
        Styles.Measure.GradientColor = clMoneyGreen
        Styles.Measure.GradientDirection = tgdHorizontal
        Styles.Measure.Font.Charset = DEFAULT_CHARSET
        Styles.Measure.Font.Color = clWindowText
        Styles.Measure.Font.Height = -11
        Styles.Measure.Font.Name = 'Tahoma'
        Styles.Measure.Font.Style = []
        Styles.DataCellsTotals.TextColor = clBlack
        Styles.DataCellsTotals.FillColor = 15466495
        Styles.DataCellsTotals.GradientColor = clWhite
        Styles.DataCellsTotals.Font.Charset = DEFAULT_CHARSET
        Styles.DataCellsTotals.Font.Color = clWindowText
        Styles.DataCellsTotals.Font.Height = -11
        Styles.DataCellsTotals.Font.Name = 'Tahoma'
        Styles.DataCellsTotals.Font.Style = []
        Styles.FieldsItem.TextColor = clCaptionText
        Styles.FieldsItem.FillColor = clMoneyGreen
        Styles.FieldsItem.Font.Charset = DEFAULT_CHARSET
        Styles.FieldsItem.Font.Color = clWindowText
        Styles.FieldsItem.Font.Height = -11
        Styles.FieldsItem.Font.Name = 'Tahoma'
        Styles.FieldsItem.Font.Style = []
        TabStop = True
        TabOrder = 0
        CaptionZone.Visible = True
      end
      object fcxSliceGridToolBar1: TfcxSliceGridToolbar
        Left = 0
        Top = 0
        Width = 754
        Height = 22
        AutoSize = True
        Caption = 'froToolBar1'
        Color = clBtnFace
        EdgeBorders = []
        Flat = True
        ParentColor = False
        TabOrder = 1
        Version = '2.0.0'
        SliceGrid = fcxSliceGrid1
      end
    end
    object OptionsSheet: TTabSheet
      BorderWidth = 7
      Caption = 'OptionsSheet'
      ImageIndex = 1
      DesignSize = (
        740
        463)
      object AutoSizeLBL: TLabel
        Left = 304
        Top = 358
        Width = 42
        Height = 13
        Anchors = [akLeft, akBottom]
        Caption = 'AutoSize'
      end
      object Box: TScrollBox
        Left = 0
        Top = 0
        Width = 740
        Height = 343
        Align = alTop
        BorderStyle = bsNone
        Color = clWindow
        ParentColor = False
        TabOrder = 0
        DesignSize = (
          740
          343)
        object PaintBox: TPaintBox
          Left = 16
          Top = 31
          Width = 708
          Height = 299
          Anchors = [akLeft, akTop, akRight, akBottom]
          OnPaint = PaintBoxPaint
        end
        object ToolBar: TToolBar
          Left = 0
          Top = 0
          Width = 740
          Height = 22
          ButtonHeight = 21
          ButtonWidth = 37
          Indent = 16
          ShowCaptions = True
          TabOrder = 0
          object StyleB: TToolButton
            Left = 16
            Top = 2
            Caption = 'StyleB'
            DropdownMenu = StylePopup
            ImageIndex = 0
          end
        end
      end
      object RepeatColumnCB: TCheckBox
        Left = 0
        Top = 394
        Width = 297
        Height = 17
        Anchors = [akLeft, akBottom]
        Caption = 'RepeatColumnCB'
        TabOrder = 3
        OnClick = CBClick
      end
      object DownAcrossCB: TCheckBox
        Left = 304
        Top = 430
        Width = 237
        Height = 17
        Anchors = [akLeft, akBottom]
        Caption = 'Print down then across'
        TabOrder = 8
        OnClick = CBClick
      end
      object BorderCB: TCheckBox
        Left = 304
        Top = 412
        Width = 237
        Height = 17
        Anchors = [akLeft, akBottom]
        Caption = 'Border around cells'
        TabOrder = 7
        OnClick = CBClick
      end
      object RowHeaderCB: TCheckBox
        Left = 0
        Top = 412
        Width = 297
        Height = 17
        Anchors = [akLeft, akBottom]
        Caption = 'Row header'
        TabOrder = 4
        OnClick = CBClick
      end
      object ColumnHeaderCB: TCheckBox
        Left = 0
        Top = 376
        Width = 297
        Height = 17
        Anchors = [akLeft, akBottom]
        Caption = 'Column header'
        TabOrder = 2
        OnClick = CBClick
      end
      object NamesCB: TCheckBox
        Left = 0
        Top = 358
        Width = 297
        Height = 17
        Anchors = [akLeft, akBottom]
        Caption = 'Show names'
        TabOrder = 1
        OnClick = CBClick
      end
      object AutoSizeCB: TComboBox
        Left = 304
        Top = 376
        Width = 188
        Height = 21
        Style = csDropDownList
        Anchors = [akLeft, akBottom]
        ItemHeight = 0
        TabOrder = 6
        OnChange = CBClick
      end
      object RepeatRowCB: TCheckBox
        Left = 0
        Top = 430
        Width = 297
        Height = 17
        Anchors = [akLeft, akBottom]
        Caption = 'RepeatRowCB'
        TabOrder = 5
        OnClick = CBClick
      end
    end
  end
  object GridCB: TComboBox
    Left = 235
    Top = 512
    Width = 125
    Height = 22
    Style = csOwnerDrawFixed
    Anchors = [akLeft, akBottom]
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ItemHeight = 16
    ParentFont = False
    TabOrder = 4
    OnClick = GridCBClick
    OnDrawItem = GridCBDrawItem
  end
  object StylePopup: TPopupMenu
    AutoHotkeys = maManual
    Left = 76
    Top = 36
    object Sep1: TMenuItem
      Caption = '-'
    end
    object SaveStyleMI: TMenuItem
      Caption = 'Save style...'
      ImageIndex = 0
      OnClick = SaveStyleMIClick
    end
  end
end
