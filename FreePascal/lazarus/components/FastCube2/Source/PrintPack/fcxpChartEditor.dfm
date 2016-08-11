object fcxpChartEditorForm: TfcxpChartEditorForm
  Left = 344
  Top = 215
  Width = 794
  Height = 579
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
  OnClose = FormClose
  OnCreate = FormCreate
  OnHide = FormHide
  OnKeyDown = FormKeyDown
  OnShow = FormShow
  DesignSize = (
    778
    541)
  PixelsPerInch = 96
  TextHeight = 13
  object CubeL: TLabel
    Left = 8
    Top = 516
    Width = 29
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Cube:'
  end
  object ChartL: TLabel
    Left = 192
    Top = 516
    Width = 31
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Chart:'
  end
  object OkB: TButton
    Left = 615
    Top = 512
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 0
  end
  object CancelB: TButton
    Left = 695
    Top = 512
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
    Top = 513
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
    Width = 778
    Height = 505
    ActivePage = StructureSheet
    Align = alTop
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 3
    object StructureSheet: TTabSheet
      Caption = 'StructureSheet'
      object fcxSliceGrid1: TfcxSliceGrid
        Left = 0
        Top = 24
        Width = 770
        Height = 453
        Version = '2.0.0'
        Align = alClient
        PaintStyle = psFlat
        Styles.CaptionArea.TextColor = clBlack
        Styles.CaptionArea.FillColor = 15466495
        Styles.CaptionArea.GradientColor = clWhite
        Styles.CaptionArea.GradientDirection = tgdHorizontal
        Styles.CaptionArea.Font.Charset = RUSSIAN_CHARSET
        Styles.CaptionArea.Font.Color = clWindowText
        Styles.CaptionArea.Font.Height = -11
        Styles.CaptionArea.Font.Name = 'Tahoma'
        Styles.CaptionArea.Font.Style = []
        Styles.HeaderArea.TextColor = clBlack
        Styles.HeaderArea.FillColor = clBtnFace
        Styles.HeaderArea.Font.Charset = RUSSIAN_CHARSET
        Styles.HeaderArea.Font.Color = clWindowText
        Styles.HeaderArea.Font.Height = -11
        Styles.HeaderArea.Font.Name = 'Tahoma'
        Styles.HeaderArea.Font.Style = []
        Styles.HeaderCells.TextColor = clBlack
        Styles.HeaderCells.FillColor = clBtnFace
        Styles.HeaderCells.Font.Charset = RUSSIAN_CHARSET
        Styles.HeaderCells.Font.Color = clWindowText
        Styles.HeaderCells.Font.Height = -11
        Styles.HeaderCells.Font.Name = 'Tahoma'
        Styles.HeaderCells.Font.Style = []
        Styles.HeaderCellsSelected.TextColor = clBtnText
        Styles.HeaderCellsSelected.FillColor = clBtnShadow
        Styles.HeaderCellsSelected.Font.Charset = RUSSIAN_CHARSET
        Styles.HeaderCellsSelected.Font.Color = clWindowText
        Styles.HeaderCellsSelected.Font.Height = -11
        Styles.HeaderCellsSelected.Font.Name = 'Tahoma'
        Styles.HeaderCellsSelected.Font.Style = []
        Styles.DataArea.TextColor = clGray
        Styles.DataArea.FillColor = clWhite
        Styles.DataArea.Font.Charset = RUSSIAN_CHARSET
        Styles.DataArea.Font.Color = clWindowText
        Styles.DataArea.Font.Height = -11
        Styles.DataArea.Font.Name = 'Tahoma'
        Styles.DataArea.Font.Style = []
        Styles.DataCells.TextColor = clBlack
        Styles.DataCells.FillColor = clWhite
        Styles.DataCells.Font.Charset = RUSSIAN_CHARSET
        Styles.DataCells.Font.Color = clWindowText
        Styles.DataCells.Font.Height = -11
        Styles.DataCells.Font.Name = 'Tahoma'
        Styles.DataCells.Font.Style = []
        Styles.DataCellsSelected.TextColor = clHighlightText
        Styles.DataCellsSelected.FillColor = clHighlight
        Styles.DataCellsSelected.Font.Charset = RUSSIAN_CHARSET
        Styles.DataCellsSelected.Font.Color = clWindowText
        Styles.DataCellsSelected.Font.Height = -11
        Styles.DataCellsSelected.Font.Name = 'Tahoma'
        Styles.DataCellsSelected.Font.Style = []
        Styles.StatusArea.TextColor = clBlack
        Styles.StatusArea.FillColor = clBtnFace
        Styles.StatusArea.Font.Charset = RUSSIAN_CHARSET
        Styles.StatusArea.Font.Color = clWindowText
        Styles.StatusArea.Font.Height = -11
        Styles.StatusArea.Font.Name = 'Tahoma'
        Styles.StatusArea.Font.Style = []
        Styles.ActiveDimension.TextColor = clCaptionText
        Styles.ActiveDimension.FillColor = clActiveCaption
        Styles.ActiveDimension.GradientColor = clGradientActiveCaption
        Styles.ActiveDimension.GradientDirection = tgdHorizontal
        Styles.ActiveDimension.Font.Charset = RUSSIAN_CHARSET
        Styles.ActiveDimension.Font.Color = clWindowText
        Styles.ActiveDimension.Font.Height = -11
        Styles.ActiveDimension.Font.Name = 'Tahoma'
        Styles.ActiveDimension.Font.Style = []
        Styles.InactiveDimension.TextColor = clInactiveCaptionText
        Styles.InactiveDimension.FillColor = clInactiveCaption
        Styles.InactiveDimension.GradientColor = clGradientInactiveCaption
        Styles.InactiveDimension.GradientDirection = tgdHorizontal
        Styles.InactiveDimension.Font.Charset = RUSSIAN_CHARSET
        Styles.InactiveDimension.Font.Color = clWindowText
        Styles.InactiveDimension.Font.Height = -11
        Styles.InactiveDimension.Font.Name = 'Tahoma'
        Styles.InactiveDimension.Font.Style = []
        Styles.Measure.TextColor = clCaptionText
        Styles.Measure.FillColor = clGreen
        Styles.Measure.GradientColor = clMoneyGreen
        Styles.Measure.GradientDirection = tgdHorizontal
        Styles.Measure.Font.Charset = RUSSIAN_CHARSET
        Styles.Measure.Font.Color = clWindowText
        Styles.Measure.Font.Height = -11
        Styles.Measure.Font.Name = 'Tahoma'
        Styles.Measure.Font.Style = []
        Styles.DataCellsTotals.TextColor = clBlack
        Styles.DataCellsTotals.FillColor = 15466495
        Styles.DataCellsTotals.GradientColor = clWhite
        Styles.DataCellsTotals.Font.Charset = RUSSIAN_CHARSET
        Styles.DataCellsTotals.Font.Color = clWindowText
        Styles.DataCellsTotals.Font.Height = -11
        Styles.DataCellsTotals.Font.Name = 'Tahoma'
        Styles.DataCellsTotals.Font.Style = []
        Styles.FieldsItem.TextColor = clCaptionText
        Styles.FieldsItem.FillColor = clMoneyGreen
        Styles.FieldsItem.Font.Charset = RUSSIAN_CHARSET
        Styles.FieldsItem.Font.Color = clWindowText
        Styles.FieldsItem.Font.Height = -11
        Styles.FieldsItem.Font.Name = 'Tahoma'
        Styles.FieldsItem.Font.Style = []
        TabStop = True
        TabOrder = 0
        CaptionZone.Visible = True
      end
      object froToolBar1: TfcxSliceGridToolbar
        Left = 0
        Top = 0
        Width = 770
        Height = 24
        AutoSize = True
        Caption = 'froToolBar1'
        EdgeBorders = []
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        Version = '2.0.0'
        SliceGrid = fcxSliceGrid1
      end
    end
    object ChartSheet: TTabSheet
      Caption = 'Chart'
      ImageIndex = 1
      object fcxChart1: TfcxChart
        Left = 0
        Top = 24
        Width = 770
        Height = 466
        Version = '2.0.0'
        Title.Text.Strings = (
          'Chart properties are not correct')
        BackColor = clBtnFace
        Align = alClient
        TabOrder = 0
        Active = True
      end
      object fcxChartToolBar1: TfcxChartToolBar
        Left = 0
        Top = 0
        Width = 770
        Height = 24
        AutoSize = True
        Caption = 'fcChartToolBar1'
        EdgeBorders = []
        ParentShowHint = False
        ShowHint = True
        TabOrder = 1
        Version = '2.0.0'
        Chart = fcxChart1
      end
    end
  end
  object ChartCB: TComboBox
    Left = 227
    Top = 513
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
    OnClick = ChartCBClick
    OnDrawItem = ChartCBDrawItem
  end
end