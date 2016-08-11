object fcxInfoForm: TfcxInfoForm
  Left = 1019
  Top = 118
  BorderStyle = bsToolWindow
  BorderWidth = 11
  Caption = 'Stat'
  ClientHeight = 400
  ClientWidth = 298
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClose = FormClose
  OnCreate = FormCreate
  DesignSize = (
    298
    400)
  PixelsPerInch = 96
  TextHeight = 13
  object FastCubeVer: TLabel
    Left = 0
    Top = 0
    Width = 298
    Height = 19
    Align = alTop
    Caption = 'FastCube Ver.'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
  end
  object GroupBox1: TGroupBox
    Left = 0
    Top = 19
    Width = 298
    Height = 142
    Align = alTop
    Caption = 'SliceGeometry'
    TabOrder = 0
    object lRowDimCount: TLabel
      Left = 11
      Top = 34
      Width = 69
      Height = 13
      Caption = 'lRowDimCount'
    end
    object lColDimCount: TLabel
      Left = 11
      Top = 50
      Width = 63
      Height = 13
      Caption = 'lColDimCount'
    end
    object lFactsCount: TLabel
      Left = 11
      Top = 66
      Width = 57
      Height = 13
      Caption = 'lFactsCount'
    end
    object lFilterCount: TLabel
      Left = 11
      Top = 82
      Width = 55
      Height = 13
      Caption = 'lFilterCount'
    end
    object vRowDimCount: TLabel
      Left = 229
      Top = 34
      Width = 73
      Height = 13
      Caption = 'vRowDimCount'
    end
    object vColDimCount: TLabel
      Left = 229
      Top = 50
      Width = 67
      Height = 13
      Caption = 'vColDimCount'
    end
    object vFactsCount: TLabel
      Left = 229
      Top = 66
      Width = 61
      Height = 13
      Caption = 'vFactsCount'
    end
    object vFilterCount: TLabel
      Left = 229
      Top = 82
      Width = 59
      Height = 13
      Caption = 'vFilterCount'
    end
    object lSourceRowCount: TLabel
      Left = 11
      Top = 19
      Width = 85
      Height = 13
      Caption = 'lSourceRowCount'
    end
    object vSourceRowCount: TLabel
      Left = 229
      Top = 19
      Width = 89
      Height = 13
      Caption = 'vSourceRowCount'
    end
    object lRowCount: TLabel
      Left = 11
      Top = 98
      Width = 52
      Height = 13
      Caption = 'lRowCount'
    end
    object vRowCount: TLabel
      Left = 229
      Top = 98
      Width = 56
      Height = 13
      Caption = 'vRowCount'
    end
    object lColCount: TLabel
      Left = 11
      Top = 114
      Width = 46
      Height = 13
      Caption = 'lColCount'
    end
    object vColCount: TLabel
      Left = 229
      Top = 114
      Width = 50
      Height = 13
      Caption = 'vColCount'
    end
  end
  object Button1: TButton
    Left = 222
    Top = 372
    Width = 75
    Height = 25
    Anchors = [akRight, akBottom]
    Cancel = True
    Caption = 'OK'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object GroupBox2: TGroupBox
    Left = 0
    Top = 161
    Width = 298
    Height = 200
    Align = alTop
    Caption = 'TimeChar'
    TabOrder = 2
    object lDBCloseTime: TLabel
      Left = 11
      Top = 115
      Width = 63
      Height = 13
      Caption = 'lDBCloseTime'
    end
    object lDBGetDataTime: TLabel
      Left = 11
      Top = 83
      Width = 77
      Height = 13
      Caption = 'lDBGetDataTime'
    end
    object lDBOpenTime: TLabel
      Left = 11
      Top = 35
      Width = 63
      Height = 13
      Caption = 'lDBOpenTime'
    end
    object lOpenTime: TLabel
      Left = 11
      Top = 51
      Width = 50
      Height = 13
      Caption = 'lOpenTime'
    end
    object vDBCloseTime: TLabel
      Left = 229
      Top = 115
      Width = 67
      Height = 13
      Caption = 'vDBCloseTime'
    end
    object vDBGetDataTime: TLabel
      Left = 229
      Top = 83
      Width = 81
      Height = 13
      Caption = 'vDBGetDataTime'
    end
    object vDBOpenTime: TLabel
      Left = 229
      Top = 35
      Width = 67
      Height = 13
      Caption = 'vDBOpenTime'
    end
    object vOpenTime: TLabel
      Left = 229
      Top = 51
      Width = 54
      Height = 13
      Caption = 'vOpenTime'
    end
    object lConvertTime: TLabel
      Left = 11
      Top = 99
      Width = 63
      Height = 13
      Caption = 'lConvertTime'
    end
    object vConvertTime: TLabel
      Left = 229
      Top = 99
      Width = 67
      Height = 13
      Caption = 'vConvertTime'
    end
    object lSortTime: TLabel
      Left = 11
      Top = 131
      Width = 44
      Height = 13
      Caption = 'lSortTime'
    end
    object vSortTime: TLabel
      Left = 229
      Top = 131
      Width = 48
      Height = 13
      Caption = 'vSortTime'
    end
    object lFullTime: TLabel
      Left = 11
      Top = 19
      Width = 40
      Height = 13
      Caption = 'lFullTime'
    end
    object vFullTime: TLabel
      Left = 229
      Top = 19
      Width = 44
      Height = 13
      Caption = 'vFullTime'
    end
    object lDBMoveTime: TLabel
      Left = 11
      Top = 67
      Width = 63
      Height = 13
      Caption = 'lDBMoveTime'
    end
    object vDBMoveTime: TLabel
      Left = 229
      Top = 67
      Width = 67
      Height = 13
      Caption = 'vDBMoveTime'
    end
    object lDataBuildTime: TLabel
      Left = 11
      Top = 155
      Width = 69
      Height = 13
      Caption = 'lDataBuildTime'
    end
    object vDataBuildTime: TLabel
      Left = 229
      Top = 155
      Width = 73
      Height = 13
      Caption = 'vDataBuildTime'
    end
  end
end
