object fcxFieldForm: TfcxFieldForm
  Left = 808
  Top = 355
  Width = 207
  Height = 381
  BorderStyle = bsSizeToolWin
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object FieldTree: TTreeView
    Left = 0
    Top = 17
    Width = 191
    Height = 297
    Align = alClient
    Indent = 19
    PopupMenu = TreeMenu
    ReadOnly = True
    TabOrder = 0
    OnChange = FieldTreeChange
    OnDeletion = FieldTreeDeletion
    OnMouseDown = FieldTreeMouseDown
    OnStartDrag = FieldTreeStartDrag
  end
  object Panel1: TPanel
    Left = 0
    Top = 314
    Width = 191
    Height = 29
    Align = alBottom
    BevelOuter = bvNone
    TabOrder = 1
    DesignSize = (
      191
      29)
    object AddBtn: TButton
      Left = 1
      Top = 2
      Width = 75
      Height = 25
      Anchors = [akLeft, akBottom]
      Default = True
      TabOrder = 0
      OnClick = AddBtnClick
    end
    object AreaList: TComboBox
      Left = 80
      Top = 2
      Width = 111
      Height = 25
      Style = csOwnerDrawFixed
      Anchors = [akLeft, akRight, akBottom]
      ItemHeight = 19
      TabOrder = 1
      OnDrawItem = AreaListDrawItem
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 191
    Height = 17
    Align = alTop
    BevelOuter = bvNone
    TabOrder = 2
  end
  object TreeMenu: TPopupMenu
    OnPopup = TreeMenuPopup
    Left = 84
    Top = 92
    object ItemRename: TMenuItem
      Caption = 'Rename'
      OnClick = ItemRenameClick
    end
  end
end
