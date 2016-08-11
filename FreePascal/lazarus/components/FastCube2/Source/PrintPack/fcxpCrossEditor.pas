{******************************************}
{                                          }
{             FastReport v4.0              }
{          FastCube 2 Cross editor         }
{                                          }
{         Copyright (c) 1998-2014          }
{         by Alexander Tzyganenko,         }
{            Fast Reports Inc.             }
{                                          }
{******************************************}

unit fcxpCrossEditor;

interface

{$I frx.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, Menus, ComCtrls, Buttons, ExtCtrls,
  fcxpCross, frxClass, frxCustomEditors, fcxSliceGrid, fcxTypes, fcxCube,
  fcxSlice, fcxSliceGridToolBar, fcxCustomGrid
{$IFDEF Delphi6}
, Variants
{$ENDIF};

type
  TfrxCrossEditor = class(TfrxViewEditor)
  public
    function Edit: Boolean; override;
    function HasEditor: Boolean; override;
  end;

  TfcxpCrossEditorForm = class(TForm)
    OkB: TButton;
    CancelB: TButton;
    StylePopup: TPopupMenu;
    Sep1: TMenuItem;
    SaveStyleMI: TMenuItem;
    CubeCB: TComboBox;
    CubeL: TLabel;
    PageControl1: TPageControl;
    StructureSheet: TTabSheet;
    OptionsSheet: TTabSheet;
    fcxSliceGrid1: TfcxSliceGrid;
    Box: TScrollBox;
    PaintBox: TPaintBox;
    ToolBar: TToolBar;
    StyleB: TToolButton;
    RepeatColumnCB: TCheckBox;
    DownAcrossCB: TCheckBox;
    BorderCB: TCheckBox;
    RowHeaderCB: TCheckBox;
    ColumnHeaderCB: TCheckBox;
    NamesCB: TCheckBox;
    fcxSliceGridToolBar1: TfcxSliceGridToolBar;
    AutoSizeLBL: TLabel;
    AutoSizeCB: TComboBox;
    RepeatRowCB: TCheckBox;
    GridL: TLabel;
    GridCB: TComboBox;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormHide(Sender: TObject);
    procedure CubeCBClick(Sender: TObject);
    procedure CubeCBDrawItem(Control: TWinControl; Index: Integer;
      ARect: TRect; State: TOwnerDrawState);
    procedure GridCBClick(Sender: TObject);
    procedure GridCBDrawItem(Control: TWinControl; Index: Integer; ARect: TRect;
        State: TOwnerDrawState);
    procedure LBDragOver(Sender, Source: TObject; X, Y: Integer;
      State: TDragState; var Accept: Boolean);
    procedure CBClick(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word;
      Shift: TShiftState);
    procedure PaintBoxPaint(Sender: TObject);
    procedure SaveStyleMIClick(Sender: TObject);
    procedure fcxSliceGrid1Change(Sender: TObject);
  private
    FCross: TfcxpCrossView;
    FOldSlice: TMemoryStream;
    FImages: TImageList;
    FStyleSheet: TfrxStyleSheet;
    FUpdating: Boolean;
    procedure ReflectChanges(ChangesFrom: TObject);
    procedure CreateStyleMenu;
    procedure StyleClick(Sender: TObject);
    procedure SetCross(const Value: TfcxpCrossView);
    procedure FillGridItems;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Cross: TfcxpCrossView read FCross write SetCross;
  end;


implementation

{$R *.DFM}

uses
  frxDsgnIntf,
  frxEditFormat,
  frxEditHighlight,
  frxEditMemo,
  frxEditFrame,
  frxDesgnCtrls,
  frxRes,
  fcxRes,
  fcxGraphicRes,
  frxUtils,
  fcxpComponents;

type
  THackSlice = class(TfcxSlice);

const
  AutoSizeCaptions: array[TfcxpCrossAutoSizeStyle] of String =
  (
{ssDefault             } 'sAutoSize_Default',
{ssBySlice             } 'sAutoSize_BySlice',
{ssAutoColWidth        } 'sAutoSize_ColWidth',
{ssAutoColWidthRestrict} 'sAutoSize_ColWidthRestrict',
{ssAutoRowHeigh        } 'sAutoSize_RowHeight',
{ssByMemoSize          } 'sAutoSize_ByMemoSize'
  );

const
  CrossStyles =
'<?xml version="1.0" encoding="utf-8"?>' +
'<stylesheet>' +
'<style Name="White">' +
'<item Name="cellheader" Color="16777215" Font.Color="0" Font.Style="0" Frame.Typ="15"/>' +
'<item Name="cell" Color="16777215" Font.Color="0" Font.Style="0" Frame.Typ="15"/>' +
'<item Name="column" Color="16777215" Font.Color="0" Font.Style="0" Frame.Typ="15"/>' +
'<item Name="colgrand" Color="16777215" Font.Color="0" Font.Style="1" Frame.Typ="15"/>' +
'<item Name="coltotal" Color="16777215" Font.Color="0" Font.Style="1" Frame.Typ="15"/>' +
'<item Name="row" Color="16777215" Font.Color="0" Font.Style="0" Frame.Typ="15"/>' +
'<item Name="rowgrand" Color="16777215" Font.Color="0" Font.Style="1" Frame.Typ="15"/>' +
'<item Name="rowtotal" Color="16777215" Font.Color="0" Font.Style="1" Frame.Typ="15"/>' +
'<item Name="corner" Color="16777215" Font.Color="0" Font.Style="0" Frame.Typ="15"/>' +
'<item Name="cellrowtotal" Color="16777215" Font.Color="0" Font.Style="1" Frame.Typ="15"/>' +
'<item Name="cellrowgrand" Color="16777215" Font.Color="52376" Font.Style="1" Frame.Typ="15"/>' +
'<item Name="cellcoltotal" Color="16777215" Font.Color="0" Font.Style="1" Frame.Typ="15"/>' +
'<item Name="cellcolgrand" Color="16777215" Font.Color="52376" Font.Style="1" Frame.Typ="15"/>' +
'<item Name="cellfulltotal" Color="16777215" Font.Color="0" Font.Style="1" Frame.Typ="15"/>' +
'<item Name="cellfullgrand" Color="16777215" Font.Color="52376" Font.Style="1" Frame.Typ="15"/>' +
'<item Name="cellrowtotalcolgrand" Color="16777215" Font.Color="52376" Font.Style="1" Frame.Typ="15"/>' +
'<item Name="cellrowgrandcoltotal" Color="16777215" Font.Color="52376" Font.Style="1" Frame.Typ="15"/>' +
'</style>' +
'<style Name="Gray">' +
'<item Name="cellheader" Color="14211288" Font.Color="0" Font.Style="0" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="cell" Color="15790320" Font.Color="0" Font.Style="0" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="column" Color="14211288" Font.Color="0" Font.Style="0" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="colgrand" Color="14211288" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="coltotal" Color="14211288" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="row" Color="14211288" Font.Color="0" Font.Style="0" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="rowgrand" Color="14211288" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="rowtotal" Color="14211288" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="corner" Color="14211288" Font.Color="0" Font.Style="0" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="cellrowtotal" Color="15790320" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="cellrowgrand" Color="15790320" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="cellcoltotal" Color="15790320" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="cellcolgrand" Color="15790320" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="cellfulltotal" Color="15790320" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="cellfullgrand" Color="15790320" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="cellrowtotalcolgrand" Color="15790320" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="cellrowgrandcoltotal" Color="15790320" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'</style>' +
'<style Name="Orange">' +
'<item Name="cellheader" Color="4643583" Font.Color="0" Font.Style="0" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="cell" Color="10218495" Font.Color="0" Font.Style="0" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="column" Color="4643583" Font.Color="0" Font.Style="0" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="colgrand" Color="4643583" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="coltotal" Color="4643583" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="row" Color="4643583" Font.Color="0" Font.Style="0" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="rowgrand" Color="4643583" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="rowtotal" Color="4643583" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="corner" Color="4643583" Font.Color="0" Font.Style="0" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="cellrowtotal" Color="16777215" Font.Color="0" Font.Style="1" Frame.Typ="15"/>' +
'<item Name="cellrowgrand" Color="16777215" Font.Color="0" Font.Style="1" Frame.Typ="15"/>' +
'<item Name="cellcoltotal" Color="16777215" Font.Color="0" Font.Style="1" Frame.Typ="15"/>' +
'<item Name="cellcolgrand" Color="16777215" Font.Color="0" Font.Style="1" Frame.Typ="15"/>' +
'<item Name="cellfulltotal" Color="16777215" Font.Color="0" Font.Style="1" Frame.Typ="15"/>' +
'<item Name="cellfullgrand" Color="16777215" Font.Color="0" Font.Style="1" Frame.Typ="15"/>' +
'<item Name="cellrowtotalcolgrand" Color="16777215" Font.Color="0" Font.Style="1" Frame.Typ="15"/>' +
'<item Name="cellrowgrandcoltotal" Color="16777215" Font.Color="0" Font.Style="1" Frame.Typ="15"/>' +
'</style>' +
'<style Name="Green">' +
'<item Name="cellheader" Color="42107" Font.Color="0" Font.Style="0" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="cell" Color="53918" Font.Color="0" Font.Style="0" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="column" Color="42107" Font.Color="0" Font.Style="0" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="colgrand" Color="42107" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="coltotal" Color="42107" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="row" Color="42107" Font.Color="0" Font.Style="0" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="rowgrand" Color="42107" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="rowtotal" Color="42107" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="corner" Color="42107" Font.Color="0" Font.Style="0" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="cellrowtotal" Color="53918" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="cellrowgrand" Color="53918" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="cellcoltotal" Color="53918" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="cellcolgrand" Color="53918" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="cellfulltotal" Color="53918" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="cellfullgrand" Color="53918" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="cellrowtotalcolgrand" Color="53918" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="cellrowgrandcoltotal" Color="53918" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'</style>' +
'<style Name="Green and Orange">' +
'<item Name="cellheader" Color="52376" Font.Color="0" Font.Style="0" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="cell" Color="52479" Font.Color="0" Font.Style="0" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="column" Color="52376" Font.Color="0" Font.Style="0" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="colgrand" Color="52376" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="coltotal" Color="52376" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="row" Color="52376" Font.Color="0" Font.Style="0" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="rowgrand" Color="52376" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="rowtotal" Color="52376" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="corner" Color="52376" Font.Color="0" Font.Style="0" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="cellrowtotal" Color="52479" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="cellrowgrand" Color="52479" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="cellcoltotal" Color="52479" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="cellcolgrand" Color="52479" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="cellfulltotal" Color="52479" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="cellfullgrand" Color="52479" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="cellrowtotalcolgrand" Color="52479" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="cellrowgrandcoltotal" Color="52479" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'</style>' +
'<style Name="Blue">' +
'<item Name="cellheader" Color="16629143" Font.Color="0" Font.Style="0" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="cell" Color="16700346" Font.Color="0" Font.Style="0" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="column" Color="16629143" Font.Color="0" Font.Style="0" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="colgrand" Color="16629143" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="coltotal" Color="16629143" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="row" Color="16629143" Font.Color="0" Font.Style="0" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="rowgrand" Color="16629143" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="rowtotal" Color="16629143" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="corner" Color="16629143" Font.Color="0" Font.Style="0" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="cellrowtotal" Color="16700346" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="cellrowgrand" Color="16700346" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="cellcoltotal" Color="16700346" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="cellcolgrand" Color="16700346" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="cellfulltotal" Color="16700346" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="cellfullgrand" Color="16700346" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="cellrowtotalcolgrand" Color="16700346" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="cellrowgrandcoltotal" Color="16700346" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'</style>' +
'<style Name="Blue and White">' +
'<item Name="cellheader" Color="16629143" Font.Color="0" Font.Style="0" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="cell" Color="536870911" Font.Color="0" Font.Style="0" Frame.Color="14211288" Frame.Typ="15"/>' +
'<item Name="column" Color="16629143" Font.Color="0" Font.Style="0" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="colgrand" Color="16629143" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="coltotal" Color="16629143" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="row" Color="16629143" Font.Color="0" Font.Style="0" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="rowgrand" Color="16629143" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="rowtotal" Color="16629143" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="corner" Color="16629143" Font.Color="0" Font.Style="0" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="cellrowtotal" Color="536870911" Font.Color="0" Font.Style="1" Frame.Color="14211288" Frame.Typ="15"/>' +
'<item Name="cellrowgrand" Color="536870911" Font.Color="0" Font.Style="1" Frame.Color="14211288" Frame.Typ="15"/>' +
'<item Name="cellcoltotal" Color="536870911" Font.Color="0" Font.Style="1" Frame.Color="14211288" Frame.Typ="15"/>' +
'<item Name="cellcolgrand" Color="536870911" Font.Color="0" Font.Style="1" Frame.Color="14211288" Frame.Typ="15"/>' +
'<item Name="cellfulltotal" Color="536870911" Font.Color="0" Font.Style="1" Frame.Color="14211288" Frame.Typ="15"/>' +
'<item Name="cellfullgrand" Color="536870911" Font.Color="0" Font.Style="1" Frame.Color="14211288" Frame.Typ="15"/>' +
'<item Name="cellrowtotalcolgrand" Color="536870911" Font.Color="0" Font.Style="1" Frame.Color="14211288" Frame.Typ="15"/>' +
'<item Name="cellrowgrandcoltotal" Color="536870911" Font.Color="0" Font.Style="1" Frame.Color="14211288" Frame.Typ="15"/>' +
'</style>' +
'<style Name="Gray and Orange">' +
'<item Name="cellheader" Color="8421504" Font.Color="16777215" Font.Style="0" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="cell" Color="52479" Font.Color="0" Font.Style="0" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="column" Color="8421504" Font.Color="16777215" Font.Style="0" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="coltotal" Color="8421504" Font.Color="16777215" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="colgrand" Color="8421504" Font.Color="16777215" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="row" Color="8421504" Font.Color="16777215" Font.Style="0" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="rowtotal" Color="8421504" Font.Color="16777215" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="rowgrand" Color="8421504" Font.Color="16777215" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="corner" Color="8421504" Font.Color="16777215" Font.Style="0" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="cellrowtotal" Color="52479" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="cellrowgrand" Color="52479" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="cellcoltotal" Color="52479" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="cellcolgrand" Color="52479" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="cellfulltotal" Color="52479" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="cellfullgrand" Color="52479" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="cellrowtotalcolgrand" Color="52479" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="cellrowgrandcoltotal" Color="52479" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'</style>' +
'<style Name="Blue and Orange">' +
'<item Name="cellheader" Color="16629143" Font.Color="0" Font.Style="0" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="cell" Color="7000063" Font.Color="0" Font.Style="0" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="column" Color="16629143" Font.Color="0" Font.Style="0" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="colgrand" Color="16629143" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="coltotal" Color="16629143" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="row" Color="16629143" Font.Color="0" Font.Style="0" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="rowgrand" Color="16629143" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="rowtotal" Color="16629143" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="corner" Color="16629143" Font.Color="0" Font.Style="0" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="cellrowtotal" Color="7000063" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="cellrowgrand" Color="7000063" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="cellcoltotal" Color="7000063" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="cellcolgrand" Color="7000063" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="cellfulltotal" Color="7000063" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="cellfullgrand" Color="7000063" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="cellrowtotalcolgrand" Color="7000063" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="cellrowgrandcoltotal" Color="7000063" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'</style>' +
'<style Name="Orange and White">' +
'<item Name="cellheader" Color="7000063" Font.Color="0" Font.Style="0" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="cell" Color="536870911" Font.Color="0" Font.Style="0" Frame.Color="14211288" Frame.Typ="15"/>' +
'<item Name="column" Color="7000063" Font.Color="0" Font.Style="0" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="colgrand" Color="7000063" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="coltotal" Color="7000063" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="row" Color="7000063" Font.Color="0" Font.Style="0" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="rowgrand" Color="7000063" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="rowtotal" Color="7000063" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="corner" Color="7000063" Font.Color="0" Font.Style="0" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="cellrowtotal" Color="536870911" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="cellrowgrand" Color="536870911" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="cellcoltotal" Color="536870911" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="cellcolgrand" Color="536870911" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="cellfulltotal" Color="536870911" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="cellfullgrand" Color="536870911" Font.Color="0" Font.Style="1" Frame.Color="16777215" Frame.Typ="15"/>' +
'<item Name="cellrowtotalcolgrand" Color="536870911" Font.Color="0" Font.Style="1" Frame.Color="14211288" Frame.Typ="15"/>' +
'<item Name="cellrowgrandcoltotal" Color="536870911" Font.Color="0" Font.Style="1" Frame.Color="14211288" Frame.Typ="15"/>' +
'</style>' +
'</stylesheet>';

type
  THackWinControl = class(TWinControl);


{ TfrxCrossEditor }

function TfrxCrossEditor.Edit: Boolean;
var
  ACrossEditorForm: TfcxpCrossEditorForm;
begin
  ACrossEditorForm := TfcxpCrossEditorForm.Create(Designer);
  try
    ACrossEditorForm.Cross := TfcxpCrossView(Component);
    Result := (ACrossEditorForm.ShowModal = mrOk);
  finally
    ACrossEditorForm.Free;
  end;
end;

function TfrxCrossEditor.HasEditor: Boolean;
begin
  Result := true;
end;


{ TfrxCrossEditorForm }

constructor TfcxpCrossEditorForm.Create(AOwner: TComponent);
var
  TempStream: TStringStream;
begin
  inherited;
  FStyleSheet := TfrxStyleSheet.Create;
  if FileExists(ExtractFilePath(Application.ExeName) + 'crossstyles.xml') then
  begin
    FStyleSheet.LoadFromFile(ExtractFilePath(Application.ExeName) + 'crossstyles.xml');
  end
  else
  begin
    TempStream := TStringStream.Create(CrossStyles);
    FStyleSheet.LoadFromStream(TempStream);
    TempStream.Free;
  end;
  FImages := TImageList.Create(nil);
end;

destructor TfcxpCrossEditorForm.Destroy;
begin
  FImages.Free;
  FStyleSheet.Free;
  FOldSlice.Free;
  inherited;
end;

procedure TfcxpCrossEditorForm.FillGridItems;
var
  I: Integer;
  Field: TfcxSliceField;
begin
  for I := 0 to fcxSliceGrid1.Slice.SliceFieldCount - 1 do
  begin
    Field := fcxSliceGrid1.Slice.SliceField[I];
    if Field.DataType in fcxNumericTypes then // add to measures
      fcxSliceGrid1.Slice.MeasuresContainer.AddMeasure(Field, Field.FieldName, Field.Caption, af_Sum)
    else // add to page
      THackSlice(fcxSliceGrid1.Slice).PageContainer.AddFilterField(Field);
  end;
end;

procedure TfcxpCrossEditorForm.FormCreate(Sender: TObject);
var
  Style: TfcxpCrossAutoSizeStyle;
begin
  Caption := fcxGet(4300);
  CubeL.Caption := 'Cube';//fcGet(4301);
  StructureSheet.Caption := fcxGet(4306);
  OptionsSheet.Caption := fcxGet(3202);
  RowHeaderCB.Caption := fcxGet(4307);
  ColumnHeaderCB.Caption := fcxGet(4308);
  NamesCB.Caption := fcxGet(4315);
  AutoSizeLBL.Caption := fcxGet(4317);
  BorderCB.Caption := fcxGet(4318);
  DownAcrossCB.Caption := fcxGet(4319);
  RepeatColumnCB.Caption := fcxGet(4316);
  RepeatRowCB.Caption := fcxGet(43160);
  RepeatColumnCB.Caption := fcxGet(43161);
  StyleB.Caption := fcxGet(4312);
  SaveStyleMI.Caption := fcxGet(4313);
  OkB.Caption := fcxResources.Get('sOkBtn');
  CancelB.Caption := fcxResources.Get('sCancelBtn');
  // auto size fill
  for Style := Low(TfcxpCrossAutoSizeStyle) to High(TfcxpCrossAutoSizeStyle) do
    AutoSizeCB.Items.Add(fcxResources.Get(AutoSizeCaptions[Style]));
  //
  {$IFNDEF FPC}
  THackWinControl(Box).BevelKind := bkFlat;
  {$ENDIF}
  CreateStyleMenu;
  StylePopup.Images := FImages;
end;

procedure TfcxpCrossEditorForm.FormShow(Sender: TObject);

  procedure SelectCube;
  begin
    CubeCB.ItemIndex := CubeCB.Items.IndexOfObject(FCross.Cube);
    if CubeCB.ItemIndex = -1 then
      CubeCB.ItemIndex := 0;
    CubeCBClick(nil);
  end;
  procedure SelectGrid;
  begin
    GridCB.ItemIndex := GridCB.Items.IndexOfObject(FCross.SliceGridProvider);
    GridCBClick(nil);
  end;

begin
  fcxpGetfcxpCubeList(FCross.Report, CubeCB.Items);
  fcxpGetSliceGridProviderList(GridCB.Items);
  if Cross.SliceGridProvider <> nil then
    SelectGrid
  else
    SelectCube;

  FUpdating := True;
  CubeL.Visible := True;


  NamesCB.Checked := FCross.ShowNames;
  ColumnHeaderCB.Checked := FCross.ShowColumnHeader;
  RowHeaderCB.Checked := FCross.ShowRowHeader;
  AutoSizeCB.ItemIndex := Ord(FCross.PaintSizes.AutoSizeStyle);
  BorderCB.Checked := FCross.Border;
  DownAcrossCB.Checked := FCross.DownThenAcross;
  RepeatRowCB.Checked := FCross.RepeatRowHeaders;
  RepeatColumnCB.Checked := FCross.RepeatColumnHeaders;

  FUpdating := False;
end;

procedure TfcxpCrossEditorForm.FormHide(Sender: TObject);
begin
  if ModalResult = mrCancel then
    if Assigned(FCross.SliceGridProvider) and Assigned(FCross.SliceGridProvider.SliceGrid) and Assigned(TfcxSliceGrid(FCross.SliceGridProvider.SliceGrid).Slice) then
      TfcxSliceGrid(FCross.SliceGridProvider.SliceGrid).Slice.LoadFromStream(FOldSlice)
    else
    if (FCross.SliceGridProvider = nil) then
      FCross.Slice.LoadFromStream(FOldSlice);
end;

procedure TfcxpCrossEditorForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Key = VK_F1 then
    frxResources.Help(Self);
end;

procedure TfcxpCrossEditorForm.CreateStyleMenu;
var
  i: Integer;
  sl: TStringList;
  m: TMenuItem;
  b: TBitmap;
  Style: TfrxStyles;
  StyleItem: TfrxStyleItem;
begin
  sl := TStringList.Create;
  FStyleSheet.GetList(sl);

  FImages.Clear;
  b := TBitmap.Create;
  b.Width := 16;
  b.Height := 16;
  frxResources.MainButtonImages.Draw(b.Canvas, 0, 0, 2);
  FImages.Add(b, nil);

  { create thumbnail images for each style }
  for i := 0 to sl.Count - 1 do
  begin
    Style := FStyleSheet[i];
    with b.Canvas do
    begin
      StyleItem := Style.Find('column');
      if StyleItem <> nil then
      begin
        Brush.Color := StyleItem.Color;
        if Brush.Color = clNone then
          Brush.Color := clWhite;
      end
      else
        Brush.Color := clWhite;
      FillRect(Rect(0, 0, 16, 8));
      StyleItem := Style.Find('cell');
      if StyleItem <> nil then
      begin
        Brush.Color := StyleItem.Color;
        if Brush.Color = clNone then
          Brush.Color := clWhite;
      end
      else
        Brush.Color := clWhite;
      FillRect(Rect(0, 8, 16, 16));
      Pen.Color := clSilver;
      Brush.Style := bsClear;
      Rectangle(0, 0, 16, 16);
    end;
    FImages.Add(b, nil);
  end;
  b.Free;

  while StylePopup.Items[0] <> Sep1 do
    StylePopup.Items[0].Free;

  for i := sl.Count - 1 downto 0 do
  begin
    m := TMenuItem.Create(StylePopup);
    m.Caption := sl[i];
    m.ImageIndex := i + 1;
    m.OnClick := StyleClick;
    StylePopup.Items.Insert(0, m);
  end;

  sl.Free;
end;

procedure TfcxpCrossEditorForm.ReflectChanges(ChangesFrom: TObject);
begin
  // Paint PaintBox if we need this
  PaintBoxPaint(nil);
end;

procedure TfcxpCrossEditorForm.CubeCBClick(Sender: TObject);
var
  Cube: TfcxpCube;
begin
  if CubeCB.ItemIndex = -1 then Exit;
  if GridCB.ItemIndex <> -1 then
  begin
    GridCB.ItemIndex := -1;
    GridCBClick(Self);
  end;
  Cube := TfcxpCube(CubeCB.Items.Objects[CubeCB.ItemIndex]);
  Cross.Cube := Cube;
  if (fcxSliceGrid1.Slice <> Cross.Slice) then
    fcxSliceGrid1.Slice := Cross.Slice;
  if not fcxSliceGrid1.Slice.HaveLayout then
    FillGridItems;

  if Sender <> nil then
    ReflectChanges(nil);
end;

procedure TfcxpCrossEditorForm.LBDragOver(Sender, Source: TObject; X,
  Y: Integer; State: TDragState; var Accept: Boolean);
begin
  Accept := (Source is TListBox) and (TListBox(Source).Items.Count > 0);
end;

procedure TfcxpCrossEditorForm.CBClick(Sender: TObject);
begin
  if FUpdating then Exit;

  FCross.ShowNames := NamesCB.Checked;
  FCross.ShowColumnHeader := ColumnHeaderCB.Checked;
  FCross.ShowRowHeader := RowHeaderCB.Checked;
  FCross.PaintSizes.AutoSizeStyle := TfcxpCrossAutoSizeStyle(AutoSizeCB.ItemIndex);
  FCross.Border := BorderCB.Checked;
  FCross.DownThenAcross := DownAcrossCB.Checked;
  FCross.RepeatRowHeaders := RepeatRowCB.Checked;
  FCross.RepeatColumnHeaders := RepeatColumnCB.Checked;
  ReflectChanges(Sender);
end;

procedure TfcxpCrossEditorForm.StyleClick(Sender: TObject);
var
  Style: TfrxStyles;
begin
  Style := FStyleSheet.Find(TMenuItem(Sender).Caption);
  if Style <> nil then
    FCross.ApplyStyle(Style);
  ReflectChanges(nil);
end;

procedure TfcxpCrossEditorForm.SaveStyleMIClick(Sender: TObject);
var
  s: string;
  Style: TfrxStyles;
begin
  s := '';
  s := InputBox(fcxGet(4313), frxResources.Get('crStName'), s);
  if s <> '' then
  begin
    Style := FStyleSheet.Find(s);
    if Style = nil then
      Style := FStyleSheet.Add;
    Style.Name := s;
    FCross.GetStyle(Style);
    FStyleSheet.SaveToFile(ExtractFilePath(Application.ExeName) + 'crossstyles.xml');
    CreateStyleMenu;
  end;
end;

procedure TfcxpCrossEditorForm.SetCross(const Value: TfcxpCrossView);
begin
  FCross := Value;
  if FCross <> nil then
  begin
    if Assigned(FCross.SliceGridProvider) then
    begin
      if Assigned(FCross.SliceGridProvider.SliceGrid) and Assigned(TfcxSliceGrid(FCross.SliceGridProvider.SliceGrid).Slice) then
      begin
        fcxSliceGrid1.Slice := TfcxSliceGrid(FCross.SliceGridProvider.SliceGrid).Slice;
        FOldSlice.Free;
        FOldSlice := TMemoryStream.Create;
        TfcxSliceGrid(FCross.SliceGridProvider.SliceGrid).Slice.SaveToStream(FOldSlice);
        FOldSlice.Position := 0;
      end
      else
      begin
        fcxSliceGrid1.Slice := nil;
        FreeAndNil(FOldSlice);
      end
    end
    else
    begin
      fcxSliceGrid1.Slice := FCross.Slice;
      FOldSlice.Free;
      FOldSlice := TMemoryStream.Create;
      FCross.Slice.SaveToStream(FOldSlice);
      FOldSlice.Position := 0;
    end;
  end else
  begin
    fcxSliceGrid1.Slice := nil;
    FreeAndNil(FOldSlice);
  end;
end;

procedure TfcxpCrossEditorForm.PaintBoxPaint(Sender: TObject);
begin
  with PaintBox.Canvas do
  begin
    Brush.Color := clWindow;
    FillRect(Rect(0, 0, PaintBox.Width, PaintBox.Height));
  end;
  FCross.DrawCross(PaintBox.Canvas, 1, 1, 0, 0);
end;

procedure TfcxpCrossEditorForm.CubeCBDrawItem(Control: TWinControl;
  Index: Integer; ARect: TRect; State: TOwnerDrawState);
begin
  CubeCB.Canvas.FillRect(ARect);
  fcxGraphicResources.FRImages.Draw(CubeCB.Canvas, ARect.Left, ARect.Top, 1);
  CubeCB.Canvas.TextOut(ARect.Left + 20, ARect.Top + 1, CubeCB.Items[Index]);
end;

procedure TfcxpCrossEditorForm.fcxSliceGrid1Change(Sender: TObject);
begin
  ReflectChanges(Sender);
end;

procedure TfcxpCrossEditorForm.GridCBClick(Sender: TObject);
var
  Provider: TfcxpSliceGridProvider;
begin
  if GridCB.ItemIndex = -1 then Exit;
  if CubeCB.ItemIndex <> -1 then
  begin
    CubeCB.ItemIndex := -1;
    CubeCBClick(Self);
  end;
  Provider := TfcxpSliceGridProvider(GridCB.Items.Objects[GridCB.ItemIndex]);
  Cross.SliceGridProvider := Provider;
  if Assigned(Provider.SliceGrid) and Assigned(TfcxSliceGrid(Provider.SliceGrid).Slice) then
  begin
    if fcxSliceGrid1.Slice <> TfcxSliceGrid(Provider.SliceGrid).Slice then
      fcxSliceGrid1.Slice := TfcxSliceGrid(Provider.SliceGrid).Slice;
  end
  else
    fcxSliceGrid1.Slice := nil;
  if Sender <> nil then
    ReflectChanges(nil);
end;

procedure TfcxpCrossEditorForm.GridCBDrawItem(Control: TWinControl; Index:
    Integer; ARect: TRect; State: TOwnerDrawState);
begin
  GridCB.Canvas.FillRect(ARect);
  fcxGraphicResources.FRImages.Draw(GridCB.Canvas, ARect.Left, ARect.Top, 1);
  GridCB.Canvas.TextOut(ARect.Left + 20, ARect.Top + 1, GridCB.Items[Index]);
end;

initialization
  frxComponentEditors.Register(TfcxpCrossView, TfrxCrossEditor);

end.
