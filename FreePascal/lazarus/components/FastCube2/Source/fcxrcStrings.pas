{******************************************}
{                                          }
{                FastCube 2                }
{          Language resource file          }
{                                          }
{         Copyright (c) 2001-2014          }
{     by Oleg Pryalkov, Paul Ishenin,      }
{            Fast Reports Inc.             }
{                                          }
{******************************************}

unit fcxrcStrings;

interface
{$H+}
implementation

uses fcxRes;

const resXML: AnsiString =
'<?xml version="1.0" encoding="utf-8"?><Resources CodePage="1252"><StrRes Name="SOkBtn" Text="OK"/><StrRes Name="SCancelBtn" Text="Cancel"/><StrRes Nam' + 
'e="sApplyBtn" Text="Apply"/><StrRes Name="sEditBtn" Text="Edit ..."/><StrRes Name="sAddBtn" Text="Add ..."/><StrRes Name="sDeleteBtn" Text="Delete"/><' + 
'StrRes Name="SDataOverflow" Text="Data overflow error!"/><StrRes Name="sDemoBlocked" Text="This function is blocked in this demo!"/><StrRes Name="sChe' + 
'ckBtn" Text="Check"/><StrRes Name="sNoErrors" Text="Errors not found"/><StrRes Name="sConfirmClearSchema" Text="Do you want to clear grid layout?"/><S' + 
'trRes Name="sNull" Text="Null"/><StrRes Name="sTrue" Text="True"/><StrRes Name="sFalse" Text="False"/><StrRes Name="SDayPrefix" Text="Day_"/><StrRes N' + 
'ame="SMonthPrefix" Text="Month_"/><StrRes Name="SQuarterPrefix" Text="Quarter_"/><StrRes Name="SYearPrefix" Text="Year_"/><StrRes Name="SDayOfWeekPref' + 
'ix" Text="Day_of_week_"/><StrRes Name="SHourPrefix" Text="Hour_"/><StrRes Name="SMinutePrefix" Text="Minute_"/><StrRes Name="SSecondPrefix" Text="Seco' + 
'nd_"/><StrRes Name="SMillisecondPrefix" Text="Millisecond_"/><StrRes Name="sDateAttributeLabelPrefix" Text="Date_"/><StrRes Name="sTimeAttributeLabelP' + 
'refix" Text="Time_"/><StrRes Name="SCounter" Text="(counter)"/><StrRes Name="SMeasures" Text="Measures"/><StrRes Name="SCounterItem" Text="System coun' + 
'ter"/><StrRes Name="sError" Text="??.?? %"/><StrRes Name="sUnknown" Text="???"/><StrRes Name="sBadValue" Text="?error?"/><StrRes Name="sFileLockError"' + 
' Text="Unable to create file %s. Posible it locked by another application."/><StrRes Name="sDataSetUnAssigned" Text="DataSet is not defined!"/><StrRes' + 
' Name="sDataSetOpenError" Text="Error of opening %s!"/><StrRes Name="sCellAlreadyExist" Text="Data cell already exist"/><StrRes Name="sUnknownCubeForm' + 
'at" Text="Unknown cube format!"/><StrRes Name="STotal" Text="Total"/><StrRes Name="sGrandTotal" Text="Grand total"/><StrRes Name="STotalBy" Text="Tota' + 
'l by "/><StrRes Name="SNothing" Text="Value"/><StrRes Name="sSchemaFilter" Text="Cube schema files|*.mds"/><StrRes Name="sCubeFilter" Text="Cube files' + 
'|*.mdc"/><StrRes Name="sChartTemplateFilter" Text="Chart template files|*.mdt"/><StrRes Name="sXLSFilter" Text="Excel files|*.xls"/><StrRes Name="sDOC' + 
'Filter" Text="Word files|*.doc"/><StrRes Name="sHTMLFilter" Text="HTML files|*.html"/><StrRes Name="sXMLFilter" Text="XML files|*.xml"/><StrRes Name="' + 
'sEditMeasure" Text="Edit Measure"/><StrRes Name="sEditMeasures" Text="Edit Measures"/><StrRes Name="sMeasure" Text="Measure"/><StrRes Name="sMeasures"' + 
' Text="Measures"/><StrRes Name="sName" Text="Name"/><StrRes Name="sCaption" Text="Caption"/><StrRes Name="sAggregate" Text="Aggregate:"/><StrRes Name=' + 
'"sUseDifferentAggregateForTotals" Text="Use different aggregate for totals"/><StrRes Name="sCalcTotalsOnTotals" Text="Calculate totals on totals"/><St' + 
'rRes Name="sUseXAxisTotalsAsBase" Text="Use column totals as base"/><StrRes Name="sTotalsConflictResolving" Text="Totals conflict resolution"/><StrRes' + 
' Name="sTotalPosition" Text="Total position"/><StrRes Name="sAdditionalTotals" Text="Additional Totals"/><StrRes Name="sEmptyCell" Text="Empty Cell"/>' + 
'<StrRes Name="sUseXAxisTotal" Text="Use Column Total"/><StrRes Name="sUseYAxisTotal" Text="Use Row Total"/><StrRes Name="sDigitsAfter" Text="Precision' + 
':"/><StrRes Name="sAggAlreadyExist" Text="Aggregate is already exist!"/><StrRes Name="sField" Text="Aggregate of field:"/><StrRes Name="sFormula" Text' + 
'="Calculation formula:"/><StrRes Name="sCalcOrder" Text="Calculation order:"/><StrRes Name="sFilter" Text="Filter:"/><StrRes Name="sEditor" Text="Edit' + 
'or ..."/><StrRes Name="sFuncNotAllWithDate" Text="It is impossible to use this function with Date!"/><StrRes Name="sFuncNotAll" Text="It is impossible' + 
' to use this function with this field!"/><StrRes Name="sFuncNotAllWithCalc" Text="It is impossible to use this function with Calculation Measure!"/><S' + 
'trRes Name="sIncorrectPrecision" Text="Incorrect precision [%d]. Precision should be integer value &#38;#62;= 0! "/><StrRes Name="sIncorrectCalcOrder"' + 
' Text="Incorrect calculation order [%d]. Calculation order should be between %d and %d!"/><StrRes Name="sDisplayFormat" Text="Display Format"/><StrRes' + 
' Name="sClearAll" Text="Clear all"/><StrRes Name="sInverse" Text="Inverse"/><StrRes Name="sFieldFormCaption" Text="Field list"/><StrRes Name="sFieldFo' + 
'rmDragLabel" Text="Move to grid"/><StrRes Name="sFieldformAddBtn" Text="Add to"/><StrRes Name="sFieldFormAreaText" Text="Filters''#$D#$A''Rows''#$D#$A''Co' + 
'lumns''#$D#$A''Measures"/><StrRes Name="sFieldFormRename" Text="Rename"/><StrRes Name="sDefCubeCaption" Text="&#60;no caption&#62;"/><StrRes Name="sFiel' + 
'dLabelChangeCaption" Text="Change field caption"/><StrRes Name="sFieldLabelChangePropmpt" Text="Input new field caption"/><StrRes Name="sCubeChangeCap' + 
'tion" Text="Change cube caption"/><StrRes Name="sCubeChangePropmpt" Text="Input new cube caption"/><StrRes Name="sInfoFormCaption" Text="Cube informat' + 
'ion"/><StrRes Name="sInfoFormSliceGeometry" Text="Slice geometry"/><StrRes Name="sInfoFormSourceRowCount" Text="Source row count"/><StrRes Name="sInfo' + 
'FormRowDimCount" Text="Count of row dimensions"/><StrRes Name="sInfoFormColDimCount" Text="Count of column dimesions"/><StrRes Name="sInfoFormFactsCou' + 
'nt" Text="Count of measures"/><StrRes Name="sInfoFormFilterCount" Text="Count of filter fields"/><StrRes Name="sInfoFormColCount" Text="Column count"/' + 
'><StrRes Name="sInfoFormRowCount" Text="Row count"/><StrRes Name="sInfoFormTimeChar" Text="Time characteristics (sec.)"/><StrRes Name="sInfoFormFullTi' + 
'me" Text="Full loading time"/><StrRes Name="sInfoFormDBOpenTime" Text="Opening data sources"/><StrRes Name="sInfoFormOpenTime" Text="Preparing to load' + 
'ing"/><StrRes Name="sInfoFormDBMoveTime" Text="Data fetch"/><StrRes Name="sInfoFormDBGetDataTime" Text="Getting values"/><StrRes Name="sInfoFormConver' + 
'tTime" Text="Converting values"/><StrRes Name="sInfoFormDBCloseTime" Text="Closing data sources"/><StrRes Name="sInfoFormSortTime" Text="Sorting data"' + 
'/><StrRes Name="sInfoFormDataBuildTime" Text="Calculation measures"/><StrRes Name="sInfoFormTimeSources" Text="Processing sources"/><StrRes Name="sInf' + 
'oFormTimeAxisX" Text="Building X axis"/><StrRes Name="sInfoFormTimeAxisY" Text="Building Y axis"/><StrRes Name="sInfoFormTimeFacts" Text="Building mea' + 
'sures"/><StrRes Name="sInfoFormTimeLinks" Text="Filling links"/><StrRes Name="sInfoFormTimeUniques" Text="Filling unique values"/><StrRes Name="sSave"' + 
' Text="Save"/><StrRes Name="sLoad" Text="Load"/><StrRes Name="sCubeSave" Text="Save cube"/><StrRes Name="sCubeLoad" Text="Load cube"/><StrRes Name="sC' + 
'ubeLoadAdditional" Text="Load additional cube"/><StrRes Name="sClearGrid" Text="Clear grid"/><StrRes Name="sSchemaSave" Text="Save schema"/><StrRes Na' + 
'me="sSchemaLoad" Text="Load schema"/><StrRes Name="sExportTo" Text="Export to ..."/><StrRes Name="sShowHints" Text="Show hints"/><StrRes Name="sHideRo' + 
'wZeros" Text="Hide row zeros"/><StrRes Name="sHideColZeros" Text="Hide column zeros"/><StrRes Name="sRowSort" Text="Row sort type"/><StrRes Name="sCol' + 
'Sort" Text="Column sort type"/><StrRes Name="sDataMarker" Text="Data marker"/><StrRes Name="sIncDecimal" Text="Increase precision"/><StrRes Name="sDec' + 
'Decimal" Text="Decrease precision"/><StrRes Name="sAlignment" Text="Alignment"/><StrRes Name="sRemoveToPage" Text="Move items to page area"/><StrRes N' + 
'ame="sDisplayAs" Text="Display as"/><StrRes Name="sFieldList" Text="Field list"/><StrRes Name="sInformation" Text="Information"/><StrRes Name="sFloatY' + 
'Axis" Text="Y Axis floating"/><StrRes Name="sTranspose" Text="Transpose"/><StrRes Name="sSortByValues" Text="Sort by axis values"/><StrRes Name="sSort' + 
'ByTotals" Text="Sort by measure totals"/><StrRes Name="sSortByXSelection" Text="Sort by focused row"/><StrRes Name="sSortByYSelection" Text="Sort by f' + 
'ocused column"/><StrRes Name="sOriginalValue" Text="Original value"/><StrRes Name="sRowPercentTotal" Text="Total percent by row"/><StrRes Name="sColPe' + 
'rcentTotal" Text="Total percent by column"/><StrRes Name="sRowPercentGroup" Text="Group percent by row"/><StrRes Name="sColPercentGroup" Text="Group p' + 
'ercent by column"/><StrRes Name="sGrandTotalPercent" Text="Grand total percent"/><StrRes Name="sRowRank" Text="Rank by row"/><StrRes Name="sColRank" T' + 
'ext="Rank by column"/><StrRes Name="sSelect" Text="Select"/><StrRes Name="sSelectRow" Text="Select row"/><StrRes Name="sSelectCol" Text="Select column' + 
'"/><StrRes Name="sSelectAll" Text="Select all"/><StrRes Name="sCopy" Text="Copy"/><StrRes Name="sDeleteMeasure" Text="Delete measure"/><StrRes Name="s' + 
'Properties" Text="Properties..."/><StrRes Name="sQuarter_1" Text="Quarter 1"/><StrRes Name="sQuarter_2" Text="Quarter 2"/><StrRes Name="sQuarter_3" Te' + 
'xt="Quarter 3"/><StrRes Name="sQuarter_4" Text="Quarter 4"/><StrRes Name="sSumPrefix" Text="Sum of "/><StrRes Name="sCountPrefix" Text="Count of "/><S' + 
'trRes Name="sMinPrefix" Text="Minimum of "/><StrRes Name="sMaxPrefix" Text="Maximum of "/><StrRes Name="sAvgPrefix" Text="Average of "/><StrRes Name="' + 
'sMulPrefix" Text="Multiplication of "/><StrRes Name="sVariancePrefix" Text="Variance of "/><StrRes Name="sStdDevPrefix" Text="Std Dev of "/><StrRes Na' + 
'me="sVarianceSPrefix" Text="Variance S of "/><StrRes Name="sStdDevSPrefix" Text="Std DevS of "/><StrRes Name="sFuncPrefix" Text="Calculation "/><StrRe' + 
's Name="sCountOfUniquePrefix" Text="Count unique of "/><StrRes Name="sFuncDetPrefix" Text="Calculation (detail) "/><StrRes Name="sMedianPrefix" Text="' + 
'Median "/><StrRes Name="sWeightedMeanPrefix" Text="Weighted arithmetic mean of "/><StrRes Name="sWeightedMeanCaption" Text="Weighted arithmetic mean"/' + 
'><StrRes Name="sFactsRegion" Text="Move measure fields here"/><StrRes Name="sRowsRegion" Text="Move row fields here"/><StrRes Name="sFiltersRegion" Te' + 
'xt="Move filter fields here"/><StrRes Name="sColsRegion" Text="Move column fields here"/><StrRes Name="sServiceRegion" Text="Service"/><StrRes Name="s' + 
'SumCaption" Text="Sum"/><StrRes Name="sCountCaption" Text="Count"/><StrRes Name="sMinCaption" Text="Minimum"/><StrRes Name="sMaxCaption" Text="Maximum' + 
'"/><StrRes Name="sAvgCaption" Text="Average"/><StrRes Name="sMulCaption" Text="Multiplication "/><StrRes Name="sVarianceCaption" Text="Variance"/><Str' + 
'Res Name="sStdDevCaption" Text="Std Dev"/><StrRes Name="sVarianceSCaption" Text="VarianceS"/><StrRes Name="sStdDevSCaption" Text="Std DevS"/><StrRes N' + 
'ame="sFuncCaption" Text="Calculation"/><StrRes Name="sCountOfUniqueCaption" Text="Count unique"/><StrRes Name="sFuncDetCaption" Text="Calculation (det' + 
'ail)"/><StrRes Name="sMedianCaption" Text="Median"/><StrRes Name="sLinkToSelf" Text="Reference to self! Code=%d."/><StrRes Name="sParentNotFound" Text' + 
'="Parent not fount! Code=%d."/><StrRes Name="sLoopLink" Text="Circular reference! Code=%d."/><StrRes Name="sAllValues" Text="All values"/><StrRes Name' + 
'="sErBadLexem" Text="Uncorrert lexema!"/><StrRes Name="sErCommentNotClosed" Text="Unfinished comment!"/><StrRes Name="sErMixDigitChar" Text="Mixing di' + 
'git with char!"/><StrRes Name="sErUnfinishedIdent" Text="Declaration of identifier is not finished"/><StrRes Name="sDetailGridCaption" Text="Cell deta' + 
'ils"/><StrRes Name="sLeftJustify" Text="Left justify"/><StrRes Name="sCenter" Text="Center"/><StrRes Name="sRightJustify" Text="Right justify"/><StrRe' + 
's Name="sPopupHint8" Text="Full expand"/><StrRes Name="sPopupHint9" Text="Full collapse"/><StrRes Name="sFormulaEditor" Text="Formula Editor"/><StrRes' + 
' Name="sScriptErrorsFound" Text="Found errors [%s]. Save anyway?"/><StrRes Name="sChartDataManagerCaption" Text="Data Management"/><StrRes Name="sErro' + 
'rFieldAfterHierarchi" Text="Moving fields behind hierarchi field temporary unavailable!"/><StrRes Name="sChartSaveTemplate" Text="Save chart template"' + 
'/><StrRes Name="sChartOpenTemplate" Text="Open chart template"/><StrRes Name="sChartStyle" Text="Chart style"/><StrRes Name="sChartByRows" Text="By Ro' + 
'ws"/><StrRes Name="sChartByCols" Text="By Columns"/><StrRes Name="sChartMarks" Text="Marks"/><StrRes Name="sChartLegend" Text="Legend"/><StrRes Name="' + 
'sChartDataManager" Text="Data managment"/><StrRes Name="sChartStyleBar" Text="Bar style"/><StrRes Name="sChartStyleLine" Text="Line style"/><StrRes Na' + 
'me="sChartStylePoint" Text="Point style"/><StrRes Name="sChartStyleArea" Text="Area style"/><StrRes Name="sChartStylePie" Text="Pie style"/><StrRes Na' + 
'me="sChartLegendLeft" Text="Left"/><StrRes Name="sChartLegendRight" Text="Right"/><StrRes Name="sChartLegendTop" Text="Top"/><StrRes Name="sChartLegen' + 
'dBottom" Text="Bottom"/><StrRes Name="sAutoSize_Default" Text="Default"/><StrRes Name="sAutoSize_BySlice" Text="By sizes of grid"/><StrRes Name="sAuto' + 
'Size_ColWidth" Text="Auto width"/><StrRes Name="sAutoSize_ColWidthRestrict" Text="Auto width with restriction"/><StrRes Name="sAutoSize_RowHeight" Tex' + 
't="Auto height"/><StrRes Name="sAutoSize_ByMemoSize" Text="By nested memo sizes"/><StrRes Name="propCube" Text="Multidimensional data source"/><StrRes' + 
' Name="propPaintSizes" Text="Size calculation algorithm and values for it"/><StrRes Name="propShowNames" Text="Determines if the cross will show field' + 
' names"/><StrRes Name="propAutoSizeStyle" Text="Size calculation algorithm"/><StrRes Name="propDefaultColWidth" Text="Width used for all cells when Au' + 
'toSizeStyle=ssDefault"/><StrRes Name="propDefaultRowHeight" Text="Height used for all cells when AutoSizeStyle=ssDefault"/><StrRes Name="propMaxColWid' + 
'th" Text="Maximum column width constraint which applied when AutoSizeStyle=ssAutoColWidthRestrict"/><StrRes Name="propFileName" Text="File name of exi' + 
'sted cube"/><StrRes Name="sConvFieldName" Text="Field name"/><StrRes Name="sConvFieldType" Text="Type"/><StrRes Name="sConvFieldSize" Text="Size"/><St' + 
'rRes Name="sConvFieldCaption" Text="Caption"/><StrRes Name="sConvFieldConv" Text="Use converter"/><StrRes Name="sConvFieldNewType" Text="Target type"/' + 
'><StrRes Name="sConvFieldNewSize" Text="Target size"/><StrRes Name="sConvEditField" Text="Edit Field"/><StrRes Name="sConvSaveToFile" Text="Save To Fi' + 
'le"/><StrRes Name="sConvLoadFromFile" Text="Load From File"/><StrRes Name="sConvNoType" Text="Need target type!"/><StrRes Name="sConvNoSize" Text="Tar' + 
'get size must be &#62; 0!"/><StrRes Name="dmSelectExample" Text="Show Example"/><StrRes Name="dmSchema" Text="Schemas"/><StrRes Name="dmFRPreview" Tex' + 
't="Preview in FastReport"/><StrRes Name="dmCrossTbl" Text="Cross table"/><StrRes Name="dmChart" Text="Chart"/><StrRes Name="dmSourceTbl" Text="Sources' + 
' table"/><StrRes Name="dmSelectExample" Text="Select Example"/><StrRes Name="dmConf" Text="democonfen.txt"/><StrRes Name="sPercentFormat" Text="Percen' + 
't Format"/><StrRes Name="sDefaultFormat" Text="Default Format"/><StrRes Name="sChartRealTimeChange" Text="Auto apply changes"/><StrRes Name="sChartDat' + 
'aSource" Text="Chart data source properties"/><StrRes Name="sChartSeriesAxis" Text="Use as series:"/><StrRes Name="sChartValuesAxis" Text="Use as cate' + 
'gories:"/><StrRes Name="sChartSeriesFieldCount" Text="Field count for Series:"/><StrRes Name="sChartValuesFieldCount" Text="Field count for Categories' + 
':"/><StrRes Name="sChartMeasureFieldIndex" Text="Measure index:"/><StrRes Name="sChartFrozen" Text="Frozen chart"/><StrRes Name="sChartDataProperty" T' + 
'ext="Chart properties"/><StrRes Name="sAxisX" Text="Dimensions from columns"/><StrRes Name="sAxisY" Text="Dimensions from rows"/><StrRes Name="sChartB' + 
'yAxisAxis" Text="Series and categories from dimensions"/><StrRes Name="sChartByAxisMeasures" Text="Series from measures, categories from dimensions"/>' + 
'<StrRes Name="sChartByMeasuresAxis" Text="Series from dimensions, categories from measures"/><StrRes Name="sChartPropsAreNotCorrect" Text="Chart prope' + 
'rties are not correct"/><StrRes Name="sScriptErrorFilter" Text="Error during filter calculation:"/><StrRes Name="sScriptErrorCalc" Text="Error during ' + 
'script calculation:"/><StrRes Name="sErrorStr" Text="Error"/><StrRes Name="SWeekNumberPrefix" Text="Week_number"/><StrRes Name="SDayOfYearPrefix" Text' + 
'="Day_of_year_"/><StrRes Name="sFirstValue" Text="First value"/><StrRes Name="sFirstValuePrefix" Text="First value of"/><StrRes Name="sMeasureName" Te' + 
'xt="Measure name"/><StrRes Name="sListOfUniquePrefix" Text="List unique of "/><StrRes Name="sListOfUniqueCaption" Text="List unique"/><StrRes Name="sE' + 
'xportCloneMasterValue" Text="Clone Master Value"/><StrRes Name="sCopyWithHeader" Text="Copy with headers"/><StrRes Name="fkCustom" Text="Custom"/><Str' + 
'Res Name="sChartStyleHorizBar" Text="Horiz Bar style"/><StrRes Name="sChartStackType" Text="Multiple Bar"/><StrRes Name="sChartStackNone" Text="None"/' + 
'><StrRes Name="sChartStackSide" Text="Side"/><StrRes Name="sChartStackStacked" Text="Stacked"/><StrRes Name="sChartStackStacked100" Text="Stacked 100%' + 
'"/><StrRes Name="sChartStackSideAll" Text="Side All"/><StrRes Name="sChartStackSelfStack" Text="Self Stack"/><StrRes Name="sLastValue" Text="Last valu' + 
'e"/><StrRes Name="sLastValuePrefix" Text="Last value of "/><StrRes Name="obfrcCube" Text="FastCube 2 Cube object"/><StrRes Name="obfrcChartView" Text=' + 
'"FastCube 2 Chart object"/><StrRes Name="obfrcCrossView" Text="FastCube 2 Cross-tab object"/><StrRes Name="sMeasureEditor" Text="Measure Editor"/><Str' + 
'Res Name="sGeneral" Text="General"/><StrRes Name="sCaption" Text="Caption"/><StrRes Name="sAggregate" Text="Aggregate"/><StrRes Name="sFieldBased" Tex' + 
't="Field Based"/><StrRes Name="sBaseField" Text="Base Field"/><StrRes Name="sCalculated" Text="Calculated"/><StrRes Name="sFunction" Text="Function"/>' + 
'<StrRes Name="sOrder" Text="Order"/><StrRes Name="sCalcAllCells" Text="Calc all cells"/><StrRes Name="sTotals" Text="Totals"/><StrRes Name="sFiltering' + 
'" Text="Filtering"/><StrRes Name="sStyle" Text="Style"/><StrRes Name="sFillStyle" Text="Fill Style"/><StrRes Name="sFillColor" Text="Fill Color %d"/><' + 
'StrRes Name="sTextStyle" Text="Text Style"/><StrRes Name="sTextColor" Text="Text Color"/><StrRes Name="sRange" Text="Range"/><StrRes Name="sExample" T' + 
'ext="Example"/><StrRes Name="sAdd" Text="Add"/><StrRes Name="sDelete" Text="Delete"/><StrRes Name="sMoveUp" Text="Move Up"/><StrRes Name="sMoveDown" T' + 
'ext="Move Down"/><StrRes Name="sExampleString" Text="Example string"/><StrRes Name="sBold" Text="Bold"/><StrRes Name="sItalic" Text="Italic"/><StrRes ' + 
'Name="sUnderline" Text="Underline"/><StrRes Name="sStrikeOut" Text="Strike Out"/><StrRes Name="sSolidColor" Text="Solid color"/><StrRes Name="sHorzGra' + 
'dient" Text="Horizontal gradient"/><StrRes Name="sVertGradient" Text="Vertical gradient"/><StrRes Name="sHorzCenterGradient" Text="Horz-Center gradien' + 
't"/><StrRes Name="sVertCenterGradient" Text="Vert-Center gradient"/><StrRes Name="sDiag1Gradient" Text="Diagonal gradient 1"/><StrRes Name="sDiag2Grad' + 
'ient" Text="Diagonal gradient 2"/><StrRes Name="sDiag1CenterGradient" Text="Diag-Center gradient 1"/><StrRes Name="sDiag2CenterGradient" Text="Diag-Ce' + 
'nter gradient 2"/><StrRes Name="sCorner1Gradient" Text="Corner gradient 1"/><StrRes Name="sCorner2Gradient" Text="Corner gradient 2"/><StrRes Name="sC' + 
'orner3Gradient" Text="Corner gradient 3"/><StrRes Name="sCorner4Gradient" Text="Corner gradient 4"/><StrRes Name="sCenterGradient" Text="Center gradie' + 
'nt"/><StrRes Name="sConfirmDelete" Text="Confirm deletion"/><StrRes Name="sDimensionEditor" Text="Dimension Editor"/><StrRes Name="sUseTotalPositionFr' + 
'omMeasure" Text="Use total position from measure"/><StrRes Name="sSortDirection" Text="Sort direction"/><StrRes Name="sExpand" Text="Expand"/><StrRes ' + 
'Name="sCollapse" Text="Collapse"/><StrRes Name="sExpandGroup" Text="Expand group"/><StrRes Name="sCollapseGroup" Text="Collapse group"/><StrRes Name="' + 
'sExpandAll" Text="Expand all"/><StrRes Name="sCollapseAll" Text="Collapse all"/><StrRes Name="sMoveToGroup" Text="Move to group"/><StrRes Name="sMoveF' + 
'romGroup" Text="Move from group"/><StrRes Name="sDeleteGroup" Text="Delete group"/><StrRes Name="sRenameGroup" Text="Rename group..."/><StrRes Name="s' + 
'FilterOutThisItem" Text="Filter out this item"/><StrRes Name="sFilterOutAllOtherItems" Text="Filter out all other items"/><StrRes Name="sHideNode" Tex' + 
't="Hide node"/><StrRes Name="sShowHiddenNodes" Text="Show hidden nodes"/><StrRes Name="sShowTopN" Text="Show Top N..."/><StrRes Name="sClearTopN" Text' + 
'="Clear Top N"/><StrRes Name="sActiveTopN" Text="Active Top N filters: %d"/><StrRes Name="sAxisType" Text="Axis type"/><StrRes Name="sBefore" Text="Be' + 
'fore"/><StrRes Name="sAfter" Text="After"/><StrRes Name="sHide" Text="Hide"/><StrRes Name="sFromMeasure" Text="From measure"/><StrRes Name="sStandard"' + 
' Text="Standard"/><StrRes Name="sTreeLike" Text="Tree like"/><StrRes Name="sCreateNew" Text="Create new"/><StrRes Name="sNewGroup" Text="New group"/><' + 
'StrRes Name="sCreateANewGroup" Text="Create a new group"/><StrRes Name="sEnterANewGroupName" Text="Enter a new group name:"/><StrRes Name="sSort" Text' + 
'="Sort"/><StrRes Name="sRename" Text="Rename..."/><StrRes Name="sMoveToPage" Text="Move to page"/><StrRes Name="sClearFilter" Text="Clear filter"/><St' + 
'rRes Name="sSplits" Text="Splits"/><StrRes Name="sCreateNewGroup" Text="Create new group..."/><StrRes Name="sCreateOtherGroup" Text="Create &#60;other' + 
'&#62; group..."/><StrRes Name="sAddNewMeasure" Text="Add new measure..."/><StrRes Name="sOther" Text="Other"/><StrRes Name="sEnterOtherGroupCaption" T' + 
'ext="Enter &#38;#60;other&#38;#62; group caption:"/><StrRes Name="sAscending" Text="Ascending"/><StrRes Name="sDescending" Text="Descending"/><StrRes ' + 
'Name="sDay" Text="Day"/><StrRes Name="sMonth" Text="Month"/><StrRes Name="sQuarter" Text="Quarter"/><StrRes Name="sYear" Text="Year"/><StrRes Name="sD' + 
'ayOfWeek" Text="Day of week"/><StrRes Name="sWeekNumber" Text="Week number"/><StrRes Name="sDayOfYear" Text="Day of year"/><StrRes Name="sHour" Text="' + 
'Hour"/><StrRes Name="sMinute" Text="Minute"/><StrRes Name="sSecond" Text="Second"/><StrRes Name="sMillisecond" Text="Millisecond"/><StrRes Name="sTime' + 
'Splits" Text="Splits of time"/><StrRes Name="sDateSplits" Text="Splits of date"/><StrRes Name="sDateAttribute" Text="Date attribute"/><StrRes Name="sT' + 
'imeAttribute" Text="Time attribute"/><StrRes Name="sCalcForSelection" Text="Calc for selection"/><StrRes Name="sScale" Text="Scale"/><StrRes Name="sSc' + 
'aleSlider" Text="Scale slider"/><StrRes Name="sDimension" Text="Dimension"/><StrRes Name="sShow" Text="Show"/><StrRes Name="sCreateOthers" Text="Creat' + 
'e &#60;others&#62;"/><StrRes Name="sTopMax" Text="most"/><StrRes Name="sTopMin" Text="least"/><StrRes Name="sStylesEditor" Text="Styles Editor"/><StrR' + 
'es Name="sCaptionArea" Text="Caption area"/><StrRes Name="sHeaderArea" Text="Header area"/><StrRes Name="sHeaderCells" Text="Header cells"/><StrRes Na' + 
'me="sHeaderCellsSelected" Text="Header cells (selected)"/><StrRes Name="sDataArea" Text="Data area"/><StrRes Name="sDataCells" Text="Data cells"/><Str' + 
'Res Name="sDataCellsSelected" Text="Data cells (selected)"/><StrRes Name="sActiveDimension" Text="Active dimension"/><StrRes Name="sInactiveDimension"' + 
' Text="Inactive dimension"/><StrRes Name="sDataCellsTotals" Text="Data cells (totals)"/><StrRes Name="sFieldsItem" Text="Fields item"/><StrRes Name="s' + 
'StatusArea" Text="Status area"/><StrRes Name="sHighlightRuleEditor" Text="Highlight rule editor"/><StrRes Name="sRangeHighlightCaption" Text="Highligh' + 
't cells matched condition"/><StrRes Name="sRule" Text="Rule (applies in order)"/><StrRes Name="sCellValueBetween" Text="Cell value between %f and %f"/' + 
'><StrRes Name="sCellValueBeyond" Text="Cell value beyond %f and %f"/><StrRes Name="sCellValueEqual" Text="Cell value = %f"/><StrRes Name="sCellValueNo' + 
'tEqual" Text="Cell value &#60;&#62; %f"/><StrRes Name="sCellValueGreater" Text="Cell value &#62; %f"/><StrRes Name="sCellValueLess" Text="Cell value &' + 
'#60; %f"/><StrRes Name="sCellValueGreaterOrEqual" Text="Cell value &#62;= %f"/><StrRes Name="sCellValueLessOrEqual" Text="Cell value &#60;= %f"/><StrR' + 
'es Name="sCellTextContains" Text="Cell text contains ''%s''"/><StrRes Name="sCellTextNotContains" Text="Cell text not contains ''%s''"/><StrRes Name="sCel' + 
'lTextStartsWith" Text="Cell text starts with ''%s''"/><StrRes Name="sCellTextEndsWith" Text="Cell text ends with ''%s''"/><StrRes Name="sCellValueIsNull" ' + 
'Text="Cell value is null"/><StrRes Name="sCellValueIsNotNull" Text="Cell value is not null"/><StrRes Name="sCellValue" Text="Value"/><StrRes Name="sCe' + 
'llText" Text="Text"/><StrRes Name="sCellDate" Text="Date"/><StrRes Name="sCellNull" Text="Empty"/><StrRes Name="sCellNotNull" Text="Not empty"/><StrRe' + 
's Name="sBetween" Text="between"/><StrRes Name="sBeyond" Text="beyond"/><StrRes Name="sEqual" Text="equal"/><StrRes Name="sNotEqual" Text="not equal"/' + 
'><StrRes Name="sGreater" Text="greater"/><StrRes Name="sLess" Text="less"/><StrRes Name="sGreaterOrEqual" Text="greater or equal"/><StrRes Name="sLess' + 
'OrEqual" Text="less or equal"/><StrRes Name="sContains" Text="contains"/><StrRes Name="sNotContains" Text="not contains"/><StrRes Name="sStartsWith" T' + 
'ext="starts with"/><StrRes Name="sEndsWith" Text="ends with"/><StrRes Name="sAnd" Text="and"/><StrRes Name="sOr" Text="or"/><StrRes Name="sEditStyle" ' + 
'Text="Style..."/><StrRes Name="sStyleEditor" Text="Style editor"/><StrRes Name="sApplyTo" Text="Apply to"/><StrRes Name="sToCells" Text="Cells"/><StrR' + 
'es Name="sToTotals" Text="Totals"/><StrRes Name="sToGrandTotal" Text="Grand Total"/><StrRes Name="sContinuousHighlightCaption" Text="Highlight all cel' + 
'ls dependent on value"/><StrRes Name="sHighlightKind" Text="Highlight type"/><StrRes Name="sTwoColorScale" Text="Two color scale"/><StrRes Name="sThre' + 
'eColorScale" Text="Three color scale"/><StrRes Name="sBarChart" Text="Bar"/><StrRes Name="sIconSet" Text="Icon set"/><StrRes Name="sMinValue" Text="Mi' + 
'nimal value"/><StrRes Name="sMinValueByRow" Text="Minimal value by row"/><StrRes Name="sMinValueByCol" Text="Minimal value by col"/><StrRes Name="sMid' + 
'Value" Text="Average value"/><StrRes Name="sMaxValue" Text="Maximal value"/><StrRes Name="sMaxValueByRow" Text="Maximal value by row"/><StrRes Name="s' + 
'MaxValueByCol" Text="Maximal value by col"/><StrRes Name="sType" Text="Type"/><StrRes Name="sValue" Text="Value"/><StrRes Name="sColor" Text="Color"/>' + 
'<StrRes Name="sNumber" Text="Number"/><StrRes Name="sPercentByRow" Text="Percent by row"/><StrRes Name="sPercentByCol" Text="Percent by col"/><StrRes ' + 
'Name="sPercentileByRow" Text="Percentile by row"/><StrRes Name="sPercentileByCol" Text="Percentile by col"/><StrRes Name="sBarColor" Text="Bar color"/' + 
'><StrRes Name="sTheShortestBar" Text="The shortest bar"/><StrRes Name="sTheLongestBar" Text="The longest bar"/><StrRes Name="sShowCellValue" Text="Sho' + 
'w cell value"/><StrRes Name="sGradientDraw" Text="Draw gradient"/><StrRes Name="sFrameColor" Text="Frame color"/><StrRes Name="sIfValue" Text="if valu' + 
'e"/><StrRes Name="sIfValue1" Text="if %s %g"/><StrRes Name="sReverseOrder" Text="Reverse order"/><StrRes Name="sIcon" Text="Icon"/><StrRes Name="sNoIc' + 
'on" Text="No icon"/><StrRes Name="sCustom" Text="Custom"/><StrRes Name="sString" Text="String"/><StrRes Name="sNumeric" Text="Numeric"/><StrRes Name="' + 
'sDateTime" Text="DateTime"/><StrRes Name="sSkipNullPoints" Text="Skip null points"/><StrRes Name="sBaseAxisDataType" Text="Data type for base axis"/><' + 
'StrRes Name="sRangeEditor" Text="Range editor"/><StrRes Name="sCustomFilter" Text="Custom filter..."/><StrRes Name="sShowAvailable" Text="Show availab' + 
'le"/><StrRes Name="dtMeasures" Text="Measures"/><StrRes Name="dtDimensions" Text="Dimensions"/><StrRes Name="dtSliceFields" Text="SliceFields"/><StrRe' + 
's Name="sAxisProperties" Text="Axis properties..."/><StrRes Name="sAxisEditor" Text="Axis editor"/><StrRes Name="sGrandTotalPosition" Text="Grand tota' + 
'l position"/><StrRes Name="sAdditionalGrandTotals" Text="Additional grand totals"/><StrRes Name="sDistinctField" Text="Distinct Field"/><StrRes Name="' + 
'sDistinct" Text="Distinct"/></Resources>' + 
' ';

initialization
  fcxResources.AddXML(resXML);

end.
