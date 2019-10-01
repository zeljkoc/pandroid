{**********************************************************
Copyright (C) 2012-2016
Zeljko Cvijanovic www.zeljus.com (cvzeljko@gmail.com) &
Miran Horjak usbdoo@gmail.com
***********************************************************} 
{%BuildWorkingDir /usr/local/pandroid/example/DemoChart/android}
{%BuildCommand sh build_debug_apk.sh}
{%BuildScan MAKE-}
unit DemoChart;


{$mode objfpc}{$H+}
{$modeswitch unicodestrings}
{$namespace zeljus.com.demochart}

interface

{$include /usr/local/pandroid/units/AndroidVersion.inc}
, Rjava, AActivity, IniFile;

var
   IniFileName : JLString;
   ini: ZCTIniFile;

   IDRadnoMjesto: Integer; //= '10';
   IDPDA: Integer;    

type
  MainActivity = class(Activity)
  public 
    procedure onCreate(savedInstanceState: AOBundle); override;
    procedure onClick(aView: AVView); override; 
    procedure onClick(dInterface: ACDialogInterface; p1: LongInt); override;
    //popup menu
    function onCreateOptionsMenu(menu: AVMenu): JBoolean; override;
    function onOptionsItemSelected(Item: AVMenuItem): JBoolean; override;
  public
      intent: ACIntent; 
  public
    procedure SetingsIniFile;
    procedure ReadIniFile;    
  end;

implementation

uses AZCForms, chart, Dialogs;

procedure MainActivity.onCreate(savedInstanceState: AOBundle);
var
layout : AZCForm; 
begin
  inherited onCreate(savedInstanceState);
   layout := AZCForm.create(Self, R.drawable.logo, 'Copyright(c) Zeljko Cvijanovic');   
  
  //================= Initialize
  IniFileName := JLString('/data/data/').concat(Self.getPackageName).concat('/assets/inifiles.ini');
  ini:= ZCTIniFile.create(IniFileName);
  SetingsIniFile; 
  //=============================

  layout.addButton(Self, 1, 'Chart');

  layout.addButton(Self, 10, 'IZLAZ');

  setContentView(layout);    
end;

procedure MainActivity.onClick(aView: AVView);
var
 incomeSeries, expenseSeries:  OAMXYSeries;
 i: integer;

 dataset : OAMXYMultipleSeriesDataset;
 incomeRenderer: OARXYSeriesRenderer;
 expenseRenderer: OARXYSeriesRenderer;
 multiRenderer: OARXYMultipleSeriesRenderer;
begin
 case aView.getId of
      1: begin
            incomeSeries  :=  OAMXYSeries.create(JLString('Osa X'));   //podatak 1
            expenseSeries := OAMXYSeries.create(JLString('Osa Y'));    //podatak 2

            for i:=0 to 10 do begin
                 incomeSeries.add(i, i * 2);
                 expenseSeries.add(i, i / 2);
            end;

             // Creating a dataset to hold each series
             dataset := OAMXYMultipleSeriesDataset.create;
             // Adding Income Series to the dataset
             dataset.addSeries(incomeSeries);
             // Adding Expense Series to dataset
              dataset.addSeries(expenseSeries);

               // Creating XYSeriesRenderer to customize incomeSeries
              incomeRenderer:= OARXYSeriesRenderer.create;
              incomeRenderer.setColor(AGColor.YELLOW);
              incomeRenderer.setPointStyle(OACPointStyle.fCIRCLE);
              incomeRenderer.setFillPoints(true);
              incomeRenderer.setLineWidth(2);
              incomeRenderer.setChartValuesTextSize(20);
              incomeRenderer.setDisplayChartValues(true);

              // Creating XYSeriesRenderer to customize expenseSeries
              expenseRenderer:= OARXYSeriesRenderer.create;
              expenseRenderer.setColor(AGColor.WHITE);
              expenseRenderer.setPointStyle(OACPointStyle.fCIRCLE);
              expenseRenderer.setFillPoints(true);
              expenseRenderer.setLineWidth(2);
              expenseRenderer.setChartValuesTextSize(20);
              expenseRenderer.setDisplayChartValues(true);

              // Creating a XYMultipleSeriesRenderer to customize the whole chart
              multiRenderer:= OARXYMultipleSeriesRenderer.create;
              multiRenderer.setXLabels(0);
              multiRenderer.setExternalZoomEnabled(true);
              multiRenderer.setChartTitle(JLString(' Chart X Y'));
              multiRenderer.setXTitle(JLString('Year 2015'));
              multiRenderer.setYTitle(JLString('xxx'));
              multiRenderer.setZoomButtonsVisible(true);
              multiRenderer.setChartValuesTextSize(20);
              multiRenderer.setZoomEnabled(true, true);
              multiRenderer.setLabelsTextSize(20);
              multiRenderer.setXLabelsAngle(0);
              multiRenderer.setXLabelsColor(AGColor.YELLOW);

              multiRenderer.addSeriesRenderer(incomeRenderer);
              multiRenderer.addSeriesRenderer(expenseRenderer);

              //version
              intent:= OAChartFactory.getLineChartIntent(getBaseContext(), dataset, multiRenderer);

              //version
              //intent:= OAChartFactory.getBarChartIntent(getBaseContext(), dataset, multiRenderer, OACBarChart.InnerType.fDEFAULT);
              //intent:= OAChartFactory.getBarChartIntent(getBaseContext(), dataset, multiRenderer, OACBarChart.InnerType.fSTACKED);

              // Start Activity
              startActivity(intent);

      end;
      10: Finish;
 end;
         
end;

procedure MainActivity.onClick(dInterface: ACDialogInterface; p1: LongInt);
begin
  //inherited onClick(dInterface, p1);
  ReadIniFile;
end;

//-------------------------------------
function MainActivity.onCreateOptionsMenu(menu: AVMenu): JBoolean;
var
 MenuItem : AVMenuItem;
 SubMenu : AVSubMenu; 
begin
  inherited onCreateOptionsMenu(menu);

  SubMenu := menu.addSubMenu(0, 0, 0, JLString(string('Meni')));
  SubMenu.add(0, 1, 0, JLString('Edit ini file')).setIcon(R.drawable.ic_next);

  MenuItem := SubMenu.getItem;
  MenuItem.setIcon(r.drawable.ic_menu); 
  MenuItem.setShowAsAction(AVMenuItem.SHOW_AS_ACTION_ALWAYS);   

  Result := true;
end;

function MainActivity.onOptionsItemSelected(Item: AVMenuItem): JBoolean;
begin
   Result := true;
   case item.getItemID of
      1: begin
         With TEditFileDialog.create(Self , IniFileName) do
          show;
      end else Result := false;
   end;
end;

procedure MainActivity.SetingsIniFile;
begin



    ini.setInt(JLString('PDAsetings'), JLString('IDRadnoMjesto'),
       ini.getInt(JLString('PDAsetings'), JLString('IDRadnoMjesto'), 1) );
    ini.setInt(JLString('PDAsetings'), JLString('IDPDA'),
       ini.getInt(JLString('PDAsetings'), JLString('IDPDA'), 1 ));  

   ReadIniFile;
end;

procedure MainActivity.ReadIniFile;
begin


  IDRadnoMjesto := ini.getInt(JLString('PDAsetings'), JLString('IDRadnoMjesto'), 1);
  IDPDA :=  ini.getInt(JLString('PDAsetings'), JLString('IDPDA'), 1);

end;  

end.
