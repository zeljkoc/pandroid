{******************************************}
{*          Zeljko Cvijanovic             *}
{*       E-mail: cvzeljko@gmail.com       *}
{*       Copyright (R)  2013 - 2019       *}
{******************************************}
unit fGrafikoni;

{$mode objfpc}{$H+}
{$modeswitch unicodestrings}
{$namespace zeljus.com.units.pascal}

interface

//uses androidr15,
{$include ../AndroidVersion.inc}
,AActivity, DB, Dialogs;

type

    { TFormGrafikoni }

    TFormGrafikoni = class(Activity)
      procedure onCreate(savedInstanceState: AOBundle); override;
      procedure onClick(aView: AVView); override;
      //
      function onOptionsItemSelected(Item: AVMenuItem): JBoolean; override;
      //
      procedure ItemClickListener (para1: AWAdapterView; para2: AVView; para3: jint; para4: jlong);
    public
      intent: ACIntent;
      GrafikMeni: TDBGridViewLayout;
      procedure IzborGrafikona(podatak: LongInt);
      procedure ViewLineChart(aCur: ADCursor; aVrsta: byte; aTitle: JLString);
      procedure ViewBarChart(aCur: ADCursor; aVrsta: byte; aTitle: JLString);
      procedure ViewPieChart(aCur: ADCursor; aVrsta: byte; aTitle: JLString);
      function RedColor(aValue: byte): longint;
    end;

    function GrafikMeniView(aContext: ACContext; aField: TFieldDef): AVView;

implementation
uses StdCtrls, Data,  Rjava, chart;

function GrafikMeniView(aContext: ACContext; aField: TFieldDef): AVView;
var
    layout: AWLinearLayout;
begin
  layout:= AWLinearLayout.Create(aContext);
  layout.setGravity(AVGravity.CENTER);

  layout.addView(AWImageView.create(aContext));
  with AWImageView(layout.getChildAt(layout.getChildCount - 1)) do begin
     setImageResource(R.drawable.chart);
  end;

  layout.addView(TTextView.create(aContext));
  with TTextView(layout.getChildAt(layout.getChildCount - 1)) do begin
     Text :=   JLString('  ').concat(aField.Value[1].AsString);
     setTextAppearance(aContext, AR.Innerstyle.TextAppearance_Medium);
  end;

   //linija ispod
   layout.addView(TTextView.create(aContext));
   TTextView(layout.getChildAt(layout.getChildCount - 1)).setBackgroundColor(AGColor.BLUE);
   TTextView(layout.getChildAt(layout.getChildCount - 1)).Height := 1;

  Result := layout;
end;

{ TFormGrafikoni }

procedure TFormGrafikoni.onCreate(savedInstanceState: AOBundle);
var
 layout : AWLinearLayout;
begin
  inherited onCreate(savedInstanceState);
   setTitle(JLString('  Exit'));
   layout:= AWLinearLayout.create(Self);
   layout.setOrientation(AWLinearLayout.VERTICAL);

   getActionBar.setDisplayHomeAsUpEnabled(true);
   getActionBar.setIcon(R.drawable.exit);

    GrafikMeni:= TDBGridViewLayout.create(self, dBase);
    with GrafikMeni do begin                                   //0           1              2           3
       Adapter.CursorDataSet.SQLSelect := JLString('select PDAGrafikID, NazivMeni, IDVrstaGrafik, SQLSelect from PDAGrafik ');
       Adapter.CreateView :=  @GrafikMeniView;
       Adapter.CursorDataSet.TableName := 'PDAGrafik';
       GridView.setNumColumns(1);  // ini file
       ReadOnlyDelete := true;
       ReadOnlyEdit := true;
       onItemClickListener:= @ItemClickListener;
    end;
    layout.addView(GrafikMeni);

  setContentView(layout);
end;

procedure TFormGrafikoni.onClick(aView: AVView);
begin

end;

function TFormGrafikoni.onOptionsItemSelected(Item: AVMenuItem): JBoolean;
begin
  Result := true;
  case item.getItemID of
     AR.Innerid.home: onBackPressed;  // exit na prethodni meni
   else  Result := false;
  end;
end;

procedure TFormGrafikoni.ItemClickListener(para1: AWAdapterView; para2: AVView;
  para3: jint; para4: jlong);
begin
    IzborGrafikona(para3); //     vrsta: (1-Linija 2-Bar 3-Pie)
end;

procedure TFormGrafikoni.IzborGrafikona(podatak: LongInt);
var
 vrsta: integer;
 cur : ADCursor;
begin
   GrafikMeni.Adapter.CursorDataSet.Index := podatak;
   vrsta := GrafikMeni.Adapter.CursorDataSet.Field.Value[2].AsInteger;  //IDVrstaGrafik
   dBase.beginTransactionNonExclusive;
   try
      cur := dBase.rawQuery(GrafikMeni.Adapter.CursorDataSet.Field.Value[3].AsString, nil);
      dBase.setTransactionSuccessful;
   finally
      dBase.endTransaction;
   end;

  // if vrsta = 1 then vrsta := 11;  //testiranje

   if (cur.getCount > 0) then
   case vrsta of
      1, 11 : ViewLineChart(cur, vrsta, GrafikMeni.Adapter.CursorDataSet.Field.Value[1].AsString);   //11 inkrementalni prikaz
      2, 22 : ViewBarChart(cur, vrsta, GrafikMeni.Adapter.CursorDataSet.Field.Value[1].AsString);    //22 inkrementalni prikaz
      3, 33 : ViewPieChart(cur, vrsta, GrafikMeni.Adapter.CursorDataSet.Field.Value[1].AsString);    //33 inkrementalni prikaz
   end else finish;
end;

procedure TFormGrafikoni.ViewLineChart(aCur: ADCursor; aVrsta: byte;
  aTitle: JLString);
var
 Series: array of OAMXYSeries;
 Renderer: array of OARXYSeriesRenderer;
 IncValue: array of Real;
 dataset: OAMXYMultipleSeriesDataset;
 multiRenderer: OARXYMultipleSeriesRenderer;

 i, j: integer;
begin
     // Creating a XYMultipleSeriesRenderer to customize the whole chart
    multiRenderer:= OARXYMultipleSeriesRenderer.create;
    multiRenderer.setXLabels(0);
    multiRenderer.setExternalZoomEnabled(true);

    multiRenderer.setYTitle(aTitle);
    multiRenderer.setZoomButtonsVisible(true);
    multiRenderer.setChartValuesTextSize(20);
    multiRenderer.setZoomEnabled(true, true);
    multiRenderer.setLabelsTextSize(20);
    multiRenderer.setXLabelsAngle(0);
    multiRenderer.setXLabelsColor(AGColor.YELLOW);

    // Creating a dataset to hold each series
    dataset := OAMXYMultipleSeriesDataset.create;

    case aVrsta of
       11, 22 :  SetLength(IncValue, aCur.getColumnCount - 1);
    end;

    SetLength(Series,  aCur.getColumnCount - 1);
    SetLength(Renderer, aCur.getColumnCount - 1);
    for i:=0 to aCur.getColumnCount - 2 do begin      //prvo polje izostavljeno opis
      case aVrsta of
       11, 22 :  IncValue[i] := 0;
      end;

      Series[i]:= OAMXYSeries.create(aCur.getColumnName(i+1));
      dataset.addSeries(Series[i]);
      Renderer[i]:= OARXYSeriesRenderer.create;
      with Renderer[i] do begin
       setColor(RedColor(i));
       setPointStyle(OACPointStyle.fCIRCLE);
       setFillPoints(true);
      // setLineWidth(4);
     //  setChartValuesTextSize(20);
       setDisplayChartValues(true);
      end;
      multiRenderer.addSeriesRenderer(Renderer[i]);
    end;

    //minimalan i maximalan podatak
    aCur.moveToPosition(0);
    multiRenderer.addXTextLabel(0, aCur.getString(0));
    aCur.moveToPosition(aCur.getCount - 1);
    multiRenderer.addXTextLabel(aCur.getCount - 1, aCur.getString(0));
    //----minimalan i maximalan podatak

    for i:= 0 to aCur.getCount - 1 do begin
        aCur.moveToPosition(i);
        for j:= 1 to aCur.getColumnCount - 1 do begin
          case aVrsta of
                1, 2: Series[j-1].add(i,  aCur.getFloat(j));
                11, 22: begin
                        IncValue[j-1] := IncValue[j-1] + aCur.getFloat(j);
                        Series[j-1].add(i, IncValue[j-1]);
                     end;
          end;
        end;
    end;


  case aVrsta of
   1, 11 : intent:= OAChartFactory.getLineChartIntent(getBaseContext, dataset, multiRenderer, aTitle);
   2, 22 : intent:= OAChartFactory.getBarChartIntent (getBaseContext, dataset, multiRenderer, OACBarChart.InnerType.fDEFAULT,  aTitle);
  end;

   startActivity(intent);
end;

procedure TFormGrafikoni.ViewBarChart(aCur: ADCursor; aVrsta: byte;
  aTitle: JLString);
begin
   ViewLineChart(aCur, aVrsta, aTitle);
end;


procedure TFormGrafikoni.ViewPieChart(aCur: ADCursor; aVrsta: byte;
  aTitle: JLString);
var
  data: OAMCategorySeries;
  multiRenderer: OARXYMultipleSeriesRenderer;
  i: integer;
begin
    // Creating a XYMultipleSeriesRenderer to customize the whole chart
    multiRenderer:= OARXYMultipleSeriesRenderer.create;
    multiRenderer.setXLabels(0);
    multiRenderer.setExternalZoomEnabled(true);

    multiRenderer.setYTitle(aTitle);
    multiRenderer.setZoomButtonsVisible(true);
    multiRenderer.setChartValuesTextSize(20);
    multiRenderer.setZoomEnabled(true, true);
    multiRenderer.setLabelsTextSize(20);
    multiRenderer.setXLabelsAngle(0);
    multiRenderer.setXLabelsColor(AGColor.YELLOW);

     aCur.moveToFirst;
     data:= OAMCategorySeries.create(JLString(''));
     for i:= 1 to aCur.getColumnCount - 1 do begin
       data.add(aCur.getColumnName(i), aCur.getFloat(i));
       multiRenderer.addSeriesRenderer(OARXYSeriesRenderer.create);
       with multiRenderer.getSeriesRenderers[i-1] do begin
        setColor(RedColor(i-1));
        setDisplayChartValues(true);
       end;
     end;

   intent:= OAChartFactory.getPieChartIntent(getBaseContext, data, multiRenderer, aTitle);
  startActivity(intent);
end;

function TFormGrafikoni.RedColor(aValue: byte): longint;
begin
   Case aValue of
      10: Result := AGColor.BLACK;
      9: Result := AGColor.DKGRAY;
      8: Result := AGColor.GRAY;
      7: Result := AGColor.LTGRAY;
      6: Result := AGColor.WHITE;
      0: Result := AGColor.RED;
      1: Result := AGColor.GREEN;
      2: Result := AGColor.BLUE;
      3: Result := AGColor.YELLOW;
      4: Result := AGColor.CYAN;
      5: Result := AGColor.MAGENTA;
      else Result := 0;
   end;
end;



end.

