//------------------- TeeChart component ----------------------------
//{$DEFINE TeeChartStd}

//------------------- TeeChart Std 7 component ----------------------
// If you have TeeChart Std 7, uncomment the following line:
//{$DEFINE TeeChartStd7}

//------------------- TeeChart Pro 4 component --------------------
// If you have TeeChart Pro 4, uncomment the following line:
//{$DEFINE TeeChart4}

//------------------- TeeChart Pro 5 component --------------------
// If you have TeeChart Pro 5, uncomment the following line:
//{$DEFINE TeeChart5}

//------------------- TeeChart Pro 6 component ----------------------
// If you have TeeChart Pro 6, uncomment the following line:
//{$DEFINE TeeChart6}

//------------------- TeeChart Pro 7 component ----------------------
// If you have TeeChart Pro 7, uncomment the following line:
//{$DEFINE TeeChart7}

//------------------- TeeChart Pro 8 component ----------------------
// If you have TeeChart Pro 8, uncomment the following line:
//{$DEFINE TeeChart8}

//------------------- TeeChart Std 8 component ----------------------
// If you have TeeChart Std 8, uncomment the following line:
//{$DEFINE TeeChartStd8}

//------------------- TeeChart Pro 9(2010) component ----------------------
// If you have TeeChart Pro 9(2010), uncomment the following line:
//{$DEFINE TeeChart9}

//------------------- TeeChart Std 9(2010) component ----------------------
// If you have TeeChart Std 9(2010), uncomment the following line:
//{$DEFINE TeeChartStd9}

// Don't change here ------------------------------------------------
{$IFDEF TeeChartStd7}
  {$UNDEF TeeChartStd}
{$ENDIF}

{$IFDEF TeeChartStd8}
  {$UNDEF TeeChartStd}
{$ENDIF}

{$IFDEF TeeChartStd9}
  {$UNDEF TeeChartStd}
{$ENDIF}

{$IFDEF TeeChart4}
  {$UNDEF TeeChartStd}
  {$DEFINE TeeChartPro}
{$ENDIF}

{$IFDEF TeeChart5}
  {$UNDEF TeeChartStd}
  {$DEFINE TeeChartPro}
{$ENDIF}

{$IFDEF TeeChart6}
  {$UNDEF TeeChartStd}
  {$DEFINE TeeChartPro}
{$ENDIF}

{$IFDEF TeeChart7}
  {$UNDEF TeeChartStd}
  {$DEFINE TeeChartPro}
{$ENDIF}

{$IFDEF TeeChart8}
  {$UNDEF TeeChartStd}
  {$DEFINE TeeChartPro}
{$ENDIF}

{$IFDEF TeeChart9}
  {$UNDEF TeeChartStd}
  {$DEFINE TeeChartPro}
{$ENDIF}
