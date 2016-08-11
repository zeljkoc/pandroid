{$R-} {- Range-Checking }
{$H+} {- Use long strings by default }
{$B-} {- Complete Boolean Evaluation }
{$T-} {- Typed @ operator }
{$P+} {- Open string params }

{$IFNDEF FPC}
  {$V-} {- Var-String Checking }
  {$X+} {- Extended syntax }
  {$J+} {- Writeable structured consts }
{$ENDIF}

{$IFDEF VER130}      // Delphi 5.0
  {$DEFINE DELPHI_5UP}
{$ENDIF}

{$IFDEF VER140}      // Delphi 6.0
  {$DEFINE DELPHI_5UP}
  {$DEFINE DELPHI_6UP}
  {$WARN SYMBOL_PLATFORM OFF}
  {$WARN UNIT_PLATFORM OFF}
{$ENDIF}

{$IFDEF VER150}      // Delphi 7.0
  {$DEFINE DELPHI_5UP}
  {$DEFINE DELPHI_6UP}
  {$DEFINE DELPHI_7UP}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CAST OFF}
  {$WARN SYMBOL_PLATFORM OFF}
  {$WARN UNIT_PLATFORM OFF}
{$ENDIF}

{$IFDEF VER170}      // Delphi 9.0 / 2005
  {$DEFINE E2356ERROR}
  {$DEFINE Delphi_5UP}
  {$DEFINE Delphi_6UP}
  {$DEFINE Delphi_7UP}
  {$DEFINE Delphi_9UP}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CAST OFF}
  {$WARN SYMBOL_PLATFORM OFF}
  {$WARN UNIT_PLATFORM OFF}
  {$WARN SYMBOL_DEPRECATED OFF}
{$ENDIF}

{$IFDEF VER180}      // Delphi 10.0 / 2006
  {$DEFINE Delphi_5UP}
  {$DEFINE Delphi_6UP}
  {$DEFINE Delphi_7UP}
  {$DEFINE Delphi_9UP}
  {$DEFINE Delphi_10UP}
  {$DEFINE RICHBCB}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CAST OFF}
  {$WARN SYMBOL_PLATFORM OFF}
  {$WARN UNIT_PLATFORM OFF}
  {$WARN SYMBOL_DEPRECATED OFF}
{$ENDIF}

{$IFDEF VER185}      // Delphi 11.0 / 2007 (Spacely)
  {$DEFINE Delphi_5UP}
  {$DEFINE Delphi_6UP}
  {$DEFINE Delphi_7UP}
  {$DEFINE Delphi_9UP}
  {$DEFINE Delphi_10UP}
  {$DEFINE Delphi_11UP}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CAST OFF}
  {$WARN SYMBOL_DEPRECATED OFF}
{$ENDIF}

{$IFDEF VER190}      // Delphi 11.0 (Highlander)
  {$DEFINE Delphi_5UP}
  {$DEFINE Delphi_6UP}
  {$DEFINE Delphi_7UP}
  {$DEFINE Delphi_9UP}
  {$DEFINE Delphi_10UP}
  {$DEFINE Delphi_11UP}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CAST OFF}
  {$WARN SYMBOL_DEPRECATED OFF}
{$ENDIF}

{$IFDEF VER200}      // Delphi 12.0 / 2009 (Tiburon)
  {$DEFINE Delphi_5UP}
  {$DEFINE Delphi_6UP}
  {$DEFINE Delphi_7UP}
  {$DEFINE Delphi_9UP}
  {$DEFINE Delphi_10UP}
  {$DEFINE Delphi_11UP}
  {$DEFINE Delphi_12UP}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CAST OFF}
  {$WARN SYMBOL_DEPRECATED OFF}
  {$WARN SYMBOL_PLATFORM OFF}
  {$WARN UNIT_PLATFORM OFF}
  {$WARN UNIT_DEPRECATED OFF}
  {$WARN IMPLICIT_STRING_CAST OFF}
{$ENDIF}

{$IFDEF VER210}      // Delphi 13.0 / 2010 (Weaver)
  {$DEFINE Delphi_5UP}
  {$DEFINE Delphi_6UP}
  {$DEFINE Delphi_7UP}
  {$DEFINE Delphi_9UP}
  {$DEFINE Delphi_10UP}
  {$DEFINE Delphi_11UP}
  {$DEFINE Delphi_12UP}
  {$DEFINE Delphi_14UP}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CAST OFF}
  {$WARN SYMBOL_DEPRECATED OFF}
  {$WARN SYMBOL_PLATFORM OFF}
  {$WARN UNIT_PLATFORM OFF}
  {$WARN UNIT_DEPRECATED OFF}
  {$WARN IMPLICIT_STRING_CAST OFF}
{$ENDIF}

{$IFDEF VER220}      // Delphi XE
  {$DEFINE Delphi_5UP}
  {$DEFINE Delphi_6UP}
  {$DEFINE Delphi_7UP}
  {$DEFINE Delphi_9UP}
  {$DEFINE Delphi_10UP}
  {$DEFINE Delphi_11UP}
  {$DEFINE Delphi_12UP}
  {$DEFINE Delphi_14UP}
  {$DEFINE Delphi_15UP}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CAST OFF}
  {$WARN SYMBOL_DEPRECATED OFF}
  {$WARN SYMBOL_PLATFORM OFF}
  {$WARN UNIT_PLATFORM OFF}
  {$WARN UNIT_DEPRECATED OFF}
  {$WARN IMPLICIT_STRING_CAST OFF}
{$ENDIF}

{$IFDEF VER230}      // Delphi XE 2
  {$DEFINE Delphi_5UP}
  {$DEFINE Delphi_6UP}
  {$DEFINE Delphi_7UP}
  {$DEFINE Delphi_9UP}
  {$DEFINE Delphi_10UP}
  {$DEFINE Delphi_11UP}
  {$DEFINE Delphi_12UP}
  {$DEFINE Delphi_14UP}
  {$DEFINE Delphi_15UP}
  {$DEFINE Delphi_16UP}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CAST OFF}
  {$WARN SYMBOL_DEPRECATED OFF}
  {$WARN SYMBOL_PLATFORM OFF}
  {$WARN UNIT_PLATFORM OFF}
  {$WARN UNIT_DEPRECATED OFF}
  {$WARN IMPLICIT_STRING_CAST OFF}
{$ENDIF}

{$IFDEF VER240}      // Delphi XE 3
  {$DEFINE Delphi_5UP}
  {$DEFINE Delphi_6UP}
  {$DEFINE Delphi_7UP}
  {$DEFINE Delphi_9UP}
  {$DEFINE Delphi_10UP}
  {$DEFINE Delphi_11UP}
  {$DEFINE Delphi_12UP}
  {$DEFINE Delphi_14UP}
  {$DEFINE Delphi_15UP}
  {$DEFINE Delphi_16UP}
  {$DEFINE Delphi_17UP}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CAST OFF}
  {$WARN SYMBOL_DEPRECATED OFF}
  {$WARN SYMBOL_PLATFORM OFF}
  {$WARN UNIT_PLATFORM OFF}
  {$WARN UNIT_DEPRECATED OFF}
  {$WARN IMPLICIT_STRING_CAST OFF}
  {$IFDEF CPUX64}
    {$DEFINE USE_NATIVE_ZLIB}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER250}      // Delphi XE 4
  {$DEFINE Delphi_5UP}
  {$DEFINE Delphi_6UP}
  {$DEFINE Delphi_7UP}
  {$DEFINE Delphi_9UP}
  {$DEFINE Delphi_10UP}
  {$DEFINE Delphi_11UP}
  {$DEFINE Delphi_12UP}
  {$DEFINE Delphi_14UP}
  {$DEFINE Delphi_15UP}
  {$DEFINE Delphi_16UP}
  {$DEFINE Delphi_17UP}
  {$DEFINE Delphi_18UP}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CAST OFF}
  {$WARN SYMBOL_DEPRECATED OFF}
  {$WARN SYMBOL_PLATFORM OFF}
  {$WARN UNIT_PLATFORM OFF}
  {$WARN UNIT_DEPRECATED OFF}
  {$WARN IMPLICIT_STRING_CAST OFF}
  {$IFDEF CPUX64}
    {$DEFINE USE_NATIVE_ZLIB}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER260}      // Delphi XE 5
  {$DEFINE Delphi_5UP}
  {$DEFINE Delphi_6UP}
  {$DEFINE Delphi_7UP}
  {$DEFINE Delphi_9UP}
  {$DEFINE Delphi_10UP}
  {$DEFINE Delphi_11UP}
  {$DEFINE Delphi_12UP}
  {$DEFINE Delphi_14UP}
  {$DEFINE Delphi_15UP}
  {$DEFINE Delphi_16UP}
  {$DEFINE Delphi_17UP}
  {$DEFINE Delphi_18UP}
  {$DEFINE Delphi_19UP}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CAST OFF}
  {$WARN SYMBOL_DEPRECATED OFF}
  {$WARN SYMBOL_PLATFORM OFF}
  {$WARN UNIT_PLATFORM OFF}
  {$WARN UNIT_DEPRECATED OFF}
  {$WARN IMPLICIT_STRING_CAST OFF}
  {$IFDEF CPUX64}
    {$DEFINE USE_NATIVE_ZLIB}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER270} // Rad Studio XE 6
  {$DEFINE Delphi_5UP}
  {$DEFINE Delphi_6UP}
  {$DEFINE Delphi_7UP}
  {$DEFINE Delphi_9UP}
  {$DEFINE Delphi_10UP}
  {$DEFINE Delphi_11UP}
  {$DEFINE Delphi_12UP}
  {$DEFINE Delphi_14UP}
  {$DEFINE Delphi_15UP}
  {$DEFINE Delphi_16UP}
  {$DEFINE Delphi_17UP}
  {$DEFINE Delphi_18UP}
  {$DEFINE Delphi_19UP}
  {$DEFINE Delphi_20UP}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CAST OFF}
  {$WARN SYMBOL_DEPRECATED OFF}
  {$WARN SYMBOL_PLATFORM OFF}
  {$WARN UNIT_PLATFORM OFF}
  {$WARN UNIT_DEPRECATED OFF}
  {$WARN IMPLICIT_STRING_CAST OFF}
  {$IFDEF CPUX64}
    {$DEFINE USE_NATIVE_ZLIB}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER280} // Rad Studio XE 7
  {$DEFINE Delphi_5UP}
  {$DEFINE Delphi_6UP}
  {$DEFINE Delphi_7UP}
  {$DEFINE Delphi_9UP}
  {$DEFINE Delphi_10UP}
  {$DEFINE Delphi_11UP}
  {$DEFINE Delphi_12UP}
  {$DEFINE Delphi_14UP}
  {$DEFINE Delphi_15UP}
  {$DEFINE Delphi_16UP}
  {$DEFINE Delphi_17UP}
  {$DEFINE Delphi_18UP}
  {$DEFINE Delphi_19UP}
  {$DEFINE Delphi_20UP}
  {$DEFINE Delphi_21UP}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CAST OFF}
  {$WARN SYMBOL_DEPRECATED OFF}
  {$WARN SYMBOL_PLATFORM OFF}
  {$WARN UNIT_PLATFORM OFF}
  {$WARN UNIT_DEPRECATED OFF}
  {$WARN IMPLICIT_STRING_CAST OFF}
  {$IFDEF CPUX64}
    {$DEFINE USE_NATIVE_ZLIB}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER290} // Rad Studio XE 8
  {$DEFINE Delphi_5UP}
  {$DEFINE Delphi_6UP}
  {$DEFINE Delphi_7UP}
  {$DEFINE Delphi_9UP}
  {$DEFINE Delphi_10UP}
  {$DEFINE Delphi_11UP}
  {$DEFINE Delphi_12UP}
  {$DEFINE Delphi_14UP}
  {$DEFINE Delphi_15UP}
  {$DEFINE Delphi_16UP}
  {$DEFINE Delphi_17UP}
  {$DEFINE Delphi_18UP}
  {$DEFINE Delphi_19UP}
  {$DEFINE Delphi_20UP}
  {$DEFINE Delphi_21UP}
  {$DEFINE Delphi_22UP}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CAST OFF}
  {$WARN SYMBOL_DEPRECATED OFF}
  {$WARN SYMBOL_PLATFORM OFF}
  {$WARN UNIT_PLATFORM OFF}
  {$WARN UNIT_DEPRECATED OFF}
  {$WARN IMPLICIT_STRING_CAST OFF}
  {$IFDEF CPUX64}
    {$DEFINE USE_NATIVE_ZLIB}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER300}
  {$DEFINE Delphi_5UP}
  {$DEFINE Delphi_6UP}
  {$DEFINE Delphi_7UP}
  {$DEFINE Delphi_9UP}
  {$DEFINE Delphi_10UP}
  {$DEFINE Delphi_11UP}
  {$DEFINE Delphi_12UP}
  {$DEFINE Delphi_14UP}
  {$DEFINE Delphi_15UP}
  {$DEFINE Delphi_16UP}
  {$DEFINE Delphi_17UP}
  {$DEFINE Delphi_18UP}
  {$DEFINE Delphi_19UP}
  {$DEFINE Delphi_20UP}
  {$DEFINE Delphi_21UP}
  {$DEFINE Delphi_22UP}
  {$WARN UNSAFE_CODE OFF}
  {$WARN UNSAFE_TYPE OFF}
  {$WARN UNSAFE_CAST OFF}
  {$WARN SYMBOL_DEPRECATED OFF}
  {$WARN SYMBOL_PLATFORM OFF}
  {$WARN UNIT_PLATFORM OFF}
  {$WARN UNIT_DEPRECATED OFF}
  {$WARN IMPLICIT_STRING_CAST OFF}
  {$IFDEF CPUX64}
    {$DEFINE USE_NATIVE_ZLIB}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER130}      // Borland C++ Builder 5.0
  {$IFDEF BCB}
    {$ObjExportAll On}
    {$DEFINE RICHBCB}
  {$ENDIF}
{$ENDIF}

{$IFDEF VER140}      // Borland C++ Builder 6.0
  {$IFDEF BCB}
    {$ObjExportAll On}
    {$DEFINE RICHBCB}
  {$ENDIF}
{$ENDIF}

{$IFDEF FPC} // Free pascal compiler
  {$IFNDEF FPCUNICODE}
    {$MODE DELPHI}
  {$ENDIF}
  {$DEFINE DELPHI_6UP} // in order to use Variants
  {$DEFINE CROSS_COMPILE}
  {$DEFINE USE_THEMES}
  {$DEFINE USE_FORMATSETTINGS}
{$ENDIF}

{$IFDEF DELPHI_7UP}
  {$DEFINE USE_THEMES}
{$ENDIF}

{$IFDEF DELPHI_15UP}
  {$DEFINE USE_FORMATSETTINGS}
{$ENDIF}

// -------------- components include -----------------

//{$DEFINE INCLUDE_FAST_SCRIPT} // fastscript interpreter
//{$DEFINE FASTREPORT4}         // use FastReport4

{$IFDEF FPC} // if it is FreePascal and Lazarus then we must undefine unimplemented yet packages
  {$UNDEF FASTREPORT4}   // FastReport is not implemented for this target
{$ENDIF}

// for internal use of developers
// -- possible demo variants of components
//{$DEFINE TRIAL}
//{$DEFINE FC_BASIC}
// -- debug purposes
//{$DEFINE DEBUG_XAxis}
//{$DEFINE DEBUG_YAxis}
{$IFDEF DEBUG_XAxis}
  {$DEFINE DEBUG_Axis}
{$ENDIF}
{$IFDEF DEBUG_YAxis}
  {$DEFINE DEBUG_Axis}
{$ENDIF}

{$IFDEF DELPHI_6UP}
  {$DEFINE SQL_TYPES_EXTRA0}
  {$IFNDEF FPC}
    {$DEFINE SQL_TYPES_EXTRA1}
  {$ENDIF}
{$ENDIF}

// fr compat:
{$IFDEF DELPHI_6UP}
  {$DEFINE DELPHI6}
{$ENDIF}
{$IFDEF DELPHI_7UP}
  {$DEFINE DELPHI7}
{$ENDIF}
{$IFDEF DELPHI_8UP}
  {$DEFINE DELPHI8}
{$ENDIF}
{$IFDEF DELPHI_9UP}
  {$DEFINE DELPHI9}
{$ENDIF}
{$IFDEF DELPHI_10UP}
  {$DEFINE DELPHI10}
{$ENDIF}
{$IFDEF DELPHI_11UP}
  {$DEFINE DELPHI11}
{$ENDIF}
{$IFDEF DELPHI_12UP}
  {$DEFINE DELPHI12}
{$ENDIF}
{$IFDEF DELPHI_14UP}
  {$DEFINE DELPHI14}
{$ENDIF}
{$IFDEF DELPHI_15UP}
  {$DEFINE DELPHI15}
{$ENDIF}
{$IFDEF DELPHI_16UP}
  {$DEFINE DELPHI16}
{$ENDIF}
{$IFDEF DELPHI_17UP}
  {$DEFINE DELPHI17}
{$ENDIF}
{$IFDEF DELPHI_18UP}
  {$DEFINE DELPHI18}
{$ENDIF}
{$IFDEF DELPHI_19UP}
  {$DEFINE DELPHI19}
{$ENDIF}
{$IFDEF DELPHI_20UP}
  {$DEFINE DELPHI20}
{$ENDIF}
{$IFDEF DELPHI_21UP}
  {$DEFINE DELPHI21}
{$ENDIF}
{$IFDEF DELPHI_22UP}
  {$DEFINE DELPHI22}
{$ENDIF}

//{$define UseAnsiString}
//{$define UseWideString}
//{$define UseUnicodeString}

{$IFDEF UseAnsiString}
  {$UNDEF UseWideString}
  {$UNDEF UseUnicodeString}
{$ENDIF}

{$IFDEF UseWideString}
  {$UNDEF UseUnicodeString}
{$ENDIF}

{$IFDEF FMX}
  {$UNDEF UseAnsiString}
  {$UNDEF UseWideString}
  {$UNDEF UseUnicodeString}
{$ENDIF}

{$ifdef UseWideString}
  {$ifndef Delphi_10UP}
     {$DEFINE UsefcWideStrings}
  {$endif}
{$endif}

// starting with delphi 2006(?) internal representation of widestring fields has
// been changed from PWideString to PWideChar
{$IFNDEF Delphi_10UP}
  {$IFNDEF FPC}
     {$DEFINE WideStringFieldIsPWideString}
  {$ENDIF}
{$ENDIF}
