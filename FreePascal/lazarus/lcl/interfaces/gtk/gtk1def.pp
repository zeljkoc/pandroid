{ $Id: gtk1def.pp 41387 2013-05-24 18:30:06Z juha $ 
                         -------------------------------
                         gtk1def.pp  -  Type definitions
                         ------------------------------- 
 
 @created(Tue Nov 20st WET 2007)
 @lastmod($Date: 2013-05-24 14:30:06 -0400 (Fri, 24 May 2013) $)
 @author(Marc Weustink <marc@@dommelstein.net>)                       

 This unit contains type definitions needed in the GTK1 <-> LCL interface
 
 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}


unit Gtk1Def;
 
{$mode objfpc} {$H+}

interface

uses
  glib, gdk, gtk, gdkpixbuf, GtkDef;

type

  { TGtk1DeviceContext }

  TGtk1DeviceContext = class(TGtkDeviceContext)
  public
    function GetFunction: TGdkFunction; override;
  end;

implementation

{$i gtk1devicecontext.inc}

end.
