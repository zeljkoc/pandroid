{ $Id: qtwsimglist.pp 41387 2013-05-24 18:30:06Z juha $}
{
 *****************************************************************************
 *                              QtWSImgList.pp                               * 
 *                              --------------                               * 
 *                                                                           *
 *                                                                           *
 *****************************************************************************

 *****************************************************************************
  This file is part of the Lazarus Component Library (LCL)

  See the file COPYING.modifiedLGPL.txt, included in this distribution,
  for details about the license.
 *****************************************************************************
}
unit QtWSImgList;

{$mode objfpc}{$H+}

interface

uses
////////////////////////////////////////////////////
// I M P O R T A N T                                
////////////////////////////////////////////////////
// To get as little as posible circles,
// uncomment only when needed for registration
////////////////////////////////////////////////////
//  ImgList,
////////////////////////////////////////////////////
  WSImgList, WSLCLClasses;

type

  { TQtWSCustomImageList }

  TQtWSCustomImageList = class(TWSCustomImageList)
  published
  end;


implementation

end.
