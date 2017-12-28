unit common;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LMessages,
  OGLCScene,
  BGRABitmap, BGRABitmapTypes;


const
// message for main window
 LM_MESSAGE_PRINCIPAL = LM_USER + 1;
  mpSceneStartLoop = 1;
  mpShowHomeScreen = 2;

// LAYER
LAYER_COUNT = 5;
  Layer_Info            = 0;
  Layer_SecondMapTop    = 1;
  Layer_WorkMap         = 2;
  Layer_SecondMapBelow  = 3;
  Layer_Grid            = 4;


var

FScene: TOGLCScene=NIL;

FTileEngine: TTileEngine=NIL;
FTopOverlayTileEngine,
FBelowOverlayTileEngine,
FTileEngineGrid: TTileEngine;

FImageBackGround: TBGRABitmap;


FTitleFont,
FHintFont,
FEventFont: TTexturedFont;

FLabelMapPosition: TGuiLabel;
FLabelTileIndexes: TGuiLabel;
FLabelSelectionInfo: TGuiLabel;
FLabelGroundType,
FLabelEventName: TGuiLabel;
FLabelDebug: TGuiLabel;

FReady: boolean =FALSE;

FProjectIsModified: boolean=FALSE;
procedure SetProjectModified;

implementation
uses Main;

procedure SetProjectModified;
begin
 if not FProjectIsModified then
 begin
   FProjectIsModified := TRUE;
   Form_Principale.Caption := Form_Principale.Caption + ' - modified';
 end;
end;

end.

