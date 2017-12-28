unit Sprites;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms,
  LazUTF8,
  BGRABitmapTypes, BGRABitmap,
  GL,
  OGLCScene,
  common,
  Language;

type

TVehicle = class( TSprite )
 NeededYSpeed,
 MaxYSpeed    : single;
end;

TArrayOfVehicle = array of TVehicle;

procedure LoadTextures;

procedure ResetTableDone;
procedure SelectRandomTable;


implementation

procedure LoadTextures;
begin
 // mouse cursor
  FScene.MouseManager.SetCursor( SPRITE_PATH+'MousePointer.png' );

 // warning
 TexWarning := TextureManager.Add(SPRITE_PATH+'Warning.png');

  // Vehicles
   TexTruckWhite := TextureManager.Add(SPRITE_PATH+'Truck.png',96,253);

   TexMoto := TextureManager.Add(SPRITE_PATH+'moto.png',35,75);

   TexCarRed := TextureManager.Add(SPRITE_PATH+'Redcar.png',66,128);

   TexCarYellowRed := TextureManager.Add(SPRITE_PATH+'Yellowcar.png',66,128);

   TexCarBlue := TextureManager.Add(SPRITE_PATH+'Bluecar.png',67,128);

   TexCarWhite := TextureManager.Add(SPRITE_PATH+'Whitecar.png',61,128);

   TexCarBlueSky := TextureManager.Add(SPRITE_PATH+'Blueskycar.png',83,128) ;

   TexOilPuddle := TextureManager.Add(SPRITE_PATH+'OilPuddle.png');

   TexTitleIntro := TextureManager.Add(SPRITE_PATH+'Title.png');

   TexStartRace := TextureManager.Add(SPRITE_PATH+'StartRace.png');

   TexHelicoBody := TextureManager.Add(SPRITE_PATH+'HelicopterBody.png');
   TexHelicoQueue := TextureManager.Add(SPRITE_PATH+'HelicopterQueue.png');
   TexHelicoBigFan := TextureManager.Add(SPRITE_PATH+'HelicopterBigPropellers.png');
   TexHelicoSmallFan := TextureManager.Add(SPRITE_PATH+'HelicopterSmallPropellers.png');

   TexBigTreeTop := TextureManager.Add(SPRITE_PATH+'BigTreeTop.png');
   TexBigTreeMiddle := TextureManager.Add(SPRITE_PATH+'BigTreeMiddle.png');
   TexBigTreeBottom := TextureManager.Add(SPRITE_PATH+'BigTreeBottom.png');

   TexStraw := TextureManager.Add(SPRITE_PATH+'Straw.png');

   TexPlayerCarHeadLight := TextureManager.Add(SPRITE_PATH+'PlayerCarHeadLight.png');

   TexTgvHead  := TextureManager.Add(SPRITE_PATH+'Tgv1.png');
   TexTgvWagon := TextureManager.Add(SPRITE_PATH+'Tgv2.png');
   TexTgvQueue := TextureManager.Add(SPRITE_PATH+'Tgv3.png');

   TexKeysView := TextureManager.Add(SPRITE_PATH+'KeysView.png');
end;

procedure ResetTableDone;
var i: integer;
begin
 for i:=2 to 10 do TableDone[i] := FALSE;
end;

procedure SelectRandomTable;
var i: integer;
begin
 repeat
   i := random( 9 ) + 2;
 until not TableDone[i];

 TableDone[i] := TRUE;
 CurrentTable := i;

 // Create table label info for player
 for i:=0 to 10 do
  begin
   TableLine[i] := TGuiLabel.Create;
   TableLine[i].Font := TableFont ;
   TableLine[i].Caption := inttostr( CurrentTable ) + ' x ' + inttostr( i ) + ' = ' ;
   TableLine[i].SetCoordinate( RoadTileEngine.RightX + 40, 10 + i * 35 );
   FScene.Add( TableLine[i], LayerScore );
  end;
end;



end.

