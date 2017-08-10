unit common;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,
  LMessages, LCLIntf,
  OGLCScene, OpenALSoundManager,
  GL;

const
 MAP_PATH       = 'DATA'+DirectorySeparator+'Map'+DirectorySeparator;
 AUDIO_PATH     = 'DATA'+DirectorySeparator+'Sounds'+DirectorySeparator;
 SCENARIO_PATH  = 'DATA'+DirectorySeparator+'Scenario'+DirectorySeparator;
 PARTICLES_PATH = 'DATA'+DirectorySeparator+'Particles'+DirectorySeparator;
 SPRITE_PATH    = 'DATA'+DirectorySeparator+'Sprites'+DirectorySeparator;


//
 VSPEED_MAX  =  900;
 SPEED_BONUS = 250;
 FULL_SPEED_DISTANCE_TO_GO = 12000;
 HELICOPTER_RESCUE_COST = -2000;

// Message
LM_MESSAGE_PRINCIPAL = LM_USER + 1;
  Mess_StartScene            = 1 ;
  Mess_Quit                  = 2 ;
  Mess_LoadResources         = 3 ;

// Layer
   NBLayer = 14 ;
   LayerIntro    = 0 ;
   LayerScore    = 1 ;
   LayerCloud    = 2 ;
   LayerRainSnow = 3 ;
   LayerPlayer2  = 4 ;    // layer for player car over second floor (when it jump)
   LayerOverRoad = 5 ;    // Layer for the second floor of the road
   LayerShadows  = 6 ;
   LayerPlayer   = 7 ;    // layer for player car
   LayerNight    = 8 ;
   LayerTrain    = 9 ;
   LayerSprite   = 10 ;    // layer for other sprites and obstacles : car, truck, motorcycle, tree, oil puddle...
   LayerTable    = 11 ;    // for multiplication table number sprites
   LayerRoad     =12 ;    // layer for the road - contain tile engine
   LayerBGAnim   =13 ;    // contain one tile engine for background animation


// Group to differentiate sprites on screen
   GroupButtonFrench            = 10000  ;
   GroupButtonEnglish           = 10001  ;
   GroupButtonItalian           = 10002  ;
   GroupButtonGermany           = 10003  ;
   GroupButtonSpanish           = 10004  ;

   GroupButtonQuitIntro         = 100000 ;
   GroupButtonPlayIntro         = 100001 ;
   GroupButtonInstruction       = 100002 ;
   GroupButtonLanguage          = 100003 ;

   GroupSpriteTruck             = 100    ;
   GroupSpriteWhiteCar          = 101    ;
   GroupSpriteBlueCar           = 102    ;
   GroupSpriteRedCar            = 103    ;
   GroupSpriteYellowRedCar      = 104    ;
   GroupSpriteMoto              = 105    ;

   GroupOilPuddle               = 200    ;
   GroupTableSprite             = 201    ;
   GroupStartRace               = 202    ;
   GroupEndRace                 = 203    ;
   GroupBigTreeBottom           = 204    ;
   GroupBigTreeMiddle           = 205    ;
   GroupBigTreeTop              = 206    ;
   GroupStrawBale               = 207    ;
   GroupTrainHead               = 208    ;
   GroupTrainWagon              = 209    ;

// vehicle ID
   idTruckWhite      = 1 ;
   idMoto            = 2 ;
   idCarRed          = 3 ;
   idCarWhite        = 4 ;
   idCarBlue         = 5 ;
   idCarYellowRed    = 6 ;
   idOilPuddle       = 7 ;

// Ground type for road tile engine
const
  GROUND_HOLE    = 0;
  GROUND_NEUTRAL = 1;
  GROUND_ROAD    = 2;
  GROUND_GRASS   = 3;
  GROUND_WATER   = 4;
  GROUND_RAILWAY = 5;

// Ground type for 'second floor' tile engine
const
  GROUND_ROOF    = 2;
  GROUND_CRASH   = 3;



// Tile event
const
  TILE_EVENT_STARTBEEPJUMP    = 0;
  TILE_EVENT_STOPBEEPJUMP     = 1;
  TILE_EVENT_POSENDRACEGRID   = 2;
  TILE_EVENT_STARTBIGTREEAREA = 3;
  TILE_EVENT_ENDBIGTREEAREA   = 4;
  TILE_EVENT_STARTSTRAWAREA   = 5;
  TILE_EVENT_ENDSTRAWAREA     = 6;
  TILE_EVENT_STARTOILAREA     = 7;
  TILE_EVENT_ENDOILAREA       = 8;
  TILE_EVENT_FOGON            = 9;
  TILE_EVENT_FOGOFF           = 10;
  TILE_EVENT_NIGHTON          = 11;
  TILE_EVENT_NIGHTOFF         = 12;
  TILE_EVENT_RAINON           = 13;



// Scenario User Event
 PlayerCarZigZag  = 1 ;

var
  TextDebug: string;

  FScene : TOGLCScene ;

  FScore : integer;

// Tile Engine
  BGAnimTileEngine,                    // background animation
  RoadTileEngine,                      // road scrolling
  TileEngineSecondFloor: TTileEngine;  // height part animation

// Table
   TableFont : TTexturedFont ;
   SpriteTableFont : TTexturedFont ;
   TableLine : array[0..10] of TGuiLabel ;
   CurrentTable : integer ;
   TableNumberFlag : array[0..10] of boolean ;
   TableDone: array[2..10] of boolean;
   WinCount : integer;

   FBonusSpeedDistanceToGo: single;
   TimerBonusSpeed: PTimerObject;

   CurrentMaxSpeed : single;

// Textures
   TexTitleIntro,
   TexPlayerCar,
   TexTruckWhite,
   TexMoto,
   TexCarRed,
   TexCarYellowRed,
   TexCarBlue,
   TexCarWhite,
   TexCarBlueSky,
   TexOilPuddle,
   TexStartRace,
   TexWarning,
   TexHelicoBody,
   TexHelicoQueue,
   TexHelicoBigFan,
   TexHelicoSmallFan,
   TexFullSpeedText,
   TexBigTreeTop,
   TexBigTreeMiddle,
   TexBigTreeBottom,
   TexStraw,
   TexPlayerCarHeadLight,
   TexTgvHead, TexTgvWagon, TexTgvQueue,
   TexKeysView: PTexture;

// Sprites
   TitleIntro : TSprite;
   PlayerCar:TSprite;

// Sounds
   SHCarStart,
   SHCarJump,
   SHCarEngineLoop,
   SHCarCollideVehicle,
   SHCarCollideStrawBale,
   SHTiresScreechingLong,
   SHHorn1,
   SHHorn2,
   SHHorn3,
   SHHorn4 ,
   SHTableWin,
   SHTableLost,
   SHFallInWater,
   SHWarning,
   SHHelico,
   SHCarCrash,
   SHTrainPassing: TOALSound ;

// musics
   SHMusicIntro,
   SHMusicEndStage: TOALSound;

// game messages
   FontCountry,
   FontMenu,
   FontInstructions,
   FontScore  : TTexturedFont;
   CurrentLanguage : integer;

// game stages
   FCurrentStage: integer;



implementation

end.

