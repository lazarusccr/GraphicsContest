unit GameUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, LCLIntf, LCLType,
  BGRABitmapTypes, BGRABitmap,
  lazutf8,
  variable_parameter_V2,
  OpenAlSoundManager,
  common, OGLCScene, Sprites, Language,
  Controls;

const
 PLAYER_CAR_POSY = 0.7;

type

{ TGame }

TGame = class (TStageSkeleton)
 // player car scenario
 scePlayerCarSkid,
 scePlayerCarJump,
 scePlayerCarCollision,
 scePlayerCarCollisionNight,
 scePlayerCarCrash,
 scePlayerCarFallFromSecondFloor,
 scePlayerCarFallAfterSecondFloorJump,
 sceCarjumpFromSecondFloor: TIDScenario ;
 FPlayerCarFallenInWater,
 FPlayerCarCrashed: boolean;
 FCoeffPlayerCarSpeedX: single;

 JumpState: byte;

 FPlayerCarFollowHelicopter: boolean;
 //
 LastSpriteCollided,
 LastTableCollided: TSimpleSurface ;
 FDistanceBeforeCreateNewNumber : single ;
 FDistanceBeforeCreateNewVehicle: single;
 FDistanceMaxForCreateNewVehicle,
 FDistanceMinForCreateNewVehicle : single ;
 //
 SpriteFullSpeed : TSprite ;
 PlayerScore : TGUILabel;
 FEndGridCreated,
 FGameEnded: boolean;
 //
 FWarning: TSprite;
 sceWarning: TIDScenario;

 // helicopter rescue
 FHelicoBody, FHelicoQueue,
 FHelicoBigFan, FHelicoSmallFan: TSprite;
 FHelicopterDropCoor: TPointF;
 FHelicoDust: TParticleEmitter;

 // colored square under table line
 FColoredSquare: TColorBackground;

 // speed bar
 FSpeedBar: TGuiProgressBar;
 // label stage
 FLabelStage: TGuiLabel;
 // Big Tree
 FRequestBigTree: boolean;
 FDistanceToRequestBigTree: single;
 // Straw
 FRequestStraw: boolean;
 FDistanceToRequestStraw: single;
 // Oil puddle area
 FRequestOilPuddle: boolean;
 // tile engine for shadows
 FFshadowsTileEngine: TTileEngine;
 // fog
 FogTileEngine: TTileEngine;
 FFogCreated,
 FFogVisible: boolean;
 // night
 CarHeadLight: TSprite;
 NightObjectArray: array[0..4] of TSimpleSurfaceWithEffect;
 FNightIsON: boolean;
 // rain
 FRainCreated,
 FRainVisible: boolean;
 FRainParticleEmitter: TParticleEmitter;
 // snow
 FSnowCreated: boolean;
 FSnow: TSnow;
 // train
 FTrain: TArrayOfVehicle;
 FTrainHeight: integer;
 FtrainAudioFadeOutInProgress: boolean;

 procedure LoadData; override;
 procedure FreeData; override;
 procedure CallBackTimerBonusSpeed;
 procedure Update( AElapsedTime: single ); override;

 function PlayerHasLost: boolean; inline;

 function IsOnRoadType( aP: TPointF ): boolean;
 function IsHoleOnSecondFloor( aP: TPointF ): boolean;

 function IsOnRoadAndNothingOnSecondFloor( aP: TPointF ): boolean;

 function SecondFloorGroundIsRoof( aP: TPointF ): boolean;
 function SecondFloorGroundIsCrash( aP: TPointF ): boolean;

 function CollisionWithOtherVehicle(aX, aY, aW, aH: single ): boolean;

 procedure ProcessScenarioEvent( aSurface: TSimpleSurfaceWithEffect; aScenarioID: TIDScenario; aUserValue: integer );
 procedure ProcessTileEngineEvent( Sender: TTileEngine; const TileTopLeftCoor: TPointF; EventValue: integer );

 procedure PlayerCarLaunchJump;
 procedure playerCarAbortJump;

 procedure PlayerCarCollisionTest;

 procedure CreateVehicle(aVehicleID: integer);
 procedure KillAllVehicleWhenPlayerLose;
 procedure SetCoordinateOnTheRoad( o: TSimpleSurfaceWithEffect );

 procedure AddToScore( aBonus: integer );
 procedure CreateBonusScore( ABonus: integer );
 procedure CreateBonusSpeed;
 procedure CreateRescueMalus( aTxt: string; aMalus: integer );

 procedure CreateStartGrid;
 procedure CreateEndGrid;
 procedure CreateBigTree;
 procedure CreateStrawBale;

 procedure MoveScoreToSceneCenter;
 procedure PlayerCarWinRaceAnimation;

 procedure LaunchHelicopterRescue( AX, AY: single );
 procedure PlayerCarFallInWater( AX, AY: single );
 procedure PlayerCarCrash;

 procedure PlayersCarHitAnObstacle( AHorn: boolean; AScoreMalus: integer );
 function PlayerCarScenarioCollisionIsRunning: boolean;

 procedure SetPlayerCarEngineAudioFrequency;
 procedure AddToScrollingSpeed( AValue: single );
 procedure AddToPlayerCarSpeedX( AValue: single );

 procedure StartWarningBeep;
 procedure StopWarningBeep;

 procedure CreateFog;
 procedure DoFogAnimation;

 procedure createNight;
 procedure SetNight( Percent, Seconds: single ); // pc=0 day   pc=1 night

 procedure CreateRain;
 procedure DoRainAnim;

 procedure CreateSnow;
 procedure DoSnowAnim;

 procedure CreateBackGroundAnimation( MapFile: string );

 procedure CreateOverRoadTileEngine( MapFile: string );
 procedure CreateShadowsTileEngine( MapFile: string );
 procedure ColoredSquareAnim;

 procedure CreateTrain( aCenterX: single; Pos, WagonCount: integer ); // pos=-1 train is out of view on top of the scene,
                                                                      // pos=0 train is visible
                                                                      // pos=1 train is out of view on bottom of the scene
 procedure TrainAnim;
 procedure PlayTrainSound;
 procedure StopTrainSound;
end;

var
  Game : TGame ;

implementation

uses Introduction;

{ TGame }

procedure TGame.LoadData;
var i: integer ;
  l: TGUILabel;

  procedure InitMap( aMapFile: string );
  begin
   with RoadTileEngine do begin
    LoadMapFile( MAP_PATH + aMapFile );
    SetViewSize( 13*64, FScene.Height );
    SetCoordinate( (FScene.Width - Width) / 2, 0 );
    ScrollSpeed.Value := PointF(0, 0);
  //  PositionOnMap.Value := PointF( 0, 0);
    OnTileEvent := @ProcessTileEngineEvent;
   end;
  end;

begin
 FFogCreated := FALSE;
 FFogVisible := FALSE;
 FNightIsON := FALSE;
 FRainCreated := FALSE;
 FRainVisible := FALSE;
 FSnowCreated := FALSE;

 // player car init
 PlayerCar := TSprite.Create( TexCarBlueSky );
 with PlayerCar do
 begin
  SetFrameLoopBounds(1, 4);
  FrameAddPerSecond( 0 );
  SetCoordinate( FScene.Width/2, Fscene.Height * PLAYER_CAR_POSY );
  scePlayerCarSkid := AddScenario( SCENARIO_PATH+'Carskid.sce' );
  scePlayerCarJump := AddScenario( SCENARIO_PATH+'Carjump.sce', @ProcessScenarioEvent ) ;
  scePlayerCarCollision := AddScenario( SCENARIO_PATH+'CarCollision.sce' );
  scePlayerCarCollisionNight := AddScenario( SCENARIO_PATH+'CarCollisionNight.sce' );
  scePlayerCarCrash := AddScenario( SCENARIO_PATH+'CarCrash.sce' );
  sceCarjumpFromSecondFloor := AddScenario( SCENARIO_PATH+'CarjumpFromSecondFloor.sce', @ProcessScenarioEvent );
  scePlayerCarFallAfterSecondFloorJump := AddScenario( SCENARIO_PATH+'PlayerCarFallAfterSecondFloorJump.sce', @ProcessScenarioEvent );
  scePlayerCarFallFromSecondFloor := AddScenario( SCENARIO_PATH+'PlayerCarFallFromSecondFloor.sce', @ProcessScenarioEvent );
 end;
 FScene.Add( PlayerCar, LayerPlayer );

 // car engine sound (looped)
 SHCarEngineLoop.FadeIn( 1 );

 SHWarning.Volume.Value := 400;

 TileEngineSecondFloor := NIL;
 BGAnimTileEngine := NIL;
 FFshadowsTileEngine := NIL;

 FScene.Layer[LayerRoad].AssignTileEngine( NIL );
 RoadTileEngine.Free;
 RoadTileEngine := TTileEngine.Create;
 FScene.Layer[LayerRoad].AssignTileEngine( RoadTileEngine, FALSE );

// FCurrentStage := 3;

 // load stage map(s)
 case FCurrentStage of
   1: begin
    InitMap( 'RoadStage1.map');
    FDistanceMinForCreateNewVehicle := 80;
   end;

   2: begin
    InitMap( 'RoadStage2.map');
    FDistanceMinForCreateNewVehicle := 110;
   end;

   3: begin
    InitMap( 'RoadStage3.map');
    FDistanceMinForCreateNewVehicle := 110;
    CreateFog;
    FFogVisible := TRUE;
    FogTileEngine.Opacity.Value := 100;
    CreateSnow;
   end;

   4: begin
    InitMap( 'RoadStage4.map');
    FDistanceMinForCreateNewVehicle := 500;
   end;

   5: begin
    InitMap( 'RoadStage5.map');
    CreateOverRoadTileEngine('RoadStage5_Over.map');
    FDistanceMinForCreateNewVehicle := 400;
    CreateNight;
    SetNight( 0, 0 );
   end;

   6: begin
    InitMap( 'RoadStage6.map');
    FDistanceMinForCreateNewVehicle := 400;
    CreateOverRoadTileEngine('RoadStage6_Over.map');
   end;

   7: begin
    InitMap( 'RoadStage7.map');
    FDistanceMinForCreateNewVehicle := 400;
    CreateBackGroundAnimation('River.map');
    BGAnimTileEngine.ScrollSpeed.x.Value := 60;   // river flow to the right
    CreateRain;
   end;

   8: begin
    InitMap( 'RoadStage8.map');
    FDistanceMinForCreateNewVehicle := 400;

    CreateBackGroundAnimation('River.map');
    BGAnimTileEngine.ScrollSpeed.x.Value := 60;   // river flow to the right

    CreateOverRoadTileEngine('RoadStage8_Over.map');

    CreateShadowsTileEngine('RoadStage8_Shadows.map');
   end;

   9: begin
    InitMap( 'RoadStage9.map');
    FDistanceMinForCreateNewVehicle := 400;

    CreateOverRoadTileEngine('RoadStage9_Over.map');

    CreateShadowsTileEngine('RoadStage9_Shadows.map');

    CreateTrain( RoadTileEngine.x.Value+ RoadTileEngine.TileSize.cx*11, 0, 5 );
    CreateNight;
    SetNight( 0, 0 );
   end;

  end;//case

// createNight;
// CreateRain;
// CreateFog;
// CreateSnow;


 if FSnowCreated
   then FCoeffPlayerCarSpeedX := 0.93   // when it snows, the road is is slippery
   else FCoeffPlayerCarSpeedX := 0.8;


 // starting grid
 CreateStartGrid;

 // colored square under table lines
 FColoredSquare := TColorBackground.Create( RoadTileEngine.RightX, 0,
                    round( FScene.Width-RoadTileEngine.RightX ), FScene.Height );
 with FColoredSquare do
  begin
   TopLeftColor.Value := BGRA(0,39,94);    // black blue
   TopRightColor.Value := BGRA(0,107,255); // blue
   BottomRightColor.Value := BGRA(0,39,94);  // black blue
   BottomLeftColor.Value := BGRA(0,107,255); // blue
   Opacity.Value := 200;
  end;
 FScene.Add( FColoredSquare, LayerScore );

 // table lines
 SelectRandomTable;

 // score label
 PlayerScore := TGUILabel.Create;
 with PlayerScore do begin
  Font := FontScore;
  Caption := 'Score: ' + inttostr( FScore );
  Tint.Value := BGRA(255,255,0,85);
  SetCoordinate( TableLine[10].x.Value, TableLine[10].BottomY + 50 );
 end;
 FScene.Add( PlayerScore, LayerScore );

 // stage number label
 FLabelStage := TGuiLabel.Create;
 with FLabelStage do begin
  Font := FontScore;
  Caption := 'stage '+inttostr( FCurrentStage );
  Tint.Value := BGRA(255,128,0,85);
  SetCoordinate( PlayerScore.X.Value, PlayerScore.BottomY+30);
 end;
 FScene.Add( FLabelStage, LayerScore );

 // full speed message
 SpriteFullSpeed := TSprite.Create( TexFullSpeedText );
 SpriteFullSpeed.SetCenterCoordinate( FScene.Width/2, FScene.Height/2-200);
 SpriteFullSpeed.Visible := FALSE ;
 FScene.Add( SpriteFullSpeed, LayerScore );

 PlayerCar.Speed.x.Value := 0;
 JumpState := 0;
 FDistanceBeforeCreateNewNumber := 2000;
 FDistanceMaxForCreateNewVehicle := 1000;
 FDistanceBeforeCreateNewVehicle := 1200;
 for i:=0 to 10 do TableNumberFlag[i] := FALSE ;

 WinCount := 0;

 FBonusSpeedDistanceToGo := FULL_SPEED_DISTANCE_TO_GO;
 TimerBonusSpeed := FScene.TimerManager.Add( @CallBackTimerBonusSpeed, 250 );

 LastSpriteCollided := NIL ;
 LastTableCollided := NIL ;
 CurrentMaxSpeed := VSPEED_MAX - 200;
 FGameEnded := FALSE;
 FEndGridCreated := FALSE;
 FPlayerCarFallenInWater := FALSE;
 FPlayerCarCrashed := FALSE;
 FPlayerCarFollowHelicopter := FALSE;

 FRequestBigTree := FALSE;
 FDistanceBeforeCreateNewNumber := 0 ;

 FRequestStraw := FALSE;
 FDistanceToRequestStraw := 0;

 FRequestOilPuddle := FALSE;

 // message 'stage x' cross the scene
 l := TGUILabel.Create;
 l.Font := FontCountry;
 l.Caption := 'stage '+inttostr( FCurrentStage);
 l.SetCenterCoordinate( FScene.Width/2, FScene.Height/2-100 );
 l.AddAndPlayScenario(SCENARIO_PATH+'StageTitle.sce');
 FScene.Add( l, LayerIntro );

 // warning icon
 FWarning := TSprite.create( TexWarning );
 FWarning.SetCenterCoordinate( RoadTileEngine.X.Value+200, 200 );
 FWarning.Visible := FALSE;
 sceWarning := FWarning.AddScenario( SCENARIO_PATH+'Warning.sce', @ProcessScenarioEvent );
 FScene.Add( FWarning, LayerScore);

 // create helicopter rescue with 4 parts, the body, the queue, the big and the small propellers.
 //
 // the main sprite is body, it have 2 childs: the queue and the big propeller
 // queue have 1 child: small propeller
 // when main sprite moves or rotates (here the helicopter body) all the childs move accordingly
 FHelicoBody := TSprite.create( TexHelicoBody );

 // particle emitter for dust under helicopter
 FHelicoDust := TParticleEmitter.Create;
 FHelicoDust.LoadFromFile(PARTICLES_PATH+'DustForHelicopter.par');
 FHelicoDust.BindCenterToSprite( FHelicoBody );
 FHelicoDust.Visible := FALSE;
 FScene.Add( FHelicoDust, LayerScore );

 FHelicoBody.Visible := FALSE;
 FScene.Add( FHelicoBody, LayerScore );

 FHelicoQueue := TSprite.create( TexHelicoQueue );
 FHelicoQueue.SetCoordinate( -122, 39 );
 FHelicoBody.AddChild( FHelicoQueue );

 FHelicoBigFan := TSprite.create( TexHelicoBigFan );
 FHelicoBigFan.Angle.AddConstant( 360*4 );
 FHelicoBigFan.SetCenterCoordinate( FHelicoBody.Width*0.5-30, FHelicoBody.Height*0.5 );
 FHelicoBody.AddChild( FHelicoBigFan );

 FHelicoSmallFan := TSprite.create( TexHelicoSmallFan );
 FHelicoQueue.AddChild( FHelicoSmallFan );
 FHelicoSmallFan.Angle.AddConstant( 360 );
 FHelicoSmallFan.SetCenterCoordinate( 16, 16 );


 // Speed bar
 FSpeedBar := TGuiProgressBar.Create( RoadTileEngine.RightX-340, 10, 300, 20 );
 FSpeedBar.SetLeftColors( BGRA(200,200,0) );
 FSpeedBar.SetRightColors( BGRA(255,30,50) );
 FScene.Add( FSpeedBar, LayerScore );

end;

procedure TGame.FreeData;
var i: integer;
begin
 TimerBonusSpeed^.Kill := TRUE;

 RoadTileEngine.OnTileEvent := NIL;

 FScene.Layer[LayerOverRoad].AssignTileEngine( NIL );
 FScene.Layer[LayerBGAnim].AssignTileEngine( NIL );
 Fscene.Layer[LayerShadows].AssignTileEngine( NIL );

 for i:=0 to NBLayer-1 do FScene.Layer[i].Clear;
 SHWarning.Stop;
 SHHelico.Stop;
 SHCarEngineLoop.Stop;
 SHMusicEndStage.FadeOut( 2, idcStartFastEndSlow );

 SetLength( FTrain, 0 );
// ShaderManager.UseNone;
end;

procedure TGame.CallBackTimerBonusSpeed;
begin
 if ( WinCount >= 11 ) and
    ( RoadTileEngine.ScrollSpeed.y.Value = VSPEED_MAX ) and
    (( JumpState = 0) or ( JumpState = 9))
   then CreateBonusSpeed;
end;

procedure TGame.Update(AElapsedTime: single);
var o: TSimpleSurfaceWithEffect;
    i: integer;
    SpeedZero: single;
    TableLabel: TGuiLabel;
    FlagObstacleForward, FlagObstacleSide: boolean;
begin
 //TextDebug := 'JumpState='+inttostr( JumpState );
// TextDebug := 'helico angle='+inttostr(round(FHelicoBody.Angle.Value));
 TextDebug := 'top/left Player= '+inttostr( RoadTileEngine.GetGroundType( PointF(PlayerCar.X.Value,PlayerCar.Y.Value)) );

 // big tree creation
 if FRequestBigTree then
 begin
      if FDistanceToRequestBigTree > 0.0
        then FDistanceToRequestBigTree -= RoadTileEngine.ScrollSpeed.y.Value * AElapsedTime
        else begin
              CreateBigTree;
              FDistanceToRequestBigTree := 100 + random( 50 );
             end;
 end;

 // Straw ball creation
 if FRequestStraw then
 begin
      if FDistanceToRequestStraw > 0.0
        then FDistanceToRequestStraw -= RoadTileEngine.ScrollSpeed.y.Value * AElapsedTime
        else begin
              CreateStrawBale;
              FDistanceToRequestStraw := 30 + random( 50 );
             end;
 end;

 // snow anim
 if FSnowCreated then DoSnowAnim;

 // fog anim
 if FFogCreated then DoFogAnimation;

 // rain anim
 if FRainCreated then DoRainAnim;

 // Background tile engine anim
 if BGAnimTileEngine <> NIL
   then BGAnimTileEngine.ScrollSpeed.y.Value := RoadTileEngine.ScrollSpeed.y.Value;

 // night
 if FNightIsON then CarHeadLight.Angle.Value := PlayerCar.Angle.Value;

 // speed bar
 FSpeedBar.Percent := RoadTileEngine.ScrollSpeed.y.Value/VSPEED_MAX;


 if WinCount >= 11
   then FBonusSpeedDistanceToGo -= AElapsedTime * RoadTileEngine.ScrollSpeed.y.Value;

 if not FGameEnded and not PlayerHasLost
   then PlayerCarCollisionTest;

 // we want more vehicle on the road if all table number are winned
 if WinCount >= 11 then
 begin
  ColoredSquareAnim;

  FDistanceMaxForCreateNewVehicle -= 1;
  if FDistanceMaxForCreateNewVehicle < FDistanceMinForCreateNewVehicle
     then FDistanceMaxForCreateNewVehicle := FDistanceMinForCreateNewVehicle;
 end;

 // vehicle creation
 if not FEndGridCreated
   then begin
         FDistanceBeforeCreateNewVehicle -= RoadTileEngine.ScrollSpeed.y.Value * AElapsedTime;
         if FDistanceBeforeCreateNewVehicle <= 0
           then begin
                FDistanceBeforeCreateNewVehicle := random(200) + FDistanceMaxForCreateNewVehicle;

                if FNightIsON
                  then repeat   // no oil puddle in the night, because rotation car cause issue...
                        i := random(7) + 1;
                       until i <> idOilPuddle
                  else i := random(7) + 1;

                if FRequestOilPuddle  // we want more oil puddle on straw ball area
                  then begin
                       i := idOilPuddle;
                       FDistanceBeforeCreateNewVehicle := FDistanceBeforeCreateNewVehicle*0.4;
                  end;

                 CreateVehicle( i );
           end;
   end;

 // escape key
 if FScene.Key[VK_ESCAPE]
   then begin
         SHCarEngineLoop.FadeOut( 1 );
         FScene.LaunchStage( Intro );
         exit ;
        end;

 if not PlayerHasLost and not FGameEnded then begin
  // Car Player left/right
  if (RoadTileEngine.ScrollSpeed.y.Value > 0) and not PlayerCar.ScenarioIsPlaying( scePlayerCarSkid )
    then begin
          if FScene.Key[VK_LEFT]
            then begin
                  AddToPlayerCarSpeedX( -RoadTileEngine.ScrollSpeed.y.Value );
                  if PlayerCar.Speed.x.Value < -300 then PlayerCar.Speed.x.Value := -300;
                  PlayerCar.Angle.Value := -6;
                 end
            else if FScene.Key[VK_RIGHT]
                   then begin
                         AddToPlayerCarSpeedX( RoadTileEngine.ScrollSpeed.y.Value );
                         if PlayerCar.Speed.x.Value > 300 then PlayerCar.Speed.x.Value := 300;
                         PlayerCar.Angle.Value := 6;
                        end
                   else begin //
                         PlayerCar.Speed.x.Value := PlayerCar.Speed.x.Value * FCoeffPlayerCarSpeedX;//0.96;//0.8;
                         if abs(PlayerCar.Speed.x.Value) < 40 then PlayerCar.Speed.x.Value := 0;
                         PlayerCar.Angle.Value := 0;
                        end;
    end;
   if RoadTileEngine.ScrollSpeed.y.Value = 0 then PlayerCar.Speed.x.Value := 0;

   // player car jump test
   if FScene.Key[VK_CONTROL] and not PlayerCar.ScenarioIsPlaying( scePlayerCarSkid )
      then PlayerCarLaunchJump;

   // scrolling speed key up/down
   if FScene.Key[VK_UP] then AddToScrollingSpeed( 2 );
   if FScene.Key[VK_Down] then AddToScrollingSpeed( -12.5 );
 end;

 // avoid player car go out off the road to the left and the right
 if PlayerCar.X.Value < RoadTileEngine.X.Value then PlayerCar.X.Value := RoadTileEngine.X.Value;
 if PlayerCar.RightX > RoadTileEngine.RightX then PlayerCar.RightX := RoadTileEngine.RightX;

 // player's car must follow helicopter when it carry it
 if FPlayerCarFollowHelicopter
    then PlayerCar.SetCenterCoordinate( FHelicoBody.CenterX, FHelicoBody.CenterY );

 SetPlayerCarEngineAudioFrequency;

 // second floor TileEngine
 if TileEngineSecondFloor <> NIL then
   TileEngineSecondFloor.PositionOnMap.Value := RoadTileEngine.GetBoundedPositionOnMap;

 // shadows tile engine anim
 if FFshadowsTileEngine <> NIL
   then FFshadowsTileEngine.PositionOnMap.Value := RoadTileEngine.GetBoundedPositionOnMap;

  SpeedZero := RoadTileEngine.ScrollSpeed.y.Value ;

  // big tree position
  for i:=0 to FScene.Layer[LayerOverRoad].SurfaceCount-1 do
   begin
    o := FScene.Layer[LayerOverRoad].Surface[i];
    case o.Group of
     GroupBigTreeMiddle,
     GroupBigTreeTop: o.Speed.y.Value := SpeedZero;
    end;//case
   end;

  // train anim
  if Length( FTrain ) > 0 then TrainAnim;

  // other sprites speed and collision test
  for i:=0 to FScene.Layer[LayerSprite].SurfaceCount-1 do
   begin
    o := FScene.Layer[LayerSprite].Surface[i];

    case o.Group of
     GroupOilPuddle,
     GroupStartRace,
     GroupBigTreeBottom,
     GroupStrawBale,
     GroupEndRace: o.Speed.y.Value := SpeedZero   // these sprites have same speed than road

     else begin
             with ( o as TVehicle ) do
              begin
               FlagObstacleForward := CollisionWithOtherVehicle( X.Value, Y.Value-50, Width, 49 ) or
                                      (FScene.Layer[LayerPlayer].CollisionTest( X.Value, Y.Value-50, Width, 49 )<>NIL) or
                                      not IsOnRoadType( PointF( X.Value+5, Y.Value - 20)) or
                                      not IsOnRoadType( PointF(RightX-5, Y.Value - 20));

               if FlagObstacleForward
                 then begin // there is an obstacle forward => we decrease vehicle speed
                       NeededYSpeed -= 50;
                       if NeededYSpeed < 0 then NeededYSpeed := 0;
                       // when the vehicle is stopped, it shifts randomly to the left or to the right
                       if o.Speed.X.Value = 0
                         then if random(100)>50
                                then o.Speed.X.Value :=100
                                else o.Speed.X.Value := -100;
                      end
                 else begin
                       // nothing forward => vehicle accelerate to its normal speed
                       NeededYSpeed += 3;
                       if NeededYSpeed > MaxYSpeed then NeededYSpeed := MaxYSpeed;
                       if o.Group <> GroupSpriteMoto then o.Speed.X.Value := 0;
                      end;
               // apply speed on vehicle
               Speed.y.Value := -( NeededYSpeed - SpeedZero );
              end;

              // test vehicle/side tile
              FlagObstacleSide := FALSE;
              if o.Speed.X.Value < 0
                then FlagObstacleSide := (FScene.Layer[LayerSprite].CollisionTest( o.X.Value-21, o.Y.Value, 20, o.Height, o ) <> NIL) or
                                         (FScene.Layer[LayerPlayer].CollisionTest( o.X.Value-21, o.Y.Value, 20, o.Height ) <> NIL) or
                                         not IsOnRoadType( PointF(o.X.Value - 20, o.Y.Value+30))
                else FlagObstacleSide := (FScene.Layer[LayerSprite].CollisionTest( o.RightX+1, o.Y.Value, o.RightX+20, o.Height, o ) <> NIL) or
                                         (FScene.Layer[LayerPlayer].CollisionTest( o.RightX+1, o.Y.Value, o.RightX+20, o.Height ) <> NIL) or
                                         not IsOnRoadType( PointF(o.RightX + 15, o.Y.Value+30));
               if FlagObstacleSide
                 then o.Speed.X.Value := - o.Speed.X.Value;
          end;
    end;//case
    if (o.y.Value< -500 ) or (o.y.Value > RoadTileEngine.Height+500) then o.Kill; // vehicles too far (forward and behind) are deleted

    // Tests whether the vehicle has been doubled by player's car
    if ( o.y.Value > PlayerCar.y.Value + 5 ) and not ( o.Tag2 ) and ( o.Group <> GroupStartRace ) and not FGameEnded
       and (o.Group <> GroupStrawBale) and (o.Group <> GroupBigTreeBottom)
      then begin
            CreateBonusScore( 100 );
            o.Tag2 := TRUE ; // means player car have doubled this vehicle

      end else if ( o.BottomY < PlayerCar.y.Value - 5 ) and o.Tag2 and not FGameEnded
        then begin
              CreateBonusScore( -200 );
              o.Tag2 := FALSE;
      end;
   // if FGameEnded and (o.Group <> GroupStartRace ) then o.Speed.Value := PointF(0,0);
   end;

// Multiplication table sprite speed
  for i:=0 to FScene.Layer[LayerTable].SurfaceCount-1 do
   begin
    o := FScene.Layer[LayerTable].Surface[i];
    if o.Tag1=1000
      then begin
            o.Speed.y.Value := -300;
            if o.Opacity.Value = 0 then o.Kill;
      end
      else o.Speed.y.Value := SpeedZero;
   end;

// Multiplication table sprite creation
   FDistanceBeforeCreateNewNumber -= RoadTileEngine.ScrollSpeed.y.Value * AElapsedTime;
   if ( FDistanceBeforeCreateNewNumber <= 0 ) and ( WinCount < 11 )
     then begin
           FDistanceBeforeCreateNewNumber := Random( 600 ) + 500;
           TableLabel := TGuiLabel.Create;
           TableLabel.Font := SpriteTableFont ;
           if random(100) > 60
             then begin // we want number within current table
                   repeat
                    i := random(11) ;
                   until not TableNumberFlag[i] ;
                   TableLabel.Tag1 := i * CurrentTable ;
                   TableLabel.Caption := inttostr( i * CurrentTable ) ;
             end else begin // we don't want number within current table
              repeat
               i := random(100) ;
              until i mod CurrentTable > 0 ;
              TableLabel.Tag1 := i ;
              TableLabel.Caption := inttostr( i ) ;
             end;
           SetCoordinateOnTheRoad ( TableLabel );
           TableLabel.Group := GroupTableSprite ;
           FScene.Add( TableLabel, LayerTable );
     end;
end;

function TGame.PlayerHasLost: boolean;
begin
 Result := FPlayerCarCrashed or FPlayerCarFallenInWater;
end;

function TGame.IsOnRoadType(aP: TPointF): boolean;
var gr: integer;
begin
 gr := RoadTileEngine.GetGroundType( aP );
 Result := (gr = GROUND_ROAD );
end;

function TGame.IsHoleOnSecondFloor(aP: TPointF): boolean;
var gr: integer;
begin
 if TileEngineSecondFloor = NIL
   then Result := TRUE
   else begin
        gr := TileEngineSecondFloor.GetGroundType( aP );
        Result := (gr = GROUND_HOLE );
   end;
end;

function TGame.IsOnRoadAndNothingOnSecondFloor(aP: TPointF): boolean;
begin
 Result := ( RoadTileEngine.GetGroundType( aP ) = GROUND_ROAD ) and
        not ( TileEngineSecondFloor.GetGroundType( aP ) = GROUND_ROOF ) and
        not ( TileEngineSecondFloor.GetGroundType( aP ) = GROUND_CRASH );
end;

function TGame.SecondFloorGroundIsRoof(aP: TPointF): boolean;
begin
 if TileEngineSecondFloor <> NIL
   then Result := TileEngineSecondFloor.GetGroundType( aP ) = GROUND_ROOF
   else Result := FALSE;
end;

function TGame.SecondFloorGroundIsCrash(aP: TPointF): boolean;
begin
 if TileEngineSecondFloor <> NIL
   then Result := TileEngineSecondFloor.GetGroundType( aP ) = GROUND_CRASH
   else Result := FALSE;
end;

function TGame.CollisionWithOtherVehicle(aX, aY, aW, aH : single): boolean;
var o: TSimpleSurfaceWithEffect;
begin
 o := FScene.Layer[LayerSprite].CollisionTest(aX, aY, aW, aH );
 if o <> NIL
   then Result := o.Group <> GroupEndRace
   else Result := FALSE;

end;

procedure TGame.SetCoordinateOnTheRoad( o : TSimpleSurfaceWithEffect ) ;
var xx : single;
    trycount: integer;
begin
  trycount := 0;
  with RoadTileEngine do
   repeat
     xx := x.Value + TileSize.cx * 1.5 + random( Width - TileSize.cx * 3 );
     inc (trycount);
   until ( ( FScene.Layer[LayerSprite].CollisionTest( o ) = NIL ) and
          IsOnRoadType( PointF(xx, -o.Height)) and IsOnRoadType( PointF(xx+o.Width, -o.Height)) and
          IsOnRoadType( PointF(xx+o.Width*0.5, -o.Height)) and
          IsOnRoadType( PointF(xx, 0)) and IsOnRoadType( PointF(xx+o.Width, 0)) and
          IsOnRoadType( PointF(xx+o.Width*0.5, 0)) and
          IsOnRoadType( PointF( xx, -o.Height - 20)) and
          IsOnRoadType( PointF(xx+o.Width, -o.Height - 20))
          )
          or ( trycount = 100 );

 if trycount < 100 then begin
   o.SetCoordinate( xx, -o.Height );
   o.Tag2 := FALSE; // means the vehicle is forward player car
 end else begin
  // trycount = 100 => we consider there is no place to put the vehicle on the road
  // hence, vehicle is placed out of view
  o.SetCoordinate( -1000, -o.Height );
 end;
end;



procedure TGame.CreateVehicle(aVehicleID: integer);
var o: TVehicle ;
    Particle: TParticleEmitter;
    function CreateCarParticleEmitter: TParticleEmitter;
    begin
     Result := TParticleEmitter.Create;
     Result.LoadFromFile( PARTICLES_PATH+'Smoke.par' );
     Result.Direction.Value:=90;
     Result.Spread.Value:= 30 + random( 100 ) ;
     Result.Opacity.Value:= 20 + random( 50 );
     Result.ParticlesToEmit.Value:= 50 + random( 100 );
     Result.Gravity.y.Value := 80;
    end;

begin
 case aVehicleID of
  idTruckWhite : begin
       o := TVehicle.Create( TexTruckWhite );
       SetCoordinateOnTheRoad( o ) ;
       o.MaxYSpeed := 150+random(200) ;   // maximum speed
       o.NeededYSpeed := o.MaxYSpeed;
       o.Speed.y.Value := o.MaxYSpeed - RoadTileEngine.ScrollSpeed.y.Value;
       o.SetFrameLoopBounds( 1, 4);
       o.FrameAddPerSecond(8);
       o.Group:=GroupSpriteTruck;
       FScene.Add(o, LayerSprite);
       Particle := TParticleEmitter.Create;
       Particle.LoadFromFile( PARTICLES_PATH+'Smoke.par' );
       Particle.Direction.Value:=90;
       Particle.Spread.Value:= 60 ;
       Particle.Gravity.y.Value:=100;
       Particle.SetCoordinate( o.Width/2, o.Height-2 );
       o.AddChild( Particle );
  end;
  idMoto : begin
   o := TVehicle.Create( TexMoto );
   SetCoordinateOnTheRoad( o ) ;
   o.MaxYSpeed := 150+random(400) ;
   o.NeededYSpeed := o.MaxYSpeed;
   o.Speed.y.Value := o.MaxYSpeed - RoadTileEngine.ScrollSpeed.y.Value;
   o.Speed.x.Value := 50 + random( 100 );
   if random(100) > 50 then o.Speed.x.Value := -o.Speed.x.Value;
   o.SetFrameLoopBounds( 1, 4);
   o.FrameAddPerSecond(8);
   o.Group:=GroupSpriteMoto;
   FScene.Add(o, LayerSprite);
   Particle := TParticleEmitter.Create;
   Particle.LoadFromFile( PARTICLES_PATH+'Smoke.par' );
   Particle.Direction.Value:=90;
   Particle.Spread.Value:= 20 ;
   Particle.ParticlesToEmit.Value:= 8;
   Particle.Gravity.y.Value:=80;
   Particle.Opacity.Value:=100;
   Particle.SetCoordinate( -5, o.Height-2 );
   o.AddChild( Particle );
  end;
  idCarRed : begin
   o := TVehicle.Create( TexCarRed );
   SetCoordinateOnTheRoad( o ) ;
   o.MaxYSpeed := 200+random(300) ;
   o.NeededYSpeed := o.MaxYSpeed;
   o.Speed.y.Value := o.MaxYSpeed - RoadTileEngine.ScrollSpeed.y.Value;
   o.SetFrameLoopBounds( 1, 4);
   o.FrameAddPerSecond(8);
   o.Group:=GroupSpriteRedCar;
   FScene.Add(o, LayerSprite);
   Particle := CreateCarParticleEmitter;
   Particle.SetCoordinate( o.Width/6, o.Height-2 );
   o.AddChild( Particle );
  end;
  idCarWhite : begin
   o := TVehicle.Create( TexCarWhite );
   SetCoordinateOnTheRoad( o ) ;
   o.MaxYSpeed := 200+random(300) ;
   o.NeededYSpeed := o.MaxYSpeed;
   o.Speed.y.Value := o.MaxYSpeed - RoadTileEngine.ScrollSpeed.y.Value;
   o.SetFrameLoopBounds( 1, 4);
   o.FrameAddPerSecond(8);
   o.Group:=GroupSpriteWhiteCar;
   FScene.Add(o, LayerSprite);
   Particle := CreateCarParticleEmitter;
   Particle.SetCoordinate( o.Width/6, o.Height-2 );
   o.AddChild( Particle );
  end;
  idCarBlue : begin
   o := TVehicle.Create( TexCarBlue );
   SetCoordinateOnTheRoad( o ) ;
   o.MaxYSpeed := 200+random(300) ;
   o.NeededYSpeed := o.MaxYSpeed;
   o.Speed.y.Value := o.MaxYSpeed - RoadTileEngine.ScrollSpeed.y.Value;
   o.SetFrameLoopBounds( 1, 4);
   o.FrameAddPerSecond(8);
   o.Group:=GroupSpriteBlueCar;
   FScene.Add(o, LayerSprite);
   Particle := CreateCarParticleEmitter;
   Particle.SetCoordinate( o.Width/6, o.Height-2 );
   o.AddChild( Particle );
  end;
  idCarYellowRed : begin
   o := TVehicle.Create( TexCarYellowRed );
   SetCoordinateOnTheRoad( o ) ;
   o.MaxYSpeed := 200+random(300) ;
   o.NeededYSpeed := o.MaxYSpeed;
   o.Speed.y.Value := o.MaxYSpeed - RoadTileEngine.ScrollSpeed.y.Value;
   o.SetFrameLoopBounds( 1, 4);
   o.FrameAddPerSecond(8);
   o.Group:=GroupSpriteYellowRedCar;
   FScene.Add(o, LayerSprite);
   Particle := CreateCarParticleEmitter;
   Particle.SetCoordinate( o.Width/6, o.Height-2 );
   o.AddChild( Particle );
  end;
  idOilPuddle : begin
   o := TVehicle.Create( TexOilPuddle );
   SetCoordinateOnTheRoad( o );
   o.Speed.y.Value := RoadTileEngine.ScrollSpeed.y.Value;
   o.Group := GroupOilPuddle;
   FScene.Add(o, LayerSprite);
  end;
 end;//case
end;


procedure TGame.KillAllVehicleWhenPlayerLose;
var i: integer;
    o: TSimpleSurfaceWithEffect;
begin
 with FScene.Layer[LayerSprite] do
   for i:=0 to SurfaceCount-1 do
    begin
     o := Surface[i];
     if (( o.Group >= GroupSpriteTruck ) and ( o.Group <= GroupOilPuddle ))
        or ( o.Group = GroupEndRace ) or ( o.Group = GroupStrawBale )
          then o.Kill;
    end;

 with FScene.Layer[LayerTable] do
  for i:=0 to SurfaceCount-1 do Surface[i].Kill;

 with FScene.Layer[LayerOverRoad] do
  for i:=0 to SurfaceCount-1 do Surface[i].Kill;

end;


// process event from scenario execution
procedure TGame.ProcessScenarioEvent(aSurface: TSimpleSurfaceWithEffect; aScenarioID: TIDScenario; aUserValue: integer);
begin
 if aSurface = PlayerCar  // message from sprite player car
 then begin
       JumpState := aUserValue;
       case JumpState of
        0,1,5: if not FPlayerCarCrashed then FScene.SurfaceChangeLayer( PlayerCar, LayerPlayer );
        3,9,10: FScene.SurfaceChangeLayer( PlayerCar, LayerPlayer2 );
       end;
      end

 else
 if aSurface = FWarning   // message from sprite warning beep
 then begin
       if aUserValue = 1
         then SHWarning.Play( TRUE )
         else SHWarning.Stop;
      end

 else
 if aSurface = FHelicoBody // message from helicopter rescue
   then begin
     case aUserValue of
      1: begin  // after player car crash, it must follow helicopter in the air
          if FPlayerCarCrashed
            then FPlayerCarFollowHelicopter := TRUE;
      end;

      2: begin // event 1: helicopter goes to landing location and drop the player car on road
           StopWarningBeep;
           FScene.SurfaceChangeLayer( PlayerCar, LayerPlayer2 );
           RoadTileEngine.PositionOnMap.ChangeTo( PointF( 0, FHelicopterDropCoor.y )+RoadTileEngine.PositionOnMap.Value-PointF(0,PlayerCar.Y.Value), 3, idcSinusoid );
      end;

      3: begin  //event 2: helicopter go away after dropping the player car

          FPlayerCarFollowHelicopter := FALSE;
          FScene.SurfaceChangeLayer( PlayerCar, LayerPlayer );
          with PlayerCar do
           begin
            SetCoordinate( FHelicopterDropCoor.x, FScene.Height * PLAYER_CAR_POSY );
            Visible := TRUE;
            StopScenario( scePlayerCarCrash );
            if FNightIsON
              then PlayScenario( scePlayerCarCollisionNight )
              else PlayScenario( scePlayerCarCollision );
            Angle.Value := 0;
            SetFrameLoopBounds(1, 4);
            FrameAddPerSecond( 0 );
            Frame := 1;
           end;

          SHCarEngineLoop.FadeIn( 1 );

          SHHelico.FadeOut( 3, idcStartFastEndSlow );

          FWarning.StopScenario( sceWarning );
          SHWarning.Stop;


          if FEndGridCreated then CreateEndGrid;
          CreateRescueMalus( Message[4,CurrentLanguage], HELICOPTER_RESCUE_COST );
          FPlayerCarFallenInWater := FALSE;
          FPlayerCarCrashed := FALSE;
      end;
      4: FHelicoDust.Visible := FALSE;
     end;//case
   end;
end;

procedure TGame.ProcessTileEngineEvent(Sender: TTileEngine;
  const TileTopLeftCoor: TPointF; EventValue: integer);
begin
 if Sender = RoadTileEngine then
 begin
  case EventValue of
   TILE_EVENT_STARTBEEPJUMP: StartWarningBeep;
   TILE_EVENT_STOPBEEPJUMP: StopWarningBeep;
   TILE_EVENT_POSENDRACEGRID: begin
    // test if need to create end grid
    if ( FBonusSpeedDistanceToGo <= 0 ) and not FEndGridCreated
      then CreateEndGrid;
   end;
   TILE_EVENT_STARTBIGTREEAREA: FRequestBigTree := TRUE;
   TILE_EVENT_ENDBIGTREEAREA: FRequestBigTree := FALSE;

   TILE_EVENT_STARTSTRAWAREA: FRequestStraw := TRUE;
   TILE_EVENT_ENDSTRAWAREA: FRequestStraw := FALSE;

   TILE_EVENT_STARTOILAREA: FRequestOilPuddle := TRUE;
   TILE_EVENT_ENDOILAREA: FRequestOilPuddle := FALSE;

   TILE_EVENT_FOGON: FFogVisible := TRUE;
   TILE_EVENT_FOGOFF: FFogVisible := FALSE;

   TILE_EVENT_NIGHTON: if PlayerCar.Y.Value <= TileTopLeftCoor.y
                         then SetNight( 0.8, 0.5 );
   TILE_EVENT_NIGHTOFF: if PlayerCar.Y.Value <= TileTopLeftCoor.y
                          then SetNight( 0, 0.5 );

   TILE_EVENT_RAINON: FRainVisible := TRUE;
  end;//case
 end;
end;

procedure TGame.PlayerCarFallInWater( AX, AY: single );
var WaterSplash: TParticleEmitter;
begin
 if FPlayerCarFallenInWater then exit;
 FPlayerCarFallenInWater := TRUE;

 PlayerCar.StopScenario( scePlayerCarCollision );
 PlayerCar.StopScenario( scePlayerCarCollisionNight );
 PlayerCar.StopBlink; // if player car hits an obstacle before fall in water
 PlayerCar.Tint.Value := BGRAPixelTransparent;

 PlayerCar.Visible := FALSE;
 RoadTileEngine.ScrollSpeed.y.Value := 0;
 SHCarEngineLoop.Stop;        // stop player car engine sound

 KillAllVehicleWhenPlayerLose;

 // create two particles emitter to simulate water splash
 // one blue
 WaterSplash := TParticleEmitter.Create;
 WaterSplash.LoadFromFile( PARTICLES_PATH+'WaterSplash.par' );
 WaterSplash.SetCenterCoordinate( AX, AY );
 FScene.Add( WaterSplash, LayerPlayer );
 WaterSplash.AddAndPlayScenario( SCENARIO_PATH+'Splash.sce' ); // launch anim. on it
 // another one, white
 WaterSplash := TParticleEmitter.Create;
 WaterSplash.LoadFromFile( PARTICLES_PATH+'WaterSplashWhite.par' );
 WaterSplash.SetCenterCoordinate( AX, AY );
 FScene.Add( WaterSplash, LayerPlayer );
 WaterSplash.AddAndPlayScenario( SCENARIO_PATH+'Splash.sce' ); // launch anim. on it

 SHFallInWater.Play( TRUE );  // play sound 'splash'

 LaunchHelicopterRescue( AX, AY );

end;

procedure TGame.PlayerCarCrash;
var o: TParticleEmitter;
begin
 if FPlayerCarCrashed then exit;
 FPlayerCarCrashed := TRUE;

 PlayerCar.FrameAddPerSecond( 0 );
 PlayerCar.SetFrameLoopBounds(0, 0);
 PlayerCar.Frame := 5;
 RoadTileEngine.ScrollSpeed.y.Value := 0;
 SHCarEngineLoop.Stop;        // stop player car engine audio

 KillAllVehicleWhenPlayerLose;

 SHCarCrash.Play( TRUE );

 // create two particles emitter for fire and smoke
 o := TParticleEmitter.Create;
 o.LoadFromFile( PARTICLES_PATH+'CarCrashedSmoke.par' );
 o.BindToSprite( PlayerCar, 0, 0 );
// o.SetCenterCoordinate( PlayerCar.CenterX, PlayerCar.CenterY );
 if PlayerCar.ParentLayer = FScene.Layer[LayerPlayer]
   then FScene.Add( o, LayerPlayer )
   else FScene.Add( o, LayerPlayer2 );
 o.AddAndPlayScenario( SCENARIO_PATH+'CarCrashSmoke.sce' ); // launch anim. on it


 PlayerCar.PlayScenario( scePlayerCarCrash );

 LaunchHelicopterRescue( PlayerCar.CenterX, PlayerCar.CenterY );

end;

procedure TGame.PlayerCarLaunchJump;
begin
 case JumpState of
  0: begin
   PlayerCar.PlayScenario( scePlayerCarJump, TRUE );
   SHCarJump.Play;
   SHCarJump.Volume.Value := 600;
  end;
  9: begin
   PlayerCar.PlayScenario( sceCarjumpFromSecondFloor, TRUE );
   SHCarJump.Play;
   SHCarJump.Volume.Value := 600;
  end;
 end;


end;

procedure TGame.playerCarAbortJump;
begin
 PlayerCar.StopScenario( scePlayerCarJump );
 PlayerCar.Scale.ChangeTo( PointF(1,1), 0.1 );
 SHCarJump.Stop;
end;

procedure TGame.PlayerCarCollisionTest;
var i, CornerCountInWater: integer;
    SurfaceCollided: TSimpleSurfaceWithEffect;
    flag_playerInGrass, flag_playerInWater, flag: boolean;
    coef, delta: single;
    p, PosPlayerInWater: TPointF;

    procedure TestPlayerCarWithNonRoadTile( AOffsetX, AOffsetY: integer );
    var pp: TPointF;
    begin
     pp := PointF( PlayerCar.x.Value + AOffsetX, PlayerCar.y.Value + AOffsetY );
      case RoadTileEngine.GetGroundType( pp ) of
       GROUND_GRASS : flag_playerInGrass := TRUE;
       GROUND_WATER, GROUND_HOLE : begin
         flag_playerInWater := TRUE;
         PosPlayerInWater := pp;
         inc( CornerCountInWater );
       end;
      end;//case
    end;
begin

 // if any, take sprite collided with player car
 SurfaceCollided := FScene.Layer[LayerSprite].CollisionTest( PlayerCar.X.Value+15, PlayerCar.Y.Value+15, PlayerCar.Width-30, PlayerCar.Height-30);
 if SurfaceCollided <> NIL then
    if SurfaceCollided.Group = GroupEndRace
      then begin
        PlayerCarWinRaceAnimation; // player car crossed the finish line
        exit;
       end;


 //  collision test with second floor tile engine (when player car jump)
 if TileEngineSecondFloor <> NIL then
 begin
  case JumpState of
   2: begin  // here, the player car start to jump: we abort jump if there is a roof above the car
          if SecondFloorGroundIsRoof( PointF(PlayerCar.CenterX, PlayerCar.Y.Value)) or
             SecondFloorGroundIsRoof( PointF(PlayerCar.CenterX, PlayerCar.BottomY)) or
             SecondFloorGroundIsCrash( PointF(PlayerCar.CenterX, PlayerCar.Y.Value)) or
             SecondFloorGroundIsCrash( PointF(PlayerCar.CenterX, PlayerCar.BottomY))
            then begin
               PlayerCarAbortJump;
               JumpState := 0;
             end;
   end;

   4: begin  // here, player car come down from the air: it may land or crash on second floor
    if SecondFloorGroundIsRoof( PointF(PlayerCar.CenterX, PlayerCar.Y.Value)) or
       SecondFloorGroundIsRoof( PointF(PlayerCar.CenterX, PlayerCar.BottomY))
      then begin  // player car land on roof tile on second floor
            p := PlayerCar.Scale.Value;
            PlayerCarAbortJump;
            PlayerCar.Scale.Value := p; // stop scale change
            JumpState := 9;   // means player car land on second floor and drive on it
       end else
    if SecondFloorGroundIsCrash( PointF(PlayerCar.CenterX, PlayerCar.Y.Value)) or
       SecondFloorGroundIsCrash( PointF(PlayerCar.CenterX, PlayerCar.BottomY))
      then begin
            PlayerCarCrash;  // player car land on prohibited tile on second floor (like tunnel)
            exit;
       end;
   end;

   9: begin // player car drive on second floor: we test if it fall down
    if not SecondFloorGroundIsRoof( PointF(PlayerCar.CenterX, PlayerCar.Y.Value+15)) and
       not SecondFloorGroundIsRoof( PointF(PlayerCar.CenterX, PlayerCar.BottomY-15))
     then if not PlayerCar.ScenarioIsPlaying( scePlayerCarFallFromSecondFloor )
            then PlayerCar.PlayScenario( scePlayerCarFallFromSecondFloor );
   end;

   11: begin  // player car comes down from second floor jump: it can land on roof or fall down on road
     if not SecondFloorGroundIsRoof( PointF(PlayerCar.CenterX, PlayerCar.Y.Value+15)) and
        not SecondFloorGroundIsRoof( PointF(PlayerCar.CenterX, PlayerCar.BottomY-15))
      then begin
            PlayerCar.PlayScenario( scePlayerCarFallAfterSecondFloorJump );
           end
     else JumpState := 9;   // means player car land on second floor and drive on it
   end;

  end;//case
 end;

 if (JumpState=0) and not FGameEnded then
 begin
  // Collision test player car with road tile engine
  CornerCountInWater := 0;
  flag_playerInGrass := FALSE;
  flag_playerInWater := FALSE;
  TestPlayerCarWithNonRoadTile( PlayerCar.Width - 20, PlayerCar.Height - 30 );  // bottom right
  TestPlayerCarWithNonRoadTile( 20, PlayerCar.Height - 30 );   // bottom left
  TestPlayerCarWithNonRoadTile( 20, 30 );  // top left
  TestPlayerCarWithNonRoadTile( PlayerCar.Width - 20, 30 ); // top right

  if flag_playerInWater then
  begin
    if JumpState = 0 then
    begin
               if CornerCountInWater = 4
                  then PlayerCarFallInWater( PlayerCar.CenterX, PlayerCar.CenterY )
                  else PlayerCarFallInWater( PosPlayerInWater.x, PosPlayerInWater.y );
               exit;
    end;
  end else
  if flag_playerInGrass and not PlayerCarScenarioCollisionIsRunning then
  begin
    PlayersCarHitAnObstacle( FALSE, -200 );
    AddToScrollingSpeed( -300 );
  end;
 end;

 if (JumpState<>0) or FGameEnded or PlayerCarScenarioCollisionIsRunning
   then exit;   // no collision test

  // Collision test with sprites on LAYER_SPRITE
  if SurfaceCollided <> NIL
    then begin
          if SurfaceCollided.Group = GroupStartRace then exit ;
          if LastSpriteCollided <> SurfaceCollided
            then begin
                  case SurfaceCollided.Group of

                   GroupOilPuddle: begin
                     SHTiresScreechingLong.Play(TRUE);
                     PlayerCar.PlayScenario( scePlayerCarSkid );
                     if random( 100 ) > 50
                       then PlayerCar.MoveRelative( -150, 0, 0.9 )
                       else PlayerCar.MoveRelative(  150, 0, 0.9 );
                     PlayerCar.Speed.x.Value := 0;
                   end;

                   else begin  // other vehicles
                         flag := (SurfaceCollided.Group <> GroupBigTreeBottom);

                         if SurfaceCollided.Group = GroupStrawBale
                           then begin
                                 SHCarCollideStrawBale.Play( TRUE );
                                 CreateBonusScore( 100 );
                           end
                           else begin
                             PlayersCarHitAnObstacle( flag, -300 );
                             AddToScrollingSpeed( -300 );
                           end;

                         coef := RoadTileEngine.ScrollSpeed.y.Value/VSPEED_MAX;
                         delta := 120*coef;
                         if delta < 30 then delta := 30;

                         if PlayerCar.Speed.x.Value < 0.0 then
                         begin
                           // we shift vehicles when they collide
                           if PlayerCar.X.Value >= SurfaceCollided.CenterX
                             then begin // shift player car to the right
                               PlayerCar.MoveXRelative( delta, 0.5, idcStartFastEndSlow );
                               if flag then SurfaceCollided.MoveXRelative( -20, 1-coef, idcStartFastEndSlow );
                             end
                             else begin // shift player car to the left
                               PlayerCar.MoveXRelative( -delta, 0.5, idcStartFastEndSlow );
                               if flag then SurfaceCollided.MoveXRelative( 20, 1-coef, idcStartFastEndSlow );
                             end;
                         end else
                         begin
                           if PlayerCar.RightX <= SurfaceCollided.CenterX
                             then begin
                                   PlayerCar.MoveXRelative( -delta, 0.5, idcStartFastEndSlow );
                                   if flag then SurfaceCollided.MoveXRelative( 20, 1-coef, idcStartFastEndSlow );
                             end
                           else begin
                            PlayerCar.MoveXRelative( delta, 0.5, idcStartFastEndSlow );
                            if flag then SurfaceCollided.MoveXRelative( -20, 1-coef, idcStartFastEndSlow );
                           end;
                         end;

                        end;
                  end;//case
                  LastSpriteCollided := SurfaceCollided;
                 end;
         end
    else begin
          LastSpriteCollided := NIL ;
         end;

 // collision test player car versus train
 SurfaceCollided := FScene.Layer[LayerTrain].CollisionTest( PlayerCar.X.Value+20, PlayerCar.Y.Value+20, PlayerCar.Width-40, PlayerCar.Height-40  );
 if SurfaceCollided <> NIL then
 begin
  PlayerCarCrash;
  exit;
 end;


 // Collision test player car versus number
 SurfaceCollided := FScene.Layer[LayerTable].CollisionTest( PlayerCar.X.Value+20, PlayerCar.Y.Value+20, PlayerCar.Width-40, PlayerCar.Height-40  );
 if SurfaceCollided <> NIL
   then begin
         if SurfaceCollided.Tag1 < 1000   // tag1=1000 means number has been won
           then begin
                 if (( SurfaceCollided.Tag1 mod CurrentTable ) = 0) and
                    (( SurfaceCollided.Tag1 div CurrentTable ) <= 10) and
                    ( WinCount < 11 )
                   then begin
                         i := SurfaceCollided.Tag1 div CurrentTable;
                         if not TableNumberFlag[i]
                           then begin
                                 SHTableWin.Play( TRUE ); // play sound 'bling'
                                 // launch blink and flash on number line won
                                 TableLine[i].Caption := TableLine[i].Caption + ' ' + inttostr(SurfaceCollided.Tag1);
                                 TableLine[i].Blink( 5, 0.25, 0.25);
                                 TableLine[i].Tint.Value := BGRA(255,255,255,0);
                                 TableLine[i].Tint.Alpha.ChangeTo(220, 1);
                                 TableNumberFlag[i] := TRUE;  // this number is won
                                 inc( WinCount );
                                 // launch animation on sprite number won (on road)
                                 SurfaceCollided.Scale.ChangeTo(PointF(3,3),1);
                                 SurfaceCollided.Opacity.ChangeTo(0,1);
                                 SurfaceCollided.Angle.AddConstant(360);
                                 SurfaceCollided.Tag1:=1000;  // mean sprite is won
                                 CreateBonusScore( 100 );
                                 if WinCount=11
                                   then begin  // player have won all number in current multiplication table
                                         CurrentMaxSpeed := VSPEED_MAX ; // enable full speed
                                         for i:=0 to 10 do  // launch color animation on each line of multiplication table
                                          TableLine[i].AddAndPlayScenario( Application.Location + SCENARIO_PATH+'TableAllWin.sce' );
                                         // show 'full speed enable' message on screen
                                         SpriteFullSpeed.PlayScenario( SpriteFullSpeed.AddScenario(Application.Location + SCENARIO_PATH+'FullSpeedMessage.sce') );
                                         // delete all sprite number
                                         FScene.Layer[LayerTable].KillAll;
                                   end
                                end;
                          end
                     else begin
                           SurfaceCollided.AddAndPlayScenario( Application.Location+SCENARIO_PATH+'TableLost.sce');
                           SurfaceCollided.Tag1 := 2000;// Means that the sprite has already been hit by the player's car
                           SHTableLost.Play( TRUE );
                           PlayersCarHitAnObstacle( FALSE, -500 );
                           AddToScrollingSpeed( -300 );
                          end;
                 LastTableCollided := SurfaceCollided ;
                 end;
        end
end;


procedure TGame.AddToScore(aBonus: integer);
begin
 FScore += ABonus;
 PlayerScore.Caption := 'Score: ' + inttostr( FScore );
end;

procedure TGame.CreateBonusScore(ABonus: integer);
var o: TGUILabel;
begin
 AddToScore( ABonus );

 o := TGUILabel.Create;
 with o do begin
       Font := FontScore;
       Caption := inttostr( ABonus );
       if ABonus < 0
         then begin
               Caption := inttostr( ABonus );
               Tint.Value := BGRA(255,0,0,100);
         end
         else begin
               Caption := '+' + inttostr( ABonus );
               Tint.Value := BGRA(0,255,0,100);
         end;
       SetCoordinate( PlayerCar.RightX, PlayerCar.CenterY );
       MoveTo(PlayerScore.CenterX, PlayerScore.CenterY, 1, idcSinusoid );
       Opacity.ChangeTo( 0, 1, idcStartSlowEndFast );
       KillDefered( 1 );
 end;
 FScene.Add( o, LayerScore );
end;

procedure TGame.CreateBonusSpeed;
var o: TGUILabel;
begin
 AddToScore( SPEED_BONUS );
 o := TGUILabel.Create;
 with o do begin
  Font := FontScore;
  Caption := '+' + inttostr( SPEED_BONUS );
  Tint.Value := BGRA(255,255,0,100);
  SetCoordinate( FSpeedBar.CenterX, FSpeedBar.Y.Value );
  X.ChangeTo( PlayerScore.CenterX, 1, idcStartSlowEndFast );
  Y.ChangeTo( PlayerScore.CenterY, 1, idcStartFastEndSlow );
  Opacity.ChangeTo( 0, 1, idcStartSlowEndFast );
  KillDefered( 1 );
 end;
 FScene.Add( o, LayerScore );
 FSpeedBar.Tint.Value := BGRA(255,255,255,255);
 FSpeedBar.Tint.Alpha.ChangeTo( 0, 0.5 );
end;

procedure TGame.CreateRescueMalus(aTxt: string; aMalus: integer);
var o: TGUILabel;
begin
 AddToScore( aMalus );
 o := TGUILabel.Create;
 with o do begin
  Font := FontScore;
  Caption := UTF8ToSys(aTxt + ' ' + inttostr( aMalus ));
  Tint.Value := BGRA(255,0,0,100);
  SetCenterCoordinate( PlayerCar.CenterX, PlayerCar.Y.Value - 50 );
  MoveYRelative( -300, 3, idcSinusoid );
  Opacity.ChangeTo( 0, 3, idcStartSlowEndFast );
  KillDefered( 3 );
 end;
 FScene.Add( o, LayerScore );
end;

procedure TGame.CreateStartGrid;
var i: integer;
    p: TPointF;
    o: TSprite;
begin
 for i:=0 to RoadTileEngine.MapTileCount.cx-1 do
  begin
   p := PointF( RoadTileEngine.X.Value+RoadTileEngine.TileSize.cx*i, RoadTileEngine.TileSize.cy*6 );
   if self.IsOnRoadType( p ) then
   begin
         o := TSprite.Create( TexStartRace );
         o.SetCoordinate( p.x, p.y );
         o.Group := GroupStartRace;
         o.Opacity.Value:=200;
         FScene.Add( o , LayerSprite );
   end;
  end;
end;


procedure TGame.CreateEndGrid;
const GRID_LINE_COUNT = 10;
var p: TPointF;
    {flag: boolean;
    c,} xx, yy: integer;
    xxx, yyy: single;
    o: TSprite;
begin
{
 // Looking forward for continuous road area to draw end grid.
 // Keep in mind that the map repeats itself vertically ( RoadTileEngine.VLoopMode property is set to TRUE )
 p := PointF( RoadTileEngine.X.Value, RoadTileEngine.Y.Value );
 c := 0;
 repeat
  p.y := p.y - RoadTileEngine.TileSize.cy;
  p.x := RoadTileEngine.X.Value;
  flag := FALSE;
  repeat
   if IsOnRoadType( p )
     then begin
           flag := TRUE;
           break;
     end;
   p.x := p.x + RoadTileEngine.TileSize.cx;
  until p.x > RoadTileEngine.RightX;
  if flag then inc ( c )
          else c := 0;
 until c = GRID_LINE_COUNT+2;
 p.y := p.y + RoadTileEngine.TileSize.cy;
}
 p := PointF(RoadTileEngine.X.Value, RoadTileEngine.Y.Value );
 // create end race sprites
 for xx:=0 to RoadTileEngine.Width div RoadTileEngine.TileSize.cx - 1 do
  for yy:=1 to GRID_LINE_COUNT do
   begin
    xxx := RoadTileEngine.X.Value + RoadTileEngine.TileSize.cx * xx;
    yyy := p.y - RoadTileEngine.TileSize.cy * yy;
    if IsOnRoadType( PointF( xxx, yyy )) then
    begin
         o := TSprite.Create( TexStartRace );
         o.SetCoordinate( xxx, yyy );
         o.Group := GroupEndRace;
         o.AddAndPlayScenario( SCENARIO_PATH+'EndRaceGrid.sce' );
         FScene.Insert( 0, o, LayerSprite );
    end;
   end;
 FEndGridCreated := TRUE;
end;

procedure TGame.CreateBigTree;
var o1, o2, o3: TSprite;
begin

 o1 := TSprite.create( TexBigTreeBottom );
 SetCoordinateOnTheRoad( o1 );
 o1.Group := GroupBigTreeBottom;
 FScene.Add( o1, LayerSprite );

 o2 := TSprite.create( TexBigTreeMiddle);
 o2.SetCoordinate( o1.X.Value, o1.Y.Value - o2.Height );
 o2.Group := GroupBigTreeMiddle;
 FScene.Add( o2, LayerOverRoad );

 o3 := TSprite.create( TexBigTreeTop);
 o3.SetCoordinate( o2.X.Value-o3.Width*0.5+o2.Width*0.5, o2.Y.Value - o3.Height+60+random(60) );
 o3.Group := GroupBigTreeMiddle;
 FScene.Add( o3, LayerOverRoad );
end;

procedure TGame.CreateStrawBale;
var o: TSprite;
begin
 o := TSprite.create( TexStraw );
 SetCoordinateOnTheRoad( o );
// o.SetCenterCoordinate( -o.Width, random( FScene.Height ) );
// o.Tag1 := 200 + random( 100 );
// o.Angle.AddConstant( o.Tag1 );
// o.X.AddConstant( o.Tag1 );
 o.Group := GroupStrawBale;
 FScene.Add( o, LayerSprite );
end;



procedure TGame.PlayersCarHitAnObstacle( AHorn: boolean; AScoreMalus: integer );
begin
 if not PlayerCarScenarioCollisionIsRunning
   then begin
         if AHorn then
           case random(4) of
            0 : SHHorn1.Play(TRUE);
            1 : SHHorn2.Play(TRUE);
            2 : SHHorn3.Play(TRUE);
            3 : SHHorn4.Play(TRUE);
           end;//case
         SHCarCollideVehicle.Play(TRUE);

         CreateBonusScore( AScoreMalus );

         if FNightIsON
           then PlayerCar.PlayScenario( scePlayerCarCollisionNight )
           else PlayerCar.PlayScenario( scePlayerCarCollision );
        end;
end;

function TGame.PlayerCarScenarioCollisionIsRunning: boolean;
begin
 Result := PlayerCar.ScenarioIsPlaying( scePlayerCarCollision ) or
           PlayerCar.ScenarioIsPlaying( scePlayerCarCollisionNight );
end;


procedure TGame.SetPlayerCarEngineAudioFrequency;
begin
 // player car engine audio frequency follow tile engine scrolling speed
 with SHCarEngineLoop do
  Pitch.Value := 1 + RoadTileEngine.ScrollSpeed.y.Value / VSPEED_MAX * 2;
end;

procedure TGame.AddToScrollingSpeed(AValue: single);
begin
 with RoadTileEngine.ScrollSpeed.y do begin
   Value := Value + AValue;
   if Value < 0 then Value := 0;
   if Value > CurrentMaxSpeed then Value := CurrentMaxSpeed;
 end;

 PlayerCar.FrameAddPerSecond( RoadTileEngine.ScrollSpeed.y.Value * 0.1 );
end;

procedure TGame.AddToPlayerCarSpeedX(AValue: single);
begin
 PlayerCar.Speed.x.Value := PlayerCar.Speed.x.Value + AValue;
end;

procedure TGame.StartWarningBeep;
begin
 if FGameEnded then exit;
 if PlayerHasLost then exit;

 if not FWarning.ScenarioIsPlaying( sceWarning )
   then FWarning.PlayScenario( sceWarning );
end;

procedure TGame.StopWarningBeep;
begin
 FWarning.Visible := FALSE;
 FWarning.StopScenario( sceWarning );
 SHWarning.Stop;
end;

procedure TGame.CreateFog;
begin
 FogTileEngine := TTileEngine.Create;
 FScene.Layer[LayerCloud].AssignTileEngine( FogTileEngine, TRUE ); // TRUE mean auto free
 with FogTileEngine do begin
  LoadMapFile('DATA\Map\Cloud.map');
  SetViewSize( 13*64, FScene.Height );
  SetCoordinate( (FScene.Width - Width) / 2, 0 );
  ScrollSpeed.Value := PointF(-30, 60);
  Opacity.Value := 0;
 end;
 FFogCreated := TRUE;
 FFogVisible := FALSE;
// FogTileEngine.BlendMode := FX_BLEND_ADD;
end;

procedure TGame.DoFogAnimation;
begin
 if FFogVisible then
 begin
       if FogTileEngine.ScrollSpeed.x.State = psNO_CHANGE  // mean last 'Changeto' command is terminated
         then FogTileEngine.ScrollSpeed.x.ChangeTo( random (200)-100, random(3000)*0.001+0.5, idcSinusoid );

       FogTileEngine.ScrollSpeed.y.Value := 60 + RoadTileEngine.ScrollSpeed.y.Value/2;

     //  if FogTileEngine.Opacity.State = psNO_CHANGE
     //    then FogTileEngine.Opacity.ChangeTo(80 + random(600)*0.1, random(5000)*0.001+2, idcSinusoid );
 end else
 begin
  if FogTileEngine.Opacity.State = psNO_CHANGE
         then FogTileEngine.Opacity.ChangeTo( 0, 3, idcSinusoid );
 end;
end;

procedure TGame.createNight;
var o: TColorBackground;
    yplayercar: single;
    function CreateNightSquare( AWidth, AHeight: integer): TColorBackground;
    begin
     Result := TColorBackground.Create(0, 0, AWidth, AHeight );
     Result.SetAllColorsTo( BGRA(0,0,0,255) );
     Result.Opacity.Value := 220;
    end;

begin
 PlayerCar.Tint.Value := BGRA(0,0,0,150);

 yplayercar := Fscene.Height-250;

 CarHeadLight := TSprite.create( TexPlayerCarHeadLight );
 CarHeadLight.SetCoordinate( 0, yplayercar );
// PlayerCar.AddChild( CarHeadLight, PointF( -CarHeadLight.Width/2+PlayerCar.Width/2, -CarHeadLight.Height+PlayerCar.Height+3 ) );
 CarHeadLight.BindToSprite( PlayerCar, -CarHeadLight.Width/2+PlayerCar.Width/2+5, -CarHeadLight.Height+PlayerCar.Height-10 );
 CarHeadLight.Opacity.Value := 220;
 FScene.Add( CarHeadLight, LayerNight );
// FScene.Insert( 0, CarHeadLight, LayerPlayer );

 NightObjectArray[0] := CarHeadLight;
 // left square
 o := CreateNightSquare( round(FScene.Width/2), FScene.Height+300 );
 o.SetCoordinate( -o.Width, CarHeadLight.Height/2-o.Height/2 );
 CarHeadLight.AddChild( o );
 NightObjectArray[1] := o;
 // right square
 o := CreateNightSquare( round(FScene.Width/2), FScene.Height+300 );
 o.SetCoordinate( CarHeadLight.Width, CarHeadLight.Height/2-o.Height/2 );
 CarHeadLight.AddChild( o );
 NightObjectArray[2] := o;
 // bottom square
 o := CreateNightSquare( CarHeadLight.Width, round(CarHeadLight.Y.Value+50) );
 o.SetCoordinate( 0, CarHeadLight.Height );
 CarHeadLight.AddChild( o );
 NightObjectArray[3] := o;
 // top square
 o := CreateNightSquare( CarHeadLight.Width, round(CarHeadLight.Y.Value+50) );
 o.SetCoordinate( 0,-o.Height );
 CarHeadLight.AddChild( o );
 NightObjectArray[4] := o;

end;

procedure TGame.SetNight(Percent, Seconds: single);
var i: integer;
begin
 if PlayerHasLost and ( Percent > 0.0 ) then exit;

 if ( JumpState >= 3 ) and ( Percent > 0.0 ) then exit;

 for i:=0 to 4 do NightObjectArray[i].Opacity.ChangeTo( 255*Percent, Seconds );

 PlayerCar.Tint.Value := BGRA(0,0,0,round(150*Percent));

 FNightIsON := Percent > 0.0;

 if TileEngineSecondFloor <> NIL
   then if FNightIsON
          then TileEngineSecondFloor.Opacity.ChangeTo( 0, Seconds )
          else TileEngineSecondFloor.Opacity.ChangeTo( 255, Seconds );
end;

procedure TGame.CreateRain;
begin
 FRainCreated := TRUE;
 FRainParticleEmitter := TParticleEmitter.Create;
 with FRainParticleEmitter do
  begin
   LoadFromFile(PARTICLES_PATH+'Rain.par');
   SetSize( RoadTileEngine.Width, RoadTileEngine.Height );
   SetCoordinate( RoadTileEngine.X.Value, RoadTileEngine.Y.Value );
   ParticlesToEmit.Value := 0;
 end;
 FScene.Add( FRainParticleEmitter, LayerRainSnow );
 FRainVisible := FALSE;
end;

procedure TGame.DoRainAnim;
begin
 if FRainVisible then
 begin
       if FRainParticleEmitter.ParticlesToEmit.State = psNO_CHANGE
         then FRainParticleEmitter.ParticlesToEmit.ChangeTo( 200 + random(800), 3 + random(5) );
       FRainParticleEmitter.Gravity.y.Value := RoadTileEngine.ScrollSpeed.y.Value;
 end else if FRainParticleEmitter.ParticlesToEmit.State=psNO_CHANGE
            then FRainParticleEmitter.ParticlesToEmit.ChangeTo( 0, 3 );
end;

procedure TGame.CreateSnow;
var ima, temp: TBGRABitmap;
begin
 FSnowCreated := TRUE;

 ima := TBGRABitmap.Create(SPRITE_PATH+'SnowFlake.png');
 temp := TBGRABitmap( ima.Resample( 18, 18 ));  //24

 FSnow := TSnow.Create;
 with FSnow do
 begin
  SetSize( RoadTileEngine.Width, RoadTileEngine.Height );
  UseTexture( temp );
  FlakeAmount.Value := 700;
  FlakeSpeed.Value := PointF(4, 80);
  HAmplitude.Value := 20;
  SetCoordinate( RoadTileEngine.X.Value, RoadTileEngine.Y.Value );
 end;
 FScene.Add( FSnow, LayerRainSnow );

 ima.Free;
 temp.free;
end;

procedure TGame.DoSnowAnim;
begin
 FSnow.Gravity.y.Value := RoadTileEngine.ScrollSpeed.y.Value;

 if FSnow.Gravity.x.State = psNO_CHANGE
    then FSnow.Gravity.x.ChangeTo( random( 500 )-250, random(5)+2, idcStartFastEndSlow );

// if FSnow.FlakeAmount.State = psNO_CHANGE
//    then FSnow.FlakeAmount.ChangeTo( random( 700 )+200, random( 1 )+5, idcSinusoid );
end;

procedure TGame.CreateBackGroundAnimation( MapFile: string );
begin
 BGAnimTileEngine := TTileEngine.Create;
 FScene.Layer[LayerBGAnim].AssignTileEngine( BGAnimTileEngine );
 with BGAnimTileEngine do
  begin
   LoadMapFile( MAP_PATH+MapFile );
   SetCoordinate( RoadTileEngine.X.Value, RoadTileEngine.Y.Value );
   SetViewSize( RoadTileEngine.Width, RoadTileEngine.Height );
  end;
end;

procedure TGame.CreateOverRoadTileEngine(MapFile: string);
begin
 TileEngineSecondFloor := TTileEngine.Create;
 FScene.Layer[LayerOverRoad].AssignTileEngine( TileEngineSecondFloor );
 with TileEngineSecondFloor do
  begin
   LoadMapFile( MAP_PATH+MapFile );
   SetViewSize( RoadTileEngine.Width, RoadTileEngine.Height );
   SetCoordinate( RoadTileEngine.X.Value, RoadTileEngine.Y.Value );
  end;
end;

procedure TGame.CreateShadowsTileEngine(MapFile: string);
begin
 FFshadowsTileEngine := TTileEngine.Create;
 FScene.Layer[LayerShadows].AssignTileEngine( FFshadowsTileEngine );
 with FFshadowsTileEngine do
  begin
   LoadMapFile( MAP_PATH+MapFile );
   SetViewSize( RoadTileEngine.Width, RoadTileEngine.Height );
   SetCoordinate( RoadTileEngine.X.Value, RoadTileEngine.Y.Value );
  end;
end;

procedure TGame.ColoredSquareAnim;
var c: TBGRApixel;
begin
 if FColoredSquare.TopLeftColor.State <> psNO_CHANGE then exit;


 // clock wise rotation colors
 c := FColoredSquare.TopLeftColor.Value;
 FColoredSquare.TopLeftColor.ChangeTo( FColoredSquare.BottomLeftColor.Value, 1.5 );
 FColoredSquare.BottomLeftColor.ChangeTo( FColoredSquare.BottomRightColor.Value, 1.5 );
 FColoredSquare.BottomRightColor.ChangeTo( FColoredSquare.TopRightColor.Value, 1.5 );
 FColoredSquare.TopRightColor.ChangeTo( c, 1.5 );
end;

procedure TGame.CreateTrain( aCenterX: single; Pos, WagonCount: integer );
var i, hhead, hwagon, hqueue: integer;

begin
 if WagonCount < 2 then WagonCount := 2;

 SetLength( FTrain, WagonCount );

 // train head
 FTrain[0] := TVehicle.create( TexTgvHead );
 with FTrain[0] do begin
  CenterX := aCenterX;
  Group := GroupTrainHead;
 end;
 FScene.Add( FTrain[0], LayerTrain );

 hhead := FTrain[0].Height;
 hwagon := TexTgvWagon^.ImageHeight;
 hqueue := TexTgvQueue^.ImageHeight;

 // wagon and queue
 for i:=1 to WagonCount-1 do
  begin
   if i = WagonCount - 1
     then FTrain[i] := TVehicle.create( TexTgvQueue )
     else FTrain[i] := TVehicle.create( TexTgvWagon );
   FTrain[i].Group := GroupTrainWagon;
   FTrain[i].BindToSprite( FTrain[0], 0, hhead + hwagon * (i - 1) );
   FScene.Add( FTrain[i], LayerTrain );
  end;

 FTrainHeight := hhead + WagonCount * hwagon + hqueue;
 with FTrain[0] do
   case Pos of
    -1: begin
       Y.Value := -FTrainHeight*2;
       NeededYSpeed := -800;
     end;
     0: begin
       FTrain[0].Y.Value := 0;
       NeededYSpeed := 800;
     end;
     1: begin
       FTrain[0].Y.Value := FScene.Height+FTrainHeight*2;
       NeededYSpeed := 800;
     end;
   end;//case

 FtrainAudioFadeOutInProgress := FALSE;
end;

procedure TGame.TrainAnim;
    function IsOnScene( aY: single ): boolean;inline;
    begin
     Result := ( aY > -100 ) and ( aY < FScene.Height+100 );
    end;
begin
 // invert train speed if needed
 if FTrain[0].Y.Value < -FTrainHeight*2 then
 begin
  FTrain[0].NeededYSpeed := -FTrain[0].NeededYSpeed;
  FTrain[0].Y.Value := -FTrainHeight*2+1;
 end;

 if FTrain[0].Y.Value > FScene.Height + FTrainHeight*2 then
 begin
  FTrain[0].NeededYSpeed := -FTrain[0].NeededYSpeed;
  FTrain[0].Y.Value := FScene.Height + FTrainHeight*2-1;
 end;

 with FTrain[0] do Speed.y.Value := -( NeededYSpeed - RoadTileEngine.ScrollSpeed.y.Value );

 // train sound
 if IsOnScene( FTrain[0].Y.Value ) or
    IsOnScene ( FTrain[0].Y.Value + FTrainHeight ) or
    ( (FTrain[0].Y.Value<0) and (FTrain[0].Y.Value + FTrainHeight>=FScene.Height) )
   then PlayTrainSound
   else StopTrainSound;
end;

procedure TGame.PlayTrainSound;
begin
 if SHTrainPassing.State = AL_PLAYING then exit;
 SHTrainPassing.FadeIn( 1000, 2 );
 FtrainAudioFadeOutInProgress := FALSE;
end;

procedure TGame.StopTrainSound;
begin
 if SHTrainPassing.State = AL_STOPPED then exit;
 if FtrainAudioFadeOutInProgress then exit;

 SHTrainPassing.FadeOut( 2, idcStartFastEndSlow );
 FtrainAudioFadeOutInProgress := TRUE;
end;

procedure TGame.MoveScoreToSceneCenter;
begin
 with PlayerScore do
 begin
   MoveTo( (FScene.Width-PlayerScore.Width)/2, FScene.Height/2-100, 1.5, idcSinusoid2 );
   Scale.ChangeTo(PointF(2,2), 3 );
 end;

 with FLabelStage do
 begin
   MoveTo( (FScene.Width-FLabelStage.Width)/2, FScene.Height/2-200, 1.5, idcSinusoid2 );
   Scale.ChangeTo(PointF(2,2), 3 );
 end;
end;

procedure TGame.PlayerCarWinRaceAnimation;
var g1, g2: TGlow;
begin
 FGameEnded := TRUE;
 StopWarningBeep;
 RoadTileEngine.ScrollSpeed.ChangeTo( PointF(0,0), 0.3, idcStartFastEndSlow );  // stop road scrolling
 with PlayerCar do begin
   Speed.Value := PointF(0,0);  // stop left/right speed
   StopAllScenario;             // if they are running, stop jump or collision scenario
   Scale.ChangeTo( PointF(1,1), 0.3, idcStartFastEndSlow); // if car is jumping, restore its original size (put it on the floor)
   if FNightIsON  // if car is red flash (after collision), remove red color
      then Tint.Value := BGRA(0,0,0,150)
      else Tint.Value := BGRA(0,0,0,0);
   FrameAddPerSecond( 0 ); // stop wheel anim on player car
   AddAndPlayScenario( SCENARIO_PATH+'EndRaceSkidPlayerCar.sce' ); // launch end game scenario on player car
 end;

 SHTiresScreechingLong.Play( TRUE );   // launch tire screech sound
 SHCarEngineLoop.FadeOut( 2 );         // volume fadeout and stop car engine sound


 // Create two glows for the headlights of the car
 g1 := TGlow.Create( 0, 0, 40, BGRA(255,255,150,200) );
 g1.AddAndPlayScenario( SCENARIO_PATH+'EndRaceLightPlayerCar.sce' );
 g1.BindToSprite( PlayerCar, 18, PlayerCar.Height-8 );
 FScene.Add( g1, LayerPlayer );

 g2 := TGlow.Create( 0, 0, 40, BGRA(255,255,150,200) );
 g2.AddAndPlayScenario( SCENARIO_PATH+'EndRaceLightPlayerCar.sce' );
 g2.BindToSprite( PlayerCar, PlayerCar.Width-18, PlayerCar.Height-8 );
 FScene.Add( g2, LayerPlayer );

 MoveScoreToSceneCenter;
 SHMusicEndStage.Volume.Value := 1000;
 SHMusicEndStage.Play( TRUE );
 FScene.ExecuteDuring( 7 );

 inc( FCurrentStage );
 if FCurrentStage > 9 then
 begin
   FCurrentStage := 1;
   ResetTableDone;
 end;
 case FCurrentStage of
  1..9: FScene.LaunchStage( Game )
  else FScene.LaunchStage( Intro );
 end;//case
end;

procedure TGame.LaunchHelicopterRescue(AX, AY: single);
var c, cc: integer;
    xbeginroad, xendroad: single;
    flag: boolean;
    HelicoScenario: TScenario;
begin
 // day light
 if FNightIsON then SetNight( 0, 2 );

 // helicopter rescue sound
 SHHelico.Play( TRUE );
 SHHelico.Volume.Value := 0;
 SHHelico.Volume.ChangeTo( 1000, 3 );

 // look for continuous road area to drop player car
 FHelicopterDropCoor.y := AY;
 c := 0;
 cc := 0;
 repeat
  FHelicopterDropCoor.y := FHelicopterDropCoor.y + RoadTileEngine.TileSize.cy;
  FHelicopterDropCoor.x := RoadTileEngine.X.Value;
  flag := FALSE;
  xbeginroad := -1;
  repeat
   if IsOnRoadType( FHelicopterDropCoor ) and
      IsOnRoadType( FHelicopterDropCoor + PointF(PlayerCar.Width, 0 )) and
      IsOnRoadType( FHelicopterDropCoor + PointF(0, PlayerCar.Height )) and
      IsOnRoadType( FHelicopterDropCoor + PointF(PlayerCar.Width, PlayerCar.Height )) and

      IsOnRoadType( FHelicopterDropCoor + PointF(PlayerCar.Width*0.5, 0 )) and
      IsOnRoadType( FHelicopterDropCoor + PointF(PlayerCar.Width*0.5, PlayerCar.Height )) and

      IsHoleOnSecondFloor( FHelicopterDropCoor )
     then begin
           flag := TRUE;
           if xbeginroad = -1 then xbeginroad := FHelicopterDropCoor.x;
           break;
     end;
   FHelicopterDropCoor.x := FHelicopterDropCoor.x + RoadTileEngine.TileSize.cx;
  until FHelicopterDropCoor.x > RoadTileEngine.RightX;
  if flag
     then inc ( c )
     else c := 0;
  inc(cc);
 until (c >= FScene.Height div RoadTileEngine.TileSize.cy + 3) or (cc=500);

 // look for the horizontal middle of the road area
 xendroad := xbeginroad+RoadTileEngine.TileSize.cx;
 while IsOnRoadType( PointF( xendroad, FHelicopterDropCoor.y ) ) and
       IsOnRoadType( PointF( xendroad, FHelicopterDropCoor.y )+ PointF(PlayerCar.Width, 0 ) ) and
       IsOnRoadType( PointF( xendroad, FHelicopterDropCoor.y )+ PointF(0, PlayerCar.Height ) ) and
       IsOnRoadType( PointF( xendroad, FHelicopterDropCoor.y )+ PointF(PlayerCar.Width, PlayerCar.Height ) ) and
       IsOnRoadType( PointF( xendroad, FHelicopterDropCoor.y )+ PointF(PlayerCar.Width*0.5, 0 ) ) and
       IsOnRoadType( PointF( xendroad, FHelicopterDropCoor.y )+ PointF(PlayerCar.Width*0.5, PlayerCar.Height ) )
   do xendroad += 1;

 xendroad -= 1;
 FHelicopterDropCoor.x := xbeginroad + ( xendroad - xbeginroad ) * 0.5;

 if cc=500 then
 begin
  FHelicopterDropCoor.x := FScene.Width*0.5;
  FHelicopterDropCoor.y := AY - RoadTileEngine.TileSize.cy*2;
 end;

 // construct scenario for helicopter animation at running time
 HelicoScenario := TScenario.Create;

 // helicopter comes near player's car
 if aX < FScene.Width/2
   then begin
         HelicoScenario.AddLine('SetCoor '+inttostr(FScene.Width+500)+' '+'-300');
         HelicoScenario.AddLine('Angle 115');
   end
   else begin
         HelicoScenario.AddLine('SetCoor -500 -300');
         HelicoScenario.AddLine('Angle 65');
   end;

 HelicoScenario.AddLine('Visible TRUE');
 HelicoScenario.AddLine('MoveCenterTo '+FormatFloat('0.00', AX)+' '+FormatFloat('0.00',FScene.Height * PLAYER_CAR_POSY+80)+' 3 StartFastEndSlow');
 HelicoScenario.AddLine('Wait 2');

 // it rotate
 if aX < FScene.Width/2
   then HelicoScenario.AddLine('RotateTo 270 1.5 Sinusoid')      //270
   else HelicoScenario.AddLine('RotateTo -90 1.5 Sinusoid');
 HelicoScenario.AddLine('Wait 0.5');

 // it goes down
 HelicoScenario.AddLine('ScaleChange 0.78 1 Sinusoid');
 HelicoScenario.AddLine('Wait 2');

 // send an event so that player's car follow helicopter in the air
 HelicoScenario.AddLine('SendEvent 1');

 // helicopter goes up
 HelicoScenario.AddLine('ScaleChange 1 1 Sinusoid');
 HelicoScenario.AddLine('Wait 1');

 // send event 2 so that map scroll to landing position
 HelicoScenario.AddLine('SendEvent 2');

 // helicopter moves at the center of the road
 HelicoScenario.AddLine('MoveXCenterTo '+FormatFloat('0.00',FHelicopterDropCoor.x + PlayerCar.Width*0.5)+' 3 Sinusoid');
 HelicoScenario.AddLine('Wait 2.5');

 // it goes down
 HelicoScenario.AddLine('ScaleChange 0.8 1 Sinusoid');
 HelicoScenario.AddLine('Wait 1');

 // goes up and leaves the scene
 HelicoScenario.AddLine('ScaleChange 1 1 Sinusoid');
 HelicoScenario.AddLine('Wait 0.5');
 if AX < FScene.Width / 2
   then begin
         HelicoScenario.AddLine('MoveTo -500 -300 3 Sinusoid');
         HelicoScenario.AddLine('RotateTo 240 2 Sinusoid');
   end
   else begin
         HelicoScenario.AddLine('MoveTo '+inttostr(FScene.Width+500)+' -300 3 Sinusoid');
         HelicoScenario.AddLine('RotateTo -60 2 Sinusoid');
   end;

 // send event 3 so that rescue is finished and game resumes
 HelicoScenario.AddLine('SendEvent 3');
 HelicoScenario.AddLine('Wait 3');

 // send event 4 to make invisible the particle emitter for dust
 HelicoScenario.AddLine('SendEvent 4');

 // set the callback method for process the event
 HelicoScenario.OnEvent := @ProcessScenarioEvent;

 // add and play the scenario to the helicopter body ( main part of the sprites )
 FHelicoBody.AddAndPlayScenario( HelicoScenario );

 FHelicoDust.Visible := TRUE;;
end;


end.

