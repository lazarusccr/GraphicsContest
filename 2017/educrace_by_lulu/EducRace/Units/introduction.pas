unit Introduction;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, LCLIntf,
  BGRABitmapTypes, BGRABitmap,
  variable_parameter_V2,
  common,
  OGLCScene, OpenAlSoundManager,
  Language ;


type


{ TIntroduction }

TIntroduction = class (TStageSkeleton)
 // scenario
 sceVPlayerZigzag,
 sceVPlayerJump,
 sceTitleAnim : TIDScenario;

 FTimeToNextSprite: single;

 // instructions
 FPanel: TColorBackground;
 FKeysView: TSprite;

 procedure LoadData; override;
 procedure FreeData; override;
 procedure Update( AElapsedTime: single ); override;

 procedure ProcessGUIEvent(aGUISurface: TSimpleSurfaceWithEffect);
 procedure CreateVehicle(aSpriteType: integer);
 procedure ProcessScenarioEvent( aSurface: TSimpleSurfaceWithEffect; aScenarioID: TIDScenario; aUserEvent: integer );
end;

var
  Intro : TIntroduction ;



implementation

uses GameUnit,
     principal, sprites,
     ChooseCountry;

procedure TIntroduction.LoadData;
var bout : TGuiButton ;
  xx , yy : single ;
  o: TGuiLabel;
begin
 FScene.Layer[LayerRoad].Visible := TRUE;

 FTimeToNextSprite := 400;

 with RoadTileEngine do begin
  LoadMapFile( MAP_PATH+'RoadIntro.map');
  SetViewSize( 13*64, FScene.Height );
  SetCoordinate( (FScene.Width - Width) / 2, 0 );
  ScrollSpeed.Value := PointF(0, 300);
 end;

 TitleIntro := TSprite.Create( TexTitleIntro ) ;
 TitleIntro.SetCenterCoordinate( FScene.Width/2, 200);
 sceTitleAnim := TitleIntro.AddScenario( SCENARIO_PATH+'TitleIntro.sce' );
 TitleIntro.PlayScenario( sceTitleAnim );
 FScene.Add( TitleIntro, LayerIntro );

 FScene.ColorFadeOut(0.5);

 FScene.ExecuteDuring(1);

 if (SHMusicIntro.State = AL_INITIAL) or
    (SHMusicIntro.State = AL_STOPPED)
   then
 begin
  SHMusicIntro.Volume.Value := 1000 ;
  SHMusicIntro.Stop;
  SHMusicIntro.Play;
 end else
 begin
  SHMusicIntro.Volume.ChangeTo( 1000, 2 );
 end;

 PlayerCar := TSprite.Create( TexCarBlueSky );
 PlayerCar.SetFrameLoopBounds(1, 4);
 PlayerCar.FrameAddPerSecond( 5 );
 PlayerCar.Frame:=1;
 PlayerCar.SetCenterCoordinate(FScene.Width/2,Fscene.Height+150);
 PlayerCar.MoveTo( (FScene.Width-83)/2-150, Fscene.Height-250, 2, idcStartFastEndSlow ) ;
 FScene.Add(PlayerCar,LayerIntro);
 sceVPlayerZigzag := PlayerCar.AddScenario( SCENARIO_PATH+'zigzag.sce', @ProcessScenarioEvent ) ;
 sceVPlayerJump := PlayerCar.AddScenario( SCENARIO_PATH+'Carjump.sce', @ProcessScenarioEvent );

 FScene.ExecuteDuring( 2 );
 PlayerCar.PlayScenario( sceVPlayerZigzag );

 // button play
 xx := RoadTileEngine.X.Value + RoadTileEngine.Width - RoadTileEngine.TileSize.cx;
 yy := FScene.Height-450;
 bout := TGuiButton.Create ( 0, 0, Message[0,CurrentLanguage], FontMenu );
 bout.SetCenterCoordinate( xx , yy );
 bout.OnClick := @ProcessGUIEvent;
 bout.Group := GroupButtonPlayIntro;
 bout.Opacity.Value := 0;
 bout.Opacity.ChangeTo( 255, 2 );
 FScene.Add(bout, LayerIntro );
 yy += bout.Height+20;

 // button instructions
 bout := TGuiButton.Create ( 0, 0, Message[2,CurrentLanguage], FontMenu );
 bout.SetCenterCoordinate( xx , yy );
 bout.OnClick := @ProcessGUIEvent;
 bout.Group := GroupButtonInstruction;
 bout.Opacity.Value := 0;
 bout.Opacity.ChangeTo( 255, 2 );
 FScene.Add(bout, LayerIntro );
 yy += bout.Height+20;

 // button choose country
 bout := TGuiButton.Create ( 0, 0, Message[11,CurrentLanguage], FontMenu );
 bout.SetCenterCoordinate( xx , yy );
 bout.OnClick := @ProcessGUIEvent;
 bout.Group := GroupButtonLanguage;
 bout.Opacity.Value := 0;
 bout.Opacity.ChangeTo( 255, 2 );
 FScene.Add(bout, LayerIntro );
 yy += bout.Height+20;

 // button quit
 bout := TGuiButton.Create ( 0, 0, Message[1,CurrentLanguage], FontMenu );
 bout.SetCenterCoordinate( xx , yy );
 bout.OnClick := @ProcessGUIEvent;
 bout.Group := GroupButtonQuitIntro;
 bout.Opacity.Value := 0;
 bout.Opacity.ChangeTo( 255, 2 );
 FScene.Add(bout, LayerIntro );

 // instructions panel
 FKeysView := TSprite.create( TexKeysView );
 FKeysView.SetCoordinate(  15, 40 );

 FPanel := TColorBackground.Create(0, 0, FKeysView.Width+320, FKeysView.Height + 60 );
 FPanel.CenterX := FScene.Width * 0.5-100;
 FPanel.Y.Value := -FPanel.Height;
 FPanel.Opacity.Value := 220;
 FPanel.SetAllColorsTo( BGRA(255,255,255) );
 FPanel.AddChild( FKeysView );
 FScene.Add( FPanel, LayerIntro );

 o := TGuiLabel.Create;      // left accelerate right  brake
 o.Font := FontInstructions;
 o.Caption := Message[5,CurrentLanguage];
 o.SetCoordinate( 20+FKeysView.Width, 20 );
 FPanel.AddChild( o );

 o := TGuiLabel.Create;
 o.Font := FontInstructions;
 o.Caption := Message[7,CurrentLanguage];
 o.SetCoordinate( 20+FKeysView.Width, 20+55 );
 FPanel.AddChild( o );

 o := TGuiLabel.Create;
 o.Font := FontInstructions;
 o.Caption := Message[6,CurrentLanguage];
 o.SetCoordinate( 20+FKeysView.Width, 20+55*2+5 );
 FPanel.AddChild( o );

 o := TGuiLabel.Create;
 o.Font := FontInstructions;
 o.Caption := Message[8,CurrentLanguage];
 o.SetCoordinate( 20+FKeysView.Width, 20+55*3+10 );
 FPanel.AddChild( o );

 o := TGuiLabel.Create;      // ctrl
 o.Font := FontInstructions;
 o.Caption := Message[9,CurrentLanguage];
 o.SetCoordinate( 20+FKeysView.Width, 20+55*3+90 );
 FPanel.AddChild( o );

 o := TGuiLabel.Create;      // esc
 o.Font := FontInstructions;
 o.Caption := Message[10,CurrentLanguage];
 o.SetCoordinate( 20+FKeysView.Width, 20+55*3+80+96 );
 FPanel.AddChild( o );

end;

procedure TIntroduction.FreeData;
var i: integer;
begin
 for i:=0 to NBLayer-1 do FScene.Layer[i].Clear;
end;

procedure TIntroduction.Update(AElapsedTime: single);
begin
 // vehicle creation
 FTimeToNextSprite -= RoadTileEngine.ScrollSpeed.y.Value * AElapsedTime;
 if FTimeToNextSprite <= 0
   then begin
        FTimeToNextSprite := random(6)*20 + 600;
        CreateVehicle( random(6) + 1 );
   end;
end;

procedure TIntroduction.ProcessGUIEvent(aGUISurface: TSimpleSurfaceWithEffect);
begin
 case aGUISurface.Group of

  GroupButtonQuitIntro : PostMessage( Form_Principal.Handle, LM_MESSAGE_PRINCIPAL, Mess_Quit, 0 );

  GroupButtonPlayIntro : begin
                           SHMusicIntro.FadeOut( 3, idcStartFastEndSlow );
                           SHCarStart.Play;
                           SHCarStart.Volume.Value := oal_VOLUME_MAX;
                           SHCarStart.FadeOut( 3 );
                           FCurrentStage := 1;
                           FScore := 0;
                           ResetTableDone;
                           FScene.LaunchStage( Game );
  end;

  GroupButtonInstruction : begin
    if not FPanel.Tag2
      then FPanel.Y.ChangeTo( (FScene.Height-FPanel.Height)*0.5+50, 1, idcStartFastEndSlow )
      else FPanel.Y.ChangeTo( -FPanel.Height, 1, idcStartFastEndSlow );
    FPanel.Tag2 := not FPanel.Tag2;
  end;

  GroupButtonLanguage: begin
   SHMusicIntro.Volume.ChangeTo( 500, 2 );
   FScene.LaunchStage( CountryChoice );
  end;

 end;//case
end;

procedure TIntroduction.CreateVehicle(aSpriteType: integer);
var o: TSprite ;
    xx: single;
    Particle: TParticleEmitter;

    function CreateCarParticleEmitter: TParticleEmitter;
    begin
     Result := TParticleEmitter.Create;
     Result.LoadFromFile( PARTICLES_PATH+'Smoke.par' );
     Result.Direction.Value:=90;
     Result.Spread.Value:= 30 + random( 100 ) ;
     Result.Opacity.Value:= 20 + random( 50 );
     Result.ParticlesToEmit.Value:= 50 + random( 100 );
     Result.Gravity.y.Value:=80;
    end;

begin
 with RoadTileEngine do
  if random(100)>50
    then xx := X.Value + TileSize.cx * 2
    else xx := X.Value + Width - TileSize.cx * 2;

 case aSpriteType of
  idTruckWhite : begin
       o := TSprite.Create(TexTruckWhite);
       xx -= o.Width/2;
       o.SetCoordinate(xx, -o.Height);
       o.Speed.Value := PointF(0, 150);
       o.SetFrameLoopBounds( 1, 4);
       o.FrameAddPerSecond(8);
       o.Group:=GroupSpriteTruck;
       FScene.Add(o, LayerSprite);
       o.KillDefered(8);

       Particle := TParticleEmitter.Create;
       Particle.LoadFromFile( PARTICLES_PATH+'Smoke.par' );
       Particle.Direction.Value:=90;
       Particle.Spread.Value:= 60 ;
       Particle.Gravity.y.Value:=100;
       Particle.SetCoordinate( o.Width*0.5, o.Height-2 );
       o.AddChild( Particle );
  end;
  idMoto : begin
   o := TSprite.Create(TexMoto);
   xx -= o.Width/2;
   o.SetCoordinate(xx, -o.Height);
   o.Speed.Value := PointF(0, 150);
   o.SetFrameLoopBounds( 1, 4);
   o.FrameAddPerSecond(8);
   o.Group:=GroupSpriteMoto;
   FScene.Add(o, LayerSprite);
   o.KillDefered(8);
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
   o := TSprite.Create(TexCarRed);
   xx -= o.Width/2;
   o.SetCoordinate(xx, -o.Height);
   o.Speed.Value := PointF(0, 150);
   o.SetFrameLoopBounds( 1, 4);
   o.FrameAddPerSecond(8);
   o.Group:=GroupSpriteRedCar;
   FScene.Add(o, LayerSprite);
   o.KillDefered(8);
   Particle := CreateCarParticleEmitter;
   Particle.SetCoordinate( o.Width/6, o.Height-2 );
   o.AddChild( Particle );
  end;
  idCarWhite : begin
   o := TSprite.Create(TexCarWhite);
   xx -= o.Width/2;
   o.SetCoordinate(xx, -o.Height);
   o.Speed.Value := PointF(0, 150);
   o.SetFrameLoopBounds( 1, 4);
   o.FrameAddPerSecond(8);
   o.Group:=GroupSpriteWhiteCar;
   FScene.Add(o, LayerSprite);
   o.KillDefered(8);
   Particle := CreateCarParticleEmitter;
   Particle.SetCoordinate( o.Width/6, o.Height-2 );
   o.AddChild( Particle );
  end;
  idCarBlue : begin
   o := TSprite.Create(TexCarBlue);
   xx -= o.Width/2;
   o.SetCoordinate(xx, -o.Height);
   o.Speed.Value := PointF(0, 150);
   o.SetFrameLoopBounds( 1, 4);
   o.FrameAddPerSecond(8);
   o.Group:=GroupSpriteBlueCar;
   FScene.Add(o, LayerSprite);
   o.KillDefered(8);
   Particle := CreateCarParticleEmitter;
   Particle.SetCoordinate( o.Width/6, o.Height-2 );
   o.AddChild( Particle );
  end;
  idCarYellowRed : begin
   o := TSprite.Create(TexCarYellowRed);
   xx -= o.Width/2;
   o.SetCoordinate(xx, -o.Height);
   o.Speed.Value := PointF(0, 150);
   o.SetFrameLoopBounds( 1, 4);
   o.FrameAddPerSecond(8);
   o.Group:=GroupSpriteYellowRedCar;
   FScene.Add(o, LayerSprite);
   o.KillDefered(8);
   Particle := CreateCarParticleEmitter;
   Particle.SetCoordinate( o.Width/6, o.Height-2 );
   o.AddChild( Particle );
  end;
  idOilPuddle : begin
   o := TSprite.Create(TexOilPuddle);
   xx -= o.Width/2;
   o.SetCoordinate(xx, -o.Height);
   o.Speed.Value := PointF(0, 150);
   FScene.Add(o, LayerSprite);
   o.KillDefered(8);
  end;
 end;//case
end;

procedure TIntroduction.ProcessScenarioEvent(aSurface: TSimpleSurfaceWithEffect; aScenarioID: TIDScenario; aUserEvent: integer);
begin
 if aScenarioID = sceVPlayerZigzag
   then begin
         PlayerCar.Tag1 := PlayerCar.Tag1+1;
         if PlayerCar.Tag1 > 2
          then begin
                PlayerCar.PlayScenario( sceVPlayerJump);
                PlayerCar.StopScenario( sceVPlayerZigzag );
                PlayerCar.Tag1 := 0;
               end;
         end;
 if ( aScenarioID = sceVPlayerJump ) and ( aUserEvent = 0 )
   then PlayerCar.PlayScenario( sceVPlayerZigzag, FALSE );
end;

end.

