unit OGLCScene;
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Graphics, Dialogs, Controls,
  types,
  OpenGLContext, GLExt, GL,
  lazutf8,
  BGRABitmapTypes, BGRABitmap, BGRATextFX,
  variable_parameter_V2,
  utilitaire_chaine_utf8,
  math,
  strutils;


//{$DEFINE DEBUG_MODE_ON}

type

TOGLCScene = class;

// Callback for OGLCScene.OnBeforePaint and OGLCScene.OnAfterPaint
TPaintEvent = procedure ( aScene:TOGLCScene ) of object;

// define procedure callback
TProcEvent = procedure () of object;

{ in the scene, each surface can play scenarios. A scenario is an external text file
  that contain the actions to execute on the surface only
  Below you can find all the available actions to build your scenario
}

// SCENARIO
TIDScenario = integer;
const
     // Boolean value
     acTRUE                 = 'TRUE'     ;
     acFALSE                = 'FALSE'    ;
     // Available actions for sprite
     acLoop                 =  'Loop'    ; // Loop
     acLabel                =  'Label'   ; // Label LabelName
     acGotoLabel            =  'Goto'    ; // Goto LabelName
     acWait                 =  'Wait'    ; // Wait second

     acKill                 =  'Kill'    ; // Kill
     acFreeze               =  'Freeze'  ; // Freeze TRUE/FALSE
     acVisible              =  'Visible' ; // Visible TRUE/FALSE

     acFlipH                =  'FlipH'       ; // FlipH TRUE/FALSE
     acFlipV                =  'FlipV'       ; // FlipV TRUE/FALSE
     acToggleFlipH          =  'ToggleFlipH' ; // ToggleFlipH
     acToggleFlipV          =  'ToggleFlipV' ; // ToggleFlipV

     acOpacity              =  'Opacity' ; // Opacity NewOpacity
     acOpacityChange        =  'OpacityChange'      ; // OpacityChange NewOpacity Duration Curve

     acAnimate              =  'Animate' ; // Animate StartFrameIndex EndFrameIndex FramePerSecond(single)
     acIncFrame             =  'IncFrame'; // IncFrame
     acDecFrame             =  'DecFrame'; // DecFrame
     acSetFrame             =  'SetFrame'; // SetFrame ImageIndex

     acTint                 =  'Tint'    ; // Tint red green blue alpha
     acTintChange           =  'TintChange' ; // TintChange Red Green Blue Alpha Duration Curve
     acTintRedChange        =  'TintRedChange' ;   // TintRedChange NewRedValue Duration Curve
     acTintGreenChange      =  'TintGreenChange' ; // TintGreenChange NewGreenValue Duration Curve
     acTintBlueChange       =  'TintBlueChange' ;  // TintBlueChange NewBlueValue Duration Curve
     acTintAlphaChange      =  'TintAlphaChange' ; // TintAlphaChange NewAlphaValue Duration Curve

     acSkew                 = 'Skew'     ; // Skew XValue YValue Duration Curve

     acAngle                = 'Angle'    ; // Angle NewAngle
     acRotate               = 'Rotate'   ; // Rotate AnglePerSecond
     acRotateTo             = 'RotateTo' ; // RotateTo Angle Duration Curve
     acRotationAroundAxis   = 'RotationAroundAxis'    ; // RotationAroundAxis XAxis YAxis AnglePerSecond SelfRotate

     acScale                = 'Scale'          ; // Scale HVValue      // [1]= normal size, [0..1[->reduced, ]1..inf[->enlarged
     acScaleChange          = 'ScaleChange'    ; // ScaleChange HVNewValue Duration Curve
     acScaleH               = 'ScaleH'         ; // ScaleH HValue      // to set scale value for horizontaly axis
     acScaleHChange         = 'ScaleHChange'   ; // ScaleHChange HNewValue Duration Curve
     acScaleV               = 'ScaleV'         ; // ScaleV VValue      // to set scale value for verticaly axis
     acScaleVChange         = 'ScaleVChange'   ; // ScaleVChange VNewValue Duration Curve


     acBlink                = 'Blink'    ; // Blink NumberOfBlink aVisibleTime aInvisibleTime   // set NumberOfBlink to '-1' for infinite blink
     acStopBlink            = 'StopBlink'; // StopBlink

     acMoveTo               = 'MoveTo'         ; // MoveTo X Y Duration Curve
     acMoveXTo              = 'MoveXTo'        ; // MoveXTo X Duration Curve
     acMoveYTo              = 'MoveYTo'        ; // MoveYTo Y Duration Curve

     acMoveCenterTo         = 'MoveCenterTo'   ; // MoveCenterTo Xcenter YCenter Duration Curve
     acMoveXCenterTo        = 'MoveXCenterTo'  ; // MoveXCenterTo X Duration Curve
     acMoveYCenterTo        = 'MoveYCenterTo'  ; // MoveYCenterTo Y Duration Curve

     acMoveRelative         = 'MoveRelative'   ; // MoveRelative DeltaX DeltaY Duration Curve
     acMoveXRelative        = 'MoveXRelative'  ; // MoveXRelative DeltaX Duration Curve
     acMoveYRelative        = 'MoveYRelative'  ; // MoveYRelative DeltaY Duration Curve

     acSetCoor              = 'SetCoor'        ; // SetCoor X Y
     acSetCenterCoor        = 'SetCenterCoor'  ; // SetCenterCoor X Y  (single)

     // Available actions for user event
     acSendEvent            = 'SendEvent' ; // SendEvent UserEvent


// Blend mode
const
     FX_BLEND_NORMAL = $00;
     FX_BLEND_ADD    = $01;
     FX_BLEND_MULT   = $02;
     FX_NOBLEND      = $03;

     FX_COLOR_MIX    = $00;
     FX_COLOR_SET    = $01;

type
ArrayOfString = array of string;


{$I oglcListTypeDeclaration.inc }
{$I oglcLayerDeclaration.inc }
{$I oglcSurfaceDeclaration.inc }
{$I oglcUtilsDeclaration.inc }
{$I oglcEnvironmentDeclaration.inc }


type

// class to manage one stage of your game application
TStageSkeleton = class
 procedure LoadData; virtual; abstract;  // override to load texture, create sprite, gui object, sound, initialization, etc...
 procedure FreeData; virtual; abstract;  // override to free all that need to be freed at the end of the stage
 procedure Update( AElapsedTime: single ); virtual; abstract; // override to update your stuff according time
end;

type

{ TSceneCamera }

TSceneCamera = class
 FParentScene: TOGLCScene;
 Constructor Create;
 Destructor Destroy; override;
 procedure Update( AElapsedTime: single );
public
  LookAt: TPointFParam;
  Scale: TPointFParam;
  Angle: TFParam;
  procedure Reset( aSeconds: single ); // reset camera to initial value in aSeconds
  procedure Use;
  procedure Release;
end;

{ TOGLCScene }
TOGLCScene = class (TLayerList)
 private
  FOGLC : TOpenGLControl ;
  FOGLCOnResize : TNotifyEvent ;
  FOGLCOnClick : TNotifyEvent ;
  FFlagMouseLeftClicked : boolean ;
  FXMouse, FYMouse : integer ;
  FGLInitialized : boolean ;
  FTimerManager : TTimerManager ;
  FMouseManager : TMouseManager ;
  FKeyMap : array[0..255] of boolean ; // TRUE if Key is pressed
  FCurrentStage,
  FNewStage: TStageSkeleton;
  FDoBlackScreenOnNewStage: boolean;
  procedure NewOnResize ( Sender:TObject );
  procedure NewOnClick ( Sender:TObject );
  procedure Draw ;
  procedure UpDate ( const DT:single );
 protected
  FShaderGlow: TShaderID;
  FShaderSin: TShaderID;
 private
  FLoopIsActive : boolean;
  FTickOrigin : QWord;
  // FPS
  FFPSCounter : integer ;
  FFPS        : integer ;
  FExecuteDuringLoop: boolean;
  FIsChangingStage: boolean;
  procedure CallBackTimerFPS ;
 private
  FGlobalFadeColor: TBGRAParam;
  function GetSceneHeight: integer; inline ;
  function GetSceneWidth: integer; inline ;
  procedure SetBackgroundColor ( aColor:TBGRAPixel );
 private
  // CallBack
  FBackgroundColor : TBGRAPixel  ;
  FOnBeforePaint   : TPaintEvent ;
  FOnAfterPaint    : TPaintEvent ;
  function GetKeyMap(index: byte): boolean;
  procedure SetLayerCount(AValue: integer ); override ;
  procedure DoLoop;
 private
  FCamera: TSceneCamera;
 public

  Constructor Create ( aOGLContext: TOpenGLControl ) ;
  Destructor Destroy; override;
  function GetRectArea: TRect;
  procedure ProcessKeyDown( Key: Word; Shift: TShiftState);
  procedure ProcessKeyUp( Key: Word; Shift: TShiftState);

  procedure Start;     // Enter the main loop
  procedure Stop;      // exit from main loop
  procedure ExecuteDuring( aTimeInSecond:single );

  procedure LaunchStage( AStage: TStageSkeleton; DoBlackScreen: boolean=TRUE );

  // Surface
  procedure Add ( aSurface: TSimpleSurfaceWithEffect; aLayerIndex:integer=0);
  procedure Insert ( aSurfaceIndex: integer; aSurface: TSimpleSurfaceWithEffect; aLayerIndex:integer=0);
  function GetSurfaceByIndex( aLayerIndex, aSurfaceIndex: integer): TSimpleSurface;
  // Global scene color fade in and out
  procedure ColorFadeIn ( const aColor : TBGRAPixel ; const aDurationInSecond : single );
  procedure ColorFadeOut ( const aDurationInSecond : single );

  property Width: integer read GetSceneWidth;
  property Height: integer read GetSceneHeight;
  property FPS: integer read FFPS;
  property BackgroundColor: TBGRAPixel read FBackgroundColor write SetBackgroundColor ;
  // Callback
  property OnBeforePaint: TPaintEvent read FOnBeforePaint write FOnBeforePaint ;
  property OnAfterPaint: TPaintEvent read FOnAfterPaint write FOnAfterPaint ;

  // Mouse
  property MouseManager : TMouseManager read FMouseManager ;
  // Timer
  property TimerManager : TTimerManager read FTimerManager ;
  // Keyboard
 public
  procedure ClearKeysState;
  property Key[index:byte]: boolean read GetKeyMap ;

  property Camera: TSceneCamera read FCamera;
end;
POGLCScene = ^TOGLCScene ;



implementation

function gl_GetProc( const Proc : UTF8String ) : Pointer;
begin
 Result := wglGetProcAddress( PAnsiChar( Proc ) );
 if Result = nil Then
  Result := wglGetProcAddress( PAnsiChar( Proc + 'ARB' ) );
 if Result = nil Then
  Result := wglGetProcAddress( PAnsiChar( Proc + 'EXT' ) );
end;
procedure InitShaderUse;
begin
 if not Load_GL_version_2_0 then showmessage('PAS DE Load_GL_version_2_0!!' ) ;
end;

{ TSceneCamera }

constructor TSceneCamera.Create;
begin
  LookAt := TPointFParam.Create;
  Scale := TPointFParam.Create;
  Scale.Value := PointF(1,1);
  Angle := TFParam.Create;
end;

destructor TSceneCamera.Destroy;
begin
 FreeAndNil( LookAt );
 FreeAndNil( Scale );
 FreeAndNil( Angle );
 inherited Destroy;
end;

procedure TSceneCamera.Update(AElapsedTime: single);
begin
 LookAt.OnElapse( AElapsedTime );
 Scale.OnElapse( AElapsedTime );
 Angle.OnElapse( AElapsedTime );
end;

procedure TSceneCamera.Reset(aSeconds: single);
begin
 LookAt.ChangeTo( PointF(0,0), aSeconds );
 Scale.ChangeTo( PointF(1,1), aSeconds );
 Angle.ChangeTo( 0, aSeconds );
end;

procedure TSceneCamera.Use;
begin
 glPushMatrix;
 glTranslatef( FParentScene.Width * 0.5, FParentScene.Height * 0.5 , 0 );
 //glTranslatef( X.Value + Width * 0.5 * Scale.x.Value, Y.Value + Height * 0.5 * Scale.y.Value, 0 );

 glScalef( Scale.x.Value, Scale.y.Value, 0 );
 glRotatef( Angle.Value, 0, 0, 1 );
 glTranslatef( -FParentScene.Width * 0.5 + LookAt.x.Value, -FParentScene.Height * 0.5 + LookAt.y.Value, 0 );
end;

procedure TSceneCamera.Release;
begin
 glPopMatrix;
end;


{$I oglcListTypeImplementation.inc }
{$I oglcLayerImplementation.inc }
{$I oglcSurfaceImplementation.inc }
{$I oglcUtilsImplementation.inc }
{$I oglcEnvironmentImplementation.inc }

{ TOGLCScene }

constructor TOGLCScene.Create(aOGLContext: TOpenGLControl);
begin
 inherited Create;

 // fix decimal separator to dot to avoid read error when converting floating point values from scenario's files
 SysUtils.FormatSettings.DecimalSeparator := '.';

 FGLInitialized := FALSE ;
 FOGLC := aOGLContext;

 FOGLCOnResize := FOGLC.OnResize ;
 FOGLC.OnResize := @NewOnResize ;

 FOGLCOnClick := FOGLC.OnClick ;
 FOGLC.OnClick := @NewOnClick ;
 FFlagMouseLeftClicked:=FALSE;

 ClearKeysState;

 FBackgroundColor := BGRABlack;

 FOnBeforePaint := NIL;
 FOnAfterPaint := NIL;

 FGlobalFadeColor:= TBGRAParam.Create;
 FGlobalFadeColor.Value := BGRA(0,0,0,0);

 TextureManager := TTextureManager.Create;
 FTimerManager := TTimerManager.Create;
 FTimerManager.Add ( @CallBackTimerFPS, 1000 );

 FMouseManager := TMouseManager.Create;
 FMouseManager.FParentScene := Self;

 FontManager := TFontManager.Create;

 FExecuteDuringLoop := FALSE;
 FIsChangingStage := FALSE;

 FCurrentStage := NIL;
 FNewStage := NIL;

 FCamera := TSceneCamera.Create;
 FCamera.FParentScene := self;
end;

destructor TOGLCScene.Destroy;
begin
 FreeAndNil( FGlobalFadeColor );

 SetLayerCount(0);
 FontManager.Free;
 FTimerManager.Free;
 FMouseManager.Free;
 TextureManager.Free;
 if ShaderManager <> NIL then FreeAndNil( ShaderManager );

 FreeAndNil( FCamera );

 inherited Destroy;
end;

procedure TOGLCScene.Start;
begin
 if not FOGLC.MakeCurrent() then exit;
 Load_GL_version_2_0; // opengl 2 for shader

 if FLoopIsActive then exit;
 FLoopIsActive := TRUE;
 FTickOrigin := GetTickCount64;
 while FLoopIsActive do
  begin
   DoLoop;
   FFlagMouseLeftClicked := FALSE ;
   Application.ProcessMessages;
   // Stage change if needed
   if (FNewStage <> NIL) and not FExecuteDuringLoop and not FIsChangingStage then
   begin
     FIsChangingStage := TRUE;
      if FDoBlackScreenOnNewStage
        then begin
          ColorFadeIn( BGRABlack, 1.0 );
          ExecuteDuring( 1.0 );
        end;
      if FCurrentStage <> NIL then FCurrentStage.FreeData;
      FCurrentStage := FNewStage;
      FCurrentStage.LoadData;
      ClearKeysState;
      if FDoBlackScreenOnNewStage
        then begin
          ColorFadeOut( 1.0 );
          ExecuteDuring( 1.0 );
        end;
      FNewStage := NIL;
      FIsChangingStage := FALSE;
   end;
  end;
end;

procedure TOGLCScene.Stop;
begin
 FLoopIsActive := FALSE;
end;

procedure TOGLCScene.DoLoop;
var t: QWord;
begin
 FXMouse := FOGLC.ScreenToClient( Controls.Mouse.CursorPos ).x;
 FYMouse := FOGLC.ScreenToClient( Controls.Mouse.CursorPos ).y;
 FMouseManager.ProcessMouse;
 FTimerManager.ProcessTimer;

 t := GetTickCount64;
 UpDate( ( t - FTickOrigin ) * 0.001 );
 FTickOrigin := t;

 Draw;
 FOGLC.SwapBuffers;
 inc( FFPSCounter );
end;

procedure TOGLCScene.ExecuteDuring(aTimeInSecond: single);
var torig, tactu: TTimeStamp;
begin
 if FExecuteDuringLoop then exit;
 torig := DateTimeToTimeStamp ( now ) ;
 FExecuteDuringLoop := TRUE;
 repeat
  DoLoop;
  Application.ProcessMessages;
  tactu := DateTimeToTimeStamp ( Now ) ;
 until ( tactu.Time - torig.Time ) / 1000 >= aTimeInSecond ;
 FExecuteDuringLoop := FALSE;
end;

procedure TOGLCScene.LaunchStage(AStage: TStageSkeleton; DoBlackScreen: boolean=TRUE);
begin
 if FIsChangingStage then exit;
 FNewStage := AStage;
 FDoBlackScreenOnNewStage := DoBlackScreen;
end;


procedure TOGLCScene.NewOnResize(Sender: TObject);
begin
 if FOGLCOnResize <> NIL then FOGLCOnResize( Sender );
end;

procedure TOGLCScene.NewOnClick(Sender: TObject);
begin
 FFlagMouseLeftClicked := TRUE ;
 if FOGLCOnClick <> NIL then FOGLCOnClick( Sender );
end;

procedure TOGLCScene.ProcessKeyDown(Key: Word; Shift: TShiftState);
begin
{ shift := GetKeyShiftState;
 if ssCtrl in Shift then FKeyMap[VK_LCONTROL] := TRUE
  else if ssShift in Shift then FKeyMap[VK_LSHIFT] := TRUE
   else if ssAlt in Shift then FKeyMap[VK_MENU] := TRUE
    else} FKeyMap[byte(Key)] := TRUE ;
end;

procedure TOGLCScene.ProcessKeyUp(Key: Word; Shift: TShiftState);
begin
{ shift := GetKeyShiftState;
 if ssCtrl in Shift then FKeyMap[VK_LCONTROL] := FALSE
  else if ssShift in Shift then FKeyMap[VK_LSHIFT] := FALSE
   else if ssAlt in Shift then FKeyMap[VK_MENU] := FALSE
    else} FKeyMap[byte(Key)] := FALSE ;
end;

procedure TOGLCScene.Add( aSurface : TSimpleSurfaceWithEffect; aLayerIndex:integer);
begin
 AddSurfaceToLayer(aSurface, aLayerIndex );
 aSurface.FParentLayer := Layer[aLayerIndex] ;
 aSurface.FParentScene := Self;
end;

procedure TOGLCScene.Insert( aSurfaceIndex: integer; aSurface: TSimpleSurfaceWithEffect; aLayerIndex:integer) ;
begin
 InsertSurfaceToLayer( aSurfaceIndex, aSurface, aLayerIndex);
 aSurface.FParentLayer := Layer[aLayerIndex] ;
 aSurface.FParentScene := Self;
end;

function TOGLCScene.GetSurfaceByIndex(aLayerIndex, aSurfaceIndex: integer ): TSimpleSurface;
begin
 Result := Layer[aLayerIndex].Surface[aSurfaceIndex];
end;

procedure TOGLCScene.ColorFadeIn(const aColor: TBGRAPixel; const aDurationInSecond: single);
begin
 if aDurationInSecond = 0
   then FGlobalFadeColor.Value := aColor
   else FGlobalFadeColor.ChangeTo( aColor, aDurationInSecond );
end;

procedure TOGLCScene.ColorFadeOut(const aDurationInSecond: single);
begin
 if aDurationInSecond=0
   then FGlobalFadeColor.Alpha.Value := 0
   else FGlobalFadeColor.Alpha.ChangeTo( 0, aDurationInSecond );
end;

procedure TOGLCScene.ClearKeysState;
begin
 FillChar( FKeyMap, sizeof(FKeyMap), FALSE ) ;
end;

procedure TOGLCScene.Draw;
var L,i : integer ;
    o : TSimpleSurface ;
begin

 if ShaderManager = NIL
   then begin
         ShaderManager := TShaderManager.Create;
         FShaderGlow := ShaderManager.CreateShader( '', GLOW_FRAGMENT_SHADER_PRG );
         FShaderSin := ShaderManager.CreateShader( SIN_VERTEX_SHADER_PRG, '' );
   end;

 if not FGLInitialized then begin
   SetBlendMode( FX_BLEND_NORMAL ) ;
  // glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);
   glEnable(GL_POLYGON_SMOOTH or GL_LINE_SMOOTH);

   glMatrixMode(GL_PROJECTION);
   glLoadIdentity();
   glOrtho( FOGLC.Left, FOGLC.Width, FOGLC.Height, FOGLC.Top, 0.0, 1.0);

   glMatrixMode(GL_MODELVIEW);
   glLoadIdentity();
   glViewport(0, 0, GetSceneWidth, GetSceneHeight );
   FGLInitialized := TRUE;
   SetBackgroundColor( FBackgroundColor );
 end;

 // set camera view
 FCamera.Use;

 glClear( GL_COLOR_BUFFER_BIT );

 for L:=0 to LayerCount-1 do
  begin
   // delete all surface with Kill=true
   for i:=Layer[L].FList.Count downto 1 do
    begin
     o := TSimpleSurface( Layer[L].FList.Items[i-1] );
     if o.FKill
       then begin
             o.Free;
             Layer[L].Delete ( i-1 );
            end;
    end;
  end;
 glEnable( GL_BLEND );
 // Before paint CallBack
 if Assigned (FOnBeforePaint) then FOnBeforePaint( Self );

 // Render all Layers
 for L:=LayerCount-1 downto 0 do Layer[L].Draw;

 // After paint CallBack
 if Assigned (FOnAfterPaint) then FOnAfterPaint( Self );

 // Scene global fade
 if FGlobalFadeColor.Alpha.Value > 0
   then FillBox( 0, 0, GetSceneWidth, GetSceneHeight, FGlobalFadeColor.Value );

 // Render mouse cursor
 if FMouseManager.FSprite <> NIL
   then FMouseManager.FSprite.Draw( 1 );

 glDisable( GL_BLEND );

 FCamera.Release;
end;

procedure TOGLCScene.UpDate(const DT: single);
var i: integer;
begin
 // camera update
 FCamera.Update( DT );

 // Do Surface change layer
 for i:=0 to length(FListSurfaceChangeLayer)-1 do
  with FListSurfaceChangeLayer[i] do
   begin
    Surface.FParentLayer.Delete( Surface.FIndex );
    Surface.FIndex := Layer[NewLayerIndex].Add( Surface );
    Surface.FParentLayer := Layer[NewLayerIndex];
 end;
 SetLength( FListSurfaceChangeLayer, 0 );

 // Update all layers
 for i:=LayerCount-1 downto 0 do Layer[i].Update( DT );
 // if any, update current stage
 if FCurrentStage <> NIL then FCurrentStage.Update( DT );

 MouseManager.UpDate( DT );

 // Scene global fade
 FGlobalFadeColor.OnElapse( DT );
end;

procedure TOGLCScene.CallBackTimerFPS;
begin
 FFPS := FFPSCounter;
 FFPSCounter := 0;
end;

function TOGLCScene.GetSceneHeight: integer;
begin
 Result := FOGLC.Height;
end;

function TOGLCScene.GetSceneWidth: integer;
begin
 Result := FOGLC.Width ;
end;

procedure TOGLCScene.SetBackgroundColor(aColor: TBGRAPixel);
begin
 glClearColor(FBackgroundColor.red/255, FBackgroundColor.green/255, FBackgroundColor.blue/255, FBackgroundColor.alpha/255);
end;

function TOGLCScene.GetKeyMap(index: byte): boolean;
begin
 Result := FKeyMap[index];
end;

function TOGLCScene.GetRectArea: TRect;
begin
 Result.Create( Point(0,0), Width,  Height );
end;

procedure TOGLCScene.SetLayerCount(AValue: integer);
var i:integer ;
begin
 inherited SetLayerCount(AValue);
 for i:=0 to GetLayerCount-1 do
  begin
   Layer[i].FParentScene := Self;
  end;
end;


end.

