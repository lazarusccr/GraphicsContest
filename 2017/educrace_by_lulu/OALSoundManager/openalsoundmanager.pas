unit OpenAlSoundManager;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, openal,
  LCLProc, LCLType, LCLIntf,
  OpenALSoundWavFileLoader,
  variable_parameter_V2;


const

// Volume range
  oal_VOLUME_MIN =    0 ;
  oal_VOLUME_MAX = 1000 ;

// Pitch range
  oal_PITCH_MAX     = 4.0;
  oal_PITCH_MIN     = 0.1;
  oal_PITCH_NORMAL  = 1.0;

// Sound state
   AL_INITIAL   = openal.AL_INITIAL;
   AL_PLAYING   = openal.AL_PLAYING;
   AL_PAUSED    = openal.AL_PAUSED;
   AL_STOPPED   = openal.AL_STOPPED;

type

TOALSound = class;

{ TVolumeParam }

TVolumeParam = class( TBoundedFParam )
private
  FParentOALSound: TOALSound;
protected
  procedure SetValue( AValue: single ); override;
end;

{ TPitchParam }

TPitchParam = class( TBoundedFParam )
private
 FParentOALSound: TOALSound;
protected
 procedure SetValue( AValue: single ); override;
end;


{ TOALSound }

TOALSound = class
protected
  FWavLoader: TWavLoader;
  FSamples : TArrayOfByte;
  FError  : boolean;
  FBuffer : LongWord;
  FSource : LongWord;
  FLoop   : boolean;
  FPaused: boolean;
  FPan: integer;
  FPitch: single;
  FVolumeFadeOut: boolean;
private
  function GetChannelCount: integer;
  function GetFormat: word;
  function GetFrequency: integer;
  function GetSampleCount: QWord;
  procedure SetLoop(AValue: boolean);
  procedure SetALVolume;
  procedure SetALPitch;
protected
  procedure Update( const aElapsedTime: single );
public
  Constructor Create( const aFilename:string );
  Destructor Destroy; override;

  procedure Play( AFromBegin: boolean=TRUE );
  procedure Stop;
  procedure Pause;

  procedure FadeIn( ATimeSec: single; AVelocityCurve: word=idcLinear );
  procedure FadeIn( AVolume: integer; ATimeSec: single; AVelocityCurve: word=idcLinear );
  procedure FadeOut( ATimeSec: single; AVelocityCurve: word=idcLinear );

  function GetState: ALInt;

  function TotalDuration : single ;
  function GetTimePosition : single ;
  function Byte2Seconds ( aPosition : QWord ) : single ;
  function Seconds2Byte ( aTimePosition : single ) : QWord;

public
  Volume: TVolumeParam;    // [0..oal_VOLUME_MAX]
  Pitch:  TPitchParam;     // pitch is [0.5,2.0] range
  property Frequency: integer read GetFrequency;
  property Loop: boolean read FLoop write SetLoop ;
  property ChannelCount : integer read GetChannelCount ;
  property Format: word read GetFormat ;
  property SampleCount: QWord read GetSampleCount ;
  property State: ALint read GetState;
end;

type
TDoUpdate=procedure( const aElapsedTime: single ) of object;

{ TTimeableThread }

TTimeableThread= class( TThread )
private
  FPeriodUpdate: integer;
  FIsTerminated: boolean;
  FDoUpdate: TDoUpdate ;
  procedure SetPeriodUpdate(AValue: integer);
protected
  procedure Execute ; override;
public
 Constructor Create( aCallBackDoUpdate: TDoUpdate; aUpdatePeriod: integer; aStart: boolean );
 property UpdatePeriod: integer read FPeriodUpdate write SetPeriodUpdate; // period is in millisecond
 property DoUpdate: TDoUpdate read FDoUpdate write FDoUpdate;
 property IsTerminated: boolean read FIsTerminated;
end;

{ TOALCustomAudioManager }

TOALCustomAudioManager = class
  Constructor Create ;
  Destructor Destroy ; override ;
protected
  FCriticalSection: TCriticalSection;
  FThread: TTimeableThread;
  procedure DoUpdate( const aElapsedTime: single );
private
  FList    : TList;
  FDevice  : PALCdevice;
  FContext : PALCcontext;
  function GetSoundCount: integer;
  function GetSoundByIndex(aIndex: integer): TOALSound;
  procedure DoDeleteSound( AIndex: integer );
public
  function Add( const aFilename: string ): TOALSound;
  procedure Delete( ASound: TOALSound );
  procedure Clear;
  procedure StopAllSound;
end;

var
  OALSoundManager : TOALCustomAudioManager ;

implementation

{ TTimeableThread }

procedure TTimeableThread.SetPeriodUpdate(AValue: integer);
begin
 if FPeriodUpdate=AValue then Exit;
 FPeriodUpdate:=AValue;
 if FPeriodUpdate<1 then FPeriodUpdate:=1;
end;

procedure TTimeableThread.Execute;
var T1, T2, DeltaT: QWord;
    v: single;
begin
 T1 := GetTickCount64;
 while not Terminated do
  begin
   T2 := GetTickCount64;
   DeltaT := T2-T1;
   if (DeltaT >= FPeriodUpdate) and (FDoUpdate <> NIL)
     then begin
           v := single( DeltaT ) * 0.001;
           if FDoUpdate<>NIL then FDoUpdate( v );
           T1 := T2;
          end;
   if FPeriodUpdate > 1 then sleep( FPeriodUpdate - 1 );
  end;
 FIsTerminated:=TRUE;
end;

constructor TTimeableThread.Create(aCallBackDoUpdate: TDoUpdate;
  aUpdatePeriod: integer; aStart: boolean);
begin
 inherited Create(true );
 FPeriodUpdate := aUpdatePeriod;
 FIsTerminated := FALSE;
 FDoUpdate := aCallBackDoUpdate;
 if aStart then Start;
end;

{ TVolumeParam }

procedure TVolumeParam.SetValue(AValue: single);
begin
 inherited SetValue(AValue);
 FParentOALSound.SetALVolume;
end;

{ TPitchParam }

procedure TPitchParam.SetValue(AValue: single);
begin
 inherited SetValue(AValue);
 FParentOALSound.SetALPitch;
end;

{ TOALCustomAudioManager }

constructor TOALCustomAudioManager.Create;
begin
 FDevice := alcOpenDevice(NIL);
 FContext := alcCreateContext(FDevice, NIL);
 if FContext=NIL then raise exception.create('OpenAL error: no context created...');
 alcMakeContextCurrent(FContext);
 alDistanceModel(AL_INVERSE_DISTANCE_CLAMPED);

 FList := TList.Create;

 InitializeCriticalSection( FCriticalSection );
 FThread:= TTimeableThread.Create(@DoUpdate, 10, TRUE );
end;

destructor TOALCustomAudioManager.Destroy;
begin
 FThread.Terminate;
 while not FThread.IsTerminated do;
 FThread.Free;

 Clear;
 FreeAndNil( FList );

 DeleteCriticalSection( FCriticalSection );

 alcMakeContextCurrent(NIL);
 alcDestroyContext(FContext);
 alcCloseDevice(FDevice);
end;

procedure TOALCustomAudioManager.DoUpdate(const aElapsedTime: single);
var i: integer;
begin
 try
  EnterCriticalSection( FCriticalSection );
  for i:=0 to GetSoundCount-1 do
    GetSoundByIndex(i).Update( aElapsedTime );
 finally
  LeaveCriticalSection( FCriticalSection );
 end;
end;

procedure TOALCustomAudioManager.Clear ;
var i : integer ;
begin
 try
  EnterCriticalSection( FCriticalSection );
  for i:=0 to GetSoundCount-1 do GetSoundByIndex(i).Free ;
  FList.Clear;
 finally
  LeaveCriticalSection( FCriticalSection );
 end;
end;

procedure TOALCustomAudioManager.StopAllSound;
var i:integer ;
begin
 try
  EnterCriticalSection( FCriticalSection );
  for i:=0 to GetSoundCount-1 do
    GetSoundByIndex(i).Stop ;
 finally
  LeaveCriticalSection( FCriticalSection );
 end;
end;

function TOALCustomAudioManager.GetSoundCount: integer;
begin
 try
  EnterCriticalSection( FCriticalSection );
  Result := FList.Count;
 finally
  LeaveCriticalSection( FCriticalSection );
 end;
end;

function TOALCustomAudioManager.GetSoundByIndex(aIndex: integer): TOALSound;
begin
 try
  EnterCriticalSection( FCriticalSection );
  Result := TOALSound( FList.Items[aIndex] );
 finally
  LeaveCriticalSection( FCriticalSection );
 end;
end;

procedure TOALCustomAudioManager.DoDeleteSound(AIndex: integer);
begin
 if (AIndex < 0) and (AIndex >= GetSoundCount ) then exit;

 try
  EnterCriticalSection( FCriticalSection );
  TOALSound( FList.Items[AIndex] ).Free;
  FList.Delete( AIndex );
 finally
  LeaveCriticalSection( FCriticalSection );
 end;
end;

function TOALCustomAudioManager.Add(const aFilename: string): TOALSound;
begin
 try
  EnterCriticalSection( FCriticalSection );
  Result := TOALSound.Create( aFilename );
  FList.Add( Result );
 finally
  LeaveCriticalSection( FCriticalSection );
 end;
end;

procedure TOALCustomAudioManager.Delete(ASound: TOALSound);
begin
 DoDeleteSound( FList.IndexOf( ASound ));
end;

{ TOALSound }
constructor TOALSound.Create(const aFilename: string);
begin
 Volume := TVolumeParam.Create;
 Volume.FParentOALSound := Self;
 Volume.MaxValue := oal_VOLUME_MAX;
 Volume.MinValue := 0;
 Volume.Value := oal_VOLUME_MAX;

 Pitch := TPitchParam.Create;
 Pitch.FParentOALSound := Self;
 Pitch.MaxValue := oal_PITCH_MAX;
 Pitch.MinValue := oal_PITCH_MIN;
 Pitch.Value := oal_PITCH_NORMAL;

 FPaused:=FALSE;
 FLoop := FALSE ;
 FError := FALSE;

 FWavLoader:= TWavLoader.Create;
 FWavLoader.OpenFile( aFilename );

 FSamples := FWavLoader.GetAllData;

 alGenBuffers(1, @FBuffer);
 alBufferData(FBuffer, GetFormat, @FSamples[0], FWavLoader.GetDataSizeInByte, GetFrequency );

 alGenSources(1, @FSource ) ;
 alSourcei(FSource, AL_BUFFER, FBuffer) ;
end;

destructor TOALSound.Destroy;
begin
 alDeleteBuffers(1, @FBuffer);
 alSourcei(FSource, AL_BUFFER, 0);
 alDeleteSources(1, @FSource);

 SetLength( FSamples, 0 );

 Volume.Free ;
 Pitch.Free;
 FWavLoader.Free;
 inherited Destroy;
end;

function TOALSound.GetChannelCount: integer;
begin
 Result := FWavLoader.GetChannelCount;
end;

function TOALSound.GetFormat: word;
begin
 Result := FWavLoader.GetFormat;
end;

function TOALSound.GetFrequency: integer;
begin
 Result := FWavLoader.GetFrequency;
end;

function TOALSound.GetSampleCount: QWord;
begin
 Result := FWavLoader.GetSampleCount;
end;


procedure TOALSound.SetLoop(AValue: boolean);
var v : integer ;
begin
 if FError then exit ;
 if AValue then v := 1
           else v := 0 ;
 alSourcei( FSource, AL_LOOPING, v );
 FLoop := AValue ;
end;

procedure TOALSound.SetALVolume;
begin
 if FError then exit;
 alSourcef( FSource, AL_GAIN, Volume.Value / oal_VOLUME_MAX );
end;

procedure TOALSound.SetALPitch;
begin
 if FError then exit;
 alSourcef( FSource, AL_PITCH, Pitch.Value );
end;

procedure TOALSound.Update(const aElapsedTime: single);
var v,p: single;
begin
 v := Volume.Value;
 Volume.OnElapse( aElapsedTime );

 p := Pitch.Value;
 Pitch.OnElapse( aElapsedTime );

 if FError then exit;

 if v <> Volume.Value then SetALVolume;

 if ( Volume.State = psNO_CHANGE ) and FVolumeFadeOut then begin
   FVolumeFadeOut := FALSE;
   alSourceStop( FSource );
 end;

 if p <> Pitch.Value then SetALPitch;

end;

function TOALSound.GetTimePosition: single;
begin
 if FError
   then Result := 0
   else alGetSourcef(FSource, AL_SEC_OFFSET, Result);
end;

function TOALSound.Byte2Seconds(aPosition: QWord): single;
begin
 if FError
   then Result := 0
   else Result := aPosition / GetFrequency ;
end;

function TOALSound.Seconds2Byte(aTimePosition: single): QWord;
begin
 if FError
   then Result := 0
   else Result := round( aTimePosition * GetFrequency ) ;
end;


function TOALSound.TotalDuration: single;
begin
 if FError
   then Result := 0
   else Result := GetSampleCount / GetFrequency ;
end;

procedure TOALSound.Play(AFromBegin: boolean);
begin
 if FError then exit;

 case GetState of
  AL_STOPPED, AL_INITIAL: begin
   alSourceRewind( FSource );
   alSourcePlay( FSource );
   SetALVolume;
  end;
  AL_PLAYING: begin
   if AFromBegin then begin
     alSourceRewind( FSource );
     alSourcePlay( FSource );
   end;
  end;
  AL_PAUSED: begin
   alSourcePlay( FSource );
  end;
 end;
 FVolumeFadeOut := FALSE;
end;

procedure TOALSound.Stop;
begin
 if FError then exit;
 alSourceStop(FSource);
 FVolumeFadeOut := FALSE;
end;

procedure TOALSound.Pause;
begin
 if FError then exit;

 case GetState of
  AL_PAUSED: Play( FALSE );
  AL_PLAYING: alSourcePause( FSource );
  AL_STOPPED,AL_INITIAL:;
 end;
 FVolumeFadeOut := FALSE;
end;

procedure TOALSound.FadeIn(ATimeSec: single; AVelocityCurve: word);
begin
 FadeIn( oal_VOLUME_MAX, ATimeSec, AVelocityCurve );
end;

procedure TOALSound.FadeIn(AVolume: integer; ATimeSec: single;
  AVelocityCurve: word);
begin
 if FError then exit;

 case GetState of
  AL_STOPPED, AL_INITIAL: begin
    Volume.Value := 0.0;
    Volume.ChangeTo( AVolume, ATimeSec, AVelocityCurve );
    alSourcePlay( FSource );
  end;
  AL_PAUSED: begin
    Volume.Value := 0.0;
    Volume.ChangeTo( AVolume, ATimeSec, AVelocityCurve );
    alSourcePlay( FSource );
  end;
  AL_PLAYING: Volume.ChangeTo(aVolume, ATimeSec, AVelocityCurve );
 end;
 FVolumeFadeOut := FALSE;
end;

procedure TOALSound.FadeOut(ATimeSec: single; AVelocityCurve: word);
begin
 if FError then exit;

 case GetState of
  AL_STOPPED, AL_INITIAL:;
  AL_PLAYING: begin
   Volume.ChangeTo( 0, ATimeSec, AVelocityCurve);
   FVolumeFadeOut := TRUE;
  end;
  AL_PAUSED: begin
   Volume.ChangeTo( 0, ATimeSec, AVelocityCurve);
   FVolumeFadeOut := TRUE;
   alSourcePlay( FSource );
  end;
 end;
end;

function TOALSound.GetState: ALInt;
begin
 if FError
   then Result := AL_INITIAL
   else alGetSourcei( FSource, AL_SOURCE_STATE, Result );
end;

Initialization
OALSoundManager := TOALCustomAudioManager.Create ;
Finalization
OALSoundManager.Free;

end.

