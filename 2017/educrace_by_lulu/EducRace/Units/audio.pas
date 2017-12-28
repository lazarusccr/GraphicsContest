unit Audio;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms,
  common,
  OpenALSoundManager;

procedure LoadSounds ;


implementation

procedure LoadSounds ;
begin
// Sounds
 SHCarStart := OALSoundManager.Add( AUDIO_PATH+'CarStartEngine.wav' ) ;
 SHCarJump := OALSoundManager.Add( AUDIO_PATH+'CarJump.wav' ) ;
 SHCarCollideVehicle := OALSoundManager.Add( AUDIO_PATH+'CarCollideVehicle.wav' ) ;
 SHCarCollideStrawBale := OALSoundManager.Add( AUDIO_PATH+'CarCollideStrawBale.wav' );
 SHCarCrash := OALSoundManager.Add( AUDIO_PATH+'CarCrash.wav' );

 SHCarEngineLoop := OALSoundManager.Add( AUDIO_PATH+'CarEngineLoop.wav' ) ;
 SHCarEngineLoop.Loop := TRUE ;
 SHTiresScreechingLong := OALSoundManager.Add( AUDIO_PATH+'TiresScreechingLong.wav' ) ;

 SHHorn1 := OALSoundManager.Add( AUDIO_PATH+'Horn1.wav' ) ;
 SHHorn2 := OALSoundManager.Add( AUDIO_PATH+'Horn2.wav' ) ;
 SHHorn3 := OALSoundManager.Add( AUDIO_PATH+'Horn3.wav' ) ;
 SHHorn4 := OALSoundManager.Add( AUDIO_PATH+'Horn4.wav' ) ;

 SHTableWin := OALSoundManager.Add( AUDIO_PATH+'TableWin.wav' ) ;
 SHTableLost := OALSoundManager.Add( AUDIO_PATH+'TableLost.wav' ) ;

 SHFallInWater := OALSoundManager.Add( AUDIO_PATH+'FallInWater.wav' ) ;

 SHWarning := OALSoundManager.Add( AUDIO_PATH+'Warning.wav' );
 SHWarning.Loop := TRUE;

 SHHelico := OALSoundManager.Add( AUDIO_PATH+'helicopter.wav' );
 SHHelico.Loop := TRUE;

 SHTrainPassing := OALSoundManager.Add( AUDIO_PATH+'train.wav' );
 SHTrainPassing.Loop := TRUE;

// music
 SHMusicIntro := OALSoundManager.Add( AUDIO_PATH+'MusicIntro.wav' );
 SHMusicIntro.Loop := TRUE;
 SHMusicIntro.Volume.Value := 600;

 SHMusicEndStage := OALSoundManager.Add( AUDIO_PATH+'MusicEndStage.wav' );
 SHMusicEndStage.Loop := TRUE;
end;

end.

