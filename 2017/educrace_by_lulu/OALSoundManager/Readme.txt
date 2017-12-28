OpenAL Sound Manager

this unit allow you to play audio using OpenAL library.


Limitations:
    - only wav files are supported.
    - sounds are loaded entirely in memory before play, no streaming support.


Below, some functions ask 'VelocityCurve' in their parameters.
There are 5 velocity curve available:
  idcLinear >> by default, linear change is applied 
  idcStartFastEndSlow >> as it means
  idcStartSlowEndFast
  idcSinusoid
  idcSinusoid2


Usage :

- In uses clause, add
     Uses OpenAlSoundManager,
          variable_parameter_V2;

- Declare a variable: 
    MySound: TOALSound;

- Load a sound
     MySound := OALSoundManager.Add('path\sound.wav');

- Looped Sound
     MySound.Loop := TRUE;


- Use standard command
     MySound.Play( AFromBegin: boolean=TRUE );
     MySound.Pause;
     MySound.Stop;

- To do a fade in ( the sound start with volume to 0 and increase gradually)
     MySound.FadeIn( ATimeSec: single; AVelocityCurve: word=idcLinear );
     MySound.FadeIn( AVolume: integer; ATimeSec: single; AVelocityCurve: word=idcLinear );

- To de an fade out
     MySound.FadeOut( ATimeSec: single; AVelocityCurve: word=idcLinear );

- Fixed volume
     MySound.Volume.Value := 1000; // volume range [0..1000]

- To change volume gradually using velocity curve
     MySound.Volume.ChangeTo(aNewValue, aSecond: single; aCurveID: word = idcLinear);


lulu
contact@lulutech.fr