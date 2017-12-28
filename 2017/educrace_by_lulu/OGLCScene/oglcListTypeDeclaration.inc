type

TTexCoor = array[0..3] of TPointF;

PTexture = ^TTexture ;

{ TTexture }

TTexture = record
            ID            : GLuint   ;
            TextureWidth  : integer  ;
            TextureHeight : integer  ;
            ImageWidth    : integer  ;
            ImageHeight   : integer  ;
            FrameWidth    : integer  ;
            FrameHeight   : integer  ;
            FrameCount    : integer  ;
            FramesCoord   : array of TTexCoor;  // [0] is whole texture, [1] is first frame, [2] is second, ...
            Next          : PTexture ;
            Prev          : PTexture ;
           end;

{ TTextureManager }

TTextureManager = class
 Constructor Create;
 Destructor Destroy; override;
private
 FTexture0 : PTexture;
 FCount    : integer;
 FCurrentBindedTexture : PTexture;
 FGL_TEXTURE_2DEnabled : boolean;
 function ValueOfNextPowerOfTwo ( aValue:integer ):integer;
 function AddTextureToList: PTexture ;
 function InitFromBGRABitmap( aIma: TBGRABitmap ): PTexture;
 procedure SetFrameSize( ATexture: PTexture; AFrameWidth, AFrameHeight: integer );
 procedure ComputeOpenGLFramesCoord(ATexture: PTexture; FramesX: Integer=1; FramesY: Integer=1);
public
 function Add( const aFilename: string ): PTexture; overload;
 function Add( const aFilename: string; AFrameWidth, AFrameHeight: integer ): PTexture; overload;
 function Add( aImage: TBGRABitmap ): PTexture; overload;
 function Add( aImage: TBGRABitmap; AFrameWidth, AFrameHeight: integer ): PTexture; overload;
 function AddEmpty( aWidth, aHeight: integer ): PTexture;

 procedure Replace( ATexture: PTexture; const aFilename : string ); overload;
 procedure Replace( ATexture: PTexture; aImage : TBGRABitmap ); overload;

 procedure Delete ( aTexture: PTexture );
 procedure Clear;

 procedure BindTexture( ATexture: PTexture );  // do 'glEnable( GL_TEXTURE_2D )' and 'glBindTexture(...)' only if necessary
 procedure DisableTextureUsage;                // do 'glDisable( GL_TEXTURE_2D )' only if necessary

 function GetFrameCount( ATexture: PTexture ): TSize;
 property Count: integer read FCount;
end;

var
 TextureManager : TTextureManager;

type

PTimerObject = ^TTimerObject;
TTimerObject = record
                ID       : integer      ;
                Interval : integer      ;
                Time     : TTimeStamp   ;
                CallBack : TProcEvent   ;
                Next     : PTimerObject ;
                Prev     : PTimerObject ;
                Kill     : boolean      ;
               end;

{ TTimerManager }

TTimerManager = class
 Constructor Create ;
 Destructor Destroy ; override ;
private
 FTimer0   : PTimerObject;
 FCount    : integer;
 function AddTimerToList : PTimerObject;
 procedure ProcessTimer;
 procedure DoDelete ( aTimer: PTimerObject );
 procedure ClearAll;
 procedure ClearAllUserTimer;
public
 function Add( aCallBack : TProcEvent; aIntervalMSec: integer ) : PTimerObject;
 procedure Delete ( aTimer: PTimerObject );
 procedure Clear; // clear all user's timer
 property Count : integer read FCount;
end;



{ TTexturedFont }

TTexturedFont = class
 Constructor Create;
 Destructor Destroy ; override ;
private
 FTexCharArray : array of PTexture ;
 FCharList : ansistring ;
 FFontName       : string ;
 FFontHeight     : integer ;
 FStyle          : TFontStyles ;
 FFontColor      : TBGRAPixel ;
 FOutLineColor   : TBGRAPixel ;
 FOutLineWidth   : single ;
 FShadowColor    : TBGRAPixel ;
 FShadowOffsetX  : integer ;
 FShadowOffsetY  : integer ;
 FShadowRadius   : integer ;
 function IndexOf( c : char ): integer;
public
 procedure SetCharList( s:ansistring ) ;
 procedure Build(FontName: string; FontHeight: integer;
                 Style: TFontStyles; FontColor: TBGRAPixel; OutLineColor: TBGRAPixel;
                 OutLineWidth: single; ShadowColor: TBGRAPixel; ShadowOffsetX, ShadowOffsetY, ShadowRadius: integer;
                 FontQuality: TBGRAFontQuality=fqFineAntialiasing );
 function GetCharTexture( c : char ) : PTexture ;
 function GetCharWidth( c : char ): integer ;
 function GetCharHeight( c : char ): integer ;
 function Text2Bitmap ( aCaption: UTF8String; aBackGround: TBGRABitmap=NIL ) : TBGRABitmap ;
end;


{ TFontManager }

TFontManager = class
 Constructor Create;
 Destructor Destroy ; override ;
private
 FList : TList ;
 FCharList : ansistring ;
public
 function AddFont( FontName:string; FontHeight:integer; Style:TFontStyles; FontColor : TBGRAPixel;
                   OutLineColor: TBGRAPixel; OutLineWidth:single;
                   ShadowColor: TBGRAPixel; ShadowOffsetX, ShadowOffsetY, ShadowRadius: integer;
                   FontQuality: TBGRAFontQuality=fqFineAntialiasing): TTexturedFont ;
 procedure Clear ;

 property CharList : ansistring read FCharList write FCharList ;
end;

var
 FontManager : TFontManager ;

const
MAX_PARTICLES = 1024 ;

type

TParticle = record
              X, Y    : single ;
              VX, VY  : single ;
              CurrentLife  : single ;
              InitialLife  : single ;

              FrameIndex : single;
              FrameCoeff : single;

              CurrentColor : TBGRAPixel ;
              FColorIndex  : integer ;

              InitialSize : single ;
              CurrentSize : single ;
              FSizeIndex  : integer ;

              InitialVelocity : single ;
              CurrentVelocity : single ;
              FVelocityIndex : integer ;

              InitialAVelocity : single ;
              CurrentAVelocity : single ;
              FAVelocityIndex : integer ;

              CurrentDirection : single ;

              InitialSpin : single  ;
              FSpinIndex  : integer ;
              CurrentAngle : single  ;
             end;

PPColor = ^TPColor ;
TPColor = record
            C    : TBGRAPixel ;
            Life : single  ;
end;
PPsingle = ^TPSingle ;
TPSingle = record
             Value : single ;
             Life  : single ;
end;

TParticleParameter = record
                      Texture   : PTexture ;
                      StartFrame : integer;
                      EndFrame   : integer;
                      ColorMode : byte ;
                      BlendMode : byte ;
                      Life          : single ;
                      LifeVariation : single ;
                      ArrayColor : array of TPColor ;
                      Velocity : single ;
                      VelocityVariation : single ;
                      ArrayVelocity : array of TPSingle ;
                      AVelocity : single ;
                      AVelocityVariation : single ;
                      ArrayAVelocity : array of TPSingle ;
                      Size : single ;
                      SizeVariation : single ;
                      ArraySize : array of TPSingle ;
                      Spin : single ;
                      SpinVariation : single ;
                      ArraySpin : array of TPSingle ;
                      StartAngle : single ;
                      StartAngleVariation : single ;
end;

PShader = ^TShader;
TShader = record
             ProgramID,
             FragmentID,
             VertexID: GLUInt;
end;

TShaderID = integer;

{ TShaderManager }

TShaderManager = class
 private
  FList : TList;
  procedure NotifyFragmentCompilationError;
  procedure NotifyVertexCompilationError;
  procedure NotifyLinkError;
  function getShader( aIndex: integer ): PShader;
  procedure Clear;
 public
  constructor Create;
  Destructor Destroy; override;
  function ShaderAvailable: boolean;

  function CreateShader( const AVertexCodeSource, AFragmentCodeSource: string ): TShaderID; // return created shader ID
  procedure DeleteShader( AShader: TShaderID );

  procedure Use( AShader: TShaderID );
  procedure UseNone;

  function GetUniform( AShader: TShaderID; const AName ): GLint;
  procedure SetUniform1f( AUniform: GLint; AValue: single );
  procedure SetUniform2f( AUniform: GLint; AValue1, AValue2: single );
  procedure SetUniform3f( AUniform: GLint; AValue1, AValue2, AValue3: single );
  procedure SetUniform4f( AUniform: GLint; AValue1, AValue2, AValue3, AValue4: single );

  procedure SetUniform1i( AUniform: GLint; AValue: GLint );
end;

var
 ShaderManager : TShaderManager=NIL;

const

  GLOW_VERTEX_SHADER_PRG = 'void main() {'+
                           'gl_Position = ftransform();'+  //gl_Vertex;'+
                           '}';

 GLOW_FRAGMENT_SHADER_PRG = 'uniform vec2 lightCenter;'+
                            'uniform vec4 lightColor;'+
                            'uniform float lightRadius;'+
                            'uniform float powerAttenuation;'+
                            'void main() {'+
                            'float d = distance( lightCenter, gl_FragCoord.xy ) / lightRadius;'+
                            'float attenuation = 1 - d;'+
                            'vec4 color = vec4(attenuation, attenuation, attenuation, pow(attenuation, powerAttenuation)) * lightColor;'+
                            'gl_FragColor = color;'+
                            '}' ;


 // sin
 SIN_VERTEX_SHADER_PRG = 'void main() {'+
                         'gl_TexCoord[0] = gl_TextureMatrix[0] * gl_MultiTexCoord0;'+
                         'gl_Position = ftransform();'+
                         'gl_FrontColor = gl_Color;'+
                         '}';
 SIN_FRAGMENT_SHADER_PRG = 'uniform sampler2D tex;'+
                           'void main() {'+
                           'vec4 pixel = texture2D(tex, gl_TexCoord[0].xy);'+
                           'gl_FragColor = vec4(0.0,1.0,0.0,1.0);'+//* pixel;'+ // gl_Color * pixel;
                           '}' ;


