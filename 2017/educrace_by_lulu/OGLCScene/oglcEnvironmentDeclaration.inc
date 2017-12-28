type

  TValue = NativeInt;
const
  watersizex = 640;
  watersizey = 480;
  precisionFactor = 256; //use power of two to improve compiler optimisation
  gravity = precisionFactor * 2 div 10;
  friction = precisionFactor * 1 div 256;
type
  PWaterPixel = ^TWaterPixel;
  TWaterPixel = packed record
    h,         //height (with average equal to zero)
    v: TValue; //speed (multiplied by precisionFactor)
  end;

{ TWaterSurface }

TWaterSurface = class( TSimpleSurfaceWithEffect )
private
   w : packed array[0..watersizey, 0..watersizex] of TWaterPixel;
   FC: single;
   FRainRadius,
   FRainHeight: integer;
   FWaterIsTransparent: boolean;
   function clamp(AValue: TValue; AMin,AMax: TValue): TValue; inline;
private
   FTexture: PTexture;
   FWidth, FHeight: integer;
protected
   function GetWidth: integer; override;
   function GetHeight: integer; override;
public
   procedure Update( const aElapsedTime: single ); override;
   procedure Draw( const aLayerPercentOpacity: single );  override;
public
   constructor Create;
   Destructor Destroy; override;
   procedure WaterReset;

   procedure SetSize( aWidth, aHeight: integer );
   procedure WaterDrop(aX, aY, r, dropHeight: TValue);
public
   RainAmount: TBoundedFParam;  // 0=no rain    10=max rain
   WaterColor: TBGRAParam;
   property WaterIsTransparent: boolean read FWaterIsTransparent write FWaterIsTransparent; // if true, draw only waterdrop and not static water
   property RainRadius: integer read FRainRadius write FRainRadius;
   property RainHeight: integer read FRainHeight write FRainHeight;
end;



{ TSnow }
const MAX_FLAKE = 1000;
type
PFlake = ^TStructFlake;
TStructFlake = record
  fx         : single ;
  fy         : single ;
  sx         : single ;
  sy         : single ;
  anglex     : single ;
  amplitudex : single ;
  xdraw      : single ;
 end;

TArrayOfFlake = array of TStructFlake;

TSnow = class( TSimpleSurfaceWithEffect )
private
   FTexture: array[0..2] of PTexture; // 3 sizes of flakes  0= smallest  1=middle  2=normal size
   FWidth, FHeight: integer;
   FFlakeArray: array[0..2] of TArrayOfFlake;
   function GetOneTypeFlakeCount: integer;
   procedure InitFlake( aPFlake: PFlake );
   procedure InitLittleFlake( aPFlake: PFlake );
   procedure InitMiddleFlake( aPFlake: PFlake );
   procedure InitBigFlake( aPFlake: PFlake );
protected
   function GetWidth: integer; override;
   function GetHeight: integer; override;
public
   constructor Create;
   Destructor Destroy; override;
public
   procedure Update( const aElapsedTime: single ); override;
   procedure Draw( const aLayerPercentOpacity: single );  override;

   procedure SetSize( aWidth, aHeight: integer );

public
   FlakeAmount: TBoundedFParam;  // 0=no snow    1000=max
   FlakeSpeed: TPointFParam;     // speed on both axis in pixels per second
   HAmplitude: TFParam;          // horizontal amplitude in pixel
   Gravity: TPointFParam;

   procedure UseTexture( aTextureFile: string ); overload; // set the image flake
   procedure UseTexture( aImage: TBGRABitmap ); overload;
end;


{
 TStrand = record
   tex      : PTexture;
   percent  : single; //
   min_scale: single;
   max_scale: single;
 end;
 PStrand = ^TStrand;
 TArrayOfStrand= array of TStrand;

 { TGrassArea }

 TGrassArea= class( TSimpleSurfaceWithEffect )
 private
  FStrandList: TArrayOfStrand;
  FSurfaceList: TSurfaceList;     // list of TSimpleSurfaceWithEffect;
   // Internal procedure wrapper
   procedure DoUpdate ( const aElapsedTime : double ) ;
   procedure DoDraw( const aLayerPercentOpacity : single ) ;
   procedure DoDestroy ;
 public
   Constructor Create( aX, aY: single; aWidth, aHeight: integer );
   Destructor Destroy; override;

   procedure AddGrassTexture( aFilename: string; aPercentage: single=100.0; aMinScale: single=1.0; aMaxScale: single= 1.0);  // example: one grass type 60%, and another grass type 40%
   procedure AddGrassTexture(aTex: PTexture; aPercentage: single; aMinScale: single; aMaxScale: single);
   procedure RandomizeGrass( aGrassCountPerLine, aLineCount: integer; aDuration: single ); // duration= time for one undulation

 end;

 TBubble= class
 end;
}


