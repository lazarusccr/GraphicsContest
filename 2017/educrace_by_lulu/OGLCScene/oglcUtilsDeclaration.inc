
function BGRAPixelToHex(aColor: TBGRAPixel): string;
function HexToBGRAPixel(const s: string): TBGRAPixel;

function Text2Bitmap ( aCaption: UTF8String; FontName:string; FontHeight:integer; Style:TFontStyles; FontColor : TBGRAPixel;
                  OutLineColor: TBGRAPixel; OutLineWidth:single;
                  ShadowColor: TBGRAPixel; ShadowOffsetX, ShadowOffsetY, ShadowRadius: integer;
                  aBackGroundTexture: TBGRABitmap=NIL) : TBGRABitmap ;

function SplitLineToStringArray( ALine: string; aCharSeparator: char ): ArrayOfString;

procedure DrawTexture( aTexture: PTexture; AFrameIndex: integer; X, Y, aAngle: single; aAlpha: byte; ATint: TBGRAPixel ); overload;
procedure DrawTexture( aTexture: PTexture; AFrameIndex: integer; X, Y: single; Param: TSimpleSurfaceWithEffect; ALayerPercentOpacity: single=1.0 ); overload;

procedure DrawBox(X, Y, Width, Height: single; const BorderColor: TBGRAPixel; const aLayerPercentOpacity: single=1.0 ); overload;
procedure DrawBox(X, Y, Width, Height: single; const BorderColor, FillColor: TBGRAPixel; const aLayerPercentOpacity: single=1.0 ); overload;

procedure FillBox(X, Y, Width, Height: single; FillColor: TBGRAPixel; const aLayerPercentOpacity: single=1.0 );
procedure FillBox(X, Y, Width, Height: single; const aTopLeftColor, aTopRightColor, aBottomRightColor, aBottomLeftColor: TBGRAPixel; const aLayerPercentOpacity: single=1.0 ); overload;
procedure DrawLine( x1, y1, x2, y2: single; Color: TBGRAPixel; const aLayerPercentOpacity: single=1.0 );

type
TBlendFuncSeparateProc = procedure(sfactorRGB: GLenum; dfactorRGB: GLenum; sfactorAlpha: GLenum; dfactorAlpha: GLenum); {$IFDEF Windows} stdcall; {$ELSE} cdecl; {$ENDIF}
procedure SetBlendMode( ABlendMode: byte );

var
  _glBlendFuncSeparateInitialized: boolean = FALSE;
  _FglCurrentBlendMode: byte = $FF;

const
  deg2rad = 0.017453292;
  rad2deg = 57.29578049;


