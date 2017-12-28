type

{ TSimpleSurface }
TSimpleSurface = class
 Constructor Create;
 Destructor Destroy; override;
protected
 FParentScene  : TOGLCScene ;
 FParentLayer  : TLayer  ;
 FVisible      : boolean ;
 FIndex        : integer ;
 FTag1         : integer ;
 FTag2         : boolean ;
 FGroup        : integer ;
 FKill         : boolean ;
 function GetCenterX: single;
 function GetRightX: single;
 function GetBottomY: single;
 function GetCenterY: single;
 procedure SetCenterX(const AValue: single);
 procedure SetRightX(const AValue: single);
 procedure SetBottomY(const AValue: single);
 procedure SetCenterY(const AValue: single);
protected
 function GetWidth: integer; virtual; abstract;
 function GetHeight: integer; virtual; abstract;
public
 procedure Draw( const aLayerPercentOpacity: single ); virtual; abstract;
 procedure Update( const AElapsedTime: single ); virtual; abstract;
public
 X, Y        : TFParam;
 function GetRectArea: TRect;
 // kill
 procedure Kill;
 // Coordinates
 procedure SetCoordinate( aX, aY: single );
 procedure SetCenterCoordinate ( aCenterX , aCenterY : single) ;
 property CenterX : single read GetCenterX write SetCenterX ;
 property RightX : single read GetRightX write SetRightX ;
 property CenterY : single read GetCenterY write SetCenterY ;
 property BottomY : single read GetBottomY write SetBottomY ;
 // Size
 property Width : integer read GetWidth;
 property Height : integer read GetHeight;
 // Visible
 property Visible : boolean read FVisible write FVisible ; // =TRUE pour que la surface soit affichée
 property SceneIndex : integer read FIndex write FIndex ; // index de la surface dans la scène
 property Tag1 : integer read FTag1 write FTag1 ;
 property Tag2 : boolean read FTag2 write FTag2 ;
 property Group : integer read FGroup write FGroup ;
 property ParentLayer: TLayer read FParentLayer;
end;

TScenarioEvent = procedure ( aSurface: TSimpleSurfaceWithEffect;
                             aScenarioID: TIDScenario;
                             aUserEvent: integer ) of object ; // CallBack for scenario event

{ TScenario }

TScenario = class
 Constructor Create ;
 Destructor Destroy ; override ;
protected
 ID           : integer     ;
 Actions      : TStringlist ;
 CurrentLine  : integer     ;
 WaitTime     : single      ;
 ParentSurface: TSimpleSurfaceWithEffect ;
 FIsRunning   : boolean     ;
 FTemp        : ArrayOfString ;
 FScenarioEvent : TScenarioEvent ;
 procedure Execute ( const aElapsedTime : double ) ;
 procedure LoadFromFile ( const aFilename: string ) ;
 procedure SaveToFile ( const aFilename: string ) ;
 procedure DecodeAction;
 procedure DecodeAdditionnalAction( aAction: ArrayOfString ); virtual;
public
 procedure AddLine( const aLine: string ) ;  // to construct the scenario manually
 property OnEvent: TScenarioEvent read FScenarioEvent write FScenarioEvent ; // Scenario event callback
end;

{ TSimpleSurfaceWithEffect }

TSimpleSurfaceWithEffect = class ( TSimpleSurface )
  Constructor Create;
  Destructor Destroy; override;
private
  procedure Init;
  procedure SetFrame(AValue: single);
protected
 FChilds     : TList;
 FParentSurface: TSimpleSurfaceWithEffect;
protected
   FBlendMode: byte;
   FFreeze       : boolean ;
   FTimeBeforeKill: single;
   // Blink
   FBlink : TBlink;
   // Rotation around axis
   FRotationAroundPt     : TRotationAroundPoint ;
   // Flip
   FFlipH:boolean;
   FFlipV:boolean;
   // Frame animation
   FFrame: single;
   FFrameDeltaPerSecond: single;
   FFrameLoopStart,
   FFrameLoopEnd: integer;
   // Bind Surface
   FBindToSprite  : TBindToSprite ;
protected
   FListScenario : array of TScenario ;    // surface can execute several scenario at the same time
   function GetScenario( AIDScenario: TIDScenario ): TScenario;
protected
   procedure PushAndPrepareMatrix; inline;
   procedure PopMatrix; inline;
   procedure DrawChildSurfaces( const aLayerPercentOpacity: single );
   procedure DrawSurface( aTexture: PTexture; AFrameIndex: integer; const aLayerPercentOpacity : single ); overload;
   procedure DrawSurface( const aTopLeftColor, aTopRightColor, aBottomRightColor, aBottomLeftColor: TBGRAPixel; const aLayerPercentOpacity : single ); overload;
public
   procedure Update( const aElapsedTime: single ); override;
   procedure Draw( const aLayerPercentOpacity: single );  override;
public
   Speed   : TPointFParam;
   Angle   : TFParam;
   Opacity : TBoundedFParam;
   Tint    : TBGRAParam;
   Scale   : TPointFParam;
   Skew    : TPointFParam;
   // add a child to the surface. AOffset is relative to top left corner (X,Y)
 //  procedure AddChild( ASurface: TSimpleSurfaceWithEffect; AOffset: TPointF );overload;
 //  procedure AddChild( ASurface: TSimpleSurfaceWithEffect; AXOffset, AYOffset: single );overload;
   procedure AddChild( ASurface: TSimpleSurfaceWithEffect );overload;  // set offset with SetCoordinate or SetCenterCoordinate
   // Kill defered
   procedure KillDefered( ASec: single );
   // Move
   procedure MoveTo( aX, aY: single ; aDuration: single; aCurve:Word= idcLinear );
   procedure MoveTo( aX, aY: single ; aDuration: single; aXCurve, aYCurve: Word );
   procedure MoveCenterTo( aCenterX, aCenterY, aDuration: single; aCurve:Word = idcLinear );
   procedure MoveXCenterTo( aX, aDuration: single; aCurve:Word = idcLinear );
   procedure MoveYCenterTo( aY, aDuration: single; aCurve:Word = idcLinear );
   procedure MoveRelative( aDeltaX, aDeltaY: single; aDuration: single; aCurve: Word = idcLinear );
   procedure MoveXRelative( aDeltaX, aDuration: single; aCurve: Word= idcLinear );
   procedure MoveYRelative( aDeltaY, aDuration: single; aCurve: Word= idcLinear );
   // Blink
   procedure Blink ( aNumberOfBlink : integer ; aVisibleTime , aInvisibleTime : single ) ; // aNumberOfBlink=-1 for infinite blink
   procedure StopBlink;
   // Rotate around point
   procedure RotationAroundPoint( aXPt, aYPt: single; aAnglePerSecond: single; aSelfRotate: boolean=TRUE ) ;
   // Bind this surface to another
   procedure BindToSprite ( aParentSurface:TSimpleSurface; aOffsetX, aOffsetY: single );
   procedure BindCenterToSprite( aParentSurface:TSimpleSurface );
   procedure UnBind;
   // Scenario
   function AddScenario( const aFilename: string; ACallBackEvent: TScenarioEvent=NIL ): TIDScenario;overload; // return Scenario's ID
   function AddScenario( aScenario: TScenario ): TIDScenario;overload;
   procedure PlayScenario( AIDScenario: TIDScenario; AFromBeginning: boolean=TRUE );
   function AddAndPlayScenario( const aFilename: string; ACallBackEvent: TScenarioEvent=NIL ): TIDScenario;overload; // return Scenario's ID
   function AddAndPlayScenario( aScenario: TScenario ): TIDScenario; overload;
   procedure StopScenario( AIDScenario: TIDScenario );
   procedure StopAllScenario;
   function ScenarioIsPlaying( AIDScenario: TIDScenario ): boolean;
   // Frame anime
   procedure SetFrameLoopBounds( AStartFrameIndex, AEndFrameIndex: integer );
   procedure FrameAddPerSecond( ADeltaFramePerSecond: single );
   property Frame: single read FFrame write SetFrame;
   // Flip H and V
   property FlipH : boolean read FFlipH write FFlipH ;
   property FlipV : boolean read FFlipV write FFlipV ;

   property Freeze : boolean read FFreeze write FFreeze ;  // set to TRUE to stop all variations on surface ( move, opacity change, scale change, etc...)
   property BlendMode: byte read FBlendMode write FBlendMode;
end;

TArrayOfPTexture = array of PTexture;

{ TSprite }

TSprite = class ( TSimpleSurfaceWithEffect )
private
   procedure Init;
protected
   FTextureList     : TArrayOfPTexture; // sprite can contain multiple textures.
   FTextureIndex    : integer;
   function GetTextureCount: integer;
   procedure SetTextureIndex(AValue: integer);
   function GetWidth: integer; override;
   function GetHeight: integer; override;
public
   procedure Draw( const aLayerPercentOpacity : single ) ; override;
public
   Constructor create( ATexture: PTexture );
   Destructor Destroy; override;
   // sprite with multiple texture
   function AddTexture ( ATexture: PTexture ):integer;
   procedure ClearAllTexture; // textures are freed by TextureManager

   property Textures: TArrayOfPTexture read FTextureList;
   property TextureCount: integer read GetTextureCount;
   property TextureIndex: integer read FTextureIndex write SetTextureIndex; // current texture

end;

TArrayOfSprite = array of TSprite ;
PArrayOfSprite = ^TArrayOfSprite ;



{ TColorBackground }

TColorBackground = class( TSimpleSurfaceWithEffect )
   Constructor Create( aX, aY: single; aWidth, aHeight : integer );
   Destructor Destroy; override;
protected
   FWidth, FHeight: integer;
   function GetWidth: integer; override;
   function GetHeight: integer; override;
public
   procedure Update( const aElapsedTime: single ); override;
   procedure Draw( const aLayerPercentOpacity: single ); override;
public
   TopLeftColor,
   TopRightColor,
   BottomRightColor,
   BottomLeftColor: TBGRAParam;
   procedure SetSize( aWidth, aHeight: integer );
   procedure SetAllColorsTo( AColor: TBGRAPixel );
   procedure SetLeftColors( AColor: TBGRAPixel );
   procedure SetRightColors( AColor: TBGRAPixel );
   procedure SetTopColors( AColor: TBGRAPixel );
   procedure SetBottomColors( AColor: TBGRAPixel );
end;


{ TMouseManager }

TMouseManager = class
 Constructor Create ;
 Destructor Destroy; override ;
protected
 FTexture: PTexture;
 FSprite : TSprite;
 FParentScene : TOGLCScene;
 procedure ProcessMouse;
 procedure DeleteObjects;
 procedure UpDate( AElapsedTime: single );
public
 procedure SetCursor( const aFilename : string ); overload;
 procedure SetCursor( const aFilename : string; aFrameWidth, aFrameHeight: integer ); overload;
 procedure SetCursor( aImage : TBGRABitmap ); overload;
 procedure SetCursor( aImage : TBGRABitmap; aFrameWidth, aFrameHeight: integer ); overload;
 property MouseSprite: TSprite read FSprite ; // for accessing all fx methods and property of TSprite
end;

{ TGuiButton }

TGuiEvent = procedure ( aGUISurface:TSimpleSurfaceWithEffect ) of object ;

TGuiButton = class ( TSprite )
private
  FMouseEntered : boolean;
  FOnMouseEnter : TGuiEvent ;
  FMouseLeave   : TGuiEvent ;
  FOnClick      : TGuiEvent ;
  procedure Init;
public
  procedure Update( const aElapsedTime : single ) ; override;
public
  Constructor Create( aX, aY : integer; const aFilename : string );   // from bitmap file
  Constructor Create( aX, aY : integer; aImage : TBGRABitmap );       // from BGRABitmap
  Constructor Create( aX, aY : single; aCaption : UTF8String; aFont: TTexturedFont; aBackgroundFilename : string='' ); // text with background

  property OnMouseEnter : TGuiEvent read FOnMouseEnter write FOnMouseEnter;
  property OnMouseLeave : TGuiEvent read FMouseLeave write FMouseLeave;
  property OnClick      : TGuiEvent read FOnClick write FOnClick;
end;


{ TGuiLabel }

TGuiLabel = class ( TSimpleSurfaceWithEffect )
 Constructor Create;
 Destructor Destroy; override;
protected
 FTexturedFont : TTexturedFont;
 FCaption : TS8;
 FWidth, FHeight: integer;
 function GetCaption: string;
 procedure SetCaption(AValue: string);
 function GetWidth: integer; override;
 function GetHeight: integer; override;
public
 procedure Draw( const aLayerPercentOpacity : single ) ; override;
public
 property Font: TTexturedFont read FTexturedFont write FTexturedFont;
 property Caption: string read GetCaption write SetCaption;
end;


{ TGuiProgressBar }
TGuiProgressBarMode = ( sbmHorizontal, sbmVertical );

TGuiProgressBar = class( TColorBackground )
  Constructor Create( aX, aY: single; aWidth, aHeight : integer );
  Destructor Destroy; override;
private
  FMode: TGuiProgressBarMode;
  FPercent: single;
public
 procedure Update( const aElapsedTime : single ) ; override;
  procedure Draw( const aLayerPercentOpacity: single ); override;
public
  BackGroundColor,
  BorderColor: TBGRAParam;
  property Mode: TGuiProgressBarMode read FMode write FMode;
  property Percent: single read FPercent write FPercent; // [0..1]
end;

{ TGlow }

TGlow = class( TSimpleSurfaceWithEffect )
 private
  class var FLocGlowCenter: GLint;
  class var FLocGlowColor: GLint;
  class var FLocGlowRadius: GLint;
  class var FLocGlowAttenuation: GLint;
  class var FLocInitDone: boolean;
protected
 FGlowAttenuation: single;
 procedure SetGlowAttenuation(AValue: single);
 function GetWidth: integer; override;
 function GetHeight: integer; override;
public
 procedure Draw( const aLayerPercentOpacity : single ) ; override;
public
 Radius    : single;
 Constructor Create( aX, aY, aRadius: single; aColor: TBGRAPixel; aBlendMode: byte=FX_BLEND_ADD ) ;
 property Attenuation: single read FGlowAttenuation write SetGlowAttenuation;
end;


{ TParticleEmitter }

TParticleEmitter = class( TSimpleSurfaceWithEffect )
 Constructor Create;
 Destructor Destroy ; override;
private
  function GetPPVelocity: single;
  procedure SetPPVelocity(AValue: single);
protected
 FParticles : array [0..MAX_PARTICLES-1] of TParticle;
 FCurrentLife : single;
 FParticleRate : single;
 FParticleRateCount : single;
 FParticleCount : integer;
 FParticleParam : TParticleParameter;
 FWidth, FHeight: integer;
 function GetWidth: integer; override;
 function GetHeight: integer; override;
 procedure DrawParticleTexture( aTexture: PTexture; AFrameIndex: integer; aTint: TBGRAPixel; aX, aY, aAngle, aZoom: single );
public
 procedure Update ( const aElapsedTime : single ) ; override;
 procedure Draw( const aLayerPercentOpacity : single ) ; override;
public
 ParticlesToEmit: TBoundedFParam;      // max particles count to emit at the same time [0..1024]
 Gravity        : TPointFParam;        // gravity in pixels per second
 Direction      : TBoundedFParam;      // [0..360] degree
 Spread         : TBoundedFParam;      // [0..360] degree
 InitialLife    : single;              // particle life in seconds
 LoopMode       : boolean;
 procedure LoadFromFile( const aFilename: string );
 procedure Shoot ;
 procedure Clear ;
 procedure SetSize( AWidth, AHeight: integer );
 // acces to particle parameters - not finished
 property PP_Velocity: single read GetPPVelocity write SetPPVelocity;

end;


{ TTileEngine }

// on map, tiles have a reference on a texture, indexes (ix,iy) for each tile on this texture
// and eventually an user value for trigger an event
TTile = record
         TextureIndex  : integer; // index of texture in TTilemap.FTextureList
         ixFrame       : integer; // frame x index on texture
         iyFrame       : integer; // frame y index
         UserEvent     : integer;
end;
PTile = ^TTile;

// map is an array of array of TTile
TArrayOfArrayOfTile = array of array of TTile;

TArrayOfArrayOfInteger = array of array of integer;
// each texture (or tileset) have an array value that allows to identify ground type for each tile
TTileTexInfo = record
     Texture  : PTexture;
     TileType : TArrayOfArrayOfInteger;  // each type by frame column and row  [x][y]
end;
PTileTexInfo = ^TTileTexInfo;
TArrayOfTileTexInfo = array of TTileTexInfo;


TTileEvent = procedure ( Sender: TTileEngine; const TileTopLeftCoor: TPointF; EventValue: integer ) of object;

TTileEngine = class( TSimpleSurfaceWithEffect )
 Constructor Create;
 Destructor Destroy ; override ;
private
 FTextureList      : TArrayOfTileTexInfo;
 FMap              : TArrayOfArrayOfTile; // [row,column]
 FMapSize          : TSize;     // total map size in pixel
 FMapTileCount     : TSize;
 FWidth, FHeight   : integer;   // screen surface size
 FTileSize         : TSize;
 FVScrollEnable,
 FHScrollEnable,
 FHLoopMode,
 FVLoopMode        : boolean;
 FColumnIndexFirstTile : integer;
 FRowIndexFirstTile : integer;
 FXOffsetFirstTile: single;
 FYOffsetFirstTile: single;
 FOnTileEvent : TTileEvent ; // callback for event
 function GetTileSize: TSize;
 function GetMapSize: TSize;
 procedure EnsurePositionInMap( var APos: TPointF );
 procedure ComputeIndexes( APosition: TPointF );
 procedure SetTileSize( AHSize, AVSize: integer );
 function IsValidRow( ARow: integer ): boolean; inline;
 function IsValidColumn( AColumn: integer ): boolean; inline;
 function IsValidTextureIndex( AITex: integer ): boolean;
 function IsValidFrameIndex( ATextureIndex, ixFrame, iyFrame: integer ): boolean;
 function TileEngineIndexFrameToTextureIndexFrame( iTexture, ixFrame, iyFrame: integer): integer;
 procedure Clear;
protected
 function GetWidth: integer; override;
 function GetHeight: integer; override;
public
 procedure Update( const aElapsedTime: single ); override;
 procedure Draw( const aLayerPercentOpacity: single ); override;
public
 // methods to construct map. Used by 'Tile Map Designer' utility
 TileMapDesignerModeEnable: boolean;
 function GetFirstTileColumnIndex: integer;
 function GetFirstTileRowIndex: integer;
 function AddTexture( AFilename: string; AFrameWidth, AFrameHeight: integer ): integer; // return index of created texture
 procedure DeleteTexture( AIndex: integer );
 function GetTexture( AIndex: integer ): PTexture;
 function GetTextureCount: integer;
 procedure SetMapTileCount( ARowCount, AColumnCount: integer );
 procedure InsertRow( ARowIndex, ACount: integer );
 procedure InsertColumn( AColumnIndex, ACount: integer );
 procedure ResetMap;
 function GetPTile( ARow, AColumn: integer ): PTile;
 function GetPTileTexInfo( ARow, AColumn: integer ): PTileTexInfo;
 procedure SetUserEventValue( ARow, AColumn, AUserEvent: integer );
 function GetUserEventValue( ARow, AColumn: integer ): integer; overload;
 procedure SetCell( ARow, AColumn, ATextureIndex, ixFrame, iyFrame: integer );
// procedure GetCellTextureAndFrameIndex( ARow, AColumn: integer; out TextureIndex, ixFrame, iyFrame: integer );
 procedure SetGroundType( ATextureIndex, ixFrame, iyFrame, AGroundType: integer );
 function GetGroundType( ATextureIndex, ixFrame, iyFrame: integer ): integer; overload;
 procedure ClearCell( ARow, AColumn: integer );
public
 // this is virtual position of the top left corner of the view
 // why virtual ? Because when HLoopMode or VLoopMode is set to TRUE, the map repeats itself indefinitely
 // for example, if your map is 100x100 pixels sized, when you set HLoopMode to TRUE, virtual position (15000,80) is valid
 // because map repeat itself on horizontal axis
 PositionOnMap : TPointFParam;

 // scrolling speed in pixel per seconds for both axis x y
 ScrollSpeed   : TPointFParam;

 // color for the 'holes' in the map
 MapHoleColor  : TBGRAParam;

 // give real position on map pointed by 'VirtualPositionOnMap'
 function GetBoundedPositionOnMap: TPointF;

 // load map previously created with 'Tile Map Designer' utility
 procedure LoadMapFile( const aFilename : string );

 // Give tile type at specified position on the scene (and not on map). Can be out of the view, but still on map
 function GetGroundType( aSceneCoor: TPointF ): integer; overload;

 // Give event's value at specified position on the scene
 function GetUserEventValue( aSceneCoor: TPointF ): integer; overload;

 // Return TRUE if aSceneCoor point is in map (can be out of view, but on map)
 // if MapPos is specified, at the end of the function, it contain Map position corresponding to aSceneCoor
 function SceneCoorIsInMap( aSceneCoor: TPointF;  MapPos: PPointF=NIL ): boolean;

 function SceneCoorToMapPosition( aSceneCoor: TPointF ): TPointF; inline;
 function MapPositionToSceneCoor( aMapPos: TPointF ): TPointF; inline;

 // set the size in pixels of the surface on which the map will be drawn in the scene. You can retrieve it with property Width and Height
 procedure SetViewSize( AWidth, AHeight: integer );

 property MapSize: TSize read GetMapSize; // give total map size in pixel

 property TileSize: TSize read GetTileSize;
 property MapTileCount: TSize read FMapTileCount; // cx = column count     cy = row count
 property HScrollEnable: boolean read FHScrollEnable write FHScrollEnable;
 property HLoopMode: boolean read FHLoopMode write FHLoopMode;
 property VScrollEnable: boolean read FVScrollEnable write FVScrollEnable;
 property VLoopMode: boolean read FVLoopMode write FVLoopMode;

 property OnTileEvent: TTileEvent read FOnTileEvent write FOnTileEvent;
end;


{ TGuiLabel }
{
TAlignment = ( alLeft , alCenter , alRight , alJustify ) ;
TXYWord = record
          x          : integer ;
          y          : integer ;
          width      : integer ;
          IndexLine  : integer ;
         end;

TGuiLabel = class ( TSprite )
  // Single line text
  Constructor Create ( aText: string; aX, aY : single; aFontHeight: integer;
                       aFontColor, aOutLineColor, aShadowColor:TBGRAPixel ;
                       aFontName:string='Arial'; aFontStyle : TFontStyles=[] ) ;
  // word wrapped text in rectangle
  Constructor Create ( aText: string; aX, aY, aWidth, aHeight, aFontHeight: integer;
                       aFontColor, aOutLineColor, aShadowColor:TBGRAPixel ;
                       aFontName:string='Arial'; aFontStyle : TFontStyles=[]; aAlignment : TAlignment=alCenter ) ;
  Destructor Destroy ; override ;
private
  FImaTampon    : TBGRABitmap ;
  FFontName     : string ;
  FFontStyle    : TFontStyles ;
  FWord         : TStringList ;
  ArrayXYWord       : array of TXYWord ;
  FHauteurTrace     : integer     ;
  FLargeurTrace     : integer     ;
  FDecalageOmbre    : integer     ;
  FHauteurFont      : integer     ;
  FViewableLine     : TStringList ;
  procedure SeparateTheWordsOfTheText ( aTxt : string ; temp : TStringList ; aSeparator: char ) ;
  procedure SimulateDrawing ;
  procedure LeftAlign ;
  procedure CenterAlign ;
  procedure RightAlign ;
  procedure JustifyAlign ;
  procedure VerticalAlign ;
  procedure FindWordIndex ( aLine : integer ; var iStart , iEnd : integer ) ;
  procedure DrawTextOn(aImage: TBGRABitmap; aFontColor:TBGRAPixel; aOutLineColor:TBGRAPixel; aShadowColor:TBGRAPixel ) ;
public
  // Internal procedure wrapper
  procedure DoUpdate ( const aElapsedTime : double ) ; override ;
end;
}

