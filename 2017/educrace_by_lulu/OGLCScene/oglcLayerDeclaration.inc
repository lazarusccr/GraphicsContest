
type

TSimpleSurface = class;
TSimpleSurfaceWithEffect = class;
TSprite = class;
TTileEngine = class;

//TCallBackNotification = procedure ( Sender : TSimpleSurfaceWithEffect ) of object;

TBlink = record
          BlinkToGo                    : integer ;
          VisibleTime                  : single  ;
          InvisibleTime                : single  ;
          CountVisible                 : single  ;
          CountInvisible               : single  ;
         end;

TRotationAroundPoint = record
                       InProgress       : boolean ;
                       XPt              : single ;
                       YPt              : single ;
                       AnglePerSecond   : single ;
                       ActualAngleRad   : single ;
                       Radius           : single ;
                      end;

TBindToSprite = record
                 InProgress       : boolean       ;
                 ParentSurface    : TSimpleSurface ;
                 OffsetX          : single        ;
                 OffsetY          : single        ;
                end;

TLayer = class;

{ TSurfaceList }

TSurfaceList = class
   Constructor Create ;
   Destructor Destroy ; override ;
   protected
     FList : TList ;
     function GetSurfaceCount: integer; inline;
     function GetSurface(aIndex: integer): TSimpleSurfaceWithEffect ; inline;
     procedure Delete ( Index:integer );
   public
     procedure Clear; virtual;
     function Add ( aSurface : TSimpleSurfaceWithEffect ) : integer ; virtual ;
     procedure Insert ( aIndex : integer ; aSurface : TSimpleSurfaceWithEffect ) ; virtual ;
     property SurfaceCount : integer read GetSurfaceCount ;
     property Surface[aIndex:integer]:TSimpleSurfaceWithEffect read GetSurface ; default;
end;

{ TLayer }

TLayer = class (TSurfaceList)
   private
    FParentScene : TOGLCScene ;
    FVisible : boolean ;
    FFreeze  : boolean ;
    // Blink
    FBlink : TBlink ;
    // Tile engine
    FTileEngine : TTileEngine ;
    FTileEngineAutoFree : boolean ;
   public
    Constructor Create;
    Destructor Destroy; override;
    procedure Draw;
    procedure Update( const aElapsedTime : single ) ;
    procedure Clear; override;
   public
    // Tile engine
    procedure AssignTileEngine( aTileEngine: TTileEngine; AutoFree: boolean=TRUE );
   public
    Angle: TFParam;
    Opacity: TBoundedFParam;

    procedure KillAll;

    // Blink
    procedure Blink ( aNumberOfBlink : integer ; aVisibleTime , aInvisibleTime : single ) ; // aNumberOfBlink=-1 for infinite blink

// Methods below affect the surfaces of the layer
//    if  aGroup = -1  methods affect all surfaces of the layer.
//    if  aGroup > -1  methods affect only the surfaces with specified group number
// Note: every created surface have group property set to zero by default.
    procedure GroupKill ( aGroup: integer) ;
    procedure GroupVisible ( aGroup: integer; aValue:boolean ) ;
    procedure GroupFreeze (aGroup: integer ; aValue:boolean ) ;
    procedure GroupOpacity ( aGroup: integer; aNewOpacity:byte );
    procedure GroupOpacityVariation ( aGroup: integer; aNewOpacity: byte; aDuration: single; aCurve: Word=idcLinear ) ;
    procedure GroupTint ( aGroup : integer; aTint : TBGRAPixel );
    procedure GroupTintOpacityVariation( aGroup : integer; aNewOpacity : byte ; aDuration: single; aCurve: Word=idcLinear);
    procedure GroupMoveTo ( aGroup : integer; aX , aY : single ; aDuration : single ; aCurve:Word = idcLinear ) ;
    procedure GroupMoveRelative ( aGroup : integer; aDeltaX , aDeltaY : single ; aDuration : single ; aCurve:Word = idcLinear ) ;
    procedure GroupBlink ( aGroup : integer; aNumberOfBlink : integer ; aVisibleTime , aInvisibleTime : single ) ; // aNumberOfBlink=-1 for infinite blink
    procedure GroupScale( aGroup : integer; aHScale, aVScale : single ; aDuration : single ; aHScaleCurve:Word=idcLinear; aVScaleCurve : Word=idcLinear ) ; // aHScale, aVScale = -1 for no change
    procedure GroupRotate ( aGroup : integer; aAnglePerSecond: single) ;
    procedure GroupFlipH ( aGroup : integer; aValue:boolean );
    procedure GroupFlipV ( aGroup : integer; aValue:boolean );

    // Collision test: return surface collided, NIL if no collision
    function CollisionTest( aSurface: TSimpleSurfaceWithEffect ) : TSimpleSurfaceWithEffect; overload;
    function CollisionTest( aSurface: TSimpleSurfaceWithEffect; HTolerance, VTolerance: single ): TSimpleSurfaceWithEffect; overload;
    function CollisionTest( aX, aY, aWidth, aHeight: single) : TSimpleSurfaceWithEffect; overload;
    function CollisionTest( aX, aY, aWidth, aHeight: single; DontTestSurface: TSimpleSurfaceWithEffect) : TSimpleSurfaceWithEffect; overload;

    // layer's property
    property Freeze : boolean read FFreeze write FFreeze ;    // Freeze all moves and variations of the layer and every surfaces of the layer
    property Visible : boolean read FVisible write FVisible;
end;


TSurfaceChangeLayer= record
                 Surface: TSimpleSurfaceWithEffect;
                 NewLayerIndex: integer;
end;
TArrayOfSurfaceChangeLayer = array of TSurfaceChangeLayer;

{ TLayerList }

TLayerList = class
   Constructor Create;
   Destructor Destroy; override;
   private
     FList : TList;
     function GetLayerCount: integer;
     function GetLayer(Index: integer): TLayer;
     function GetSurfaceCount: integer;
     procedure SetLayerCount(AValue: integer ); virtual ;
     procedure AddSurfaceToLayer ( aSurface: TSimpleSurfaceWithEffect; aLayerIndex: integer=0 );
     procedure InsertSurfaceToLayer ( aSurfaceIndex: integer; aSurface: TSimpleSurfaceWithEffect; aLayerIndex: integer=0 );
   private
     FListSurfaceChangeLayer: TArrayOfSurfaceChangeLayer;
   public
    procedure ClearAllLayer; // kill all surfaces in all layer
    procedure ClearLayer( aLayerIndex:integer ); // kill all surfaces in one layer


    procedure SurfaceChangeLayer( aSurface: TSimpleSurfaceWithEffect; aNewLayerIndex: integer );

    procedure AllLayerVisible;
    procedure OnlyThisLayerVisible ( tab:array of integer );

    property LayerCount:integer read GetLayerCount write SetLayerCount;
    property Layer[Index:integer]:TLayer read GetLayer;
    property SurfaceCount:integer read GetSurfaceCount;
end;

TProcedureWrapper = procedure of object;
TProcedureDrawWrapper = procedure ( const aLayerPercentOpacity:single ) of object ;
TProcedureUpdateWrapper = procedure ( const DT:single ) of object ;
TProcedureDestroyWrapper = procedure of object;



