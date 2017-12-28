unit principal;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, OpenGLContext, Forms, Controls, Graphics,
  Dialogs, LMessages, LCLIntf, LCLType, ExtCtrls,
  OGLCScene,
  BGRABitmapTypes, BGRABitmap, //SvgToBGRABitmap,
  common,
  audio, sprites,
  ChooseCountry,
  Introduction,
  GameUnit;

type

  { TForm_Principal }

  TForm_Principal = class(TForm)
    OpenGLControl1: TOpenGLControl;
    Timer1: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormShow(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    procedure MyMessageHandler(var Message: TLMessage); message LM_MESSAGE_PRINCIPAL;
    procedure LoadResources;
  public
  end;

var
  Form_Principal: TForm_Principal;


implementation

{$R *.lfm}

{ TForm_Principal }

procedure TForm_Principal.FormCreate(Sender: TObject);
begin
 randomize;
// Application.ExtendedKeysSupport := TRUE; // enabled left and right SHIFT and CONTROL key

 FScene := TOGLCScene.Create( OpenGLControl1 );
 FScene.LayerCount:=NBLayer;

 RoadTileEngine := TTileEngine.Create;
 FScene.Layer[LayerRoad].AssignTileEngine( RoadTileEngine, FALSE );

 CountryChoice := TCountryChoice.Create;
 Intro := TIntroduction.Create;
 Game := TGame.Create;

 TextDebug := '';
end;

procedure TForm_Principal.FormDestroy(Sender: TObject);
begin
 FreeAndNil( RoadTileEngine );
 FreeAndNil( FScene );
 FreeAndNil( CountryChoice );
 FreeAndNil( Intro );
 FreeAndNil( Game );
end;

procedure TForm_Principal.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
 FScene.ProcessKeyDown( Key, Shift );
end;

procedure TForm_Principal.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
 FScene.ProcessKeyUp( Key, Shift );
end;

procedure TForm_Principal.FormShow(Sender: TObject);
begin
 Timer1.Enabled:=TRUE;
 PostMessage( Handle, LM_MESSAGE_PRINCIPAL, Mess_StartScene, 0 );
 PostMessage( Handle, LM_MESSAGE_PRINCIPAL, Mess_LoadResources, 0 );
end;

procedure TForm_Principal.Timer1Timer(Sender: TObject);
var p: TPointF;
begin
 p := RoadTileEngine.PositionOnMap.Value;//  GetBoundedPositionOnMap;
 Caption := inttostr(FScene.FPS)+' Fps  -  '+
              inttostr(FScene.SurfaceCount)+' sprites'+
              '  tileenginepos='+inttostr(round(p.x))+','+inttostr(round(p.y))+ '     '+
              TextDebug;
end;

procedure TForm_Principal.MyMessageHandler(var Message: TLMessage);
begin
 case Message.wParam of
  Mess_StartScene : begin
                      FScene.Start;// Enter the Scene main loop and exit only when FScene.Stop is called.
                      Timer1.Enabled := FALSE; // this line is executed only when FScene.Stop is called
                      Close;
  end;
  Mess_LoadResources : LoadResources;
  Mess_Quit : begin
                      FScene.ColorFadeIn(BGRABlack , 0.5 );
                      FScene.ExecuteDuring(0.5);
                      FScene.Stop;
  end;
 end;//case
end;

// here we load common ressources uses by the game
procedure TForm_Principal.LoadResources;
begin

 LoadTextures;

 LoadSounds;

 // Font
 TableFont := FontManager.AddFont('Arial Black', 30, [], BGRA(150,150,125), BGRA(255,255,255), 1, BGRA(0,0,0,0), 0, 0, 0);
 SpriteTableFont := FontManager.AddFont('Arial Black', 40, [], BGRA(255,255,25), BGRA(0,0,0), 2, BGRA(0,0,0), 2, 2, 5 );
 FontCountry := FontManager.AddFont('Arial Black', 60, [], BGRA(255,255,25), BGRA(30,0,0), 3, BGRA(0,255,0), 5, 5, 10 );
 FontMenu := FontManager.AddFont('Arial Black', 40, [], BGRA(0,218,255), BGRA(50,0,0), 5, BGRA(0,0,150), 5, 5, 10 );
 FontScore := FontManager.AddFont('Arial Black', 30, [], BGRA(255,255,255), BGRA(1,1,1), 3, BGRA(0,0,0,0), 0, 0, 0);
 FontInstructions := FontManager.AddFont('Arial Black', 18, [], BGRA(255,65,180), BGRA(126,32,89), 2, BGRA(0,0,0,180), 4, 5, 2);

 TexFullSpeedText := NIL;

 FScene.LaunchStage( CountryChoice );
end;



end.

