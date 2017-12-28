unit ChooseCountry;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, LCLIntf,
  LazUTF8,
  BGRABitmapTypes, BGRABitmap,
  OGLCScene,
  variable_parameter_V2,
  common,
  Language;


type


{ TCountryChoice }

TCountryChoice = class (TStageSkeleton)
private
   w:TWaterSurface;
  procedure FadeInScene( ASurface: TSimpleSurfaceWithEffect; ALayer: integer);
public
  procedure LoadData; override;
  procedure FreeData; override;
  procedure Update( AElapsedTime: single ); override;
  procedure ProcessGUIEvent(aGUISurface: TSimpleSurfaceWithEffect);
end;

var
 CountryChoice : TCountryChoice ;

implementation
uses introduction;

procedure TCountryChoice.LoadData;
var b : TGuiButton;
    LangCount : integer;
    yspace : single;

    procedure InitButton( aB: TGuiButton; yCoor: single );
    begin
     aB.SetCenterCoordinate( FScene.Width/2, yCoor );
     aB.OnClick := @ProcessGUIEvent;
     FadeInScene( aB, LayerIntro );
    end;

begin
 FScene.Layer[LayerRoad].Visible := FALSE;

 LangCount := 5 ;

 yspace := FScene.Height/(LangCount+1);
 w:=TWaterSurface.Create;
 w.SetSize( 1024, 768 );
 w.SetCenterCoordinate(FScene.Width/2, FScene.Height/2);
 FScene.Add( w, LayerIntro );
 w.RainAmount.ChangeTo( 10, 20, idcStartFastEndSlow );

 w.WaterIsTransparent:=TRUE;
 w.WaterColor.ChangeTo(BGRA(255,255,255), 1 );


 b := TGuiButton.Create(0, 0, UTF8ToSys('Français'), FontCountry ) ;
 InitButton( b, yspace );
 b.Group := GroupButtonFrench ;

 b := TGuiButton.Create(0, 0, 'English', FontCountry ) ;
 InitButton( b, yspace*2 );
 b.Group := GroupButtonEnglish ;

 b := TGuiButton.Create(0, 0, 'Italiano', FontCountry ) ;
 InitButton( b, yspace*3 );
 b.Group := GroupButtonItalian ;

 b := TGuiButton.Create(0, 0, 'Dutch', FontCountry ) ;
 InitButton( b, yspace*4 );
 b.Group := GroupButtonGermany ;

 b := TGuiButton.Create(0, 0, UTF8ToSys('Español'), FontCountry ) ;
 InitButton( b, yspace*5 );
 b.Group := GroupButtonSpanish ;

end;

procedure TCountryChoice.FreeData;
var i: integer;
begin
 for i:=0 to NBLayer-1 do FScene.Layer[i].Clear;
end;

procedure TCountryChoice.Update(AElapsedTime: single);
begin
 // do nothing here
end;

procedure TCountryChoice.ProcessGUIEvent(aGUISurface: TSimpleSurfaceWithEffect);
var ima: TBGRABitmap;
begin
 case aGUISurface.Group of
  GroupButtonFrench  : CurrentLanguage := 0 ;
  GroupButtonEnglish : CurrentLanguage := 1 ;
  GroupButtonItalian : CurrentLanguage := 2 ;
  GroupButtonGermany : CurrentLanguage := 3 ;
  GroupButtonSpanish : CurrentLanguage := 4 ;
 end;//case

 // Texture for message 'Full speed activated'
 if TexFullSpeedText <> NIL then TextureManager.Delete( TexFullSpeedText );
 ima := Text2Bitmap( Message[3,CurrentLanguage], 'Arial Black', 40, [], BGRA(255,255,25), BGRA(30,0,0), 3, BGRA(0,255,0), 5, 5, 7 );
 TexFullSpeedText := TextureManager.Add( ima );
 ima.Free;

 FScene.LaunchStage( Intro );
end;

procedure TCountryChoice.FadeInScene(ASurface: TSimpleSurfaceWithEffect; ALayer: integer);
begin
 ASurface.Opacity.Value := 0;
 Asurface.Opacity.ChangeTo( 255, 1 );
 FScene.Add( ASurface, ALayer );
end;


end.

