unit pb_clock;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils,Graphics,Controls,ExtCtrls,
  dateutils, BGRABitmap,BGRABitmapTypes,BGRAGradientScanner,math,strutils;

type

TNumericMode = (nmDecimal,nmRoman,nmCatholic);

TRelogio = class(TGraphicControl)
Const
  barriga = 18;
  finuria = 3;
private
  timer1:TTimer;
  fGMT : Real;
  fShowShadow,FContinuous,Fglass:boolean;
  fGMTAdjust:Boolean;
  BMP,vidro:TBGRABitmap;
  LastTime:Integer;
  fGlassColorFront,
  fGlassColorBack,
  FClockColorFront,
  FClockColorBack:TBGRAPixel;
  fNumericMode:TNumericMode;
  fRoundPercent,
  fMarginalPercent,
  fNumeralPercent,
  fmarcadores,
  fespacadores:byte;

public
  constructor Create(Aowner:TComponent); override;
  destructor Destroy; override;
  procedure Timer1Timer(Sender: TObject);
  procedure Paint; override;
  procedure Reset;
published
  property NumericMode     :TNumericMode read fNumericMode     write fNumericMode;
  property ShowShadow      :boolean      read fShowShadow      write fShowShadow;
  property ShowContinuous  :boolean      read fContinuous      write fContinuous;
  property ShowGlass       :boolean      read Fglass           write Fglass;
  property GMTAdjust       :boolean      read fGMTAdjust       write fGMTAdjust;
  property GMT             :Real         read fGMT             write fGMT;
  property RoundPercent    :byte         read fRoundPercent    write fRoundPercent;
  property BorderPercent   :byte         read fMarginalPercent write fMarginalPercent;
  property DisplayPercent  :byte         read fNumeralPercent  write fNumeralPercent;
  property MarkPercent     :byte         read fmarcadores      write fmarcadores;
  property SeparatorPercent:byte         read fespacadores     write fespacadores;
end;

implementation
Destructor TRelogio.Destroy;
begin
  timer1.free;
  BMP.free;
  inherited Destroy;
end;
Constructor TRelogio.Create(Aowner:TComponent);
var
  t:Tdatetime;
begin
  inherited create(Aowner);
  fRoundPercent:=100;
  fMarginalPercent:=5;
  fNumeralPercent:=10;
  fmarcadores:=8;
  fespacadores:=2;
  fClockColorFront:=ColorToBGRA(RGBToColor(064,064,128),255);
  fClockColorBack :=ColorToBGRA(RGBToColor(032,032,064),255);
  fGlassColorFront:=ColorToBGRA(RGBToColor(255,255,128),128);
  fGlassColorBack :=ColorToBGRA(RGBToColor(128,128,128),064);
  fNumericMode    :=nmRoman;
  lasttime:=0;
  fShowShadow:=true;
  FContinuous:=false;
  timer1:=TTimer.Create(self);
  timer1.Interval:=100;
  timer1.OnTimer:=@timer1timer;
  timer1.Enabled:=true;
  t:=now;
  fgmt:=round((LocalTimeToUniversal(t)-t)*2400)/100;
  fGMTAdjust:=true;
  fglass:=true;
  BMP:=nil;
end;
procedure TRelogio.Reset;
begin
  BMP.free;
  vidro.free;
  bmp:=nil;
  vidro:=nil;
end;

procedure TRelogio.Paint;
var
  ClockTime:Tdatetime;
  BGRA:TBGRABitmap;
  Centerpoint,Shadow:TPoint;
  oitavax,oitavay:Integer;
  HRect,MRect,SRect,HShad,MShad,SShad:TRect;
  horas,minutos,segundos:Valreal;
  procedure PaintToCanvas(Condition:Boolean;Central:Tpoint;cor,borda:TBGRAPixel;aRect:TRect;RadAngle,Imperative1,imperative2:ValReal);
  var
    Extreme,extreme10,extreme11,extreme20,extreme21:TPointF;
  begin
    if condition then begin
      Extreme:=PointF(
        round(central.x+(Central.X-aRect.left)*cos(RadAngle)),
        round(central.Y+(Central.Y-aRect.top)*sin(RadAngle))
      );
      Extreme21:=PointF(
        round(central.x+(Central.X-aRect.left)*cos(RadAngle-imperative2)/finuria),
        round(central.Y+(Central.Y-aRect.top )*sin(RadAngle-imperative2)/finuria));
      Extreme20:=PointF(
        round(central.x+(Central.X-aRect.left)*cos(RadAngle-Imperative1)/barriga),
        round(central.Y+(Central.Y-aRect.top )*sin(RadAngle-Imperative1)/barriga));
      Extreme10:=PointF(
        round(central.x+(Central.X-aRect.left)*cos(RadAngle+Imperative1)/barriga),
        round(central.Y+(Central.Y-aRect.top )*sin(RadAngle+Imperative1)/barriga));
      Extreme11:=PointF(
        round(central.x+(Central.X-aRect.left)*cos(RadAngle+imperative2)/finuria),
        round(central.Y+(Central.Y-aRect.top )*sin(RadAngle+imperative2)/finuria));
      BGRA.CanvasBGRA.Pen.BGRAColor:=Borda;
      BGRA.CanvasBGRA.Brush.BGRAColor:=Cor;
      BGRA.CanvasBGRA.PolygonF([extreme,extreme11,extreme10,extreme20,extreme21]);
    end;
  end;
  function GetGlass(sizeX,sizeY:Integer):TBGRABitmap;
  var
    scanner: TBGRAGradientScanner;
  begin
    result:=TBGRABitmap.create(sizeX,sizeY,ColorToBGRA(clwhite,0));

    scanner := TBGRAGradientScanner.Create(
      fGlassColorFront,
      fGlassColorBack,
      gtReflected,
      PointF(0,0),
      PointF(sizex,sizey));

    Result.FillRoundRectAntialias(
      sizeX * fMarginalPercent div 200 -1,
      sizeY * fMarginalPercent div 200 -1,
      sizex - sizeX * fMarginalPercent div 200 -1,
      sizey - sizeY * fMarginalPercent div 200 -1,
      sizeX * (froundPercent-fMarginalPercent*fRoundpercent div 100) div 200,
      sizeY * (fRoundPercent-fMarginalPercent*fRoundpercent div 100) div 200,
      scanner);

    scanner.Free;

  end;
  function GetClock(sizeX,sizeY:Integer):TBGRABitmap;
  var
    numeral:byte;
    NumeralCenter,NumeralExtreme,Centerpoint:TPoint;
    Angle:ValReal;
    scanner : TBGRAGradientScanner;
    numeralstr:String;
  begin
    result:=TBGRABitmap.create(sizeX,sizeY,ColorToBGRA(clSilver ,255));
    Centerpoint:=Point(Sizex div 2,sizeY div 2);
    scanner := TBGRAGradientScanner.Create(
      fClockColorFront,
      fClockColorBack,
      gtReflected,
      PointF(0,0),
      PointF(sizex,sizey));
    Result.FillRoundRectAntialias(
      1,
      1,
      sizex -1,
      sizey -1,
      sizeX * froundPercent div 200,
      sizeY * fRoundPercent div 200 ,
      scanner);

    scanner.Free;


    Result.CanvasBGRA.Pen.BGRAColor:=fClockColorBack;
    Result.CanvasBGRA.Brush.BGRAColor:=ColorToBGRA(clwhite,255);

    Result.CanvasBGRA.RoundRect(
      sizeX * fMarginalPercent div 200 ,
      sizeY * fMarginalPercent div 200 ,
      sizex - sizeX * fMarginalPercent div 200 ,
      sizey - sizeY * fMarginalPercent div 200 ,
      sizeX * (froundPercent-fMarginalPercent*fRoundpercent div 100) div 100,
      sizeY * (fRoundPercent-fMarginalPercent*fRoundpercent div 100) div 100);
    result.CanvasBGRA.pen.BGRAColor:=ColorToBGRA(clblack,255);
    result.CanvasBGRA.pen.Width:=3;
    for numeral:=0 to 59 do begin
      if numeral mod 5 = 0 then
        result.CanvasBGRA.Pen.Width:=sizeY*fNumeralPercent div 1000
      else
        result.CanvasBGRA.Pen.Width:=sizeY*fNumeralPercent div 3000;
      if result.CanvasBGRA.Pen.Width<1 then
        result.CanvasBGRA.Pen.Width:=1;
      Angle:=(45+Numeral )*pi/30;
      NumeralCenter:=Point(
        round(Centerpoint.x+(sizeX * (100-(fMarginalPercent+fespacadores*2)) / 200 )*cos(Angle)),
        round(Centerpoint.Y+(sizeY * (100-(fMarginalPercent+fespacadores*2)) / 200 )*sin(Angle))
      );
      NumeralExtreme:=Point(
        round(Centerpoint.x+(sizeX * (100-(fMarginalPercent+fMarcadores+fespacadores*2)) / 200 )*cos(Angle)),
        round(Centerpoint.Y+(sizeY * (100-(fMarginalPercent+fmarcadores+fespacadores*2)) / 200 )*sin(Angle))
      );
      result.CanvasBGRA.MoveTo(NumeralCenter);
      result.CanvasBGRA.LineTo(NumeralExtreme);
    end;

    result.CanvasBGRA.Pen.BGRAColor:=FClockColorFront;
    result.CanvasBGRA.Brush.BGRAColor:=ColorToBGRA(clblue,0);
    result.CanvasBGRA.Font.Style:=[fsBold];
    for numeral:=1 to 12 do begin
      case fNumericMode of
        nmDecimal: numeralstr:=inttostr(numeral);
        nmRoman: numeralstr:=ifthen(numeral=4,'IIII',IntToRoman(numeral));
      else
        numeralstr:=IntToRoman(numeral);
      end;
      if numeral mod 3 = 0 then
        result.CanvasBGRA.Font.Height:=-sizeY*fNumeralPercent div ifthen(fnumericmode=nmdecimal,100,160) +1
      else
        result.CanvasBGRA.Font.Height:=-sizeY*fNumeralPercent div ifthen(fnumericmode=nmdecimal,200,320) +1;
      if result.CanvasBGRA.Font.Height>-1 then result.CanvasBGRA.Font.Height:=-1;
      Angle:=(45+Numeral*5 )*pi/30;
      NumeralCenter:=Point(
        round(Centerpoint.x+(sizeX * (100-(fMarginalPercent+fNumeralPercent+fmarcadores+fespacadores*2)) / 200 )*cos(Angle)),
        round(Centerpoint.Y+(sizeY * (100-(fMarginalPercent+fNumeralPercent+fmarcadores+fespacadores*2)) / 200 )*sin(Angle)));
      result.CanvasBGRA.TextOut(
        NumeralCenter.x-result.CanvasBGRA.TextWidth(numeralstr) div 2,
        NumeralCenter.y-result.CanvasBGRA.TextHeight(numeralstr) div 2,
        numeralStr
        );
    end;

  end;

begin
  BGRA:=TBGRABitmap.Create(width,Height,BGRAPixelTransparent);
  BGRA.CanvasBGRA.AntialiasingMode:=amOn;
  oitavax:=ClientWidth  div 32;
  oitavay:=ClientHeight div 32;
  if (BMP=nil) or (bmp.Width<>ClientWidth) or (bmp.Height<>ClientHeight) then begin
    if not(BMP=nil) then begin
      bmp.free;
      vidro.free;
    end;
    bmp:=GetClock(Width,Height);
    vidro:=GetGlass(Width,Height);
  end;
  BGRA.PutImage(0,0,bmp,dmDrawWithTransparency);

  HRect:=Rect(oitavax*8,oitavay*8,ClientWidth-oitavax*8,ClientHeight-oitavay*8);
  MRect:=Rect(oitavax*5,oitavay*5,ClientWidth-oitavax*5,ClientHeight-oitavay*5);
  SRect:=Rect(oitavax*4,oitavay*4,ClientWidth-oitavax*4,ClientHeight-oitavay*4);

  BGRA.CanvasBGRA.Pen.Width:=1;

  if fGMTAdjust then begin
    ClockTime:=LocalTimeToUniversal(now)-fGMT/24;
  end else
    ClockTime:=now;
  horas   :=frac(ClockTime)*24;
  if horas>12 then horas:=horas-12;
  minutos :=frac(ClockTime*24)*60;
  segundos:=frac(ClockTime*24*60)*60;
  if not FContinuous then
    segundos:=round(segundos);
  HShad:=Rect(Hrect.Left+6,HRect.Top+6,HRect.Right+6,HRect.Bottom+6);
  MShad:=Rect(Mrect.Left+8,MRect.Top+8,MRect.Right+8,MRect.Bottom+8);
  SShad:=Rect(Srect.Left+10,SRect.Top+10,SRect.Right+10,SRect.Bottom+10);

  Shadow:=Point(Width div 2 +4, Height div 2 +4);
  Centerpoint:=Point(Width div 2, Height div 2);
  PaintToCanvas(fShowShadow,Shadow,ColorToBGRA(clBlack ,064),ColorToBGRA(clBlack  ,064),HShad,(45+horas*5 )*pi/30,pi-0.1,0.2);
  PaintToCanvas(true       ,Centerpoint,ColorToBGRA(clBlue  ,255),ColorToBGRA(clNavy  ,255),HRect,(45+horas*5 )*pi/30,pi-0.1,0.2);
  PaintToCanvas(fShowShadow,Shadow,ColorToBGRA(clBlack ,064),ColorToBGRA(clBlack  ,064),MShad,(45+minutos )*pi/30,pi-0.1,0.1);
  PaintToCanvas(true       ,Centerpoint,ColorToBGRA(clBlue  ,255),ColorToBGRA(clNavy  ,255),Mrect,(45+minutos )*pi/30,pi-0.1,0.1);
  PaintToCanvas(fShowShadow,Shadow,ColorToBGRA(clBlack ,064),ColorToBGRA(clBlack  ,064),SShad,(45+segundos)*pi/30,pi-0.1,0.05);
  PaintToCanvas(true       ,Centerpoint,ColorToBGRA(clRed   ,255),ColorToBGRA(clMaroon,255),SRect,(45+segundos)*pi/30,pi-0.1,0.05);

  BGRA.CanvasBGRA.Ellipse(Centerpoint.x-oitavax div 2,Centerpoint.Y-oitavay div 2,
                 Centerpoint.x+oitavax div 2,Centerpoint.Y+oitavay div 2);
  BGRA.CanvasBGRA.Brush.Color:=clMaroon;
  BGRA.CanvasBGRA.Brush.Color:=clred;
  BGRA.CanvasBGRA.Ellipse(Centerpoint.X+oitavax div 3,Centerpoint.Y+oitavay div 3,
                 Centerpoint.X-oitavax div 3,Centerpoint.Y-oitavay div 3);
  if Fglass then
    BGRA.PutImage(0,0,vidro,dmLinearBlend);
  BGRA.Draw(Canvas,rect(0,0,width,Height),true);
  BGRA.Free;
end;
procedure TRelogio.Timer1Timer(Sender: TObject);
var
  newtime:LongInt;
begin
  Newtime:=ifthen(fcontinuous,trunc(frac(now)*864000),trunc(frac(now)*86400));
  if (LastTime<>newtime) then begin
    LastTime:=newtime;
    self.Invalidate;
  end;
end;

end.

