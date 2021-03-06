unit umain;

{
  movingdots by lainz (github.com/lainz)
  For the third lazarus graphics contest 2017
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  FGL, Math, BGRABitmap, BGRABitmapTypes;

const
  MAXDOTS = 10000;

type

  { TDot }

  TDot = class
  private
    FColor: TBGRAPixel;
    Fspeed: byte;
    Fx: integer;
    Fy: integer;
  public
    procedure MoveTo(Offset, Min, Max: TPoint);
    procedure Randomize(w, h: integer);
    property color: TBGRAPixel read FColor write FColor;
  published
    property speed: byte read Fspeed write Fspeed;
    property x: integer read Fx write Fx;
    property y: integer read Fy write Fy;
  end;

  TDotList = specialize TFPGObjectList<TDot>;

  { TForm1 }

  TForm1 = class(TForm)
    PaintBox1: TPaintBox;
    Timer1: TTimer;
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PaintBox1Click(Sender: TObject);
    procedure PaintBox1Paint(Sender: TObject);
    procedure PaintBox1Resize(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
    Dots: TDotList;
    elapsed: QWord;
    nextLap: QWord;
    PositionX: byte;
    Abitmap: TBGRABitmap;
    procedure InitializeDots(Clicked: boolean = False);
    procedure MoveDots(Offset, Min, Max: TPoint);
    procedure DrawDots(Bitmap: TBGRABitmap);
  public
    { public declarations }

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TDot }

procedure TDot.MoveTo(Offset, Min, Max: TPoint);
begin
  if (x = Min.x) or (x = Max.x) or (y = Min.y) or (y = Max.y) then
  begin
    Randomize(Max.x, Max.y);
    exit;
  end;

  x := x + Round(Offset.x * speed / RandomRange(speed, 255));

  if x < Min.x then
    x := Min.x;

  if x > Max.x then
    x := Max.x;

  y := y + Round(Offset.y * speed / RandomRange(speed, 255));

  if y < Min.y then
    x := Min.y;

  if y > Max.y then
    y := Max.y;
end;

procedure TDot.Randomize(w, h: integer);
begin
  x := RandomRange(0, w);
  y := RandomRange(0, h);
  speed := RandomRange(1, 256);
end;

{ TForm1 }

procedure TForm1.PaintBox1Paint(Sender: TObject);

  function Offset(): TPoint;
  begin
    case PositionX of
      0: Result := Point(RandomRange(-10, -5), RandomRange(-10, -5));
      1: Result := Point(RandomRange(-10, -5), RandomRange(5, 10));
      2: Result := Point(RandomRange(5, 10), RandomRange(-10, -5));
      3: Result := Point(RandomRange(5, 10), RandomRange(5, 10));
    end;
  end;

var
  tmp: TBGRABitmap;
begin
  BGRAReplace(ABitmap, ABitmap.FilterBlurRadial(5, 5, rbFast));
  ABitmap.Rectangle(0, 0, Width, Height, BGRAPixelTransparent, BGRA(0, 0, 0, RandomRange(5, 10)),
    dmDrawWithTransparency);
  MoveDots(Offset(), Point(0, 0), Point(Width - 1, Height - 1));
  DrawDots(Abitmap);
  Abitmap.Draw(PaintBox1.Canvas, 0, 0);
end;

procedure TForm1.PaintBox1Resize(Sender: TObject);
begin
  if ABitmap <> nil then
    ABitmap.Free;
  Abitmap := TBGRABitmap.Create(Width, Height, BGRABlack);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  Invalidate;
  Inc(Elapsed);

  if Elapsed >= nextLap then
  begin
    nextLap := nextLap + 100;
    PositionX := RandomRange(0, 4);
  end;
end;

procedure TForm1.InitializeDots(Clicked: boolean);
var
  i: integer;
  d: TDot;
begin
  if Clicked then
  begin
    for d in Dots do
      d.Randomize(Width, Height);
  end
  else
  begin
    Dots := TDotList.Create();
    for i := 0 to MAXDOTS do
    begin
      Dots.Add(TDot.Create);
      Dots[i].Randomize(Width, Height);
      Dots[i].color := BGRA(255, 255, 255, RandomRange(10, 70));
    end;
  end;
end;

procedure TForm1.MoveDots(Offset, Min, Max: TPoint);
var
  d: TDot;
begin
  for d in Dots do
    d.MoveTo(Offset, Min, Max);
end;

procedure TForm1.DrawDots(Bitmap: TBGRABitmap);
var
  d: TDot;
begin
  for d in Dots do
    Bitmap.DrawPixel(d.x, d.y, d.color);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  InitializeDots;
  nextLap := 0;
  elapsed := 0;
end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  Timer1.Enabled := False;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  Dots.Free;
  Abitmap.Free;
end;

procedure TForm1.PaintBox1Click(Sender: TObject);
begin
  InitializeDots(True);
end;

end.

