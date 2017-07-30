unit Unit1; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  mmsystem, lresources, StdCtrls, windows;

type

  { TForm1 }

  TForm1 = class(TForm)
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    Image4: TImage;
    Image5: TImage;
    Image6: TImage;
    Image7: TImage;
    Timer1: TTimer;
    Timer2: TTimer;
    Timer3: TTimer;
    Timer4: TTimer;
    Timer5: TTimer;
    Timer6: TTimer;
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure Timer1Timer(Sender: TObject);
    procedure Timer2Timer(Sender: TObject);
    procedure Timer3Timer(Sender: TObject);
    procedure Timer4Timer(Sender: TObject);
    procedure Timer5Timer(Sender: TObject);
    procedure Timer6Timer(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  Form1: TForm1;
  s1, archivo, archivo2, archivo3: string;
  indice, indice2, indice3: integer;
  t1: boolean;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  if image1.Left<-824 then image1.Left:=image1.Left+form1.Width +822
                   else image1.Left:=image1.Left-1;

  //image1.Left:=image1.Left-1;
end;

procedure TForm1.Timer2Timer(Sender: TObject);
begin
  if image2.Left<-824 then image2.Left:=image2.Left+form1.Width +822
                   else image2.Left:=image2.Left-1;
end;

procedure TForm1.Timer3Timer(Sender: TObject);
begin
  if (indice < 1) or
     (indice > 3) then
     indice:= 1;
  archivo:='mario'+inttostr(indice);
  image5.Picture.LoadFromLazarusResource(archivo);
  indice:=indice+1;
end;

procedure TForm1.Timer4Timer(Sender: TObject);
begin
  if image5.Top>=144 then t1:=false;  // 144 es la ubicación en donde esta el image5
    if image5.Top<=70 then t1:=true;  // 70 lo puse al calculo si es mas, el image5 salta menos

    if t1=true then image5.Top:=image5.Top+10   // 10 es un pequeño porción de salto
               else image5.Top:=image5.Top-10;

    if image5.Top=144 then timer4.Enabled:=false;
end;

procedure TForm1.Timer5Timer(Sender: TObject);
begin
   if (indice2 < 1) or
     (indice2 > 2) then
     indice2:= 1;
  archivo2:='goomba'+inttostr(indice2);
  image6.Picture.LoadFromLazarusResource(archivo2);
  indice2:=indice2+1;

  if (indice3 < 1) or
     (indice3 > 2) then
     indice3:= 1;
  archivo3:='koopatropa'+inttostr(indice3);
  image7.Picture.LoadFromLazarusResource(archivo3);
  indice3:=indice3+1;  // aquí me quede ayer miercoles 05 sep 2011

end;

procedure TForm1.Timer6Timer(Sender: TObject);
begin
   if image6.Left<-100 then image6.Left:=image6.Left+form1.Width +822
                   else image6.Left:=image6.Left-2;

   if image7.Left<-100 then image7.Left:=image7.Left+form1.Width +822
                   else image7.Left:=image7.Left-2;


  //image6.Left:=image6.Left-2;  // funciona bacán
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  s1:=lazarusresources.Find('sound1').Value;
  playsound(pchar(s1),hinstance, snd_memory or snd_Async or snd_loop);
   //s1:=lazarusresources.Free;
   //lazarusresources.Free;    //este funciona pero ....
  //playsound(pchar('sound1.wav'),hinstance,snd_async or snd_nodefault or snd_loop);  // AL SER DEL MISMO RECURSO QUE EL DE ARRIBA HACE QUE EL PROGRAMA SE BLOQUEE
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  PlaySound(nil, 0, 0);
  Form1.Free;
  Application.Terminate;

end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
    //lazarusresources.Free;    // que pasa....???
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
  );
begin
  if key= vk_up then timer4.Enabled:=true;
end;

initialization
{$I koopatropa.lrs}
{$I goomba.lrs}
{$I mariocronch.lrs}
{$I mariosonido.lrs}
end.

