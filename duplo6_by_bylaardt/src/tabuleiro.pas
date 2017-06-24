unit tabuleiro;

{$mode objfpc}{$H+}

interface


uses
  Classes, SysUtils, FileUtil, Forms, LazUTF8,
  Controls, Graphics, Dialogs, LResources, sobre,
  {$IFDEF WINDOWS}
  MMsystem,
  {$ELSE}
  process,
  {$ENDIF}
  StdCtrls, ExtCtrls, Menus, Buttons, Math, strutils, placar_geral,BGRABitmap,BGRABitmapTypes;

resourcestring
  tr_round = 'Score';
  tr_Score = 'Matches';
  tr_connect = 'click at the end';
  tr_names ='Diana;Apolo;Hermes';
const
  tamanho_da_peca = 60;  //padrão = 60    max=78
  bolas = 64;            //padrão = 64
  pecas = 256;
  distanciax = 22;
  distanciay = 22;
  margem = 4;
  transparencia = clGreen;
  TimeInterval = 5;      //10
  TimeSpace_H = 15;      //30
  TimeSpace_V = 10;      //20
  sleeptime = 180;       //180
type

  { TForm1 }
  TDomino = class(TGraphicControl)
    procedure Timer1Timer(Sender: TObject);
  private
    { private declarations }
    Imagem:TBitmap;
    Destino:Tpoint;
    Timer1:Ttimer;
  public
    Vertical:Boolean;
    Mostrar:Boolean;
    Valor:TPoint;
    Dono,Indice:ShortInt;
    procedure setdestino(X,Y:Integer);
    procedure CriaImagem;
    procedure Paint; override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    { public declarations }
  end;
  Tnaipes = record
     Duplo:boolean;
     Manteve,Puxou,Quebrou:ShortInt;
     Passou:boolean;
  end;
  Tjogador = record
     Nome: String;
     count:ShortInt;
     pedras: array[0..6] of TDomino;
     Naipes : array[0..6] of Tnaipes;
  end;

  Tmesa = record
     Jogador,vencedor,
     ProximoMao,
     NaipeDireita,NaipeEsquerda,
     DirecEsquerda,DirecDireita:ShortInt;
     PedraEsquerda,PedraDireita,PedraSelecao:TDomino;
     Naipes : array[0..6] of ShortInt;
     soma_meu,soma_teu,
     placar_meu,placar_teu,placar_ultimo:Integer;
  end;
  TForm1 = class(TForm)
    GroupBox1: TGroupBox;
    GroupBox2: TGroupBox;
    Iadversario1: TImage;
    IEu1: TImage;
    IEu2: TImage;
    IEu3: TImage;
    Image1: TImage;
    Image2: TImage;
    Ioponente: TImage;
    Ioponente1: TImage;
    Iparceiro: TImage;
    IEu: TImage;
    Iadversario: TImage;
    Iparceiro1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    Pano: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    PopupMenu1: TPopupMenu;
    SpeedButton1: TSpeedButton;
    StaticText1: TStaticText;
    StaticText2: TStaticText;
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItem5Click(Sender: TObject);
    Procedure MostraJogadores;
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem2Click(Sender: TObject);
    procedure PanoClick(Sender: TObject);
    procedure PedraClick(Sender: TObject);
    procedure PreencheJogada(Pedra:TDomino;Jogador,Manteve,Puxou,Quebrou:Shortint;passou:boolean);
    procedure FormCreate(Sender: TObject);
    procedure PoePedra(PedraAnterior,PedraAtual:Tdomino);
    procedure ControleDeMesa;
    Function NovoJogo:ShortInt;
    procedure PlaySound(Arquivo:String);
    Procedure AcresceMesa(EixoY:integer);
    procedure SpeedButton1Click(Sender: TObject);
  private
    { private declarations }
    TodasAsPecas:array [0..27] of TDomino;
    Estatistica: array[1..4] of Tjogador;
  public
    { public declarations }
  end;

var
  Form1: TForm1;
  Modo_cor:ShortInt;
  Mesa:Tmesa;
  stretchme:integer;
  cores: Array[0..10] of TPortableNetworkGraphic;

implementation

{$R *.lfm}

procedure TForm1.PedraClick(Sender: TObject);
var
  analitico:ShortInt;
  function analisa(pedra1,pedra2:TDomino):boolean;
    begin
      result:=(mesa.PedraDireita.indice=pedra1.indice) and (mesa.NaipeDireita in [pedra2.Valor.X,pedra2.Valor.Y]) or
              (mesa.PedraEsquerda.Indice=pedra1.indice) and (mesa.NaipeEsquerda in [pedra2.Valor.X,pedra2.Valor.Y])
    end;
begin
  if TDomino(sender).Dono=0 then begin
    if not (mesa.PedraSelecao=nil) then begin
       if analisa(TDomino(sender),mesa.PedraSelecao) then
         PoePedra(TDomino(sender),mesa.PedraSelecao);
    end
  end else if (TDomino(sender).Dono=mesa.Jogador) then begin
    if (mesa.PedraDireita=nil) and (mesa.PedraDireita=nil) then analitico:=1
    else if (mesa.NaipeEsquerda=mesa.NaipeDireita) or (mesa.PedraEsquerda.Indice=mesa.PedraDireita.Indice) then
      analitico:=ifthen(analisa(mesa.PedraEsquerda,TDomino(sender)),1,0)
    else
      analitico:=ifthen(analisa(mesa.PedraEsquerda,TDomino(sender)),1,0)+ifthen(analisa(mesa.PedraDireita,TDomino(sender)),2,0);
    if (analitico=3) and (Estatistica[mesa.Jogador].count=1) then analitico:=1;
    case analitico of
      0: begin
           mesa.PedraSelecao:=nil;
           ControleDeMesa;
         end;
      1: PoePedra(mesa.PedraEsquerda,TDomino(sender)); //click
      2: PoePedra(mesa.PedraDireita,TDomino(sender));  //click
      3: mesa.PedraSelecao:=TDomino(sender);
    end;
    Label1.visible:=analitico=3;
  end;
end;
procedure TForm1.MenuItem1Click(Sender: TObject);
var
  x:ShortInt;
begin
  Form3.cor:=TMenuItem(sender).tag;
  if Form3.cor<>Modo_cor then begin
    Screen.Cursor:=crHourGlass;
    Application.ProcessMessages;
    Modo_cor:=Form3.cor;
    for x:= 0 to 27 do
       if TodasAsPecas[x].Mostrar then begin
         TodasAsPecas[x].CriaImagem;
       end;
    form1.Repaint;
    Screen.Cursor:=crDefault;
  end;
end;

procedure TForm1.MenuItem2Click(Sender: TObject);
begin
  form2.showmodal;
end;

procedure TForm1.PanoClick(Sender: TObject);
begin
  pano.checked:=not pano.checked;
  sobre.Form3.papeldefundo:=pano.checked;
  repaint;
end;

{ TForm1 }
procedure TForm1.FormCreate(Sender: TObject);
var
  loop:integer;
begin

  GroupBox1.Caption:=tr_Score;
  GroupBox2.Caption:=tr_round;
  label1.caption:=tr_connect;
  Application.CreateForm(TForm3, Form3);
  for loop:=0 to 6 do begin
    cores[loop]:=TPortableNetworkGraphic.Create;
    cores[loop].LoadFromLazarusResource('bola_'+inttostr(loop));
  end;
  cores[7]:=TPortableNetworkGraphic.Create;
  cores[7].LoadFromLazarusResource('frente_v');
  cores[8]:=TPortableNetworkGraphic.Create;
  cores[8].LoadFromLazarusResource('frente_h');
  cores[9]:=TPortableNetworkGraphic.Create;
  cores[9].LoadFromLazarusResource('verso_v');
  cores[10]:=TPortableNetworkGraphic.Create;
  cores[10].LoadFromLazarusResource('verso_h');
  Randomize;
  mesa.placar_meu:=0;
  mesa.placar_teu:=0;
  mesa.placar_ultimo:=0;
  mesa.soma_meu:=0;
  mesa.soma_teu:=0;
  stretchme:=trunc(min(ClientWidth / 1920, ClientHeight / 1080)* tamanho_da_peca);
  Form2:=Tform2.Create(self);
  Label1.BorderSpacing.Bottom:=margem*2+stretchme*2;
  pano.checked:=Form3.papeldefundo;
  MenuItem5.Checked:=Form3.som;
  Modo_cor:=form3.cor;
end;

Function TForm1.NovoJogo:ShortInt;
var
  x,y:ShortInt;
  nomes:string;
  pecasx,pecasy:string;
  PosX,PosY:Integer;
  naotinha:boolean;
  AlturaMaxima:Integer;
begin
  nomes:=sobre.form3.edit1.text;
  if pos(';',nomes)>0 then nomes:=copy(nomes,1,pos(';',nomes)-1);
  if nomes='' then nomes:='eu';
  nomes:=nomes+';'+tr_names;
  label1.Visible:=false;
  form1.StaticText1.caption:=inttostr(mesa.soma_meu)+' x '+inttostr(mesa.soma_teu);
  mesa.vencedor:=0;
  mesa.PedraDireita:=nil;
  mesa.PedraEsquerda:=nil;
  for y:= 0 to 6 do mesa.Naipes[y]:=0;
  for x:=1 to 4 do begin
      for y:= 0 to 6 do begin
        Estatistica[x].Naipes[y].Manteve:=0;
        Estatistica[x].Naipes[y].Passou:=false;
        Estatistica[x].Naipes[y].Duplo:=false;
        Estatistica[x].Naipes[y].Puxou:=0;
        Estatistica[x].Naipes[y].Quebrou:=0;
      end;
    Estatistica[x].count:=0;
    Estatistica[x].Nome:=ExtractDelimited(x,nomes,[';']);
  end;
  label2.caption:=Estatistica[1].Nome;
  label3.caption:=Estatistica[2].Nome;
  label4.caption:=Estatistica[3].Nome;
  label5.caption:=Estatistica[4].Nome;
  pecasx := '0000000111111222223333444556';
  pecasy := '0123456123456234563456456566';
  result:=0;
  ioponente.Width:=stretchme*2;       iparceiro.width:=ioponente.width ; iadversario.width:=ioponente.width;    ieu.width:=ioponente.width;
  ioponente.height:=ioponente.Width;  iparceiro.height:=ioponente.height; iadversario.height:=ioponente.height; ieu.height:=ioponente.height;

  iparceiro.top:=margem;
  AlturaMaxima:=ClientHeight-margem;
  ieu.top:=AlturaMaxima-ieu.Height-margem;
  ioponente.top:=(AlturaMaxima-(stretchme+margem)*7) div 2 -((stretchme+margem))*2-label5.Height-2;
  Iadversario.top:=ioponente.top;

  iparceiro.left:=(ClientWidth-(stretchme+margem)*7) div 2 -((stretchme+margem))*2;
  ieu.left:=(ClientWidth-(stretchme+margem)*7) div 2 +((stretchme+margem))*7;
  ioponente.left:=margem;
  Iadversario.left:=ClientWidth-Iadversario.Width-margem;

  ioponente1.left:=ioponente.left;     iparceiro1.left:=iparceiro.left ; iadversario1.left:=iadversario.left;    ieu1.left:=ieu.left;
  ioponente1.top:=ioponente.top;       iparceiro1.top:=iparceiro.top ; iadversario1.top:=iadversario.top;    ieu1.top:=ieu.top;
  ioponente1.width:=ioponente.width;   iparceiro1.width:=iparceiro.width ; iadversario1.width:=iadversario.width;    ieu1.width:=ieu.width;
  ioponente1.height:=ioponente.height; iparceiro1.height:=iparceiro.height ; iadversario1.height:=iadversario.height;    ieu1.height:=ieu.height;
  IEu2.Top:=IEu.top;IEu2.left:=IEu.left;IEu2.Width:=IEu.Width;IEu2.Height:=IEu.Height;
  IEu3.Top:=IEu.top;IEu3.left:=IEu.left;IEu3.Width:=IEu.Width;IEu3.Height:=IEu.Height;

  Label2.top:=ieu.Top-label2.Height-2;
  label3.top:=Iadversario.top+Iadversario.Height+2;
  label4.top:=Iparceiro.top+Iparceiro.Height+2;
  label5.top:=Ioponente.top+Ioponente.Height+2;
  label2.left:=ieu.left+(ieu.Width-label2.Width) div 2;
  label3.left:=Iadversario.left+(ieu.Width-label3.Width) div 2;
  label4.left:=Iparceiro.left+(ieu.Width-label4.Width) div 2;
  label5.left:=Ioponente.left+(ieu.Width-label5.Width) div 2;
  for x:=27 downto 0 do begin
    y:=Random(x+1)+1;
    naotinha:=TodasAsPecas[x] = nil;
    if naotinha then begin
       TodasAsPecas[x]:=TDomino.Create(self);
       TodasAsPecas[x].OnClick:=@PedraClick;
    end;
    TodasAsPecas[x].dono:=(x div 7) +1;
    Estatistica[TodasAsPecas[x].dono].pedras[Estatistica[TodasAsPecas[x].dono].count]:=TodasAsPecas[x]; //Diz a cada jogador a pedra que ele tem
    inc(Estatistica[TodasAsPecas[x].dono].count);                                                       //Diz a cada jogador a quantia de pedras que ele tem
    TodasAsPecas[x].Vertical:=TodasAsPecas[x].dono in [1,3];
    TodasAsPecas[x].Indice:=x;
    posY:=ifthen(TodasAsPecas[x].dono=1,   AlturaMaxima- stretchme*2 - margem,
          ifthen(TodasAsPecas[x].dono=3,   margem,
                                          (AlturaMaxima-(stretchme+margem)*7) div 2 +(stretchme+margem)*(x mod 7)));
    PosX:=ifthen(TodasAsPecas[x].dono=2,   ClientWidth - stretchme*2 - margem,
          ifthen(TodasAsPecas[x].dono=4,   margem,
                                          (ClientWidth-(stretchme+margem)*7) div 2 +(stretchme+margem)*(x mod 7)));
    TodasAsPecas[x].top:=PosY;
    TodasAsPecas[x].left:=PosX;
    TodasAsPecas[x].mostrar:=TodasAsPecas[x].dono=1;
    TodasAsPecas[x].Valor:=Point(strtoint(pecasx[y]),strtoint(pecasy[y]));
    pecasx:=copy(pecasx,1,y-1)+copy(pecasx,y+1,x);
    pecasy:=copy(pecasy,1,y-1)+copy(pecasy,y+1,x);
    if (TodasAsPecas[x].Valor.X=6) and (TodasAsPecas[x].Valor.Y=6) then result:=x;
    if naotinha then InsertControl(TodasAsPecas[x]);
    TodasAsPecas[x].CriaImagem;
  end;
end;

procedure TDomino.Timer1Timer(Sender: TObject);
  procedure MostraVitoria(Pontos:Integer); //Pontos pontos positivos= nós, negativos= eles
  var
    x:integer;
  begin
    if (pontos>0) then mesa.soma_meu:=mesa.soma_meu+abs(pontos) else mesa.soma_teu:=mesa.soma_teu+abs(pontos);
    if mesa.soma_meu>99 then  inc(mesa.placar_meu);
    if mesa.soma_teu>99 then  inc(mesa.placar_teu);
    form1.StaticText1.caption:=inttostr(mesa.soma_meu)+' x '+inttostr(mesa.soma_teu);
    form2.StringGrid1.RowCount:=form2.StringGrid1.RowCount+1;
    if not (mesa.vencedor=0) then begin
      form2.StringGrid1.Cells[0,form2.StringGrid1.RowCount-2]:=ifthen(mesa.vencedor>0,'','X');
      form2.StringGrid1.Cells[1,form2.StringGrid1.RowCount-2]:=form1.Estatistica[abs(mesa.vencedor)].nome;
      form2.StringGrid1.Cells[2,form2.StringGrid1.RowCount-2]:=formatfloat('#0;"-";"-"',pontos);
      form2.StringGrid1.Cells[3,form2.StringGrid1.RowCount-2]:=formatfloat('#0;"-";"-"',-pontos);
    end;
    form2.StringGrid1.Cells[0,form2.StringGrid1.RowCount-1]:=ifthen(mesa.vencedor>0,'','X');
    form2.StringGrid1.Cells[1,form2.StringGrid1.RowCount-1]:='Total';
    form2.StringGrid1.Cells[2,form2.StringGrid1.RowCount-1]:=formatfloat('#0;"(0)";"-"',mesa.soma_meu);
    form2.StringGrid1.Cells[3,form2.StringGrid1.RowCount-1]:=formatfloat('#0;"(0)";"-"',mesa.soma_teu);
    for x:= 27 downto 0 do begin
      if not form1.TodasAsPecas[x].Mostrar then begin
        form1.TodasAsPecas[x].mostrar:=true;
        form1.TodasAsPecas[x].CriaImagem;
      end;
    end;
    form1.update;
    form2.label1.caption:=inttostr(mesa.placar_meu)+' x '+inttostr(mesa.placar_teu);
    form1.StaticText2.caption:=form2.label1.caption;
    form2.ShowModal;
    if (mesa.soma_meu>99) or (mesa.soma_teu>99) then begin
      mesa.soma_meu:=0;
      mesa.soma_teu:=0;
      form1.poepedra(nil,form1.TodasAsPecas[form1.NovoJogo]);
      form2.StringGrid1.Clear;
      form2.StringGrid1.RowCount:=2;
      form2.StringGrid1.Cells[0,1]:='';
      form2.StringGrid1.Cells[1,1]:='Total'; //traduzir
      form2.StringGrid1.Cells[2,1]:='-';
      form2.StringGrid1.Cells[3,1]:='-';
    end else begin
      form1.novojogo;
      mesa.Jogador:=mesa.ProximoMao;
      Form1.ControleDeMesa;
    end;
  end;
begin
       if (destino.x-left)div TimeSpace_H >0 then left:=left+TimeSpace_H
  else if (destino.x-left)div TimeSpace_H <0 then left:=left-TimeSpace_H
  else if destino.x<>left then left:=destino.x;

       if (destino.y-top)div TimeSpace_V >0 then top:=top+TimeSpace_V
  else if (destino.y-top)div TimeSpace_V <0 then top:=top-TimeSpace_V
  else if destino.y<>top then top:=destino.y;

       if destino.y>top  then top:=top+1;
  timer1.Enabled:=not ((left=Destino.X) and (top=Destino.y));
  application.ProcessMessages;
  if not timer1.enabled then begin
    form1.mostrajogadores;
    if mesa.vencedor<>0 then begin
      form1.playsound('click');
      MostraVitoria(mesa.placar_ultimo)
    end else begin
       form1.playsound('click');
       Form1.ControleDeMesa;
    end;
  end;
end;
procedure TForm1.PlaySound(Arquivo:String);
{$IFNDEF WINDOWS}
var
  Proc:TProcess;
  {$ENDIF}
begin
  Application.ProcessMessages;
  if form3.som then begin
  {$IFDEF WINDOWS}
  sndPlaySound(pchar(extractfilepath(ParamStr(0))+arquivo+'.wav'), SND_SYNC);
  {$ELSE}
  Proc:=TProcess.create(nil);
  Proc.Executable:='aplay';
  Proc.Parameters.add('-q');
  Proc.Parameters.add(utf8tosys(extractfilepath(ParamStr(0)))+arquivo+'.wav');
  Proc.Options:=[];
  proc.execute;
  Proc.free;
  {$ENDIF}
  end;
  sleep(sleeptime);
end;

procedure TDomino.setdestino(X,Y:Integer);
var
  margemideal:Integer;
begin
  timer1.Enabled:=false;
  destino.x:=X;
  Destino.y:=Y;
  margemideal:=stretchme*5 div 2+margem*2;
  if destino.y>(form1.ClientHeight- margemideal) then begin
    form1.AcresceMesa(-stretchme div 2);
    Destino.y:=Destino.y-stretchme div 2;
  end;
  if destino.y< margemideal    then begin
    form1.AcresceMesa(stretchme div 2);
    Destino.y:=Destino.y+stretchme div 2;
  end;
  timer1.Enabled:=not ((left=Destino.X) and (top=Destino.y));
end;

constructor TDomino.Create(AOwner: TComponent);
begin
  Inherited create(AOwner);
  Imagem:=TBitmap.Create;
  Imagem.Transparent:=true;
  Imagem.TransparentColor:= transparencia;
  Timer1:=Ttimer.Create(self);
  timer1.Interval:=TimeInterval;
  timer1.enabled:=false;
  timer1.OnTimer:=@Timer1Timer;
end;

destructor TDomino.Destroy;
begin
  timer1.free;
  Imagem.Free;
end;
procedure TDomino.Paint;
begin
  canvas.Draw(0,0,imagem);
end;

procedure TDomino.CriaImagem;
var
  cor:TPortableNetworkGraphic;
  fator:extended;
  ImagemOriginal:TBGRABitmap;
  new:TBGRACustomBitmap;
  procedure pinta(numero,left1,top1:Integer);
  begin
    if numero>0 then begin
      cor:=cores[ifthen(modo_cor<7,modo_cor,numero)];
      if numero in [1,3,5] then
        ImagemOriginal.Canvas.Draw((pecas-bolas) div 2+left1,(pecas-bolas) div 2+top1,cor); //centro
      if ((numero >1) and vertical) or (numero >3) then begin // Diagonal Direita/esquerda
        ImagemOriginal.Canvas.Draw(pecas-bolas-distanciax+left1,distanciay+top1,cor);
        ImagemOriginal.Canvas.Draw(distanciax+left1,pecas-bolas-distanciay+top1,cor);
      end;
      if ((numero >1) and (not vertical)) or (numero >3) then begin // Diagonal Esquerda/Direita
        ImagemOriginal.Canvas.Draw(distanciax+left1,distanciay+top1,cor);
        ImagemOriginal.Canvas.Draw(pecas-bolas-distanciax+left1,pecas-bolas-distanciay+top1,cor);
      end;
      if (numero =6) then begin
         if vertical then begin
            ImagemOriginal.Canvas.Draw(distanciax+left1,(pecas-bolas) div 2+top1,cor);
            ImagemOriginal.Canvas.Draw(pecas-bolas-distanciax+left1,(pecas-bolas) div 2+top1,cor);
         end else begin
           ImagemOriginal.Canvas.Draw((pecas-bolas) div 2+left1,distanciay+top1,cor); //centro
           ImagemOriginal.Canvas.Draw((pecas-bolas) div 2+left1,pecas-bolas-distanciay+top1,cor); //centro
         end;
      end;
    end;
  end;
begin
  cor:=cores[ifthen(mostrar,7,9)+ifthen(vertical,0,1)];
  ImagemOriginal:=TBGRABitmap.Create(cor.Width,cor.Height);
  ImagemOriginal.Canvas.pen.color:=transparencia;
  ImagemOriginal.Canvas.Brush.color:=transparencia;
  ImagemOriginal.Canvas.Rectangle(0,0,ImagemOriginal.Width,ImagemOriginal.Height);
  ImagemOriginal.Canvas.Draw(0,0,cor);
  fator:=pecas/stretchme;
  width:=round(cor.Width/fator);
  Height:=round(cor.Height/fator);
  if mostrar then begin
     pinta(valor.x,0,0);
     if vertical then
       pinta(valor.y,0,pecas)
     else
       pinta(valor.y,pecas,0);
  end;
  Imagem.Width:=Width;
  Imagem.Height:=Height;
  new:=ImagemOriginal.Resample(Width,Height);
  new.Draw(Imagem.canvas,rect(0,0,Width,Height));
  //Imagem.Canvas.StretchDraw(rect(0,0,Width,Height),ImagemOriginal.Bitmap);
  new.free;
  ImagemOriginal.free;
end;
Procedure TForm1.PoePedra(PedraAnterior,PedraAtual:TDomino);
var
  direcao,DirecaoAnterior:ShortInt;
  Manteve,Puxou,Quebrou:ShortInt;
  PosX,PosY:Integer;
  procedure inverter(Pedra:TDomino);
  var
    t:ShortInt;
  begin
    t:=Pedra.Valor.X;
    Pedra.Valor.X:=Pedra.Valor.Y;
    Pedra.Valor.Y:=t;
  end;
  function MudarRumo(var dire:ShortInt;Pedr:Tdomino):ShortInt;
    var
      procede:boolean;
    begin
      result:=dire;
      case dire of
          1: procede:=Pedr.Destino.X+Pedr.Width+ ifthen(PedraAtual.Valor.x=PedraAtual.Valor.y,7,9)*stretchme div 2+margem*3>ClientWidth;
          3: procede:=Pedr.Destino.X < ifthen(PedraAtual.Valor.x=PedraAtual.Valor.y,7,9)*stretchme div 2+margem*3;
      else
        procede:=not (PedraAtual.Valor.x=PedraAtual.valor.Y);
      end;
      if procede then begin
        case dire of
          1,3: dire:=ifthen(pedraanterior.Indice=mesa.PedraDireita.indice,2,4);
          2,4: dire:=ifthen(pedr.Destino.x<(ClientWidth div 2),1,3);
        end;
      end;
      direcao:=dire;
    end;
    procedure SetXY(toX,toY:Integer);
    begin
       posx:=toX;
       posy:=toY;
    end;
    procedure SetXYDuplo(toX,toY,ToDuploAtualX,toDuploAtualY:integer);
    begin
      if PedraAtual.valor.x=PedraAtual.valor.y then begin
        posx:=ToDuploAtualX;
        posy:=toDuploAtualY;
      end else begin
        posx:=toX;
        posy:=toY;
       end;
    end;
begin
  try
    PedraAtual.BringToFront;
  PedraAtual.Mostrar:=true;
  If PedraAnterior=Nil then begin
     PedraAtual.Vertical:=PedraAtual.valor.X=PedraAtual.Valor.Y;
     PedraAtual.CriaImagem;
     PedraAtual.setdestino((ClientWidth-Pedraatual.Width) div 2,(ClientHeight-Pedraatual.Height) div 2);
     Mesa.DirecDireita:=1;
     Mesa.DirecEsquerda:=3;
     mesa.PedraDireita:=PedraAtual;
     mesa.PedraEsquerda:=PedraAtual;
     mesa.NaipeDireita:=PedraAtual.valor.y;
     mesa.NaipeEsquerda:=PedraAtual.valor.x;
     Manteve:=PedraAtual.Valor.X;
     Puxou:=PedraAtual.Valor.Y;
     Quebrou:=-1;
     mesa.ProximoMao:=(PedraAtual.Dono mod 4) +1;
  end else begin
     if (pedraanterior.Indice=mesa.PedraDireita.indice) and (mesa.NaipeDireita in [PedraAtual.valor.x,PedraAtual.valor.y]) then begin
        Manteve:=mesa.NaipeEsquerda;
        Quebrou:=mesa.NaipeDireita;
        DirecaoAnterior:=MudarRumo(mesa.DirecDireita,Mesa.PedraDireita);
        Mesa.PedraDireita:=pedraatual;
        if mesa.NaipeDireita=ifthen(Direcao in[1,4],Pedraatual.Valor.Y,Pedraatual.Valor.X) then inverter(PedraAtual);
     end else begin
        Manteve:=mesa.NaipeDireita;
        Quebrou:=mesa.NaipeEsquerda;
        DirecaoAnterior:=MudarRumo(mesa.DirecEsquerda,Mesa.PedraEsquerda);
        Mesa.PedraEsquerda:=pedraatual;
        if mesa.NaipeEsquerda=ifthen(Direcao in[1,4],Pedraatual.Valor.Y,Pedraatual.Valor.X) then inverter(PedraAtual);
     end;
     PedraAtual.Vertical:=((PedraAtual.valor.X=PedraAtual.Valor.Y) XOR (direcao in [2,4])) or
                          ((direcao in [2,4]) and (DirecaoAnterior<>direcao));
     puxou:=ifthen(direcao in[1,4],PedraAtual.valor.y,PedraAtual.valor.x);

     PedraAtual.CriaImagem;
     if (PedraAnterior.Valor.X=PedraAnterior.Valor.Y) and
        (pedraanterior.Vertical = (direcao in [2,4])) then
       direcaoanterior:=direcao;
     case direcao of
     1: begin //Para a direita
          case DirecaoAnterior of
            1: SetXY(pedraanterior.Width,((pedraanterior.Height-pedraatual.Height) div 2));
            2: SetXYDuplo(0                                            ,-PedraAtual.Height,
                         (PedraAnterior.Width-pedraanterior.Width)div 2,-PedraAtual.Height);
            4: SetXYDuplo(0                                            ,pedraanterior.Height,
                         (PedraAnterior.Width-pedraanterior.Width)div 2,pedraanterior.Height);
          end;
        end;
     2: begin //Para Cima
         case DirecaoAnterior of
           2: SetXY((pedraanterior.Width-pedraatual.Width) div 2,-PedraAtual.Height);

           1: SetXY(pedraanterior.Width-PedraAtual.Width               ,-PedraAtual.Height);
           3: SetXY(pedraanterior.Width-pedraanterior.Width            ,-PedraAtual.Height);
         end;
        end;
     3: begin //Para a esquerda
         case DirecaoAnterior of
           3: SetXY(-PedraAtual.Width,((pedraanterior.Height-pedraatual.Height) div 2));

           2: SetXYDuplo(pedraanterior.Width-PedraAtual.Width         ,-pedraatual.Height,
                        (PedraAnterior.Width-pedraanterior.Width)div 2,-pedraatual.Height);
           4: SetXYDuplo(pedraanterior.Width-PedraAtual.Width         ,PedraAnterior.Height,
                        (PedraAnterior.Width-pedraanterior.Width)div 2,PedraAnterior.Height);
         end;
        end;
     4: begin //Para baixo
         case DirecaoAnterior of
           4: SetXY((pedraanterior.Width-pedraatual.Width) div 2      ,PedraAnterior.Height);
           1: SetXY(pedraanterior.Width-pedraatual.Width              ,pedraanterior.Height);
           3: SetXY(0                                                 ,pedraanterior.Height);
         end;
        end;
     end;
     PedraAtual.setdestino(Pedraanterior.Destino.X+PosX,Pedraanterior.Destino.Y+PosY);
  end;
  mesa.NaipeEsquerda:=IfThen(Mesa.DirecEsquerda in [1,4],mesa.PedraEsquerda.valor.y,mesa.PedraEsquerda.valor.x);
  mesa.NaipeDireita:=IfThen(Mesa.DirecDireita in [1,4],mesa.PedraDireita.valor.y,mesa.Pedradireita.valor.x);
  PreencheJogada(PedraAtual,PedraAtual.Dono,Manteve,Puxou,Quebrou,false);
  if mesa.vencedor=0 then begin
    label1.Visible:=false;
    application.ProcessMessages;
  end;
  except
    Sleep(sleeptime);
    ControleDeMesa;
  end;
end;
Procedure Tform1.MostraJogadores;
begin
  if sobre.Form3.ComboBox1.ItemIndex=0 then begin
    ieu.Visible :=mesa.jogador=1;
    ieu1.Visible:= not ieu.Visible;
    ieu2.Visible:=false;
    ieu3.Visible:=false;
  end else begin
     ieu.Visible :=false;
     ieu1.Visible:=false;
     ieu2.Visible:=mesa.jogador=1;
     ieu3.Visible:= not ieu2.Visible;
  end;
  Iadversario.Visible:=mesa.jogador=2;
  Iparceiro.Visible:=mesa.jogador=3;
  Ioponente.Visible:=mesa.jogador=4;

  Iadversario1.Visible:=not Iadversario.Visible;
  Iparceiro1.Visible:=not Iparceiro.Visible;
  Ioponente1.Visible:=not Ioponente.Visible;
  Application.ProcessMessages;
end;

procedure TForm1.FormPaint(Sender: TObject);
var
  x,y:integer;
begin
 if pano.checked then begin
   for x:=0 to (ClientWidth-1) div 256 +1 do
       for y:=0 to (ClientHeight-1) div 256 +1 do
          canvas.Draw(x*256,y*256,image1.Picture.PNG);
 end;
end;

procedure TForm1.FormDestroy(Sender: TObject);
var
  loop:integer;
begin
  for loop:=0 to 10 do begin
     cores[loop].free;
  end;
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  poepedra(nil,TodasAsPecas[NovoJogo]);
end;

procedure TForm1.MenuItem4Click(Sender: TObject);
begin
  form3.showmodal;
end;

procedure TForm1.MenuItem5Click(Sender: TObject);
begin
  MenuItem5.Checked:=not MenuItem5.Checked;
  sobre.Form3.som:=MenuItem5.Checked;
end;

Procedure Tform1.Controledemesa;
var
  passou:boolean;
  x:ShortInt;
  MelhorPedra:ShortInt;
  Score,MelhorScore:Integer;
  MelhorDireita:boolean;
  function AnalisaMinhaJogada(direita:boolean;Pedra:Tdomino):Integer;
    var
      fica,tem:TPoint;
      duplox,duploy:boolean;
      x,eum,par,adv,opn,mao:ShortInt;
      function somapontos(FicaX,TemX:ShortInt;duplo:boolean):Integer;
      var
        pts:Integer;
        y:integer;
        meus,deles,parce:Extended;
        propd,propp:shortint;
        RiscoDeBate:boolean;
      begin
        result:=max(TemX*2-mesa.Naipes[FicaX]+ifthen(duplo,0,1),0)*mesa.Naipes[FicaX];
        pts:=(TemX-ifthen(duplo,1,0))*ifthen(mao=eum,6,4)*ifthen(Estatistica[par].Naipes[TemX].Passou,1,3);
        if Estatistica[opn].Naipes[ficax].Passou then pts:=pts*ifthen(mao=opn,5,3);
        if Estatistica[adv].Naipes[ficax].Passou then pts:=pts*ifthen(mao=opn,5,3);
        if Estatistica[par].Naipes[ficax].Passou and (Estatistica[par].count<Estatistica[eum].count) then pts:=pts div 3;

        if mao in [opn,adv] then result:=result+
                                         (-Estatistica[opn].Naipes[ficax].Puxou-Estatistica[adv].Naipes[ficax].Puxou+Estatistica[par].Naipes[ficax].Puxou)*10+
                                         (+Estatistica[opn].Naipes[ficax].Manteve+Estatistica[adv].Naipes[ficax].Manteve-Estatistica[par].Naipes[ficax].Manteve)*2+
                                         (+Estatistica[opn].Naipes[ficax].quebrou+Estatistica[adv].Naipes[ficax].quebrou-Estatistica[par].Naipes[ficax].quebrou)*5
        else                     result:=result+
                                             (-Estatistica[opn].Naipes[ficax].Puxou-Estatistica[adv].Naipes[ficax].Puxou+Estatistica[par].Naipes[ficax].Puxou)*9+
                                             (+Estatistica[opn].Naipes[ficax].Manteve+Estatistica[adv].Naipes[ficax].Manteve-Estatistica[par].Naipes[ficax].Manteve)*1+
                                             (+Estatistica[opn].Naipes[ficax].quebrou+Estatistica[adv].Naipes[ficax].quebrou-Estatistica[par].Naipes[ficax].quebrou)*7;
        if (pedra.valor.x=pedra.valor.y) and (pedra.valor.x=ficax) then begin
            result:=result+ifthen((7-mesa.Naipes[ficax]-temx)>temx+1,1400 div max(temx*2,1),0);
        end;

        RiscoDeBate:=false;
        if (mesa.Naipes[FicaX]>4) and (fica.x=fica.y) then begin //fechou
          meus:=ifthen(duplo,-temx*2,0);
          deles:=0;
          parce:=0;
          for y:=0 to 27 do
            begin
              if TodasAsPecas[y].Dono=eum then
                 meus:=meus+TodasAsPecas[y].Valor.X+TodasAsPecas[y].Valor.Y
              else if TodasAsPecas[y].Dono>0 then begin
                propp:=1;
                propd:=2;
                if (Estatistica[par].Naipes[TodasAsPecas[y].valor.X].Passou) or
                   (Estatistica[par].Naipes[TodasAsPecas[y].valor.y].Passou) then
                   dec(propp);
                if (Estatistica[adv].Naipes[TodasAsPecas[y].valor.X].Passou) or
                   (Estatistica[adv].Naipes[TodasAsPecas[y].valor.y].Passou) then
                   dec(propd);
                if (Estatistica[opn].Naipes[TodasAsPecas[y].valor.X].Passou) or
                   (Estatistica[opn].Naipes[TodasAsPecas[y].valor.y].Passou) then
                   dec(propd);
                if (TodasAsPecas[y].valor.Y=TodasAsPecas[y].valor.X) and (TodasAsPecas[y].valor.Y=temx) then begin //verifica se alguém tem o duplo
                  if (propd=0) then begin //eles tem o duplo
                    deles:=deles-TodasAsPecas[y].valor.X-TodasAsPecas[y].valor.Y;
                    if ((Estatistica[adv].count=1) and Estatistica[opn].Naipes[temx].Passou) or
                       ((Estatistica[opn].count=1) and Estatistica[adv].Naipes[temx].Passou) or
                       (Estatistica[opn].count+Estatistica[adv].count=2) then begin //certeza que não bate
                       deles:=0;
                       parce:=160;
                       break;
                    end else
                      if not(((Estatistica[adv].count>1) and Estatistica[opn].Naipes[temx].Passou) or
                         ((Estatistica[opn].count>1) and Estatistica[adv].Naipes[temx].Passou)) then begin //certeza que não bate (com not - para inverter)
                         RiscoDeBate:=true;
                    end;
                  end else if (propp=0) then begin //parceiro tem o duplo
                    parce:=parce-TodasAsPecas[y].valor.X-TodasAsPecas[y].valor.Y;
                    if Estatistica[par].count=1 then begin
                       deles:=160;
                       parce:=0;
                       break;
                    end;
                  end;
                end;
                if propd+propp>0 then begin
                  deles:=(TodasAsPecas[y].valor.X+TodasAsPecas[y].valor.Y)*propd/(propd+propp);
                  parce:=(TodasAsPecas[y].valor.X+TodasAsPecas[y].valor.Y)*propp/(propd+propp);
                end;
            end;
        end;
        if parce=0 then result:=1000 else
        if parce+meus<deles then result:=ifthen(RiscoDeBate,max(random(3),1)*1000,130) else
           result:=pedra.Valor.x+pedra.valor.Y;
      end;
      result:=max(result*pts,0)+pedra.Valor.x+pedra.valor.Y;
    end;
    begin
      //A identificação da melhor peça (indice e ponta)
      //O Score da melhor peça a ser jogada
      //Preciso saber quantos pontos tem na mão
      //Preciso saber se tem o duplo da ponta
      //Preciso saber qual ponta eu posso jogar
      //Preciso identificar o Adversário, o parceiro e o oponente
      //Preciso saber se a pedra fecha o jogo
      result:=0;
      fica.X:=ifthen(direita,mesa.NaipeEsquerda,mesa.NaipeDireita);
      fica.y:=ifthen(pedra.valor.x=ifthen(direita,mesa.NaipeDireita,mesa.NaipeEsquerda),pedra.valor.y,pedra.valor.x);
      eum:=mesa.jogador;
      adv:=eum mod 4 +1;
      par:=adv mod 4 +1;
      opn:=par mod 4 +1;
      mao:=eum;
      for x:=1 to 4 do if Estatistica[x].count<Estatistica[mao].count then
        mao:=x;
      tem:=Point(0,0);
      duplox:=false;
      duploy:=false;
      for x:=0 to Estatistica[eum].count-1 do begin
        if not(estatistica[eum].pedras[x].indice=pedra.Indice) then begin
           if fica.x in [estatistica[eum].pedras[x].Valor.x,estatistica[eum].pedras[x].Valor.y] then begin
             inc(tem.X);
             duplox:=duplox or (estatistica[eum].pedras[x].Valor.x=estatistica[eum].pedras[x].Valor.y);
           end;
           if fica.y in [estatistica[eum].pedras[x].Valor.x,estatistica[eum].pedras[x].Valor.y] then begin
             inc(tem.Y);
             duploy:=duploy or (estatistica[eum].pedras[x].Valor.x=estatistica[eum].pedras[x].Valor.y);
           end;
        end;
      end;
      result:=somapontos(Fica.X,Tem.X,duplox)+somapontos(Fica.y,Tem.y,duploy);
      passou:=false;
    end;
begin
  passou:=true;
  repeat
    MelhorScore:=-20000;
    melhorpedra:=0;
    MelhorDireita:=true;
    for x:=0 to Estatistica[mesa.jogador].count-1 do begin
         if (Estatistica[mesa.jogador].pedras[x].Valor.X = mesa.NaipeDireita) or
            (Estatistica[mesa.jogador].pedras[x].Valor.Y = mesa.NaipeDireita) then begin
               Score:=AnalisaMinhaJogada(true,Estatistica[mesa.jogador].pedras[StrToInt(inttostr(x))]);
               if (Score>melhorScore) then begin
                   MelhorScore:=Score;
                   MelhorPedra:=x;
                   MelhorDireita:=true;
                 end;
            end;
         if (Estatistica[mesa.jogador].pedras[x].Valor.X = mesa.NaipeEsquerda) or
            (Estatistica[mesa.jogador].pedras[x].Valor.Y = mesa.NaipeEsquerda) then begin
               Score:=AnalisaMinhaJogada(false,Estatistica[mesa.jogador].pedras[StrToInt(inttostr(x))]);
                  if (Score>melhorScore) then begin
                      MelhorScore:=Score;
                      MelhorPedra:=x;
                      MelhorDireita:=false;
                    end;
            end;
    end;
    if passou then begin//Passou
      passou:=mesa.vencedor=0;
      Preenchejogada(nil,mesa.jogador,0,0,0,true);
    end
    else if mesa.jogador>1 then begin
      if melhorDireita then
        PoePedra(Mesa.PedraDireita,Estatistica[mesa.jogador].pedras[MelhorPedra])
      else
        PoePedra(Mesa.PedraEsquerda,Estatistica[mesa.jogador].pedras[MelhorPedra]);
    end;
  until (mesa.vencedor <>0) or (not passou);
  MostraJogadores;
end;

procedure Tform1.PreencheJogada(Pedra:TDomino;Jogador,Manteve,Puxou,Quebrou:Shortint;passou:boolean);
var
  x,X1,Y1:ShortInt;
  procedure SomaOsPontos;
  var
    pontos_meu,pontos_teu:Integer;
    x:integer;
    begin
      pontos_meu:=0;      pontos_teu:=0;
      for x:= 0 to 27 do
        if TodasAsPecas[x].Dono>0 then begin
          if (TodasAsPecas[x].dono in [1,3]) then  pontos_meu:=pontos_meu+TodasAsPecas[x].valor.X+TodasAsPecas[x].valor.Y
          else pontos_teu:=pontos_teu+TodasAsPecas[x].valor.X+TodasAsPecas[x].valor.Y;
        end;
      if mesa.vencedor<0 then begin
        if (abs(mesa.vencedor) in [1,3]) then
            mesa.placar_ultimo:=ifthen(pontos_teu>pontos_meu,pontos_teu,-pontos_meu)
        else
            mesa.placar_ultimo:=ifthen(pontos_meu>pontos_teu,-pontos_meu,pontos_teu);
      end else
      mesa.placar_ultimo:=ifthen(mesa.vencedor in [1,3],pontos_teu,-pontos_meu);
    end;
begin
  if passou then begin
    Estatistica[jogador].Naipes[mesa.NaipeDireita].Passou:=true;
    Estatistica[jogador].Naipes[mesa.Naipeesquerda].Passou:=true;
    MostraJogadores;
    playsound('toc');
    mesa.jogador:=(mesa.jogador mod 4)+1;
  end else begin
    If Pedra.valor.x>Pedra.valor.Y then begin
       X1:=Pedra.Valor.Y;
       Y1:=Pedra.Valor.X;
    end else begin
       X1:=Pedra.Valor.X;
       Y1:=Pedra.Valor.Y;
    end;
    if quebrou<0 then begin //Jogada inicial
      inc(Estatistica[jogador].Naipes[manteve].Puxou);
      inc(Estatistica[jogador].Naipes[Puxou].Puxou);
    end else begin
        if manteve=puxou then begin//forçou
          inc(Estatistica[jogador].Naipes[Puxou].Puxou);
          inc(Estatistica[jogador].Naipes[Manteve].Manteve);
          inc(Estatistica[jogador].Naipes[Quebrou].Quebrou);

          inc(Estatistica[jogador].Naipes[Puxou].Puxou);
          inc(Estatistica[jogador].Naipes[Manteve].Manteve);
        end else if X1=Y1 then begin//Duplo
          Estatistica[jogador].Naipes[Puxou].Duplo:=true;
          if Estatistica[jogador].Naipes[X1].Manteve>0 then begin
            inc(Estatistica[jogador].Naipes[X1].Manteve);
            inc(Estatistica[jogador].Naipes[X1].puxou);
          end;
        end else begin //Jogada normal
           inc(Estatistica[jogador].Naipes[Puxou].Puxou);
           inc(Estatistica[jogador].Naipes[Manteve].Manteve);
           inc(Estatistica[jogador].Naipes[Quebrou].Quebrou);
        end;
    end;
    dec(Estatistica[jogador].count);
    if Estatistica[jogador].count=0 then //Temos um vencedor
       mesa.vencedor:=jogador
    else begin //Ninguém bateu, mas pode ter fechado o jogo...
      inc(mesa.Naipes[pedra.valor.x]);
      if not(pedra.valor.x=pedra.valor.y) then inc(mesa.Naipes[pedra.valor.y]);
      if (mesa.NaipeDireita=mesa.NaipeEsquerda) and (mesa.Naipes[mesa.NaipeEsquerda]=7) then //O Jogo fechou (vencedor negativo)
        mesa.vencedor:=-jogador
      else begin //O Jogo continua
        for x:= 0 to Estatistica[jogador].count-1 do begin //Elimina a pedra das estatisticas da mão do jogador
           if Estatistica[Pedra.Dono].pedras[x].Indice=pedra.indice then begin
              Estatistica[Pedra.Dono].pedras[x]:=Estatistica[Pedra.Dono].pedras[Estatistica[Pedra.Dono].count];
           end;
        end;
        mesa.jogador:=(pedra.dono mod 4)+1;
      end;
      Pedra.Dono:=0;
    end;
    if not (mesa.vencedor=0) then
      somaospontos;
  end; //End não passou
end;
Procedure TForm1.AcresceMesa(EixoY:integer);
var
  x:ShortInt;
begin
  for x:=0 to 27 do
    if TodasAsPecas[x].dono=0 then begin
      TodasAsPecas[x].top:=TodasAsPecas[x].top+EixoY;
      TodasAsPecas[x].Destino.Y:=TodasAsPecas[x].Destino.Y+EixoY;
    end;
end;

procedure TForm1.SpeedButton1Click(Sender: TObject);
begin
  close;
end;

initialization
{$i dominoes.lrs}

end.

