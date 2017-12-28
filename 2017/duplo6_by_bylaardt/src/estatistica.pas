unit Estatistica;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ValEdit, StdCtrls;

type

  { TEstatisticas }

  TEstatisticas = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    pecas: TListView;
    oponente1: TListView;
    parceiro: TListView;
    oponente2: TListView;
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

implementation

{$R *.lfm}

{ TEstatisticas }

procedure TEstatisticas.FormCreate(Sender: TObject);
var
  x,y:byte;
  lis:TListitem;
begin
  for x:= 0 to 6 do
    for y:= 0 to 6 do begin
      lis:=Pecas.Items.Add;
      lis.caption:=inttostr(x)+' '+inttostr(y);
      lis.SubItems.add('');
    end;
  for x:=0 to 6 do begin
    lis:=oponente1.items.add;
    lis.caption:=inttostr(x);
    for y:=0 to 4 do lis.subitems.add('0');
    lis:=parceiro.items.add;
    lis.caption:=inttostr(x);
    for y:=0 to 4 do lis.subitems.add('0');
    lis:=oponente2.items.add;
    lis.caption:=inttostr(x);
    for y:=0 to 4 do lis.subitems.add('0');
  end;

end;

end.

