unit placar_geral;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs,
  Buttons, Grids, StdCtrls, ExtCtrls,LCLTranslator;

Resourcestring
tr_PlayerName = 'Player';
tr_We='We';
tr_Them='Them';
tr_total='Total';

type

  { TForm2 }

  TForm2 = class(TForm)
    BitBtn1: TBitBtn;
    Label1: TLabel;
    Panel1: TPanel;
    StringGrid1: TStringGrid;
    procedure BitBtn1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure StringGrid1DrawCell(Sender: TObject; aCol, aRow: Integer;
      aRect: TRect; aState: TGridDrawState);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form2: TForm2;

implementation

{$R *.lfm}

{ TForm2 }

procedure TForm2.BitBtn1Click(Sender: TObject);
begin
  close;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  Label1.Caption:='0 x 0';
  StringGrid1.Columns[1].Title.Caption:=tr_PlayerName;
  StringGrid1.Columns[2].Title.Caption:=tr_We;
  StringGrid1.Columns[3].Title.Caption:=tr_Them;
  StringGrid1.Cells[0,1]:='';
  StringGrid1.Cells[1,1]:=tr_total;
  StringGrid1.Cells[2,1]:='-';
  StringGrid1.Cells[3,1]:='-';
end;

procedure TForm2.StringGrid1DrawCell(Sender: TObject; aCol, aRow: Integer;
  aRect: TRect; aState: TGridDrawState);
begin
  if arow=StringGrid1.RowCount-1 then begin
     StringGrid1.canvas.font.Style:=[fsbold];
  end;
  StringGrid1.DefaultDrawCell(acol,arow,arect,[]);
end;

end.

