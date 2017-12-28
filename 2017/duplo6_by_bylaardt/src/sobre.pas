unit sobre;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  StdCtrls, ComCtrls, Buttons, strutils;

resourcestring
  Female='Female';
  Male='Male';
  Apresentation =
   '          This four player partnership game are played with a double six dominoes set randomly disposed, 7 tiles each player and 2 partners at each end of the deck.'+LineEnding+
   '          The game starts with the double six bone [6-6], proceeding counterclockwise until the round ends when one player plays her last stone, or by block.'+LineEnding+
   '          The winners score the pip total of the losing team''s unplayed tiles. If the teams have equal pip totals in a blocked game the team that played the last tile loses.'+LineEnding+
   '          The first team that achieves a cumulative score of 100 points or more wins the match.';

type

  { TForm3 }

  TForm3 = class(TForm)
    BitBtn1: TBitBtn;
    ComboBox1: TComboBox;
    Edit1: TEdit;
    Image1: TImage;
    Image2: TImage;
    Image3: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Memo1: TMemo;
    TreeView1: TTreeView;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure TreeView1Change(Sender: TObject; Node: TTreeNode);
  private
    { private declarations }
  public
    { public declarations }
    homefile:String;
    cor:integer;
    som,papeldefundo:Boolean;
  end;

var
  Form3: TForm3;

implementation

{$R *.lfm}

{ TForm3 }

procedure TForm3.TreeView1Change(Sender: TObject; Node: TTreeNode);
begin
end;

procedure TForm3.FormCreate(Sender: TObject);
var
  texto:Textfile;
  linha:String;
begin
  {$IFDEF windows}
     Homefile:=GetUserDir+'bylaardt'+DirectorySeparator;
  {$ELSE}
     Homefile:=GetUserDir+'.bylaardt'+DirectorySeparator;
  {$ENDIF}
  memo1.Lines.Text:= apresentation;
  combobox1.Items.add(Female);
  combobox1.Items.add(Male);
  if not DirectoryExistsUTF8(Homefile) then mkdir(Homefile);
  if FileExists(homefile+'duplo6.cfg') then begin
     AssignFile(texto,homefile+'duplo6.cfg');
     Reset(texto);
     ReadLn(texto,linha);
     edit1.Text:=linha;
     ReadLn(texto,linha);
     try
       combobox1.ItemIndex:=strtoint(linha);
     except
       combobox1.ItemIndex:=0;
     end;
     try
       ReadLn(texto,linha);
       som:=not(linha='0');
       ReadLn(texto,linha);
       papeldefundo:=not(linha='0');
       ReadLn(texto,linha);
       cor:=StrToIntDef(linha,7);
     except
       som:=true;
       papeldefundo:=true;
       cor:=7;
     end;
     CloseFile(texto);
  end else begin
    som:=true;
    papeldefundo:=true;
    cor:=7;
    combobox1.ItemIndex:=0;
    Edit1.Text:=GetEnvironmentVariableUTF8('USER');
  end;
end;

procedure TForm3.FormDestroy(Sender: TObject);
var
  texto:Textfile;
begin
  assignfile(texto,homefile+'duplo6.cfg');
  Rewrite(texto);
  WriteLn(texto,edit1.Text);
  WriteLn(texto,inttostr(combobox1.ItemIndex));
  WriteLn(texto,ifthen(som,'1','0'));
  WriteLn(texto,ifthen(papeldefundo,'1','0'));
  WriteLn(texto,inttostr(cor));
  closefile(texto);
end;

procedure TForm3.FormShow(Sender: TObject);
begin
end;


end.

