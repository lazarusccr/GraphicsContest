unit builderoptions;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, ComCtrls;

type

  { TOptions }

  TOptions = class(TForm)
    Label1: TLabel;
    TrackBar1: TTrackBar;
    procedure FormCreate(Sender: TObject);
    procedure TrackBar1Change(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Options: TOptions;


implementation

uses builder;
{$R *.lfm}

{ TOptions }

procedure TOptions.TrackBar1Change(Sender: TObject);
begin
  cb.TimerInterval:=1000 div TrackBar1.Position;
end;

procedure TOptions.FormCreate(Sender: TObject);
begin
  if assigned(cb) then begin
    TrackBar1.Position:=1000 div cb.TimerInterval;
  end;
end;

end.

