unit horas;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Dialogs, Graphics, Controls, ExtCtrls,
  Buttons, StdCtrls, ComCtrls, dateutils, BGRABitmap, BGRABitmapTypes,
  BGRAGradientScanner, pb_clock;
type

  { TForm1 }

  TForm1 = class(TForm)
    ComboBox1: TComboBox;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    ShowGlass: TCheckBox;
    ShowShadow: TCheckBox;
    ScrollBox1: TScrollBox;
    ShowContinuous: TCheckBox;
    Splitter1: TSplitter;
    TrackBar1: TTrackBar;
    TrackBar2: TTrackBar;
    TrackBar3: TTrackBar;
    TrackBar4: TTrackBar;
    TrackBar5: TTrackBar;
    procedure ChangeClockProperties(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    { private declarations }
    MyPB_Clock:TRelogio;
  public
    { public declarations }
  end;


var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  MyPB_Clock:=TRelogio.Create(self);
  MyPB_Clock.Align:=alClient;
  MyPB_Clock.Visible:=true;
  InsertControl(MyPB_Clock);
  TrackBar1.Position:=MyPB_Clock.RoundPercent;
  TrackBar2.Position:=MyPB_Clock.BorderPercent;
  TrackBar3.Position:=MyPB_Clock.DisplayPercent;
  TrackBar4.Position:=MyPB_Clock.MarkPercent;
  TrackBar5.Position:=MyPB_Clock.SeparatorPercent;
  ShowShadow.Checked:=MyPB_Clock.ShowShadow;
  ShowContinuous.Checked:=MyPB_Clock.ShowContinuous;
  ShowGlass.Checked:=MyPB_Clock.ShowGlass;

  ShowShadow.OnChange:=@ChangeClockProperties;
  ShowContinuous.OnChange:=@ChangeClockProperties;
  ShowGlass.OnChange:=@ChangeClockProperties;
  TrackBar1.OnChange:=@ChangeClockProperties;
  TrackBar2.OnChange:=@ChangeClockProperties;
  TrackBar3.OnChange:=@ChangeClockProperties;
  TrackBar4.OnChange:=@ChangeClockProperties;
  TrackBar5.OnChange:=@ChangeClockProperties;
  ComboBox1.OnChange:=@ChangeClockProperties;
end;

procedure TForm1.ChangeClockProperties(Sender: TObject);
begin
  MyPB_Clock.ShowShadow       := ShowShadow.Checked;
  MyPB_Clock.ShowContinuous   := ShowContinuous.Checked;
  MyPB_Clock.ShowGlass        := ShowGlass.Checked;
  MyPB_Clock.RoundPercent     :=TrackBar1.Position;
  MyPB_Clock.BorderPercent    :=TrackBar2.Position;
  MyPB_Clock.DisplayPercent   :=TrackBar3.Position;
  MyPB_Clock.MarkPercent      :=TrackBar4.Position;
  MyPB_Clock.SeparatorPercent :=TrackBar5.Position;
  case ComboBox1.Text of
    'Roman'   : MyPB_Clock.NumericMode:=nmRoman;
    'Catholic': MyPB_Clock.NumericMode:=nmCatholic;
  else
     MyPB_Clock.NumericMode:=nmDecimal;
  end;
  MyPB_Clock.reset;
  MyPB_Clock.Repaint;
end;

end.

