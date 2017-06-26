unit propedit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ValEdit,
  ComCtrls, pb_clock;

type

  { TPropForm }

  TPropForm = class(TForm)

    vl: TValueListEditor;
    procedure vlClick(Sender: TObject);
    procedure AddClocPart(Part:TClockPart);
    procedure vlEditingDone(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
    ActualNode:TTreeNode;
    cb:TClockBuilder;
  end;

var
  PropForm: TPropForm;

implementation

{$R *.lfm}

{ TPropForm }

procedure TPropForm.AddClocPart(Part:TClockPart);
begin
  vl.BeginUpdate;
  vl.clear;
  vl.Row := 0;
  vl.InsertRow('Name',part.PartName,true);
  vl.InsertRow('Stretch Factor',FloatToStr(part.StretchSize),true);
  vl.InsertRow('Relative Motor Center',format('%dx%d',[part.MotorCenterx,part.MotorCentery]),true);
  vl.InsertRow('Image Center',format('%dx%d',[part.ImageCenterx,part.ImageCentery]),true);
  vl.InsertRow('Min Angle',IntToStr(part.MinAngle),true);
  vl.InsertRow('Max Angle',IntToStr(part.MaxAngle),true);
  vl.InsertRow('Cycle per day',FloatToStr(part.DayCycles),true);
  vl.InsertRow('Interval',FloatToStr(part.interval),true);
  vl.EndUpdate(true);
end;

procedure TPropForm.vlEditingDone(Sender: TObject);
var
  Part:TClockPart;
  Value:String;
  procedure Settointegers(var value1,value2:integer);
  var
    f1,f2:string;
    position:integer;
  begin
    position:=1;
    f1:='';
    f2:='';
    while (position<=length(value)) and (value[position] in ['0'..'9']) do begin
      f1:=f1+value[position];
      inc(position);
    end;
    while (position<=length(value)) and (not(value[position] in ['0'..'9'])) do
      inc(position);
    while (position<=length(value)) and (value[position] in ['0'..'9']) do begin
      f2:=f2+value[position];
      inc(position);
    end;
    value1:=StrToIntDef(f1,value1);
    value2:=StrToIntDef(f2,value2);
  end;
begin
  Part:=TClockPart(ActualNode.data);
  Value:=vl.Cells[1,vl.row];
  case vl.Cells[0,vl.row] of
    'Name':
      begin
        ActualNode.Text:=Value;
        part.PartName:=Value;
      end;
    'Stretch Factor':
      begin
        Part.StretchSize:=Strtofloatdef(Value,Part.StretchSize);
        value:=floattostr(Part.StretchSize);
      end;
    'Relative Motor Center':
      begin
        Settointegers(part.MotorCenterx,part.MotorCentery);
        value:=format('%dx%d',[part.MotorCenterx,part.MotorCentery]);
      end;
    'Image Center':
      begin
          Settointegers(part.ImageCenterx,part.ImageCentery);
          value:=format('%dx%d',[part.ImageCenterx,part.ImageCentery]);
      end;
    'Min Angle':
      begin
        Part.MinAngle:=StrToIntDef(Value,Part.MinAngle);
        value:=IntToStr(Part.MinAngle);
      end;
    'Max Angle':
      begin
        Part.MaxAngle:=StrToIntDef(Value,Part.MaxAngle);
        value:=IntToStr(Part.MaxAngle);
      end;
    'Cycle per day':
      begin
        Part.DayCycles:=StrToFloatDef(Value,Part.DayCycles);
        value:=FloatToStr(Part.DayCycles);
      end;
    'Interval':
      begin
        Part.Interval:=StrToFloatDef(Value,Part.Interval);
        value:=FloatToStr(Part.Interval);
      end;
  end;
  vl.Cells[1,vl.row]:=Value;
  cb.RedoBmp(part);
end;

procedure TPropForm.vlClick(Sender: TObject);
begin

end;

end.

