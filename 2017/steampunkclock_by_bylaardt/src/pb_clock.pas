unit pb_clock;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Controls, BGRABitmap,BGRABitmapTypes, BGRAGraphicControl, BCTypes,math,ExtCtrls, Forms,LazUTF8,DOM,XMLWrite,XMLRead, Graphics;

type

  TClockPart = class(TPortableNetworkGraphic)
    bmp:TBGRABitmap;
    public
      ImageCenterx,ImageCentery,MotorCenterx,MotorCentery:Integer;
      MinAngle,MaxAngle:Integer;
      Angle,DayCycles,StretchSize,Interval:Extended;
      PartName:String;
      Function ToHex:String;
      Procedure FromHex(Texto:String);
    end;

  TClockBuilder = class(TBGRAGraphicControl)
  private
    FItems: TList;
    Timer1:TTimer;
    ispainting:boolean;
    procedure timer1timer(Sender:TObject);
    procedure SetTimerInterval(Value:Integer);
    function GetTimerInterval:integer;
  protected
    function fcount:Integer;
    function GetPart(Index:Integer):TClockPart;
  public
    MotorReference:Integer;
    procedure clear;
    procedure SaveToClockFile(FileName:Tfilename);
    procedure LoadFromClockFile(FileName:TFilename);
    Constructor Create(AOwner: TComponent); override;
    Destructor Destroy; override;
    procedure Add(Part:TClockPart);
    procedure MoveTo(Part:TClockPart;relativeIndex:integer);
    procedure Delete(Part:TClockPart);
    procedure DeleteByIndex(Index:Integer);
    procedure RedoBmp(cp:TClockPart);
    procedure RedoAll;
    procedure RedrawBitmapContent; override;
    property Count:Integer read fcount;
    property Items[index:integer]:TClockPart read GetPart;default;
    property TimerInterval:Integer read GetTimerInterval write SetTimerInterval;
  end;

implementation

Constructor TClockBuilder.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  MotorReference:=1000;
  ispainting:=false;
  FItems:=Tlist.Create;
  AutoSize:=true;
  timer1:=TTimer.Create(self);
  timer1.OnTimer:=@timer1Timer;
  timer1.Interval:=16;
  timer1.Enabled:=true;
end;
Destructor TClockBuilder.Destroy;
var
  i:Integer;
  cp:TClockPart;
begin
  timer1.Enabled:=false;
  for i:= fitems.count-1 downto 0 do begin
    try
      cp:=TClockPart(FItems[i]);
      try
        cp.bmp.Free;
      except
      end;
      cp.free;
    except
    end;
  end;
  FItems.Free;
  timer1.Free;
  inherited Destroy;
end;
procedure TClockBuilder.SetTimerInterval(Value:Integer);
begin
  timer1.Interval:=Value;
end;
function TClockBuilder.GetTimerInterval:integer;
begin
  result:=timer1.Interval;
end;

procedure TClockBuilder.timer1timer(Sender:TObject);
var
  rangeAngle,index:Integer;
  cp:TClockPart;
  Moment:TDateTime;
begin
  if not ispainting then begin
    ispainting:=true;
    Moment:=now;
    for index:=0 to FItems.Count-1 do begin
      cp:=TClockPart(FItems[index]);
      rangeAngle:=cp.MaxAngle-cp.MinAngle;
      if rangeAngle<>0 then begin
        if cp.interval=0 then begin
          if abs(rangeAngle)=360 then
            cp.Angle:=frac(moment*cp.DayCycles)*rangeAngle+cp.MinAngle
          else begin
            if rangeAngle<0 then rangeAngle:=rangeAngle+360;
            cp.Angle:=cos(frac(moment*cp.DayCycles)*pi*2)*rangeAngle/2+cp.MinAngle+rangeAngle/2;
          end;

        end else begin
          if abs(rangeAngle)=360 then
            cp.Angle:=trunc(frac(moment*cp.DayCycles)*cp.interval)*rangeAngle/cp.interval+cp.MinAngle
          else begin
            if rangeAngle<0 then rangeAngle:=rangeAngle+360;
            cp.Angle:=cos(trunc(frac(moment*cp.DayCycles)*cp.interval)*pi*2)*rangeAngle/cp.interval/2+cp.MinAngle+rangeAngle/2;
          end;
        end;
      end;
    end;
    self.RedrawBitmap;
    Application.ProcessMessages;
    ispainting:=false;
  end;
end;
procedure TClockBuilder.Clear;
var
  index:Integer;
  cp:TClockPart;
begin
  for index:=FItems.Count-1 downto 0 do begin
    cp:=TClockPart(fitems[index]);
    try
      cp.bmp.Free;
    except
    end;
    FItems.Remove(cp);
    cp.free;
  end;
end;

procedure TClockBuilder.SaveToClockFile(FileName:Tfilename);
var
  index:Integer;
  cp:TClockPart;
  XMLDoc:TXMLDocument;
  Root,Element,ElementItems:TDOMElement;
  defdecimal:Char;
  Function XMLRoot(Const XMLName:String):TDOMElement;
  begin
    Result := XMLDoc.CreateElement(Widestring(XMLName));
    XMLDoc.AppendChild(result);
  end;
  Function XMLNode(XMLHeader:TDOMElement;Const XMLName:String):TDOMElement;
  begin
    result := XMLDoc.CreateElement(Widestring(XMLName));
    XMLHeader.AppendChild(result);
  end;
  procedure XMLReg(Node:TDOMElement;Const FieldName,XmlValue:String);
  var
    XmlHedaer: TDOMElement;
    XmlContent: TDOMText;
  begin
      XmlHedaer := XMLDoc.CreateElement(Widestring(FieldName));
      XmlContent := XMLDoc.CreateTextNode(Widestring(XmlValue));
      //XmlHedaer.SetAttribute('FieldType'    ,FieldType);
      XmlHedaer.AppendChild(XmlContent);
      Node.AppendChild(XmlHedaer);
  end;
begin
  screen.Cursor:=crHourGlass;
  timer1.Enabled:=false;
  defdecimal:=DefaultFormatSettings.DecimalSeparator;
  DefaultFormatSettings.DecimalSeparator:='.';
  XMLDoc:=TXMLDocument.Create;
  Root:=XMLRoot('ClockBuider');
  Element:=XMLNode(root,'ClockParts');
  for index:=0 to fitems.Count-1 do begin
     cp:=TClockPart(fitems[index]);
     ElementItems:=XMLNode(Element,'ClockPart');
     XMLReg(ElementItems,'Description',cp.PartName);
     XMLReg(ElementItems,'png',cp.ToHex());
     XMLReg(ElementItems,'ImageCenter',format('%dx%d',[cp.ImageCenterx,cp.ImageCentery]));
     XMLReg(ElementItems,'MotorCenter',format('%dx%d',[cp.MotorCenterx,cp.MotorCentery]));
     XMLReg(ElementItems,'Angles',floattostr(cp.MinAngle)+'to'+floattostr(cp.MaxAngle));
     XMLReg(ElementItems,'DayCycles',floattostr(cp.DayCycles));
     XMLReg(ElementItems,'Interval',floattostr(cp.Interval));
     XMLReg(ElementItems,'StretchFactor',floattostr(cp.StretchSize));
  end;
  WriteXMLFile(XMLDoc,UTF8ToSys(FileName));
  XMLDoc.Free;
  DefaultFormatSettings.DecimalSeparator:=defdecimal;
  timer1.Enabled:=true;
  screen.Cursor:=crDefault;
end;
Function TClockPart.ToHex:String;
Const
  HexSequence='0123456789ABCDEF';
var
  f:file of byte;
  tmp:TFilename;
  ch1:byte;
  x:LongInt;
begin
  tmp:=GetTempFileName+'.png';
  SaveToFile(tmp);
  AssignFile(f,tmp);
  reset(f);
  x:=0;
  result:='';
  while not eof(f) do begin
    inc(x);
    read(f,ch1);
    result:=result+
      HexSequence[ch1 Div 16+1]+
      HexSequence[ch1 Mod 16+1];
    if x mod 128 = 0 then result:=result+'  ';   //to Make XML readable
  end;
  closefile(f);
  deletefile(tmp);
end;

Procedure TClockPart.FromHex(texto:String);
var
  F: file of Char;
  x:integer;
  och1,och2:Integer;
  tmp:TFilename;
begin
  tmp:=GetTempFileName+'.png';
  Angle:=0;
  AssignFile(F,tmp);
  rewrite(f);
  for x:= 1 to (length(Texto) div 2) do begin
    if not( texto[x*2-1] in [#10,#13,#32]) then begin
      och1:=ord(texto[x*2-1])-48;
      och2:=ord(texto[x*2])-48;
      if och1>15 then och1:=och1-7;
      if och2>15 then och2:=och2-7;
      write(f,char(och1*16+och2));
    end;
  end;
  closefile(f);
  LoadFromFile(tmp);
  deletefile(tmp);
end;


procedure TClockBuilder.LoadFromClockFile(FileName:TFilename);
var
  XMLDoc:TXMLDocument;
  DNode,INode,DVariable:TDOMNode;
  Index:Integer;
  cp:TClockPart;
  DNodename,Value:String;
  defdecimal:char;
  procedure DualVariables(Const Content:String;Var Variable1,Variable2:integer;Separator:String);
  begin
    Variable1:=StrToIntDef(copy(Content,1,pos(Separator,Content)-1),0);
    variable2:=StrToIntDef(copy(Content,pos(Separator,Content)+length(Separator),16),0);
  end;
begin
  timer1.Enabled:=false;
  ispainting:=true;
  defdecimal:=DefaultFormatSettings.DecimalSeparator;
  DefaultFormatSettings.DecimalSeparator:='.';
  ReadXMLFile(XMLDoc, LazUTF8.UTF8ToSys(Filename));
  DNode:=XMLDoc.DocumentElement.FirstChild;
  while assigned(DNode) do begin
    case Dnode.NodeName of
      'ClockParts':
        begin
           inode:=Dnode.FirstChild;
           while assigned(INode) do begin
             cp:=TClockPart.Create;
             For index:=0 to INode.ChildNodes.Count-1 do begin
               DVariable:=INode.ChildNodes.Item[index];
               DNodename:=String(DVariable.NodeName);
               Value:=String(DVariable.FirstChild.NodeValue);
               case DNodename of
                 'ImageCenter'  : DualVariables(value,cp.ImageCenterx,cp.ImageCentery,'x');
                 'MotorCenter'  : DualVariables(value,cp.MotorCenterx,cp.MotorCentery,'x');
                 'Angles'       : DualVariables(value,cp.MinAngle,cp.MaxAngle,'to');
                 'DayCycles'    : cp.DayCycles:=StrToFloatDef(Value,0);
                 'Interval'     : cp.Interval:=StrToFloatDef(Value,0);
                 'StretchFactor': cp.StretchSize:=StrToFloatDef(value,1);
                 'png'          : cp.fromhex(value);
                 'Description'  : cp.PartName:=Value;
               end;
             end;
             add(cp);
             inode:=inode.NextSibling;
           end;
        end;
    end;
    DNode:=DNode.NextSibling;
  end;
  XMLDoc.free;
  DefaultFormatSettings.DecimalSeparator:=defdecimal;
  RedoAll;
  ispainting:=false;
  timer1timer(self);
  timer1.Enabled:=true;
end;

Function TClockBuilder.fcount:Integer;
begin
  result:=FItems.Count;
end;
Function TClockBuilder.GetPart(Index:Integer):TClockPart;
begin
  if index<fitems.Count then
    result:=TClockPart(FItems[Index])
  else
    result:=nil;
end;
Procedure TClockBuilder.Add(Part:TClockPart);
begin
  fitems.Add(Part);
end;
Procedure TClockBuilder.MoveTo(Part:TClockPart;RelativeIndex:Integer);
var
  newIndex,Index:integer;
begin
  Index:=fitems.IndexOf(part);
  Newindex:=min(max(0,Index+RelativeIndex),fitems.Count-1);
  if index<>NewIndex then
    fitems.Move(index,newIndex);
end;
Procedure TClockBuilder.Delete(Part:TClockPart);
begin
  try
    part.bmp.free;
  except
  end;
  fitems.Remove(Part);
  part.free;
end;
Procedure TClockBuilder.DeleteByIndex(Index:Integer);
begin
  delete(TClockPart(fitems.Items[Index]));
end;
procedure TClockBuilder.RedoAll;
var
  index:Integer;
begin
  for index:=0 to fitems.Count-1 do
    Redobmp(TClockPart(FItems[index]));
end;

procedure TClockBuilder.ReDoBmp(cp:TClockPart);
var
  Proportional:Extended;
  positionx,positiony:integer;
  oldPNG,
  MyBMP:TBGRABitmap;
  bmpsize:integer;
  tmp:TFilename;
begin
    try
      cp.bmp.Free;
    except
    end;
    bmpsize:=min(width,Height);
    proportional:=min(
                      bmpsize/max(cp.Width ,max(cp.ImageCenterx,cp.Width-cp.ImageCenterx)*2),
                      bmpsize/max(cp.Height,max(cp.ImageCentery,cp.Height-cp.ImageCentery)*2))*cp.StretchSize;
    tmp:=GetTempFileName+'.png';
    cp.SaveToFile(tmp);
    OldPNG:=TBGRABitmap.Create(tmp);
    DeleteFile(tmp);
    MyBMP:=TBGRABitmap(oldPNG.Resample(round(OldPNG.Width*proportional),round(OldPNG.Height*proportional)));
    cp.bmp:=TBGRABitmap.Create(Width,Height,BGRAPixelTransparent);
    positionx:=round(cp.bmp.width/2-cp.imagecenterx*Proportional);
    positiony:=round(cp.bmp.Height/2-cp.imagecentery*Proportional);
    cp.bmp.CanvasBGRA.Draw(positionx,positiony,MyBMP);
    oldpng.Free;
    mybmp.free;
end;

procedure TClockBuilder.RedrawBitmapContent;
var
  index:Integer;
  RedEv:TBGRARedrawEvent;
  cp:TClockPart;
  bmpsize:integer;
begin
  RedEv:=self.OnRedraw;
  self.OnRedraw:=nil;
  bitmap.SetSize(ClientWidth,ClientHeight);
  inherited RedrawBitmapContent;
  bitmap.FillRect(0,0,bitmap.Width,bitmap.Height,BGRA(255,255,255,0),dmSet);
  bmpsize:=min(bitmap.Width,bitmap.Height);
  for index:=0 to fitems.Count-1 do begin
    cp:=TClockPart(FItems[index]);

    Bitmap.PutImageAngle(
      bmpsize*cp.MotorCenterx/MotorReference,
      bmpsize*cp.MotorCentery/MotorReference,
      cp.bmp,
      cp.angle,
      cp.bmp.Width div 2,
      cp.bmp.Height div 2);
  end;
  if Assigned(RedEv) then begin
    self.OnRedraw:=OnRedraw;
    RedEv(self,Bitmap);
  end;
end;

end.

