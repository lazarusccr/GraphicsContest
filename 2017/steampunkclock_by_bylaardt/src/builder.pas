unit builder;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LazFileUtils, Forms, Controls, Graphics, Dialogs, ComCtrls,
  ExtCtrls, Buttons, Menus, ExtDlgs, lazutf8, pb_clock,math,propedit,builderoptions;

type

  { TBuilderform }

  TBuilderform = class(TForm)
    cd: TColorDialog;
    ImageList1: TImageList;
    MainMenu1: TMainMenu;
    MenuItem1: TMenuItem;
    MenuItem10: TMenuItem;
    MenuItem11: TMenuItem;
    MenuItem12: TMenuItem;
    MenuItem13: TMenuItem;
    MenuItem14: TMenuItem;
    MenuItem15: TMenuItem;
    MenuItem16: TMenuItem;
    MenuItem17: TMenuItem;
    MenuItem18: TMenuItem;
    MenuItem2: TMenuItem;
    MenuItem3: TMenuItem;
    MenuItem4: TMenuItem;
    MenuItem5: TMenuItem;
    MenuItem6: TMenuItem;
    MenuItem7: TMenuItem;
    MenuItem8: TMenuItem;
    MenuItem9: TMenuItem;
    op: TOpenPictureDialog;
    Oc: TOpenDialog;
    PopupMenu1: TPopupMenu;
    sc: TSaveDialog;
    TreeView1: TTreeView;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure DoEdit(Node:TTreeNode);
    procedure Drawitem(Node: TTreeNode);
    procedure MenuItem10Click(Sender: TObject);
    procedure MenuItem12Click(Sender: TObject);
    procedure MenuItem13Click(Sender: TObject);
    procedure MenuItem14Click(Sender: TObject);
    procedure MenuItem16Click(Sender: TObject);
    procedure MenuItem17Click(Sender: TObject);
    procedure MenuItem18Click(Sender: TObject);
    procedure MenuItem1Click(Sender: TObject);
    procedure MenuItem4Click(Sender: TObject);
    procedure MenuItem7Click(Sender: TObject);
    procedure MenuItem8Click(Sender: TObject);
    procedure MenuItem9Click(Sender: TObject);
    procedure PaintAdjusted(Sender: TObject;MyImage:TPortableNetworkGraphic;ajustable:boolean=true;X:Integer=0;y:integer=0);
    procedure Panel1Paint(Sender: TObject);
    procedure LoadTreeview1;
    procedure PopupMenu1Close(Sender: TObject);
    procedure PopupMenu1Popup(Sender: TObject);
    procedure ScrollBox1Paint(Sender: TObject);
    procedure TreeView1AdvancedCustomDraw(Sender: TCustomTreeView;const ARect: TRect; Stage: TCustomDrawStage; var DefaultDraw: Boolean);
    procedure TreeView1MouseDown(Sender: TObject; Button: TMouseButton;Shift: TShiftState; X, Y: Integer);
    procedure TreeView1MouseLeave(Sender: TObject);
    procedure TreeView1MouseMove(Sender: TObject; Shift: TShiftState; X,Y: Integer);
    procedure TreeView1MouseUp(Sender: TObject; Button: TMouseButton;Shift: TShiftState; X, Y: Integer);
  private
    BGImage:TPortableNetworkGraphic;
    MouseDownNode:TTreeNode;
    ColorFolderTXT:TColor;
    modeldir:TFilename;
    { private declarations }
  public
    { public declarations }
  end;
Const
  Captionbase = 'Steampunk Clock Builder';
var
  Builderform: TBuilderform;
  cb:TClockBuilder;

implementation

{$R *.lfm}

{ TBuilderform }

procedure TBuilderform.DoEdit(Node:TTreeNode);
Var
  Part:TClockPart;
  newNode:TTreeNode;
begin
    case node.ImageIndex of
     0: begin //EditPart
          PropForm.ActualNode:=Node;
          PropForm.cb:=cb;
          PropForm.addclocpart(TClockPart(Node.Data));
          PropForm.show;
        end;
     1: begin //Open
          screen.Cursor:=crHourGlass;
          cb.clear;
          cb.LoadFromClockFile(modeldir+Node.text);
          sc.FileName:=modeldir+Node.text;
          caption:=Captionbase+': '+ExtractFileNameOnly(sc.FileName);
          LoadTreeview1;
          screen.Cursor:=crDefault;
        end;
     2: begin //NewPart
          if op.Execute then begin
            if FileExistsUTF8(op.FileName) then begin
              part:=TClockPart.Create;
              try
                part.PartName:=ExtractFileNameOnly(op.Filename);
                part.LoadFromFile(op.Filename);
                Part.Angle:=00;
                Part.ImageCenterx:=part.Width div 2;
                Part.ImageCentery:=part.Height div 2;
                Part.MotorCenterx:=500;
                Part.MotorCentery:=500;
                Part.StretchSize:=1;
                Part.MaxAngle:=360;
                Part.MinAngle:=0;
                Part.DayCycles:=1440;
                Part.Interval:=0;
                cb.Add(Part);
                cb.RedoBmp(Part);
                newNode:=TreeView1.Items.AddObject(node,part.PartName,part);
                NewNode.Index:=1;
                newnode.ImageIndex:=0;
                newnode.SelectedIndex:=0;
                PropForm.ActualNode:=newNode;
                PropForm.cb:=cb;
                PropForm.addclocpart(part);
                PropForm.show;
              except
                part.free;
              end;
            end;
          end;
        end;
     end;
end;

procedure TBuilderform.TreeView1AdvancedCustomDraw(Sender: TCustomTreeView;
  const ARect: TRect; Stage: TCustomDrawStage; var DefaultDraw: Boolean);
var
  x:integer;
begin
  if (stage=cdPostPaint) then
    PaintAdjusted(Sender,BGImage);
    if stage=cdPostPaint then begin
      for x:=0 to treeview1.items.Count-1 do
        Drawitem(treeview1.Items[x]);
    end;
end;
procedure TBuilderform.Drawitem(Node: TTreeNode);
var
  NodeRect:TRect;
  imh,iml,tt,tl:Integer;
  png:TPortableNetworkGraphic;
begin
  if node.IsVisible then begin
    Treeview1.Canvas.Font.bold:=node.level=0;
    Noderect:=node.DisplayRect(true);
    imh:=Noderect.Top;
    iml:=Noderect.Left-treeview1.Images.Width-2;
    tl:=NodeRect.Left + 2;
    tt:=NodeRect.Top+(Noderect.Bottom-NodeRect.Top-Treeview1.Canvas.TextHeight(node.text)) div 2;
    treeview1.Images.Draw(TreeView1.Canvas,iml,imh,node.ImageIndex);
    Treeview1.Canvas.Brush.Style:=bsClear;
    if node=MousedownNode then begin
      Treeview1.Canvas.Font.color:=clBlack;
      Treeview1.Canvas.TextOut(tl-2, tt, node.text);

      Treeview1.Canvas.Font.color:=clWhite;
      Treeview1.Canvas.TextOut(tl, tt-2, node.text);

      Treeview1.Canvas.Font.color:=ColorFolderTXT;
      Treeview1.Canvas.TextOut(tl-1, tt-1, node.text);
    end else begin;
      Treeview1.Canvas.Font.color:=ColorFolderTXT;
      Treeview1.Canvas.TextOut(tl, tt, node.text);
    end;
  end;
end;

procedure TBuilderform.MenuItem10Click(Sender: TObject);
var
  Oldfilename:String;
begin
  Oldfilename:=sc.FileName;
  if sc.Execute then begin
    cb.SaveToClockFile(sc.FileName);
    caption:=Captionbase+': '+ExtractFileNameOnly(sc.FileName);
    LoadTreeview1;
  end else sc.FileName:=Oldfilename;
end;

procedure TBuilderform.MenuItem12Click(Sender: TObject);
begin
  close;
end;

procedure TBuilderform.MenuItem13Click(Sender: TObject);
begin

end;

procedure TBuilderform.MenuItem14Click(Sender: TObject);
begin

end;

procedure TBuilderform.MenuItem16Click(Sender: TObject);
begin
  if op.execute then begin
    try
      BGImage.LoadFromFile(op.FileName);
      BGImage.SaveToFile(ExtractFilePath(ParamStrUTF8(0))+DirectorySeparator+'bg.png');
    except
      try
        if FileExists(ExtractFilePath(ParamStrUTF8(0))+DirectorySeparator+'bg.png') then
         BGImage.loadfromFile(ExtractFilePath(ParamStrUTF8(0))+DirectorySeparator+'bg.png');
      except
      end;
    end;
  end;
end;

procedure TBuilderform.MenuItem17Click(Sender: TObject);
begin
  Options.Show;
end;

procedure TBuilderform.MenuItem18Click(Sender: TObject);
begin
  if cd.execute then
     ColorFolderTXT:=cd.Color;
end;

procedure TBuilderform.MenuItem1Click(Sender: TObject);
var
  cp:TClockPart;
begin
  cp:=TClockPart(MousedownNode.data);
  cb.MoveTo(cp,TComponent(sender).Tag);
  MousedownNode.Index:=min(max(MousedownNode.Index-TComponent(sender).Tag,1),MouseDownNode.Parent.Count-1);
end;

procedure TBuilderform.MenuItem4Click(Sender: TObject);
var
  cp:TClockPart;
begin
  cp:=TClockPart(MousedownNode.data);
  cb.Delete(cp);
  MouseDownNode.Delete;
end;

procedure TBuilderform.MenuItem7Click(Sender: TObject);
begin
  cb.clear;
  LoadTreeview1;
  sc.FileName:='';
  caption:=Captionbase;
end;

procedure TBuilderform.MenuItem8Click(Sender: TObject);
begin
  if oc.execute then begin
    screen.Cursor:=crHourGlass;
    cb.clear;
    cb.LoadFromClockFile(oc.FileName);
    sc.FileName:=oc.FileName;
    caption:=Captionbase+': '+ExtractFileNameOnly(sc.FileName);
    LoadTreeview1;
    screen.Cursor:=crDefault;
  end;
end;

procedure TBuilderform.MenuItem9Click(Sender: TObject);
begin
  if length(sc.FileName)=0 then
    MenuItem10Click(MenuItem10)
  else
    cb.SaveToClockFile(sc.FileName);
end;

procedure TBuilderform.TreeView1MouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  oldnode,node:TTreeNode;
  NodeRect:TRect;
begin
  if (ssLeft in Shift) or(ssRight in shift) then begin
    Node:=TreeView1.GetNodeAt(x,y);
    oldnode:=MousedownNode;
    MousedownNode:=nil;
    if (not (node=nil)) then begin
      NodeRect:=node.DisplayRect(True);
      if node.IsVisible and
         (NodeRect.Left-TreeView1.Images.Width-4<x) and
         (NodeRect.Left+canvas.TextWidth(node.Text)+2>x) then
           MousedownNode:=node;
    end;
    if not(oldnode=MousedownNode) then
    treeview1.Invalidate;
    if (not(MousedownNode=nil)) and (ssRight in shift) then
      if MousedownNode.imageindex=0 then PopupMenu1.PopUp;
  end;
end;

procedure TBuilderform.TreeView1MouseLeave(Sender: TObject);
begin
   if not (MousedownNode=nil) then begin
    MousedownNode:=nil;
    Repaint;
  end;
end;

procedure TBuilderform.TreeView1MouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
begin
    TreeView1MouseDown(Sender,mbRight,shift,x,y);
end;

procedure TBuilderform.TreeView1MouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  node:TTreeNode;
  NodeRect:TRect;
begin
  Node:=TreeView1.GetNodeAt(x,y);
  if (not (node=nil)) then begin
    NodeRect:=node.DisplayRect(True);
    if node.IsVisible and
      (NodeRect.Left-TreeView1.Images.Width-4<x) and
      (NodeRect.Left+canvas.TextWidth(node.Text)+2>x) and
      (Node=MousedownNode) then
         DoEdit(node);
  end;
  MousedownNode:=nil;
  treeview1.Invalidate;
end;


procedure TBuilderform.FormCreate(Sender: TObject);
var
  fs:TextFile;
  txtvalue:String;
begin
  caption:=Captionbase;
  DoubleBuffered:=false;
  modeldir:=ExtractFilePath(lazutf8.ParamStrUTF8(0))+'models';
  if not DirectoryExistsUTF8(modeldir) then
    mkdir(modeldir);
  modeldir:=modeldir+DirectorySeparator;
  op.InitialDir:=ExtractFilePath(lazutf8.ParamStrUTF8(0));
  sc.InitialDir:=modeldir;
  sc.FileName:='';
  MouseDownNode:=nil;
  BGImage:=TPortableNetworkGraphic.Create;
  if FileExists(ExtractFilePath(ParamStrUTF8(0))+DirectorySeparator+'bg.png') then
   BGImage.loadfromFile(ExtractFilePath(ParamStrUTF8(0))+DirectorySeparator+'bg.png');

  ColorFolderTXT:=TreeView1.font.Color;

  cb:=TClockBuilder.Create(self);
  cb.Align:=alClient;
  cb.Width:=self.ClientWidth;
  cb.Height:=self.ClientHeight;

  InsertControl(cb);
  if FileExists(ExtractFilePath(ParamStrUTF8(0))+DirectorySeparator+'spc.cfg') then begin
    AssignFile(fs,ExtractFilePath(ParamStrUTF8(0))+DirectorySeparator+'spc.cfg');
    Reset(fs);
    ReadLn(fs,txtvalue);
    cb.TimerInterval:=StrToIntDef(txtvalue,16);
    ReadLn(fs,txtvalue);
    ColorFolderTXT:=StringToColorDef(txtvalue,ColorFolderTXT);
    closefile(fs);
  end;
  loadTreeview1;
end;
procedure TBuilderform.loadtreeview1;
var
  Index:integer;
  cp:TClockPart;
  RootNode:TTreeNode;
  rec:TSearchRec;
  MustExpand:Boolean;
  procedure AddChild(NameChild:String;IconIndex:Integer;Part:TClockPart=nil);
  var
    Node:TTreeNode;
  begin
    if part=nil then
      Node:=Treeview1.Items.AddChild(RootNode,NameChild)
    else
      Node:=Treeview1.Items.AddChildObject(RootNode,NameChild,Part);
    Node.imageindex:=IconIndex;
    Node.SelectedIndex:=IconIndex;
  end;

begin
  MustExpand:=(TreeView1.items.Count=0) or (TreeView1.items.GetFirstNode.Expanded);
  Treeview1.Items.Clear;
  RootNode:=Treeview1.Items.Add(nil,'Models');
  if FindFirstUTF8(modeldir+'/*.spcf',faAnyFile,rec)=0 then
    repeat
      AddChild(rec.Name,1);
    until FindNextUTF8(rec)<>0;
  FindCloseUTF8(rec);
  RootNode:=Treeview1.Items.Add(nil,'Parts');
  AddChild('New part',2);
  for index:= cb.Count-1 downto 0 do
    AddChild(cb[index].PartName,0,cb[index]);
  if MustExpand then
    treeview1.FullExpand
  else
    RootNode.Expand(false);
  MousedownNode:=nil;
end;

procedure TBuilderform.PopupMenu1Close(Sender: TObject);
begin
  MousedownNode:=nil;
end;

procedure TBuilderform.PopupMenu1Popup(Sender: TObject);
begin
  MenuItem1.Enabled:=not(MouseDownNode=nil);
  MenuItem2.Enabled:=MenuItem1.Enabled;
  MenuItem3.Enabled:=MenuItem1.Enabled;
  MenuItem4.Enabled:=MenuItem1.Enabled;
end;

procedure TBuilderform.FormDestroy(Sender: TObject);
var
  fs:TextFile;
begin
  AssignFile(fs,ExtractFilePath(ParamStrUTF8(0))+DirectorySeparator+'spc.cfg');
  rewrite(fs);
  writeln(fs,cb.TimerInterval);
  writeln(fs,ColorToString(ColorFolderTXT));
  closefile(fs);
  BGImage.free;
  cb.Free;
end;

procedure TBuilderform.FormResize(Sender: TObject);
begin
  Application.ProcessMessages;
  cb.RedoAll;
end;

procedure TBuilderform.PaintAdjusted(Sender: TObject;MyImage:TPortableNetworkGraphic;ajustable:boolean=true;X:Integer=0;y:integer=0);
var
  loop1,loop2:integer;
  ajuste:Tpoint;
  h,v:integer;
begin
  if not(MyImage=nil) then begin
    h:=MyImage.Width;
    v:=MyImage.Height;
    if (h>0)and (v>0) then begin
      if ajustable then
         ajuste:=Point((Tcontrol(sender).Left+X) mod h,(Tcontrol(sender).top+Y)  mod v)
      else
        ajuste:=Point(0,0);
      if sender is TScrollingWinControl then begin
        for loop1:=0     to max(TScrollingWinControl(Sender).ClientWidth  + ajuste.x,TScrollingWinControl(Sender).HorzScrollBar.Range) div h do
           for loop2:= 0 to max(TScrollingWinControl(Sender).ClientHeight + ajuste.y,TScrollingWinControl(Sender).VertScrollBar.Range) div v do
              TScrollingWinControl(Sender).Canvas.Draw(loop1*h-ajuste.X,loop2*v-ajuste.Y,MyImage);
      end else if sender is TTreeView then begin
        for loop1:=0     to max(TTreeView(Sender).Width  + ajuste.x,TTreeView(Sender).Width) div h  do
           for loop2:= 0 to max(TTreeView(Sender).Height + ajuste.y,TTreeView(Sender).Height) div v  do
             TTreeView(Sender).Canvas.Draw(loop1*h-ajuste.X,loop2*v-ajuste.Y,MyImage);
      end else if sender is TCustomControl then begin
        for loop1:=0     to max(TCustomControl(Sender).Width  + ajuste.x,TCustomControl(Sender).Width) div h  do
           for loop2:= 0 to max(TCustomControl(Sender).Height + ajuste.y,TCustomControl(Sender).Height) div v  do
             TCustomControl(Sender).Canvas.Draw(loop1*h-ajuste.X,loop2*v-ajuste.Y,MyImage);
      end;
    end;
  end;
end;

procedure TBuilderform.Panel1Paint(Sender: TObject);
begin
  PaintAdjusted(self,BGImage,false);
end;

procedure TBuilderform.ScrollBox1Paint(Sender: TObject);
begin
end;

end.

