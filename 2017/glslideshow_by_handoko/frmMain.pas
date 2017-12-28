unit frmMain;

{$mode objfpc}{$H+}

interface

uses
  Forms, Graphics, OpenGLContext, Dialogs, Buttons, ExtCtrls, StdCtrls,
  ComCtrls, math, uniGraphics;

type

  { TForm1 }

  TForm1 = class(TForm)
    PlayPause: TButton;
    Background: TButton;
    Proportional: TCheckBox;
    ColorDialog1: TColorDialog;
    OpenDialog1: TOpenDialog;
    GLOutput: TOpenGLControl;
    Panel1: TPanel;
    Timer1: TTimer;
    Speed: TTrackBar;
    Transition1: TComboBox;
    LoadImage1: TBitBtn;
    LoadImage2: TBitBtn;
    LoadImage3: TBitBtn;
    LoadImage4: TBitBtn;
    LoadImage5: TBitBtn;
    Transition2: TComboBox;
    Transition3: TComboBox;
    Transition4: TComboBox;
    Transition5: TComboBox;
    procedure PlayPauseClick(Sender: TObject);
    procedure BackgroundClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure GLOutputPaint(Sender: TObject);
    procedure LoadImageClick(Sender: TObject);
    procedure ProportionalChange(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure SpeedChange(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  Form1:        TForm1;
  SlideImages:  array [1..10] of TBitmap;

const
  SlideCount:   Byte    = 0;
  PlayerSlide:  Byte    = 0;
  PlayerStep:   Byte    = 0; // 100 means 100% completed and move to next slide
  PlayerStill:  Integer = 0;
  StillDuration         = 100;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.LoadImageClick(Sender: TObject);
var
  AJpg:  TJPEGImage;
  Index: Byte;
begin
  if not(OpenDialog1.Execute) then Exit;

  Index := (Sender as TBitBtn).Tag;

  // Load new or replace
  if (Index > SlideCount) then
    begin
      SlideImages[Index] := TBitmap.Create;
      Inc(SlideCount);
    end
  else
    SlideImages[Index].Clear;

  // Load the jpeg image
  AJpg := TJpegImage.Create;
  AJpg.LoadFromFile(OpenDialog1.FileName);
  SlideImages[Index].Assign(AJpg);
  UpdateButtonImage(Sender as TBitBtn, SlideImages[Index]);
  BGRAtoRGBA(SlideImages[Index]);
  AJpg.Free;

  // Show next available button
  case Index of
    1: begin
        LoadImage2.Visible  := True;
        Transition1.Visible := True;
        Transition2.Visible := True;
       end;
    2: begin
        LoadImage3.Visible := True;
        Transition3.Visible := True;
       end;
    3: begin
        LoadImage4.Visible := True;
        Transition4.Visible := True;
       end;
    4: begin
        LoadImage5.Visible := True;
        Transition5.Visible := True;
       end;
  end;
end;

procedure TForm1.ProportionalChange(Sender: TObject);
begin
  EnableProportionalCalculation(Proportional.Checked);
end;

procedure TForm1.Timer1Timer(Sender: TObject);
begin
  // Delay, wait until Still reach StillDuration
  if (PlayerStep = 0) then
    begin
      Inc(PlayerStill);
      if (PlayerStill >= StillDuration) then Inc(PlayerStep);
    end
  else
    Inc(PlayerStep);

  GLOutput.Paint;

  // Back to first slide
  if (PlayerStep >= 100) then
    begin
      PlayerStep  := 0;
      PlayerStill := 0;
      Inc(PlayerSlide);
      if (PlayerSlide > SlideCount) then PlayerSlide := 1;
    end;
end;

procedure TForm1.SpeedChange(Sender: TObject);
begin
  case Speed.Position of
    1: Timer1.Interval := 150;
    2: Timer1.Interval := 100;
    3: Timer1.Interval := 64;
    4: Timer1.Interval := 32;
    5: Timer1.Interval := 16;
  end;
end;

procedure TForm1.PlayPauseClick(Sender: TObject);
begin
  if (SlideCount <= 0) then Exit;
  Timer1.Enabled := not(Timer1.Enabled);
  if (Timer1.Enabled) then
    begin
      PlayPause.Caption := 'Stop';
      PlayPause.Hint := 'Click to stop';
    end
  else
    begin
      PlayPause.Caption := 'Play';
      PlayPause.Hint := 'Click to play'
    end;
end;

procedure TForm1.BackgroundClick(Sender: TObject);
var
  R, G, B, L: Real;
begin
  if not(ColorDialog1.Execute) then Exit;
  with ColorDialog1 do
  begin
    Background.Color := Color;
    R := Red(Color) / 255;
    G := Green(Color) / 255;
    B := Blue(Color) / 255;
    SetBackgroundColor(Color);
  end;
  L := (Max (R, Max (G, B)) + Min (R, Min (G, B))) / 2;
  if (L < 0.5) then
    Background.Font.Color := clWhite
  else
    Background.Font.Color := clBlack;
end;

procedure TForm1.FormCreate(Sender: TObject);
var
  i: Byte;
begin
  for i := Low(GetTransitionList) to High(GetTransitionList) do
  begin
    Transition1.Items.Add(GetTransitionList[i]);
    Transition2.Items.Add(GetTransitionList[i]);
    Transition3.Items.Add(GetTransitionList[i]);
    Transition4.Items.Add(GetTransitionList[i]);
    Transition5.Items.Add(GetTransitionList[i]);
  end;
  Transition1.ItemIndex := 1;
  Transition2.ItemIndex := 1;
  Transition3.ItemIndex := 1;
  Transition4.ItemIndex := 1;
  Transition5.ItemIndex := 1;
  OutputAspectResized(GLOutput.Width/GLOutput.Height);
end;

procedure TForm1.GLOutputPaint(Sender: TObject);
const
  SceneCx: TBitmap = nil; // Order: Cx <-- Bx <-- A <-- B <-- C
  SceneBx: TBitmap = nil;
  SceneA:  TBitmap = nil; // Current slide
  SceneB:  TBitmap = nil; // Next slide
  SceneC:  TBitmap = nil;

var
  Transition, Z:  Byte;
begin
  if not(Timer1.Enabled) then Exit;

  // Get scenes' image
  if (PlayerSlide > 2) then
    SceneBx := SlideImages[PlayerSlide-2]
  else
    SceneBx := SlideImages[SlideCount];
  if (PlayerSlide > 1) then
    SceneBx := SlideImages[PlayerSlide-1]
  else
    SceneBx := SlideImages[SlideCount];
  if (PlayerSlide > 0) then
    SceneA := SlideImages[PlayerSlide]
  else
    SceneA := nil;
  if (PlayerSlide >= SlideCount) then
    begin
      SceneB := SlideImages[1];
      if (SlideCount <= 1) then
        SceneC := SlideImages[1]
      else
        Scenec := SlideImages[2];
    end
  else
    begin
      SceneB := SlideImages[PlayerSlide+1];
      if (PlayerSlide+1 >= SlideCount) then
        SceneC := SlideImages[1]
      else
        SceneC := SlideImages[PlayerSlide+2];
    end;

  // Get transition type
  Z := PlayerSlide;
  if (Z >= SlideCount) or (Z = 0) then Z := 5; // Todo: avoid hardcoding "5"
  case Z of
    1: Transition := Transition2.ItemIndex;
    2: Transition := Transition3.ItemIndex;
    3: Transition := Transition4.ItemIndex;
    4: Transition := Transition5.ItemIndex;
    5: Transition := Transition1.ItemIndex;
  end;

  // Do the actual painting and animation
  PrepareOpenGLForPainting;
  case Transition of
     0: AB_SlideInUp(SceneA, SceneB, PlayerStep);
     1: AB_SlideInDown(SceneA, SceneB, PlayerStep);
     2: AB_SlideInLeft(SceneA, SceneB, PlayerStep);
     3: AB_SlideInRight(SceneA, SceneB, PlayerStep);
     4: AB_FadeToBackground(SceneA, SceneB, PlayerStep);
     5: AB_ScaleDown(SceneA, SceneB, PlayerStep);
     6: AB_ScaleUp(SceneA, SceneB, PlayerStep);
     7: AB_RibbonLeft(SceneBx, SceneA, SceneB, SceneC, PlayerStep);
     8: AB_RibbonUp(SceneBx, SceneA, SceneB, SceneC, PlayerStep);
     9: AB_RotateCW(SceneA, SceneB, PlayerStep);
    10: AB_RotateCCW(SceneA, SceneB, PlayerStep);
  end;

  GLOutput.SwapBuffers;
end;

end.

