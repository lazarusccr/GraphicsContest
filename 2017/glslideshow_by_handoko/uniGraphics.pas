unit uniGraphics;

{$mode objfpc}{$H+}

interface

uses
  Classes, Buttons, Graphics, LCLType, GL;

const
  GetTransitionList: array [1..11] of string =
    ('Slide In: Up',
    'Slide In: Down',
    'Slide In: Left',
    'Slide In: Right',
    'Fade To Background',
    'Scale: Down',
    'Scale: Up',
    'Ribbon: Left',
    'Ribbon: Up',
    'Rotate: CW',
    'Rotate: CCW');

// Proportionally stretch the bitmap and show it to the bitbtn
procedure UpdateButtonImage(Button: TBitBtn; Image: TBitmap);

// Swap bitmap's blue and red channels
procedure BGRAtoRGBA(Image: TBitmap);

// Change background color
procedure SetBackgroundColor(NewColor: TColor);

// Turn on/off proportional calculation
procedure EnableProportionalCalculation(YesNo: Boolean);

// Keep the width/height aspect needed for proportional calculation
procedure OutputAspectResized(NewAspect: Single);

// Prepare OpenGL settings for transition
procedure PrepareOpenGLForPainting;

// AB Transitions. A --> B: A - still scene, B - incomming scene, 100 steps
procedure AB_SlideInUp(A, B: TBitmap; Step: Integer);
procedure AB_SlideInDown(A, B: TBitmap; Step: Integer);
procedure AB_SlideInLeft(A, B: TBitmap; Step: Integer);
procedure AB_SlideInRight(A, B: TBitmap; Step: Integer);
procedure AB_FadeToBackground(A, B: TBitmap; Step: Integer);
procedure AB_ScaleDown(A, B: TBitmap; Step: Integer);
procedure AB_ScaleUp(A, B: TBitmap; Step: Integer);
procedure AB_RibbonLeft(Bx, A, B, C: TBitmap; Step: Integer);
procedure AB_RibbonUp(Bx, A, B, C: TBitmap; Step: Integer);
procedure AB_RotateCW(A, B: TBitmap; Step: Integer);
procedure AB_RotateCCW(A, B: TBitmap; Step: Integer);

implementation

var
  BackgroundR:  Byte    = 0;
  BackgroundG:  Byte    = 0;
  BackgroundB:  Byte    = 0;
  Proportional: Boolean = False;
  OutputAspect: Single  = 1;

procedure UpdateButtonImage(Button: TBitBtn; Image: TBitmap);
var
  ThumbNail: TBitmap;
  Rect:      TRect;
begin
  ThumbNail := TBitmap.Create;
  Thumbnail.Assign(Image);

  // Proportional resize the image
  Rect.Left := 0;
  Rect.Top  := 0;
  if (ThumbNail.Width > ThumbNail.Height) then
    begin
      Rect.Right  := Button.Width;
      Rect.Bottom := (Button.Width * ThumbNail.Height) div ThumbNail.Width;
    end
  else
    begin
     Rect.Bottom := Button.Height;
     Rect.Right  := (Button.Height * ThumbNail.Width) div ThumbNail.Height;
    end;
  ThumbNail.Canvas.StretchDraw(Rect, ThumbNail);
  ThumbNail.Width  := Rect.Right;
  ThumbNail.Height := Rect.Bottom;

  // Update the button's image
  Button.Caption := '';
  Button.Glyph.Clear;
  Button.Glyph.Assign(ThumbNail);

  ThumbNail.Free;
end;

procedure BGRAtoRGBA(Image: TBitmap);
var
  ScanData: PRGBQuad;
  X, Y:     Integer;
  Temp:     Byte;
begin
  with Image do
  begin
    for y := (Height - 1) downto 1 do
    begin
      ScanData := ScanLine[Y];
      for X := 0 to Width do
      begin
        Temp := ScanData^.rgbBlue;
        ScanData^.rgbBlue := ScanData^.rgbRed;
        ScanData^.rgbRed := Temp;
        Inc(ScanData);
      end;
    end;
  end;
end;

procedure SetBackgroundColor(NewColor: TColor);
begin
  BackgroundR := Red(NewColor);
  BackgroundG := Green(NewColor);
  BackgroundB := Blue(NewColor);
end;

procedure EnableProportionalCalculation(YesNo: Boolean);
begin
  Proportional := YesNo;
end;

procedure OutputAspectResized(NewAspect: Single);
begin
  OutputAspect := NewAspect;
end;

procedure PrepareOpenGLForPainting;
begin
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_LINEAR);
  glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_LINEAR);
  glClearColor(BackgroundR/255, BackgroundG/255, BackgroundB/255, 1);
  glClear(GL_COLOR_BUFFER_BIT or GL_DEPTH_BUFFER_BIT);
  glLoadIdentity;
end;

procedure DrawBasicRectangle;
begin
  glBegin(GL_QUADS);
    glVertex3f(-1, -1, 0);
    glVertex3f( 1, -1, 0);
    glVertex3f( 1,  1, 0);
    glVertex3f(-1,  1, 0);
  glEnd;
end;

procedure DrawTexturedRectangle;
begin
  glColor3ub(255, 255, 255);
  glEnable(GL_TEXTURE_2D);
  glBegin(GL_QUADS);
    glTexCoord2f(0, 1); glVertex3f(-1, -1, 0);
    glTexCoord2f(1, 1); glVertex3f( 1, -1, 0);
    glTexCoord2f(1, 0); glVertex3f( 1,  1, 0);
    glTexCoord2f(0, 0); glVertex3f(-1,  1, 0);
  glEnd;
end;

procedure DrawNonTexturedRectangle;
begin
  glDisable(GL_TEXTURE_2D);
  glColor3ub(BackgroundR, BackgroundG, BackgroundB);
  DrawBasicRectangle;
end;

procedure DrawScene(Image: TBitmap);
var
  ImageAspect: Single;
begin
  if (Image = nil) then Exit;

  // Proportional scaling image
  if (Proportional) then
    begin
      ImageAspect := Image.Width/Image.Height;
      if (ImageAspect < OutputAspect) then  // Fill right and left edges
        glScalef(ImageAspect/OutputAspect, 1, 1)
      else
        if (ImageAspect > OutputAspect) then // Fill top and buttom edges
          glScalef(1, OutputAspect/ImageAspect, 1);
    end;

  glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA, Image.Width, Image.Height, 0,
    GL_RGBA, GL_UNSIGNED_BYTE, Image.RawImage.Data);
  DrawTexturedRectangle;
  glLoadIdentity;
end;

procedure AB_SlideInUp(A, B: TBitmap; Step: Integer);
begin
  DrawScene(A);
  glTranslatef(0, Step/50-2, 0);
  DrawNonTexturedRectangle;
  DrawScene(B);
end;

procedure AB_SlideInDown(A, B: TBitmap; Step: Integer);
begin
  DrawScene(A);
  glTranslatef(0, (100-Step)/50, 0);
  DrawNonTexturedRectangle;
  DrawScene(B);
end;

procedure AB_SlideInLeft(A, B: TBitmap; Step: Integer);
begin
  DrawScene(A);
  glTranslatef((100-Step)/50, 0, 0);
  DrawNonTexturedRectangle;
  DrawScene(B);
end;

procedure AB_SlideInRight(A, B: TBitmap; Step: Integer);
begin
  DrawScene(A);
  glTranslatef(Step/50-2, 0, 0);
  DrawNonTexturedRectangle;
  DrawScene(B);
end;

procedure AB_FadeToBackground(A, B: TBitmap; Step: Integer);
var
  Alpha: Single;
begin
  glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

  // Fade to black
  if (Step <= 50) then
    begin
      DrawScene(A);
      Alpha := Step/50;
      glEnable(GL_BLEND);
      glDisable(GL_TEXTURE_2D);
      glColor4f(BackgroundR/255, BackgroundG/255, BackgroundB/255, Alpha);
      DrawBasicRectangle;
    end
  else
    // Appearing
    begin
      DrawNonTexturedRectangle;
      DrawScene(B);
      Alpha := 1 - (Step-50)/50;
      glEnable(GL_BLEND);
      glDisable(GL_TEXTURE_2D);
      glColor4f(BackgroundR/255, BackgroundG/255, BackgroundB/255, Alpha);
      DrawBasicRectangle;
    end;

    glDisable(GL_BLEND);
end;

procedure AB_ScaleDown(A, B: TBitmap; Step: Integer);
var
  Scale: Single;
begin
  DrawScene(B);
  Scale := (100-Step)/100;
  glScalef(Scale, Scale, Scale);
  DrawNonTexturedRectangle;
  DrawScene(A);
end;

procedure AB_ScaleUp(A, B: TBitmap; Step: Integer);
var
  Scale: Single;
begin
  DrawScene(A);
  Scale := Step/100;
  glScalef(Scale, Scale, Scale);
  DrawNonTexturedRectangle;
  DrawScene(B);
end;

procedure AB_RibbonLeft(Bx, A, B, C: TBitmap; Step: Integer);
var
  Scale, XOffset: Single;
begin
  if (A = nil) then A := Bx;
  if (Step <= 30) then // Shrink
    begin
      glTranslatef(-0.66, 0, 0);
      glScalef(0.3, 0.3, 0.3);
      DrawScene(Bx);
      glScalef(0.3, 0.3, 0.3);
      DrawScene(A);
      glTranslatef(0.66, 0, 0);
      glScalef(0.3, 0.3, 0.3);
      DrawScene(B);
      Scale := (30-Step)/30*0.67+0.33;
      glScalef(Scale, Scale, Scale);
      DrawNonTexturedRectangle;
      DrawScene(A);
    end
  else
    if (Step <= 70) then // Slide
      begin
        XOffset := (30-Step)/40*0.66;
        glTranslatef(XOffset-0.66, 0, 0);
        glScalef(0.3, 0.3, 0.3);
        DrawScene(Bx);
        glTranslatef(XOffset, 0, 0);
        glScalef(0.3, 0.3, 0.3);
        DrawScene(A);
        glTranslatef(XOffset+0.66, 0, 0);
        glScalef(0.3, 0.3, 0.3);
        DrawScene(B);
        glTranslatef(XOffset+1.32, 0, 0);
        glScalef(0.3, 0.3, 0.3);
        DrawScene(C);
      end
    else
      begin // Grow
        glTranslatef(-0.66, 0, 0);
        glScalef(0.3, 0.3, 0.3);
        DrawScene(A);
        glScalef(0.3, 0.3, 0.3);
        DrawScene(B);
        glTranslatef(0.66, 0, 0);
        glScalef(0.3, 0.3, 0.3);
        DrawScene(C);
        Scale := (Step-70)/30*0.67+0.33;
        glScalef(Scale, Scale, Scale);
        DrawNonTexturedRectangle;
        DrawScene(B);
      end;
end;

procedure AB_RibbonUp(Bx, A, B, C: TBitmap; Step: Integer);
var
  Scale, YOffset: Single;
begin
  if (A = nil) then A := Bx;
  if (Step <= 30) then // Shrink
    begin
      glTranslatef(0, 0.66, 0);
      glScalef(0.3, 0.3, 0.3);
      DrawScene(Bx);
      glScalef(0.3, 0.3, 0.3);
      DrawScene(A);
      glTranslatef(0, -0.66, 0);
      glScalef(0.3, 0.3, 0.3);
      DrawScene(B);
      Scale := (30-Step)/30*0.67+0.33;
      glScalef(Scale, Scale, Scale);
      DrawNonTexturedRectangle;
      DrawScene(A);
    end
  else
    if (Step <= 70) then // Slide
      begin
        YOffset := (Step-30)/40*0.66;
        glTranslatef(0, YOffset-1.32, 0);
        glScalef(0.3, 0.3, 0.3);
        DrawScene(C);
        glTranslatef(0, YOffset-0.66, 0);
        glScalef(0.3, 0.3, 0.3);
        DrawScene(B);
        glTranslatef(0, YOffset, 0);
        glScalef(0.3, 0.3, 0.3);
        DrawScene(A);
        glTranslatef(0, YOffset+0.66, 0);
        glScalef(0.3, 0.3, 0.3);
        DrawScene(Bx);
      end
    else
      begin // Grow
        glTranslatef(0, 0.66, 0);
        glScalef(0.3, 0.3, 0.3);
        DrawScene(A);
        glScalef(0.3, 0.3, 0.3);
        DrawScene(B);
        glTranslatef(0, -0.66, 0);
        glScalef(0.3, 0.3, 0.3);
        DrawScene(C);
        Scale := (Step-70)/30*0.67+0.33;
        glScalef(Scale, Scale, Scale);
        DrawNonTexturedRectangle;
        DrawScene(B);
      end;
end;

procedure AB_RotateCW(A, B: TBitmap; Step: Integer);
var
  Scale: Single;
begin
  DrawScene(A);
  Scale := Step/100;
  glScalef(Scale, Scale, Scale);
  glRotatef(180-Scale*180, 0, 0, 1);
  DrawNonTexturedRectangle;
  DrawScene(B);
end;

procedure AB_RotateCCW(A, B: TBitmap; Step: Integer);
var
  Scale: Single;
begin
  DrawScene(A);
  Scale := Step/100;
  glScalef(Scale, Scale, Scale);
  glRotatef(180+Scale*180, 0, 0, 1);
  DrawNonTexturedRectangle;
  DrawScene(B);
end;

end.

