


function BGRAPixelToHex(aColor: TBGRAPixel): string;
begin
 Result := inttoHex(aColor.red,2)+inttoHex(aColor.green,2)+inttoHex(aColor.blue,2)+inttoHex(aColor.alpha,2);
end;


function HexToBGRAPixel(const s: string): TBGRAPixel;
var ss: string;
begin
 ss := copy( s, 1, 2);
 Result.red:=Hex2Dec(ss);
 ss := copy( s, 3, 2);
 Result.green:=Hex2Dec(ss);
 ss := copy( s, 5, 2);
 Result.blue:=Hex2Dec(ss);
 ss := copy( s, 7, 2);
 Result.alpha:=Hex2Dec(ss);
end;

function Text2Bitmap ( aCaption: UTF8String; FontName:string; FontHeight:integer; Style:TFontStyles; FontColor : TBGRAPixel;
                  OutLineColor: TBGRAPixel; OutLineWidth:single;
                  ShadowColor: TBGRAPixel; ShadowOffsetX, ShadowOffsetY, ShadowRadius: integer;
                  aBackGroundTexture: TBGRABitmap) : TBGRABitmap ;
var r:single;
    renderer: TBGRATextEffectFontRenderer;
begin
 renderer:= TBGRATextEffectFontRenderer.Create;
 renderer.ShadowVisible := ShadowColor<>BGRAPixelTransparent ;
 renderer.ShadowColor := ShadowColor;
 renderer.ShadowOffset.x := abs( ShadowOffsetX ) ;
 renderer.ShadowOffset.y := abs( ShadowOffsetY ) ;
 renderer.ShadowRadius := ShadowRadius ;
 renderer.OutlineVisible := (OutLineColor<>BGRAPixelTransparent) and (OutLineWidth>0) ;
 renderer.OutlineColor := OutLineColor;
 renderer.OuterOutlineOnly := True;
 renderer.OutlineWidth := OutLineWidth ;
 Result := TBGRABitmap.Create( 1, 1 );
 Result.FontRenderer := renderer ;
 Result.FontQuality:= fqFineAntialiasing;
 Result.FontName := FontName ;
 Result.FontHeight := FontHeight ;
 Result.FontStyle := Style ;
 if aBackGroundTexture = NIL
    then begin
          Result.SetSize( Result.TextSize(aCaption).cx + round(OutLineWidth)+abs( ShadowOffsetX )+ShadowRadius, Result.TextSize(aCaption).cy + round(OutLineWidth)+abs( ShadowOffsetY )+ShadowRadius );
          Result.Fill( BGRAPixelTransparent ) ;
          Result.TextOut( OutLineWidth/2, OutLineWidth/2, aCaption, FontColor ) ;
         end
    else begin
          Result.SetSize( Result.TextSize(aCaption+'  ').cx, Result.TextSize(aCaption+'  ').cy );
          if Result.Width > Result.Height
            then r := Result.Height * 0.3
            else r := Result.Width * 0.3 ;
          Result.RoundRectAntialias( 0, 0, Result.Width-1, Result.Height-1, r, r, nil, 0, aBackGroundTexture ) ;
          Result.RoundRectAntialias( 0, 0, Result.Width-1, Result.Height-1, r, r, OutLineColor, OutLineWidth ) ;
          Result.TextOut(Result.TextSize(' ').cx + OutLineWidth/2, OutLineWidth/2, aCaption, FontColor ) ;
         end;
end;

function SplitLineToStringArray(ALine: string; aCharSeparator: char): ArrayOfString;
var i, w: integer;
begin
 SetLength( Result, 0 );
 if Length( ALine ) = 0 then exit;

 if ALine[1] <> aCharSeparator
   then ALine := aCharSeparator + ALine;

 i := 1;
 w := -1;
 repeat
  if ALine[i] <> aCharSeparator
    then Result[w] := Result[w] + ALine[i]
    else begin
          inc ( w );
          SetLength( Result, w + 1 );
          Result[w] := '';
    end;
  inc ( i );
 until i > Length( ALine );
end;


procedure DrawTexture(aTexture: PTexture; AFrameIndex: integer; X, Y, aAngle: single; aAlpha: byte; ATint: TBGRAPixel);
var
  quad : array[ 0..3 ] of TPointF;
  TexCoord : TTexCoor;
  AlphaTint: byte;
  x1, x2 : Single;
  y1, y2 : Single;
  cX, cY : Single;
  c, s   : Single;
  aX, aY, aW, aH : Single ;
  procedure DrawVertex ;
   begin
    glBegin( GL_QUADS );
      glTexCoord2fv( @TexCoord[0] );  glVertex2fv( @quad[ 0 ] );
      glTexCoord2fv( @TexCoord[1] );  glVertex2fv( @quad[ 1 ] );
      glTexCoord2fv( @TexCoord[2] );  glVertex2fv( @quad[ 2 ] );
      glTexCoord2fv( @TexCoord[3] );  glVertex2fv( @quad[ 3 ] );
    glEnd;
   end;
begin
 if aTexture = nil then exit;

 if AFrameIndex < 0
   then AFrameIndex := 0
   else if AFrameIndex >= aTexture^.FrameCount
          then AFrameIndex := aTexture^.FrameCount-1;

 TexCoord := aTexture^.FramesCoord[AFrameIndex];

 aX := X;
 aY := Y;
 if AFrameIndex > 0
   then begin
    aW := aTexture^.FrameWidth;
    aH := aTexture^.FrameHeight;
   end else begin
    aW := aTexture^.ImageWidth;
    aH := aTexture^.ImageHeight;
   end;

 if aAngle <> 0
   then begin
         x1 := -aW / 2;
         y1 := -aH / 2;
         x2 := -x1;
         y2 := -y1;
         cX :=  aX + x2;
         cY :=  aY + y2;
         sincos( aAngle * deg2rad, s, c );
         quad[ 0 ].X := x1 * c - y1 * s + cX; quad[ 0 ].Y := x1 * s + y1 * c + cY;
         quad[ 1 ].X := x2 * c - y1 * s + cX; quad[ 1 ].Y := x2 * s + y1 * c + cY;
         quad[ 2 ].X := x2 * c - y2 * s + cX; quad[ 2 ].Y := x2 * s + y2 * c + cY;
         quad[ 3 ].X := x1 * c - y2 * s + cX; quad[ 3 ].Y := x1 * s + y2 * c + cY;
        end
   else begin
         quad[ 0 ].X := aX; quad[ 0 ].Y := aY;
         quad[ 1 ].X := aX + aW; quad[ 1 ].Y := aY;
         quad[ 2 ].X := aX + aW; quad[ 2 ].Y := aY + aH;
         quad[ 3 ].X := aX; quad[ 3 ].Y := aY + aH;
        end;

// glEnable(GL_BLEND);
// glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

 TextureManager.BindTexture( aTexture );

 if ATint.alpha < 255
   then begin
         glTexEnvi( GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE );
         glColor4ub( 255, 255, 255, aAlpha );
         DrawVertex;
        end;

 if ATint.alpha <> 0
   then begin  //Apply Tint
         glTexEnvi( GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, $8570 );
         glTexEnvi( GL_TEXTURE_ENV, $8571,  GL_REPLACE );
         glTexEnvi( GL_TEXTURE_ENV, $8580,  $8577 );
         AlphaTint:=round( ATint.alpha * aAlpha / 255 );
         glColor4ub( ATint.red, ATint.Green, ATint.Blue, AlphaTint);
         DrawVertex;
        end;

 TextureManager.DisableTextureUsage;
 {$IFDEF DEBUG_MODE_ON}
   DrawBox(X, Y, aTexture^.TextureWidth, aTexture^.TextureHeight, BGRA(255,0,0) );
 {$ENDIF}
end;

procedure DrawTexture(aTexture: PTexture; AFrameIndex: integer; X, Y: single; Param: TSimpleSurfaceWithEffect; ALayerPercentOpacity: single);
var
  quad : array[ 0..3 ] of TPointF;
  tci  : PTextureCoordIndex;
  TexCoord : TTexCoor;

  x1, x2 : Single;
  y1, y2 : Single;
  cX, cY : Single;
  c, s   : Single;
  AlphaTint:byte;
  i:integer;
  aX, aY, aW, aH : Single ;
  procedure DrawVertex ;
   begin
    glBegin( GL_QUADS );
      glTexCoord2fv( @TexCoord[ tci^[ 0 ] ] );  glVertex2fv( @quad[ 0 ] );
      glTexCoord2fv( @TexCoord[ tci^[ 1 ] ] );  glVertex2fv( @quad[ 1 ] );
      glTexCoord2fv( @TexCoord[ tci^[ 2 ] ] );  glVertex2fv( @quad[ 2 ] );
      glTexCoord2fv( @TexCoord[ tci^[ 3 ] ] );  glVertex2fv( @quad[ 3 ] );
    glEnd;
   end;
begin
 if aTexture = nil then exit;

 if AFrameIndex < 0
   then AFrameIndex := 0
   else if AFrameIndex >= aTexture^.FrameCount
          then AFrameIndex := aTexture^.FrameCount-1;

 TexCoord := aTexture^.FramesCoord[AFrameIndex];

 aX := X;
 aY := Y;

 with Param do
  begin
            if AFrameIndex>0
              then begin
                    aW := Width;
                    aH := Height;
              end else begin
                    aW := aTexture^.ImageWidth;
                    aH := aTexture^.ImageHeight;
              end;

            // Scale
            if (Scale.x.Value <> 1) or (Scale.y.Value <> 1) Then
              begin
                aX := aX + ( aW - aW * Scale.x.Value ) / 2;
                aY := aY + ( aH - aH * Scale.y.Value ) / 2;
                aW := aW * Scale.x.Value;
                aH := aH * Scale.y.Value;
              end;
            // Flip
            i:=0;
            if FFlipH then inc( i );
            if FFlipV then inc( i, 2 );
            tci := @FLIP_TEXCOORD[ i ];

            if Angle.Value <> 0
              then begin
                    x1 := -aW / 2;
                    y1 := -aH / 2;
                    x2 := -x1;
                    y2 := -y1;
                    cX :=  aX + x2;
                    cY :=  aY + y2;
                    sincos( Angle.Value * deg2rad, s, c );
                    quad[ 0 ].X := x1 * c - y1 * s + cX; quad[ 0 ].Y := x1 * s + y1 * c + cY;
                    quad[ 1 ].X := x2 * c - y1 * s + cX; quad[ 1 ].Y := x2 * s + y1 * c + cY;
                    quad[ 2 ].X := x2 * c - y2 * s + cX; quad[ 2 ].Y := x2 * s + y2 * c + cY;
                    quad[ 3 ].X := x1 * c - y2 * s + cX; quad[ 3 ].Y := x1 * s + y2 * c + cY;
                   end
              else begin
                    quad[ 0 ].X := aX; quad[ 0 ].Y := aY;
                    quad[ 1 ].X := aX + aW; quad[ 1 ].Y := aY;
                    quad[ 2 ].X := aX + aW; quad[ 2 ].Y := aY + aH;
                    quad[ 3 ].X := aX; quad[ 3 ].Y := aY + aH;
                   end;
            // Skew
            quad[0].X += Skew.x.Value;
            quad[0].Y += Skew.y.Value;
            quad[1].X += Skew.x.Value;
            quad[1].Y += Skew.y.Value;

            glEnable(GL_BLEND);
            glBlendFunc(GL_SRC_ALPHA, GL_ONE_MINUS_SRC_ALPHA);

            TextureManager.BindTexture( aTexture );

            if Tint.alpha.Value < 255
              then begin
                    glTexEnvi( GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE );
                    glColor4ub(255,255,255,round(Opacity.Value * aLayerPercentOpacity));
                    DrawVertex;
                   end;

            if Tint.alpha.Value <> 0
              then begin  //Apply Tint
                    glTexEnvi( GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, $8570 );
                    glTexEnvi( GL_TEXTURE_ENV, $8571,  GL_REPLACE );
                    glTexEnvi( GL_TEXTURE_ENV, $8580,  $8577 );
                    AlphaTint:=round( Tint.alpha.Value * Opacity.Value / 255 * aLayerPercentOpacity );
                    glColor4ub( round(Tint.red.Value), round(Tint.Green.Value), round(Tint.Blue.Value), AlphaTint);
                    DrawVertex;
                   end;

            TextureManager.DisableTextureUsage;
            {$IFDEF DEBUG_MODE_ON}
              DrawBox(X.Value, Y.Value, Width, Height, BGRA(255,0,0));
            {$ENDIF}
  end;
end;


procedure DrawBox(X, Y, Width, Height: single; const BorderColor: TBGRAPixel; const aLayerPercentOpacity: single);
begin
 X += 0.5;
 Y += 0.5;

 if (width=0) or (Height=0) then exit;

 glEnable( GL_BLEND );
 glColor4ub( BorderColor.red, BorderColor.green, BorderColor.blue, round(BorderColor.alpha*aLayerPercentOpacity) );
 glBegin( GL_LINES );
   glVertex2f( X, Y );
   glVertex2f( X + Width, Y );

   glVertex2f( X + Width, Y );
   glVertex2f( X + Width, Y + Height );

   glVertex2f( X + Width, Y + Height );
   glVertex2f( X, Y + Height );

   glVertex2f( X, Y + Height );
   glVertex2f( X, Y );
 glEnd;
end;

procedure DrawBox(X, Y, Width, Height: single; const BorderColor, FillColor: TBGRAPixel; const aLayerPercentOpacity: single);
begin
 FillBox( X, Y, Width, Height, FillColor, aLayerPercentOpacity );
 DrawBox(X, Y, Width, Height, BorderColor, aLayerPercentOpacity );
end;

procedure FillBox(X, Y, Width, Height: single; FillColor: TBGRAPixel; const aLayerPercentOpacity : single);
begin
 if (width=0) or (Height=0) then exit;
 glEnable( GL_BLEND );
 glColor4ub( FillColor.red, FillColor.green, FillColor.blue, round(FillColor.alpha*aLayerPercentOpacity) );
 glBegin( GL_QUADS );
   glVertex2f( X, Y );
   glVertex2f( X + Width, Y );
   glVertex2f( X + Width, Y + Height );
   glVertex2f( X, Y + Height );
 glEnd;
end;

procedure FillBox(X, Y, Width, Height: single; const aTopLeftColor, aTopRightColor, aBottomRightColor,
  aBottomLeftColor: TBGRAPixel; const aLayerPercentOpacity: single);
begin
 if (width=0) or (Height=0) then exit;
 glEnable( GL_BLEND );
 glBegin( GL_QUADS );
   with aTopLeftColor do glColor4ub( red, green, blue, round(alpha*aLayerPercentOpacity) );
   glVertex2f( X, Y );
   with aTopRightColor do glColor4ub( red, green, blue, round(alpha*aLayerPercentOpacity) );
   glVertex2f( X + Width, Y );
   with aBottomRightColor do glColor4ub( red, green, blue, round(alpha*aLayerPercentOpacity) );
   glVertex2f( X + Width, Y + Height );
   with aBottomLeftColor do glColor4ub( red, green, blue, round(alpha*aLayerPercentOpacity) );
   glVertex2f( X, Y + Height );
 glEnd;
end;

procedure DrawLine(x1, y1, x2, y2: single; Color: TBGRAPixel; const aLayerPercentOpacity: single);
begin
 glEnable( GL_BLEND );
 glEnable( GL_LINE_SMOOTH );
 glColor4ub( Color.red, Color.green, Color.blue, round(Color.alpha*aLayerPercentOpacity) );
 glBegin( GL_LINES );
   glVertex2f( x1 + 0.5, y1 + 0.5 );
   glVertex2f( x2 + 0.5, y2 + 0.5 );
 glEnd;
 glDisable( GL_LINE_SMOOTH );
end;


procedure SetBlendMode(ABlendMode: byte);
var src : LongWord;
    dst : LongWord;
begin
 if _FglCurrentBlendMode = ABlendMode then exit;
 case ABlendMode of

   FX_BLEND_ADD: begin
       src := GL_SRC_ALPHA;
       dst := GL_ONE;
       glEnable( GL_BLEND );
   end;

   FX_BLEND_MULT: begin
       src := GL_ZERO;
       dst := GL_SRC_COLOR;
       glEnable( GL_BLEND );
   end;

   FX_NOBLEND: glDisable( GL_BLEND )

   else begin
      src := GL_SRC_ALPHA;
      dst := GL_ONE_MINUS_SRC_ALPHA;
      glEnable( GL_BLEND );
   end;
 end;
 if not _glBlendFuncSeparateInitialized then begin
   glBlendFuncSeparate := TBlendFuncSeparateProc( wglGetProcAddress( 'glBlendFuncSeparate' ));
   _glBlendFuncSeparateInitialized := true;
 end;
 if Assigned( glBlendFuncSeparate )
   then glBlendFuncSeparate( src, dst, GL_ONE, GL_ONE_MINUS_SRC_ALPHA )
   else glBlendFunc( src, dst );
 _FglCurrentBlendMode := ABlendMode;
end;

