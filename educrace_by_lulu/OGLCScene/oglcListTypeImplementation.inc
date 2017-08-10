
{ TShaderManager }

procedure TShaderManager.NotifyFragmentCompilationError;
begin
 raise Exception.Create('Fragment Shader not compiled...');
end;

procedure TShaderManager.NotifyVertexCompilationError;
begin
 raise Exception.Create('Vertex Shader not compiled...');
end;

procedure TShaderManager.NotifyLinkError;
begin
 raise Exception.Create('Shader Program not linked...');
end;

function TShaderManager.getShader(aIndex: integer): PShader;
begin
 if (aIndex < 0) or (aIndex >= FList.Count)
   then Result := NIL
   else Result := PShader( FList.Items[aIndex] );
end;

procedure TShaderManager.Clear;
begin
 while FList.Count > 0 do DeleteShader( 0 );
end;

constructor TShaderManager.Create ;
begin
 FList := TList.Create ;
end;

destructor TShaderManager.Destroy;
begin
 Clear;
 FreeAndNil( FList );
 inherited Destroy;
end;

function TShaderManager.ShaderAvailable: boolean;
begin
 Result := glUseProgram <> NIL;
end;

function TShaderManager.CreateShader(const AVertexCodeSource, AFragmentCodeSource: string): TShaderID;
var R : GLint ;
    code:string;
    o: PShader;
begin
 if not ShaderAvailable then begin
   Showmessage('Cannot load OpenGL 2.0, no shader available...');
   Result := -1;
   exit;
 end;
 o := new(PShader);

 o^.ProgramID := 0;
 o^.VertexID := 0;
 o^.FragmentID := 0;
 if ( Length (AVertexCodeSource ) = 0 ) and ( Length (AFragmentCodeSource ) = 0 ) then exit;

 o^.ProgramID := glCreateProgram();

 if Length (AVertexCodeSource ) > 0
  then begin
        o^.VertexID := glCreateShader( GL_VERTEX_SHADER ) ;
        code := AVertexCodeSource;
        glShaderSource( o^.VertexID, 1, @code, NIL ) ;
        glCompileShader( o^.VertexID ) ;
        glGetShaderiv( o^.VertexID, GL_COMPILE_STATUS, @R ) ;
        if R = GL_FALSE then NotifyVertexCompilationError;

        glAttachShader( o^.ProgramID, o^.VertexID ) ;
  end;

 if Length (AFragmentCodeSource ) > 0
   then begin
         o^.FragmentID := glCreateShader( GL_FRAGMENT_SHADER );
         code := AFragmentCodeSource;
         glShaderSource( o^.FragmentID, 1, @code, NIL );
         glCompileShader( o^.FragmentID );
         glGetShaderiv( o^.FragmentID, GL_COMPILE_STATUS, @R );
         if R = GL_FALSE then NotifyFragmentCompilationError;

         glAttachShader( o^.ProgramID, o^.FragmentID );
   end;

 glLinkProgram ( o^.ProgramID );
 glGetProgramiv(o^.ProgramID, GL_LINK_STATUS, @R);
 if R = GL_FALSE then NotifyLinkError;

 glValidateProgram ( o^.ProgramID );

 Result := TShaderID( FList.Add( o ) );
end;

procedure TShaderManager.DeleteShader(AShader: TShaderID);
var o: PShader;
begin
 if not ShaderAvailable then exit;
 o := GetShader( AShader );
 if o = NIL then exit;
 with o^ do
  begin
   glDeleteShader( FragmentID );
   glDeleteShader( VertexID );
   glDeleteProgram( ProgramID )
  end;
 Dispose( o );
 FList.Delete( AShader );
end;

procedure TShaderManager.Use(AShader: TShaderID);
var o: PShader;
begin
 if not ShaderAvailable then exit;
 o := getShader(AShader);
 if o = NIL
   then glUseProgram( 0 )
   else glUseProgram( o^.ProgramID );
end;

procedure TShaderManager.UseNone;
begin
 if ShaderAvailable then glUseProgram( 0 );
end;

function TShaderManager.GetUniform(AShader: TShaderID; const AName): GLint;
var o: PShader;
begin
 Result := 0;
 if not ShaderAvailable then exit;
 o := getShader( AShader);
 if o <> NIL
   then Result := glGetUniformLocation( o^.ProgramID, @AName );
end;

procedure TShaderManager.SetUniform1f(AUniform: GLint; AValue: single);
begin
 if ShaderAvailable then glUniform1f( AUniform, AValue );
end;

procedure TShaderManager.SetUniform2f(AUniform: GLint; AValue1, AValue2: single);
begin
 if ShaderAvailable then glUniform2f( AUniform, AValue1, AValue2 );
end;

procedure TShaderManager.SetUniform3f(AUniform: GLint; AValue1, AValue2, AValue3: single);
begin
 if ShaderAvailable then glUniform3f( AUniform, AValue1, AValue2, AValue3 );
end;

procedure TShaderManager.SetUniform4f(AUniform: GLint; AValue1, AValue2, AValue3, AValue4: single);
begin
 if ShaderAvailable then glUniform4f( AUniform, AValue1, AValue2, AValue3, AValue4 );
end;

procedure TShaderManager.SetUniform1i(AUniform: GLint; AValue: GLint);
begin
 if ShaderAvailable then glUniform1i( AUniform, AValue );
end;



{ TTexturedFont }

constructor TTexturedFont.Create;
begin
 SetLength( FTexCharArray, 0 );
end;

destructor TTexturedFont.Destroy;
var i:integer;
begin
 for i:=0 to Length( FTexCharArray )-1 do
  if FTexCharArray[i] <> NIL
     then TextureManager.Delete( FTexCharArray[i] );
 SetLength( FTexCharArray, 0 );
 inherited Destroy;
end;

function TTexturedFont.IndexOf(c: char): integer;
var i : integer ;
begin
 Result := -1 ;
 for i:=1 to Length( FCharList ) do
  if c = FCharList[i]
     then begin
           Result := i-1 ;
           exit ;
          end;
end;

function TTexturedFont.GetCharTexture(c: char): PTexture;
var i: integer ;
begin
 Result := NIL ;
 i := IndexOf( c );
 if i=-1
    then Result := NIL
    else Result := PTexture( FTexCharArray[IndexOf( c )] ) ;
end;

procedure TTexturedFont.SetCharList(s: ansistring);
var i:integer;
begin
 FCharList := s ;
 SetLength( FTexCharArray, UTF8Length( s ) );
 for i:=0 to Length( FTexCharArray )-1 do
   FTexCharArray[i] := NIL ;
end;

procedure TTexturedFont.Build(FontName: string; FontHeight: integer;
  Style: TFontStyles; FontColor: TBGRAPixel; OutLineColor: TBGRAPixel;
  OutLineWidth: single; ShadowColor: TBGRAPixel; ShadowOffsetX, ShadowOffsetY, ShadowRadius: integer;
  FontQuality: TBGRAFontQuality);
var i : integer;
    ima: TBGRABitmap;
    s1 : string ;
    renderer: TBGRATextEffectFontRenderer;
begin
 FFontName := FontName ;
 FFontHeight := FontHeight ;
 FStyle := Style ;
 FFontColor := FontColor ;
 FOutLineColor := OutLineColor ;
 FOutLineWidth := OutLineWidth ;
 FShadowColor := ShadowColor ;
 FShadowOffsetX := ShadowOffsetX ;
 FShadowOffsetY := ShadowOffsetY ;
 FShadowRadius := ShadowRadius ;
//
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
 ima:= TBGRABitmap.Create(1,1);
 ima.FontRenderer := renderer ;
 ima.FontQuality:= FontQuality;
 ima.FontName := FontName ;
 ima.FontHeight := FontHeight ;
 ima.FontStyle := Style ;
 for i:=0 to Length( FTexCharArray )-1 do
  begin
   s1 := SysToUTF8( FCharList[i+1] );
   ima.SetSize( ima.TextSize( s1 ).cx + round(OutLineWidth)+abs( ShadowOffsetX )+ShadowRadius, ima.TextSize( s1 ).cy + round(OutLineWidth)+abs( ShadowOffsetY )+ShadowRadius );
   ima.Fill( BGRAPixelTransparent ) ;
   ima.TextOut(OutLineWidth/2, OutLineWidth/2, s1, FontColor ) ;
   FTexCharArray[i] := TextureManager.Add( ima ) ;
  end;
 ima.Free ;
end;

function TTexturedFont.GetCharWidth(c: char): integer;
var i: integer;
begin
 i := IndexOf( c );
 if i=-1
   then Result := 0
   else Result := FTexCharArray[i]^.ImageWidth ;
end;

function TTexturedFont.GetCharHeight(c: char): integer;
var i: integer;
begin
 i := IndexOf( c );
 if i=-1
   then Result := 0
   else Result := FTexCharArray[i]^.ImageHeight ;
end;

function TTexturedFont.Text2Bitmap(aCaption: UTF8String; aBackGround: TBGRABitmap): TBGRABitmap;
var w, r:single;
    renderer: TBGRATextEffectFontRenderer;
begin
 renderer:= TBGRATextEffectFontRenderer.Create;
 renderer.ShadowVisible := FShadowColor<>BGRAPixelTransparent ;
 renderer.ShadowColor := FShadowColor;
 renderer.ShadowOffset.x := abs( FShadowOffsetX ) ;
 renderer.ShadowOffset.y := abs( FShadowOffsetY ) ;
 renderer.ShadowRadius := FShadowRadius ;
 renderer.OutlineVisible := (FOutLineColor<>BGRAPixelTransparent) and (FOutLineWidth>0) ;
 renderer.OutlineColor := FOutLineColor;
 renderer.OuterOutlineOnly := True;
 renderer.OutlineWidth := FOutLineWidth;
 Result := TBGRABitmap.Create( 1, 1 );
 Result.FontRenderer := renderer ;
 Result.FontQuality:= fqFineAntialiasing;
 Result.FontName := FFontName ;
 Result.FontHeight := FFontHeight;
 Result.FontStyle := FStyle;
 if aBackGround = NIL
    then begin
          Result.SetSize( Result.TextSize(aCaption).cx + round(FOutLineWidth)+abs( FShadowOffsetX )+FShadowRadius, Result.TextSize(aCaption).cy + round(FOutLineWidth)+abs( FShadowOffsetY )+FShadowRadius );
          Result.Fill( BGRAPixelTransparent ) ;
          Result.TextOut( FOutLineWidth/2, FOutLineWidth/2, aCaption, FFontColor ) ;
         end
    else begin
          Result.SetSize( Result.TextSize(aCaption+'  ').cx, Result.TextSize(aCaption+'  ').cy );
          if Result.Width > Result.Height
            then begin
                  w := Result.Height * 0.03 ;
                  r := Result.Height * 0.3 ;
                 end
            else begin
                  w := Result.Width * 0.03 ;
                  r := Result.Width * 0.3 ;
                 end;
          if w < 0.5 then w := 0.5 ;
          Result.RoundRectAntialias( 0, 0, Result.Width-1, Result.Height-1, r, r, nil, 0, aBackGround ) ;
          Result.RoundRectAntialias( 0, 0, Result.Width-1, Result.Height-1, r, r, BGRA(255,255,255), w ) ;
          Result.TextOut(Result.TextSize(' ').cx + FOutLineWidth/2, FOutLineWidth/2, aCaption, FFontColor ) ;
         end;
end;




{ TFontManager }
constructor TFontManager.Create;
begin
 FList := TList.Create ;
 FCharList :='0123456789 ?!,.;:/*-+=()abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZéèàçùï[]@' ;
end;

destructor TFontManager.Destroy;
begin
 Clear ;
 FList.Free ;
 inherited Destroy;
end;

function TFontManager.AddFont(FontName: string; FontHeight: integer;
                              Style: TFontStyles; FontColor: TBGRAPixel;
                              OutLineColor: TBGRAPixel; OutLineWidth: single;
                              ShadowColor: TBGRAPixel; ShadowOffsetX, ShadowOffsetY, ShadowRadius: integer;
                              FontQuality: TBGRAFontQuality): TTexturedFont;
begin
 Result := TTexturedFont.Create;
 Result.SetCharList( FCharList );
 Result.Build( FontName, FontHeight, Style, FontColor, OutLineColor, OutLineWidth, ShadowColor, ShadowOffsetX, ShadowOffsetY, ShadowRadius, FontQuality );
 FList.Add( Result );
end;

procedure TFontManager.Clear;
 var i: integer;
begin
 for i:=0 to FList.Count-1 do
  TTexturedFont( FList.Items[i] ).Free ;
 FList.Clear;
end;

{ TTimerManager }

constructor TTimerManager.Create;
begin
 GetMem( FTimer0, Sizeof(TTimerObject) );
 FTimer0^.prev := NIL ;
 FTimer0^.Next := NIL ;
 FCount := 0 ;
end;

destructor TTimerManager.Destroy;
begin
 ClearAll;
 FreeMem( FTimer0 ) ;
 inherited Destroy;
end;

function TTimerManager.AddTimerToList: PTimerObject;
var p : PTimerObject ;
begin
 p := FTimer0 ;
 while p^.Next <> NIL do p := p^.Next ;
 GetMem( Result, SizeOf( TTimerObject ));
 p^.Next := Result ;
 Result^.Next := NIL ;
 Result^.Prev := p ;
 Result^.Kill := FALSE;
 inc(FCount);
end;

function TTimerManager.Add(aCallBack: TProcEvent; aIntervalMSec: integer ): PTimerObject;
begin
 Result := AddTimerToList ;
 Result^.CallBack := aCallBack ;
 Result^.ID := FCount ;
 Result^.Interval := aIntervalMSec ;
 Result^.Time := DateTimeToTimeStamp ( Now ) ;
end;

procedure TTimerManager.Delete( aTimer: PTimerObject);
begin
 aTimer^.Kill := TRUE;
end;

procedure TTimerManager.Clear;
begin
 ClearAllUserTimer;
end;

procedure TTimerManager.DoDelete(aTimer: PTimerObject);
var p, n :PTimerObject ;
begin
 if aTimer^.Next <> NIL
   then begin
         p := aTimer^.Prev ;
         n := aTimer^.Next ;
         p^.Next := n ;
         n^.Prev := p ;
        end
   else begin
         p := aTimer^.Prev ;
         p^.Next := NIL ;
        end;
 FreeMem( aTimer ) ;
 dec(FCount);
end;

procedure TTimerManager.ClearAll;
begin
 while FTimer0^.Next <> NIL do
  DoDelete ( FTimer0^.Next );
end;

procedure TTimerManager.ClearAllUserTimer;
var pTimerFPS: PTimerObject;
begin
 if FCount < 2 then exit;
 pTimerFPS := FTimer0^.Next;
 while pTimerFPS^.Next <> NIL do
  DoDelete ( pTimerFPS^.Next );
end;

procedure TTimerManager.ProcessTimer;
var p :PTimerObject ;
    tactual : TTimeStamp ;
    delta : integer ;
begin
 // delete all killed timer
 p := FTimer0;
 while p <> NIL do
  begin
   if p^.Next <> NIL
     then begin
           if p^.Next^.Kill
             then DoDelete( p^.Next )
             else p := p^.Next;
     end else p:=NIL;
  end;
 // process timer
 p := FTimer0^.Next;
 tactual := DateTimeToTimeStamp ( Now ) ;
 while p <> NIL do
  begin
   delta := tactual.Time - p^.Time.Time ;
   if delta >= p^.Interval
     then begin
           p^.Time.Time := tactual.Time + delta - p^.Interval ; //delta - p^.Interval ;
           if p^.CallBack <> NIL then p^.CallBack ();
          end;
   p := p^.Next;
  end;
end;



{ TTextureManager }

constructor TTextureManager.Create;
begin
 GetMem( FTexture0, Sizeof(TTexture) );
 FTexture0^.prev := NIL ;
 FTexture0^.Next := NIL ;
 FCount := 0 ;
 FCurrentBindedTexture := NIL;
 FGL_TEXTURE_2DEnabled := FALSE;
end;

destructor TTextureManager.Destroy;
begin
 Clear ;
 FreeMem( FTexture0 ) ;
 inherited Destroy;
end;

function TTextureManager.ValueOfNextPowerOfTwo(aValue: integer): integer;
begin
 Result :=1;
 while ( power (2,Result)<aValue ) do inc ( Result );
 Result := trunc(power (2,Result));
end;

function TTextureManager.AddTextureToList: PTexture;
var p : PTexture ;
begin
 p := FTexture0 ;
 while p^.Next <> NIL do p := p^.next ;

 new( Result );
 Result^.Next := NIL ;
 Result^.Prev := p ;

 p^.Next := Result ;
end;

function TTextureManager.InitFromBGRABitmap(aIma: TBGRABitmap ): PTexture;
var temp:TBGRABitmap;
begin
 Result := AddTextureToList;
 temp := TBGRABitmap.Create ( ValueOfNextPowerOfTwo(aIma.Width),ValueOfNextPowerOfTwo(aIma.Height),BGRAPixelTransparent );
 Result^.ImageWidth := aIma.Width;
 Result^.ImageHeight := aIma.Height;

 Result^.TextureWidth := temp.Width;
 Result^.TextureHeight := temp.Height;

 Result^.FrameWidth := aIma.Width;
 Result^.FrameHeight := aIma.Height;

 Result^.FrameCount := 1;

 temp.PutImage ( 0, 0, aIma, dmSet);
 temp.SwapRedBlue;

 glGenTextures(1, @Result^.ID );
 BindTexture( Result );// glBindTexture(GL_TEXTURE_2D, Result^.ID );
 glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA{0x1908}, temp.Width , temp.Height, 0, GL_RGBA, GL_UNSIGNED_BYTE, temp.Data );

 temp.Free;

 glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE );
 glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE );
 glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR); // Linear Filtering
 glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR); // Linear Filtering
end;

procedure TTextureManager.ComputeOpenGLFramesCoord(ATexture: PTexture; FramesX: Integer; FramesY: Integer);
var U,V: Single;
  tx, ty, fU, fV : Single;
  ix,iy,i: Integer;
begin
  with ATexture^ do
  begin
    U := ImageWidth/TextureWidth;
    V := ImageHeight/TextureHeight;

    if FramesX < 1 then FramesX := 1;
    if FramesY < 1 then FramesY := 1;
    FrameCount := FramesX * FramesY;

    if FrameCount > 1
      then FrameCount := FrameCount + 1; // because frame[0] fit whole texture, frame index start at 1
    SetLength( FramesCoord, FrameCount );

    FramesCoord[ 0, 0 ].X := 0;
    FramesCoord[ 0, 0 ].Y := 1;
    FramesCoord[ 0, 1 ].X := U;
    FramesCoord[ 0, 1 ].Y := 1;
    FramesCoord[ 0, 2 ].X := U;
    FramesCoord[ 0, 2 ].Y := 1 - V;
    FramesCoord[ 0, 3 ].X := 0;
    FramesCoord[ 0, 3 ].Y := 1 - V;

    if FrameCount = 1 then exit;

    fU := U / FramesX;
    fV := V / FramesY;

    i := 1;
    for iy := 1 to FramesY do
     for ix := 1 to FramesX do
      begin
        ty := 1 - fV * ( iy - 1 );
        tx := ( ix - 1 ) * fU;

        FramesCoord[ i, 0 ].X := tx;
        FramesCoord[ i, 0 ].Y := ty;

        FramesCoord[ i, 1 ].X := tx + fU;
        FramesCoord[ i, 1 ].Y := ty;

        FramesCoord[ i, 2 ].X := tx + fU;
        FramesCoord[ i, 2 ].Y := ty - fV;

        FramesCoord[ i, 3 ].X := tx;
        FramesCoord[ i, 3 ].Y := ty - fV;

        inc( i );
      end;
  end;
end;

function TTextureManager.Add(const aFilename: string): PTexture;
var ima : TBGRABitmap ;
begin
 try
  ima := TBGRABitmap.Create( aFilename ) ;
 except
  raise Exception.Create('Texture manager can''t create texture from file '+aFilename );//   ima := TBGRABitmap.Create( 50, 50, BGRA(255,255,255,255) );
  Result := NIL;
  exit;
 end;
 Result := InitFromBGRABitmap( ima );
 SetFrameSize( Result, Result^.ImageWidth, Result^.ImageHeight );
 ima.Free ;
 inc(FCount);
end;

function TTextureManager.Add(const aFilename: string; AFrameWidth,
  AFrameHeight: integer): PTexture;
begin
 Result := Add( aFilename );
 SetFrameSize( Result, AFrameWidth, AFrameHeight );
end;

function TTextureManager.Add(aImage: TBGRABitmap): PTexture;
begin
 Result := InitFromBGRABitmap( aImage ) ;
 SetFrameSize( Result, Result^.ImageWidth, Result^.ImageHeight );
 inc(FCount);
end;

function TTextureManager.Add(aImage: TBGRABitmap; AFrameWidth,
  AFrameHeight: integer): PTexture;
begin
 Result := Add( aImage );
 SetFrameSize( Result, AFrameWidth, AFrameHeight );
end;

function TTextureManager.AddEmpty(aWidth, aHeight: integer): PTexture;
begin
 Result := AddTextureToList;
 Result^.ImageWidth := aWidth;
 Result^.ImageHeight := aHeight;

 Result^.TextureWidth := ValueOfNextPowerOfTwo( aWidth );
 Result^.TextureHeight := ValueOfNextPowerOfTwo( aHeight );

 Result^.FrameWidth := aWidth;
 Result^.FrameHeight := aHeight;

 Result^.FrameCount := 1;

 glGenTextures(1, @Result^.ID );
 BindTexture( Result ); // glBindTexture(GL_TEXTURE_2D, Result^.ID );
 glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA{0x1908}, Result^.TextureWidth , Result^.TextureHeight, 0, GL_RGBA, GL_UNSIGNED_BYTE, NIL );

 glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP_TO_EDGE );
 glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP_TO_EDGE );
 //glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_LINEAR); // Linear Filtering
 //glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_LINEAR); // Linear Filtering
 glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MAG_FILTER, GL_NEAREST);
 glTexParameteri(GL_TEXTURE_2D, GL_TEXTURE_MIN_FILTER, GL_NEAREST);
 SetFrameSize( Result, Result^.ImageWidth, Result^.ImageHeight );
end;

procedure TTextureManager.Replace(ATexture: PTexture; const aFilename: string);
var ima : TBGRABitmap ;
begin
 try
  ima := TBGRABitmap.Create( aFilename ) ;
 except
  ima := TBGRABitmap.Create( 5, 5, BGRA(255,255,255,255) );
 end;
 Replace( ATexture, ima );
 ima.Free;
end;

procedure TTextureManager.Replace(ATexture: PTexture; aImage: TBGRABitmap);
var temp: TBGRABitmap;
begin
 temp := TBGRABitmap.Create ( ValueOfNextPowerOfTwo(aImage.Width),ValueOfNextPowerOfTwo(aImage.Height),BGRAPixelTransparent );
 temp.PutImage ( 0, 0, aImage, dmSet);
 temp.SwapRedBlue;

 BindTexture( ATexture );
 glTexImage2D(GL_TEXTURE_2D, 0, GL_RGBA{0x1908}, temp.Width , temp.Height, 0, GL_RGBA, GL_UNSIGNED_BYTE, temp.Data );
 temp.Free;
end;

procedure TTextureManager.SetFrameSize( ATexture: PTexture; AFrameWidth, AFrameHeight: integer );
begin
 if (ATexture^.ImageWidth = 0) or (ATexture^.ImageHeight = 0) then exit;
 if (AFrameWidth <= 0) or (AFrameHeight <= 0) or
    (AFrameWidth > ATexture^.ImageWidth) or (AFrameHeight > ATexture^.ImageHeight) then
 begin
   raise exception.Create('Invalid frame size');
   exit;
 end;
 ComputeOpenGLFramesCoord(ATexture, ATexture^.ImageWidth div AFrameWidth, ATexture^.ImageHeight div AFrameHeight);

 ATexture^.FrameWidth:= AFrameWidth;
 ATexture^.FrameHeight:= AFrameHeight;

end;

procedure TTextureManager.Delete(aTexture: PTexture);
var p, n : PTexture ;
begin
 if aTexture^.Next <> NIL
   then begin
         p := aTexture^.Prev ;
         n := aTexture^.Next ;
         p^.Next := n ;
         n^.Prev := p ;
        end
   else begin
         p := aTexture^.Prev ;
         p^.Next := NIL ;
        end;
 glDeleteTextures( 1, @aTexture^.ID );
 SetLength( aTexture^.FramesCoord, 0 );
 Dispose( aTexture );
 dec( FCount );
end;

procedure TTextureManager.Clear;
begin
 while FTexture0^.Next <> NIL do
  Delete( FTexture0^.Next );
end;

procedure TTextureManager.BindTexture(ATexture: PTexture);
begin
 if ATexture = NIL then exit;
 if not FGL_TEXTURE_2DEnabled
   then begin
         FGL_TEXTURE_2DEnabled := TRUE;
         glEnable( GL_TEXTURE_2D );
   end;
 if FCurrentBindedTexture <> aTexture
  then begin
        glBindTexture( GL_TEXTURE_2D, aTexture^.ID );
        FCurrentBindedTexture := aTexture;
  end;
end;

procedure TTextureManager.DisableTextureUsage;
begin
 if not FGL_TEXTURE_2DEnabled then exit;
 glDisable( GL_TEXTURE_2D );
 FGL_TEXTURE_2DEnabled := FALSE;
end;

function TTextureManager.GetFrameCount(ATexture: PTexture): TSize;
begin
 if ATexture = NIL
    then begin
          Result.cx := 0;
          Result.cy := 0;;
    end else begin
          Result.cx := ATexture^.ImageWidth div ATexture^.FrameWidth;
          Result.cy := ATexture^.ImageHeight div ATexture^.FrameHeight;
    end;
end;


