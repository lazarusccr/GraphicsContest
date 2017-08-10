
{ TTileEngine }

constructor TTileEngine.Create;
begin
 inherited Create;
 ScrollSpeed := TPointFParam.Create;
 PositionOnMap := TPointFParam.Create;
 MapHoleColor := TBGRAParam.Create;
 FOnTileEvent := NIL;

 FVScrollEnable := TRUE;
 FHScrollEnable := TRUE;

 TileMapDesignerModeEnable := FALSE;

 Clear;
end;

destructor TTileEngine.Destroy;
begin
 Clear;
 FreeAndNil( ScrollSpeed );
 FreeAndNil( PositionOnMap );
 FreeAndNil( MapHoleColor );
 inherited Destroy;
end;

procedure TTileEngine.Update(const aElapsedTime: single);
begin
 if FFreeze then exit;

 inherited Update( aElapsedTime );

 ScrollSpeed.OnElapse( aElapsedTime );
  if ScrollSpeed.x.Value <> 0.000
    then PositionOnMap.x.Value := PositionOnMap.x.Value + aElapsedTime * ScrollSpeed.x.Value
    else PositionOnMap.x.OnElapse( aElapsedTime );
  if ScrollSpeed.y.Value <> 0.000
    then PositionOnMap.y.Value := PositionOnMap.y.Value - aElapsedTime * ScrollSpeed.y.Value
    else PositionOnMap.y.OnElapse( aElapsedTime );

  MapHoleColor.OnElapse( aElapsedTime );
end;

procedure TTileEngine.EnsurePositionInMap(var APos: TPointF);
begin
 // ensure position on map is ok according horizontal and vertical loop mode
 if ( APos.x < 0 )
  then begin
        if not FHScrollEnable
         then APos.x := 0
         else if FHLoopMode
                then begin
                      repeat
                       APos.x := APos.x + FMapSize.cx ;
                      until APos.x > 0 ;
                     end;
       end
  else if APos.x >= FMapSize.cx - FWidth
         then begin
               if not FHScrollEnable
                then APos.x := FMapSize.cx - Width
                else if FHLoopMode
                       then begin
                             if APos.x >= FMapSize.cx
                               then repeat
                                     APos.x := APos.x - FMapSize.cx ;
                                    until APos.x < FMapSize.cx ;
                            end
              end;

 if APos.y < 0
  then begin
        if not FVScrollEnable
         then APos.y := 0
         else if FVLoopMode
               then begin
                     repeat
                      APos.y := APos.y + FMapSize.cy;
                     until APos.y > 0 ;
                    end;
       end
  else if APos.y >= FMapSize.cy - FHeight
         then begin
               if not FVScrollEnable
                 then APos.y := FMapSize.cy - Height
                 else if FVLoopMode
                        then if APos.y >= FMapSize.cy
                               then begin
                                     repeat
                                      APos.y := APos.y - FMapSize.cy;
                                     until APos.y < FMapSize.cy ;
                                    end;
              end;
end;

procedure TTileEngine.ComputeIndexes(APosition: TPointF);
begin
 EnsurePositionInMap( APosition );

 FColumnIndexFirstTile := trunc( APosition.x / FTileSize.cx );
 if APosition.x < 0 then dec( FColumnIndexFirstTile );
 FXOffsetFirstTile := - ( APosition.x / FTileSize.cx - FColumnIndexFirstTile ) * FTileSize.cx ;

 FRowIndexFirstTile := trunc( APosition.y / FTileSize.cy ) ;
 if APosition.y < 0 then dec( FRowIndexFirstTile );
 FYOffsetFirstTile := - ( APosition.y / FTileSize.cy - FRowIndexFirstTile ) * FTileSize.cy ;
end;

procedure TTileEngine.Draw(const aLayerPercentOpacity: single);
var ixtile, iytile : integer;
    indexTex, indexFrame: integer;
    xx, yy, xxx, yyy: single;
    c: TBGRAPixel;
    p: TPointF;

    procedure DrawColoredSquare; inline;
    begin
     if c.alpha = 0 then exit;
     FillBox( xx + x.Value, yy + y.Value, FTileSize.cx, FTileSize.cy, c );
    end;

begin
 if not Visible then exit;

 c := MapHoleColor.Value;
 c.alpha := round( Opacity.Value * aLayerPercentOpacity * c.alpha/255 );

// PushAndPrepareMatrix;          <- issue with glScissor
 SetBlendMode( FBlendMode );

 if ( Length( FMap ) = 0 ) or ( GetTextureCount = 0 ) then begin
  FillBox( x.Value, y.Value, Width-1, Height-1, c );
 end else begin
  // compute first tile indexes and its offset
  p := PositionOnMap.Value;
  ComputeIndexes( p );

  if not TileMapDesignerModeEnable then
  begin
   glScissor( round(x.Value), round(y.Value), Width, Height );
   glEnable( GL_SCISSOR_TEST );
  end;

  yy := FYOffsetFirstTile;
  iytile := FRowIndexFirstTile;

  repeat
    xx := FXOffsetFirstTile;
    ixtile := FColumnIndexFirstTile;

    repeat
      xxx := xx + x.Value;
      yyy := yy + y.Value;
      if not( ( xxx > FParentScene.Width ) or ( xxx+TileSize.cx < 0 ) or
              ( yyy > FParentScene.Height) or ( yyy+TileSize.cy < 0 ) )
       then begin  // draw only non clipped tile
             if ( ixtile < 0 ) or ( ixtile >= FMapTileCount.cx ) or
                ( iytile < 0 ) or ( iytile >= FMapTileCount.cy )
               then DrawColoredSquare
               else begin
                     if ( FMap[iytile,ixtile].TextureIndex > -1 )
                      then begin
                            indexTex := FMap[iytile,ixtile].TextureIndex;
                            indexFrame := TileEngineIndexFrameToTextureIndexFrame( indexTex, FMap[iytile,ixtile].ixFrame, FMap[iytile,ixtile].iyFrame );

                            DrawTexture( FTextureList[indexTex].Texture, indexFrame, xxx, yyy, 0, round( Opacity.Value * aLayerPercentOpacity ), Tint.Value );
                           end
                      else DrawColoredSquare;
                     // callback event
                     if ( FOnTileEvent <> NIL ) and (( FMap[iytile][ixtile].UserEvent <> -1 ) or TileMapDesignerModeEnable )
                       then FOnTileEvent( self, PointF(xxx,yyy), FMap[iytile][ixtile].UserEvent );
               end;
       end;

      xx += FTileSize.cx;
      inc(ixtile);
      if FHLoopMode and ( ixtile >= FMapTileCount.cx ) then ixtile := 0;
    until xx >= FWidth;

    yy += FTileSize.cy;
    inc(iytile);
    if FVLoopMode and ( iytile >= FMapTileCount.cy ) then iytile := 0;
  until yy >= FHeight;

  glDisable( GL_SCISSOR_TEST );
 end;

 DrawChildSurfaces( aLayerPercentOpacity );
// PopMatrix;
end;

function TTileEngine.GetWidth: integer;
begin
 Result := FWidth;
end;

function TTileEngine.GetHeight: integer;
begin
 Result := FHeight;
end;

function TTileEngine.AddTexture(AFilename: string; AFrameWidth, AFrameHeight: integer): integer;
var ix, iy: integer;
    fc: TSize;
begin
 Result := Length( FTextureList );
 SetLength( FTextureList, Result + 1 );
 FTextureList[Result].Texture := TextureManager.Add( AFilename );
 TextureManager.SetFrameSize( FTextureList[Result].Texture, AFrameWidth, AFrameHeight );

 SetTileSize( AFrameWidth, AFrameHeight );

 TextureManager.BindTexture( FTextureList[Result].Texture );
 glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_S, GL_CLAMP ); //GL_CLAMP_TO_EDGE );
 glTexParameteri( GL_TEXTURE_2D, GL_TEXTURE_WRAP_T, GL_CLAMP ); //GL_CLAMP_TO_EDGE );
 glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MIN_FILTER,GL_NEAREST); // nearest Filtering
 glTexParameteri(GL_TEXTURE_2D,GL_TEXTURE_MAG_FILTER,GL_NEAREST); // nearest Filtering

 // tile type to default (-1)
 fc := TextureManager.GetFrameCount( FTextureList[Result].Texture );
 SetLength( FTextureList[Result].TileType, fc.cx, fc.cy );

 for ix := 0 to fc.cx-1 do
  for iy := 0 to fc.cy-1 do
   FTextureList[Result].TileType[ix][iy] := 1;  // neutral ground type by default
end;

procedure TTileEngine.DeleteTexture(AIndex: integer);
var i, co, ro: integer;
    p: PTile;
begin
 if not IsValidTextureIndex( AIndex ) then exit;

 for ro:=0 to FMapTileCount.cy-1 do
  for co:=0 to FMapTileCount.cx-1 do begin
    p := GetPTile( ro, co );
    if p^.TextureIndex = AIndex then SetCell( ro, co, -1, 0, 0 );
  end;

 for i:= AIndex to Length( FTextureList ) - 2 do
  FTextureList[i] := FTextureList[i+1];
 SetLength( FTextureList, Length( FTextureList ) - 1 );
end;

function TTileEngine.GetTexture(AIndex: integer): PTexture;
begin
 if (AIndex >= 0) and (AIndex<Length(FTextureList))
   then Result := FTextureList[AIndex].Texture
   else Result := NIL;
end;

function TTileEngine.GetTextureCount: integer;
begin
 Result := Length( FTextureList );
end;

procedure TTileEngine.SetMapTileCount(ARowCount, AColumnCount: integer);
var ro, co, oldRowCount, oldColumnCount: integer;
begin
 oldRowCount := MapTileCount.cy;
 oldColumnCount := MapTileCount.cx;

 SetLength( FMap, ARowCount, AColumnCount );
 FMapTileCount.cx := AColumnCount;
 FMapTileCount.cy := ARowCount;

 FMapSize.cx := FMapTileCount.cx * FTileSize.cx;
 FMapSize.cy := FMapTileCount.cy * FTileSize.cy;

 if oldColumnCount < AColumnCount then
   for ro:=0 to ARowCount-1 do
    for co:=oldColumnCount to AColumnCount-1 do
     begin
      FMap[ro][co].TextureIndex := -1;
      FMap[ro][co].UserEvent := -1;
     end;

 if oldRowCount < ARowCount then
  for ro:=oldRowCount to ARowCount-1 do
   for co:=0 to AColumnCount-1 do
    begin
     FMap[ro][co].TextureIndex := -1;
     FMap[ro][co].UserEvent := -1;
    end;
end;

procedure TTileEngine.InsertRow(ARowIndex, ACount: integer);
var ro, co, oldrowcount: integer;
begin
 if ACount <= 0 then exit;
 if not IsValidRow( ARowIndex ) then exit;

 oldrowcount := MapTileCount.cy;
 SetMapTileCount( MapTileCount.cy + ACount, MapTileCount.cx );

 if ARowIndex <= oldrowcount-1 then
 for ro:=FMapTileCount.cy-1 downto ARowIndex+ACount do
  for co:=0 to FMapTileCount.cx-1 do begin
    FMap[ro][co].TextureIndex := FMap[ro-ACount][co].TextureIndex;
    FMap[ro][co].ixFrame := FMap[ro-ACount][co].ixFrame;
    FMap[ro][co].iyFrame := FMap[ro-ACount][co].iyFrame;
    FMap[ro][co].UserEvent := FMap[ro-ACount][co].UserEvent;
  end;

 for ro:=ARowIndex to ARowIndex+ACount-1 do
  for co:=0 to FMapTileCount.cx-1 do
   begin
    FMap[ro][co].TextureIndex := -1;
    FMap[ro][co].UserEvent := -1;
   end;
end;

procedure TTileEngine.InsertColumn(AColumnIndex, ACount: integer);
var ro, co: integer;
begin
 if ACount <= 0 then exit;
 if not IsValidColumn( AColumnIndex ) then exit;

 SetMapTileCount( MapTileCount.cy , MapTileCount.cx + ACount );

 for ro:=0 to FMapTileCount.cy-1 do
  for co:=FMapTileCount.cx-1 downto AColumnIndex+ACount do begin
    FMap[ro][co].TextureIndex := FMap[ro][co-ACount].TextureIndex;
    FMap[ro][co].ixFrame := FMap[ro-ACount][co].ixFrame;
    FMap[ro][co].iyFrame := FMap[ro][co-ACount].iyFrame;
    FMap[ro][co].UserEvent := FMap[ro][co-ACount].UserEvent;
  end;

 for ro:=0 to FMapTileCount.cy-1 do
  for co:=AColumnIndex to AColumnIndex+ACount-1 do
   begin
    FMap[ro][co].TextureIndex := -1;
    FMap[ro][co].UserEvent := -1;
   end;
end;

procedure TTileEngine.ResetMap;
var co, ro: integer;
begin
 for ro:=0 to Length(FMap)-1 do
  for co:=0 to Length(FMap[ro])-1 do
   begin
    SetCell( ro, co, -1, 0, 0 );
    SetUserEventValue( ro, co, -1 );
   end;
end;

procedure TTileEngine.SetTileSize(AHSize, AVSize: integer);
begin
 FTileSize.cx := AHSize;
 FTileSize.cy := AVSize;
end;

function TTileEngine.IsValidRow(ARow: integer): boolean;
begin
 Result :=  ( ARow >= 0 ) and ( ARow < MapTileCount.cy );
end;

function TTileEngine.IsValidColumn(AColumn: integer): boolean;
begin
 Result := ( AColumn >= 0 ) and ( AColumn < MapTileCount.cx );
end;

function TTileEngine.IsValidTextureIndex(AITex: integer): boolean;
begin
 Result := ( AITex >= 0 ) and ( AITex < Length( FTextureList ));
end;

function TTileEngine.IsValidFrameIndex(ATextureIndex, ixFrame, iyFrame: integer ): boolean;
var fc: TSize;
begin
 Result := IsValidTextureIndex( ATextureIndex );
 if Result then begin
  fc := TextureManager.GetFrameCount( FTextureList[ATextureIndex].Texture );
  Result := ( ixFrame > -1 ) and ( ixFrame < fc.cx ) and
            ( iyFrame > -1 ) and ( iyFrame < fc.cy );
 end;
end;

function TTileEngine.TileEngineIndexFrameToTextureIndexFrame(iTexture, ixFrame, iyFrame: integer ): integer;
var p: PTexture;
begin
 p := GetTexture( iTexture );
 if p^.FrameCount = 1
   then Result := 0
   else Result := iyFrame * TextureManager.GetFrameCount( p ).cx + ixFrame + 1;  //[ix,iy]=[0,0]=> FrameIndex := 1
end;

procedure TTileEngine.SetCell(ARow, AColumn, ATextureIndex, ixFrame, iyFrame: integer);
var p: PTile;
    fc: TSize;
begin
 p := GetPTile( ARow, AColumn );
 if p = NIL then exit;

 p^.TextureIndex := ATextureIndex;
 p^.ixFrame := ixFrame;
 p^.iyFrame := iyFrame;

 if ATextureIndex = -1 then exit;

 fc:= TextureManager.GetFrameCount( FTextureList[ATextureIndex].Texture );
 if p^.ixFrame >= fc.cx then  p^.ixFrame := fc.cx-1;
 if p^.ixFrame < 0 then  p^.ixFrame := 0;

 if p^.iyFrame >= fc.cy then  p^.iyFrame := fc.cy-1;
 if p^.iyFrame < 0 then  p^.iyFrame := 0;
end;

procedure TTileEngine.SetUserEventValue(ARow, AColumn, AUserEvent: integer);
var p: PTile;
begin
 p := GetPTile( ARow, AColumn );
 if p <> NIL then p^.UserEvent := AUserEvent;
end;

function TTileEngine.GetUserEventValue(ARow, AColumn: integer): integer;
var p: PTile;
begin
 p := GetPTile( ARow, AColumn );
 if p <> NIL then Result := p^.UserEvent;
end;

{
 procedure TTileEngine.GetCellTextureAndFrameIndex(ARow, AColumn: integer; out TextureIndex, ixFrame, iyFrame: integer);
 var t: PTile;
 begin
  t := GetPTile( ARow, AColumn );
  if t <> NIL
    then begin
      TextureIndex := t^.TextureIndex;
      ixFrame := t^.ixFrame;
      iyFrame := t^.iyFrame;
    end else begin
      TextureIndex := -1;
      ixFrame := -1;
      iyFrame := -1;
    end;
 end;
}

function TTileEngine.GetPTile(ARow, AColumn: integer): PTile;
begin
 if IsValidRow( ARow ) and IsValidColumn( AColumn )
   then Result := @FMap[ARow][AColumn]
   else Result := NIL;
end;

function TTileEngine.GetPTileTexInfo(ARow, AColumn: integer): PTileTexInfo;
var p: PTile;
begin
 p := GetPTile( ARow, AColumn );
 if p = NIL
   then Result := NIL
   else Result := @FTextureList[p^.TextureIndex];
end;

procedure TTileEngine.SetGroundType(ATextureIndex, ixFrame, iyFrame, AGroundType: integer);
begin
 if not IsValidFrameIndex( ATextureIndex, ixFrame, iyFrame ) then exit;
 FTextureList[ATextureIndex].TileType[ixFrame][iyFrame] := AGroundType;
end;

function TTileEngine.GetGroundType(ATextureIndex, ixFrame, iyFrame: integer ): integer;
begin
 if not IsValidFrameIndex( ATextureIndex, ixFrame, iyFrame )
   then Result := 0    // value for ground 'hole'
   else Result := FTextureList[ATextureIndex].TileType[ixFrame][iyFrame];
end;

procedure TTileEngine.ClearCell(ARow, AColumn: integer);
begin
 if ( AColumn < 0 ) or ( AColumn > MapTileCount.cx-1 ) then exit;
 if ( ARow < 0 ) or ( ARow > MapTileCount.cy-1 ) then exit;

 FMap[ARow][AColumn].TextureIndex := -1;
end;

function TTileEngine.GetBoundedPositionOnMap: TPointF;
begin
 Result := PositionOnMap.Value;
 EnsurePositionInMap( Result );
end;

procedure TTileEngine.Clear;
var i : integer;
begin
 SetLength( FMap, 0 );

 for i:=0 to Length( FTextureList )-1 do
  TextureManager.Delete ( FTextureList[i].Texture );
 SetLength( FTextureList, 0 );

 FMapSize.cx := 0;
 FMapSize.cy :=0;
 FMapTileCount.cx := 0;
 FMapTileCount.cy := 0;
// FPositionOnMap := PointF( 0, 0 );
// ScrollSpeed.Value := PointF( 0, 0 );
// MapHoleColor.Value := BGRA(0,0,0,0);
end;

procedure TTileEngine.SetViewSize(AWidth, AHeight: integer);
begin
 if AWidth >= 0 then FWidth := AWidth;
 if AHeight >= 0 then FHeight := AHeight;
end;

function TTileEngine.GetFirstTileColumnIndex: integer;
begin
 Result := FColumnIndexFirstTile;
end;

function TTileEngine.GetFirstTileRowIndex: integer;
begin
 Result := FRowIndexFirstTile;
end;

function TTileEngine.GetGroundType(aSceneCoor: TPointF): integer;
var p: PTile;
    mp: TPointF;
    ro, co: integer;
begin
 Result := 1;    // Neutral ground by default
 if ( TileSize.cx = 0 ) or ( TileSize.cy = 0 ) then exit;
 if not SceneCoorIsInMap( aSceneCoor, @mp ) then exit;

 co := round( mp.x ) div TileSize.cx + FColumnIndexFirstTile;
 ro := round( mp.y ) div TileSize.cy + FRowIndexFirstTile;

 p := GetPTile( ro, co );
 if p <> NIL
   then Result := GetGroundType( p^.TextureIndex, p^.ixFrame, p^.iyFrame );
end;

function TTileEngine.GetUserEventValue(aSceneCoor: TPointF): integer;
var p: PTile;
    mp: TPointF;
    ro, co: integer;
begin
 Result := -1;
 if ( TileSize.cx = 0 ) or ( TileSize.cy = 0 ) then exit;
 if not SceneCoorIsInMap( aSceneCoor, @mp ) then exit;

 co := round( mp.x ) div TileSize.cx + FColumnIndexFirstTile;
 ro := round( mp.y ) div TileSize.cy + FRowIndexFirstTile;

 p := GetPTile( ro, co );
 if p <> NIL then Result := p^.UserEvent;
end;

function TTileEngine.SceneCoorIsInMap(aSceneCoor: TPointF;  MapPos: PPointF): boolean;
var f1, f2: boolean;
    P: TPointF;
begin
 P := SceneCoorToMapPosition( aSceneCoor );
 f1 := ( P.x >= 0 ) and ( P.x < FMapSize.cx );
 f1 := f1 OR FHLoopMode;

 f2 := ( P.y >= 0 ) and ( P.y < FMapSize.cy );
 f2 := f2 OR FVLoopMode;

 Result := f1 and f2 ;

 if MapPos <> NIL then MapPos^ := p;
end;

function TTileEngine.SceneCoorToMapPosition(aSceneCoor: TPointF): TPointF;
begin
 Result := aSceneCoor - PointF(X.Value, Y.Value );
end;

function TTileEngine.MapPositionToSceneCoor(aMapPos: TPointF): TPointF;
begin
 Result := aMapPos + PointF(X.Value, Y.Value );
end;

function TTileEngine.GetTileSize: TSize;
begin
 Result := FTileSize;
end;

function TTileEngine.GetMapSize: TSize;
begin
 Result := FMapSize;
end;

procedure TTileEngine.LoadMapFile(const aFilename: string);
var FMapFile: TStringList;
    path: string;
    i, j, k, z, Count, ix, iy : integer;
    txt:string;
    txtsplited, temp: ArrayOfString;
begin
 Clear ;
 FMapFile := TStringList.Create;
 FMapFile.LoadFromFile( aFilename );
 path := ExtractFilePath( aFileName );

 // load map's textures
 k := FMapFile.IndexOf('TEXTURES');
 if k=-1 then raise exception.Create('Tile engine - LoadMap error : can''t retrieve texture list');
 inc(k);
 Count := strtoint( FMapFile.Strings[k] );
 inc(k);
 for i:=0 to Count-1 do
  begin
   txtsplited := SplitLineToStringArray( FMapFile.Strings[k], '|' );
   if Length( txtsplited ) <> 3
     then raise exception.Create('Tile engine - LoadMap error : can''t retrieve texture name and frame size');
   AddTexture( path + txtsplited[0], strtoint( txtsplited[1] ), strtoint( txtsplited[2] ));
   inc(k);
  end;

 // load Frame Type
 //  FRAMES_TYPE
 //  texture_count
 //  ixCount|iyCount|values_first_line|values_second_line|...        <- texture 1
 //  ixCount|iyCount|values_first_line|values_second_line|...        <- texture 2
 //  ....
 k := FMapFile.IndexOf('FRAMES_TYPE');
 if k <> -1 then begin
  inc (k);
  Count := strtoint( FMapFile.Strings[k] );  // texture count
  inc(k);
  for i:=0 to Count-1 do
   begin
    z := 0;
    txtsplited := SplitLineToStringArray( FMapFile.Strings[k], '|' );
    for iy:=0 to strtoint(txtsplited[1])-1 do
     for ix:=0 to strtoint(txtsplited[0])-1 do
      begin
       SetGroundType( i, ix, iy, strtoint( txtsplited[2+z{ix}] ));
       inc(z);
      end;
    inc (k);
   end;
 end;

 // Draw Size
 k := FMapFile.IndexOf('DRAW_SIZE');
 if k <> -1
   then begin
         inc(k);
         txtsplited := SplitLineToStringArray( FMapFile.Strings[k], '|' );
         if Length( txtsplited ) <> 2
           then raise exception.Create('Tile engine - LoadMap error : can''t retrieve draw size');
         SetViewSize( strtoint( txtsplited[0]), strtoint( txtsplited[1] ));
         x.Value := ( FParentScene.Width - FWidth ) / 2;    // center on scene by default
         y.Value := ( FParentScene.Height - FHeight ) / 2;
   end;

 // Tile Size
 k := FMapFile.IndexOf('TILE_SIZE');
 if k = -1
   then raise exception.Create('Tile engine - LoadMap error : can''t retrieve tile size');
 inc(k);
 txtsplited := SplitLineToStringArray( FMapFile.Strings[k], '|' );
 if Length( txtsplited ) <> 2
   then raise exception.Create('Tile engine - LoadMap error : can''t retrieve tile size');
 SetTileSize( strtoint( txtsplited[0] ), strtoint( txtsplited[1] ) );

 // Map tile count
 k := FMapFile.IndexOf('MAP_SIZE');
 if k = -1
   then raise exception.Create('Tile engine - LoadMap error : can''t retrieve map tile count');
 inc(k);
 txtsplited := SplitLineToStringArray( FMapFile.Strings[k], '|' );
 if Length( txtsplited ) <> 2
   then raise exception.Create('Tile engine - LoadMap error : can''t retrieve map size');
 SetMapTileCount( strtoint( txtsplited[0] ), strtoint( txtsplited[1] ) );

 // Scroll enable
 FHScrollEnable := FALSE;
 FVScrollEnable := FALSE;
 k := FMapFile.IndexOf('SCROLL_ENABLE');
 if k <> -1 then begin
   txt := UpperCase(FMapFile.Strings[k+1]);
   FHScrollEnable := ( txt='H' ) or ( txt='VH' ) or (  txt='HV' );
   FVScrollEnable := ( txt='V' ) or ( txt='VH' ) or (  txt='HV' );
 end;
 // Scroll Loop Mode
 FHLoopMode := FALSE;
 FVLoopMode := FALSE;
 k := FMapFile.IndexOf('SCROLL_LOOP_MODE');
 if k <> -1 then begin
   txt := UpperCase(FMapFile.Strings[k+1]);
   FHLoopMode := ( txt='H' ) or ( txt='VH' ) or (  txt='HV' );
   FVLoopMode := ( txt='V' ) or ( txt='VH' ) or (  txt='HV' );
 end;

 // Start tile
 PositionOnMap.Value := PointF( 0, 0);
 k := FMapFile.IndexOf('START_TILE');
 if k <> -1 then begin
   txtsplited := SplitLineToStringArray( FMapFile.Strings[k+1], '|' );
   if Length( txtsplited ) = 2 then begin
     PositionOnMap.x.Value := strtoint( txtsplited[0] ) * FTileSize.cx;
     PositionOnMap.y.Value := strtoint( txtsplited[1] ) * FTileSize.cy;
   end;
 end;

 // hole color
 MapHoleColor.Value := BGRA(0,0,0,255);
 k := FMapFile.IndexOf('HOLE_COLOR');
 if k <> -1 then MapHoleColor.Value := HexToBGRAPixel( FMapFile.Strings[k+1] );

 // Build map from Data
 k := FMapFile.IndexOf('DATA');
 if k = -1 then raise exception.Create('Tile engine - LoadMap error : can''t retrieve map data');
 inc(k);

 for i:=0 to FMapTileCount.cy-1 do
  begin
   j := 0;
   temp := SplitLineToStringArray( FMapFile.Strings[k], ' ' );
   for z:=0 to Length( temp )-1 do
    begin
     txtsplited := SplitLineToStringArray( temp[z], ',' );

     case txtsplited[0][1] of
      'T' : begin
             SetCell( i, j, strtoint( copy(txtsplited[0], 2, length( txtsplited[0] ) - 1 )),
                            strtoint( txtsplited[1] ),
                            strtoint( txtsplited[2] ));
             SetUserEventValue( i, j, strtoint( txtsplited[3] ));
             inc(j);
            end;
     end;//case

    end;
   inc(k);
  end;

 FMapFile.Free;
 ScrollSpeed.Value := PointF( 0, 0 );
end;


{ TParticleEmitter }

constructor TParticleEmitter.Create ;
var i:integer ;
begin
 inherited Create;
 for i:=0 to MAX_PARTICLES-1 do
  begin
   FParticles[i].VX := 0;
   FParticles[i].VY := 0;
   FParticles[i].InitialLife := 0;
   FParticles[i].CurrentLife := 0;
  end;
 LoopMode := TRUE;
 InitialLife := 0;
 FCurrentLife := 0;
 FParticleCount := 0;
 ParticlesToEmit := CreateBoundedFParam( 0, 1024 );
 Gravity := TPointFParam.Create;
 Spread := CreateBoundedFParam( 0, 360, TRUE );
 Direction := CreateBoundedFParam( 0, 360, TRUE );
end;

destructor TParticleEmitter.Destroy;
begin
 SetLength( FParticleParam.ArrayColor, 0 ) ;
 SetLength( FParticleParam.ArraySpin, 0 ) ;
 SetLength( FParticleParam.ArraySize, 0 ) ;
 SetLength( FParticleParam.ArrayVelocity, 0 ) ;
 SetLength( FParticleParam.ArrayAVelocity, 0 ) ;
 TextureManager.Delete( FParticleParam.Texture );
 ParticlesToEmit.Free;
 Gravity.Free;
 Spread.Free;
 Direction.Free;
 inherited Destroy;
end;

function TParticleEmitter.GetPPVelocity: single;
begin
 Result := FParticleParam.Velocity;
end;

procedure TParticleEmitter.SetPPVelocity(AValue: single);
begin
 FParticleParam.Velocity := AValue;
end;

procedure TParticleEmitter.Update(const aElapsedTime: single);
var i, PToCreate, _Spread : integer ;
    iLife, coeff, Dir, Vel : single ;
    pc, nc : PPColor ;
    ps, ns : PPSingle ;
begin
 inherited Update( aElapsedTime );
 if FFreeze then exit;
 // update parameters
 Gravity.OnElapse( aElapsedTime );
 ParticlesToEmit.OnElapse( aElapsedTime );
 Spread.OnElapse( aElapsedTime );
 Direction.OnElapse( aElapsedTime );
 _Spread := round( Spread.Value );
 // particle move
 for i:=0 to MAX_PARTICLES-1 do
 with FParticles[i], FParticleParam do
  begin
   if CurrentLife > 0
     then begin
           CurrentLife -= aElapsedTime ;
           if CurrentLife <= 0
             then begin
                   dec(FParticleCount);
                  end
             else begin
                   iLife := (InitialLife - CurrentLife) / InitialLife;
                   // frame index
                   FrameIndex := FrameIndex + FrameCoeff * aElapsedTime;
                   // color
                   while ArrayColor[FColorIndex].Life < iLife do
                    inc(FColorIndex) ;
                   pc := @ArrayColor[FColorIndex-1] ;
                   nc := @ArrayColor[FColorIndex] ;
                   coeff := (iLife - pc^.Life) / (nc^.Life - pc^.Life) ;
                   CurrentColor.red := pc^.C.red + round( ( nc^.C.red - pc^.C.red ) * coeff ) ;
                   CurrentColor.green := pc^.C.green + round( ( nc^.C.green - pc^.C.green ) * coeff ) ;
                   CurrentColor.blue := pc^.C.blue + round( ( nc^.C.blue - pc^.C.blue ) * coeff ) ;
                   CurrentColor.alpha := pc^.C.alpha + round( ( nc^.C.alpha - pc^.C.alpha ) * coeff ) ;
                   // size
                   while ArraySize[FSizeIndex].Life < iLife do inc(FSizeIndex) ;
                   ps := @ArraySize[FSizeIndex-1] ;
                   ns := @ArraySize[FSizeIndex] ;
                   CurrentSize := InitialSize * ( ps^.Value + ( ns^.Value - ps^.Value ) * ( iLife - ps^.Life ) / ( ns^.Life - ps^.Life ) ) ;
                   // Spin
                   while ArraySpin[FSpinIndex].Life < iLife do inc(FSpinIndex) ;
                   ps := @ArraySpin[FSpinIndex-1] ;
                   ns := @ArraySpin[FSpinIndex] ;
                   CurrentAngle := CurrentAngle + InitialSpin * ( ps^.Value + ( ns^.Value - ps^.Value ) * ( iLife - ps^.Life ) / ( ns^.Life - ps^.Life ) ) * aElapsedTime ;
                   // Velocity
                   while ArrayVelocity[FVelocityIndex].Life < iLife do inc(FVelocityIndex) ;
                   ps := @ArrayVelocity[FVelocityIndex-1] ;
                   ns := @ArrayVelocity[FVelocityIndex] ;
                   CurrentVelocity := InitialVelocity + InitialVelocity * ( ps^.Value + ( ns^.Value - ps^.Value ) * ( iLife - ps^.Life ) / ( ns^.Life - ps^.Life ) ) ;
                   // Angular Velocity
                   while ArrayAVelocity[ FAVelocityIndex ].Life < iLife do INC( FAVelocityIndex );
                   ps     := @ArrayAVelocity[ FAVelocityIndex-1 ];
                   ns     := @ArrayAVelocity[ FAVelocityIndex ];
                   CurrentAVelocity := InitialAVelocity + InitialAVelocity * ( ps^.Value + ( ns^.Value - ps^.Value ) * ( iLife - ps^.Life ) / ( ns^.Life - ps^.Life ) );
                   CurrentDirection += CurrentAVelocity * aElapsedTime ;

                   VX := cos( CurrentDirection {* deg2rad} ) * CurrentVelocity ;
                   VY := sin( CurrentDirection {* deg2rad} ) * CurrentVelocity ;
                   // Gravity
                   X += ( VX + Gravity.x.Value ) * aElapsedTime ;
                   Y += ( VY + Gravity.y.Value ) * aElapsedTime ;
                  end;
          end;
  end;
 // Emitter life
 FCurrentLife -= aElapsedTime ;
 if ( FCurrentLife <= 0 ) and not LoopMode
   then begin
         FCurrentLife := 0 ;
         exit ;
        end;
 // create new particles
 if ParticlesToEmit.Value = 0.0 then exit;
 FParticleRate := 1/ParticlesToEmit.Value;
 FParticleRateCount += aElapsedTime ;
 PToCreate := 0 ;
 while FParticleRateCount > FParticleRate do
  begin
   FParticleRateCount -= FParticleRate;
   inc(PToCreate) ;
  end;
  if FParticleCount + PToCreate > MAX_PARTICLES then PToCreate := MAX_PARTICLES - FParticleCount ;
  if PToCreate > 0
    then begin
          FParticleCount += PToCreate ;
          i := 0 ;
          repeat
           while FParticles[i].CurrentLife > 0 do inc( i ) ;
           FParticles[i].InitialLife := FParticleParam.Life + random(round(FParticleParam.LifeVariation*1000))/1000;
           FParticles[i].CurrentLife := FParticles[i].InitialLife ;

           FParticles[i].FrameIndex := FParticleParam.StartFrame;
           FParticles[i].FrameCoeff := (FParticleParam.EndFrame - FParticleParam.StartFrame) / FParticles[i].InitialLife;

           FParticles[i].CurrentColor := FParticleParam.ArrayColor[0].C ;
           FParticles[i].FColorIndex := 1 ;

           FParticles[i].CurrentAngle := FParticleParam.StartAngle + random( round(FParticleParam.StartAngleVariation*1000) )/1000 ;

           FParticles[i].InitialSpin := FParticleParam.Spin + random( round(FParticleParam.SpinVariation*1000) )/1000 ;
           FParticles[i].FSpinIndex := 1 ;

           FParticles[i].InitialSize := FParticleParam.Size + random( round(FParticleParam.SizeVariation*1000) )/1000 ;
           FParticles[i].InitialSize := FParticles[i].InitialSize * FParticleParam.ArraySize[0].Value ;
           FParticles[i].CurrentSize := FParticles[i].InitialSize ;
           FParticles[i].FSizeIndex := 1 ;

           FParticles[i].X := x.Value + random( Width ) ;
           FParticles[i].Y := y.Value + random( Height ) ;

           FParticles[i].CurrentDirection := ( Direction.value + random( _Spread ) - _Spread / 2 ) * 2 * PI / 360 ;

           FParticles[i].InitialVelocity := FParticleParam.Velocity + random( round( FParticleParam.VelocityVariation ) ) - FParticleParam.VelocityVariation / 2 ;
           FParticles[i].CurrentVelocity := FParticles[i].InitialVelocity + FParticles[i].InitialVelocity * FParticleParam.ArrayVelocity[0].Value ;
           FParticles[i].FVelocityIndex := 1 ;

           FParticles[i].InitialAVelocity := FParticleParam.AVelocity + random( round( FParticleParam.AVelocityVariation ) ) - FParticleParam.AVelocityVariation / 2 ;
           FParticles[i].CurrentAVelocity := FParticles[i].InitialAVelocity + FParticles[i].InitialAVelocity * FParticleParam.ArrayAVelocity[0].Value ;
           FParticles[i].FAVelocityIndex := 1 ;

           inc( i ) ;
           dec( PToCreate ) ;
          until PToCreate = 0 ;
         end;
end;

procedure TParticleEmitter.Draw(const aLayerPercentOpacity: single);
var i: integer ;
    a : byte ;
    aa: single;
begin
 if FParticleCount = 0
   then begin
         {$IFDEF DEBUG_MODE_ON}
           DrawBox(X.Value, Y.Value, Width, Height, BGRA(255,0,0));
         {$ENDIF}
         exit ;
   end;
 if FParticleParam.Texture = NIL then exit ;
 SetBlendMode( FParticleParam.BlendMode );

 TextureManager.BindTexture( FParticleParam.Texture );

 if FParticleParam.ColorMode = FX_COLOR_SET
   then begin
         glTexEnvi( GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_COMBINE_ARB );
         glTexEnvi( GL_TEXTURE_ENV, GL_COMBINE_RGB_ARB,  GL_REPLACE );
         glTexEnvi( GL_TEXTURE_ENV, GL_SOURCE0_RGB_ARB,  GL_PRIMARY_COLOR_ARB );
        end
   else glTexEnvi( GL_TEXTURE_ENV, GL_TEXTURE_ENV_MODE, GL_MODULATE );

 aa := Opacity.Value/255;

 for i:=0 to MAX_PARTICLES-1 do
  with FParticles[i] do
   if CurrentLife > 0
     then begin
           a := CurrentColor.alpha;
           CurrentColor.alpha := round ( CurrentColor.alpha * aLayerPercentOpacity * aa );
           DrawParticleTexture( FParticleParam.Texture, round( FrameIndex ), CurrentColor, X, Y, CurrentAngle, CurrentSize );
           CurrentColor.alpha := a;
     end;

 {$IFDEF DEBUG_MODE_ON}
   DrawBox(X.Value, Y.Value, Width, Height, BGRA(255,0,0));
 {$ENDIF}
end;

procedure TParticleEmitter.LoadFromFile(const aFilename: string);
var F: TStringList;
    i, k, count :integer;
    SplittedText: ArrayOfString;
begin
 Clear;
 F := TStringList.Create;
 try
   F.LoadFromFile( aFilename );

   k := F.IndexOf('E_Coor');
   SplittedText := SplitLineToStringArray( F.Strings[k+1], ' ' );
   x.Value := StrToFloat( SplittedText[0] );
   y.Value := StrToFloat( SplittedText[1] );

   k := F.IndexOf('E_Size');
   SplittedText := SplitLineToStringArray( F.Strings[k+1], ' ' );
   FWidth := StrToInt( SplittedText[0] );
   FHeight := StrToInt( SplittedText[1] );

   k := F.IndexOf('E_Direction');
   Direction.Value := StrToFloat( F.Strings[k+1] );

   k := F.IndexOf('E_Spread');
   Spread.Value := StrToFloat( F.Strings[k+1] );

   k := F.IndexOf('E_Life');
   InitialLife := StrToFloat( F.Strings[k+1] );

   k := F.IndexOf('E_LoopMode');
   LoopMode := (F.Strings[k+1] = 'TRUE') or (F.Strings[k+1] = 'True') or (F.Strings[k+1] = 'true');

   k := F.IndexOf('E_Emission');
   ParticlesToEmit.Value := StrToInt( F.Strings[k+1] );
   FParticleRate := 1/ParticlesToEmit.Value;
   FParticleRateCount := 0;

   k := F.IndexOf('E_Gravity');
   SplittedText := SplitLineToStringArray( F.Strings[k+1], ' ' );
   Gravity.x.Value := StrToFloat( SplittedText[0] );
   Gravity.y.Value := StrToFloat( SplittedText[1] );

   k := F.IndexOf('P_Texture');
   SplittedText := SplitLineToStringArray( F.Strings[k+2], ' ' );
   FParticleParam.Texture := TextureManager.Add( ExtractFilePath( aFilename ) + F.Strings[k+1],
                                                 strtoint( SplittedText[0] ), strtoint( SplittedText[1] ));

   k := F.IndexOf('P_Frame');   // StartFrameIndex EndFrameIndex
   SplittedText := SplitLineToStringArray( F.Strings[k+1], ' ' );
   FParticleParam.StartFrame := StrToInt( SplittedText[0] );
   FParticleParam.EndFrame := StrToInt( SplittedText[1] );

   k := F.IndexOf('P_Life');  // Life  LifeVariation
   SplittedText := SplitLineToStringArray( F.Strings[k+1], ' ' );
   FParticleParam.Life := StrToFloat( SplittedText[0] );
   FParticleParam.LifeVariation := StrToFloat( SplittedText[1] );

   k := F.IndexOf('P_ColorMode');
   FParticleParam.ColorMode := StrToint( F.Strings[k+1] );

   k := F.IndexOf('P_BlendMode');
   FParticleParam.BlendMode := StrToint( F.Strings[k+1] );

   k := F.IndexOf('P_Color');    // count   lifepos red green blue alpha    life red green...
   SplittedText := SplitLineToStringArray( F.Strings[k+1], ' ' );
   count := StrToInt( SplittedText[0] );
   SetLength( FParticleParam.ArrayColor, count );
   k := 1 ;
   for i:=0 to count-1 do
    begin
     FParticleParam.ArrayColor[i].Life := StrToFloat( SplittedText[k] ) ;
     FParticleParam.ArrayColor[i].C.red := StrToInt( SplittedText[k+1] ) ;
     FParticleParam.ArrayColor[i].C.green := StrToInt( SplittedText[k+2] ) ;
     FParticleParam.ArrayColor[i].C.blue := StrToInt( SplittedText[k+3] ) ;
     FParticleParam.ArrayColor[i].C.alpha := StrToInt( SplittedText[k+4] ) ;
     inc(k,5);
    end;

   k := F.IndexOf('P_Velocity');  // velocity velocityVariation count   lifepos value   lifepos value...
   SplittedText := SplitLineToStringArray( F.Strings[k+1], ' ' );
   FParticleParam.Velocity := StrToFloat( SplittedText[0] );
   FParticleParam.VelocityVariation := StrToFloat( SplittedText[1] );
   count := StrToInt( SplittedText[2] );
   SetLength( FParticleParam.ArrayVelocity, count );
   k := 3;
   for i:=0 to count-1 do
    begin
     FParticleParam.ArrayVelocity[i].Life := StrToFloat( SplittedText[k] );
     FParticleParam.ArrayVelocity[i].Value := StrToFloat( SplittedText[k+1] );
     inc(k,2);
    end;

   k := F.IndexOf('P_AVelocity');     // AngleVelocity AngleVelocityVariation Count   lifepos value   lifepos value...
   SplittedText := SplitLineToStringArray( F.Strings[k+1], ' ' );
   FParticleParam.AVelocity := StrToFloat( SplittedText[0] );
   FParticleParam.AVelocityVariation := StrToFloat( SplittedText[1] );
   count := StrToInt( SplittedText[2] );
   SetLength( FParticleParam.ArrayAVelocity, count );
   k := 3;
   for i:=0 to count-1 do
    begin
     FParticleParam.ArrayAVelocity[i].Life := StrToFloat( SplittedText[k] );
     FParticleParam.ArrayAVelocity[i].Value := StrToFloat( SplittedText[k+1] );
     inc(k,2);
    end;

   k := F.IndexOf('P_Size');   // Size SizeVariation Count   lifepos value   lifepos value...
   SplittedText := SplitLineToStringArray( F.Strings[k+1], ' ' );
   FParticleParam.Size := StrToFloat( SplittedText[0] );
   FParticleParam.SizeVariation := StrToFloat( SplittedText[1] );
   count := StrToInt( SplittedText[2] );
   SetLength( FParticleParam.ArraySize, count );
   k := 3;
   for i:=0 to count-1 do
    begin
     FParticleParam.ArraySize[i].Life := StrToFloat( SplittedText[k] );
     FParticleParam.ArraySize[i].Value := StrToFloat( SplittedText[k+1] );
     inc(k,2);
    end;

   k := F.IndexOf('P_StartAngle');  // StartAngle StartAngleVariation
   SplittedText := SplitLineToStringArray( F.Strings[k+1], ' ' );
   FParticleParam.StartAngle := StrToFloat( SplittedText[0] );
   FParticleParam.StartAngleVariation := StrToFloat( SplittedText[1] );

   k := F.IndexOf('P_Spin');    // Spin SpinVariation Count   lifepos value   lifepos value...
   SplittedText := SplitLineToStringArray( F.Strings[k+1], ' ' );
   FParticleParam.Spin := StrToFloat( SplittedText[0] );
   FParticleParam.SpinVariation := StrToFloat( SplittedText[1] );
   count := StrToInt( SplittedText[2] );
   SetLength( FParticleParam.ArraySpin, count );
   k := 3;
   for i:=0 to count-1 do
    begin
     FParticleParam.ArraySpin[i].Life := StrToFloat( SplittedText[k] );
     FParticleParam.ArraySpin[i].Value := StrToFloat( SplittedText[k+1] );
     inc(k,2);
    end;
 finally
  F.Free ;
 end;
end;

function TParticleEmitter.GetWidth: integer;
begin
 Result := FWidth;
end;

function TParticleEmitter.GetHeight: integer;
begin
 Result := FHeight;
end;

procedure TParticleEmitter.DrawParticleTexture(aTexture: PTexture;
  AFrameIndex: integer; aTint: TBGRAPixel; aX, aY, aAngle, aZoom: single);
var
  quad : array[ 0..3 ] of TPointF;
  TexCoord : TTexCoor;
  aW, aH : Single ;
  x1, x2 : Single;
  y1, y2 : Single;
  cX, cY : Single;
  c, s   : Single;
begin

 if aTexture = nil then exit;
 aW := aTexture^.FrameWidth;
 aH := aTexture^.FrameHeight;

 TexCoord := aTexture^.FramesCoord[AFrameIndex];

 // Zoom
 if aZoom <> 1 Then
   begin
     aX := aX + ( aW - aW * aZoom ) / 2 ;
     aY := aY + ( aH - aH * aZoom ) / 2 ;
     aW := aW * aZoom ;
     aH := aH * aZoom ;
   end;
 // Angle
 if aAngle <> 0
   then begin
         x1 := -aW / 2;
         y1 := -aH / 2;
         x2 := -x1;
         y2 := -y1;
         cX :=  aX + x2;
         cY :=  aY + y2;
         sincos( aAngle * deg2rad, s, c );//m_SinCos( aAngle * deg2rad, s, c );
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

 glColor4ub( aTint.red, aTint.green, aTint.blue, aTint.alpha );
 glBegin( GL_QUADS );
  glTexCoord2fv( @TexCoord[0] );  glVertex2fv( @quad[ 0 ] );
  glTexCoord2fv( @TexCoord[1] );  glVertex2fv( @quad[ 1 ] );
  glTexCoord2fv( @TexCoord[2] );  glVertex2fv( @quad[ 2 ] );
  glTexCoord2fv( @TexCoord[3] );  glVertex2fv( @quad[ 3 ] );
 glEnd;
end;

procedure TParticleEmitter.Shoot;
begin
 FCurrentLife := InitialLife ;
 FParticleRateCount := 0 ;
end;

procedure TParticleEmitter.Clear;
var i: integer ;
begin
 for i:=0 to MAX_PARTICLES-1 do
  begin
   FParticles[i].CurrentLife := 0 ;
   FParticles[i].InitialLife := 0 ;
  end;
 FParticleCount := 0 ;
end;

procedure TParticleEmitter.SetSize(AWidth, AHeight: integer);
begin
 FWidth := AWidth;
 FHeight := AHeight;
end;


{ TGlow }

procedure TGlow.SetGlowAttenuation(AValue: single);
begin
 if AValue < 0.01 then AValue := 0.01;
 FGlowAttenuation:=AValue;
end;

function TGlow.GetWidth: integer;
begin
 Result := round( Radius * 2 );
end;

function TGlow.GetHeight: integer;
begin
 Result := round( Radius * 2 );
end;

procedure TGlow.Draw(const aLayerPercentOpacity: single);
var OldBlendMode : byte ;
  r: single;
begin
 if not FVisible then exit;


 OldBlendMode := _FglCurrentBlendMode;
 SetBlendMode( FBlendMode );

 r := Radius * Scale.x.Value;

// glPushMatrix;
// glTranslatef( X, Y, 0);

 with ShaderManager do begin
  Use( FParentScene.FShaderGlow );
  SetUniform2f( FLocGlowCenter, x.Value, FParentScene.Height - y.Value);
  SetUniform4f( FLocGlowColor, Tint.Red.Value/255, Tint.Green.Value/255, Tint.Blue.Value/255, Tint.Alpha.Value/255*aLayerPercentOpacity );
  SetUniform1f( FLocGlowRadius, r );
  SetUniform1f( FLocGlowAttenuation, Attenuation );
 end;

 glBegin( GL_QUADS ) ;
  glVertex2f( x.Value-r, y.Value-r );
  glVertex2f( x.Value+r, y.Value-r );
  glVertex2f( x.Value+r, y.Value+r );
  glVertex2f( x.Value-r, y.Value+r );
 glEnd ;

 SetBlendMode( OldBlendMode );

 ShaderManager.UseNone;


 {$IFDEF DEBUG_MODE_ON}
   DrawBox(X.Value-Radius, Y.Value-Radius, Radius*2, Radius*2, BGRA(255,0,0));
 {$ENDIF}
// glPopMatrix;

end;

constructor TGlow.Create(aX, aY, aRadius: single; aColor: TBGRAPixel;
  aBlendMode: byte);
begin
 inherited Create;
 x.Value := aX;
 y.Value := aY;
 Radius := aRadius;
 FGlowAttenuation := 2;
 Tint.Value := aColor;
 FBlendMode := aBlendMode;

 if not FLocInitDone
   then begin
    FLocInitDone := TRUE;
    FLocGlowCenter := ShaderManager.GetUniform( 0, 'lightCenter' ) ;
    FLocGlowColor := ShaderManager.GetUniform( 0, 'lightColor' ) ;
    FLocGlowRadius := ShaderManager.GetUniform( 0, 'lightRadius' ) ;
    FLocGlowAttenuation := ShaderManager.GetUniform( 0, 'powerAttenuation' ) ;
end;
end;

{ TGuiProgressBar }

constructor TGuiProgressBar.Create(aX, aY: single; aWidth, aHeight: integer);
begin
 inherited Create( aX, aY, aWidth, aHeight );

 BackGroundColor := TBGRAParam.Create;
 BackGroundColor.Value := BGRA(10,5,3);

 BorderColor := TBGRAParam.Create;
 BorderColor.Value := BGRA(150,150,150);

 FMode := sbmHorizontal;
 FPercent := 0.0;
end;

destructor TGuiProgressBar.Destroy;
begin
 FreeAndNil( BackGroundColor );
 FreeAndNil( BorderColor );
 inherited Destroy;
end;

procedure TGuiProgressBar.Update(const aElapsedTime: single);
begin
 BackGroundColor.OnElapse( aElapsedTime );
 BorderColor.OnElapse( aElapsedTime );
 inherited Update(aElapsedTime);
end;

procedure TGuiProgressBar.Draw(const aLayerPercentOpacity: single);
var xx, yy, ww, hh: single;
begin
 if FMode = sbmHorizontal
   then begin
     yy := 0;
     hh := Height;
     if FFlipH
       then begin
        xx := 0;
        ww := Width * ( 1 - FPercent );
       end else begin
        xx := Width * FPercent;
        ww := Width - xx;
       end;
   end
   else begin
     xx := 0;
     ww := Width;
     if not FFlipV
       then begin
        yy := 0;
        hh := Height * ( 1 - FPercent );
       end else begin
        yy := Height * FPercent;
        hh := Height - yy;
       end;
   end;


 // Skew is ignored

 PushAndPrepareMatrix;
 SetBlendMode( FBlendMode );
 TextureManager.DisableTextureUsage;

 if Tint.alpha.Value < 255
   then begin
         FillBox( 0, 0, Width, Height, // fill entire bar
                  TopLeftColor.Value, TopRightColor.Value, BottomRightColor.Value,
                  BottomLeftColor.Value, aLayerPercentOpacity );
         FillBox( xx, yy, ww, hh, BackGroundColor.Value, aLayerPercentOpacity ); // background
         DrawBox( 0, 0, Width, Height, BorderColor.Value, aLayerPercentOpacity ); // border
   end;

 if Tint.alpha.Value <> 0
   then begin  //Apply Tint
         FillBox( 0, 0, Width, Height, Tint.Value, aLayerPercentOpacity * Opacity.Value / 255 );
        end;

 {$IFDEF DEBUG_MODE_ON}
   DrawBox(0, 0, Width, Height, BGRA(255,0,0));
 {$ENDIF}

 DrawChildSurfaces( aLayerPercentOpacity );
 PopMatrix;
end;

{ TGuiLabel }

constructor TGuiLabel.Create;
begin
 inherited Create ;
 FTexturedFont := NIL ;
 FCaption := TS8.Create;
 FWidth := 0;
 FHeight := 0;
end;

destructor TGuiLabel.Destroy;
begin
 FreeAndNil( FCaption );
 inherited Destroy;
end;

function TGuiLabel.GetCaption: string;
begin
 Result := FCaption.All;
end;

procedure TGuiLabel.SetCaption(AValue: string);
var i: integer;
begin
 FCaption.InitWith( AValue );
 FWidth := 0;
 for i:=1 to FCaption.CharCount do
  FWidth += FTexturedFont.GetCharWidth( FCaption[i][1] );
 FHeight := FTexturedFont.GetCharHeight( FCaption[1][1] );
end;

function TGuiLabel.GetWidth: integer;
begin
 Result := FWidth;
end;

function TGuiLabel.GetHeight: integer;
begin
 Result := FHeight;
end;

procedure TGuiLabel.Draw(const aLayerPercentOpacity: single);
var xx: single ;
    i, cw : integer ;
    PT: PTexture;
begin
 if FCaption = '' then exit ;
 if FTexturedFont = NIL then exit ;
 xx := x.Value;
 for i:=1 to FCaption.CharCount do
  begin
   PT := FTexturedFont.GetCharTexture( FCaption[i][1] ) ;
   if PT <> NIL
     then begin
           SetBlendMode( FBlendMode );
           DrawTexture( PT, 0, xx, y.Value, self, aLayerPercentOpacity);//, ParentScale );

           //cw := FTexturedFont.GetCharWidth( FCaption[i][1] );
           cw := PT^.FrameWidth;
           xx += cw + cw * (Scale.x.Value-1.0);
          end;
  end;

 {$IFDEF DEBUG_MODE_ON}
   DrawBox(X.Value, X.Value, xx - X.Value, Height, BGRA(255,0,0));
 {$ENDIF}
end;



{ TGuiLabel }
{
constructor TGuiLabel.Create(aText: string; aX, aY: single;
  aFontHeight: integer; aFontColor, aOutLineColor, aShadowColor: TBGRAPixel;
  aFontName: string; aFontStyle: TFontStyles);
var i : integer ;
  xx , yy : single ;
  txt : string ;
  renderer: TBGRATextEffectFontRenderer;
begin
 FImaTampon:=TBGRABitmap.Create(1, 1);
 renderer := TBGRATextEffectFontRenderer.Create;
 FImaTampon.FontRenderer := renderer;
 renderer.ShadowVisible := aShadowColor<>BGRAPixelTransparent ;
 renderer.ShadowColor := aShadowColor;
 renderer.ShadowOffset.x := round(aFontHeight*0.06);
 renderer.ShadowOffset.y := round(aFontHeight*0.06);
 renderer.ShadowRadius := round(aFontHeight*0.04);
 renderer.OutlineVisible := aOutLineColor<>BGRAPixelTransparent ;
 renderer.OutlineColor := aOutLineColor;
 renderer.OuterOutlineOnly := True;
 renderer.OutlineWidth := aFontHeight*0.04;
 FImaTampon.FontQuality:= fqFineAntialiasing;
 FImaTampon.FontHeight := aFontHeight ;
 FImaTampon.FontName := aFontName ;
 FImaTampon.FontStyle := aFontStyle ;
 FImaTampon.SetSize( FImaTampon.TextSize(aText).cx, FImaTampon.TextSize(aText).cy);
 FImaTampon.Fill( BGRAPixelTransparent ) ;
 FImaTampon.TextOut ( 0 , 0 , aText , aFontColor ) ;
 inherited Create( FImaTampon );
 FImaTampon.Free;
 SetCoordinate ( aX, aY );
end;

Constructor TGuiLabel.Create ( aText: string; aX, aY, aWidth, aHeight, aFontHeight : integer;
                               aFontColor, aOutLineColor, aShadowColor: TBGRAPixel;
                               aFontName:string='Arial'; aFontStyle : TFontStyles=[]; aAlignment : TAlignment=alCenter ) ;
var i  : integer ;
    xx , yy : integer ;
    largmot : integer ;
    largespace : integer ;
    line : string ;
    indexligne : integer ;
begin
 FWord := TStringList.Create;
 FViewableLine := TStringList.Create ;
 SeparateTheWordsOfTheText( aText, FWord, ' ' ) ;
 SetLength ( ArrayXYWord , FWord.Count ) ;
 FHauteurFont := aFontHeight ;
 FFontStyle := aFontStyle ;
 FFontName := aFontName ;
 FX := aX ;
 FY := aY ;
 FWidth := aWidth ;
 FHeight := aHeight ;
 FImaTampon:=TBGRABitmap.Create(aWidth, aHeight,BGRAPixelTransparent);
 FImaTampon.FontName := aFontName ;
 FImaTampon.FontHeight := aFontHeight ;
 FImaTampon.FontStyle := aFontStyle ;

 SimulateDrawing;

 FDecalageOmbre := FHauteurFont * 5 div 100 ;
 if FDecalageOmbre < 3 then FDecalageOmbre := 3 ;
 // word align
 case ord( aAlignment ) of
  ord(alLeft) : LeftAlign ;
  ord(alCenter) : CenterAlign ;
  ord(alRight) : RightAlign ;
  ord (alJustify) : JustifyAlign ;
 end;//case aAlignement
 VerticalAlign ;

 DrawTextOn( FImaTampon, aFontColor, aOutLineColor, aShadowColor );

 inherited Create( FImaTampon );
 FImaTampon.Free;
 SetCoordinate ( aX, aY );
end;

procedure TGuiLabel.SimulateDrawing ;
var
  i  : integer ;
  xx , yy : integer ;
  largword : integer ;
  largespace : integer ;
  line : string ;
  indexline : integer ;
begin
 FViewableLine.Clear ;
 FHauteurTrace := 0 ;
 FLargeurTrace := 0 ;
 if FWord.Count = 0 then exit ;
 line := '' ;
 largespace := FImaTampon.TextSize(' ').cx ;
 xx := -largespace ;
 yy := 0 ;
 indexline := 1 ;
 for i:=1 to FWord.Count do
  begin
   largword := FImaTampon.TextSize( SysToUTF8(FWord.Strings[i-1]) ).cx ;
   if xx+largespace+largword <= FWidth
     then begin // on peut rester sur la même ligne
           ArrayXYWord[i-1].x := xx + largespace ; ArrayXYWord[i-1].y := yy ; ArrayXYWord[i-1].IndexLine := indexline ; ArrayXYWord[i-1].Width := largword ;
           xx := xx + largespace + largword ;
           if line='' then line := FWord.Strings[i-1]
                      else line := line + ' ' + FWord.Strings[i-1] ;
          end
     else begin  // on passe à la ligne suivante
           if FLargeurTrace < xx then FLargeurTrace := xx ;
           yy := yy + FImaTampon.TextSize( SysToUTF8(FWord.Strings[i-1]) ).cy ;
           inc ( indexline ) ;
           ArrayXYWord[i-1].x := 0 ; ArrayXYWord[i-1].y := yy ; ArrayXYWord[i-1].IndexLine := indexline ; ArrayXYWord[i-1].Width := largword ;
           xx := largword ;
           FHauteurTrace := FHauteurTrace + FImaTampon.TextSize( SysToUTF8(FWord.Strings[i-1]) ).cy ;
           FViewableLine.Add ( line ) ;
           line := FWord.Strings[i-1] ;
          end;
  end;
 if line <> ''
   then begin
         FViewableLine.Add ( line ) ;
         FHauteurTrace := FHauteurTrace + FImaTampon.TextSize( SysToUTF8(line) ).cy ;
         if FLargeurTrace < xx then FLargeurTrace := xx ;
        end;
end;


destructor TGuiLabel.Destroy;
begin
 FWord.Free ;
 FViewableLine.Free ;
 SetLength ( ArrayXYWord, 0 ) ;
 inherited Destroy;
end;

procedure TGuiLabel.SeparateTheWordsOfTheText(aTxt: string; temp: TStringList; aSeparator: char);
var
 i : integer ;
 t : string ;
begin
 temp.Clear ;
 if length ( aTxt ) = 0 then exit ;
 i := 1 ;
 t := '' ;
 repeat
  if aTxt[i] <> aSeparator then t := t + aTxt[i] ;
  if ( aTxt[i] = aSeparator ) and ( length ( t ) > 0  )
    then begin
          temp.Add ( t ) ;
          t := '' ;
         end;
  inc (i) ;
 until i > length ( aTxt ) ;
 if t <> '' then temp.Add ( t ) ;
end;

procedure TGuiLabel.LeftAlign;
var i , j : integer ;
  w : integer ;
  istart , iend : integer ;
begin
 for i:=1 to FViewableLine.Count do
  begin
   FindWordIndex ( i , istart , iend ) ;
   w := 0 ;
   for j:=istart to iend do
    begin
     ArrayXYWord[j].x := w ;
     w := w + ArrayXYWord[j].Width + FImaTampon.TextSize ( ' ' ).cx ;
    end;
  end;
end;

procedure TGuiLabel.CenterAlign;
var i , j : integer ;
  w : integer ;
  istart , iend : integer ;
begin
 for i:=1 to FViewableLine.Count do
  begin
   FindWordIndex ( i , istart , iend ) ;
   w := ArrayXYWord[iend].x + ArrayXYWord[iend].Width - ArrayXYWord[istart].x ; // largeur totale de la ligne
   w := ( FWidth - w ) div 2 ;
   for j:=istart to iend do
    begin
     ArrayXYWord[j].x := w ;
     w := w + ArrayXYWord[j].Width + FImaTampon.TextSize ( ' ' ).cx ;
    end;
  end;
end;

procedure TGuiLabel.RightAlign;
var i , j : integer ;
  w : integer ;
  istart , iend : integer ;
begin
 for i:=1 to FViewableLine.Count do
  begin
   FindWordIndex ( i , istart , iend ) ;
   w := FWidth + FImaTampon.TextSize ( ' ' ).cx ;
   for j:=iend downto istart do
    begin
     w := w - ArrayXYWord[j].Width - FImaTampon.TextSize ( ' ' ).cx ;
     ArrayXYWord[j].x := w ;
    end;
  end;
end;

procedure TGuiLabel.JustifyAlign;
begin

end;

procedure TGuiLabel.VerticalAlign;
var i , j : integer ;
  istart , iend : integer ;
  h : integer ;
  yy : integer ;
begin
 h := 0 ;
 for i:=1 to FViewableLine.Count do h := h + FImaTampon.TextSize ( SysToUTF8(FViewableLine.Strings[i-1]) ).cy ;
 yy := ( FHeight - h ) div 2 ;
 for i:=1 to FViewableLine.Count do
  begin
   FindWordIndex ( i , istart , iend ) ;
   for j:=istart to iend do ArrayXYWord[j].y := yy ;
   yy := yy + FImaTampon.TextSize ( SysToUTF8(FViewableLine.Strings[i-1]) ).cy ;
  end;
end;

procedure TGuiLabel.FindWordIndex(aLine: integer; var iStart, iEnd: integer);
var i : integer ;
begin
 iStart := -1 ;
 iEnd := -1 ;
 for i:=low(ArrayXYWord) to high(ArrayXYWord) do
  begin
   if ArrayXYWord[i].IndexLine = aLine
     then begin
           if iStart = -1 then iStart := i ;
           iEnd := i ;
          end
     else if iStart <> -1 then exit ;
  end;
end;

procedure TGuiLabel.DrawTextOn(aImage: TBGRABitmap;
                               aFontColor: TBGRAPixel; aOutLineColor: TBGRAPixel; aShadowColor: TBGRAPixel);
var i : integer ;
  xx , yy : single ;
  txt : string ;
  renderer: TBGRATextEffectFontRenderer;
begin
 renderer := TBGRATextEffectFontRenderer.Create;
 aImage.FontRenderer := renderer;
 renderer.ShadowVisible := aShadowColor<>BGRAPixelTransparent ;
 renderer.ShadowColor := aShadowColor;
 renderer.ShadowOffset.x := round(FHauteurFont*0.06);
 renderer.ShadowOffset.y := round(FHauteurFont*0.06);
 renderer.ShadowRadius := round(FHauteurFont*0.04);
 renderer.OutlineVisible := aOutLineColor<>BGRAPixelTransparent ;
 renderer.OutlineColor := aOutLineColor;
 renderer.OuterOutlineOnly := True;
 renderer.OutlineWidth := FHauteurFont*0.04;
 aImage.FontQuality:= fqFineAntialiasing;
 aImage.FontHeight := FHauteurFont ;
 aImage.FontName := FFontName ;
 aImage.FontStyle := FFontStyle ;
 for i:=low(ArrayXYWord) to high(ArrayXYWord) do
  begin
   txt := SysToUTF8( FWord.Strings[i] ) ;
   xx := ArrayXYWord[i].x ;
   yy := ArrayXYWord[i].y ;
   aImage.TextOut ( xx , yy , txt , aFontColor ) ;
  end;
end;


procedure TGuiLabel.DoUpdate(const aElapsedTime: double);
begin
  inherited DoUpdate(aElapsedTime);
end;

}

{ TGuiButton }

procedure TGuiButton.Init;
begin
 FMouseEntered := FALSE ;
 FOnMouseEnter := NIL ;
 FMouseLeave   := NIL ;
 FOnClick      := NIL ;
 FFrame := 0;
// SetFrameLoopBounds(0, 0);
end;

constructor TGuiButton.Create(aX, aY: integer; const aFilename: string);
var T: PTexture;
begin
 T := TextureManager.Add( aFilename );
 inherited Create ( T );
 X.Value := aX;
 Y.Value := aY;
 Init;
end;

constructor TGuiButton.Create(aX, aY: integer; aImage: TBGRABitmap);
var T: PTexture;
begin
 T := TextureManager.Add( aImage );
 inherited Create ( T );
 X.Value := aX ;
 Y.Value := aY ;
 Init;
end;

constructor TGuiButton.Create(aX, aY: single; aCaption: UTF8String; aFont: TTexturedFont; aBackgroundFilename: string);
var ima, back: TBGRABitmap;
    T: PTexture;
begin
 if aBackgroundFilename=''
   then ima := aFont.Text2Bitmap( aCaption, NIL )
   else begin
         back := TBGRABitmap.Create( aBackgroundFilename );
         ima := aFont.Text2Bitmap( aCaption, back ) ;
         back.Free;
        end;
 T := TextureManager.Add( ima );
 inherited Create ( T );
 ima.Free ;
 x.Value := aX;
 y.Value := aY ;
 Init;
end;

procedure TGuiButton.Update(const aElapsedTime: single);
var xx,yy:integer;
begin
 inherited Update(aElapsedTime);
 xx := FParentScene.FXMouse;
 yy := FParentScene.FYMouse;
 // Pool mouse activity
 if ( xx >= X.Value ) and ( xx <= GetRightX ) and
    ( yy >= Y.Value ) and ( yy <= GetBottomY )
   then begin
         if not FMouseEntered
           then begin
                 FMouseEntered := TRUE ;
                 Scale.ChangeTo( PointF(1.1,1.1), 0.4, idcStartFastEndSlow );
                 if FOnMouseEnter<>NIL then FOnMouseEnter ( Self );
                end
           else begin
                 if FParentScene.FFlagMouseLeftClicked
                   then begin
                         FParentScene.FFlagMouseLeftClicked := FALSE;
                         Scale.ChangeTo( PointF(0.95,0.95), 0);
                         Scale.ChangeTo(PointF(1,1), 0.4);
                         Tint.Value := BGRA(255,255,150,200);
                         Tint.Alpha.ChangeTo(0,0.4);
                         if FOnClick <> NIL then FOnClick( Self ) ;
                        end;
                end;
        end
   else begin
         if FMouseEntered
           then begin
                 FMouseEntered := FALSE ;
                 Scale.ChangeTo( PointF(1,1), 0.4, idcStartFastEndSlow );
                 if FMouseLeave <> NIL then FMouseLeave( Self ) ;
           end;
        end;
end;


{ TMouseManager }

constructor TMouseManager.Create;
begin
 FSprite := NIL;
 FTexture:= NIL;
end;

destructor TMouseManager.Destroy;
begin
 if FSprite <> NIL then FSprite.Free;
 if FTexture <> NIL then TextureManager.Delete( FTexture );
 inherited Destroy;
end;

procedure TMouseManager.ProcessMouse;
begin
 if FSprite = NIL then exit;
 FSprite.X.Value := FParentScene.FXMouse;
 FSprite.Y.Value := FParentScene.FYMouse;
end;

procedure TMouseManager.DeleteObjects;
begin
 if FTexture <> NIL then TextureManager.Delete( FTexture );
 if FSprite <> NIL then FSprite.Free;
end;

procedure TMouseManager.UpDate(AElapsedTime: single);
begin
 if FSprite <> NIL then FSprite.Update( AElapsedTime );
end;

procedure TMouseManager.SetCursor(const aFilename: string);
begin
 DeleteObjects;

 FTexture := TextureManager.Add( aFilename );
 FSprite := TSprite.Create( FTexture );

 FParentScene.FOGLC.Cursor := crNone ;
end;

procedure TMouseManager.SetCursor(const aFilename: string; aFrameWidth, aFrameHeight: integer);
begin
 DeleteObjects;

 FTexture := TextureManager.Add( aFilename, aFrameWidth, aFrameHeight );
 FSprite := TSprite.Create( FTexture );

 FParentScene.FOGLC.Cursor := crNone ;
end;

procedure TMouseManager.SetCursor(aImage: TBGRABitmap);
begin
 DeleteObjects;

 FTexture := TextureManager.Add( aImage );
 FSprite := TSprite.Create( FTexture );

 FParentScene.FOGLC.Cursor := crNone ;
end;

procedure TMouseManager.SetCursor(aImage: TBGRABitmap; aFrameWidth, aFrameHeight: integer);
begin
 DeleteObjects;

 FTexture := TextureManager.Add( aImage, aFrameWidth, aFrameHeight );
 FSprite := TSprite.Create( FTexture );

 FParentScene.FOGLC.Cursor := crNone ;
end;


{ TColorBackground }

constructor TColorBackground.Create(aX, aY: single; aWidth, aHeight: integer);
begin
 inherited Create;
 X.Value := aX;
 Y.Value := aY;
 SetSize( aWidth, aHeight );

 TopLeftColor := TBGRAParam.Create;
 TopRightColor := TBGRAParam.Create;
 BottomRightColor := TBGRAParam.Create;
 BottomLeftColor := TBGRAParam.Create;

 TopLeftColor.Value := BGRA(255,0,0);
 TopRightColor.Value := BGRA(0,255,0);
 BottomRightColor.Value := BGRA(0,0,255);
 BottomLeftColor.Value := BGRA(255,255,0);
end;

destructor TColorBackground.Destroy;
begin
 FreeAndNil( TopLeftColor );
 FreeAndNil( TopRightColor );
 FreeAndNil( BottomRightColor );
 FreeAndNil( BottomLeftColor );
 inherited Destroy;
end;

function TColorBackground.GetWidth: integer;
begin
 Result := FWidth;
end;

function TColorBackground.GetHeight: integer;
begin
 Result := FHeight;
end;

procedure TColorBackground.Update(const aElapsedTime: single);
begin
  inherited Update(aElapsedTime);
  TopLeftColor.OnElapse( aElapsedTime );
  TopRightColor.OnElapse( aElapsedTime );
  BottomRightColor.OnElapse( aElapsedTime );
  BottomLeftColor.OnElapse( aElapsedTime );
end;

procedure TColorBackground.Draw(const aLayerPercentOpacity: single);
begin
 DrawSurface(TopLeftColor.Value, TopRightColor.Value, BottomRightColor.Value, BottomLeftColor.Value, aLayerPercentOpacity );
end;

procedure TColorBackground.SetSize(aWidth, aHeight: integer);
begin
 FWidth := aWidth;
 FHeight := aHeight;
end;

procedure TColorBackground.SetAllColorsTo(AColor: TBGRAPixel);
begin
 TopLeftColor.Value := AColor;
 TopRightColor.Value := AColor;
 BottomRightColor.Value := AColor;
 BottomLeftColor.Value := AColor;
end;

procedure TColorBackground.SetLeftColors(AColor: TBGRAPixel);
begin
 TopLeftColor.Value := AColor;
 BottomLeftColor.Value := AColor;
end;

procedure TColorBackground.SetRightColors(AColor: TBGRAPixel);
begin
 TopRightColor.Value := AColor;
 BottomRightColor.Value := AColor;
end;

procedure TColorBackground.SetTopColors(AColor: TBGRAPixel);
begin
 TopLeftColor.Value := AColor;
 TopRightColor.Value := AColor;
end;

procedure TColorBackground.SetBottomColors(AColor: TBGRAPixel);
begin
 BottomRightColor.Value := AColor;
 BottomLeftColor.Value := AColor;
end;

{ TSprite }

constructor TSprite.create(ATexture: PTexture);
begin
 inherited Create;
 Init;
 SetLength ( FTextureList , 1 );
 FTextureList[0] := ATexture;
 FTextureIndex := 0;
end;


destructor TSprite.Destroy;
begin
 SetLength( FTextureList,0 );
 inherited Destroy;
end;

procedure TSprite.Init;
begin
 SetLength ( FTextureList , 0 ) ;
end;

procedure TSprite.Draw(const aLayerPercentOpacity : single);
begin
 if GetTextureCount = 0 then exit;
 if (FTextureIndex < 0 ) or (FTextureIndex >= GetTextureCount) then exit;

 DrawSurface( FTextureList[FTextureIndex], trunc( FFrame ), aLayerPercentOpacity );
end;

function TSprite.GetTextureCount: integer;
begin
 Result := Length( FTextureList );
end;

procedure TSprite.SetTextureIndex(AValue: integer);
begin
 AValue := AValue mod Length( FTextureList );
 FTextureIndex:=AValue;
end;

function TSprite.GetWidth: integer;
begin
 Result := FTextureList[FTextureIndex]^.FrameWidth;
end;

function TSprite.GetHeight: integer;
begin
 Result := FTextureList[FTextureIndex]^.FrameHeight;
end;

function TSprite.AddTexture(ATexture: PTexture): integer;
var i: integer;
begin
 if ATexture = NIL then exit;
 i := GetTextureCount;
 SetLength(FTextureList, i + 1 );
 FTextureList[i] := ATexture;
 Result := i;
end;

procedure TSprite.ClearAllTexture;
begin
 SetLength( FTextureList , 0 );
end;



{ TSimpleSurfaceWithEffect }

constructor TSimpleSurfaceWithEffect.Create;
begin
 inherited Create ;
 Init;
end;

procedure TSimpleSurfaceWithEffect.Init;
begin
 FBlendMode := FX_BLEND_NORMAL;
 FFreeze := FALSE ;
 FTimeBeforeKill := 0.0;
 Speed := TPointFParam.Create;
 // Blink
 FBlink.BlinkToGo := 0 ;
 // Rotation Around Axis
 FRotationAroundPt.InProgress := FALSE ;

 Angle:= TFParam.Create;
 Opacity := CreateBoundedFParam(0, 255);
 Opacity.Value := 255;
 Tint := TBGRAParam.Create;
 Tint.Value := BGRA(0,0,0,0);
 Scale := TPointFParam.Create;
 Scale.Value := PointF(1, 1);
 Skew := TPointFParam.Create;
 // Flip
 FFlipH := FALSE;
 FFlipV := FALSE;
 // Bind
 FBindToSprite.InProgress:=FALSE;
 FBindToSprite.ParentSurface:=NIL;
 // Frame animation
 FFrame := 0;
 FFrameDeltaPerSecond := 0.0;
 FFrameLoopStart := 0;
 FFrameLoopEnd := 0;

 FChilds := TList.Create;
 FParentSurface := NIL;
end;

procedure TSimpleSurfaceWithEffect.SetFrame(AValue: single);
begin
 FFrame:=AValue;
end;

{
 procedure TSimpleSurfaceWithEffect.RemoveChild( ASurface: TSimpleSurfaceWithEffect);
 var i: integer;
 begin
  i := FChilds.IndexOf( ASurface );
  if i <> -1 then FChilds.Delete( i );
 end;
}

function TSimpleSurfaceWithEffect.GetScenario(AIDScenario: TIDScenario ): TScenario;
begin
 if ( AIDScenario > -1 ) and ( AIDScenario < Length(FListScenario) )
   then Result := FListScenario[AIDScenario]
   else Result := NIL;
end;

destructor TSimpleSurfaceWithEffect.Destroy;
var i: integer;
begin
 while FChilds.Count > 0 do
  begin
   TSimpleSurfaceWithEffect( FChilds.Items[0] ).Free;
   FChilds.Delete( 0 );
  end;
 FreeAndNil( FChilds );

 FreeAndNil( Speed );
 FreeAndNil( Angle );
 FreeAndNil( Opacity );
 FreeAndNil( Tint );
 FreeAndNil( Scale );
 FreeAndNil( Skew );

 for i:=0 to Length( FListScenario )-1 do
  FListScenario[i].Free;
 SetLength( FListScenario, 0 );

 inherited Destroy;
end;


procedure TSimpleSurfaceWithEffect.Update(const aElapsedTime: single);
var Xr,Yr,s,c: single ;
    i, loopLength: integer;
begin
 if FFreeze then exit ;

 Speed.OnElapse( aElapsedTime );

  if X.State <> psNO_CHANGE
    then X.OnElapse( aElapsedTime )
    else X.Value := X.Value + Speed.x.Value * aElapsedTime;
  if Y.State <> psNO_CHANGE
    then Y.OnElapse( aElapsedTime )
    else Y.Value := Y.Value + Speed.y.Value * aElapsedTime;

  if FTimeBeforeKill <> 0
   then begin
         FTimeBeforeKill -= aElapsedTime;
         if FTimeBeforeKill <= 0 then FKill := TRUE;
   end;

 FFrame += aElapsedTime * FFrameDeltaPerSecond;
 if (FFrameLoopEnd <> 0) and (FFrameLoopStart <> 0) then
   begin
     loopLength := FFrameLoopEnd-FFrameLoopStart+1;
     if FFrame >= FFrameLoopEnd then
       begin
         if loopLength <= 1 then
           FFrame := FFrameLoopEnd
         else
         begin
           FFrame := FFrame - Trunc((FFrame-(FFrameLoopStart))/loopLength)*loopLength;
           if FFrame > FFrameLoopEnd then FFrame := FFrameLoopStart;
           if FFrame < FFrameLoopStart then FFrame := FFrameLoopStart;
         end;
       end else
     if FFrame < FFrameLoopStart then
       begin
         if loopLength <= 1 then
           FFrame := FFrameLoopStart
         else
         begin
           FFrame += Trunc((FFrameLoopEnd-FFrame)/loopLength)*loopLength;
           if FFrame > FFrameLoopEnd then FFrame := FFrameLoopEnd;
           if FFrame < FFrameLoopStart then FFrame := FFrameLoopEnd;
         end;
       end;
   end;

 // Do Blink
 with FBlink do
 if BlinkToGo <> 0
 then begin
       if CountInvisible > 0
         then begin // surface is invisible
               FVisible := FALSE ;
               CountInvisible -= aElapsedTime ;
               CountVisible := VisibleTime ;
              end
         else begin // surface is visible
               FVisible := TRUE ;
               CountVisible -= aElapsedTime ;
               if CountVisible <= 0
                 then begin
                       CountInvisible := InvisibleTime ;
                       if BlinkToGo > 0 then dec ( BlinkToGo ) ;
                       if BlinkToGo = 0 then FVisible := TRUE ;
                      end;
              end;
      end;
 // Angle
 Angle.OnElapse( aElapsedTime );
 // Do Rotation around an axis
 with FRotationAroundPt do
  if InProgress
    then begin
           Xr := GetCenterX - XPt ;
           Yr := GetCenterY - YPt ;
           sincos( AnglePerSecond * aElapsedTime * deg2rad, s, c );
           SetCenterCoordinate( XPt + Xr * c + Yr * s, YPt - Xr * s  + c * Yr );
   end;
 // Do Opacity Variation
 Opacity.OnElapse( aElapsedTime );
 // Do Tint Variation
 Tint.OnElapse( aElapsedTime );
 // Do Scale Variation
 Scale.OnElapse( aElapsedTime );
 // Do Skew Variation
 Skew.OnElapse( aElapsedTime );
 // Do Bind to another sprite
 if (FBindToSprite.InProgress) and ( FBindToSprite.ParentSurface<>NIL )
   then begin
         X.Value := FBindToSprite.ParentSurface.X.Value + FBindToSprite.OffsetX ;
         Y.Value := FBindToSprite.ParentSurface.Y.Value + FBindToSprite.OffsetY ;
   end;
 // Scenario
 for i:=0 to Length( FListScenario )-1 do
  if FListScenario[i].FIsRunning
    then FListScenario[i].Execute( aElapsedTime );
 // childs update
 for i:=0 to FChilds.Count-1 do
  TSimpleSurfaceWithEffect( FChilds.Items[i] ).Update( aElapsedTime );
 // childs free
 for i:=FChilds.Count-1 downto 0 do
  if TSimpleSurfaceWithEffect( FChilds.Items[i] ).FKill
    then begin
          TSimpleSurfaceWithEffect( FChilds.Items[i] ).Free;
          FChilds.Delete( i );
    end;
end;

procedure TSimpleSurfaceWithEffect.Draw(const aLayerPercentOpacity : single);
begin
 // draw is done in descendant classes
 if aLayerPercentOpacity = 0 then;
end;

{
 procedure TSimpleSurfaceWithEffect.AddChild(ASurface: TSimpleSurfaceWithEffect; AOffset: TPointF);
 begin
  AddChild( ASurface, AOffset.x, AOffset.y );
 end;

 procedure TSimpleSurfaceWithEffect.AddChild(ASurface: TSimpleSurfaceWithEffect;
   AXOffset, AYOffset: single);
 begin
  AddChild( ASurface );

  Asurface.X.Value := AXOffset;
  Asurface.Y.Value := AYOffset;
 end;
}

procedure TSimpleSurfaceWithEffect.AddChild(ASurface: TSimpleSurfaceWithEffect);
begin
 FChilds.Add( ASurface );

 ASurface.FParentSurface := self;
 ASurface.FParentScene := FParentScene;
 ASurface.FParentLayer := FParentLayer;
end;

procedure TSimpleSurfaceWithEffect.KillDefered(ASec: single);
begin
 FTimeBeforeKill := ASec;
end;

procedure TSimpleSurfaceWithEffect.MoveTo(aX, aY: single; aDuration: single; aCurve: Word);
begin
 MoveTo( aX, aY, aDuration, aCurve, aCurve);
end;

procedure TSimpleSurfaceWithEffect.MoveTo(aX, aY: single; aDuration: single; aXCurve, aYCurve: Word);
begin
 X.ChangeTo( aX, aDuration, aXCurve );
 Y.ChangeTo( aY, aDuration, aYCurve );
end;

procedure TSimpleSurfaceWithEffect.MoveCenterTo(aCenterX, aCenterY, aDuration: single; aCurve: Word);
begin
 MoveXCenterTo( aCenterX, aDuration, aCurve );
 MoveYCenterTo( aCenterY, aDuration, aCurve );
end;

procedure TSimpleSurfaceWithEffect.MoveXCenterTo(aX, aDuration: single; aCurve: Word);
begin
 X.ChangeTo( aX - GetWidth / 2, aDuration, aCurve );
end;

procedure TSimpleSurfaceWithEffect.MoveYCenterTo(aY, aDuration: single; aCurve: Word);
begin
 Y.ChangeTo( aY - GetHeight / 2, aDuration, aCurve );
end;

procedure TSimpleSurfaceWithEffect.MoveRelative(aDeltaX, aDeltaY: single; aDuration: single; aCurve: Word);
begin
 MoveTo (X.Value+aDeltaX, Y.Value+aDeltaY, aDuration, aCurve ) ;
end;

procedure TSimpleSurfaceWithEffect.MoveXRelative(aDeltaX, aDuration: single; aCurve: Word);
begin
 X.ChangeTo( X.Value + aDeltaX, aDuration, aCurve );
end;

procedure TSimpleSurfaceWithEffect.MoveYRelative(aDeltaY, aDuration: single; aCurve: Word);
begin
 Y.ChangeTo( Y.Value + aDeltaY, aDuration, aCurve );
end;

procedure TSimpleSurfaceWithEffect.Blink(aNumberOfBlink: integer; aVisibleTime, aInvisibleTime: single );
begin
 with FBlink do
  begin
   BlinkToGo := aNumberOfBlink ;
   VisibleTime := aVisibleTime ;
   InvisibleTime := aInvisibleTime ;
   CountVisible := aVisibleTime ;
   CountInvisible := aInvisibleTime ;
  end;
end;

procedure TSimpleSurfaceWithEffect.StopBlink;
begin
 FBlink.BlinkToGo := 0;
end;

procedure TSimpleSurfaceWithEffect.RotationAroundPoint( aXPt , aYPt : single ; aAnglePerSecond : single ; aSelfRotate : boolean) ;
var xx,yy:single;
begin
 with FRotationAroundPt do
  begin
   InProgress := TRUE ;
   XPt := aXPt ;
   YPt := aYPt ;
   AnglePerSecond := aAnglePerSecond ;
   // calculate actual angle and radius
   xx:=CenterX-aXPt; yy:=CenterY-aYPt;
   Radius:=sqrt( xx * xx + yy * yy );
   if Radius>0 then ActualAngleRad := arccos( xx / Radius )
               else ActualAngleRad := 0;
  end;
 if aSelfRotate then Angle.AddConstant( -aAnglePerSecond );
end;

{
type
  FlipCoordIndex = array[0..1] of integer ;
const
  FLIP_COORD : array[0..3,0..3] of FlipCoordIndex = ( ((0,1), (1,1), (1,0), (0,0)),
                                                      ((1,1), (0,1), (0,0), (1,0)),
                                                      ((0,0), (1,0), (1,1), (0,1)),
                                                      ((1,0), (0,0), (0,1), (1,1)) );
}


procedure TSimpleSurfaceWithEffect.PushAndPrepareMatrix;
begin
 glPushMatrix;
 if FParentSurface = NIL then glTranslatef( X.Value + Width * 0.5, Y.Value + Height * 0.5 , 0 )
                         else glTranslatef( X.Value + Width * 0.5 * Scale.x.Value, Y.Value + Height * 0.5 * Scale.y.Value, 0 );

 glScalef( Scale.x.Value, Scale.y.Value, 0 );
 glRotatef( Angle.Value, 0, 0, 1 );
 glTranslatef( -Width * 0.5, -Height * 0.5, 0 );
end;

procedure TSimpleSurfaceWithEffect.PopMatrix;
begin
 glPopMatrix;
end;

procedure TSimpleSurfaceWithEffect.DrawChildSurfaces(const aLayerPercentOpacity: single);
var i: integer;
begin
 for i:=0 to FChilds.Count-1 do
  TSimpleSurfaceWithEffect( FChilds.Items[i] ).Draw( aLayerPercentOpacity );
end;


type
  PTextureCoordIndex = ^TTextureCoordIndex;
  TTextureCoordIndex = array[ 0..3 ] of integer;
const
  FLIP_TEXCOORD : array[ 0..3 ] of TTextureCoordIndex = ( ( 0, 1, 2, 3 ), ( 1, 0, 3, 2 ), ( 3, 2, 1, 0 ), ( 2, 3, 0, 1 ) );//( ( 3, 2, 1, 0 ), ( 2, 3, 0, 1 ), ( 0, 1, 2, 3 ), ( 1, 0, 3, 2 ) );

procedure TSimpleSurfaceWithEffect.DrawSurface(aTexture: PTexture; AFrameIndex: integer; const aLayerPercentOpacity: single);
var
  quad : array[ 0..3 ] of TPointF;
  tci  : PTextureCoordIndex;
  TexCoord : TTexCoor;
  AlphaTint:byte;
  i: integer;
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

 PushAndPrepareMatrix;

 quad[ 0 ].X := 0;  quad[ 0 ].Y := 0;
 quad[ 1 ].X := Width; quad[ 1 ].Y := 0;
 quad[ 2 ].X := Width; quad[ 2 ].Y := Height;
 quad[ 3 ].X := 0;  quad[ 3 ].Y := Height;

 // Skew
 quad[0].X += Skew.x.Value * Scale.x.Value;
 quad[0].Y += Skew.y.Value * Scale.y.Value;
 quad[1].X += Skew.x.Value * Scale.x.Value;
 quad[1].Y += Skew.y.Value * Scale.y.Value;

 // Flip
 i:=0;
 if FFlipH then inc( i );
 if FFlipV then inc( i, 2 );
 tci := @FLIP_TEXCOORD[ i ];

// glEnable(GL_BLEND);
 SetBlendMode( FBlendMode );

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
   DrawBox(X.Value, Y.Value, aTexture^.TextureWidth, aTexture^.TextureHeight, BGRA(255,0,0));
 {$ENDIF}

 DrawChildSurfaces( aLayerPercentOpacity );
 PopMatrix;
end;

procedure TSimpleSurfaceWithEffect.DrawSurface(const aTopLeftColor, aTopRightColor,
  aBottomRightColor, aBottomLeftColor: TBGRAPixel; const aLayerPercentOpacity: single);
var quad : array[ 0..3 ] of TPointF;
    a : Single ;
begin
 a := aLayerPercentOpacity * Opacity.Value / 255;

 PushAndPrepareMatrix;

 quad[ 0 ].X := 0;  quad[ 0 ].Y := 0;
 quad[ 1 ].X := Width; quad[ 1 ].Y := 0;
 quad[ 2 ].X := Width; quad[ 2 ].Y := Height;
 quad[ 3 ].X := 0;  quad[ 3 ].Y := Height;

 // Skew
 quad[0].X += Skew.x.Value * Scale.x.Value;
 quad[0].Y += Skew.y.Value * Scale.y.Value;
 quad[1].X += Skew.x.Value * Scale.x.Value;
 quad[1].Y += Skew.y.Value * Scale.y.Value;

 // Flip property is ignored

 SetBlendMode( FBlendMode );

 TextureManager.DisableTextureUsage;

 if Tint.alpha.Value < 255
   then begin
         glBegin( GL_QUADS );
           with aTopLeftColor do
             glColor4ub( red, green, blue, round(alpha * a) ); glVertex2fv( @quad[ 0 ] );
           with aTopRightColor do
             glColor4ub( red, green, blue, round(alpha * a) ); glVertex2fv( @quad[ 1 ] );
           with aBottomRightColor do
             glColor4ub( red, green, blue, round(alpha * a) ); glVertex2fv( @quad[ 2 ] );
           with aBottomLeftColor do
             glColor4ub( red, green, blue, round(alpha * a) ); glVertex2fv( @quad[ 3 ] );
        glEnd;
   end;

 if Tint.alpha.Value <> 0
   then begin  //Apply Tint
         glColor4ub( round(Tint.red.Value), round(Tint.Green.Value), round(Tint.Blue.Value), round(Tint.alpha.Value * a));
         glBegin( GL_QUADS );
           glVertex2fv( @quad[ 0 ] );
           glVertex2fv( @quad[ 1 ] );
           glVertex2fv( @quad[ 2 ] );
           glVertex2fv( @quad[ 3 ] );
         glEnd;
        end;

 {$IFDEF DEBUG_MODE_ON}
   DrawBox(X.Value, Y.Value, Width, Height, BGRA(255,0,0));
 {$ENDIF}

 DrawChildSurfaces( aLayerPercentOpacity );
 PopMatrix;
end;

procedure TSimpleSurfaceWithEffect.SetFrameLoopBounds( AStartFrameIndex, AEndFrameIndex: integer );
var temp: integer;
begin
 if AStartFrameIndex > AEndFrameIndex
   then begin
         temp := AStartFrameIndex;
         AStartFrameIndex := AEndFrameIndex;
         AEndFrameIndex := temp;
   end;

 FFrameLoopStart := AStartFrameIndex;
 FFrameLoopEnd := AEndFrameIndex;

 if FFrame < FFrameLoopStart
   then FFrame := FFrameLoopStart
   else if FFrame > FFrameLoopEnd
          then FFrame := FFrameLoopEnd;
end;

procedure TSimpleSurfaceWithEffect.FrameAddPerSecond(
  ADeltaFramePerSecond: single);
begin
 FFrameDeltaPerSecond := ADeltaFramePerSecond;
end;

function TSimpleSurfaceWithEffect.AddScenario(const aFilename: string; ACallBackEvent: TScenarioEvent): TIDScenario;
var o: TScenario ;
begin
 o := TScenario.Create;
 o.LoadFromFile( aFilename );
 o.CurrentLine := 0;
 o.OnEvent := ACallBackEvent;
 Result := AddScenario( o );
end;

function TSimpleSurfaceWithEffect.AddScenario(aScenario: TScenario ): TIDScenario;
begin
 Result := Length( FListScenario );
 SetLength( FListScenario, Result + 1 );
 FListScenario[Result] := aScenario;
 aScenario.ParentSurface := Self;
 aScenario.ID := Result;
end;

procedure TSimpleSurfaceWithEffect.PlayScenario(AIDScenario: TIDScenario; AFromBeginning: boolean);
var s: TScenario;
begin
 s:= GetScenario(AIDScenario);
 if s = NIL then exit;

 if AFromBeginning then
 begin
  s.CurrentLine := 0;
  s.WaitTime := 0.0;
 end;

 s.FIsRunning:=TRUE;
end;

function TSimpleSurfaceWithEffect.AddAndPlayScenario(const aFilename: string;
  ACallBackEvent: TScenarioEvent): TIDScenario;
begin
 Result := AddScenario( aFilename, ACallBackEvent );
 PlayScenario( Result );
end;

function TSimpleSurfaceWithEffect.AddAndPlayScenario(aScenario: TScenario
  ): TIDScenario;
begin
 Result := AddScenario( aScenario );
 PlayScenario( Result );
end;

procedure TSimpleSurfaceWithEffect.StopScenario(AIDScenario: TIDScenario);
var s: TScenario;
begin
 s:= GetScenario( AIDScenario );
 if s <> NIL then s.FIsRunning := FALSE;
end;

procedure TSimpleSurfaceWithEffect.StopAllScenario;
var i: integer;
begin
 for i :=0 to Length( FListScenario )-1 do
  FListScenario[i].FIsRunning := FALSE;
end;

function TSimpleSurfaceWithEffect.ScenarioIsPlaying(AIDScenario: TIDScenario ): boolean;
var s: TScenario;
begin
 s:= GetScenario( AIDScenario );
 if s = NIL
   then Result := FALSE
   else Result := S.FIsRunning;
end;


procedure TSimpleSurfaceWithEffect.BindToSprite(aParentSurface: TSimpleSurface; aOffsetX, aOffsetY: single);
begin
 FBindToSprite.InProgress:=TRUE;
 FBindToSprite.ParentSurface:=aParentSurface;
 FBindToSprite.OffsetX:=aOffsetX;
 FBindToSprite.OffsetY:=aOffsetY;
end;

procedure TSimpleSurfaceWithEffect.BindCenterToSprite(
  aParentSurface: TSimpleSurface);
begin
 BindToSprite( aParentSurface,
        (aParentSurface.Width-Width)*0.5, (aParentSurface.Height-Height)*0.5 );
end;

procedure TSimpleSurfaceWithEffect.UnBind;
begin
 FBindToSprite.InProgress := FALSE;
end;

{ TScenario }

constructor TScenario.Create;
begin
 ID := -1 ;
 Actions := TStringList.Create ;
 CurrentLine := 0;
 WaitTime := 0.0 ;
 ParentSurface := NIL ;
 FScenarioEvent := NIL;
 FIsRunning := FALSE ;
end;

destructor TScenario.Destroy;
begin
 FreeAndNil(Actions);
end;

procedure TScenario.Execute(const aElapsedTime: double);
var OriginLine: integer;
begin
 if WaitTime > 0
   then WaitTime -= aElapsedTime ;
 if WaitTime <= 0.000
   then begin
         OriginLine := CurrentLine;
         repeat
          DecodeAction;
          inc(CurrentLine);
         until ( WaitTime > 0 ) or ( CurrentLine >= Actions.Count ) or
               not FIsRunning or ( OriginLine = CurrentLine ); // avoid infinite loop in buggy scenario
         if CurrentLine >= Actions.Count then FIsRunning := FALSE ;
   end;
end;

procedure TScenario.LoadFromFile(const aFilename: string);
begin
 Actions.Clear ;
 Actions.LoadFromFile( aFilename ) ;
end;

procedure TScenario.SaveToFile(const aFilename: string);
begin
 Actions.SaveToFile( aFilename ) ;
end;

procedure TScenario.DecodeAction;
var i: integer;
begin
 if Length(Actions.Strings[CurrentLine]) = 0 then exit;

 FTemp := SplitLineToStringArray( Actions.Strings[CurrentLine], ' ' );

 case FTemp.[0] of
  acWait : WaitTime := StrToFloat( FTemp[1] );
  acKill : ParentSurface.Kill;
  acFreeze : ParentSurface.Freeze := FTemp[1] = acTRUE ;
  acVisible : ParentSurface.Visible := FTemp[1] = acTRUE ;

  acOpacity : ParentSurface.Opacity.Value := strtoint( FTemp[1] );
  acOpacityChange : ParentSurface.Opacity.ChangeTo( strtoint( FTemp[1] ), StrToFloat( FTemp[2] ),  StringToCurveID( FTemp[3] ) );

  acTint : ParentSurface.Tint.Value := BGRA( strtoint( FTemp[1] ), strtoint( FTemp[2] ), strtoint( FTemp[3] ), strtoint( FTemp[4] ) );
  acTintChange : ParentSurface.Tint.ChangeTo( BGRA( strtoint( FTemp[1] ),
                                                    strtoint( FTemp[2] ),
                                                    strtoint( FTemp[3] ),
                                                    strtoint( FTemp[4] )),
                                                    StrToFloat( FTemp[5] ),
                                                    StringToCurveID( FTemp[6] ));
  acTintRedChange: ParentSurface.Tint.Red.ChangeTo( strtoint( FTemp[1] ), StrToFloat( FTemp[2] ),  StringToCurveID( FTemp[3] ));
  acTintGreenChange: ParentSurface.Tint.Green.ChangeTo( strtoint( FTemp[1] ), StrToFloat( FTemp[2] ),  StringToCurveID( FTemp[3] ));
  acTintBlueChange: ParentSurface.Tint.Blue.ChangeTo( strtoint( FTemp[1] ), StrToFloat( FTemp[2] ),  StringToCurveID( FTemp[3] ));
  acTintAlphaChange : ParentSurface.Tint.Alpha.ChangeTo( strtoint( FTemp[1] ), StrToFloat( FTemp[2] ),  StringToCurveID( FTemp[3] ));

  acAngle : ParentSurface.Angle.Value := StrToFloat( FTemp[1] ) ;
  acRotate : ParentSurface.Angle.AddConstant( StrToFloat( FTemp[1] ) );
  acRotateTo : ParentSurface.Angle.ChangeTo( StrToFloat( FTemp[1] ), StrToFloat( FTemp[2] ),  StringToCurveID( FTemp[3] )  );
  acRotationAroundAxis : ParentSurface.RotationAroundPoint( StrToFloat( FTemp[1] ), StrToFloat( FTemp[2] ), StrToFloat( FTemp[3] ), FTemp[4] = acTRUE );

  acSkew : ParentSurface.Skew.ChangeTo( PointF( strtoint( FTemp[1] ), strtoint( FTemp[2] ) ), StrToFloat( FTemp[3] ),  StringToCurveID( FTemp[4]  ) );

  acScale : ParentSurface.Scale.Value := PointF( StrToFloat( FTemp[1] ), StrToFloat( FTemp[1] ) );
  acScaleChange : begin
   ParentSurface.Scale.x.ChangeTo( StrToFloat( FTemp[1] ), StrToFloat( FTemp[2] ), StringToCurveID( FTemp[3] ));
   ParentSurface.Scale.y.ChangeTo( StrToFloat( FTemp[1] ), StrToFloat( FTemp[2] ), StringToCurveID( FTemp[3] ));
  end;
  acScaleH : ParentSurface.Scale.x.Value := StrToFloat( FTemp[1] );
  acScaleHChange : ParentSurface.Scale.x.ChangeTo( StrToFloat( FTemp[1] ), StrToFloat( FTemp[2] ), StringToCurveID( FTemp[3] ));
  acScaleV : ParentSurface.Scale.y.Value := StrToFloat( FTemp[1] );
  acScaleVChange : ParentSurface.Scale.y.ChangeTo( StrToFloat( FTemp[1] ), StrToFloat( FTemp[2] ), StringToCurveID( FTemp[3] ));

  acBlink: ParentSurface.Blink( strtoint( FTemp[1] ), StrToFloat( FTemp[2] ), StrToFloat( FTemp[3] ) );
  acStopBlink: ParentSurface.StopBlink;

  acFlipH: ParentSurface.FlipH := FTemp[1] = acTRUE ;
  acFlipV: ParentSurface.FlipV := FTemp[1] = acTRUE ;
  acToggleFlipH: ParentSurface.FlipH := not ParentSurface.FlipH;
  acToggleFlipV: ParentSurface.FlipV := not ParentSurface.FlipV;

  acAnimate: begin
              ParentSurface.SetFrameLoopBounds( strtoint( FTemp[1] ), strtoint( FTemp[2] ) );
              ParentSurface.FrameAddPerSecond( StrToFloat( FTemp[3] ));
  end;
  acIncFrame : ParentSurface.Frame := ParentSurface.Frame + 1;
  acDecFrame : ParentSurface.Frame := ParentSurface.Frame - 1;
  acSetFrame : ParentSurface.Frame := strtoint( FTemp[1] );


  acMoveTo  : ParentSurface.MoveTo( StrToFloat( FTemp[1] ), StrToFloat( FTemp[2] ), StrToFloat( FTemp[3] ), StringToCurveID( FTemp[4] ));
  acMoveXTo : ParentSurface.X.ChangeTo( StrToFloat( FTemp[1] ), StrToFloat( FTemp[2] ), StringToCurveID( FTemp[3] ));
  acMoveYTo : ParentSurface.Y.ChangeTo( StrToFloat( FTemp[1] ), StrToFloat( FTemp[2] ), StringToCurveID( FTemp[3] ));

  acMoveCenterTo  : ParentSurface.MoveCenterTo( StrToFloat( FTemp[1] ), StrToFloat( FTemp[2] ), StrToFloat( FTemp[3] ), StringToCurveID( FTemp[4] ) );
  acMoveXCenterTo : ParentSurface.MoveXCenterTo( StrToFloat( FTemp[1] ), StrToFloat( FTemp[2] ), StringToCurveID( FTemp[3] ) );
  acMoveYCenterTo : ParentSurface.MoveYCenterTo( StrToFloat( FTemp[1] ), StrToFloat( FTemp[2] ), StringToCurveID( FTemp[3] ) );

  acMoveRelative   : ParentSurface.MoveRelative( StrToFloat( FTemp[1] ), StrToFloat( FTemp[2] ), StrToFloat( FTemp[3] ), StringToCurveID( FTemp[4] ));
  acMoveXRelative  : ParentSurface.MoveXRelative( StrToFloat( FTemp[1] ), StrToFloat( FTemp[2] ), StringToCurveID( FTemp[3] ));
  acMoveYRelative  : ParentSurface.MoveYRelative( StrToFloat( FTemp[1] ), StrToFloat( FTemp[2] ), StringToCurveID( FTemp[3] ));

  acSetCoor : begin
    ParentSurface.X.Value := StrToFloat( FTemp[1] );
    ParentSurface.Y.Value := StrToFloat( FTemp[2] );
  end;
  acSetCenterCoor : ParentSurface.SetCenterCoordinate( StrToFloat( FTemp[1] ), StrToFloat( FTemp[2] ));

  acSendEvent : if FScenarioEvent <> NIL then FScenarioEvent( ParentSurface, ID, strtoint( FTemp[1] ) );

  acLoop : CurrentLine := -1 ;
  acLabel :;
  acGotoLabel : begin
   i := Actions.IndexOf( acLabel + ' ' + FTemp[1] );
   if i <> -1 then CurrentLine := i - 1;
  end

  else DecodeAdditionnalAction( FTemp );
 end;//case
end;

procedure TScenario.AddLine(const aLine: string);
begin
 Actions.Add( aLine );
end;

procedure TScenario.DecodeAdditionnalAction(aAction:ArrayOfString);
begin
// nothing here
end;


{ TSimpleSurface }

constructor TSimpleSurface.Create;
begin
 X := TFParam.Create;
 Y := TFParam.Create;

 FVisible := TRUE ;
 FKill := FALSE ;
 FIndex := 0 ;
 FTag1 := 0 ;
 FTag2 := FALSE ;

 FParentLayer := NIL;
 FParentScene := NIL;
end;

destructor TSimpleSurface.Destroy;
begin
 FreeAndNil( X );
 FreeAndNil( Y );
 inherited Destroy;
end;

function TSimpleSurface.GetCenterX: single;
begin
 Result := x.Value + GetWidth * 0.5 ;
end;

function TSimpleSurface.GetRightX: single;
begin
 Result := x.Value + GetWidth - 1 ;
end;

function TSimpleSurface.GetBottomY: single;
begin
 Result := y.Value + GetHeight -1 ;
end;

function TSimpleSurface.GetCenterY: single;
begin
 Result := y.Value + GetHeight * 0.5 ;
end;

procedure TSimpleSurface.SetCenterX(const AValue: single);
begin
 x.Value := AValue - GetWidth * 0.5 ;
end;

procedure TSimpleSurface.SetRightX(const AValue: single);
begin
 x.Value := AValue - GetWidth - 1 ;
end;

procedure TSimpleSurface.SetBottomY(const AValue: single);
begin
 y.Value := AValue - GetHeight - 1 ;
end;

procedure TSimpleSurface.SetCenterY(const AValue: single);
begin
 y.Value := AValue - GetHeight * 0.5 ;
end;

function TSimpleSurface.GetRectArea: TRect;
begin
 Result.Create( Point(round(X.Value), round(Y.Value)), Width, Height );
end;

procedure TSimpleSurface.Kill;
begin
 FKill := TRUE;
end;

procedure TSimpleSurface.SetCoordinate(aX, aY: single);
begin
 x.Value := aX;
 Y.Value := aY;
end;

procedure TSimpleSurface.SetCenterCoordinate(aCenterX, aCenterY:single);
begin
 SetCenterX( aCenterX );
 SetCenterY( aCenterY );
end;







