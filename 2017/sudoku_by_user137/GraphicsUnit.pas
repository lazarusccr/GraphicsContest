unit GraphicsUnit;

{$mode objfpc}{$H+}

interface

uses
  SysUtils, Dialogs, dglOpenGL, nxGL, math, nxMath, nxMath3D,
  nxTypes, GameUnit, sudokuUnit;

type

  { TGraphicalGame }

  TGraphicalGame = class(TGame)
  public
    constructor Create;
    destructor Destroy; override;
    procedure Draw; override;
  end;

implementation

{ TGame }

constructor TGraphicalGame.Create;
begin
  inherited Create;
  nx.Clear(true, true);
  nx.Flip;

  nx.CreateFont('Arial', 12, 256);
  tex.AddTexture('tile', GetPath('data\tile.png'), true);
  tex.SetPattern(tex.count-1, 31, 31, 1, 1);
  tex.AddTexture('numbers', GetPath('data\numbers.png'), true);
  tex.SetPattern(tex.count-1, 32, 48, 0, 0);

  //tex.TextureQuality:=GL_NEAREST;
  //tex.AddTexture('numbers_small', GetPath('data\numbers_small.png'), true);
  //tex.SetPattern(tex.count-1, 30, 30, 0, 0);

  tex.AddTexture('back', GetPath('data\back.jpg'), false);
  tex.AddTexture('shadow', GetPath('data\shadowy_tile.png'), true);

  // Last operations
  if nx.LastError<>'' then ShowMessage(nx.LastError);
  ResetTick; Initialized:=true;
end;

destructor TGraphicalGame.Destroy;
begin
  sudoku.Free;
  inherited Destroy;
end;

procedure TGraphicalGame.Draw;
var i, j: integer; hs, alpha: single;
    mpos: TVector;
begin
  nx.Clear(true, true);

  // Initial camera angle and position
  glLoadIdentity;
  if state=gsFinished then begin
    glTranslatef(0, 0, -13.5+appearZ);
    glRotatef(170+appearZ*10, 1, 0, 0);
  end else begin
    glTranslatef(0, 0, -13.5);
    glRotatef(170, 1, 0, 0);
  end;

  hs:=sudoku.size/2;

  // Draw background
  tex.SetByName('back');
  nx.SetColor(0.6, 0.7, 0.6, 1);
  nx.DrawScaled(-8, -6.2, 16, 12);

  // Align camera to game grid
  glTranslatef(-hs, -hs, 0);

  // Find focused grid point
  if state=gsPlaying then begin
    mpos:=nx.MouseRayAtPlane(mp.x, mp.y, nullVector, vector(0, 0, 1));
    gridFocus.x:=floor(mpos.x);
    gridFocus.y:=floor(mpos.y);
    if (not pointinrect(gridFocus.x, gridFocus.y, recti(0, 0, 8, 8))) or
       (sudoku.locked[gridFocus.x, gridFocus.y]) then
      gridFocus:=point(-1, -1);
  end;

  // Draw tiles
  nx.rs.AddBlend:=true;
  tex.SetByName('tile');
  nx.SetColor(1, 1, 1);
  for j:=0 to sudoku.size-1 do begin
    for i:=0 to sudoku.size-1 do begin
      if (i div 3+j div 3) mod 2 = 1 then nx.SetColor(0.5, 1, 0.5)
      else nx.SetColor(1, 1, 1);
      if sudoku.grid[i, j]>0 then
        nx.DrawScaled(i, j, 1.02, 1.02, 1)
      else
        nx.DrawScaled(i, j, 1.02, 1.02, 0);
    end;
  end;
  if gridFocus.x>=0 then begin
    // Draw focused tile
    nx.DrawScaled(gridFocus.x+0.02, gridFocus.y+0.02, 0.96, 0.96, 0);
  end;
  tex.SetByName('numbers');
  for j:=0 to sudoku.size-1 do begin
    for i:=0 to sudoku.size-1 do begin
      if sudoku.grid[i, j]>0 then begin
        if sudoku.thread = nil then alpha:=1
        else alpha:=fmod(i/2.3+j/3.2+t*0.002, 1);
        if sudoku.locked[i, j] then begin
          // Locked number
          nx.SetColor(1, 1, 0.5, alpha);
          nx.DrawScaled(i-0.1, j-0.1, 1.2, 1.2, sudoku.grid[i, j])
        end else begin
          // Player placed number
          nx.SetColor(1, 1, 1, alpha*0.7);
          nx.DrawScaled(i+0.05, j+0.05, 0.9, 0.9, sudoku.grid[i, j]);
        end;
      end;
    end;
  end;

  nx.rs.AddBlend:=false;

  // Selecting a number
  if state=gsSelectNumber then begin
    glTranslatef(gridFocus.x, gridFocus.y, -appearZ*0.5);
    glScalef(0.8, 0.8, 1);
    glTranslatef(-1, -1, 0);

    // Find what cursor is focusing
    mpos:=nx.MouseRayAtPlane(mp.x, mp.y, nullVector, vector(0, 0, 1));
    selFocus.x:=floor(mpos.x);
    selFocus.y:=floor(mpos.y);
    if not pointinrect(selFocus.x, selFocus.y, recti(0, 0, 2, 2)) then
      selFocus:=point(-1, -1);

    // Draw selection overlay
    tex.SetByName('shadow');
    for j:=0 to 2 do
      for i:=0 to 2 do begin
        if (selFocus.x=i) and (selFocus.y=j) then nx.SetColor(0.1, 0.2, 0.1, 0.7)
        else nx.SetColor(0, 0, 0, 0.7*appearZ);
        nx.DrawScaled(i-0.2, j-0.2, 1.4, 1.4);
      end;
    nx.SetColor(1, 1, 1, appearZ);
    tex.SetByName('numbers');
    for j:=0 to 2 do
      for i:=0 to 2 do nx.DrawScaled(i, j, 1, 1, i+j*3+1);
  end;

  // 2D overlay
  nx.Enable2D;
  nx.SetFont(0);

  if ShowFPS then begin
    nx.SetColor(1, 1, 1);
    nx.Font[0].Draw(1, 1, format('FPS: %d', [nx.FPS]));
  end else begin
    nx.SetColor(1, 1, 1, 0.2);
    nx.Font[0].Draw(1, 1, format('F2 - show FPS', [nx.FPS]));
  end;
  nx.SetColor(0.3, 0.8, 0.3, 0.7);
  nx.Font[0].DrawC(nx.Width/2, 12, 'F1 - New Game');
  if state=gsFinished then begin
    nx.SetColor(1, 1, 0.3);
    nx.Font[0].DrawC(nx.Width/2, 30, '--- Game finished, congratulations! ---');
  end;
  nx.Disable2D;

  nx.Flip;
end;

end.

