unit sudokuUnit;

{$mode objfpc}{$H+}

interface

uses Forms, Classes, SysUtils;

type
  TGrid = array of array of byte;
  TPoint = record
    x, y: shortint;
  end;

  TUpdateEvent = TThreadMethod;
  TGenerationThread = class;

  { TSudoku }

  TSudoku = class
  private
    cols, maxResults: shortint;
    check: array of boolean;
    updateDelay: cardinal;
    procedure AddResult(const g: TGrid);
    function CellAtPoint(x, y: integer): integer;
    function CheckLines(const g: TGrid; x, y: integer): boolean;
    function CheckCell(const g: TGrid; c: integer): boolean;
    procedure Clone(const src: TGrid; out dest: TGrid);
    procedure CopyValues(const src: TGrid; var dest: TGrid);
    procedure RandomNumbers;
    function Recurse(var g: TGrid; x, y: integer; value: byte; numResults: integer
      ): TGrid;
    procedure RemoveOne(var g: TGrid);
  public
    size, cw, ch: byte;
    grid: TGrid;
    recurseSkips: integer;
    onRecurse: TUpdateEvent;
    locked: array of array of boolean;
    results: array of TGrid;
    thread: TGenerationThread;
    constructor Create(_size: byte; cWidth: shortint = -1);
    destructor Destroy; override;
    procedure Clear;
    function IsOK(const g: TGrid; x, y: integer): boolean;
    function IsOK: boolean;
    function Solution(const src: TGrid; numResults: integer): TGrid;
    procedure Generate(fill: single);
    function GetEmpties: integer;
    procedure LockNumbers;
  end;

  { TGenerationThread }

  TGenerationThread = class(TThread)
  private
    numRes: integer;
    fill: single;
  public
    sudoku: TSudoku;
    done: boolean;
    debugMsg: string;
    constructor Create(_sudoku: TSudoku; numResults: integer; _fill: single);
    procedure Execute; override;
  end;

function Point(x, y: shortint): TPoint;

implementation

//uses Main;

function Point(x, y: shortint): TPoint;
begin
  result.x:=x;
  result.y:=y;
end;

{ TGenerationThread }

constructor TGenerationThread.Create(_sudoku: TSudoku; numResults: integer;
  _fill: single);
begin
  sudoku:=_sudoku;
  numRes:=numResults;
  fill:=_fill;
  FreeOnTerminate:=true;
  done:=false;
  inherited Create(false);
end;

procedure TGenerationThread.Execute;
var i: integer; g: TGrid;
begin
  with sudoku do begin
    g:=Solution(grid, numRes);
    if g<>nil then begin
      for i:=0 to round((1-fill)*size*size)-1 do begin
        RemoveOne(g);
      end;
      CopyValues(g, grid);
      LockNumbers;
    end;
  end;
  sudoku.thread:=nil;
end;

{ TSudoku }

function TSudoku.Solution(const src: TGrid; numResults: integer): TGrid;
var i, j: integer; temp: byte;
    order: array of byte;
    g, res: TGrid;
begin
  Clone(src, g);
  setlength(order, size);
  if g[0, 0]>0 then begin
    result:=Recurse(g, 0, 0, g[0, 0], numResults);
  end else begin
    for i:=0 to size-1 do order[i]:=i+1;
    for i:=0 to size-1 do begin
      j:=random(size);
      temp:=order[i];
      order[i]:=order[j];
      order[j]:=temp;
    end;
    for i:=0 to size-1 do begin
      res:=Recurse(g, 0, 0, order[i], numResults);
      if result=nil then result:=res;
      if (length(results)>=numResults) or ((thread<>nil) and thread.done) then break;
    end;
  end;
end;

function TSudoku.Recurse(var g: TGrid; x, y: integer; value: byte; numResults: integer): TGrid;
var i, j, nx, ny: integer; temp, old: byte;
    order: array of byte;
    res: TGrid; ok: boolean;
begin
  result:=nil;
  if ((thread<>nil) and thread.done) then exit;
  g[x, y]:=value;
  ok:=IsOK(g, x, y);

  // Sync thread to event in main app
  if onRecurse<>nil then begin
    updateDelay:=updateDelay+1;
    if updateDelay>recurseSkips then begin
      updateDelay:=0;
      if g<>nil then CopyValues(g, grid);
      thread.Synchronize(onRecurse);
    end;
  end;

  if ok then begin
    nx:=(x+1) mod size;
    if nx=0 then ny:=y+1
    else ny:=y;
    if ny>=size then begin
      if length(results)<numResults then begin
        AddResult(g);
        clone(g, result);
      end;
    end else begin
      old:=g[nx, ny];
      setlength(order, size);
      if old=0 then begin
        for i:=0 to size-1 do order[i]:=i+1;
        for i:=0 to size-1 do begin
          j:=random(size);
          temp:=order[i];
          order[i]:=order[j];
          order[j]:=temp;
        end;
        for i:=0 to size-1 do begin
          res:=Recurse(g, nx, ny, order[i], numResults);
          if result=nil then result:=res;
          if (length(results)>=numResults) or ((thread<>nil) and thread.done) then break;
        end;
      end else begin
        result:=Recurse(g, nx, ny, g[nx, ny], numResults);
      end;
      g[nx, ny]:=old;
    end;
  end;
end;

procedure TSudoku.AddResult(const g: TGrid);
begin
  if length(results)>=maxResults then exit;
  setlength(results, length(results)+1);
  Clone(g, results[high(results)]);
end;

function TSudoku.CellAtPoint(x, y: integer): integer;
begin
  result:=x div cw + y div ch*cols;
end;

function TSudoku.CheckLines(const g: TGrid; x, y: integer): boolean;
var i, n: integer;
begin
  result:=false;
  // Vertical lines
  for i:=1 to size do check[i]:=false;
  for i:=0 to size-1 do begin
    n:=g[x, i];
    if n>0 then begin
      if check[n] then exit
      else check[n]:=true;
    end;
  end;

  // Horizontal lines
  for i:=1 to size do check[i]:=false;
  for i:=0 to size-1 do begin
    n:=g[i, y];
    if n>0 then begin
      if check[n] then exit
      else check[n]:=true;
    end;
  end;
  result:=true;
end;

function TSudoku.CheckCell(const g: TGrid; c: integer): boolean;
var i, n, x, y: integer;
begin
  result:=false;
  x:=(c mod cols)*cw;
  y:=(c div cols)*ch;
  for i:=1 to size do check[i]:=false;
  for i:=0 to size-1 do begin
    n:=g[x + (i mod cw), y + (i div cw)];
    if n>0 then begin
      if check[n] then exit
      else check[n]:=true;
    end;
  end;
  result:=true;
end;

procedure TSudoku.Clone(const src: TGrid; out dest: TGrid);
var i, j: integer;
begin
  setlength(dest, size, size);
  for i:=0 to size-1 do
    for j:=0 to size-1 do dest[i, j]:=src[i, j];
end;

procedure TSudoku.CopyValues(const src: TGrid; var dest: TGrid);
var i, j: integer;
begin
  for i:=0 to size-1 do
    for j:=0 to size-1 do dest[i, j]:=src[i, j];
end;

procedure TSudoku.RandomNumbers;
var i, j, i2, j2: integer; temp: byte;
begin
  for i:=0 to size-1 do
    for j:=0 to size-1 do grid[i, j]:=(j * 3) mod 9 + i mod 3 +1;
  for i:=0 to size-1 do
    for j:=0 to size-1 do begin
      i2:=random(size); j2:=random(size);
      temp:=grid[i, j];
      grid[i, j]:=grid[i2, j2];
      grid[i2, j2]:=temp;
    end;
end;

procedure TSudoku.RemoveOne(var g: TGrid);
var i, j, n: integer;
    points: array of TPoint;
begin
  setlength(points, size*size);
  n:=0;
  for i:=0 to size-1 do
    for j:=0 to size-1 do begin
      if g[i, j]>0 then begin
        points[n].x:=i;
        points[n].y:=j;
        inc(n);
      end;
    end;
  if n>0 then begin
    n:=random(n);
    g[points[n].x, points[n].y]:=0;
  end;
end;

constructor TSudoku.Create(_size: byte; cWidth: shortint);
begin
  size:=_size;
  if cWidth < 0 then cw:=round(sqrt(size))
  else cw:=cWidth;
  ch:=size div cw;
  cols:=size div cw;
  setlength(grid, size, size);
  setlength(locked, size, size);
  setlength(check, size+1);
  maxResults:=4;
  recurseSkips:=20;
  Clear;
end;

destructor TSudoku.Destroy;
begin
  if thread<>nil then begin
    thread.done:=true;
    repeat
      application.ProcessMessages;
      sleep(50);
    until thread=nil;
  end;
  inherited Destroy;
end;

procedure TSudoku.Clear;
var i, j: integer;
begin
  for i:=0 to size-1 do
    for j:=0 to size-1 do begin
      grid[i, j]:=0;
      locked[i, j]:=false;
    end;
  setlength(results, 0);
end;

procedure TSudoku.Generate(fill: single);
var i: integer;
begin
  Clear;
  if thread=nil then begin
    thread:=TGenerationThread.Create(self, 1, fill);
  end;
end;

function TSudoku.GetEmpties: integer;
var i, j: integer;
begin
  result:=0;
  for i:=0 to size-1 do
    for j:=0 to size-1 do
      if grid[i, j]=0 then inc(result);
end;

procedure TSudoku.LockNumbers;
var i, j: integer;
begin
  for i:=0 to size-1 do
    for j:=0 to size-1 do begin
      locked[i, j]:=(grid[i, j]>0);
    end;
end;

function TSudoku.IsOK(const g: TGrid; x, y: integer): boolean;
begin
  result:=CheckCell(g, CellAtPoint(x, y)) and CheckLines(g, x, y);
end;

function TSudoku.IsOK: boolean;
var i: integer;
begin
  result:=true;
  for i:=0 to size-1 do
    result:=result and CheckCell(grid, i) and CheckLines(grid, i, i);
end;

end.

