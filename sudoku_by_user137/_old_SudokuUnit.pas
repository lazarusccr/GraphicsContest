unit _old_SudokuUnit;

{$MODE Delphi}

interface

uses sysutils, forms;

type
  TSudoku = array[1..650] of shortint;
  PSudoku = ^TSudoku;

  TSudokuCalc = class
  private
    maxn, cw, ch, cols, maxResult: shortint;
    procedure Count(const sudoku: PSudoku; firstN: smallint);
    procedure AddResult(const sudoku: PSudoku);
    function CellAtPoint(x, y: integer): integer;
    function CheckLines(const sudoku: PSudoku; x, y: integer): boolean;
    function CheckCell(const sudoku: PSudoku; c: integer): boolean;
    function IsOK(const sudoku: PSudoku; x, y: integer): boolean;
    procedure Clone(const sudoku: PSudoku; out dest: PSudoku);
    procedure CopySudoku(const source: PSudoku; const dest: PSudoku);
  public
    ResultCount, Size: smallint;
    results: array of PSudoku;
    function StartCount(maxn, maxResult: shortint; sudoku: PSudoku; cWidth: shortint = -1): shortint;
    procedure Clear;
    destructor Destroy; override;
  end;

var SudokuStr: string = '123456789ABCDEF0';
    SudokuStr2: string = 'ABCDEFGHIJKLMNOPQRSTUVWXY';
    recurse, progress: integer;
    ExitCmd: boolean;
    snapshot: PSudoku;

implementation

var CheckTable: array[1..25] of boolean;
   
{ TSudokuCalc }

procedure TSudokuCalc.Clear;
var i: integer;
begin
  if resultcount=0 then exit;
  for i:=0 to ResultCount-1 do freemem(results[i], size);
  setlength(results,0); ResultCount:=0;
end;

destructor TSudokuCalc.Destroy;
begin
  Clear;
  inherited;
end;

procedure TSudokuCalc.Clone(const sudoku: PSudoku; out dest: PSudoku);
var i: integer;
begin
  dest:=PSudoku(allocmem(size));
  for i:=maxn+1 to size-1 do dest[i]:=sudoku[i];
end;

procedure TSudokuCalc.AddResult(const sudoku: PSudoku);
begin
  if resultCount>=maxResult then exit;
  inc(resultCount); setlength(results,resultCount);
  Clone(sudoku,results[resultCount-1]);
end;

function TSudokuCalc.StartCount(maxn,maxResult: shortint; sudoku: PSudoku; cWidth: shortint): shortint;
var changed: boolean; sol,sc: integer;
   n,i,j,k: integer; s: PSudoku;
begin
  result:=-1;
  if recurse>0 then exit;
  Clear; ExitCmd:=false;
  if (maxn<2) or (maxn>64) then exit;
  if cWidth=-1 then cWidth:=round(sqrt(maxn))
  else if (cWidth<1) or (cWidth>maxn) then exit;
  if frac(maxn/cWidth)>0.01 then exit;
  self.maxn:=maxn; cw:=cWidth; ch:=maxn div cw;
  cols:=maxn div cw; Size:=maxn*(maxn+1)+1;
  self.maxResult:=maxResult;
  recurse:=0;

  // Go through the board and fill every spot with only 1 choice
  sol:=0; s:=sudoku;
  repeat
    changed:=false;
    for n:=0 to maxn*maxn-1 do begin
      if resultCount>=maxResult then break;
      if s[n+1+maxn]=0 then begin
        i:=n mod maxn+1; j:=n div maxn+1;
        // Look for 1 solution
        sc:=0;
        for k:=1 to maxn do begin
          s[n+1+maxn]:=k;
          if isok(s,i,j) then begin
            inc(sc);
            if sc=2 then break;
            sol:=k;
          end;
        end;
        if sc=1 then begin
          s[n+1+maxn]:=sol; changed:=true;
        end else s[n+1+maxn]:=0;
      end;
    end;
  until not changed;
  snapshot:=PSudoku(allocmem(maxn*(maxn+1)+1));
  CopySudoku(sudoku,snapshot);
  try Count(sudoku,0);
  except end;
  result:=ResultCount;
  recurse:=0;
  freemem(snapshot,maxn*(maxn+1)+1);
  ExitCmd:=false;
end;

function TSudokuCalc.CellAtPoint(x, y: integer): integer;
begin
  result:=(x-1) div cw + (y-1) div ch*cols+1;
end;

function TSudokuCalc.IsOK(const sudoku: PSudoku; x, y: integer): boolean;
begin
  result:=CheckLines(sudoku,x,y);
  if result then result:=CheckCell(sudoku,CellAtPoint(x,y));
end;

function TSudokuCalc.CheckCell(const sudoku: PSudoku; c: integer): boolean;
var i,n,w,h: integer;
begin
  result:=false;
  w:=((c-1) mod cols)*cw +1; h:=((c-1) div cols)*ch +1;
  for i:=1 to maxn do CheckTable[i]:=false;
  for i:=1 to maxn do begin // Index in cell
    n:=sudoku[
      w + ((i-1) mod cw) +        // X
     (h + ((i-1) div cw))*maxn ]; // Y
    if n>0 then
      if CheckTable[n] then exit
      else CheckTable[n]:=true;
  end;
  result:=true;
end;

function TSudokuCalc.CheckLines(const sudoku: PSudoku; x, y: integer): boolean;
var i,n: integer;
begin
  result:=false;
  // Vertical lines
  for i:=1 to maxn do CheckTable[i]:=false;
  for i:=1 to maxn do begin // Index in line
    n:=sudoku[x+i*maxn];
    if n>0 then
      if CheckTable[n] then exit
      else CheckTable[n]:=true;
  end;

  // Horizontal lines
  for i:=1 to maxn do CheckTable[i]:=false;
  for i:=1 to maxn do begin // Index in line
    n:=sudoku[i+y*maxn];
    if n>0 then
      if CheckTable[n] then exit
      else CheckTable[n]:=true;
  end;
  result:=true;
end;

procedure TSudokuCalc.Count(const sudoku: PSudoku; firstN: smallint);
var s: PSudoku; i,j,k,n: integer; r: array[1..32] of shortint;
begin
  if ExitCmd then exit;
  inc(recurse); application.ProcessMessages;
  try Clone(sudoku,s);
  except exit; end;
  //if firstN mod 30=0 then application.ProcessMessages;

  for i:=1 to maxn do r[i]:=i;
  for i:=1 to maxn do begin
    j:=random(maxn)+1; n:=r[i]; r[i]:=r[j]; r[j]:=n;
  end;

  // Go through the board and stop on first empty
  for n:=firstN to maxn*maxn-1 do begin
    progress:=n*100 div (maxn*maxn-1);
    if resultCount>=maxResult then break;
    if s[n+1+maxn]=0 then begin
      i:=n mod maxn+1; j:=n div maxn+1;
      // Test all choices that fit and create clones
      for k:=1 to maxn do begin
        s[n+1+maxn]:=r[k];
        if isok(s,i,j) then
          if n<maxn*maxn-1 then begin
            count(s,n+1);
            if resultCount>=maxResult then break;
          end else addresult(s);
      end;
      break;
    end else if n=maxn*maxn-1 then addresult(s);
  end;

  try FreeMem(s,size);
  except end;
  //if random<0.1 then
    CopySudoku(sudoku,snapshot);
  dec(recurse); application.ProcessMessages;
end;

procedure TSudokuCalc.CopySudoku(const source, dest: PSudoku);
var i: integer;
begin
  for i:=maxn+1 to size-1 do dest[i]:=source[i];
end;

end.

