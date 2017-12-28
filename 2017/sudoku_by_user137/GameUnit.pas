unit GameUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, Dialogs, SysUtils, LCLType, math, nxMath, nxMath3D, nxGL,
  nxGame, nxTypes, sudokuUnit;

type

  TGameState = (gsGenerating, gsPlaying, gsSelectNumber, gsFinished);

  { TGame }

  TGame = class(TGameHandler)
  public
    test: string;
    playerPos: TVector2f;
    sudoku: TSudoku;
    fill, appearZ: single;
    gridFocus, selFocus: TPoint;
    state: TGameState;
    oldMb: array[1..2] of boolean;
    showFPS: boolean;
    constructor Create;
    procedure GameLoop; override;
  end;

implementation

{ TCustomGame }

constructor TGame.Create;
begin
  inherited Create;
  // Initialize and load game variables
  state:=gsGenerating;
  sudoku:=TSudoku.Create(9);
  fill:=0.5;
  //fill:=0.98; // Test completion...
end;

procedure TGame.GameLoop;
var i: integer;
begin
  // Process game tick
  case state of
    gsPlaying: begin
      if mb[1] and (not oldMb[1]) and (gridFocus.x>=0) then begin
        state:=gsSelectNumber;
        selFocus.x:=-1;
        appearZ:=0;
      end else if mb[2] and (gridFocus.x>=0) then begin
        sudoku.grid[gridFocus.x, gridFocus.y]:=0;
      end;
    end;

    gsSelectNumber: begin
      appearZ:=interpolate(appearZ, 1, 0.2);
      if mb[1] and (not oldMb[1]) then begin
        state:=gsPlaying;
        if selFocus.x>=0 then begin
          // Place a number
          sudoku.grid[gridFocus.x, gridFocus.y]:=selFocus.x+selFocus.y*3+1;
          // Check if finished
          if (sudoku.GetEmpties=0) and (sudoku.IsOK) then begin
            state:=gsFinished;
            appearZ:=0;
          end;
        end;
        gridFocus.x:=-1;
      end else if mb[2] and (not oldMb[2]) then begin
        gridFocus.x:=-1;
        state:=gsPlaying;
      end;
    end;

    gsGenerating: begin
      if sudoku.thread=nil then state:=gsPlaying;
    end;

    gsFinished: begin
      appearZ:=interpolate(appearZ, 1, 0.01);
    end;
  end;
  for i:=1 to 2 do oldMb[i]:=mb[i];
end;

end.

