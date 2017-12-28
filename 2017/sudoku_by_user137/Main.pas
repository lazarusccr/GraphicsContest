unit Main;

{$mode objfpc}{$H+}

{
  Made with nxPascal and Lazarus
  by: User137
}

interface

uses Classes, SysUtils, Forms, Controls, Dialogs, LCLType, Graphics, StdCtrls,
  nxGL, nxTypes, GameUnit, GraphicsUnit;

type

  { TForm1 }

  TForm1 = class(TForm)
    AppProperties: TApplicationProperties;
    Memo1: TMemo;
    procedure FormChangeBounds(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure AppPropertiesIdle(Sender: TObject; var Done: Boolean);
    procedure FormClose(Sender: TObject; var {%H-}CloseAction: TCloseAction);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; {%H-}X, {%H-}Y: Integer);
    procedure FormMouseWheel(Sender: TObject; {%H-}Shift: TShiftState; {%H-}WheelDelta: Integer; {%H-}MousePos: TPoint; var {%H-}Handled: Boolean);
  private
    game: TGraphicalGame;
    updateDelay: cardinal;
    procedure OnSudokuRecurse;
  public
  end;

var Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  // Initialize game window
  clientHeight:=screen.Height * 3 div 5;
  clientWidth:=clientHeight * 4 div 3;

  Randomize;

  if not nx.CreateGlWindow(self) then begin
    showmessage('Failed to initialize OpenGL!');
  end;
end;

procedure TForm1.FormChangeBounds(Sender: TObject);
begin
  clientHeight:=clientWidth * 3 div 4;
end;

procedure TForm1.FormPaint(Sender: TObject);
begin
  // Create game
  if (game=nil) and nx.AllOK then begin
    game:=TGraphicalGame.Create;
    if not game.Initialized then begin
      // Failed to initialize game
      FreeAndNil(game);
    end else with game do begin
      // Start game
      updateDelay:=t;
      sudoku.onRecurse:=@OnSudokuRecurse;
      sudoku.Generate(fill);
    end;
    onPaint:=nil; // No need to trigger this event again
  end;
end;

procedure TForm1.AppPropertiesIdle(Sender: TObject; var Done: Boolean);
begin
  if game<>nil then begin
    Done:=false;
    game.Idle;
  end;
end;

procedure TForm1.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  if game<>nil then FreeAndNil(game);
  nx.KillGLWindow;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if game<>nil then
    with game do begin
      KeyDown(key, shift);
      case key of
        VK_ESCAPE: Close;
        VK_F2: ShowFPS:=not ShowFPS;
        VK_F1: begin
          // Start new game
          if sudoku.thread<>nil then begin
            // Abort existing thread if any
            sudoku.thread.done:=true;
            while sudoku.thread<>nil do begin
              application.ProcessMessages;
              sleep(10);
            end;
          end;
          gridFocus.x:=-1;
          selFocus.x:=-1;
          state:=gsGenerating;
          sudoku.Generate(fill);
        end;
      end;
    end;
end;

procedure TForm1.FormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if game<>nil then
    with game do begin
      KeyUp(key, shift);
    end;
end;

procedure TForm1.FormMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if game<>nil then
    with game do begin
      MouseDown(button, shift);
    end;
end;

procedure TForm1.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if game<>nil then
    with game do begin
      MouseMove(x, y, shift);
    end;
end;

procedure TForm1.FormMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if game<>nil then
    with game do begin
      MouseUp(button, shift);
    end;
end;

procedure TForm1.FormMouseWheel(Sender: TObject; Shift: TShiftState;
  WheelDelta: Integer; MousePos: TPoint; var Handled: Boolean);
begin
  if game<>nil then
    with game do begin

    end;
end;

procedure TForm1.OnSudokuRecurse;
begin
  if game<>nil then begin
    sleep(30); // Sleep will cause fps to drop slightly
               // but it allows us to see the numbers a little while.
  end;
end;

end.

